#!/usr/bin/env bash
set -euo pipefail

# Creates VPC, subnets, NAT gateway, security groups, and EFS.
# Run once to set up the infrastructure, then update infra/.env with the output.

REGION="us-east-1"

echo "=== Creating VPC ==="
VPC_ID=$(aws ec2 create-vpc \
  --cidr-block 10.0.0.0/16 \
  --query 'Vpc.VpcId' --output text \
  --region "$REGION")
aws ec2 modify-vpc-attribute --vpc-id "$VPC_ID" --enable-dns-support --region "$REGION"
aws ec2 modify-vpc-attribute --vpc-id "$VPC_ID" --enable-dns-hostnames --region "$REGION"
echo "  VPC: $VPC_ID"

echo "=== Creating subnets ==="
SUBNET_A=$(aws ec2 create-subnet \
  --vpc-id "$VPC_ID" --cidr-block 10.0.1.0/24 --availability-zone "${REGION}a" \
  --query 'Subnet.SubnetId' --output text --region "$REGION")
SUBNET_B=$(aws ec2 create-subnet \
  --vpc-id "$VPC_ID" --cidr-block 10.0.2.0/24 --availability-zone "${REGION}b" \
  --query 'Subnet.SubnetId' --output text --region "$REGION")
PUBLIC_SUBNET=$(aws ec2 create-subnet \
  --vpc-id "$VPC_ID" --cidr-block 10.0.0.0/24 --availability-zone "${REGION}a" \
  --query 'Subnet.SubnetId' --output text --region "$REGION")
echo "  Subnets: $SUBNET_A, $SUBNET_B (private), $PUBLIC_SUBNET (public)"

echo "=== Setting up internet access ==="
IGW_ID=$(aws ec2 create-internet-gateway \
  --query 'InternetGateway.InternetGatewayId' --output text --region "$REGION")
aws ec2 attach-internet-gateway --vpc-id "$VPC_ID" --internet-gateway-id "$IGW_ID" --region "$REGION"

PUBLIC_RT=$(aws ec2 create-route-table --vpc-id "$VPC_ID" \
  --query 'RouteTable.RouteTableId' --output text --region "$REGION")
aws ec2 create-route --route-table-id "$PUBLIC_RT" \
  --destination-cidr-block 0.0.0.0/0 --gateway-id "$IGW_ID" --region "$REGION"
aws ec2 associate-route-table --route-table-id "$PUBLIC_RT" \
  --subnet-id "$PUBLIC_SUBNET" --region "$REGION" > /dev/null

EIP_ALLOC=$(aws ec2 allocate-address --domain vpc \
  --query 'AllocationId' --output text --region "$REGION")
NAT_ID=$(aws ec2 create-nat-gateway \
  --subnet-id "$PUBLIC_SUBNET" --allocation-id "$EIP_ALLOC" \
  --query 'NatGateway.NatGatewayId' --output text --region "$REGION")

echo "  Waiting for NAT gateway $NAT_ID (1-2 min)..."
aws ec2 wait nat-gateway-available --nat-gateway-ids "$NAT_ID" --region "$REGION"

PRIVATE_RT=$(aws ec2 create-route-table --vpc-id "$VPC_ID" \
  --query 'RouteTable.RouteTableId' --output text --region "$REGION")
aws ec2 create-route --route-table-id "$PRIVATE_RT" \
  --destination-cidr-block 0.0.0.0/0 --nat-gateway-id "$NAT_ID" --region "$REGION"
aws ec2 associate-route-table --route-table-id "$PRIVATE_RT" \
  --subnet-id "$SUBNET_A" --region "$REGION" > /dev/null
aws ec2 associate-route-table --route-table-id "$PRIVATE_RT" \
  --subnet-id "$SUBNET_B" --region "$REGION" > /dev/null
echo "  Internet access configured"

echo "=== Creating security groups ==="
EFS_SG=$(aws ec2 create-security-group \
  --group-name sabela-efs --description "EFS access for Sabela" \
  --vpc-id "$VPC_ID" --query 'GroupId' --output text --region "$REGION")
aws ec2 authorize-security-group-ingress \
  --group-id "$EFS_SG" --protocol tcp --port 2049 --cidr 10.0.0.0/16 --region "$REGION"

TASK_SG=$(aws ec2 create-security-group \
  --group-name sabela-tasks --description "Sabela Fargate tasks" \
  --vpc-id "$VPC_ID" --query 'GroupId' --output text --region "$REGION")
aws ec2 authorize-security-group-ingress \
  --group-id "$TASK_SG" --protocol tcp --port 3000 --cidr 10.0.0.0/16 --region "$REGION"
echo "  Security groups: $EFS_SG (EFS), $TASK_SG (tasks)"

echo "=== Creating EFS ==="
EFS_ID=$(aws efs create-file-system \
  --performance-mode generalPurpose --throughput-mode bursting --encrypted \
  --tags Key=Name,Value=sabela-shared \
  --query 'FileSystemId' --output text --region "$REGION")

echo "  Waiting for EFS to be available..."
while true; do
  STATE=$(aws efs describe-file-systems --file-system-id "$EFS_ID" \
    --query 'FileSystems[0].LifeCycleState' --output text --region "$REGION")
  if [ "$STATE" = "available" ]; then break; fi
  echo "    EFS state: $STATE, waiting..."
  sleep 5
done

aws efs create-mount-target --file-system-id "$EFS_ID" \
  --subnet-id "$SUBNET_A" --security-groups "$EFS_SG" --region "$REGION" > /dev/null
aws efs create-mount-target --file-system-id "$EFS_ID" \
  --subnet-id "$SUBNET_B" --security-groups "$EFS_SG" --region "$REGION" > /dev/null

AP_ID=$(aws efs create-access-point --file-system-id "$EFS_ID" \
  --posix-user Uid=0,Gid=0 \
  --root-directory "Path=/sabela,CreationInfo={OwnerUid=0,OwnerGid=0,Permissions=755}" \
  --query 'AccessPointId' --output text --region "$REGION")
echo "  EFS: $EFS_ID, Access Point: $AP_ID"

echo ""
echo "========================================="
echo "  Infrastructure created!"
echo "========================================="
echo ""
echo "Update infra/.env with these values:"
echo ""
echo "VPC_ID=\"$VPC_ID\""
echo "SUBNET_PRIVATE_A=\"$SUBNET_A\""
echo "SUBNET_PRIVATE_B=\"$SUBNET_B\""
echo "SUBNET_PUBLIC=\"$PUBLIC_SUBNET\""
echo "SG_EFS=\"$EFS_SG\""
echo "SG_TASKS=\"$TASK_SG\""
echo "EFS_ID=\"$EFS_ID\""
echo "EFS_ACCESS_POINT=\"$AP_ID\""
