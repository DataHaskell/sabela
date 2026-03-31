aws login
aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin 768860981221.dkr.ecr.us-east-1.amazonaws.com
docker build --platform linux/amd64 -t datahaskell/sabela .
docker tag datahaskell/sabela:latest 768860981221.dkr.ecr.us-east-1.amazonaws.com/datahaskell/sabela:latest
docker push 768860981221.dkr.ecr.us-east-1.amazonaws.com/datahaskell/sabela:latest
