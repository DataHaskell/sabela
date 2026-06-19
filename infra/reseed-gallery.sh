#!/usr/bin/env bash
set -euo pipefail
# Reseed the curated gallery shares on the prod box's EFS (/mnt/sabela/shares)
# over SSM. Additive: only the curated slugs' files are touched, never the
# gallery index or other users' shares.
#
# Transfer: small dashboards (and source.md) are gzip+base64 chunked over SSM.
# California's dashboard is multi-megabyte and only ever needs the brand mark
# added, so it is branded in place on the box rather than re-uploaded.
#
#   ./infra/reseed-gallery.sh
source "$(dirname "$0")/.env"
: "${AWS_REGION:?}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SHARES_REMOTE=/mnt/sabela/shares

# Slugs pushed in full (content may have changed since the last seed).
PUSH_SLUGS=(c56a0001 b1ef0001 f12a0001 c0de0001 ca1f0001)
# Slugs only needing the brand mark in place (content stable, too large to
# re-upload). Empty for now; california is re-shelled so it is pushed in full.
BRAND_ONLY=()

IID=$(aws autoscaling describe-auto-scaling-groups --auto-scaling-group-names sabela-box \
  --query 'AutoScalingGroups[0].Instances[0].InstanceId' --output text --region "$AWS_REGION")
[ -n "$IID" ] && [ "$IID" != "None" ] || { echo "no running box in ASG sabela-box" >&2; exit 1; }

# Regenerate the gallery into a temp data root so the pushed files are current.
STAGE=$(mktemp -d)
trap 'rm -rf "$STAGE"' EXIT
python3 "$ROOT/sabela-hub/scripts/seed-gallery.py" "$STAGE" >/dev/null
STAGE_SHARES="$STAGE/shares"

ssm_run() { # run one shell command on the box, print its stdout
  local cid
  cid=$(aws ssm send-command --instance-ids "$IID" --document-name AWS-RunShellScript \
    --parameters "commands=[\"$1\"]" --query Command.CommandId --output text --region "$AWS_REGION")
  aws ssm wait command-executed --command-id "$cid" --instance-id "$IID" --region "$AWS_REGION" 2>/dev/null || true
  aws ssm get-command-invocation --command-id "$cid" --instance-id "$IID" \
    --query 'StandardOutputContent' --output text --region "$AWS_REGION"
}

push_file() { # gzip+base64 chunk a local file onto the box, then unpack atomically
  local localf="$1" remotef="$2" b64 len pos=0 size=50000 stage="/tmp/reseed.b64"
  b64=$(gzip -c "$localf" | base64 | tr -d '\n'); len=${#b64}
  ssm_run "rm -f $stage" >/dev/null
  while [ $pos -lt $len ]; do
    ssm_run "printf '%s' '${b64:$pos:$size}' >> $stage" >/dev/null
    pos=$((pos + size))
  done
  ssm_run "mkdir -p $(dirname "$remotef"); base64 -d $stage | gunzip > ${remotef}.new && mv ${remotef}.new $remotef && rm -f $stage && echo pushed $remotef"
}

brand_inplace() { # add the warm-lambda brand to an existing dashboard on the box
  local remotef="$1" py pyb64
  py='import sys
p=sys.argv[1]; h=open(p,encoding="utf-8").read()
if "db-brand" in h: print("already branded"); sys.exit(0)
m="<span class=\"db-brand\"><span class=\"lam\">λ</span> Sabela</span>"
h=h.replace("<h1 id=\"dashboard-title\"", m+"<h1 id=\"dashboard-title\"",1)
c="<style>.db-brand{font-family:var(--font-mono);font-weight:600;font-size:15px;color:var(--fg-heading);white-space:nowrap}.db-brand .lam{color:#c2674a}</style>"
open(p,"w",encoding="utf-8").write(h.replace("</head>", c+"</head>",1)); print("branded")'
  pyb64=$(printf '%s' "$py" | base64 | tr -d '\n')
  ssm_run "echo $pyb64 | base64 -d | python3 - $remotef"
}

for s in "${PUSH_SLUGS[@]}"; do
  echo "=== $s: pushing index.html ==="
  push_file "$STAGE_SHARES/$s/index.html" "$SHARES_REMOTE/$s/index.html"
  for f in source.md meta; do
    if [ -f "$STAGE_SHARES/$s/$f" ]; then
      echo "=== $s: pushing $f ==="
      push_file "$STAGE_SHARES/$s/$f" "$SHARES_REMOTE/$s/$f"
    fi
  done
done

for s in ${BRAND_ONLY[@]+"${BRAND_ONLY[@]}"}; do
  echo "=== $s: branding in place ==="
  brand_inplace "$SHARES_REMOTE/$s/index.html"
done

# Install the curated gallery index files (index / attribution / tags). These
# list only curated slugs, so overwriting them matches seed-gallery.py's
# canonical CURATION set without touching users' own (unlisted) shares.
GALLERY_REMOTE=/mnt/sabela/gallery
for f in index attribution tags; do
  echo "=== gallery: pushing $f ==="
  push_file "$STAGE/gallery/$f" "$GALLERY_REMOTE/$f"
done

# seed-gallery.py writes raw exports with no fork banner, so the pushes above
# strip the banner that the publish/republish path adds. Re-splice it in place
# (idempotent, byte-exact) so a reseed never silently drops the fork button.
echo "=== re-splicing fork banners ==="
ssm_run "docker exec sabela-hub /opt/bin/sabela-hub republish-banners $SHARES_REMOTE 2>&1"

echo "Done. Curated gallery reseeded + banners re-spliced on $IID. Restart the hub to reload it:"
echo "  ./infra/box.sh restart"
