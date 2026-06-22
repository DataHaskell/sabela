#!/usr/bin/env bash
set -euo pipefail
# Reseed the curated gallery on the prod box's EFS (/mnt/sabela/shares) over SSM.
# Pushes the five curated single-share dashboards AND the 14-chapter "Learn You a
# Haskell" collection. Additive at the share level (only these slugs' files are
# touched, never other users' shares); the gallery index/attribution/tags ARE
# overwritten to the canonical curated+collection set.
#
# The curated dashboards come from the legacy python seeder (unchanged path).
# The LYAH chapters come from the Haskell seeder (`sabela-hub seed-gallery`),
# which already splices the fork banner + WASM MicroHs runner into each chapter,
# so they need no runner backfill.
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
GALLERY_REMOTE=/mnt/sabela/gallery

# The LYAH chapter slugs (1ea40001..1ea40014), seeded as collection 1ea40000.
LYAH_SLUGS=(); for i in $(seq -w 1 14); do LYAH_SLUGS+=("1ea400$i"); done
LYAH_CID=1ea40000

# Slugs pushed in full (content may have changed since the last seed).
PUSH_SLUGS=(c56a0001 b1ef0001 f12a0001 c0de0001 ca1f0001)
# Slugs only needing the brand mark in place (content stable, too large to
# re-upload). Empty for now; california is re-shelled so it is pushed in full.
BRAND_ONLY=()

IID=$(aws autoscaling describe-auto-scaling-groups --auto-scaling-group-names sabela-box \
  --query 'AutoScalingGroups[0].Instances[0].InstanceId' --output text --region "$AWS_REGION")
[ -n "$IID" ] && [ "$IID" != "None" ] || { echo "no running box in ASG sabela-box" >&2; exit 1; }

# Regenerate the gallery into temp data roots so the pushed files are current.
# STAGE: legacy python seeder (curated dashboards). HSTAGE: Haskell seeder
# (curated + LYAH chapters + the collection + a collection-aware gallery index).
STAGE=$(mktemp -d)
HSTAGE=$(mktemp -d)
trap 'rm -rf "$STAGE" "$HSTAGE"' EXIT
python3 "$ROOT/sabela-hub/scripts/seed-gallery.py" "$STAGE" >/dev/null
STAGE_SHARES="$STAGE/shares"

echo "=== building the Haskell seeder ==="
(cd "$ROOT/sabela-hub" && cabal build -v0)
HUB_BIN=$(cd "$ROOT/sabela-hub" && cabal list-bin sabela-hub)
"$HUB_BIN" seed-gallery "$HSTAGE" "$ROOT" >/dev/null

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

# Push the LYAH chapters from the Haskell stage. These already carry the fork
# banner + WASM runner (spliced by `sabela-hub seed-gallery`), so no backfill.
for s in "${LYAH_SLUGS[@]}"; do
  echo "=== $s: pushing LYAH chapter ==="
  for f in index.html source.md meta; do
    push_file "$HSTAGE/shares/$s/$f" "$SHARES_REMOTE/$s/$f"
  done
done
echo "=== gallery: pushing collection $LYAH_CID ==="
push_file "$HSTAGE/gallery/collections/$LYAH_CID/meta" "$GALLERY_REMOTE/collections/$LYAH_CID/meta"

# Install the gallery index files (index / attribution / tags) from the Haskell
# stage so the feed lists the five curated shares AND the LYAH collection. These
# list only curated/collection slugs, so overwriting them never touches users'
# own (unlisted) shares.
for f in index attribution tags; do
  echo "=== gallery: pushing $f ==="
  push_file "$HSTAGE/gallery/$f" "$GALLERY_REMOTE/$f"
done

# seed-gallery.py writes raw exports with no fork banner, so the pushes above
# strip the banner that the publish/republish path adds. Re-splice it in place
# (idempotent, byte-exact) so a reseed never silently drops the fork button.
echo "=== re-splicing fork banners ==="
ssm_run "docker exec sabela-hub /opt/bin/sabela-hub republish-banners $SHARES_REMOTE 2>&1"

echo "Done. Curated gallery + LYAH collection reseeded, banners re-spliced on $IID."
echo "Restart the hub to reload it:"
echo "  ./infra/box.sh restart"
