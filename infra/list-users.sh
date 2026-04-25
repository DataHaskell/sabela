#!/usr/bin/env bash
set -euo pipefail

# Lists all users who have signed in to Sabela.

source "$(dirname "$0")/.env"

aws logs filter-log-events --log-group-name /ecs/sabela-hub \
  --filter-pattern "Started task" \
  --region "$AWS_REGION" \
  --query 'events[*].message' --output text | tr '\t' '\n' | \
  sed -n 's/.*for //p' | sort -u
