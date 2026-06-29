#!/usr/bin/env bash
set -euo pipefail

# Lists all users who have signed in to Sabela, with the date they were last
# active and how many distinct days they have been active.
#
# Usage: list-users.sh [--csv]
#   --csv   emit comma-separated rows instead of an aligned table

source "$(dirname "$0")/.env"

csv=0
for arg in "$@"; do
  case "$arg" in
    --csv) csv=1 ;;
    *) echo "unknown argument: $arg" >&2; exit 2 ;;
  esac
done

aws logs filter-log-events --log-group-name /ecs/sabela-hub \
  --filter-pattern "Started task" \
  --region "$AWS_REGION" \
  --query 'events[*].[timestamp,message]' --output text | \
  awk -F'\t' -v csv="$csv" '
    # Civil date (UTC) from epoch seconds, per Howard Hinnant`s algorithm.
    function ymd(secs,    days, z, era, doe, yoe, y, doy, mp, d, m) {
      days = int(secs / 86400)
      z = days + 719468
      era = int((z >= 0 ? z : z - 146096) / 146097)
      doe = z - era * 146097
      yoe = int((doe - int(doe / 1460) + int(doe / 36524) - int(doe / 146096)) / 365)
      y = yoe + era * 400
      doy = doe - (365 * yoe + int(yoe / 4) - int(yoe / 100))
      mp = int((5 * doy + 2) / 153)
      d = doy - int((153 * mp + 2) / 5) + 1
      m = (mp < 10 ? mp + 3 : mp - 9)
      y = (m <= 2 ? y + 1 : y)
      return sprintf("%04d-%02d-%02d", y, m, d)
    }
    {
      ts = $1
      # The user id is the token after the last " for "; lines without one
      # (e.g. task-ARN-only log lines) are not sign-ins, so skip them.
      n = split($2, parts, / for /)
      if (n < 2) next
      user = parts[n]
      secs = int(ts / 1000)
      date = ymd(secs)
      if (!(user in last) || secs > lastsecs[user]) {
        lastsecs[user] = secs
        last[user] = date
      }
      key = user SUBSEP date
      if (!(key in seen)) {
        seen[key] = 1
        days[user]++
      }
    }
    END {
      if (csv)
        printf "USER,LAST ACTIVE,ACTIVE DAYS\n"
      else
        printf "%-30s  %-12s  %s\n", "USER", "LAST ACTIVE", "ACTIVE DAYS"
      n = 0
      for (u in last) users[n++] = u
      # Insertion sort keeps it dependency-free and the list is small.
      for (i = 1; i < n; i++) {
        v = users[i]
        for (j = i - 1; j >= 0 && users[j] > v; j--) users[j + 1] = users[j]
        users[j + 1] = v
      }
      for (i = 0; i < n; i++) {
        u = users[i]
        if (csv)
          printf "%s,%s,%d\n", u, last[u], days[u]
        else
          printf "%-30s  %-12s  %d\n", u, last[u], days[u]
      }
    }
  '
