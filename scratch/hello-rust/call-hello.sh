#!/usr/bin/env bash
set -euo pipefail

URL="${URL:-http://127.0.0.1:7878/hello}"
OUT="${OUT:-hello.out}"

curl -fsS "$URL" -o "$OUT"
printf '\n' >> "$OUT"
echo "wrote $(wc -c < "$OUT" | tr -d ' ') bytes to $OUT"
