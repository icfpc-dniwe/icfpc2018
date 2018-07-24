#!/usr/bin/env bash

set -euo pipefail

private_id=4e8ca3b1b1ed48fa929fded0efd59a1e
problems_zip=https://icfpcontest2018.github.io/assets/problemsF.zip
submit_url=https://script.google.com/macros/s/AKfycbzQ7Etsj7NXCN5thGthCvApancl5vni5SFsb1UoKgZQwTzXlrH7/exec
upload_url=abbradar.moe:/srv/http/nginx/me/icfpc2018
submission_url=https://abbradar.moe/me/icfpc2018
workers_num=4

workdir=$(mktemp -d)

# Get problems
curl -o "$workdir/problems.zip" -L "$problems_zip"
mkdir "$workdir/problems"
unzip -d "$workdir/problems" "$workdir/problems.zip"

# Solve
mkdir "$workdir/solutions"
cabal build solver
ls "$workdir/problems/"FA*.mdl | sed 's,_tgt.mdl,,' | parallel -j "$workers_num" --eta dist/build/solver/solver {}_tgt.mdl "$workdir/solutions/"{/}.nbt

# Submit
name="result-$(date +%s).zip"
submission="$workdir/$name"
cd "$workdir/solutions"
zip --encrypt --password "$private_id" "$submission" *
checksum="$(sha256sum "$submission" | cut -f 1 -d ' ')"
scp "$submission" "$upload_url/$name"

curl -L -f -v \
  --data-urlencode "action=submit" \
  --data-urlencode "privateID=$private_id" \
  --data-urlencode "submissionURL=$submission_url/$name" \
  --data-urlencode "submissionSHA=$checksum" \
  "$submit_url"
