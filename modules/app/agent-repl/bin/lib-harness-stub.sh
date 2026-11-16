# shellcheck shell=bash
# lib-harness-stub.sh: the one way a script harness puts a recording stub on
# PATH and reads what the stubs ran.
#
# A case sets CASE to its own directory and makes "$CASE/stubs" (which the
# script under test finds first on PATH, with STUB_DIR naming it). Every stub
# appends "NAME ARGV" to "$CASE/transcript" before running its body.
#
#   write_stub NAME BODY   a stub NAME that records its argv, then runs BODY
#   ran TEXT               succeeds when some recorded line contains TEXT

write_stub() {
  printf '#!/usr/bin/env bash\nprintf "%%s %%s\\n" "%s" "$*" >>"$STUB_DIR/../transcript"\n%s\n' "$1" "$2" >"$CASE/stubs/$1"
  chmod +x "$CASE/stubs/$1"
}

ran() { grep -qF -- "$1" "$CASE/transcript"; }
