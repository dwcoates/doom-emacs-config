#!/usr/bin/env bash
# linux-vm.sh: the Linux VM agent-repl is tried on, and its smoke run.
#
# Usage:
#   linux-vm.sh create        create the VM, stopped, with no mounts
#   linux-vm.sh smoke [REV]   start the VM, give it REV (default HEAD), run the
#                             installer, which checks every requirement after
#                             installing it, then stop the VM
#   linux-vm.sh stop          stop the VM
#
# THE VM SHARES NO HOST FOLDER (owner ruling, 2026-11-15): a mount of the
# worktree held ~49,500 of the host's open files (node_modules) and filled its
# file table, which blocked the merge queue. The VM is created with no mounts,
# a smoke refuses a VM that has any, and the code reaches the VM as a commit
# pushed over Lima's ssh, so node_modules and uncommitted edits never cross.
#
# THE VM RUNS ONLY WHILE A SMOKE DOES: a smoke stops the VM on every exit,
# success, failure or interrupt.
#
# Environment:
#   AGENT_REPL_LINUX_VM   the Lima instance's name (default agent-repl-linux)
#   LIMA_HOME             Lima's directory (default ~/.lima)

set -euo pipefail

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
MODULE_DIR="$(cd "$THIS_DIR/.." && pwd -P)"
# shellcheck source=lib-grep-in.sh
. "$THIS_DIR/lib-grep-in.sh"
VM="${AGENT_REPL_LINUX_VM:-agent-repl-linux}"
LIMA_DIR="${LIMA_HOME:-$HOME/.lima}/$VM"
# The VM's checkout sits where a Linux user's would, so the installer runs from
# a main checkout, the one place it installs services from.
VM_REPO=.config/doom
VM_MODULE="$VM_REPO/modules/app/agent-repl"

# Bounds, each two to three times the healthy time of what it bounds, measured
# on an M-series Mac with the VM at 8 CPUs and 12 GiB (2026-11-16):
#   START    limactl start of the stopped VM until ssh answers: 13s.
#   INSTALL  requirements.sh install, which rebuilds Emacs whenever its pin
#            moves: a cold Emacs build (autogen, configure, make -j8 with
#            native compilation) took 314s, and the rest of an install whose
#            packages are present took 95s. A fresh VM's first package
#            installs are not measured.
START_BOUND=40s
INSTALL_BOUND=1200
# A dead connection is noticed after ServerAliveInterval x ServerAliveCountMax.
SSH_OPTS=(-F "$LIMA_DIR/ssh.config" -o ConnectTimeout=10 -o ServerAliveInterval=5 -o ServerAliveCountMax=3)
SSH_HOST="lima-$VM"

# The one record of this script's steps and failures.
log() { printf 'linux-vm: vm=%s %s\n' "$VM" "$*" >&2; }
die() { log "outcome=failed $*"; exit 1; }

usage() { sed -n '3,8p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//' >&2; exit 2; }

# The listing is captured whole before grep reads it: a grep that matches early
# would kill a writer piped into it (bin/lib-grep-in.sh).
vm_exists() {
  local names
  names="$(limactl list --quiet)" || die "step=list cause=\"limactl list failed\""
  grep_in "$names" -qx "$VM"
}

# The VM's configured mounts, one per line: none answers nothing.
vm_mounts() {
  limactl list --json "$VM" | jq -r '.config.mounts // [] | .[] | .location'
}

vm_ssh() { ssh "${SSH_OPTS[@]}" "$SSH_HOST" "$@"; }

# Runs COMMAND in the VM's module checkout in a login shell (so ~/.local/bin is
# on PATH), killed by the VM's own timeout after BOUND seconds.
vm_run() {
  local bound="$1" step="$2" rc=0
  shift 2
  vm_ssh "cd $VM_MODULE && timeout --kill-after=10 $bound bash -lc $(printf '%q' "$*")" || rc=$?
  if [ "$rc" -eq 124 ] || [ "$rc" -eq 137 ]; then
    die "step=$step cause=\"timed out after ${bound}s\""
  fi
  [ "$rc" -eq 0 ] || die "step=$step exit=$rc"
}

cmd_create() {
  vm_exists && die "step=create cause=\"the VM already exists\" dir=$LIMA_DIR"
  limactl create --name="$VM" --tty=false --vm-type=vz --cpus=8 --memory=12 \
    --mount-none --containerd=none template:ubuntu ||
    die "step=create cause=\"limactl create failed\""
  log "step=create outcome=created mounts=none"
}

cmd_stop() {
  local status
  status="$(limactl list --json "$VM" | jq -r .status)"
  if [ "$status" = Stopped ]; then
    log "step=stop outcome=already-stopped"
    return 0
  fi
  limactl stop "$VM" || die "step=stop cause=\"limactl stop failed\" status=$status"
  log "step=stop outcome=stopped"
}

cmd_smoke() {
  local rev="${1:-HEAD}" commit mounts
  commit="$(git -C "$MODULE_DIR" rev-parse --verify "$rev^{commit}")" ||
    die "step=resolve rev=$rev cause=\"not a commit\""
  vm_exists || die "step=start cause=\"no such VM; run linux-vm.sh create\""
  mounts="$(vm_mounts)"
  [ -z "$mounts" ] || die "step=start cause=\"the VM shares host folders\" mounts=\"$(echo "$mounts" | tr '\n' ' ')\""
  trap cmd_stop EXIT
  limactl start --tty=false --timeout="$START_BOUND" "$VM" ||
    die "step=start cause=\"limactl start failed\" bound=$START_BOUND"
  log "step=start outcome=running"

  # A VM that has never had a checkout gets an empty repository to push into.
  if ! vm_ssh "git -C $VM_REPO rev-parse --git-dir" >/dev/null 2>&1; then
    vm_ssh "git init -q $VM_REPO" || die "step=init cause=\"git init failed\""
  fi
  GIT_SSH_COMMAND="ssh ${SSH_OPTS[*]}" \
    git -C "$MODULE_DIR" push --quiet --force "$SSH_HOST:$VM_REPO" "$commit:refs/heads/smoke" ||
    die "step=push commit=$commit"
  vm_ssh "git -C $VM_REPO checkout --quiet --force --detach $commit" ||
    die "step=checkout commit=$commit"
  log "step=push outcome=checked-out commit=$commit"

  vm_run "$INSTALL_BOUND" install bin/requirements.sh install
  log "step=install outcome=ok commit=$commit"
}

[ $# -ge 1 ] || usage
case "$1" in
  create) [ $# -eq 1 ] || usage; cmd_create ;;
  smoke) [ $# -le 2 ] || usage; shift; cmd_smoke "$@" ;;
  stop) [ $# -eq 1 ] || usage; cmd_stop ;;
  *) usage ;;
esac
