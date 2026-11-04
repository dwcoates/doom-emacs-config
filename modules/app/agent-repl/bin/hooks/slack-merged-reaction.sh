#!/usr/bin/env bash
# slack-merged-reaction.sh — the built-in post_deploy hook of an emoji-ack
# workspace (repoconfig.v1.LifecycleConfig): add the :merged: reaction to every
# Slack message that announces a pull request this workspace's work merged.
#
# THE MESSAGES ARE FOUND THROUGH THE WORKSPACE'S STORED LINKS ALONE. The finish
# stage's default stores each Slack post saying a pull request was created as
# an owned link announcing that pull request
# (agentrepl.v1.WorkspaceLinkOwned.announces); this reads the links back with
# `agent-repld workspace status` and never searches the thread.
#
# A PULL REQUEST GITHUB DOES NOT REPORT MERGED is left unreacted, with a line
# saying so: a repository whose deploy does not wait for the merge reaches
# post_deploy before it. A message already carrying the reaction is no
# failure, so the hook bears running again.
#
#   slack-merged-reaction.sh    run by the daemon as a script hook, with
#                               AGENT_REPL_CALLING_WORKSPACE set
#
# Exit status: 0 every merged announcement carries the reaction (or there was
# none); 1 a read or a reaction failed.
set -euo pipefail

readonly EMOJI="merged"

say() { echo "[slack-merged-reaction] $*"; }
die() {
  echo "[slack-merged-reaction] $*" >&2
  exit 1
}

workspace="${AGENT_REPL_CALLING_WORKSPACE:-}"
[ -n "$workspace" ] || die "AGENT_REPL_CALLING_WORKSPACE is unset; the daemon runs this as a workspace's hook"

status="$(agent-repld workspace status "$workspace")" || die "could not read the links of $workspace"
# Every owned link announcing another, as "<permalink><TAB><announced url>".
announcements="$(jq -r '.. | objects | select(.ownership?.owned?.announces?.url? != null)
  | [.url, .ownership.owned.announces.url] | @tsv' <<<"$status")" ||
  die "the status of $workspace is not the JSON agent-repld answers"

if [ -z "$announcements" ]; then
  say "no stored Slack message of $workspace announces a pull request; nothing to react on"
  exit 0
fi

# slack_message PERMALINK — print "<channel> <ts>" of a Slack message link
# (https://<workspace>.slack.com/archives/<channel>/p<ts digits>[?...]).
slack_message() {
  local path="${1#https://*.slack.com/archives/}"
  path="${path%%\?*}"
  local channel="${path%%/*}" digits="${path#*/p}"
  [ "$path" != "$1" ] && [ -n "$channel" ] && [[ "$digits" =~ ^[0-9]{7,}$ ]] || return 1
  printf '%s %s.%s\n' "$channel" "${digits:0:${#digits}-6}" "${digits: -6}"
}

while IFS=$'\t' read -r permalink announced; do
  state="$(gh pr view "$announced" --json state --jq .state)" ||
    die "could not read whether $announced merged"
  if [ "$state" != "MERGED" ]; then
    say "$announced is $state, not merged; $permalink is left unreacted"
    continue
  fi
  message="$(slack_message "$permalink")" || die "$permalink announces $announced but is no Slack message link"
  read -r channel ts <<<"$message"
  if out="$(gns slack react --channel "$channel" --ts "$ts" --emoji "$EMOJI" 2>&1)"; then
    say "reacted :$EMOJI: on $permalink, which announces $announced"
  elif grep -qF already_reacted <<<"$out"; then
    say "$permalink already carries :$EMOJI:"
  else
    die "Slack refused the :$EMOJI: reaction on $permalink: $out"
  fi
done <<<"$announcements"
