#!/bin/bash
# Claude Code's Notification hook fires for two distinct semantics under
# the same notification_type=permission_prompt: real action-required
# prompts ("Claude needs your permission to use X" / "Claude Code needs
# your approval for Y") AND a 60s-idle nudge ("Claude Code needs your
# attention").  Earlier versions of the script filtered the idle nudge
# out here in bash, but Claude Code also uses the "needs your attention"
# wording for some real permission prompts, which caused the tab to stay
# stuck on :thinking (red, no ❓) instead of flipping to :permission.
# We now write the sentinel unconditionally and let elisp decide whether
# to flip state, gating on the current :claude-state (see
# `claude-repl--on-permission-event' in sentinel.el).
LOGFILE=~/.claude/workspace-notifications/hook-debug.log
mkdir -p ~/.claude/workspace-notifications
INPUT=$(cat)
echo "$(date '+%H:%M:%S.%3N') [permission] raw_input=$INPUT" >> "$LOGFILE"
CWD=$(echo "$INPUT" | jq -r '.cwd')
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')
echo "$(date '+%H:%M:%S.%3N') [permission] parsed_cwd=$CWD session_id=$SESSION_ID" >> "$LOGFILE"
printf '%s\n%s\n' "$CWD" "$SESSION_ID" > ~/.claude/workspace-notifications/permission_prompt
