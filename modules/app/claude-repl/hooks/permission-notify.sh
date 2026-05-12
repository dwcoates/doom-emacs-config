#!/bin/bash
# Claude Code's Notification hook fires for two distinct semantics under
# the same notification_type=permission_prompt: real action-required
# prompts ("Claude needs your permission to use X" / "Claude Code needs
# your approval for Y") AND a 60s-idle nudge ("Claude Code needs your
# attention").  Treating the idle nudge as :permission causes the tab's
# ❓ to appear after the user has already responded (or never had to),
# so we filter on .message and only write the sentinel for real prompts.
LOGFILE=~/.claude/workspace-notifications/hook-debug.log
mkdir -p ~/.claude/workspace-notifications
INPUT=$(cat)
echo "$(date '+%H:%M:%S.%3N') [permission] raw_input=$INPUT" >> "$LOGFILE"
MESSAGE=$(echo "$INPUT" | jq -r '.message // empty')
case "$MESSAGE" in
  "Claude needs your permission to use "*|"Claude Code needs your approval for "*)
    ;;
  *)
    echo "$(date '+%H:%M:%S.%3N') [permission] SKIPPED non-permission message=$MESSAGE" >> "$LOGFILE"
    exit 0
    ;;
esac
CWD=$(echo "$INPUT" | jq -r '.cwd')
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')
echo "$(date '+%H:%M:%S.%3N') [permission] parsed_cwd=$CWD session_id=$SESSION_ID" >> "$LOGFILE"
printf '%s\n%s\n' "$CWD" "$SESSION_ID" > ~/.claude/workspace-notifications/permission_prompt
