#!/bin/bash
LOGFILE=~/.claude/workspace-notifications/hook-debug.log
mkdir -p ~/.claude/workspace-notifications
INPUT=$(cat)
echo "$(date '+%H:%M:%S.%3N') [prompt_submit_$$] raw_input=$INPUT" >> "$LOGFILE"
CWD=$(echo "$INPUT" | jq -r '.cwd')
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')
echo "$(date '+%H:%M:%S.%3N') [prompt_submit_$$] parsed_cwd=$CWD session_id=$SESSION_ID" >> "$LOGFILE"
printf '%s\n%s\n' "$CWD" "$SESSION_ID" > ~/.claude/workspace-notifications/prompt_submit_$$
