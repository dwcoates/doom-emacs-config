#!/bin/bash
LOGFILE=${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}/workspace-notifications/hook-debug.log
mkdir -p ${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}/workspace-notifications
INPUT=$(cat)
echo "$(date '+%H:%M:%S.%3N') [stop_$$] raw_input=$INPUT" >> "$LOGFILE"
CWD=$(echo "$INPUT" | jq -r '.cwd')
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')
echo "$(date '+%H:%M:%S.%3N') [stop_$$] parsed_cwd=$CWD session_id=$SESSION_ID" >> "$LOGFILE"
printf '%s\n%s\n' "$CWD" "$SESSION_ID" > ${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}/workspace-notifications/stop_$$
