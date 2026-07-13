#!/bin/bash
LOGFILE=${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}/workspace-notifications/hook-debug.log
mkdir -p ${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}/workspace-notifications
INPUT=$(cat)
echo "$(date '+%H:%M:%S.%3N') [stop_$$] raw_input=$INPUT" >> "$LOGFILE"
CWD=$(echo "$INPUT" | jq -r '.cwd')
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')
echo "$(date '+%H:%M:%S.%3N') [stop_$$] parsed_cwd=$CWD session_id=$SESSION_ID" >> "$LOGFILE"
# Ownership marker (sentinel line 3): module-launched CLIs carry
# AGENT_REPL_OWNED=1 (vterm start command, daemon shim), and every
# sandbox session is module-launched by definition (DOOM_SANDBOX=1).
# Foreign sessions (e.g. a terminal claude in the same cwd) leave it
# blank, and Emacs then refuses to adopt their session ids.
OWNED=""
if [ -n "$AGENT_REPL_OWNED" ] || [ "$DOOM_SANDBOX" = "1" ]; then OWNED="owned"; fi
printf '%s\n%s\n%s\n' "$CWD" "$SESSION_ID" "$OWNED" > ${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}/workspace-notifications/stop_$$
