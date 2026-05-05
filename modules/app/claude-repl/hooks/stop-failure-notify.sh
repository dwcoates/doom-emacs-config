#!/bin/bash
# StopFailure hook: fires when Claude's turn ends due to an API error
# (rate_limit, authentication_failed, oauth_org_not_allowed, billing_error,
# invalid_request, server_error, max_output_tokens, unknown).  We capture
# the error_type as the third sentinel-file line so the Emacs side can
# render or surface it later (today the consumer ignores it and only
# uses the file's existence to flip :claude-state to :stop-failed).
LOGFILE=~/.claude/workspace-notifications/hook-debug.log
mkdir -p ~/.claude/workspace-notifications
INPUT=$(cat)
echo "$(date '+%H:%M:%S.%3N') [stop_failure_$$] raw_input=$INPUT" >> "$LOGFILE"
CWD=$(echo "$INPUT" | jq -r '.cwd')
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')
ERROR_TYPE=$(echo "$INPUT" | jq -r '.error_type // empty')
echo "$(date '+%H:%M:%S.%3N') [stop_failure_$$] parsed_cwd=$CWD session_id=$SESSION_ID error_type=$ERROR_TYPE" >> "$LOGFILE"
printf '%s\n%s\n%s\n' "$CWD" "$SESSION_ID" "$ERROR_TYPE" > ~/.claude/workspace-notifications/stop_failure_$$
