#!/bin/bash
# Claude Code's PermissionRequest hook fires the moment a permission
# dialog appears — BEFORE the user responds.  That's the real-time
# signal we want for the tab-bar `:permission' state.
#
# Contrast with the Notification hook (notification_type=permission_prompt),
# which dispatches when Claude Code SENDS A NOTIFICATION about the prompt
# — that signal can lag the dialog appearance (and, historically, fires
# the 60s-idle "Claude Code needs your attention" nudge under the same
# type), so by the time it arrives the user may already have answered
# and the tab flips yellow AFTER the response instead of when Claude is
# actually waiting.
#
# We just write a sentinel file unconditionally and let elisp gate
# the state transition on the current :claude-state (see
# `claude-repl--on-permission-event' in sentinel.el).  No hookSpecificOutput
# is emitted, so the hook is purely informational — Claude Code's normal
# permission flow continues.
LOGFILE=${CLAUDE_REPL_STATE_DIR:-$HOME/.claude-emacs}/workspace-notifications/hook-debug.log
mkdir -p ${CLAUDE_REPL_STATE_DIR:-$HOME/.claude-emacs}/workspace-notifications
INPUT=$(cat)
echo "$(date '+%H:%M:%S.%3N') [permission_request] raw_input=$INPUT" >> "$LOGFILE"
CWD=$(echo "$INPUT" | jq -r '.cwd')
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')
echo "$(date '+%H:%M:%S.%3N') [permission_request] parsed_cwd=$CWD session_id=$SESSION_ID" >> "$LOGFILE"
printf '%s\n%s\n' "$CWD" "$SESSION_ID" > ${CLAUDE_REPL_STATE_DIR:-$HOME/.claude-emacs}/workspace-notifications/permission_request
