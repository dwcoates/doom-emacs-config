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
# to flip state, gating on the current :agent-state (see
# `agent-repl--on-permission-event' in sentinel.el).
LOGFILE=${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}/workspace-notifications/hook-debug.log
mkdir -p ${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}/workspace-notifications
INPUT=$(cat)
echo "$(date '+%H:%M:%S.%3N') [permission] raw_input=$INPUT" >> "$LOGFILE"
CWD=$(echo "$INPUT" | jq -r '.cwd')
SESSION_ID=$(echo "$INPUT" | jq -r '.session_id // empty')
echo "$(date '+%H:%M:%S.%3N') [permission] parsed_cwd=$CWD session_id=$SESSION_ID" >> "$LOGFILE"
# Ownership marker (sentinel line 3): module-launched CLIs carry
# AGENT_REPL_OWNED=1 (vterm start command, daemon shim), and every
# sandbox session is module-launched by definition (DOOM_SANDBOX=1).
# Foreign sessions (e.g. a terminal claude in the same cwd) leave it
# blank, and Emacs then refuses to adopt their session ids.
OWNED=""
if [ -n "$AGENT_REPL_OWNED" ] || [ "$DOOM_SANDBOX" = "1" ]; then OWNED="owned"; fi
printf '%s\n%s\n%s\n' "$CWD" "$SESSION_ID" "$OWNED" > ${AGENT_REPL_STATE_DIR:-$HOME/.claude-emacs}/workspace-notifications/permission_prompt
