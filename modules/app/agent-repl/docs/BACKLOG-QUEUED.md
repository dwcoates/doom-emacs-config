# Agent-repl — queued backlog (owner-held)

Items the owner has asked for or approved but told the lead to HOLD (not dispatch
yet). Kept here so they survive compaction. Remove an item when it lands.

Last updated: 2026-09-16.

## Held features / fixes

1. **Footer auto-expand by activity.**
   - When async agents are running, the footer's async-agents section is
     auto-expanded (open).
   - When no async agents run but other tools are running, the tools section is
     auto-expanded instead.

2. **Purple prompt border not clearing (#4).**
   - The purple (live/working) border on a prompt bubble doesn't reliably clear
     on an ADOPTED last prompt (a conversation continued from Claude Code): the
     last prompt sent in Claude Code stays purple-bordered after subsequent
     prompts are sent.
   - Fix the data-wave="working" lifecycle for adopted prompts.

3. **Fork client feedback parity (#3).**
   - `SPC TAB f` (fork) doesn't give the same client (Emacs) open-progress
     feedback that create (`SPC TAB C-n`) does. Make fork emit the same
     open-progress feedback as create (daemon create/port lifecycle).

4. **Pseudo-workspace M-<n> off-by-one live-verify.**
   - Branch `fix/delete-pseudo-workspaces-after-load` is written + batch-green but
     UN-MERGED and never live-verified. Deletes persp-mode's "main" pseudo
     perspective after the first workspace loads so `M-<n>` maps to the right
     workspace. Needs: merge + deploy + LIVE verify (a startup-bringup event, so
     hot-load won't re-run it — verify by eval'ing the deletion fn live or after a
     restart, then check `persp-names` no longer has "main" and M-1 -> first real
     workspace).

## Optional / owner-steer

5. **Hook cards into the header-only collapse model.**
   - The tool-card header-only collapse currently covers tool-call/skill/subagent;
     hook cards were left on their per-section expand. Owner's "etc" may include
     hook. Small follow-up if wanted.

6. **Part-C keepalive polish (user turns never wait on a keepalive).**
   - The keepalive/user-turn collision is already fixed (A+B: the daemon
     re-drives a keepalive `turn_already_open` and keeps submitting status up).
     Part C would make the shim ABORT an in-flight keepalive when a real prompt
     arrives so a user turn never queues behind a keepalive at all. Deferred
     because a naive abort could let the keepalive's vendor `result` close the
     real turn (turn-accounting corruption); a safe design exists (route the
     abort through the normal close path with a bounded wait, degrading to A+B on
     timeout) — dispatch only if the owner wants the extra polish.

## Active-next (do the moment the final-answer-flag agent merges)

7. **Thinking-landing debug logging + turn debug on.**
   - Add daemon debug log at the thinking-row EMIT site (thinking.go) —
     `daemon.feed.thinking_emitted` (daemon currently logs only thinking
     deferred/withheld/fragment, never the emit).
   - Add webapp debug log where a thinking bubble is drawn/marked
     (feed/cards/response.ts where THINKING_BUBBLE_CLASS is applied) —
     `feed.draw-thinking`.
   - Turn debug on: `AGENT_REPL_LOG_LEVEL=debug` for daemon/shim/webapp; deploy.
   - Purpose (owner): check whether thinking responses ever land in the webapp —
     correlate `thinking_emitted` (daemon) vs `feed.draw-thinking` (webapp).
   - SEQUENCING: both files are edited by the in-flight final-answer-flag agent
     (fix/final-answer-on-row-data); do this right AFTER it merges to avoid
     conflict.
