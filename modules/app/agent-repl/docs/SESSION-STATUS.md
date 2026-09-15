# Session status — overhaul stream (compaction insurance)

Project lead works off master, merges agent branches --no-ff, deploys via
bin/deploy-all.sh (hot-reloads elisp). Updated live.

## LANDED on master (this stream)
- Pseudo-workspace delete-after-load (M-<n> off-by-one) — merged + deployed + live-verified.
- Proto foundation for reply-to-response (SelectResponse, SubmitPrompt.reference_response_feedid, WatchFeed FeedSelection carrier) — merged.
- Account routing by inode identity (os.SameFile) not byte-prefix — merged. Fixes case/normalization mis-route to wrong account. NOT yet deployed (daemon restart pending batch).
- Subagent restore-on-resume (open WatchAgent per spawned subagent on opening page; restores subagent conversations + detached terminals) — merged. NOT deployed yet.
- status.el overlong-docstring warning fix — merged.

## DONE, not merged
- reply-to-response WEBAPP (feat/reply-webapp): blue selected border, center-scroll+clamp, autoscroll suppression while active, return-to-bottom on clear. 4518 vitest green. HOLD merge until daemon piece + integration.
- reply-to-response ELISP (feat/reply-elisp): C-p/C-n :n-scoped nav, consecutive-escape (last-command based) warn/clear, submit carries reference_response_feedid. Batch ERT green. HOLD merge for daemon piece.

## RUNNING (background agents)
- reply-to-response DAEMON (feat/reply-daemon, agent af47a16e27b32bbb7): selection state, prev/next/wrap/clear, replace SelectResponse stub, push FeedSelection, submit prefix (owner wording). Worktree path uses session seg d5c97ccdec15.
- scroll intent-arm gate (fix/scroll-intent-arm, agent ad3a536d67e6c3ef0): replace spatial EDGE_PX gate with arm-by-pointermove-entry/pointerdown; scrolled-into-bubble no longer captures wheel.

## AWAITING OWNER DECISIONS (not dispatched)
1. Synthesized title fallback (when vendor ai-title absent):
   - CONFIRMED FINDING: vendor ai-title is CLI-version-gated — 2.1.270 emits it under SDK, 2.1.215–2.1.267 do not. The 4 fallback workspaces run 2.1.220 (no ai-title). NOT an SDK-vs-interactive thing.
   - Plan: precedence vendor-ai-title -> synthesized -> name. Digest = prompts since last /clear, or (if /compact) most-recent compact summary + prompts after. Daemon makes a cheap Haiku headless call (workspace's own account). Trigger at session start (no vendor title) + end-of-turn when digest changed (hash+debounce); reset on clear/compact.
   - Shim<->daemon boundary: LEAN dedicated shim RPC GatherTitleDigest(session) -> {boundary_kind, last_compact_summary?, prompts_since_boundary[]}. (Alt: daemon-only from streams it already watches.)
   - OWNER SPEC for the Haiku prompt: the summary MUST be a single short sentence, NO clauses or sentence-extension tricks (no emdashes, semicolons, commas).
   - DECISIONS NEEDED: (a) RPC vs daemon-only [lead: RPC]; (b) re-synth cadence end-of-turn-on-change [lead] vs boundary-only; (c) prompts-only vs prompts+light-context [lead: prompts + last compact summary].
2. Resume most-recent existing transcript on workspace open:
   - FINDING: agent-repl resumes only its own recorded session (wsm.db sessions.vendor_session_id) or starts fresh; NO on-disk discovery. iterm-1 got fresh empty vsid instead of continuing the rich external transcript.
   - Plan: at shim creation, probe $CLAUDE_CONFIG_DIR/projects/<cwd-slug>/*.jsonl (ACCOUNT-ROUTED config dir), pick newest by last-record timestamp, adopt via ResumeCold if newer than our record. Daemon resolves candidate vsid, hands to shim.
   - HAZARD: two writers on one transcript (shim engine/session.ts warns). Need idle guard.
   - DECISIONS NEEDED: (a) adopt-newest-if-newer-than-our-record [lead] vs only-when-no-record; (b) idle guard: refuse adopting a transcript modified within ~30-60s (assume live external writer)? [lead: yes].

## DISPATCHED THIS TURN (owner-ordered, minimal decisions)
- Footer usage: DROP the "usage unread" message; render only the most-recent READ usage figures with a live "N ago" duration since last read (e.g. "<figures> 10m 30s ago"). Needs additive proto field (last-read instant, epoch ms) so webapp ticks the age; drop unread sentence/arm in daemon activity.go + webapp expanded.ts/strip.ts.

## OWED (lead to diagnose personally — NOT delegated)
- Pre-existing detached-shell restore integration failures (~8, incl TestSessionStartedRestoredLiveWorkRoutesToTheRootFeed) — CONFIRMED failing on clean master (35s deadline). Same detached-work-restore family. Standing order forbids "pre-existing" pass; lead must diagnose + fix.
- Deploy batch: account-routing + subagent-restore merged but daemon not restarted/deployed yet.
