# Post-deploy remediation (2026-09-29) — READ THIS WHOLE FILE AFTER COMPACTION

Written before a context compaction. After compaction, re-read this file, then
WAIT for the owner's go-ahead before starting.

## Where things stand

- Both earlier workstreams are on local `master` (tip `c5598cdb6`):
  daemon-owned desktop notifications, and the footer `working` status with the
  quiet-stretch line. Full `bin/test-all.sh` was green (26 suites, `e2e` and
  `e2e-emacs` included). Deployed with `daemon/bin/claude-repld deploy`
  (unforced; 7 workspaces handed over to successor pid 54811; sidecar
  restarted; elisp reload pushed). Emacs restarted (pid 55349), linked, and
  reporting focus.
- Branch `workspace-turn-notifications` (this worktree) was reset to `master`
  (identical tree), so new work cherry-picks cleanly.
- Docker was quit; `e2e-emacs` needs it — start Docker Desktop
  (`open -a Docker`, wait for `docker info`) before the final test run, quit it
  after (`osascript -e 'quit app "Docker"'`; if it hangs, `pkill -f Docker.app`).

## How the work runs (owner rulings, 2026-09-29)

- ALL of the items below are fixed IN THIS WORKSPACE, on the ONE branch
  `workspace-turn-notifications`. Do the work myself (no implementation
  subagents).
- When every item is done and ALL tests pass (`bin/test-all.sh`, every suite,
  `e2e-emacs` included), PROCEED AUTOMATICALLY: cherry-pick the branch's
  commits onto local `master` (`git -C ~/.config/doom cherry-pick
  master..workspace-turn-notifications`; the merge queue stays bypassed), then
  deploy (`~/.config/doom/modules/app/agent-repl/daemon/bin/claude-repld
  deploy`, unforced) and restart Emacs (check for unsaved file buffers first;
  `emacsclient --eval '(kill-emacs)'`, then `open -a Emacs`; emacsclient is
  `/Applications/Emacs.app/Contents/MacOS/bin/emacsclient`). Then read the
  logs since the deploy (`bin/logs.sh --all --since 10m --level warn --tally
  --sample 2`) — the deploy must be clean.
- Standing rules still apply: atomic commits, unit tests for every change and
  every error path, canonical logging, structural fixes over probabilistic
  ones, consolidation sweep before landing, changelog lines in
  `docs/REMEDIATION-CHANGELOG.md`.

## The items

### A. `watch_agent` refused on a shim the daemon already asked to stand down (owner: fix)

Evidence (successor daemon, right after the handover, one per busy/stale shim,
6 workspaces):

```
17:15:29.085 ERROR daemon.shimclient.watch_agent  shim stream refused
  {"error": "invalid_argument: protocol error: incomplete envelope: unexpected EOF"}
17:15:29.085 ERROR daemon.sessionwatcher.watch_agent  a standing stream ended without the session ending
  {"detail": "WatchAgent could not be opened", "error": "shimclient: watch_agent stream refused: ...unexpected EOF",
   "shim_pid": 54657, "shim_reaped": false, "stand_down_asked": true, "workspace": "0fc93df488234fea"}
```

- Every record carries `stand_down_asked: true`: the successor had already
  asked the stale shim to stand down (the deploy's shim bounce) and then a
  WatchAgent open raced that stand-down and hit EOF.
- To decide: is it a mis-levelled record of an EXPECTED end (the daemon asked
  for it), or a real ordering defect (a watch opened on a shim already told to
  stand down)? Prefer the structural answer: a watch on a shim the daemon has
  asked to stand down should never be opened (or its end is classified by the
  stand-down, not by timing). Read `daemon/internal/sessionwatcher` (where
  `stand_down_asked` is set and where `watch_agent` ERROR is recorded) and
  `daemon/internal/shimclient` (`watch_agent` refusal).

### B. `daemon.rollout.bounce` records a normal hand-off as a failure (owner: fix the warning)

```
17:15:28.781 ERROR daemon.rollout.bounce  the shim bounce failed; the workspace is served as it was
  {"cause": "bounce: handed across; the daemon the workspace moved to runs the replacement after its adoption",
   "force": false, "reason": "restart_verb", "workspace": "cd49d7e840044642"}
```

- The cause says the bounce was HANDED to the successor, which runs it after
  adoption — normal operation during a deploy, not a failure. Owner: "sounds
  like the warning incorrectly surfacing during normal operation". Make the
  hand-across a distinct, non-error outcome (typed, not string-matched) and
  record it at INFO; keep real bounce failures at ERROR.

### C. Closing/killing/nuking a workspace: roster everywhere, and an instant tab close (owner ruling)

Evidence that triggered it: after the handover Emacs re-registered two
workspaces whose worktrees no longer exist, and the successor refused them:

```
17:15:29.053 ERROR emacs elisp.host.register-refused dir=".../doom-worktrees/agent-repl-perf" error=(:cause (:arm :not-a-worktree :value nil))
17:15:29.053 ERROR emacs elisp.host.promotion-register-failed ws=agent-repl-perf ... address="127.0.0.1:62511"
(same pair for response-duration-tick)
17:15:28.838 WARN  emacs elisp.core.log-central-fallback workspace="response-duration-tick" reason=agent-repl log routing invariant violated: ...
```

Owner requirements (verbatim intent):

1. The ROSTER is successfully updated after closing / killing / nuking a
   workspace, and that is reflected EVERYWHERE — webapp, daemon, Emacs. (A
   workspace whose worktree is gone must not linger in Emacs's registry and be
   re-registered on a promotion.)
2. Emacs FIRST dispatches the workspace request (close/kill/nuke) to the
   daemon, the daemon IMMEDIATELY ACKS, and then Emacs IMMEDIATELY CLOSES THE
   WORKSPACE'S TAB. Today Emacs waits until the whole closure is done in the
   daemon, which is a very noticeable delay; the tab closing must be nearly
   immediate.

To investigate: the close/kill/nuke verbs in `lisp/` (verbs, workspace.el,
host.el promotion register path), the daemon's close/kill/nuke rpc answers
(does the proto answer before the teardown finishes? if not, the proto may
need an ack-then-progress shape — protobuf changes go through the
create-or-update-protobufs flow; no new RPCs unless required), and how the
roster push removes the row. Also why `agent-repl-perf` /
`response-duration-tick` stayed in Emacs's registry (they were merged/removed
earlier by other sessions).

### D. Webapp `feed.final-answer-row-absent` (owner: investigate)

```
17:16:23.789 WARN webapp feed.final-answer-row-absent  the concluded turn names an answering row this feed has not drawn
  {"answer": "f1.ODExMDA5ODVlMzgzNDM1Mh9yHx9hY3Rpdml0eR9tc2dfMDExQ2ZZN3BrS1VhNGNnUVc2ZVlQMURROjEf",
   "workspace_dir": ".../doom-worktrees/prompt-bubble-height", "workspace_id": "81100985e3834352"}
```

- Fired right after Emacs restarted (fresh webviews, precreated on focus),
  after a handover. Likely: the webview drew the turn-ended row before the
  page that holds the answering response row (replay/page ordering after a
  fresh connect), or the feed page omitted the row. Read the webapp's
  final-answer resolution (`grep -rn final-answer-row-absent webapp/src`) and
  the daemon's feed paging on a fresh WatchFeed/OpenFeed.

### E. On Emacs startup the first (auto-selected) workspace has no agent-repl panel (owner: new)

- When Emacs opens, the automatically selected workspace (the first one) does
  not show the agent-repl panel until the user switches away and back. It must
  already be open by the time the user sees the workspace.
- Log context from the restart: `elisp.webview-recovery.precreate-created
  ws=<name> reason=focused` records for several workspaces at 17:16:22 — look
  at the startup path (config.el load order, the landing / panel-open on
  workspace activation, `agent-repl-frontend-open-panel`, persp activation
  hooks) for why the initially selected workspace never gets its panel opened.

## Order

A and B (daemon logging/ordering) → C (roster + instant tab close; may touch
proto, daemon, lisp, webapp) → D → E → consolidation sweep → full test-all
(Docker up) → cherry-pick onto master → deploy → restart Emacs → verify logs →
quit Docker → report.
