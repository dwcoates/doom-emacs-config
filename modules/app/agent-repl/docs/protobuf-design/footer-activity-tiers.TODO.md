# TODO — footer-activity-updates, remaining work (written 2026-09-30, before a compaction)

Read this first after the compaction. Background and the full rules live in
`footer-activity-tiers.RESUME.md` (history, plans H1-H8, design notes) and
`footer-activity-tiers.md` ("THE PLAN", "Held-queue", "Detached-work
liveness", landed changes 1-4). Worktree
`~/.config/doom-worktrees/footer-activity-updates`, branch
`footer-activity-updates`. Work in-session, no implementation subagents, no
questions; commit per atomic unit, check EXIT CODES (`make test` hides a gofmt
failure).

The owner's interrupts in this session were ADDITIONS, never rejections: a
"tool use was rejected" message on an interrupt means "new info arrived",
not "stop".

## Done this stretch (all committed)

- e2e on the combined model; landed change 3 recorded.
- Footer percentages colored green <40 → yellow 70 → orange 90, red >=90.
- Detached-work liveness fix (shell run claims, waiting WatchBash,
  `vendor_moved` cause, sidecar claim pass, timeout sentence); landed change 4.
- Held-queue fix: acts are durable held entries, FIFO with prompts, judged
  only against the item ahead, coalescing into a still-queued prompt,
  tray act + coalesced badge, worked-example integration test.
- 4a: response bubble token figure colored by the shared token-heat rule;
  expanded items auto-collapse only once wholly out of view.
- 4c DONE: proto (`05360ec4d`), sidecar (`70aea48d5`), daemon footer
  (`e986ba0ab`), webapp countdown/overdue/restored (`40c589c13`), daemon
  integration (landed change 5).
- H6/H7 verdict split DONE: proto (`fa0bdd0e5`, `132d53b2d`), shim join
  (`5825dc3fa`) and vendor note (`e39c8a6fe`), wsm.CloseFolded + feed
  (`a65a1b2ff`), watcher (`da13f884c`), footer stage + tray (`73da3d1ad`),
  classifier + queue join (`947cc4342`), integration (`d6839a6a8`), webapp
  (`6dce4c2db`), interruption note (`893d3b353`). Extractions:
  TurnClose.String (`7f8afb030`), ClassificationArm.String (`d7cf4b927`).

## Remaining, in order

1. **4c network-interruption footer — FINISH (was in progress).**
   - Diagnosis (for the final report): the vendor's own `system:api_error`
     records show attempt 8 at 15:36:52 local promising the next try in 32s;
     no 9th attempt, response or result ever came until the owner's
     interrupt at 15:54:09. The line was not left uncleared after a
     recovery the system saw: the vendor HUNG. The daemon also self-counted
     one ahead of the vendor. Fixed: the vendor's attempt/limit/next-instant
     now ride `conversation.v1.ApiRequestFailed.retry` to
     `frontend.v1.FooterStatusActivityRetrying.{next_attempt,max_attempt}`,
     and the first response raises `FooterActivityTransientApiRestored`.
   - LEFT: webapp `src/footer/activity.ts`: draw the retrying line as
     "retrying · attempt 9 of 11 · next try in 12s · <status>", the
     countdown ticking off `next_attempt` (shared ticker, like the usage
     resets), and when past due "next try overdue by 2m" (makes a hung vendor
     visible). Draw the `api_restored` transient ("API answering again after
     8 failed attempts"). Exhaustive transient switch needs the new arm.
     Tests for each (Vitest), typecheck, lint, integration.
   - Check the daemon's transient-kind name switches/log (`activity_line
     _changed` kind names) cover `api_restored`; daemon integration test
     that a scheduled failure then a response yields the transient.
   - Record in the design record ("Landed changes 5").
2. **H6/H7 verdict split** (`queue` / `after_tool_call` / `interrupt`,
   unsure → `after_tool_call`). Design notes are in RESUME "4-PLAN" and
   "4b". Key fact from the live probe: the SDK folds a pushed user message
   into the running turn only at a TOOL BOUNDARY; with no tool call in flight
   it becomes the vendor's NEXT turn. So the new shim verb (StartTurn
   `fold_into_open_turn` or a `FoldPrompt` rpc) must handle both outcomes:
   folded (result `user_message_uuids` lists it) and not folded (the shim
   opens the next SDK turn under the prompt's turn id and origin; the daemon
   learns it as that turn, not a vendor-started adoption). `interrupt`
   verdict: attach a daemon note to the delivered prompt naming the
   classifier's reason (work was cut because this prompt changes it; follow
   it, do not stop). Classifier prompt/parsing to three verdicts
   (`internal/classifier`, incl. the `-fake` judge's markers). wsm already
   has `ArmAfterToolCall`. Proto: `frontend.v1.HeldPrompt` classification
   arm `after_tool_call` + badge; footer submitting stage arm for
   after-tool-call; tray projection (`resolve/holds/tray.go`, today its
   default branch logs an error for the arm). Tests incl. the worked
   example's last step ("actually also do X" → after_tool_call).
3. **Docs**: `AGENTS.md` (footer: four tiers, shared salient kinds, 80%
   rule, submitting stages, retry countdown/restored; prompt queue: acts are
   held entries, FIFO, judge-ahead, coalescing, verdict split; detached-work
   claims/waiting watch; auto-collapse visibility rule); webapp `AGENTS.md`;
   one line per landed fix in `docs/REMEDIATION-CHANGELOG.md` (combined
   model, compaction-failed clearing, cold-gate flash, composer height,
   toggle style, item centering, expanded ceiling, footer percent gradient,
   token-heat stamp, visible-only collapse, liveness fix, held-queue fix,
   retry countdown/restored, verdict split).
4. **Green everywhere, exit codes checked**: daemon `make test`,
   `make integration`; shim typecheck/lint/`npm test`/`test:integration`
   (AGENT_REPL_FORBID_VENDOR_CALLS=1); sidecar and store
   `go test -race ./...` via `bin/background.sh`; webapp
   typecheck/lint/test/test:integration/test:webkit; proto `make validate`;
   the touched e2e scenarios (see RESUME for the command) plus any e2e the
   held-queue / liveness changes touch.
5. **Consolidation sweep** over `git diff master..HEAD`; extractions as their
   own behavior-preserving commits with tests (done so far: percent gradient,
   token heat, visibility-fake test helper).
6. **Land through the merge queue** (`/merge-queue`,
   `.claude/skills/merge-queue/SKILL.md`), NOT cherry-pick (owner,
   2026-09-30). On park/fail/refusal report and stop.
7. **Bounce all systems**: `bin/build-frontend.sh` + restart claude-repld;
   build shim-store and shim-claude-sidecar into `~/.cache/agent-repl/bin` +
   `launchctl kickstart`; verify `bin/readiness-report.sh` and
   `scripts/agent-shim-doctor.sh`; hot-load `lisp/panels.el` into the main
   Emacs (never test-*.el).

## Leftovers for the final report (out of scope)

- All four resolved; see "Status, 2026-09-30 night" below.

## Next, after this branch (owner, 2026-09-30 evening)

A. **Merge queue analysis (no changes until reviewed with the owner).**
   Compare three things and list the differences:
   - how the daemon's merge queue works today;
   - how it was specified recently (about 2026-09-29; find it in the
     transcripts);
   - the owner's statement now. Once popped from the head of the queue, a
     merge runs: rebase onto master, conflict resolution, tests, test
     remediation, then merge. If master moved since the process started, it
     starts over. The footer shows a step for each, with activity updates.
   - A merge runs IN THE WORKSPACE THAT ASKED FOR IT, never a new dedicated
     workspace. Its item appears in that workspace's main feed, and its shim
     and agent context are reused, because that context is what makes
     conflict resolution and test remediation good. Several merges from one
     workspace are handled by telling the daemon not to close the workspace
     after the merge, not by making new workspaces.
B. **After a workspace closes** (killed, closed or nuked by the user, or
   implicitly, e.g. a completed merge), Emacs and the webapp select the LAST
   selected workspace, not the first one in the tab bar or sidebar.
   - Only when the user is looking at the closing workspace (owner, 2026-09-30).
     If they are on another workspace when it closes, the selection stays where
     it is: switching them away would disrupt what they are looking at.
   - How the workspace closed does not matter. There is one rule for an
     explicit close by the user and an implicit one such as a merge: if the
     closing workspace is the selected one, select the one selected before it;
     otherwise change nothing.
C. **The merge bubble's test log link.** The test failure log's path rides
   statically in the proto on the merge queue feed entry. The Tests tab draws
   it as a link, in the same blue as links elsewhere in response bubbles.
   Clicking it opens the file in Emacs in a horizontal split on the right,
   beside the agent-repl panels and not replacing them, the way other
   agent-repl file views open. Proto additions to the existing workspace rpc
   are pre-approved. Check whether the right-side split already exists.

## Rulings and decisions, 2026-09-30 late evening

The owner: "everything else you can implement as you see fit (with whatever
orchestration you like)"; the light theme is not to be worried about. The
lead's decisions on the open merge-queue questions (recorded here so they can
be revisited):

- **The rebase runs in a worktree checked out on the branch being merged**:
  the requesting workspace's own worktree when it merges its own branch; the
  branch's existing worktree when it has one (another workspace, a subagent's
  Agent-tool worktree); otherwise a worktree the daemon makes for that branch.
  The repair agent is ALWAYS the requesting workspace's own session, told
  which directory to work in. No workspace is ever created for a merge.
- **"merge workspace <dir>" means "merge that workspace's branch from here"**:
  the requesting workspace runs it and shows it; the workspace whose branch
  landed is closed once it lands, as its work is done.
- **"keep open after merge" is a flag on each merge request.** Without it, a
  workspace that merged its own branch closes on landing; a workspace that
  merged another branch is never closed by that merge.
- **Nothing acts on an overdue retry, by design** (metaprompt: never work
  around a broken mechanism with a fallback). The sad path is reported
  loudly: the footer shows "overdue", and the daemon records one WARN when a
  promised retry passes with no attempt.
- Orchestration: the lead does the merge queue rework (items A and C);
  opus-medium agents take item B, the shim's late-resume bug and the agent
  moved-by-patch cause, and the master duplication plus the overdue WARN; the
  explanation-engine test-skill failures go through /explanation-engine-skill.

## Status, 2026-09-30 night

Landed on master (cherry-picked):
- The enduring-line trim: no context-window line, only each `<number>%` colored, no reading age; the blue `|` between allowances.
- B, reselect after close, with the roster row's durable last-selected instant.
- Leftovers: the shim's late-resume bug, an agent moved by a patch announced `vendor_moved`, and the overdue-retry WARN.
- A live background item is re-announced to a newly attached daemon, and a revival decides survival by where the work ran.
- A settled run is never tracked again or concluded LOST (store.v1 GetRunSettlements).
- Two test races: the merge drain's admission count, and the adopted kept turn.

On the landing branch `merge-queue-rework`, waiting for the last two agents:
- A and C, the merge queue rework (proto, daemon, webapp, Emacs); the e2e rewrite is in progress on `mq-e2e`.
- The footer draws only the main agent's live work (owner ruling).
- Fold above for held prompts (FoldHeldPrompt).
- Workspace names in logs, and the metaprompt rules on memory changes and workspace names.
- The resumed subagent's footer identity is in progress on `resumed-agent-footer`.

Then: rebase the landing branch onto master, run every suite once in sequence at background priority, cherry-pick onto master. The owner deploys.

Dropped by the owner: repairing the store rows the sidecar wrongly concluded LOST, and queueing the explanation-engine PR.
