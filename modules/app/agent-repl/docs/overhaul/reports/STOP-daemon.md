# STOPPING POINT — daemon rebuild (overhaul/daemon), 2026-08-31

Written at the user-directed wind-down. A cold resume (possibly a fresh
lead) picks up from here without re-derivation. The governing documents are
`daemon/ARCHITECTURE.md` (package map, seams, every landed ruling — read it
FIRST and in full), `daemon/AGENTS.md` (flags, env contracts, test knobs,
conventions), `daemon/integration/SPEC.md` (integration suite spec),
`daemon/ERROR-ARMS.md` (refusal-arm ledger; 6 rows pending landing 6), and
`docs/overhaul/daemon.md` incl. its appended "Landing N relay" sections and
the recorded override.

## Branch state

- Branch `overhaul/daemon`, worktree `~/.config/doom-overhaul/daemon`.
- The stopping-point commit is the tip; its parent 1b20da30e is the last
  code merge. `cd modules/app/agent-repl/daemon && go build ./... && go vet
  ./... && go test ./...` is GREEN at 1b20da30e: 20 test packages ok.
- Landings 1–5 from overhaul/integration are merged (last: 081dbbba8 at
  merge 05b460c4b). The vocab files (render-colors.json trim +
  paint-classes.json + footer_allowance) are daemon-owned and landed.

## Merged and green (packages with implementation + tests)

ids, notimpl, envc, stateroot, publish, dlog (surfaces, run log, borrow
handle, mirror), daemonaddr, pprofsurface, wsm (16 tables incl. the durable
merge queue), sessionlock (probes), gitclient (incl. Commit; fake-git tests
only), shimclient (spawn/adopt/redial/verbs/streams), sessionwatcher (fleet,
connectivity, routing incl. landing-4 adaptations), resolve/feed,
resolve/footer, resolve/topbar (allowance sourcing per ruling),
resolve/sidebar, resolve/holds, drain (schedule/sweep/refusal
rate-limiting), rollout (trigger/handover/adopt rendezvous/relaunch/
manifest; Join/CheckStaleness/Reconcile), prompthandler? NO — see queued. workspace
(verbs/health/commandfile, typed arms), merge (orchestrator, durable queue,
both methods, briefs, test gate). Skeletons only (api.go + stubs): vocab,
paint, prompts, feedid (Encode/Decode still stubs — footer chip jump
targets are nil in production until it lands), account, login,
externalbrowser, promptqueue, classifier, drain, rollout, server, boot,
health is implemented, commandfile implemented; cmd/claude-repld parses
flags and exits 2 ("not wired yet").

## Worktrees left in place (NOT merged; unreported at the wind-down)

- ~/.config/doom-overhaul/daemon-agents/hostside — branch
  overhaul/daemon-hostside at 87e3ca5f1, 6 ahead, clean. account/login/
  externalbrowser; login landed (one pty per account root, scrollback);
  account porting + externalbrowser + remaining tests unverified.
- ~/.config/doom-overhaul/daemon-agents/integration-tests — branch
  overhaul/daemon-integration-tests at 438c81e12, 5 ahead, clean. Harness
  (daemon lifecycle, fakes, view watchers, log readers) + fakeshim landed;
  suites incomplete; MUST be converted per the no-real-git directive
  (harness fake `git` on the daemon's PATH; NewFakeRepo fixtures) — its
  last commit aligned the flag/env spellings.
- ~/.config/doom-overhaul/daemon-agents/paintvocab and .../promptflow —
  at the old base ab737aefc with no commits: DELETE these two worktrees and
  cut fresh ones from the tip when dispatching those briefs.

## Queued briefs, in dispatch order (each: opus tier per the fair-share
ruling then in force; every subagent works in its own worktree
`~/.config/doom-overhaul/daemon-agents/<slug>` on branch
`overhaul/daemon-<slug>` cut from the tip; commit after every compiling
step; no real git and no vendor calls in tests)

1. FEED REMEDIATION (small): (a) AgentBashOutput `not_observed` (and a
   not_observed interrupted output) → UNSET FeedShell.spool, never
   FeedShellSpool{text:""}, settled arm as recorded — pin with a test;
   (b) vendor-synthesized notices set FeedResponse.notice{heading} instead
   of a prose prefix; (c) extract the canonical token formatter to
   `internal/figures.Tokens` per ARCHITECTURE "The canonical token-figure
   format" (incl. the 999,950 → "1M" boundary and the pinned example
   table) and swap feed/footer/topbar onto it; (d) verify the
   `$ `-less command-input-text pin test landed (FeedToolCallInput.text ==
   the bare command line; the webapp draws the chrome) and add it if not.
2. MERGE REMEDIATION (small): UpdateMergeQueuePause/Resume gained
   `optional RepositoryRef repository` (UNSET = every repository); scope
   the pause flag per repo (wsm.SetMergeQueuePaused already keys by repo);
   typed refusal on an unknown ref.
3. SIDEBAR/WSM REMEDIATION (small): (a) route
   LifecycleSink.OnLiveWorkChanged to the sidebar so idle_async retires
   without waiting for the next turn; (b) add `Parent *WorkspaceID` to
   wsm.Workspace recorded at creation and nest the roster off it (the
   ParentBranch derivation stays as fallback for registered-not-created
   workspaces).
4. PAINT/VOCAB/FEEDID/PROMPTS (the four pure leaves; the full brief is
   reconstructable from ARCHITECTURE.md's FeedId scheme + "Paint classes"
   + "Cross-system literals" (sentinels `<!--agent-repl:meta-->` …
   `<!--/agent-repl:meta-->`) + prompts/README.md): feedid Encode/Decode
   (versioned base64url, deterministic); vocab loaders + protoreflect
   descriptor assertions + the footer_allowance accessor; paint ParseANSI
   (SGR incl. 256/truecolor to the 16 names, one class per span by
   ansi_precedence) + Highlight (Go/TS/Python/Shell/JSON/Markdown/Elisp/
   protobuf + plain) asserting classes against paint-classes.json; prompts
   Load/Splice per prompts/README.md + StripSentinels. ALSO REWRITE the
   bodies of prompts/merge-conflict-resolve.md and
   merge-test-failure-resolve.md for the no-ff-merge-in-target flow
   (placeholder sets unchanged; they still describe the retired
   rebase/cherry-pick mechanism).
5. PROMPT HANDLER + QUEUE + CLASSIFIER (the big remaining wave-2 brief;
   reconstruct from ARCHITECTURE's promptqueue seam + daemon.md entries
   4/5 + the landed rulings): recognition via the session_command_spec
   enum option (panels /status /todos /mcp /context answered inline +
   mirrored as command_panel rows; /agents /help and the rest refused as
   command_refused with the add-support offer; bare /model refused);
   SubmitPrompt keys on the landed `workspace` field, `feed` must belong
   to it; origin REQUIRED; mirror before StartTurn; the one queue path
   (lease policies, holds via wsm, the -fake classifier rule in AGENTS.md,
   interject re-spec, parked route to the merge orchestrator, Accept,
   RestoreHolds, boot reconciliation, SetMainAgent + OnTurnOpened calls
   into the sessionwatcher, the one-shot finish hook calling the workspace
   verb at the success-marker terminal).
6. INTEGRATION-TESTS resume (worktree above): finish every SPEC.md suite
   against the landed daemon; git fully mocked.
7. WAVE 3: (a) server/boot/cmd — Connect handlers with base-function
   validation mapping the typed refusal values onto the landed arms,
   publishers per the subscription invariant, flush-on-accept per
   ARCHITECTURE (a proven copy is referenced there), static asset origin
   (re-stat + no-store on the entry point), unowned-workspace refusal,
   boot sequence + adoption reconciliation + intent manifest, main.go
   wiring of every Deps (incl. rollout's hooks and the
   workspace Deps.EvictLogSink → dlog Surfaces.Evict swap); collect NEW
   refusal arms into ERROR-ARMS.md → landing 6; (b) deploy chain —
   bin/deploy-all.sh + build-frontend.sh adaptation (the elisp hook is
   `(agent-repl-runtime-restart-await)` in services.el), DELETE
   agent-shim/wire (importers are gone) + its bin/test-all.sh roster
   entry; (c) daemon/AGENTS.md final pass.
8. INTEGRATION LOOP: the teamlead runs `go test -tags integration
   ./integration/...`, remediates warnings to zero, then the adversarial
   suite audits (fresh fable agents against docs/overhaul/daemon.md +
   webapp.md + elisp.md) until clean.

## Post-stop merge: drain/rollout landed (2026-08-31, after the first stop commit)

The drain/rollout agent finished naturally and is merged (155 tests). For
the wave-3 server/boot brief, its Deps hooks to wire: drain.Deps{Stand,
Freeness, Announcer, Exit, Clock, SweepEvery, RefusalWindow} + the queue
calls Controller.NoteRefusal per drain-refused submission; helpers
drain.EncodeReason/DecodeReason (UpdateShutdownSchedule handler encodes
before PutDrainSchedule), drain.ScheduleID (the tray's schedule id),
drain.ErrNothingScheduled. rollout.Deps{Deploy, Spawner
(NewProcessSpawner), Announcer, Pusher, Participants (one web slot),
Quiesce, DrainIntake, Freeness, Shims (Prelaunch/Adopt/Resume with a cold
ANSWER), LockProbe, PublishViews, WriteDaemonAddr (only when the last
manifest workspace is owned), DeployStamp, SessionBuildSHA, ColdGate,
Exit, DB, SelfRepoDir, SelfAddress, Instance, StateDir, Clock, windows};
cmd under --joining calls rollout.ReportJoiningAddr at bind and boot calls
Controller.Join; the expected-participant snapshot rides the intent
manifest; Reconcile persists all four dispositions as faults (PRESERVED/
ROLLED closed immediately, DIED/UNKNOWN open). Its refusal errors map onto
the landed Adopt* arms; ErrNoTransferAnnounced on AdoptWeb logs INFO.

Recorded concerns from that agent: (a) CheckStaleness compares the daemon
deploy stamp; the shim's own stamp is agent-shim/claude/shim/dist/
.built-sha — DeployStamp is an opaque func, point it wherever the project
lead prefers; wsm has no ShimBuildSHA column (SessionBuildSHA is a
process fact hook) — flag if a column is wanted. (b) KNOWN FLAKE in
internal/workspace: TestStartSurfacesANonColdStartFailure intermittently
fails t.TempDir cleanup ("unlinkat …: bad file descriptor" — a log-sink
descriptor double-close), ~1 in 8 full-suite runs; queue a remediation.
(c) The drain schedule id is derived (drain.ScheduleID from SetAt), not a
column. (d) bin/deploy-all.sh step 5 still names the dead elisp function
(wave-3 item). (e) store/sidecar stay unhandled by design; one WARN names
them at classification.

## Open items awaiting others

- ERROR-ARMS.md holds 6 unlanded rows (the shim-propagated refusal names
  on Interrupt/AnswerPermission/AnswerQuestion, the one-shot finish
  brief_missing, the unspecified-kind guard) — landing 6 with the wave-3
  server arms.
- USER QUESTION (open, via the project lead): whether the webapp hides
  the controls that can only answer `not_deliverable` / `unsupported`
  (currently: shown, refusals surfaced honestly).
- The e2e suite pins the daemon token-figure format per the canon in
  ARCHITECTURE.md; the webapp copies it for the cold-gate figure.
- `SessionUpdate.account_usage` stays (figures) beside rate_limit_status
  (verdicts) per the final ruling — already implemented.

## Process facts a fresh lead must not re-derive

- Subagent branches are `overhaul/daemon-<slug>` (a `/`-nested name under
  overhaul/daemon is impossible in git's ref namespace).
- The 20-subagent session cap and the 3-concurrent fair-share ruling were
  in force; check with the project lead whether they still stand.
- Every pause/landing cycle: pause agents (commit + hold) → ACK the
  project lead → on the landing message `git merge overhaul/integration`
  → relay → resume. Landings 1–5 are merged; landing 6 is pending.
- Agents are resumed by SendMessage to their existing id, never
  re-dispatched, and any message RESUMES a paused agent — include "stay
  paused" when relaying rulings mid-pause. Commit-after-every-compiling-
  step is in every brief because usage-limit outages kill agents mid-step.
