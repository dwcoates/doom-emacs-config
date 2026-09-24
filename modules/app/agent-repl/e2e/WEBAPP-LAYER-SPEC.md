# The webapp e2e layer — specification

Scope: how the REAL webapp is put in the loop of the cross-system e2e world,
so the chain under test is

```
fake SDK -> real shim -> real store + real sidecar -> real claude-repld -> real webapp (jsdom)
```

This document is a design record plus a scenario list. It states what to
BUILD. Nothing here was derived by running the suite; every fact comes from
`e2e/SPEC.md`, `e2e/world_test.go`, `e2e/main_test.go`,
`daemon/integration/harness/daemon.go`, `docs/overhaul/webapp.md`, the
`frontend/v1` proto comments, and the webapp's own integration harness
(`webapp/test/integration/{harness.ts,fake-daemon.ts}`).

---

## A. The design decision

**Chosen: option (b) — the Go e2e world owns process lifecycle and hands its
daemon's address to a vitest child process it launches for the webapp
assertions.**

### The mechanics

1. One new Go area file, `e2e/webapplayer_e2e_test.go`, builds an ordinary
   `World` through the suite's own `NewWorld` — the same real store, real
   sidecar, real daemon, scripted fake git, fake SDK, pinned build identity
   and resolved config roots every other area file gets. No second bring-up
   definition exists anywhere.

2. It mints a fake-git repository (`harness.NewRepo`) and registers it
   (`harness.Register`), exactly as `tlNewWorkspace` does, so the webapp is
   addressed to a workspace the daemon really knows.

3. The daemon's serving address is already a **loopback TCP** address, written
   by the daemon into `<state root>/daemon.addr` and read back by
   `harness.Daemon.Addr` / `AwaitAddrFile`. That is the whole handoff: the Go
   test passes `http://<addr>` to the child. Nothing needs a unix-socket
   dispatcher, which is why this layer's fetch path is simpler than the fake
   daemon's (the fake is a unix socket only because a loopback port per
   in-process fake exhausted the ephemeral range under parallel vitest runs —
   see `fake-daemon.ts`; there is exactly ONE real daemon per Go test, so that
   pressure does not exist here).

4. **ONE GO TEST PER AREA** (project-lead ruling, 2026-09-03). Each area
   builds its OWN world and names exactly one vitest file, so its artifacts
   are preserved on its own failure, a red run names the area, and the areas
   parallelize; the ~2s world cost per area is acceptable at nine areas. The
   §F7 merge area needs a different world SHAPE (a repository plus a child
   workspace plus a test gate), so it has its own bring-up on top of
   `NewWorld` rather than sharing `wlDriveArea`.

5. The Go test runs `npm run test:webapp-layer` with `cmd.Dir` = `webapp/`
   and these variables added to the child's environment:

   | variable | value | read by |
   |---|---|---|
   | `AGENT_REPL_WEBAPP_LAYER` | `1` | the layer's gate — the vitest project refuses to run without it |
   | `AGENT_REPL_E2E_DAEMON_URL` | `http://<daemon.addr>` | the transport's base url |
   | `AGENT_REPL_E2E_WORKSPACE_ID` | the registered ref's id | `workspaceRef(...)` |
   | `AGENT_REPL_E2E_WORKSPACE_DIR` | the registered ref's dir | `workspaceRef(...)` |
   | `AGENT_REPL_E2E_WEBAPP_BUILD` | the entry the daemon's served dist names (`harness.FakeWebappEntry`) | the harness shell's built entry tag, which the page reads as its webapp build |
   | `AGENT_REPL_FORBID_VENDOR_CALLS` | `1` | standing tripwire, as in the fake-daemon integration config |

   The child's stdout/stderr are streamed into the Go test's log, so a vitest
   failure is read in the Go failure output rather than hunted for.

6. The webapp side is a THIRD vitest project
   (`webapp/vitest.webapp-layer.config.ts`, `test/webapp-layer/**`), not a
   fourth harness: it mounts the app through the SAME
   `webapp/test/integration/harness.ts` mount path the fake-daemon suite
   uses, entered through a new export that takes a base url instead of
   starting a fake. The real `index.html` is still the document, every
   component is still mounted through its own published signature, and
   `settle()` is still the only wait.

### Why (b)

- **One bring-up definition.** Bring-up here is not "start four processes" —
  it is `NewWorld`: a store on a short unix socket with its own log,
  a sidecar pinned to the one spool root the fake SDK writes into, a daemon
  with `ShimNode`/`ShimMain`/`StoreSocket` forced, the real services
  reporting their builds where the daemon's deploy reads them,
  `resolveConfigRoots` symlink resolution, `assertOneSpoolRoot`,
  `preserveLogsOnFailure`, the scripted fake git, and a LIFO teardown whose
  order is itself load-bearing. Option (b) reuses all of it verbatim and adds
  zero process management on the TypeScript side — the child process starts
  no servers at all.
- **The invariants that fail silently stay in one place.** `e2e/SPEC.md`
  section B names two invariants that, when broken, do not fail as
  themselves but as a suite of timeouts (one build identity in both roles;
  one string per config root). Both are enforced in Go, once, before any
  test runs. A TypeScript bring-up would have to re-derive both.
- **The webapp keeps its own idiom.** jsdom, fake timers, `settle()` and the
  DOM-hooks queries all stay in vitest, where they already are.

### Why not (a) — vitest spawns the quartet itself

It duplicates every item in the list above in a second language: building the
shim bundle and staging its siblings, `go build` of the store and sidecar,
resolving and pinning the build identity, installing scripted fake git,
symlink-resolving the config roots, the store-ready and daemon-serving
awaits, the spool-root pairing, the log-preserving teardown. Two
implementations of a bring-up whose failure mode is "everything times out
and nothing says why" is exactly the drift this repo's shared-harness rule
exists to prevent. It also cannot reuse the Go-only levers the world is built
on (`t.TempDir` lifetimes, `AwaitLogRecord`, `fakegit.State` mutation,
`harness.Register`).

### Why not (c) — one shared bring-up script

A shell script cannot express this bring-up. Its per-test state root and temp
dirs, its readiness gating on structured-log records, its build-identity
resolution and cross-check, and its ordered teardown are Go API calls against
live objects, not a sequence of shell commands. Extracting them to a script
would mean rewriting the Go harness as a script AND rewriting every existing
Go area test against it — a strictly larger change that makes the Go suite
worse to buy the webapp suite nothing that (b) does not already give it. It
also loses per-test isolation: the Go suite builds a FRESH world per test,
which one long-lived script-managed stack does not model.

### Rejected variant, recorded

Reversing the direction (a vitest suite that shells out to `go test` to bring
up a world and then talks to it) has the same duplication problem as (a) plus
an inverted process tree, and gives the webapp no way to fail the Go suite.

---

## B. How the real daemon is reached

- **Address**: `http://<harness.Daemon.Addr>` — loopback TCP, read from
  `daemon.addr`. Passed in as `AGENT_REPL_E2E_DAEMON_URL`.
- **Transport**: the app's own `createDaemonTransport(baseUrl, { fetch })`.
  Node's global `fetch` is handed in because jsdom's window has none; it needs
  no undici dispatcher, since the url is a real reachable origin. The
  jsdom-`AbortSignal` bridge (`node:util`'s transferable controller) stays —
  it is the same missing-capability fix, and without it every `Watch*` request
  throws before leaving the page.
- **Workspace**: `workspaceRef(AGENT_REPL_E2E_WORKSPACE_ID,
  AGENT_REPL_E2E_WORKSPACE_DIR)`, the ref `RegisterWorkspace` minted.
- **Boot**: production's own order, unchanged — `adoptAtBoot` (a real
  `AdoptWebWorkspace` against the real daemon), then `startLifecycle`, then
  feed, footer, login, topbar, sidebar, tray, and the composer when the dev
  flag is on. A terminal adoption refusal still mints `boot_failed` and
  throws.
- **Prompts**: submitted through the webapp's OWN composer
  (`mountComposer`), so the client mints the idempotency key and sets the
  webapp origin. Fake-SDK scenarios are selected the way the Go suite selects
  them: the prompt text is `!<scenario>` (`fake/registry.ts`).
- **The HOST participant is the Go driver's to hold.** The footer's
  connectivity truth is the PAIR of participants (`server.holdParticipant`
  fires on the open/close edges of `WatchHostWorkspace` and
  `WatchWebWorkspace`). The real webapp supplies the web hop itself; Emacs is
  an external system this suite mocks, so the Go driver opens a
  `WatchHostWorkspace` stream for the child's lifetime. Without it the footer
  correctly draws `disconnected` forever, a disconnected footer CLOSES the
  composer gate, and the page's SECOND submission and every one after it is
  refused by the app itself — the first symptom this layer produced.
- **The unit project must EXCLUDE this layer.** `vitest.config.ts`'s `exclude`
  now carries `test/webapp-layer/**` alongside `test/integration/**`. Without
  it the fast unit run collects the layer's files, they find no daemon, and
  the loud gate fails `npm test`. A future config edit that drops that entry
  silently re-collects the layer into the unit run.
- **Writes**: everything the layer writes lands under the Go test's own
  `t.TempDir()`/state root or vitest's own temp space. The vitest child gets
  no writable path of its own beyond `node_modules` it already has.

## C. How the page is mounted

`webapp/test/integration/harness.ts` is split at its existing seam rather
than copied:

- `MountedApp` — the interface every existing query and control already
  lives on (`settle`, `tick`, `$`, `row`, `rowIds`, `click`, `failureArms`,
  `refusalArms`, `ctx`, `shell`, `feed`, `footer`, `login`, `panels`,
  `disposeMounts`, `stop`).
- `Harness extends MountedApp` — adds the three fake-daemon-only members
  (`fake`, `secondFake`, `startSecondDaemon`). The 13 existing integration
  files are untouched.
- `startHarness(options)` — unchanged behavior: starts a fake daemon.
- `startAppAgainst(baseUrl, options)` — NEW: mounts the identical app against
  a base url the caller supplies, returning `MountedApp`. `arrange` is
  refused here (there is no fake to script), loudly.

`webapp/test/webapp-layer/real-daemon.ts` wraps `startAppAgainst` with the
environment read and the loud skip.

## D. Loud missing prerequisites

The layer refuses to pass quietly when it cannot run. Every unmet prerequisite
names itself and the exact command that supplies it, and FAILS the test — a run
that covered nothing must never look like a pass. `AGENT_REPL_E2E_ALLOW_MISSING_DEPS=1`
turns those failures back into skips for a deliberate local poke, and every skip
so taken is reprinted in the end-of-run summary block (`precondition_test.go`).

- **Go side** (`webapplayer_e2e_test.go`), each a `requireDependency` with the reason:
  - `npm` not on `PATH`.
  - `webapp/node_modules` absent (the message names
    `npm ci --prefix webapp`); `npm run` would otherwise fail as a build
    error rather than a missing prerequisite. `pretest`-style bootstrap is
    deliberately NOT relied on here: a network `npm ci` inside an e2e test is
    not this suite's business.
  - the world's own prerequisites (node, shim bundle, store and sidecar
    binaries) are already loud skips inside `requireNode`,
    `requireShimBundle`, `requireStoreBinary`, `requireSidecarBinary`.
  - the webapp package directory not writable (`wlRequireWebappWritable`,
    over the pure `wlWebappWritable` that `harness_selftest_test.go` covers in
    both directions). This one is a hard failure and NOT the
    `AGENT_REPL_E2E_ALLOW_MISSING_DEPS` opt-out: no command the reader could
    run supplies it. It exists because the layer already paid for its absence
    — the e2e sandbox links `webapp/node_modules` at a read-only image layer,
    vite's default `cacheDir` is `node_modules/.vite`, and all eleven areas
    failed in the container (and only there) with a filesystem error nowhere
    near the config that caused it. The cache moved to `webapp/.vite-cache`
    (`webapp/vite-cache.ts`, held there by `webapp/test/vite-cache.test.ts`);
    this precondition is what makes the next such regression say "the layer
    cannot run here" once, by name, instead of eleven times from inside a node
    process.
- **Vitest side** (`real-daemon.ts`): if `AGENT_REPL_WEBAPP_LAYER` is unset
  the config's own setup THROWS naming that the layer is Go-driven and must
  be run through `go test ./e2e -run TestWebappLayer`, so a stray
  `npx vitest -c vitest.webapp-layer.config.ts` cannot silently pass with
  zero assertions.
- A non-zero vitest exit is a Go test FAILURE, never a skip.

## E. Bounds

Every bound below is either REUSED from an already-measured constant or set at
~3x an observed max. Nothing was widened.

| bound | value | basis |
|---|---|---|
| `WebappLayerTimeout` (Go, per area) | 10 s | MEASURED: two full nine-area runs, child durations 1.21-3.16 s (slowest: feed families, 22 real turns in one child); 3x the max. Bounds a HANG only. |
| `BOOT_BUDGET_MS` (vitest) | 5 s | REUSED: the Go suite's `harness.DefaultTimeout`, minus the shim spawn boot does not pay. |
| `TURN_BUDGET_MS` (vitest) | 5 s | REUSED: the same measured 5 s, for exactly the shape it was measured on (a real shim spawn plus a turn through a real store). |
| `COMPOSER_READY_BUDGET_MS` (vitest) | 5 s | REUSED: `TURN_BUDGET_MS`. What it waits out is the TAIL OF THE PREVIOUS SUBMISSION — `composer.ts` keeps Send disabled for the whole of its `SubmitPrompt` unary — which is strictly less than the turn that budget already bounds. Zero rounds on a healthy chain. |
| per-test `TURN_TEST_MS` | 10 s | boot + one turn, at each site that drives a real turn — never a raised global. |
| vitest `testTimeout`/`hookTimeout` | 900 ms | UNCHANGED from the integration project. Tests that cost real process time carry their own budget at the site. |
| unit project's 300 ms | untouched | this layer is excluded from it. |

A DROPPED PRESS IS NOT A BOUND PROBLEM, and was mistaken for one. `send`
used to click and settle. `src/composer/composer.ts` drops a press it cannot
take — an empty box, a closed gate, or a submission still in flight — and says
nothing, which is right for production; and the harness's in-flight set clears
when a response HEAD lands while the composer stays `inFlight` until the whole
unary resolves, so `settle()` could report the page quiet with Send still
disabled. `driveTurn` pressed into that window and the turn never started.
Measured on the run that found it (`feed-families.layer.test.ts`, `!rotate`,
2026-09-10): 11514 settle rounds inside the 5 s budget with nothing in flight,
and no `StartTurn` in the shim between the previous turn's and the NEXT test's.
`send` now waits for a pressable button and then PROVES the press was taken,
reading `disabled` synchronously after the click; `press` is the same thing
answering whether it was taken, for §F8 #33, the one scenario that presses
expecting nothing. Neither bound moved.

Two bound corrections worth recording, both of which came from measuring
rather than guessing:

- `WebappLayerTimeout` was drafted at 60 s and then 20 s on the theory that a
  COLD Vite transform dominated and could not be measured. The theory was
  wrong: the transform is ~400 ms on every run, cache or none (clearing
  the vite cache changes nothing — vitest transforms sources per run),
  so there was no hidden cold-start term. 10 s, from measurement.
- The §F7 merge area briefly carried a 15 s "merge chain" bound. That was
  covering a HARNESS FAULT — a missing `AGENT_REPL_TEST_ALL_SCRIPT` made the
  merge gate exit 127, so the merge never reached a terminal and the bubble
  sometimes never appeared. With the gate provided, the bubble is drawn
  19-57 ms after the enqueue and the tab strip 6-8 ms after that, and the
  area needs no bound of its own at all.

No sleeps anywhere: waits are `settle()` (DOM quiescence plus zero in-flight
requests) on the webapp side and `cmd.Wait` over a channel on the Go side.

## F. Scenario list — AS BUILT

Nine areas, nine Go tests, nine vitest files, **63 vitest tests**, all green
over two consecutive full runs. Each area's file header is its own spec.

### The family -> scenario map, established EMPIRICALLY

Every `!name` this layer submits is a fake-SDK scenario a Go area test already
drives (project-lead ruling: never mint one). The mapping was not read off
scenario names — it was measured by driving each candidate through the real
chain and recording which row arms the real daemon resolved:

| drawn family | scenario | Go area that drives it |
|---|---|---|
| `user_prompt`, `turn_ended` | any | every area |
| `activity.response` | `md`, prose (no prefix) | turn lifecycle |
| `activity.simple_tool_call` | `bash`, `edit`, `read`, `web-search` | file tools, detached bash, web |
| `activity.skill` | `skill`, `skill-fail` | skills |
| `activity.hook` | `hook-blocked`, `hook-failed` | hooks |
| `activity.plan` | `plan` | remainder |
| `activity.findings` | `findings` | remainder |
| `activity.artifact` | `artifact-publish` | remainder |
| `activity.subagent` | `subagent` | subagents |
| `detached_subagent` | `subagent-detached` | subagents |
| `detached_shell` | `bash-detach` | detached bash |
| `separation` | `compact`, `rotate`, `worktree-keep` | compaction, identity rotation |
| `permission` | `perm-hold`, `perm-allow-standing-mode` | permissions |
| `question` | `ask-single`, `ask-multi`, `ask-free`, `ask-unanswered` | questions |
| `activity.merge` + `merge_tab` | `MergeWorkspace` (no scenario) | merge queue |

Two contract-conformant NON-drawings are pinned as their own tests, because
the contract says they must draw nothing: a SUCCEEDED hook (`hook-success`)
and an artifact LIST act (`artifact-list`).

### F1. Proof of life (1 test) — `proof-of-life.layer.test.ts`

1. A prompt typed into the webapp's own composer reaches the real daemon, the
   real shim answers it, and the response row (carrying the echo of what this
   page sent) is drawn in the real webapp DOM.

### F2. Feed row families (22 tests) — `feed-families.layer.test.ts`, plus
`query-death.layer.test.ts` (1 test)

One test per drawn family, plus one per output form and per non-drawing rule:
`user_prompt`; response body and terminal state; the tool-call shell's input
form and output body; the diff form; the read form; the links form; a skill
card; a skill's outcome state; a blocked hook; a failed hook; NO row for a
succeeded hook; a plan bubble; a findings bubble with its findings; an
artifact bubble with its url; NO row for a list act; a sync subagent head; a
detached subagent drawn through the SAME head; a detached shell; the terminal
row; compaction, rotation and worktree separations.

**ONE QUERY DEATH PER AREA FILE — `query-death.layer.test.ts`.** A dead vendor
query ends its session: the shim refuses every later `StartTurn` with
`StartTurnFailure.query_dead`. One area file mounts one page against one
workspace and therefore drives ONE session, so a file can exercise at most one
query death. `!query-eof` (`unexpected_eof`) is this file's last test, and
`!query-fail` (`iterator_failure`) has an area file of its own so the Go driver
gives it its own world. Both areas DECLARE `daemon.health.open_fault`: the
vendor query dying is their subject, not incidental noise.

**`agent_prompt` IS NOT A ROOT-FEED FAMILY.** It is a SUB-FEED row — "THE
CONNECTION IS THE PLACEMENT ... a subagent's rows never name a parent, they
arrive on the bubble's own feed" (`proto/src/frontend/v1/feed.proto:41-45`), and
the daemon composes the row for whichever feed it resolves
(`feed.proto:1421-1435`). This map therefore never covers it and its absence
here is NOT evidence of a gap; what the chain does and does not produce for a
subagent commission is Finding (3).

### F3. Sub-feed plumbing (6 tests) — `subfeeds.layer.test.ts`

Expand issues `OpenFeed` on the bubble's own `FeedId` and draws its rows
inside that container; collapse folds it away and KEEPS the address (the
contract's collapse abandons the TOKEN, not the DOM); re-opening the same
address answers the same rows; the root feed's rows never leak into the
bubble's container.

THE COMMISSION IS A ROW OF THAT SUB-FEED. A spawn's instruction is drawn on
the CREATED agent's own feed as an `agent_prompt` row under a "from <sender>"
address, for the sync spawn and the detached one alike — a bubble's body IS
its sub-feed, so this is the only place the instruction appears and neither
form draws a second row on the caller's feed.

### F4. Permission and question cards (9 tests) — `cards.layer.test.ts`

A real permission ask with its buttons and waiting line; the standing-allow
button only when a standing form was offered; answering from the card, with
the answered state arriving on the FEED PUSH onto the same row and the
controls gone; a policy denial drawn as an outcome, not a refusal; a
question's free-text escape always drawn alongside its options; a
multi-choice question's own mode; answering through an option; answering
through the escape; an unanswered ask drawn in its own state.

### F5. Footer and topbar surfaces (7 tests) — `surfaces.layer.test.ts`

The status strip redrawn between idle and a running turn; the clock and tokens
cells after a real turn; an expanded panel drawn from the push with no extra
round trip; the topbar's account and connectivity; the model selector's served
options; the context chip; an unmodeled tool surfacing in the topbar warnings
and NOT as a feed row.

### F6. Command panels (5 tests) — `panels.layer.test.ts`

A recognized slash command answered programmatically draws its panel and mints
no feed row and no turn; the composer clears; no refusal; a second command
REPLACES the standing panel; and a recognized command with NO PRODUCER draws
the daemon's own refusal card and no panel at all.
(No fake-SDK scenario is involved — the daemon's recognition table intercepts
these, so no scenario was invented.)

The last scenario read "the help panel carries the daemon's rows" until
2026-09-09, which the daemon has never done: `/help` is ruled UNPRODUCED
(daemon/ERROR-ARMS.md) and answers `command_refused`. It passed because the
file's own `command()` helper waited for `panels().length > 0`, which the
previous scenario's panel already satisfied, so the assertion read a stale
panel. Both the helper and the scenario now name what actually happens.

### F7. Merge bubble tabs (5 tests) — `merge-tabs.layer.test.ts`

The merge drawn as ACTIVITY rather than a turn of its own; a tab strip whose
every tab names itself; the open tab's own resolved content; no raw ANSI
anywhere (colour arrives as paint classes); and THE PARITY INVARIANT — the
merge body lives in a sub-feed at the bubble's own `FeedId`, the same address
a subagent bubble's sub-feed uses, so a merge-specific loader would fail here.

### F8. Refusal wording and placement (5 tests) — `refusals.layer.test.ts`

A second submission of the same text mints a second turn and refuses nothing;
an empty composer sends nothing and refuses nothing; a control's answer stays
inside that control and out of the page's failure overlay; an idle footer
offers no interrupt; a failed turn draws its cause on its terminal row rather
than as a refusal.

### F9. Tray, sidebar and lifecycle (5 tests) — `roster.layer.test.ts`

A really-held prompt drawn in the tray and NEVER as a feed row; discarding it
through the card's own `UpdateHeldPrompt`; the roster drawn from the global
stream; both groupings offered resolved with the rendered one among them; the
page-wide restart banner drawn from the daemon's own drain push, naming the
cause and repeating the operator's note verbatim.

#### F9 #39, the restart handover — `restart-handover.layer.test.ts`

ITS OWN AREA FILE AND ITS OWN GO TEST (`TestWebappLayerRestartHandover`),
because it is the one area whose Go side acts WHILE the page is mounted.

THE RENDEZVOUS. The child logs a marker through the daemon's own `ClientLog`
rpc (`webapp-layer.handover.page-mounted`) once its page is mounted and its
streams are standing; the Go side awaits that record with `AwaitLogRecord` on
`harness.ClientLogPath(ws)` and only then lands the commit whose one deploy
finds the daemon stale and hands it over. Nothing sleeps on either side. The
handover itself is the suite's existing machinery, reused verbatim from
`adoption_e2e_test.go` (`adSelfRepoWorld`, `adTriggerDeploy`, `adDial`,
`adAwaitAddrFileChange`) — there is no second way to replace a daemon here.

WHAT THE PAGE ASSERTS: the `transferred{address}` push draws the terminal
"workspace moved to <address>" notice naming a DIFFERENT origin than the one
it booted against; the page then goes quiet, and the refusal it draws for a
further submission is the LOCAL one (`callUnary` refuses before the wire,
drawn at the composer as its `transport` pseudo-arm with
`unary.ts`'s own sentence); and the RELOAD Emacs would perform — a fresh page
at the address this banner named — boots, adopts the workspace on the
successor itself through `adoptAtBoot`, and draws a resolved footer status and
its root feed again.

WHAT IT IS NOT HELD TO, and why: the page NEVER REDIALS
(`webapp/src/lifecycle/lifecycle.ts:12-21`, project lead, final), so the
adopts on the successor are issued by the Go side, playing the lagging client
exactly as §C #50 does — concurrently, because every expected participant
succeeds together. And the `transferring_away` refusal ARM is unreachable from
the page: it is the same fact arriving as an answer, and the old daemon starts
answering it one line before it pushes `transferred`
(`daemon/internal/rollout/handover.go:129-130`), which quiesces the page — so
reaching the arm from here would mean racing the two. The Go suite pins it.

TWO DAEMON-SIDE FACTS THIS AREA SURFACED, both recorded under Findings: the
successor RE-ARMS the adopt rendezvous from the stand-down intent manifest
after the first arming was already satisfied, so a page that reloads and
adopts needs a host participant to arrive AGAIN; and the recovered page's
footer correctly reads disconnected, because the host hop this suite holds
lived on the daemon that went away.

### Counts

| area | tests |
|---|---|
| F1 proof of life | 1 |
| F2 feed row families | 22 |
| F3 sub-feed plumbing | 6 |
| F4 permission / question | 9 |
| F5 footer / topbar | 7 |
| F6 command panels | 5 |
| F7 merge bubble tabs | 5 |
| F8 refusal placement | 5 |
| F9 tray / sidebar / lifecycle | 5 |
| F9 #39 the restart handover | 1 |
| **total** | **66** |

## G. Findings and gaps

Findings this layer surfaced. Each is recorded in place, in the test file that
found it, and NONE is papered over with a fixture.

1. **The response usage stamp is never populated.** The contract says the
   usage stamp "rides every state" of a `FeedResponse`, and the renderer draws
   one whenever `FeedResponse.usage` is set. Against this real chain it is
   never set — `md`, `usage-full` and `prose-streamed` all draw a response with
   no `.usage-stamp` anywhere on the page. Production/contract gap, for the
   daemon owner. (`feed-families.layer.test.ts`)

2. **`SetModel` on a SERVED option fails at the transport.** Picking an option
   the daemon itself served comes back as a transport refusal ("the daemon
   could not be reached", drawn in `.topbar-model`) rather than the echoed
   `AgentModel` token the contract promises. Production fault. The placement
   rule still holds over it, which is what §F8's test pins.
   (`refusals.layer.test.ts`)

3. **`agent_prompt` was drawn from nothing this chain produced — FIXED, and
   the finding is now §F3's two assertions.** The family map's "no scenario
   draws `agent_prompt`" was first re-examined (project lead, 2026-09-03) as a
   MEASUREMENT ARTIFACT: `agent_prompt` is a SUB-FEED row ("THE CONNECTION IS
   THE PLACEMENT — a subagent's rows never name a parent, they arrive on the
   bubble's own feed", `proto/src/frontend/v1/feed.proto:41-45`), so a map that
   scans only the root feed could never see it. The assertion written on that
   correction still failed: the sub-feed drew `activity.response`,
   `activity.simpleToolCall`, `activity.response` and no prompt row, because
   `applyPrompt` folded only `subagent_type` and `description` onto the bubble
   HEAD and the commission's BODY was never drawn anywhere.

   The daemon path landed (`drawCommission`,
   `daemon/internal/resolve/feed/subagent.go`): a spawn's commission is drawn
   on the CREATED agent's own feed as a `FeedAgentPrompt` under
   `KindPrompt`/`Sub "commission"`, addressed "from <sender feed>", with the
   instruction as one text block — and no second row on the caller's feed,
   whose head IS the sender's end. §F3 now carries the two assertions this
   finding promised: the sync commission and the detached one.

   THE F2 FAMILY MAP IS CORRECTED accordingly: `agent_prompt` is a SUB-FEED
   row and is NOT expected on the root feed, so its absence there is never
   again read as a gap.

4. **No scenario or Go area drives `cold_gate`.** No Go e2e test references it
   at all. Same disposition as (3).

5. **The duplicate-key refusal is unreachable from the page.** The app reuses
   an idempotency key only to retry a FAILED send, so provoking the daemon's
   duplicate arm needs a fault injected between page and daemon, which this
   layer has no seam for. Covered directly by the fake-daemon integration
   suite. (`refusals.layer.test.ts`)

6. **`nothing_running` is unreachable from the page.** An idle footer draws no
   interrupt control at all, so the page can never issue the call that would
   answer it. Covered by the fake-daemon integration suite; the layer pins the
   reason instead. (`refusals.layer.test.ts`)

7. **Two clocks, and they are not the same.** The page's ticker starts at
   `HARNESS_EPOCH_MS`; the daemon runs on the wall clock. A wire timestamp the
   page MINTS must be on the daemon's clock (a page-clock instant is decades in
   the daemon's past and fires a scheduled drain immediately), and the
   consequence is that such a banner's COUNTDOWN reads absurdly on the page.
   Assert the cause and the note, never the countdown. (`roster.layer.test.ts`)

8. **§F9's two declared daemon faults are its own subjects**, declared through
   `ExpectWarnings` so a NEW fault cannot hide behind them: the refused lease
   acquisition IS the prompt hold, and `daemon.drain.fire` is the drain this
   area scheduled.

9. **The successor RE-ARMS the adopt rendezvous after it was already
   satisfied.** Observed in §F9 #39: the joining daemon arms the rendezvous
   from a manifest that arrives after boot, both hops adopt and the workspace
   is adopted — and then `daemon.rollout.join` arms it AGAIN from the
   stand-down intent manifest, resetting `host_called`/`web_called`. A page
   that reloads at the successor's address and adopts at boot therefore waits
   for a host participant a SECOND time, and with no host caller arriving it
   sits until its own budget runs out and answers `not_yet_adopted`. In
   production Emacs re-points the webview and re-adopts the host hop, so the
   pair completes; the layer's Go side plays that half explicitly, and keeps
   calling for as long as the page runs (a host adopt accepted "at once" a
   millisecond BEFORE the re-arm satisfies nothing). Whether a satisfied
   rendezvous should be re-armed at all is the daemon owner's call.

10. **A recovered page's footer correctly reads disconnected.** The host
    participant this layer holds lives on the daemon that went away, so after
    a handover nothing holds the host hop on the successor. §F9 #39 asserts
    that a status was RESOLVED at all, never which one.

### Still open

- **§F9 #39, the restart handover — BUILT** (2026-09-03), in the shape this
  section proposed: the child's `ClientLog` marker, awaited with
  `AwaitLogRecord`, is the rendezvous. See §F9 above for what the page is held
  to and what it is not.
- Findings (1) and (2) are production faults; when fixed, each becomes one
  more assertion in the file that found it.
