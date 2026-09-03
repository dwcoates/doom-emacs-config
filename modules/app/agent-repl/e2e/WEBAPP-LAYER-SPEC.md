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

4. The Go test runs `npm run test:webapp-layer` with `cmd.Dir` = `webapp/`
   and these variables added to the child's environment:

   | variable | value | read by |
   |---|---|---|
   | `AGENT_REPL_WEBAPP_LAYER` | `1` | the layer's gate — the vitest project refuses to run without it |
   | `AGENT_REPL_E2E_DAEMON_URL` | `http://<daemon.addr>` | the transport's base url |
   | `AGENT_REPL_E2E_WORKSPACE_ID` | the registered ref's id | `workspaceRef(...)` |
   | `AGENT_REPL_E2E_WORKSPACE_DIR` | the registered ref's dir | `workspaceRef(...)` |
   | `AGENT_REPL_FORBID_VENDOR_CALLS` | `1` | standing tripwire, as in the fake-daemon integration config |

   The child's stdout/stderr are streamed into the Go test's log, so a vitest
   failure is read in the Go failure output rather than hunted for.

5. The webapp side is a THIRD vitest project
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
  with `ShimNode`/`ShimMain`/`StoreSocket` forced, `buildIdentityEnv()`
  (`SHIM_BUILD_SHA` == `AGENT_REPL_DEPLOY_STAMP`, checked before `m.Run()`),
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

## D. Loud skips

The layer refuses to pass quietly when it cannot run. Every skip names the
missing prerequisite and the exact command that supplies it.

- **Go side** (`webapplayer_e2e_test.go`), each a `t.Skip` with the reason:
  - `npm` not on `PATH`.
  - `webapp/node_modules` absent (the message names
    `npm ci --prefix webapp`); `npm run` would otherwise fail as a build
    error rather than a missing prerequisite. `pretest`-style bootstrap is
    deliberately NOT relied on here: a network `npm ci` inside an e2e test is
    not this suite's business.
  - the world's own prerequisites (node, shim bundle, store and sidecar
    binaries) are already loud skips inside `requireNode`,
    `requireShimBundle`, `requireStoreBinary`, `requireSidecarBinary`.
- **Vitest side** (`real-daemon.ts`): if `AGENT_REPL_WEBAPP_LAYER` is unset
  the config's own setup THROWS naming that the layer is Go-driven and must
  be run through `go test ./e2e -run TestWebappLayer`, so a stray
  `npx vitest -c vitest.webapp-layer.config.ts` cannot silently pass with
  zero assertions.
- A non-zero vitest exit is a Go test FAILURE, never a skip.

## E. Bounds

- The Go test's own budget: `WebappLayerTimeout`, a NAMED constant. It bounds
  one real world bring-up plus a Node/vitest child process start plus a real
  turn through the real shim, store and sidecar. **20 s** — the only new bound
  in this work. MEASURED on the first green run: the child reported 1.41 s
  wall (transform 406 ms, collect 584 ms, environment 435 ms, tests 233 ms)
  and the whole Go test 2.73 s. It sits deliberately above ~3x that because
  the dominant term is a COLD vitest+jsdom start and the 406 ms transform
  above is a WARM cache — no measurement of the cold case exists to derive a
  3x from, and a bound sized off the warm one would be a race on a cold
  checkout. It is a HANG bound, not a synchronization wait: nothing sleeps,
  the child's exit is awaited on its own channel, and a stuck turn fails
  inside the child's own 5 s budget long before this fires.
- The vitest project's own `testTimeout`/`hookTimeout`: **900 ms is NOT
  widened**, and the existing 300 ms unit / 900 ms integration bounds are not
  touched. Instead the two things that genuinely cost real time here get
  per-site budgets with stated reasons, per the same discipline
  `vitest.integration.config.ts` states: the `beforeAll` that boots the app
  against the real daemon, and any test that drives a real turn. Both are
  measured on the first green run (boot plus one real turn: 233 ms of test
  time), and each carries a one-line reason at its own site — never a raised
  global. Both reuse the Go suite's own measured 5 s per-rpc/turn budget
  rather than minting a third number.
- No `sleep`, no `sit-for`, no fixed interval anywhere: waits are `settle()`
  (DOM quiescence plus zero in-flight requests) on the webapp side and
  channel/`cmd.Wait` on the Go side.

## F. Scenario list

Numbered, prioritized so that what ONLY the webapp can cover comes first.
Everything below is drawn DOM against a real daemon push; nothing asserts a
wire frame the Go area tests already assert.

### F1. Proof of life (1 scenario) — built now

1. A prompt typed into the webapp's own composer and sent reaches the real
   daemon, the real shim answers it (the fake SDK's default PROSE turn, whose
   conclusion echoes the prompt this page sent), and both the prompt row and
   the response row are drawn in the real webapp DOM.
   BUILT AND GREEN: `webapp/test/webapp-layer/proof-of-life.layer.test.ts`,
   driven by `TestWebappLayer`.

### F2. Feed row rendering, one per drawn family (14)

Each drives the fake-SDK scenario that produces the family and asserts the
row's own drawn shape, not just its presence.

2. `user_prompt` — the composer's own submission drawn back from the daemon's
   push (no optimistic row before it).
3. `agent_prompt` — the agent-addressed variant wears its own border class.
4. `activity` / `FeedResponse` — settled markdown body plus the usage stamp.
5. `activity` / `FeedSimpleToolCall` — composed input line and each output
   form the scenario emits (text, code, diff, lines, links).
6. `activity` / `FeedSkill` — skill heading with its outcome.
7. `activity` / `FeedHook` — hook row with its outcome.
8. `activity` / `FeedPlan` — purple response-styled bubble, plan-edit links.
9. `activity` / `FeedFindings` — findings bubble with jump-to-file links.
10. `activity` / `FeedArtifact` — artifact bubble in each state the scenario
    reaches.
11. `activity` / `FeedSubagent` — live head, then settled head after the
    subagent concludes.
12. `detached_*` — the detached wrapper draws the SAME component its sync form
    draws (placement differs, drawing does not).
13. `turn_ended` — the terminal row, and the reason it carries.
14. `separation` — one arm per meta divider the scenarios produce (context
    cleared, context compacted, worktree entered/left).
15. `FeedColdGate` — the one card the client formats and ticks itself, drawn
    from raw facts.

### F3. Sub-feed plumbing (3)

16. Expanding a subagent bubble issues `OpenFeed` on the bubble's `FeedId` and
    draws its rows inside that bubble's container.
17. Collapsing abandons the token and the sub-feed's rows leave the DOM.
18. A settled bubble pages with `GetFeedPage` (first/next, no cursor) and the
    drawn rows extend rather than replace.

### F4. Permission and question cards (4)

19. A real permission ask from the shim draws the card with its buttons, and
    the standing-allow presence marker (never the token) gates the button.
20. Clicking allow sends `AnswerPermission`; the card's answered state arrives
    on the feed push and is drawn on the same row.
21. A question card always draws the free-text escape alongside the offered
    options.
22. An expired question draws as expired, never as pending.

### F5. Footer and topbar surfaces (5)

23. Footer status/substatus/activity redraw as the real turn moves through
    them.
24. The footer's tokens cell and clock reflect the real usage the turn
    accrued.
25. An expanded footer panel draws its fully-resolved rows with no extra round
    trip, and a panel row jumps to its `FeedId`.
26. The topbar draws the real account label and the connectivity dot; the
    context chip draws the current context size.
27. A real unmodeled-tool or session-fault surfacing lands in the topbar
    warning dropdown.

### F6. Command panels (2)

28. A slash command the daemon answers programmatically returns a panel arm,
    the panel is drawn in the composer's area, and NO feed row is drawn for
    it.
29. A second command's panel replaces the first (a panel answers one
    submission, not the conversation).

### F7. Merge bubble tabs (3)

30. A real `MergeWorkspace` draws the merge bubble, and its tab strip carries
    only the tabs whose work has begun.
31. A resolved tab (queue / merge / tests) draws the row's own content, with
    test output as paint-class spans and no ANSI parsing.
32. An agentic tab (pre-prompt / conflicts / fixes / post-prompt) draws its
    rows through the subagent sub-feed path, and a PARKED conflicts/fixes tab
    draws the standing line plus the paused badge.

### F8. Refusal wording (3)

33. A duplicate submission (same idempotency key) is refused and the refusal
    is drawn AT THE COMPOSER, with the text kept in the box and no feed row.
34. A per-method typed error arm from the real daemon renders its refusal at
    its own call site (the clicked control), with the arm's own wording.
35. A domain outcome the contract calls a SUCCESS arm (deny,
    nothing-running, empty result) draws as an outcome and never as a
    refusal.

### F9. Tray, sidebar, lifecycle (4)

36. A real held prompt draws in the daemon-hold tray at the feed's tail, and
    `UpdateHeldPrompt` (deliver-now / discard) is issued from it.
37. The sidebar's roster draws from the real global stream, both groupings
    arriving resolved.
38. A drain-scheduled `WatchDaemon` push draws the standing page-wide restart
    banner.
39. A real daemon restart drives the `transferred` push: the page calls
    `AdoptWebWorkspace` on the new daemon and drops the old stream, drawing
    through the handover.

### Counts by area

| area | scenarios |
|---|---|
| F1 proof of life | 1 |
| F2 feed row families | 14 |
| F3 sub-feed plumbing | 3 |
| F4 permission / question | 4 |
| F5 footer / topbar | 5 |
| F6 command panels | 2 |
| F7 merge bubble tabs | 3 |
| F8 refusal wording | 3 |
| F9 tray / sidebar / lifecycle | 4 |
| **total** | **39** |

## G. Open items

1. Which fake-SDK scenario produces each F2 family is resolved against
   `agent-shim/claude/shim/src/fake/scenarios/*.ts` and
   `e2e/SPEC.md` section D's 69-golden mapping when those scenarios are
   written — the Go area files already name most of them, and the webapp
   layer reuses the same names rather than minting new fixtures.
2. F7's merge scenarios depend on the merge-queue area's scripted fake-git
   fixtures; the webapp layer drives `MergeWorkspace` and asserts drawing
   only, never a git fact.
3. Whether the layer runs as ONE Go test with many vitest files inside it
   (one world, one child) or one Go test per area (one world each) is a
   cost question to settle after the first measurement. Proof of life is
   built as one Go test with one vitest file.
