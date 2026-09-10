# STOPPING POINT — webapp system (teamlead), second pause (2026-09-02)

Branch `overhaul/webapp`, worktree `~/.config/doom-overhaul/webapp`. The recorded
dispatch queue from the first pause is COMPLETE. Tip stated in the final report;
working tree clean; zero implementer worktrees under webapp-agents/.

## Verification at the tip

- `npm run typecheck` 0 errors; `npm test` exit 0, 97 files / 3279 tests, zero
  unhandled errors; `npm run build` green.
- `npm run test:integration` green: 13 files / 1601 assertions / 0 failed, every
  file run individually with `--no-file-parallelism --testTimeout=5000`
  (client-log 16, composer 78, failures 85, feed-kinds 314, feed-routing 44,
  footer 219, lifecycle 85, refusals 327, sidebar 138, streams 76, topbar 107,
  tray 56, fake-daemon.self 56).

## What landed since the first pause (b8aeaabaa → tip, 111 commits)

- Briefs 1–4 of the recorded queue: topbar strip + reveals, login overlay over the
  server-stream terminal, lifecycle (adopt-at-boot, no-redial moved notice, drain
  and shutdown banners); merge bubble head + tab-strip body over the shared
  sub-feed plumbing; landing-4 typed-refusal pass with the ONE refusal hook
  (src/rpc/refuse.ts + refusal.ts) and ONE click guard (src/rpc/guard.ts); wiring
  (main.ts boot order, createRowRenderers registry, composer gate from footer
  status, ONE formatter src/format.ts per the ruled token table, legacy/ deleted,
  tsconfig exclusion flipped, AGENTS.md rewritten).
- Landing 6 merged and adapted (command_acted, duplicate_submission,
  turn_already_open retired; UpdateMergeQueue has no webapp surface).
- Integration suite's first run and remediation loop: systemic test-side root
  cause (jsdom AbortSignal vs Node fetch; unset fixture breadcrumbs; settle()
  returning early) plus one production ordering fix (startLifecycle before any
  view mount); then component remediation by area; then two fresh-context
  adversarial audits (18 + 14 critiques, all folded in; production fixes: root
  and bubble feed reopen via OpenFeed instead of re-echoing a dead token;
  Interrupt refusals through the one hook; vendor_unmodeled type drawn).
- Programmatic dead-code pass (knip, ts-prune, tsc strict, coverage): deletion
  and kept-with-reason lists are in the final report; knip.json rides on the
  branch; ruled-dead files verified absent.
- Rulings recorded in webapp.md: health surfaces (none pull-driven this wave;
  session faults via the pushed topbar warnings), UpdateMergeQueue (no webapp
  surface). Preamble §5 reveal enumeration gained `mode`.

## Audit loop state

Stopped by the teamlead after round 2: both rounds' critiques are covered; round
2 was finer-grained and exposed one production gap (bubble reopen). A third
fresh-context audit is optional on resume.

## Open UX questions for the user

webapp-briefs/UX-LIST.md is the running list (extended this session with merge
bubble, topbar, sidebar/tray remediation, and audit-derived items: refused
sub-feed reopen placement, footer jump to an undrawn target, markdown links in
prose, nuke typed-name guard, tab auto-select on a settled merge).

## Queue on resume (nothing outstanding from the recorded queue)

1. Optional third adversarial audit (fresh fable, read-only).
2. Any rulings the user makes on UX-LIST.md items → small opus-low dispatches.
3. Landing-7+ relays from the project lead → pause/ack/merge/adapt.
4. The project lead merges overhaul/webapp into overhaul/integration and runs the
   cross-system e2e suite; e2e-attributed webapp reds come back as remediation.

## Standing constraints that survive the pause

Fable-low lead; opus-low implementers (sonnet-medium offloads and the dead-code
pass only); 3-concurrent cap; no proto edits; no real git in tests; no pushes or
PRs; hand-created worktrees fast-forwarded to the tip; round-trip before reaping;
scratch files prefixed `webapp-`.

## Integration suite: parallel is now a supported mode (2026-09-02)

`npm run test:integration` runs file-parallel by default and is green that way;
`--no-file-parallelism` is a convenience for readable output, not a requirement.

The boot-time flake it used to show — `AdoptionFailed: AdoptWebWorkspace refused
(transport): fetch failed`, up to 113 cases in a loaded run, all green in
isolation — was NOT a race and NOT a production defect. Mechanism, measured:

- Each fake daemon bound a fresh `127.0.0.1:0` listener. A run is ~1600 tests,
  each booting a daemon and holding several standing streams, so a run opened
  many thousands of loopback connections; each burns an ephemeral port that then
  sits in `TIME_WAIT` for an MSL (macOS: 15 s).
- With a few runs concurrent, macOS's `net.inet.ip.portrange` (49152-65535,
  16384 ports) went dry. Instrumenting the harness fetch caught the errno
  directly: `connect EADDRNOTAVAIL 127.0.0.1:<port>`, thousands of them.
- Undici surfaces that as a bare `fetch failed`; adoption is the FIRST call a
  page makes, so the exhaustion always landed there and failed the boot.

Fix (harness only, `test/integration/fake-daemon.ts` + `harness.ts`): the fake
daemon listens on a unix socket in a per-daemon temp dir, and the harness's
fetch is given an undici `Agent` pinned to that socket. `baseUrl` is now an
identity (`http://fake-daemon-N.invalid`) — the wire carries a daemon address as
a string and a transfer test asserts it is drawn, but nothing dials it. No
ephemeral port is consumed anywhere in the suite, so the exhaustion is
unrepresentable rather than unlikely. `lifecycle.ts` was NOT changed: its refusal
to retry a boot transport error is the frozen contract and was never the fault.

Locked by two tests in `fake-daemon.self.test.ts` ("the listener's contract"):
the first dial after `start()` resolves lands with no retry, and the listener is
a filesystem path while `baseUrl` resolves nowhere.

Known non-fault: four suite runs at once (52 vitest workers on 16 cores) fail on
the 900 ms `testTimeout`, with zero `AdoptionFailed`. That is machine
oversubscription, not a suite defect.
