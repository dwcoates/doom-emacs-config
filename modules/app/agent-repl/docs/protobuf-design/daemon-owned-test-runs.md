# Daemon-owned test runs

Every test run on the host goes through the daemon, so every run shares the
host's one test-CPU budget and none can start outside it. Agents name exactly
the tests their change touches, down to a single Go subtest; the whole roster
is the merge queue's to run.

## Core design principles

- The daemon is the one runner of tests (owner, 2026-10-23). Consequence: the
  agent API gains RunTests, and agents' documented test commands go through it.
  It does not claim that the runner itself (testrun) moves into the daemon: the
  daemon runs the workspace's own testrun, from the workspace's worktree.
- Agents run only the tests they need, and almost never the whole roster
  (owner, 2026-10-23). Consequence: a selection reaches a single subtest in
  every framework the repository has, and nothing in the request means "run
  everything".

## Landed changes

### testrun.v1 (new package): TestRunRequest, TestSelection, TestRunEvent

- WHAT: the selection (Go package by directory with all or named tests, each
  test whole or narrowed to subtest paths; vitest file with all or named full
  names; ERT file with all or named tests; harness with all or named groups),
  and the events a run reports (planned, unit_ended with per-test results,
  ended, refused).
- WHY: one canonical form owned by the runner, which resolves and reports it;
  the daemon relays it verbatim, so agentrepl.v1 embeds these messages whole
  instead of re-declaring them.
- Paths are repository-relative so a selection names one place from anywhere.
- A target no suite owns, or a name the target does not define, is refused,
  never run as nothing.
- Subtest segments and names are matched exactly (quoted), never as patterns.
- Consequence: testrun's go.mod now depends on the proto module.

### agentrepl.v1 RunTests (server-streaming)

- WHAT: RunTestsRequest { workspace, testrun.v1.TestRunRequest }; each
  RunTestsResponse frame is a relayed testrun.v1.TestRunEvent or a
  RunTestsError (unknown workspace, no selection, runner failed).
- WHY: the agent API is how agents reach the daemon; the daemon runs the
  workspace's bin/test-all.sh with the selection, so the build, the git-state
  net, the background priority, the run log and the shared core seats all
  apply unchanged.
- The runner's events reach the daemon on an inherited pipe (a file
  descriptor), typed, never parsed out of the log text.

## Every test run is the daemon's (2026-10-24)

Owner ruling, 2026-10-24: `bin/background.sh` refuses every test run the daemon
did not start, so nothing runs tests by hand. Every hand tool that ran tests
becomes a daemon request instead.

| Hand tool | Replaced by |
|---|---|
| `bin/test-all.sh` (whole roster) | `testrun.v1.TestSelection.every_suite` |
| `bin/test-all.sh --suites a,b` | `testrun.v1.TestSelection.suite` |
| `bin/test-all.sh --coverage`, `bin/report-nonlisp-coverage.sh`, `bin/e2e-coverage.sh`, every `make coverage` | `testrun.v1.TestRunRequest.coverage` over whole suites |
| `npm run coverage:verify` | coverage-only roster suites, run by every coverage run |
| `bin/test-all.sh --record` | `testrun.v1.TestRunRequest.record` with `every_suite` |
| `bin/e2e-repeat.sh` | `agentrepl.v1.RunTestsRequest.runs` and `stop_at_first_failure`; each frame names its run (`agentrepl.v1.RunTestsResponse.run`) |
| `bin/with-cpu-load.sh` by hand | `agentrepl.v1.RunTestsRequest.cpu_load`, refused unless `agentrepl.v1.RunTestsRequest.requester` is the user; the daemon runs the script itself |
| `testrun redundancy` | `agentrepl.v1.RunRedundancy` carrying `testrun.v1.RedundancyRequest` |

Decisions:

- **Coverage is a whole suite's measurement.** `coverage` with a narrower
  selection is refused by the runner, not narrowed silently.
- **`record` is never automatic.** Running it after every landing would double
  every landing's test load, and the timing row it commits would itself land
  through the queue and trigger another record. It stays an explicit request
  on master.
- **Repeats are whole runs.** Each repetition is its own runner invocation
  with its own log and RAM disk, the property `bin/e2e-repeat.sh` existed for:
  the one failing run's evidence is never overwritten.
- **CPU load is the user's alone, by policy, not by proof.** Every caller of
  the agent API runs as the same user over the same HTTP surface, so no caller
  identity is unforgeable. The command line offers load only with `-as-user`,
  which agents pass only when the user asks, and the daemon refuses load from
  a workspace requester.
- **The runner refuses a request it does not fully understand.** A worktree
  older than the daemon reads a newer request with unknown fields; it refuses
  instead of running a narrower run than was asked.
- **`realtest.sh` is not a test suite.** It drives the live editor, so it
  stops routing through `bin/background.sh` and sets its own priority.
