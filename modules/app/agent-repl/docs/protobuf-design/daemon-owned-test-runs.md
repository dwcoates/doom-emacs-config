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
