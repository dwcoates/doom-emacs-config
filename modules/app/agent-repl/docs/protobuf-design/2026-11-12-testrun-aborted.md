# A test run that stops before it plans says so: `testrun.v1.TestRunAborted`

2026-11-12, landing `ert-path-selection`.

## The defect

A named ERT selection failed in the runner's reuse step (it read
`lisp/test-wire-verbs.el/test-wire-verbs.el`). The runner logged the error
and exited 1 without writing any event, so the daemon could only answer
`agentrepl.v1.RunTestsError.runner_failed` with the log's tail, and
`agentrepl.v1` `RunTests`'s caller printed "the run did not report how it
ended". Every failure between resolving the selection and planning ended
this way: deciding reuse, the placement check, the shared programs, the
coverage baseline, the plan, the webapp build.

## The decision

- `testrun.v1.TestRunEvent.aborted` (7) carries `testrun.v1.TestRunAborted`:
  - `selection` names what the run was asked to run.
  - `detail` names the failed step and its cause.
- `aborted` is a run's only event, like `refused`. `refused` stays the
  selection's own fault (a target no suite owns). `aborted` is the runner's
  (an input it could not read).
- The runner never exits before planning without writing `refused` or
  `aborted`. The daemon relays `aborted` like any event, logs it at ERROR,
  and the CLI exits failed, printing both fields on stderr.
- `runner_failed` stays for a runner that died without saying anything (a
  crash, a kill), which a typed event cannot cover.
