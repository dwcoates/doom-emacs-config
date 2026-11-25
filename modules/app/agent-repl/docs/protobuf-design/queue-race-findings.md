# Queue race findings handed to the deploy mediator

The merge queue's gates fail about half the time, and many failures are races in
production code that pass when the repair reruns them. A repair then treats the
race as resolved, and the same race fails merge after merge. The owner ruled
(2026-11-25) that the daemon finds these races itself, and the deploy mediator
dispatches their fixes as it dispatches a deploy-caused problem.

## Assumptions (autonomous design)

- Objective: every race a gate exposes reaches a remediation that fixes it at its source.
- No frontend component: the findings reach the mediator through its brief, and the mediator decides through the existing `merge-queue mediation` verb.
- Systems: the test runner (testrun), the daemon (merge gate, RunTests relay, deploy mediation, store), the CLI.
- Non-additive changes: none. Every change is a new optional field, a new repeated field, a new message or a new error arm.

## Iteration sequence

1. The runner's per-unit inputs digest (testrun.v1).
2. The decision's naming of race findings (agentrepl.v1.DeployProblem).
3. The refusal of a decision naming an unknown finding (agentrepl.v1.UpdateMergeQueueError).

## Landed changes

### testrun.v1.TestRunUnitEnded.inputs_digest

- What: each unit's verdict carries the reuse key digest the run confined the unit to; unset for an unconfined unit.
- Why: "no change to the code it covers" must be decided by the daemon, not by an agent. The reuse key is already the digest of every input the sandbox lets the unit read, so equal digests prove the tests ran the same code. A git diff of the branch cannot say what a test covers, and a hand-kept mapping would drift.
- Consequences:
  - The gate learns per-test outcomes from the runner's events, written to a file (`--events-file`), since the test service relays only lines. The archive text stays what the repair reads.
  - A batch unit's digest covers all of its members, so a targeted rerun that forms a different batch carries a different digest and finds no race. Detection misses rather than misfires there; the next gate, with the same batches, still finds it.
  - Infrastructure units carry no digest and never yield a finding.

### agentrepl.v1.DeployProblem.race_findings and QueueRaceFindingRef

- What: a dispatched problem may name the race findings it is; a finding is named by suite and test.
- Why: the daemon must know which findings a dispatch took on, so it lists them as at work rather than open (reusing the one remediation at work on that test) and marks them fixed when the fix deploys. Evidence lines are log records and cannot name a test.
- Consequences:
  - One finding per (repository, suite, test). Go tests are keyed by their top-level name, since a subtest's failure fails its parent.
  - A finding taken on stays at work across recurrences; its recurrences are appended to it.
  - A fixed finding that recurs is open again, its earlier fixes counted.

### agentrepl.v1.UpdateMergeQueueError.unknown_race_finding

- What: a decision naming a finding its repository does not hold is refused, naming the first such finding.
- Why: a mistyped name would otherwise be taken on as nothing, and the real finding would stay open with no word to the mediator.

## Rejected alternatives

- Parsing "runs (inputs X)" lines from the archive: the subject labels do not map one-to-one onto unit ids (pooled webapp units), so the join would be a parser of display text. The events are typed.
- Refusing an approval while findings are open: the owner ruled (2026-11-19, 2026-11-20) that the mediator alone decides a check. Open findings are instead put to it on every check until it dispatches them.
