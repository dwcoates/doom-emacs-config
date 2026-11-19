# A pull request's whole CI run, told once per outcome

A workspace following a pull request was told one notice line per finished
check run (`agentrepl.v1.PullRequestActivity.check_finished`), and only once
every run on the head had finished. A pull request with about fifty checks
(explanation-engine) produced about fifty lines, and nothing ever said whether
the run as a whole passed or failed. Some repositories also run a check that
fails until a person acts (explanation-engine's `review-or-self-certified`),
which made "all checks passed" unreachable before approval.

Designed autonomously (no collaborative design was asked for).

## Assumptions

- The subscriber wants the run's outcome, and the names of the checks that
  failed, never each pass. Evidence: the request, and how the agents act on a
  notice (they fix a failure, they wait on a pass).
- The run is the set of check runs GitHub reports for the commit; the legacy
  commit-status API is not read, as before.
- Gate checks are a property of the repository, not of the subscriber.

## Iteration sequence

1. The activity arm the outcome is told in.
2. The per-repository gate check setting.

## Landed changes

### 1. `checks_concluded` and `merge_group_checks_concluded` replace the per-run arms

- WHAT: `agentrepl.v1.PullRequestActivity` reserves tags 7 and 12
  (`check_finished`, `merge_group_check_finished`) and gains
  `checks_concluded = 13` and `merge_group_checks_concluded = 14`, each an
  `agentrepl.v1.PullRequestActivityChecksConcluded`: the commit, and a oneof of
  `passed` (the count judged) and `failed` (each failed run's name and
  conclusion, reusing `agentrepl.v1.PullRequestActivityCheckFinished`).
- WHY: one short notice per outcome. A failure is told as soon as one run
  fails, while the rest still run, so the agent can start on it at once.
- WHY NOT KEEP PER-RUN NOTICES OPT-IN: no consumer acts on a single pass, and
  the failed list carries every name a failure needs. An opt-in would be a
  second vocabulary for the same fact, and it was the per-run stream itself
  that made notices long.
- WHY NOT AN OUTCOME ARM WITH A STATE ENUM: per the skill, the arm is the
  outcome.
- Consequences:
  - The daemon's told snapshot records the outcome last read per commit
    (`daemon/internal/prwatch/diff.go` `told.ChecksOutcome`,
    `told.GroupChecksOutcome`) instead of each run's id. Rows written before
    this change carry a `checks` map the decoder ignores, so a live
    subscription whose run already finished is told its outcome once after
    the deploy: a new kind of item, never a repeat.
  - Telling only on a change of outcome makes "one notice" structural: a
    further failure in a failed run changes nothing. A re-run of every failed
    check makes the run undecided, which is recorded, so its next outcome is
    told whichever it is.
  - A check run registered after the run was told passing (a workflow started
    by another's completion) makes the run undecided again, and its outcome is
    told again. Read from check runs alone, a run cannot know a workflow that
    has not registered yet; check suites were rejected because apps that
    never run anything leave suites queued forever, which would make a pass
    unreachable.

### 2. `repoconfig.v1.LifecycleConfig.pull_requests.gate_checks`

- WHAT: `optional repoconfig.v1.PullRequestFollowing pull_requests = 13`,
  holding `repeated string gate_checks`, check run names left out of every
  commit's outcome.
- WHY A SETTING, NOT BRANCH PROTECTION: branch protection's required checks
  usually include the gate itself, so they cannot tell a gate from a test;
  reading them needs administrator rights the `gh` login may not hold; and a
  failing check that is not required is still worth telling.
- WHERE IT IS READ: from GitHub, from the file at the pull request's head
  commit (`daemon/internal/prwatch/read.go` `reader.gateChecks`), through one
  GraphQL query (`LifecycleConfigAt`), because a REST contents read of a
  missing file is a 404 the GitHub client records at ERROR, on every new head
  of every repository without the file. A commit's file never changes, so
  each commit's answer (an unusable file included) is kept for the daemon's
  life; a failed call is not.
- Consequences: an unusable file at the head fails the subscription's read
  with the file named, until a commit fixes it. The file is parsed with
  `lifecycleconfig.Parse`, so the rest of the file must be valid too.
