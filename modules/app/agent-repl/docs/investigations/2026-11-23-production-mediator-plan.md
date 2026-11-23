# Production mediator: plan (owner request, 2026-11-20)

A sibling of the deploy mediator
(`docs/investigations/2026-11-14-deploy-intake-plan.md`), built on the same
engine and configured differently. It is written for review before it is
built. The log-site keys it rests on landed on master on 2026-11-23
(`merge(master): log-site-keys`).

## What the owner asked for

- A PRODUCTION MEDIATOR, prompted after each deploy, that never holds the
  queue or blocks a deploy.
- It reviews what production logged between deploys: the warnings and
  errors that are new since the previous window, and performance
  regressions between the two versions.
- A fix it judges clear is dispatched to a child workspace that lands as a
  one-shot at the front of the merge queue.
- A finding whose fix is unclear, or that needs the owner's opinion on the
  user experience, goes to an investigation child, which convenes with the
  owner and then lands at the front the same way.
- Neither the mediator nor any child commits to master or bypasses the
  queue.
- Both mediators take their model from configuration, and this repository
  sets both to `fable`.
- One implementation serves both mediators, configured per role.

## Owner rulings

1. Each mediator's metaprompt is its own text, never a copy of the other's
   (2026-11-20).
2. Every log helper in every runtime takes a static literal template with
   named placeholders, and every record names its site and template
   (2026-11-20; landed 2026-11-23).
3. The daemon keeps, per deploy, the sites that fired, with their counts,
   first and last times and one sample, for at least the last three deploys
   (2026-11-20; landed 2026-11-23).
4. Site keys are required at every level (2026-11-23).
5. Children are created all at once in one daemon call. Each child is told
   its siblings, and findings that are likely related go to the same child
   (2026-11-20).
6. A child contacts a sibling only when it suspects their work converges,
   and then tells the production mediator, which may fold one child into
   the other (2026-11-20).
7. A child's finish strategy is recorded when it is created, and a finish
   that names a conflicting placement is refused (2026-11-20).
8. THE WINDOWS (2026-11-23). The production mediator reviews what was logged
   from the moment a landing finishes until the next landing starts. The
   deploy mediator reviews the landing itself, from its start to its finish.
   The two windows meet at those two instants, with no overlap and no gap.
9. The children run on `opus` by default, never inheriting the mediator's
   model (2026-11-23).
10. The mediator's effort is configurable alongside its model (2026-11-23,
    on the recommendation).
11. Both mediators share the ignored record kinds and
    `docs/DEPLOY-MEDIATOR-NOTES.md` (2026-11-23).

## Design

### One engine, two roles

A `mediatorRole` value in `daemon/internal/merge` fixes everything that
differs between the two:

| Aspect | Deploy | Production |
|---|---|---|
| Workspace name | `deploy-mediator` | `production-mediator` |
| Landing profile and metaprompt | `deploy_mediator` | `production_mediator` |
| Brief templates | `deploy-intake`, `deploy-remediation-restored` | `production-intake`, `production-child-restored` |
| Window | landing start to landing finish | previous landing finish to this landing start |
| Holds the queue | yes | never |
| Its fixes | dispatched to itself, landing directly | dispatched to children, landing at the queue's front |
| Escalation and chain | yes | none |

The rest is shared and moves out of the deploy-only files: the standing
lifecycle (`standingchecks.go`), reading the logs, the digest, the ignored
kinds, `classifyFindings`, telling a turn and awaiting it, resuming once, and
the metrics spans. Each shared function takes the role, so a new behavior
that differs between the two is a field of the role, never a branch on its
name.

### The windows

Each landing records two instants: when it started and when it finished.
The deploy mediator's window becomes that landing's start through its
finish; it reads from where its last check read to today
(`windowStart`, `daemon/internal/merge/intakefacts.go:427`), so this changes
the deploy mediator as well.

When landing N finishes, the production mediator is owed a check of the
window that closed when landing N started: from landing N-1's finish to
landing N's start, which is the time version N-1 served on its own. The
check compares that window's sites against the window before it, so a site
is new when it fired in this window and not in the previous one.

A daemon crash with no landing closes no window. The crash is the deploy
mediator's, as today, and the production window runs on through it.

### The store

The mediation rows split in two:

- a generic CHECK row per role and repository: when it was owed, the turn it
  was told, its window, and its decision;
- a deploy-only HOLD row: the chain, the dispatches, the escalation, and
  the fixer's turn.

A production check has no hold row, so it cannot hold a chain.

A new table records each production child: its name, its finish strategy,
and the site keys it owns. A site an open child owns is never dispatched
again, and leaves the brief's diff until the child lands or closes.

### The brief

The brief stays small. The daemon computes the diff between the two windows
with the same code that answers `agent-repld logs sites`, and attaches it as
a file:

- the sites new in this window, at warning level or above, with their
  template, count, first and last time, and one sample;
- the sites a child fixed earlier that fired again, with when the fix
  landed;
- leaving out the ignored kinds and every site an open child owns.

For performance, the brief points at `/agent-repl-metrics` and names the two
versions to compare: CPU and memory per runtime from the deploy-cycle
metrics, and the test CPU metrics (`agentrepl_test_cpu_seconds`) by landing.

The mediator may read further through `agent-repld` itself.

### Children

- `agent-repld production-mediation dispatch` takes the findings as JSON on
  stdin and creates every child in one call. Each child is a one-shot child
  of the mediator, lands into the main branch at the queue's front, and runs
  on `opus` at the default effort.
- Each finding names the site keys it covers and whether its fix is clear.
  A clear fix creates a remediation child. An unclear one creates an
  investigation child, whose brief tells it to convene with the owner before
  it changes anything.
- Every child's brief names its siblings and the sites each one owns.
- `agent-repld production-mediation fold <from> <into>` moves the folded
  child's sites to the surviving child, tells the survivor the folded
  child's branch, and closes the folded child.

### Finish strategy

`workspace create` gains `-finish front`, stored on the workspace.
`workspace finished` reads the stored strategy. A finish that names a
different `-placement` is refused, and the refusal names the stored one.

### Roles on the workspace

`wsm.Workspace.DeployMediator` (`daemon/internal/wsm/types.go`) becomes a
role enum: none, deploy mediator, production mediator.

- `refuseDeployMediatorCreate` (`daemon/internal/server/workspaces.go`)
  keeps refusing creates by the deploy mediator only.
- The production mediator may create only through
  `production-mediation dispatch`.
- No production child may land except through its stored finish.

### Contract changes

- `repoconfig.v1.ProductionChecks`, a new section of
  `repoconfig.v1.LifecycleConfig`; its presence turns the checks on.
- `model` and `effort` on both `repoconfig.v1.DeployChecks` and
  `repoconfig.v1.ProductionChecks`. An absent model keeps today's default.
- A new arm `SessionProfileAgentLandingProductionMediator` in
  `proto/src/shim/v1/endpoint_start_session.proto`, with
  `metaprompt-production-mediator.md` registered in
  `agent-shim/claude/shim/src/metaprompt.ts`.
- The role enum on `workspace.Workspace`, replacing the deploy mediator's
  bool.
- `finish` on the create request, and the refusal of a conflicting
  placement.
- The production-mediation dispatch and fold requests and their refusals.

This repository's `.agent-repl/lifecycle.yaml` then sets `model: fable` on
both sections.

### Metrics

The production checks count how each check ended (nothing new, children
dispatched), how many children each dispatched and how each child ended, and
how long each check took. `CATALOG.md` gains each one.

## Order of work

1. The contract changes, landed together.
2. The role engine: move the shared code under the role, with the deploy
   mediator's behavior unchanged except its window.
3. The deploy mediator's window becomes its landing's start through its
   finish.
4. The store split and the child table.
5. The production role: its check, brief, diff file, and metaprompt.
6. Dispatch, fold, and the stored finish strategy.
7. Model and effort from configuration, and `model: fable` in this
   repository's lifecycle.
8. Metrics and `CATALOG.md`.

Each step lands through the merge queue with the tests for what it adds or
changes.
