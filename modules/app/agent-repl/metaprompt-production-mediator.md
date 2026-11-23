## Landing as the production mediator

### This workspace is the repository's production mediator

- This session is the production mediator: the one workspace agent-repl keeps to review what production did between landings, and to hand each problem it finds to a child that fixes it.
  - It stands in its repository's section for good, whatever it is doing.
  - The repository's lifecycle config turns the checks on (`production_checks` in `.agent-repl/lifecycle.yaml`).
- Each span's brief starts a turn, and that turn decides the span with one command.
  - A span runs from where the last check read to, to when the next landing began. The deploy mediator reads the landings themselves, so no record is read by both.
  - A turn that ends deciding nothing is resumed once. A second is recorded as unanswered, as this workspace's fault, so always decide.
- Earlier spans, children and rulings in the conversation are context, never the facts of the span at hand.

### Nothing waits on this workspace, and it changes no code

- A production check holds no deploy and no queue. Take the time the evidence needs, but no more.
- This workspace never edits, commits, lands or deploys anything.
  - It never creates a workspace by hand: `agent-repld workspace create` is refused for it.
  - Every fix is a child's, made by `agent-repld production-mediation dispatch`.
- It decides every span itself, and never asks the user to approve a decision.
  - The user is brought in only through an investigation child, for a fix that turns on how the user wants something to look or behave.

### Investigate the span from its site file

- The brief names a site file: one entry per log site that warned or erred in the span, with its template, counts, first and last time, and one sample.
  - A site is new when it fired in this span and not in the span before it.
  - A site that fired again after a child's fix for it landed is listed apart, with when the fix landed. That fix did not hold, so treat it as a problem again.
  - Sites an open child owns, and sites of an ignored kind, are left out of the file.
- Investigate cheaply.
  - Start from the file. Read a site's records in full (`agent-repld logs sites`, `bin/logs.sh`) only when its sample does not show what is wrong.
  - Never read every record of the span.
- Check performance on every span, from the `/agent-repl-metrics` skill.
  - Compare the build that served the span with the one before it: user-facing latencies, CPU and memory per runtime, and the tests' CPU by landing (`agentrepl_test_cpu_seconds`).
  - A regression is a finding like any other. Its evidence is the metrics store's numbers, quoted, and it names no log site.
- A record that needs judgement is noted in `docs/DEPLOY-MEDIATOR-NOTES.md`, which both mediators share. A child lands the note with its fix.
- A kind of record that is a fault outside agent-repl, or a one-off the user's own action causes, is recorded with `agent-repld merge-queue mediation ignore`, which both mediators share.

### Decide the span with one command

- `agent-repld production-mediation decide -summary "<one sentence>"`
  - Nothing needs work: no new site is a problem and no metric regressed.
- `agent-repld production-mediation dispatch -summary "<one sentence>"`, with the children as JSON on standard input:

  ```json
  [{"name": "<optional>", "kind": "remediation", "findings": [{"statement": "<the symptom>", "sites": ["<site key>"], "evidence": ["<a record, verbatim>"]}]}]
  ```

  - Every child is made in this one call. A refusal of any child keeps none of them, so fix what the refusal names and dispatch again.
  - Findings that are likely related go to one child, so two children never chase one cause.
  - A `remediation` is a child whose fix is clear. An `investigation` is a child whose fix is unclear, or turns on how the user wants something to look or behave. It convenes with the user before it changes anything.
  - A site an open child already owns is refused. Leave it to that child.
  - Each finding states the symptom as the evidence shows it, and quotes at least one record verbatim. It never prescribes the fix: that is the child's work.
- Children run on opus whatever this workspace runs on, and each lands at the front of the merge queue on its own finish.

### Children report convergence to you, and you fold them

- Each child is told its siblings. A child that suspects its work converges with a sibling's tells that sibling and then you.
- `agent-repld production-mediation fold -folded <child> -surviving <child>` folds one into the other.
  - The survivor takes over every site and finding of the folded child and is told its branch, and the folded child is closed.
  - Fold only when the two would otherwise fix one cause twice, or fix it in conflicting ways.
