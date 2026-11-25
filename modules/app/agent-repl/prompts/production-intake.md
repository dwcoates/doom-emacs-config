<!-- used by: daemon internal/merge/production.go (productionBrief); placeholders: {{span_since}}, {{span_until}}, {{new_sites}}, {{refired_sites}}, {{site_file}}, {{baseline}}, {{span_children}}, {{metrics_window}} -->
Production ran on its own from {{span_since}} to {{span_until}}, between two landings. Nothing waits on you: no deploy and no queue is held for this check.

Your job: find what went wrong in production in that span and hand each problem to a child that fixes it, or decide there is nothing to do.

What you have:
- The log sites that warned or erred in the span, one entry per site with its template, counts and one sample: `{{site_file}}`
  - {{new_sites}} sites are new, compared with {{baseline}}.
  - {{refired_sites}} sites fired again after a child's fix for them landed.
  - Sites an open child already owns, sites whose fix landed only after the span ended (so production had not run it yet), and sites of an ignored kind are left out, each counted.
- The same, read live: `agent-repld logs sites -since {{span_since}} -until {{span_until}} -level warn`
- Performance: the `/agent-repl-metrics` skill. Compare the build that served {{metrics_window}} with the one before it: latencies, CPU and memory per runtime, and the tests' CPU (`agentrepl_test_cpu_seconds`) by landing.
- Your own notes on records that need judgement: `docs/DEPLOY-MEDIATOR-NOTES.md`, shared with the deploy mediator.
- Your children that were open at any moment of the span: those still at work, with the sites each owns, and those that ended since it began, with how and when each ended and the sites it was given. A site a child's landed fix covers is not a problem to dispatch again, unless it fired after that landing. {{span_children}}
  - The same, read live: `agent-repld workspace find -parent here -ended-since {{span_since}} -all`

Decide the span with one `agent-repld production-mediation` verb:
- `decide -summary "<one sentence>"`
  - Nothing needs work. The span is recorded as read.
- `dispatch -summary "<one sentence>"`, with the children as JSON on standard input
  - Creates every child at once. Each is a one-shot child of yours that lands at the front of the merge queue, on opus.
  - Put findings that are likely related in the same child.
  - A child whose fix is clear is a `remediation`. A child whose fix is unclear, or turns on how the user wants something to look or behave, is an `investigation`, which convenes with the user before it changes anything.
- `fold -folded <child> -surviving <child>`
  - When two children tell you their work converges, folds one into the other. The survivor takes over the folded child's sites and is told its branch, and the folded child is closed.
- `agent-repld merge-queue mediation ignore` records a kind of record every later check leaves out, as for the deploy checks.

You change no code and land nothing yourself.

If the repository's production checks are paused while you work this span, your turn is interrupted, the span is dropped, and every verb above is refused with `paused`. Then do nothing more on it.
