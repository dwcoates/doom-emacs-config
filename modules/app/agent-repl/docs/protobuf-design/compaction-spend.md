# The cold gate's compaction states what it cost

Owner request, 2026-10-17: after a compact option is chosen at the cold gate,
the footer's activity line names how many tokens the compaction took and on
which model, and the token figure wears the cold gate's gradient.

## Landed shape

- `conversation.v1.SessionCompactionProgress.spend`
  (`conversation.v1.SessionCompactionSpend`): the summarizer's model, as the
  summarizing query's opening stated it, and every token that query spent
  (input, cache writes, cache reads and output). Set from `summarized` on,
  because only the summarizing query's result states it.
- `frontend.v1.FooterColdGateFiguredLine`: a line around one cold-gate token
  figure, in `lead` + `figure` + `tail` parts.
- `frontend.v1.FooterStatusActivityCompaction.parts` and
  `frontend.v1.FooterActivityTransientCompactionConcluded.parts`: the
  compaction line in parts whenever it names the spend, so the client colors
  the figure with `coldGateFigureColor`.

## Decisions

- The figure's window fill is measured against the window the gate's own
  figure was measured against (the context chip's window), so the spend of
  re-reading N tokens reads the color the gate gave those N tokens.
- The model is the vendor's statement of what ran, not an echo of the
  request, so a default-model summarizer is still named.
