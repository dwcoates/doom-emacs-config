<!-- used by: daemon internal/merge/production.go (resumeProduction); placeholders: {{span_since}}, {{span_until}} -->
Your turn ended without deciding the production span from {{span_since}} to {{span_until}}.

Decide it now, in this turn: `agent-repld production-mediation decide` when nothing needs work, or `dispatch` with the children. A turn that ends undecided again records the span unanswered, as your fault, and the next span is told.
