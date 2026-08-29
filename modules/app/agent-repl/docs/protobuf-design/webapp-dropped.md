# Webapp — instructions deliberately NOT carried into docs/overhaul/webapp.md

Final-audit triage, 2026-08-29. The removals ARE noted in webapp.md's
2026-08-29 slate (chess widget, host hooks, search, nav, copy chords,
counter chips, outage queue, gns fold, catalogue, meta stripping, ungated
banner, title/log trivia) — informed removals. Below are the sub-details
that got NO doc text; absence is silence, not prohibition.

- The counter chips' TURN-AGED RETENTION rule (finished work lingers
  until the turn ages out): dropped with the chips; the footer live-work
  chips show live work only, and no lingering rule was carried.
- Search's fold/tab reveal semantics (opening closed folds and inactive
  merge tabs on match): dropped with search itself.
- The isearch interaction details (match classes, current-match
  stepping): dropped with search.
- The capped-section numeric caps (N-line preview sizes): capping is
  blessed generically; numbers unstated.
- The TLDR-tree detection heuristics (looksLikeIntendedTree): the
  re-render is blessed generically; the sniff heuristics are the
  implementer's.
