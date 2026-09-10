# Landing 3 follow-up (dispatch after the pause → ack → merge of overhaul/landing-3)

Small remediation brief for src/feed/cards/tool-call.ts (+ test/feed/cards/tool-call.test.ts):
- `FeedToolCallReturned.form.none` (FeedToolCallNoOutput): omit the output section entirely
  (no dashed divider, no output block); unset form stays MalformedView.
- `FeedToolCallInput.form` oneof {command | path | query}: apply the EXISTING treatments —
  command = shell line (`pre.cmd.bash-input` look), path = muted file path (`.file-path` look),
  query = the query treatment; UNSET = plain text. The client still knows no tool.
- Fixtures/fake daemon: integration suite covers the new arms (feed-kinds cases).
- Enumerate the new arms from the schema so a later arm fails loudly.

Also in the same remediation brief (rulings Q8/Q10, 2026-08-29):
- src/feed/rows/turn-ended.ts: `FeedTurnEndedErrored.headline` (FeedTurnErrorHeadline{text}, REQUIRED) is drawn
  verbatim; DELETE the 16-sentence client table; only the retry countdown stays client-ticked from
  retry_after_ms; an unset headline is MalformedView. Update tests (enumerate arms; assert verbatim headline).
- src/feed/rows/subagent.ts (+ src/feed/cards/shell.ts if it grew one): REMOVE the confirm path on
  detached stops — the daemon answers `confirm_required` only to the turn target; a detached stop that
  receives it is drawn as an ordinary refusal (`.refusal[data-arm="confirmRequired"]`) and logged at warn.
- Q9 stands as built: FeedPageErrorHeadline.tone validated against render-colors.json `colors` (+ "none").
- src/feed/asks/cold-gate.ts: `FeedColdGateResolvedCompact.scope` (SessionCompactScope) is drawn on the
  resolved trace ("compacted <scope words> with <model>", using the same scope labels as the submenu);
  never UNSPECIFIED (MalformedView). (Owned by the cards-B+asks agent if still building; else remediation.)
- FeedPageErrorHeadline.tone: comment now names render-colors.json `colors`; feed-core already validates so.

Policy relay (webapp.md "Landing 3 relay"): a per-bubble prompt to a subagent may be refused this wave
(shim `not_deliverable`) and the detach control may be refused (`unsupported`); render refusals honestly as
the daemon states them (the composer's existing generic refusal path); never disable controls client-side.
The cold-gate scope item is the cards-B+asks agent's, not the remediation agent's.

DROPPED: shell not_observed (never reaches frontend.v1; unset spool + settled arm is the mapping).
