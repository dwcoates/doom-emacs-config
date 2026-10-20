# Vendor notices and fault endings as one card

The feed drew one vendor failure twice: a notice bubble holding the prose
Claude Code wrote into the transcript ("API Error: …", headed "Notice from the
vendor's tooling, not the agent's answer."), then the turn's ending as a small
`vendor error · internal` pill that opened to the same sentence. The owner
found the pill ugly and asked for one consolidated look.

## Core design principles

- **A card's border is the color of the workspace status its event raises.**
  Owner, 2026-10-20: "if it results in a turquoise status for the workspace,
  then it gets the turquoise border. if it results in detached/disconnected
  status, then it gets blue border."
  - Consequence: the border is the marker FAMILY's color, because the family
    already follows `ladder.ResolveTurnFault`, the table the footer and roster
    raise their fault from (vendor fault turquoise, agent-repl fault blue).
  - A notice on its own raises no status, so it is the neutral family (grey).
  - Does not claim: that every neutral marker (interrupted, denied, plan
    failed) becomes a card. Those stay pills; only faults and notices are cards.

## Landed changes

### 2026-10-20: notices fold into fault endings; faults and notices draw as cards

- WHAT: `frontend.v1.FeedOutcomeMarker.notices` (repeated
  `frontend.v1.FeedOutcomeNotice`) carries the vendor notices of the event's
  turn. `frontend.v1.FeedResponseNotice.heading` is removed and replaced by
  `marker`, a neutral `frontend.v1.FeedOutcomeMarker` that heads a standalone
  notice card. `frontend.v1.FeedOutcomeVendorFaultExpansion.message` is drawn
  in the card's closed face.
- WHY: one fault is one card (owner: "a more consolidated look"); the card is
  tool-card width with a hairline border, and opens in place like a tool card
  ("an expanded view, rather than a toggle, to be consistent with other
  items").
- Consequences:
  - The daemon folds in its one write path (`resolver.upsert`), reading the
    feed's own stored rows, so the fold holds in either arrival order (notice
    before ending on the stream plane, notice after ending on the sidecar's
    transcript plane) and on every replay. No new per-turn state is kept.
  - A folded notice row is RETIRED (`FeedRow.removed`); live readers see the
    notice card drop as the ending card lands.
  - A notice whose words equal the expansion's vendor message is not
    repeated in `notices`.
  - Notices fold only into fault markers on the same feed as the ending; a
    notice of an interrupted or concluded turn stays its own grey card.
  - The resend action's rule is unchanged: a turn that drew a notice counts
    as having produced something.
