# A held prompt shows one badge

The owner's report (2026-10-08): the doom workspace's held prompt showed two
badges, "after this turn" and "after the merge". The contract composed one
badge per standing fact. The owner's ruling is that a held card never shows
two badges, as an invariant.

## Landed changes

### 1. `HeldPrompt.badge` replaces `HeldPrompt.badges`

- **What changed:** `repeated HeldPromptBadge badges = 14` is retired
  (reserved). `HeldPrompt.badge = 21` is the one badge, always set, and
  `HeldPrompt.notes = 22` carries every other standing fact as one sentence
  each. `HeldPromptBadge.stands_for` names the fact the badge shows, so a
  frontend colors it without re-ranking the facts.
- **Why:** one badge, structurally: the wire cannot carry two.
- **The ranking (owner, 2026-10-08):** the edit, then the hold arm, then the
  classification verdict. The verdict's confirmation and a coalescence never
  claim the badge; they and every outranked fact are notes.
- **Consequences:**
  - The daemon composes the badge and notes in one place
    (`resolve/holds/tray.go`).
  - The webapp tray draws one pill colored by `stands_for` and the notes in
    the expand-only details; its check that the badge count matches the
    facts is gone, because the field holds one.
  - The `accepted` and `coalesced` badge colors are retired with their
    badges.
