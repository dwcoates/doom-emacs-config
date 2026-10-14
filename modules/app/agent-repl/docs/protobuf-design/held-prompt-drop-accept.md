# Held prompts lose the Accept action

A held prompt the classifier holds for the running turn's end offered an
`Accept` button. Accepting changed nothing about delivery: it only recorded
that the user had seen the hold, and the card swapped the button for a quiet
marker. The owner ruled (2026-10-13) that the button is useless, since doing
nothing already means accepting the wait, and asked for it to be removed.

## Landed changes

### Accept removed from the contract (2026-10-13)

- **What changed.** Removed the accept arm of `UpdateHeldPrompt`'s request
  and its message, the refusal arm an accept on any other verdict answered
  and its message, and the `accepted` field of the hold-for-turn-end verdict
  with its wrapper message. The comments in `feed_held.proto` that listed
  accept among a held prompt's actions now list only force and cancel.
- **Why.** In the owner's words: "ditch the accept button. sounds useless."
  The action only recorded that the user saw a hold, and nothing read that
  record except the card that drew it.
- **Verification.** A repository-wide search found the accepted flag read
  only by the daemon's prompt queue and store (`daemon/internal/promptqueue`,
  `daemon/internal/wsm`), the held-section resolver, and the webapp card.
  The classifier, the shim and the elisp read none of it. That rules out the
  concern that the classifier learned from acceptances.
- **Consequences.**
  - The daemon's held-prompt store keeps its `accepted` column for now, under
    the store's two-deploy rule for breaking layout changes
    (`daemon/internal/wsm/migrate.go`). This landing stops reading it and
    writes 0 on every write path, including the classification, edit and
    coalescence updates. That way the previous build, whose decoder refuses
    an acceptance beside any verdict but hold_for_turn_end, keeps working
    across a hand-over.
  - OUTSTANDING: once no running build reads the column, a breaking
    migration step drops `held_prompts.accepted`. It also deletes the `= 0`
    writes in `daemon/internal/wsm/heldprompts.go` and the column's
    "RETIRED" note in `daemon/internal/wsm/schema.go`.
  - The fold, edit and re-hold paths no longer reset an acceptance, because
    there is none to reset.
  - The UpdateHeldPrompt dispatch now has two actions, release and drop.
  - Field numbers 5 (request arm), 7 (error arm) and 2 (verdict field) are
    freed without `reserved`, following the repository's convention of
    never reserving: every producer and consumer ships from this one tree.
