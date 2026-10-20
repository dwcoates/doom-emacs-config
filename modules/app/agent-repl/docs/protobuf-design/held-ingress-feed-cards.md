# Held-ingress prompts drawn in the feed

A prompt sent while no daemon answered is written to the held-prompt ingress
(`$AGENT_REPL_STATE_DIR/held-prompts/`) and drawn nowhere in the webapp feed
until a daemon accepts it into its queue. On 2026-10-20 in workspace
`cee-transport-agnostic` the user sent "what's dmitry's comment" during a
daemon outage, saw no prompt in the feed, sent it again, and both copies were
delivered when the daemon came back (two entries, two idempotency keys). The
owner asked for held prompts to be drawn in the feed while the daemon is down.

## Core design principles

- **At every instant exactly one party draws a waiting prompt.** While the
  page's root feed stream is down, the page draws the editor's own list of
  ingress entries (pushed by Emacs through the web view host). Once the
  stream is live again (an `OpenFeed` answer applied), the daemon's held
  section is the only source, and the daemon lists the ingress entries it has
  read and not accepted. The switch is the page's own stream state, so the
  two sources never draw at once.
  - Consequence: the daemon must list un-accepted ingress entries in the held
    section, or a prompt vanishes between the reconnect and its acceptance.
  - Does NOT claim: that the page ever reads the ingress directory, or that
    Emacs's list is drawn while the daemon serves.

## Landed changes

### `frontend.v1.FeedHeldItem.ingress` and `frontend.v1.HeldIngressPrompt`

- WHAT: a new held-item arm for an ingress prompt the daemon has read and not
  accepted: the whole `said`, when it was written, and a daemon-composed badge
  whose arm says what the entry waits on (not yet offered or waiting its turn,
  no session, merge, cold gate, moving to another daemon, refused by a fault).
  No actions: the queue has not accepted it, so force, cancel, edit and fold
  cannot apply.
- WHY: the user chose the design where the daemon lists pending ingress
  entries over (a) deduplicating Emacs's cards by idempotency key, which still
  leaves a gap on reconnect and no key on replayed rows, and (b) a page-only
  change, which hides the prompt from the reconnect until acceptance (4 s in
  the incident).
- Identity: the item's FeedId is the entry's own, keyed by its idempotency
  key (`feedid.KindIngress`), because the turn does not exist before
  acceptance.
- Settling in one publication: the prompt handler claims the entry's key for
  the turn it mints before the queue sees the submission
  (`resolver.ClaimIngress`). The resolver drops a claimed entry from the
  section in the same publication that draws that turn as a held prompt or as
  a prompt row (`renderHeld`, `heldPromptRow`).
- Consequences accepted:
  - An entry answered as a duplicate (its key was already accepted) or a
    prompt the classifier coalesced into an earlier hold has no turn of its
    own to settle into; it leaves the section when the sweep drops it right
    after the queue answers, a publication later.
  - An entry whose directory names no registered workspace has no feed and is
    drawn nowhere.
