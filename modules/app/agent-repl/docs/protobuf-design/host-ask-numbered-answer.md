# The host view's standing ask (numbered answers from the composer)

The owner wants a lone number typed into the Emacs composer, while a question
or permission card stands, to answer that card at once with the matching
choice. No return key is needed. On a multi-select question the number toggles
the option's tick and `C-RET` confirms. When the first non-whitespace character
is not a digit, or no card stands that the number could match, the digit is
inserted as ordinary text.

Emacs had no view of standing asks: the composer could only send text, which
the daemon reads as a typed answer. A typed number picks a question's option
but denies a permission with the number as the reason, and it cannot toggle a
multi-select tick.

## Landed changes

### `agentrepl.v1.HostWorkspace.ask` (2026-10-16)

- WHAT: `HostWorkspace` gains `optional HostAsk ask = 6`. It names the oldest
  open question or permission card on the root feed, giving its row
  (`frontend.v1.FeedId`) and either the shown question (text plus options,
  as a `single_select` / `multi_select` arm) or the permission's standing
  offer (by presence).
- WHY: Emacs must know, per keystroke, whether a number matches a choice. It
  must also know which verb and which echoed values answer that choice.
- Decision on the answer path: Emacs answers on the card's existing verbs
  (`UpdateQuestionDraft` pick / confirm, `AnswerPermission`) and names the row.
  It does not use a new endpoint, and it does not send the digit as
  `SubmitPrompt` text. The reasons the alternatives lost:
  - `SubmitPrompt("2")` on a permission means "deny with reason 2". On a
    multi-select it means "answer with option 2 alone". It also cannot confirm
    a multi-select.
  - A new "answer standing ask by number" endpoint would duplicate the two
    existing verbs. The webapp's clicks already go through those verbs, so
    with them Emacs is a second clicker on the same card.
  - Naming the row makes a stale view safe by construction. A card that
    settled is refused as not standing, and the answer can never land on a
    different card.
- Consequences:
  - The permission buttons' numbering (1 allow once, then "always allow" if
    offered, then deny) is a contract stated on `HostAskPermission`. The
    webapp draws those numbers on the buttons, and Emacs maps them. Two
    clients implement one documented order.
  - A number on a single-select is a CLICK. Clicking the option that is
    already chosen clears it, as the card's own click does.
  - The daemon must republish the host view whenever the root feed's ask
    cards change. The feed resolver signals this outside its lock (an event
    the server follows), because composing the host view reads the feed's
    ask snapshot.
  - "Oldest" is the ask's open order, the same rule the daemon's
    typed-answer routing (`feed.StandingAsk`) uses. A typed number and typed
    text therefore address the same card.
