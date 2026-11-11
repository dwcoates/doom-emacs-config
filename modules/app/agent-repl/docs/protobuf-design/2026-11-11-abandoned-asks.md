# Abandoned asks

A question or consent card kept standing after the session that asked it was
gone (workspace `explanation-engine-remote-control`, 2026-11-11). The shim
holding the ask died without a teardown overnight, so the stored conversation
never said the ask ended. Every later daemon then stood the card from the
stored history. Every answer was refused `no_open_ask`, and every typed prompt
was routed to that dead ask, so the workspace could never be prompted again.

## Assumptions (autonomous design)

- Objective: an ask whose session is gone is settled everywhere, both in the
  stored record and in every daemon view, and the user is told the ask's
  session ended rather than that it was already answered.
- No new frontend component. The card's existing `expired` state draws an
  abandoned question, and the existing closed state draws an abandoned consent
  ask, so no look-and-feel change is made.
- Systems: proto, shim, store, daemon, Emacs input notice.

## Iteration sequence

`conversation.v1` (the leaf) first, then `store.v1`, which imports it.

## Landed changes

### `conversation.v1.AgentQuestionSuccess.abandoned` and `conversation.v1.AgentPermissionSuccess.abandoned`

- WHAT: a new empty arm on each ask's settle, meaning the session that held
  the ask ended before anyone answered it.
- WHY: the stored record had no honest way to say this.
  - `unanswered` means the agent stopped waiting and went on without a choice.
  - A user denial accuses the user of a refusal that never happened.
  - Both are claims nobody can support for an ask whose vendor process is gone.
- WHY AN ARM OF SUCCESS, NOT FAILURE: the failure arm means the ask could not
  be put to the user, but this ask was put.
- CONSEQUENCES:
  - Every consumer that switches on the settle (the feed's card drawing) gains
    an arm.
  - Consumers that only test "is this still `start`" need no change.
  - `conversation.v1.AgentPermissionDecision` deliberately stays without the
    arm, because no user can say it.

### `store.v1.GetLiveWorkSuccess.open_asks`

- WHAT: the store answers every ask in the session's lineage whose start it
  holds with no settle, as `conversation.v1.SessionOpenAsk` (the ask's start
  under the agent whose book holds it).
- WHY: an ask is an open obligation exactly like live detached work.
  - The shim already reconciles detached work at session start through
    `GetLiveWork`.
  - A shim that died without a teardown left its asks open in the record, and
    only a record-derived answer reaches them.
- WHY `SessionOpenAsk` IS IMPORTED, not respelled: it is already the
  "this agent's ask, as its start recorded it" message.
- CONSEQUENCES:
  - The store gains an `open_ask` table, kept by the same lifecycle
    transaction as the entry row.
  - The table is backfilled once from the stored entries, so asks already
    orphaned before this change are answered too.
  - The shim settles every answered ask as abandoned before it states its
    session's facts.

### `ask_session_ended` on `agentrepl.v1.AnswerPermissionError`, `agentrepl.v1.AnswerQuestionError`, `agentrepl.v1.UpdateQuestionDraftError` and `agentrepl.v1.SubmitPromptError`

- WHAT: a new empty refusal arm on each endpoint that delivers an answer to an
  ask.
  - The card was standing when the answer was sent.
  - The shim refused the answer `no_open_ask`, because the session that posed
    the ask no longer holds it.
  - The daemon settles the card abandoned through the same path a live settle
    takes, and answers this arm.
- WHY: the existing `ask_not_standing` arm drew "the ask was already answered".
  - That was false for an ask whose session ended.
  - The user could not tell why the workspace refused every answer and every
    prompt.
- WHY A NEW ARM, not a cause inside `ask_not_standing`:
  - `ask_not_standing` stays the arm for a card the daemon no longer stands.
  - The new arm is for a card it stood until this very refusal.
  - Two facts get two arms, and the client switches on the arm.
- CONSEQUENCES:
  - Emacs's wire decoders list every arm they accept, so each of the three
    decoders Emacs uses gains it (`lisp/wire-verbs.el`).
  - The webapp's refusal texts gain it too.
