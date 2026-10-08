# A reply's quote is its own block, drawn only when the bubble is expanded

## The owner's request (2026-10-08)

- When a prompt is sent with a feed bubble selected, the quoted bubble's text
  is wrapped in a markdown code block, so it draws as a code block wherever
  the prompt is drawn as markdown.
- A prompt bubble's collapsed view shows only what the person typed. The
  quoted bubble appears only in the expanded view.
- The webapp must not find the quote by parsing the reply markers out of
  flat text. The structure is carried explicitly.
- Protobuf changes were pre-approved by the owner.

## What was there before

- The daemon built one text block for a reply: the reply preamble, the
  quoted bubble's markdown, the "My message" marker, and the person's own
  words, all flattened together.
- That one block was what the agent received, what the store recorded, and
  what every client drew.
- So nothing downstream could tell the person's words from the quote without
  parsing the markers back out.

## The design

### The quote is a block of what the person said

- `conversation.v1.UserContentBlock` gained a fourth arm, `quote`, holding a
  new message, `conversation.v1.UserQuoteBlock`.
- A reply's prompt is now the quote block followed by every block the
  person composed, unchanged.
  - The person's text blocks are no longer flattened into the quote.
  - Their images keep their own blocks and their own order.
- The quote block's text is the quote as the agent receives it:
  - the reply preamble naming what is quoted (a response of the agent's, or
    an earlier prompt);
  - the quoted bubble's markdown inside a code fence;
  - the "My message" marker introducing the person's words.

### Why a block arm rather than a field on the prompt

- Prompts are combined by concatenating their blocks.
  - The held-prompt queue folds one held prompt into another.
  - The classifier can coalesce a prompt into the one queued ahead of it.
- With the quote as a block, each quote stays in front of the words it
  introduces through any combination, and no combination has to choose
  between two quotes.
- A single quote field on the prompt could hold only one, so combining two
  quoted prompts would have had to drop one or refuse the fold.

### Why the record holds the composed text

- The daemon composes the preamble, the fence and the marker once.
- The shim delivers the block's text to the agent exactly as it delivers a
  text block.
- A client draws the same text as markdown.
- So the wording lives in one place, and what a reader sees on expanding a
  bubble is exactly what the agent was told.
- Recording only the raw quoted markdown would have needed the same wording
  composed twice: once in the shim for the agent, and once in the daemon
  for drawing.

### The fence

- The fence is a run of backticks one longer than the longest backtick run
  anywhere in the quoted text, and never shorter than three.
- So no code fence or inline code inside the quoted bubble can close it.

### The feed carries the quote as its own drawn block

- `frontend.v1.FeedUserPromptBlock` and `frontend.v1.FeedAgentPromptBlock`
  each gained a fourth arm, `quote`, holding a new shared drawn block,
  `frontend.v1.FeedQuoteBlock`.
- The daemon resolves a quote block into this arm, with its text verbatim.
- The webapp draws it as markdown, in its place among the blocks, and hides
  it while the bubble is collapsed.
- The agent-prompt arm exists so the two prompt block unions keep the same
  arms, as they always have.

### The held-prompt tray

- A held prompt carries the prompt record itself, so the tray reads the
  quote block directly.
- Its collapsed one-line view shows the person's words, and the quote
  appears when the card is expanded.

### Editors hold the person's words, never a quote

- A held prompt being edited is handed to the editor as the person's words
  alone (`agentrepl.v1.HostHeldPromptEdit.said`).
- The commit (`agentrepl.v1.EditHeldPromptCommit.said`) replaces those words,
  and the daemon puts the held prompt's quotes back ahead of them.
  - An edit changes what the person typed, never what they replied to.
  - A commit carrying a quote block is refused, because the editor is never
    handed one.
- A rolled-back prompt returns to the composer as the person's words alone
  (`agentrepl.v1.RollBackSuccess.prompt`).
  - A resend replies to whatever is selected when it is sent.
- Only the comments of these three fields changed.

### What reads the person's words

- Command recognition and the classifier read text blocks alone.
- A quote block is not text, so both see only what the person typed.
- So a command typed while a bubble is selected is recognized as that
  command.

## History replay keeps the split

- The shim records the prompt exactly as the daemon handed it over, quote
  block included, and the store keeps that record.
- A replay after a restart reads the same record back, and the feed resolver
  resolves its quote block into the feed's quote arm again.
- The vendor's own transcript holds the flattened text, but agent-repl's own
  prompts are never drawn from the transcript, so that copy never reaches a
  bubble.

## What changed in each message

- `conversation.v1.UserContentBlock`: new `quote` arm.
- `conversation.v1.UserQuoteBlock`: new message, one text field.
- `frontend.v1.FeedUserPromptBlock`: new `quote` arm.
- `frontend.v1.FeedAgentPromptBlock`: new `quote` arm.
- `frontend.v1.FeedQuoteBlock`: new message, one text field.
- `agentrepl.v1.HostHeldPromptEdit.said`,
  `agentrepl.v1.EditHeldPromptCommit.said` and
  `agentrepl.v1.RollBackSuccess.prompt`: comments only, saying a quote is
  left out.
- Every change adds an arm or a message, or changes a comment, so no package
  version changes.
