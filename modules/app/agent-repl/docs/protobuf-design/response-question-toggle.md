# Response question toggle

A final response's bubble gets a question-mark control beside its copy
control. Pressing it moves the keyboard to the composer, sets the composer's
draft aside (saved to its history, so the up arrow brings it back), and makes
the next prompt sent a question about that response, answered in a dedicated
workspace. Pressing it again withdraws the question and puts the draft back.

## Assumptions (autonomous design)

- **Dedicated workspace.** The question is answered in a new CHILD workspace
  forked from the asking workspace's conversation (`context_origin.fork`,
  parent = the asking workspace), so the answering agent holds the whole
  conversation the response came from. Its first prompt is the response,
  quoted, then the user's words. A fork never crosses repositories, so for a
  doom workspace the answer lands in a doom workspace.
- **The daemon is the only holder** of the question, exactly as it holds the
  feed selection and the response highlight. The webapp only reports clicks
  and draws the pushed state; Emacs only edits its composer on pushed
  transitions.
- **The fork is not selected**, per the standing rule that no creation moves
  the user; its tab arrives once created.

## Landed changes

### agentrepl.v1.AskAboutResponse (new endpoint)

- WHAT: a toggle rpc naming one final response's row. The daemon computes the
  toggle: the asked row withdraws, any other final response asks (moving a
  standing question). Refuses a row that is no settled final response.
- WHY: a click names what was clicked; deciding the new state on the client
  would let two clients disagree with the holder.

### frontend.v1.FeedResponseQuestion (root feed watch, field 7)

- WHAT: state push (`none` / `asking{row}`) replayed on watch accept.
- WHY: the pressed look must survive a page reload and agree with the holder.

### agentrepl.v1.HostWorkspaceResponseQuestion (host watch, field 20)

- WHAT: an event with four transition arms: `asked`, `moved`, `withdrawn`,
  `taken`.
- WHY: Emacs owes a different composer edit per transition (set aside, keep,
  restore, let go); naming the transition means Emacs never diffs states.
  An event, never replayed, because replaying would repeat a composer edit.
- CONSEQUENCE ACCEPTED: an Emacs that restarts while a question stands has no
  set-aside draft to restore on withdrawal. The draft is still in the
  composer's history (the asked transition saved it there).

### agentrepl.v1.SubmitPromptQuestionForked (SubmitPromptSuccess arm 7)

- WHAT: a user prompt to the root feed sent while a question stands is taken
  by the question: the daemon starts a background child-fork create and
  answers with the row and the create's op id.
- WHY: the diversion is decided where the question is held, in the same call
  that takes it, so a send can never race a toggle into the wrong workspace.
- THE OP ID IS THE PROMPT'S IDEMPOTENCY KEY, so Emacs registers its
  mutation-progress callbacks under the key before sending whenever its copy
  says a question stands. Only the progress messages depend on Emacs's copy;
  where the prompt goes never does.
- Precedence: a standing question outranks a response highlight and the
  selection. The selection is left standing (it was not what the prompt was
  sent with).
- An unresolvable asked row (rolled back) refuses the send as
  `reference_response_unresolvable` and still ends the question (`taken`).

## Obviated-declaration sweep

Nothing obviated: every change is additive.
