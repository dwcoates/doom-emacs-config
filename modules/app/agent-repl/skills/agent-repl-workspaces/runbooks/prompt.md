# Prompting a workspace

- **CRITICAL: A workspace talks to another WORKSPACE only through
  `workspace prompt`, never `SendMessage`.** `SendMessage` stays the way to
  talk to this workspace's own subagents. `workspace prompt` goes through
  agent-repld, which holds the prompt behind the recipient's running work,
  passes a cold gate the recipient is parked at, and draws the send
  in both workspaces' feeds; a `SendMessage` to another session goes around
  all of it.
- `ListAgents` never lists workspaces: an open workspace that is idle or
  cold has no running Claude process, so it is missing there however open it
  is. Find workspaces with `workspace list`, `workspace status` and
  `workspace find`. agent-repl's shim notes this on every `ListAgents`
  answer, and refuses a `SendMessage` to an `agent-repl:NAME` address.
- `workspace prompt WS TEXT` submits TEXT to WS as this agent's prompt, from
  this workspace: WS's feed heads it with a left arrow, then this workspace's
  name, and this workspace's feed shows it sent, with WS's name then a right
  arrow, and whether it was held or refused. A prompt that begins with a slash command is refused: an agent never
  clears another workspace's conversation.
- `workspace prompt -slack-forward URL WS TEXT` hands WS a Slack message with
  the prompt (the Slack emoji-ack intake forwarding a reaction). The daemon
  stores the thread among WS's links as a reference, "Forwarded Slack
  thread", and adds a directive asking WS to post one brief reply there naming
  itself. That reply is the only post the forwarded thread gets.
- A refused `workspace prompt` exits 6, prints the daemon's answer on
  stdout, and prints one plain sentence on stderr saying why WS cannot take
  the prompt. Read that sentence to tell the user what became of the send.
- **A prompt an agent sends to a workspace parked at a cold gate passes the
  gate for it.** The daemon passes the gate at once with the compaction
  `workspace pass-gate` makes (Haiku, the whole context; prompts the user left
  held at the gate stay held for the user). The prompt waits at the gate and
  is delivered when that compaction brings the session up. Prompts arriving
  together start one compaction. A prompt passes the gate at most once: if the
  compaction fails, the gate stands again, this workspace gets a notice that
  the pass failed, and the prompt stays waiting at the gate for the user's
  answer on the gate card (it is not dropped; do not resend it). A prompt the
  user types is still refused by a cold gate, and daemon notices wait at a
  gate without passing it. `-as-user` changes none of this; it lifts only the
  fan-out cap in the next point.
- **CRITICAL: NEVER broadcast to the fleet.** Every workspace a prompt
  reaches wakes and resends its whole context. A notice every workspace
  needs goes in the harness (`/agent-repl-update-harness`), never in a
  prompt to each one. The daemon refuses a prompt that would take this
  workspace past 3 distinct unrelated recipients in an hour
  (`fan_out_refused`); its family, its merge's requester and workspaces that
  prompted it do not count. `workspace prompt -as-user` lifts the cap ONLY
  when the user explicitly asked for that send.

- A prompt another workspace sent reaches you as a message from another
  session, `from="agent-repl:NAME"`. That is the workspace NAME, and the
  host's messaging tool it points you at is `workspace prompt NAME TEXT`.
  A prompt between two workspaces whose permission classes differ (one
  bypasses permissions, the other does not) is held by the vendor, so
  agent-repl refuses to deliver it rather than let it stall.
- **A REQUESTED UPDATE IS SENT WHEN ITS CONDITION IS MET, unasked.** When
  another workspace asks to be told once something happens ("tell me when
  this lands", "send me the results once the tests pass"), send the update
  with `workspace prompt` as soon as that condition is met, without asking the
  user for confirmation: the other workspace's request is the authorization.
