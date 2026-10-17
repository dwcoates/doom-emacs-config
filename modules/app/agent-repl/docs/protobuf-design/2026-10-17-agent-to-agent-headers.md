# Agent-to-agent bubble headers

The owner asked (2026-10-17) for cross-session agent-to-agent bubbles to lose
their border and to be marked by their header alone:

- A RECEIVED message (another workspace prompted us) reads `<left arrow icon> <sending workspace name>`.
- A SENT message (we prompted another workspace) reads `<receiving workspace name> <right arrow icon>`.
  - This replaces the text `→ <name>`.
- Only the other workspace's name is shown.
  - Never the vendor's socket path (`uds:/tmp/cc-socks/<pid>.sock`).
  - Never the vendor's per-process session suffix (`fix-flaky-3a`).
- The name is the agent-repl WORKSPACE name, the one the sidebar shows.
- A sender that is genuinely not an agent-repl workspace shows its vendor session name.
  - That fallback is an explicit typed case in the proto, never a silent default.

## Core design principles

- **Both ends of a cross-session message are named by the vendor-minted delivery id** (parent rulings, 2026-10-17).
  - The sender's SendMessage `msg_id` is the recipient's `origin.msg_id`.
    - Observed: the same id appeared in both transcripts of one message.
  - The SENT side names the recipient by joining on that id (ruling A).
    - Rejected: (B) parsing the socket out of the SendMessage result's display prose.
    - Rejected: (C) stripping the `-xx` suffix from the addressed name, which is a heuristic.
  - The RECEIVED side names the sender by the SAME join, not by the pid.
    - What the owner asked for is only that the header show the other workspace's name.
    - The pid mapping was the parent's suggestion, not the owner's.
  - A session outside agent-repl writes no record into the host's store.
    - So it is the unresolved case structurally, never by a guess.
  - It does NOT claim the delivery id authenticates anything. It is a display join only.

## Correction, kept visible: the pid design was retracted

- Landed changes 1 and 2 below first keyed the sender on the vendor's kernel-verified pid (`origin.verifiedPeerPid`).
  - The pid was mapped through a `SessionVendorProcess` level, guarded by the vendor process's start time.
- RETRACTED (2026-10-17): a pid names a workspace only while that vendor process runs.
  - Vendor processes restart on every deploy, rollback and hibernate.
  - So once the sender's vendor had restarted, every older received message lost its workspace name.
  - It would have fallen back to the outside-session name.
- Fixing that within the pid design needed a durable table of pid lifetimes.
  - That table's end times are known only as of when the daemon notices a death.
  - So a daemon that was down leaves a window where a recycled pid could mislabel a message.
  - That is unlikely, not impossible.
- ROOT CAUSE of the error: the vendor's SDK doc ("key sender identity on this") was read as the right key for a DISPLAY name across HISTORY.
  - That doc is about live trust, not durable provenance.
  - The lifecycle review (what a consumer sees when it replays a message whose sender has restarted) was not run before the shape was agreed.
- Removed: `conversation.v1.SessionVendorProcess` (and `SessionUpdate.vendor_process`), and `conversation.v1.PeerMessageSenderProcess`.
  - Never shipped, so tags were renumbered rather than reserved.

## Landed changes

### 1. RETRACTED (see the correction above): `conversation.v1.SessionVendorProcess`, a standing session level

- WHAT: the shim states `running {pid, started_at_ms}` once its vendor child has a pid.
  - It states `ended` the moment the child exits.
  - It restates the level on every WatchSession open.
- WHY: a peer's record names its sender by a vendor pid, so the daemon needs every session's current vendor pid to name the sending workspace.
- EVIDENCE (observed, 2026-10-17): every `/tmp/cc-socks/<pid>.sock` on the host belonged to a `claude` process.
  - That process's parent was a `shim/dist/main.js`, so the socket pid IS the shim's direct spawn (`sdk/real-query.ts` `vendorSpawner`).
- CONSEQUENCE, pids recycle: a pid names a session only while `running` stands, and only for an event at or after `started_at_ms`.
  - `started_at_ms` is taken AFTER the spawn returned, so it is never earlier than the true start.
  - A false match is therefore impossible.
  - A false miss would need the vendor to message before it finished booting, which it cannot do.
- CONSEQUENCE: the daemon drops a workspace's binding when the shim's stream ends.

### 2. `conversation.v1.PeerMessageInterSession` carries what the vendor states about the sender

- WHAT: `sender_session_name` (`origin.name`) and `delivery_id` (`origin.msg_id`).
  - The verified pid was removed (see the correction above).
- WHY: the arm states facts only. Which workspace sent the message is the daemon's resolution, by the delivery-id join.

### 3. `conversation.v1.AgentSendMessageSuccess.recipient` oneof, with a new `peer_session` arm

- WHAT: `recipient_agent_id` moved into a `recipient` oneof, beside `peer_session {delivery_id}`.
- WHY: a cross-session send has no recipient agent this conversation books.
  - Before this change, the shim produced NO success frame for one.
  - The sidecar minted `recipient_agent_id` from the addressed NAME, which is a respelled identity and disagreed with the shim.
- `conversation.v1.PeerDeliveryId` is ONE type, declared beside the send in `agent_activity.proto` and imported by `peer.proto`.
  - `agent_activity.proto` is the file `peer.proto` already imports.

### 4. `store.v1.GetPeerDelivery` and `shim.v1.ResolvePeerDelivery`

- WHAT: given a delivery id, the store answers each END on its own.
  - The sender's end is the session whose SendMessage success carries the id.
  - The recipient's end is the session holding the delivered message.
  - Each end is the MAIN agent at the root of the holding book's lineage, or `not_recorded`.
  - The shim relays this to the daemon.
- WHY: a row replayed from history must name the other end even when the daemon never saw it land in this process.
  - The daemon may not read `store.v1` (`proto/AGENTS.md`), so the shim relays.
- Unscoped by design: the answer is always another session, so the usual lineage scope would refuse it.
- Store side: an in-place `peer_delivery` table with an `end_kind` column, and a Go backfill from stored frames.
  - Not a schema version bump, which would nuke the database.
- The sidecar's conversion version is 3, so every transcript re-derives and the backfill finds old messages' ids.
- CONSEQUENCE: the store answers a MAIN AGENT id (the session's ORIGINAL vendor session id).
  - The daemon maps it to a workspace only through a main agent it has learned (the session watcher's `OnMainAgent`).
  - `wsm.sessions.vendor_session_id` is the CURRENT id and rotates on `/clear`, so it is not the key.
  - A row whose other end's workspace is not yet known stays unresolved and redraws in place when that workspace's main agent is learned.

### 5. `frontend.v1.FeedPeerMessage.from` and `frontend.v1.FeedAgentPromptAddress` oneof

- WHAT, received side: `FeedPeerMessage.sender` (string) is retired.
  - It is replaced by `from {name, oneof sender {workspace, unresolved, unnamed}}`.
  - The client draws a left arrow icon, then the name.
- WHAT, sent side: `FeedAgentPromptAddress.text` is retired.
  - It is replaced by `oneof address {in_session {text}, peer_session {name, oneof recipient {workspace, unresolved}}, unclassified {name}}`.
- BORDER (parent ruling, 2026-10-17): only cross-session bubbles lose the border.
  - Subagent prompts (`in_session`) keep it, with the token split so the two can differ.
  - A send not yet known to be a session's or a subagent's (`unclassified`: a start frame, or a refusal) starts borderless.
    - Cross-session is the common case.
    - It gains the border if it settles as `in_session`.
- Whether subagent prompts also go borderless is with the owner (the parent is asking).
- CONSEQUENCE: a send's row updates IN PLACE when its recipient resolves, with only the name changing.
  - The daemon re-resolves rows whose other end was unresolved whenever that end's record lands, or the holding workspace's main agent is learned.

### 6. Subagent prompts follow the same rules (owner ruling, 2026-10-17, superseding change 5's border split)

- WHAT: `frontend.v1.FeedAgentPromptAddress` is the other end's `name`, plus a `direction` oneof.
  - `sent` puts a right arrow after the name.
    - Its recipient is `agent` (main, named or identified), `peer_session` (workspace or unresolved), or `unclassified`.
  - `received` puts a left arrow before the name.
    - Its sender is an `agent`.
  - Every agent-to-agent bubble is borderless.
    - The border token `--agent-prompt-border` is removed, because nothing reads it any more.
- WHY: the owner ruled subagent bubbles take EXACTLY the cross-session rules.
  - No border, and the same icon header.
  - The subagent's requested name, else its identifier. Description and type are never used (parent confirmation).
- The identifier is the subagent's AgentId (the spawning `tool_use_id`), the same value every other surface keys it by.

### 7. A message from an agent of this session: `conversation.v1.PeerMessageInSession` and `shim.v1.ResolveVendorTask`

- WHAT: a peer record stating `origin.senderTaskId` (and not a hand-back) is a new kind on both planes.
  - It carries the sender's vendor task id.
  - The daemon asks the record which agent the task id names, through `shim.v1.ResolveVendorTask`.
    - That reads the store's existing locator pairing, `store.v1.GetAgentByVendorTask`, within the workspace's own lineage, which the daemon passes.
  - The bubble is headed `<- <requested name, else agent id>`, with the task id under `unresolved` until the record answers.
- EVIDENCE (observed, 2026-10-17): every `senderTaskId` record on the host is one of two kinds.
  - A hand-back (`handback: true`).
  - Or a subagent's message: 135 in main transcripts, and 388 in sibling subagents' transcripts.
  - The task id is the sending subagent's LOCATOR (the `<id>` of `agent-<id>.jsonl`), not its AgentId.
- SCOPE (parent ruling): the hand-back proper stays the inline badge it is.
- The sidecar's test fixture had stated `senderTaskId` on every peer line. It now states it only on a hand-back, matching the observed records.

### 8. Agent-to-agent bubbles take the final response's fill (owner request, 2026-10-17)

- WHAT: the `agent` and `peer` variants feed `--bubble-bg` from `--assistant`, the response bubble's own token.
- The muted header reads at a higher contrast on it than on the prompt blue, so it was not brightened.
  - Dark theme: 7.1:1 against 2.3:1 on the blue.
