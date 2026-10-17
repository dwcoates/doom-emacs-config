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

- **Sender identity is keyed on the kernel-verified pid, never on `from`** (the owner, via the parent workspace).
  - The owner's terms: "map the sending process (the pid in /tmp/cc-socks/<pid>.sock) to its workspace in the daemon".
  - Consequence: `conversation.v1.PeerMessageInterSession.sender_process.verified_pid` carries the vendor's `origin.verifiedPeerPid`.
    - The vendor's own SDK contract says "key sender identity on this, never on `from`".
  - Consequence: the daemon maps that pid to a workspace through each session's `conversation.v1.SessionVendorProcess` level.
  - It does NOT claim the pid is an authentication token. It is provenance only, and a relay's pid is the relay's.
- **The SENT side is joined on the vendor-minted delivery id** (parent ruling A, 2026-10-17).
  - The sender's `msg_id` is the recipient's `origin.msg_id`.
  - Rejected: (B) parsing the socket out of the SendMessage result's display prose.
  - Rejected: (C) stripping the `-xx` suffix from the addressed name. It is a heuristic.

## Landed changes

### 1. `conversation.v1.SessionVendorProcess`, a standing session level (`SessionUpdate.vendor_process = 38`)

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

### 2. `conversation.v1.PeerMessageInterSession` carries the sender as the vendor states it

- WHAT: three new fields.
  - `sender_process {verified_pid, received_at_ms}`
  - `sender_session_name` (`origin.name`)
  - `delivery_id` (`origin.msg_id`)
- WHY: the arm states facts only. Which workspace a pid belongs to is the daemon's resolution.
- `received_at_ms` comes from the transcript `timestamp` on the file plane, and from the shim's receipt instant on the live plane.
  - The two planes collapse on one uuid as before, so either value is right.

### 3. `conversation.v1.AgentSendMessageSuccess.recipient` oneof, with a new `peer_session` arm

- WHAT: `recipient_agent_id` moved into a `recipient` oneof, beside `peer_session {delivery_id}`.
- WHY: a cross-session send has no recipient agent this conversation books.
  - Before this change, the shim produced NO success frame for one.
  - The sidecar minted `recipient_agent_id` from the addressed NAME, which is a respelled identity and disagreed with the shim.
- `conversation.v1.PeerDeliveryId` is ONE type, declared beside the send in `agent_activity.proto` and imported by `peer.proto`.
  - `agent_activity.proto` is the file `peer.proto` already imports.

### 4. `store.v1.GetPeerRecipient` and `shim.v1.ResolvePeerDelivery`

- WHAT: given a delivery id, the store returns the MAIN agent of the session whose record holds the delivered message.
  - The shim relays this to the daemon.
- WHY: a send replayed from history must name its recipient even when the daemon never saw the recipient's copy land in this process.
  - The daemon may not read `store.v1` (`proto/AGENTS.md`), so the shim relays.
- Unscoped by design: the answer is always another session, so the usual lineage scope would refuse it.
- Store side: an in-place `peer_delivery` table with a Go backfill from stored frames, not a schema version bump, which would nuke the database.

### 5. `frontend.v1.FeedPeerMessage.from` and `frontend.v1.FeedAgentPromptAddress` oneof

- WHAT, received side: `FeedPeerMessage.sender` (string) is retired.
  - It is replaced by `from {name, oneof sender {workspace, outside_session, unnamed}}`.
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
  - The daemon re-resolves rows whose recipient or sender was unresolved whenever a vendor-process binding or a delivery lands.
