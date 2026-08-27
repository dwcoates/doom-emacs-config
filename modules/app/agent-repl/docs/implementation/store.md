# Store implementation planning

## Dead code to remove (with the store.v1 service port)
- The UDS + wire-Any framing front end whole: store.v1 is a Connect service
  (WriteBatch/OpenAgentSession/ReadAgentPage/WatchAgentSession/GetLiveWork/
  GetWorkflow/GetSidecarCursors); the old Subscribe/EntryDelivery/
  ConnectionHeartbeat/HealthCheck dial protocol is deleted from the contract.
- The (session_id, seq) + top_level_message_id schema: superseded by the
  settled four-table architecture (agent / workflow / entry spine with
  upsert_key PK / detached_work) under nuke-never-migrate — drop and
  recreate, no migration code.
- Ingest's ErrRecordPersistenceUnreconciled refusal stub: replaced by the
  StoreEntry ingest under the new addressing.

## Blockers / decisions owed (surfaced at reconciliation)
- NO GO SERVICE STUBS EXIST: the Makefile runs protoc-gen-go only, so
  store.v1 (and shim.v1, agentrepl.v1) have message types but no Connect
  handler interfaces in Go — protoc-gen-connect-go must join the codegen
  before any Go service can be implemented. (LEAD-LEVEL, cross-system.)
- StoreItemPointer minting and the row key under StoreEntry's upsert_key
  addressing (the design record's stage-5 architecture entries are the spec).
- AgentSessionToken mint/resolve (OpenAgentSession → WatchAgentSession).
- Idle-producer liveness: ConnectionHeartbeat died deliberately (streams +
  transport own liveness); confirm the sidecar needs no substitute.
- Store health probing: no health verb exists by design; agent-shim-doctor's
  probe re-derives from the Connect endpoints.
- Pre-existing Serve/Close race (trackConn after Accept vs Close snapshot):
  fix in production during the port; tests currently barrier around it.

## Replacement integration-test specs
(Unit specs deliberately absent per the mapping convention.)
- WriteBatch: durable-ack semantics — success = records + cursor advance in
  one transaction; replay absorption by write_id is the same success arm;
  failure = nothing committed (replaces the 23-test ingest suite's subjects
  under the new shapes).
- OpenAgentSession/WatchAgentSession: open answers page + token; watch is a
  pure tail pinned exactly after the page; known_through repaint vs catch-up.
- ReadAgentPage: after-pointer walk, order by first insert, floor/more arms.
- GetLiveWork: ended_at IS NULL scans across agent + detached_work.
- Keep-alive exclusion (Owed G) and logical-session scoping (Owed H): no
  page ever returns a keep-alive row; rotation never splits a page.
