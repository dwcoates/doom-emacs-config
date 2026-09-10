# Shim integration-suite audit #1 (fresh-context fable auditor) — triaged by the shim lead

Input for remediation #2. Every item below is ACCEPTED unless marked. Specs
cited by the auditor: shim.md, shim-fanout.md, daemon.md §2/§10b/§11,
store.md, sidecar.md, the protos, the shim AGENTS.md.

## A. Specified but untested (add the test; a mock lever where noted)

1. Main AgentId survives a resume by the ROTATED id: `!rotate`, kill, resume
   naming the new vendor id, StartTurn → prompt.agent == original; new rows'
   page_agent_id == original; link file `vendor-id/<new-id>.json` and
   `agent-id.json` exist with the ruled fields (session.test + record.test).
2. KillTurn scope = this turn only: t1 `!bash-detach-live` (item live after
   the turn), t2 `!hold`, KillTurn t2 force=false → agent_only; t1's run
   stays live (turn.test).
3. KillSession force names EVERY live item across turns (two items from two
   turns in stopped_work) and each WatchBash concludes interrupted before
   exit (session.test).
4. Permission mode conditions the gate: set bypass → `!perm-allow-once`
   opens no ask; set dont_ask → denied.policy; an ask open at the change
   keeps its mode (gate.test).
5. SetSessionModel cold gate: cold_threshold_tokens below the context →
   failure.cold{model_switch}; retry with a remediation succeeds; the next
   turn-end context_usage.model shows the switch (session.test).
6. Transcript backup: after a turn `$AGENT_REPL_STATE_DIR/shim/<key>/backups/`
   holds a byte-equal copy; after `!rotate` a second; count bounded by the
   keep constant (record.test).
7. StartTurn query_dead (`!query-eof` then StartTurn) and every no_session
   arm (each verb before StartSession) (turn.test/session.test).
8. StartSession conversation_owned: two shims in different workspaces sharing
   one AGENT_REPL_LOCK_DIR, the second resuming the first's vendor id
   (process.test).
9. StartTurn known_through returns only newer entries; page_size honored
   with a `more` boundary when older exist (turn.test).
10. Stable pointers across upserts: a unit's settle frame carries the same
    pointer as its start; a repaint places it at first-insert position.
11. Hibernate: kill after the ack, resume with no remediation succeeds (not
    cold) and the first page carries the compacted cut; compaction_failed
    and no_session arms (session.test).
12. WatchBash FOLLOWS: open the watch first, then seed deltas + terminal;
    assert arrival and end (detached.test).
13. UpdateAgent stop targeted at a detached subagent's AgentId → its own
    WatchAgent concludes interrupted; a subagent-raised ask answered with
    target set (needs `!subagent-detached` to stay live until stopped —
    mock lever if not) (turn.test/gate.test).
14. Reconciliation restates the ORIGINAL start instant and description for
    re-adopted work (detached.test).
15. Pending-callback liveness on interrupt (UpdateAgent.stop during an open
    ask → ask denied, turn interrupted) and on query death mid-ask (mock
    lever: a scenario that dies mid-ask) (gate.test).
16. Producer stays `claude-shim:<original>` across `!rotate` (record.test).
17. SessionUpdate rows keyed `session:<arm>:<uuid>` (e.g.
    identity_rotated after `!rotate`) are written (record.test).
18. Prose/thinking unit keys `activity:<msg>:<n>` 0-based across the mock's
    split lines; usage carrier is index 0 (record.test/turn.test).
19. top_level for subagent rows (sync → main agent; detached → the
    detached agent) (record.test).
20. The api_error page line and the 16-arm turn-stop taxonomy: one
    parametrized test over the table's `!api-*`, `!fail-*`, `!max-tokens`,
    `!refusal-*`, `!context-window`, `!interrupt` rows asserting the
    terminal arm (and the preceding api_error kind) (turn.test).
21. Unused scenarios carrying landed arms: `!bash-timeout` (timed_out +
    timeout_ms), `!bash-detach-fail` (non-zero exit is completed),
    `!monitor-*` (monitor in the live set; KillSession names it),
    `!subagent-failed`, `!compact` (trigger requested; `!compact-auto`
    asserts automatic), `!unmodeled` + the exempt-set drop (no row, no
    warning for TaskStop), `!hook-*`, `!usage-opus-absent`,
    `!usage-sampling-failure`, `!mcp-healthy` (change-only), `!slash`,
    `!skill`/`!skill-fail`, `!send-message*` — one test each.
22. Lock before bind: the refused second shim never created its socket.
23. SIGTERM exits NONZERO when the stand-down failed (writes never acked)
    with the loud drop record (process.test/record.test).
24. Store client CANCELS WatchAgentSession: fake-store ledger of open
    tails drops the entry after the client closes WatchAgent (fake-store
    lever) (transport.test).
25. degraded_windows oldest-first after two faults; no re-push of
    model_changed/permission_mode_changed on an unchanged turn.
26. Mock file-layout facts: task-id shapes (9-char base36 shell, 17-hex
    agent), agent spools carry no terminator, the rotated identity's old
    file keeps its content plus one closing system record, a resume adopts
    the last chained record's uuid as chain head, unchained metadata lines
    carry no uuid (record.test).

## B. Tested wrongly or weakly (rewrite the assertion)

1. gate "standing grant carrying set_mode": hands back an UNOFFERED standing.
   Mock lever: `!perm-allow-standing-mode` whose offered standing carries
   set_mode; plus the negative: an altered standing → answer_mismatch.
2. turn "stale_pointer"/"unknown_agent": the fake store never refuses. Lever:
   the fake store answers the TYPED arms (invalid_request | stale_pointer |
   storage_failure); PRODUCTION FIX: src/store/reader.ts readFailure must
   switch on the typed arm, never on detail substrings; map stale_pointer →
   stale_pointer, invalid_request on an unknown book → unknown_agent,
   storage_failure/transport → store_unavailable.
3. record "every upsert_key uses a ruled prefix": pin the RULED residue
   spellings `residue:<vendor record uuid>` / `residue:stream:<sequence>`
   (remediation #1 lands them) instead of an open `residue:` prefix.
4. session "SetSessionModel resolves only after the turn ends": a race across
   connections. Stronger: after the turn ends assert the next turn-end
   context_usage.model / the mock's recorded setModel instant vs the
   turn's result.
5. record "SIGTERM exits only after acks": keep failWrites across the SIGTERM,
   observe the process alive after the stand-down record begins, then clear
   and assert exit follows the final ack.
6. session "forced kill concludes the detached stream first": hold WatchBash
   open on the run; assert its terminal is interrupted and `exited`
   resolves after it.
7. detached "vendor-backgrounded unit confirmed": assert origin.detached
   unconditionally and the turn's terminal is AgentSuccess.backgrounded.
8. turn "page replays no start frames": also reject permission/question
   entries in `start`.
9. turn "usage rides exactly one unit": the carrier's id ends in `:0` and is
   the earliest-pointered unit of the message.
10. record "re-sent frame mints the same write_id": assert 64-hex ids,
    distinct arms → distinct ids, duplicates only across the failed-then-
    accepted batches.
11. process "SIGINT refused": send SIGINT during `!hold`; then stop the turn
    and assert interrupted.by_user.
12. session `!usage-full` / `!mcp-all`: assert every populated window
    (utilization + reset), subscription_type non-empty; server names and
    failed.error text.
13. detached `!cancel-all`: assert the arm per kind (stopped_by_user for
    agents, interrupted for the shell) and the agents_killed transcript record.
14. session "remediation.compact": cut.case == compacted, non-empty summary,
    tokens_before > tokens_after, and the next turn runs on the resumed
    session.
15. detached/record identity tests compare two shim-minted values: compare
    against the FILE PLANE (meta.json toolUseId / the transcript's tool_use
    block id); the permission-key test must match per gated call, not any.

## C. Edge cases with no test

1. Question answered out of batch order; multi-select with labels + free
   text in one selection (residue after labels removed).
2. Standing grant when no standing was offered → refused.
3. known_through newer than the book / from another book → stale_pointer.
4. WatchAgent known_through gap wider than page_size → page + `more` into
   the gap.
5. Two concurrent WatchAgent subscribers on one book; one closing mid-burst
   does not stall the other.
6. Multi-block UserSaid (image path/url, UnsupportedBlock) round-trips into
   the prompt row.
7. SessionStarted.turn_in_flight (resume during `!hold` — lever: a shim
   killed mid-turn then resumed) and SessionLive.turn_in_flight on a
   KillSession refusal during `!hold`.
8. Retry-buffer overflow: loud oldest-drop, survivors' order preserved.
9. `--listen` with a stale socket file (unlinked) and with a live one
   (refused).
10. Resume identity mismatch (the query landed on another conversation) —
    likely needs a mock lever; todo with reason if none.
11. ReadHistory after with page_size larger than the remainder → floor +
    exact count.
12. An answer arriving after the turn was killed → no_open_ask, no crash.

## D. The four todos

1–2 keep-alive: remediation #1 adds `AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS`;
   the rewind assertion also needs the mock to record the rewind target
   (which uuid `resumeSessionAt` named) — add that ledger to the mock
   (src/fake) and assert it.
3 GetLiveWork count: remediation #1 adds the fake-store read ledger.
4 budget warning: stays todo (sidecar plane).

## E. Internals and flakiness (fix)

1. Numeric pointer comparisons (`Number(at.value)`): assert order by served
   sequence and pointer inequality with more.last_entry; pointers opaque.
2. Vendor-file helpers import the producer's own slug/path functions: add one
   fixture oracle (`/private/var/folders/_m/x` → `-private-var-folders--m-x`).
3. `awaitSpoolExit`/`awaitFile` watch the directory: watch the file itself
   once it exists (fs.watch on the path), and re-check the level after
   installing the watcher (level-then-edge).
4. Retry-ladder tests depend on module constants: read the constants from
   src/store/writer.ts exports rather than hard-coding timing.
5. `!context-usage-drift`/`!model-fallback` waits can consume the drifted
   frame as the opening one: subscribe BEFORE the turn and key the wait on a
   value observed after the turn's result.
6. Negatives read from another stream after a terminal: drain the other
   stream up to a marker frame that follows the terminal (e.g. the turn-end
   context_usage push) before asserting absence.
7. Log-shape assertions (flag names in stderr, context.store_socket): keep,
   but centralize the record field names in one support constant.
8. h1 raw dial hard-codes bodyBeforeHeaders=false: make the h1 raw client
   observe the head/body boundary as the h2 one does.
