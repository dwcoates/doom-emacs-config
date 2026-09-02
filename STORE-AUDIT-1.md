# Store audit 1 — critiques (verbatim from the fresh-context auditor) and the teamlead's rulings

## Critiques
1. Replay of a superseded write regresses the row: `upsertEntry` ON CONFLICT overwrites write_id, so `absorbedBefore` misses any OLDER write, re-applies it (overwriting newer content) and bumps write_seq. Test: w1→u "A", w2→u "A settled", open+watch, replay w1 → page shows "A settled", watcher receives nothing, response success.
2. Second store on the same socket steals it: `Listen` unlinks any socket path without dialing first. Test: A ready; launch B same --socket/--db → B exits non-zero with an error record, A still answers GetLiveWork.
3. Concurrent producers never exercised (BUSY_SNAPSHOT rationale, one upsert_key space): N goroutines (stream+file) writing simultaneously → all success, page holds N×k lines with distinct pointers, watcher receives exactly N×k. Plus stream writes u-x "draft", file writes u-x "final" (different write_id) → one line "final" at the original pointer; replaying both write_ids is success with no watcher delivery.
4. WriteBatch all-or-nothing asserted at the wrong layer (server validation, no transaction). Make the bad entry one the db classifies (agent_frame.agent_id empty / top_level present-but-empty), placed LAST → cursor and records absent after restart; detail names `entries[1]` and the write_id.
5. Cursor update/rename never tested: batch (id,/a.jsonl,100,carry) then (id,/a.rotated.jsonl,250,nil) → exactly one cursor with the second's path/offset/carry, after restart too. Cursor-only batch: server refuses `batch_empty` while db accepts — layers disagree.
6. Untested: detached_work closure via the ORIGIN UNIT's activity terminal; `detached` origin arm; prompts as page lines (`promptLine`/`detachedSubagentFrame` are dead helpers); `failure` run frames. The bash join relies on DetachedWorkId string == AgentActivityId string.
7. Logging-contract fields never asserted (timestamp fixed-width local RFC3339, runtime=store, pid, level, verbosity, operation, message, context). X-Agent-Repl-Request-Id only tested in-process.
8. "Every error logged exactly once" unasserted and violated: db.refuse forces Level error for every db-side refusal (stale pointer included) and the server adds a warn for ErrInvalid.
9. Backpressure: `len(warnings)==0` accepts any warn; filter by operation store.fanout.overflow and assert watch_token_hash, book_agent_id, dropped>0. Add: default buffer absorbs a 4096-line burst with no overflow.
10. Upsert that changes book or kind is unpinned (upsertEntry updates book_agent_id and kind on conflict).
11. Nuke-never-migrate only pinned in-process; a garbage --db exits non-zero rather than nuking.
12. Catch-up gap EXACTLY equal to page_size untested (4 lines, mark at L2, page_size 2 → [L4,L3] and floor).
13. JSON codec covers only GetLiveWork; carry (bytes), Struct raw and the streaming watch untested under JSON.

## Helper defects
- assertNoDatabaseTouch is vacuous (`statement` only on slow-query warns; verbose not persisted).
- restart() removes the socket before relaunch, masking the stale-socket reclaim path; no SIGKILL variant.
- vendorSpecificLine/unknownLine omit `raw` (non-optional); the store accepts them.
- frameLine always sets page_agent_id from the frame's agent; envelope-vs-frame disagreement never tested.
- warning assertions accept unrelated warns.
- keep-alive durability proven only via absorption (weak per critique 1).

## Teamlead rulings (binding for the remediation)
R-A1 Absorption ledger: a `write_ledger` table (write_id PK, upsert_key, write_seq, applied_at_ms) receives one row per APPLIED write in the same transaction; absorption = a row exists in the ledger; `entry.write_id` stays as the latest applied write for the row. Constant cost, one indexed lookup.
R-A2 Socket exclusivity: before unlinking an existing socket path, DIAL it; a successful dial means a live store owns it → refuse to boot (bootstrap error, exit non-zero, one error record `store.listen.occupied`); a refused/absent dial → reclaim with the existing warn.
R-A3 Cursor-only batches are LEGAL at every layer (a sidecar that read bytes yielding no entries must still advance); the server refuses only a batch with neither entries nor cursor_advance (`batch_empty` keeps that meaning).
R-A4 Logging exactly-once: db-side refusals (ErrInvalid, ErrStalePointer) are logged by db at VERBOSE only; the SERVER records the single normal-level record for them (warn, with refusal_site, rpc, request_id, producer); ErrStorage is db's error record once and the server's verbose trace. A stale pointer therefore never yields a `level: error` record. Tests assert exactly one normal-level record per refusal.
R-A5 Upsert identity: an upsert whose book (page_agent_id) or kind differs from the existing row's is REFUSED (`invalid_request`, field naming which changed; refusal site `upsert_changes_identity`), nothing committed. Identity per thing.
R-A6 Envelope vs frame: a page line whose page_agent_id differs from the frame's agent (AgentFrame.agent_id / AgentPrompt.agent) is REFUSED (`page_book_mismatch`). Residue with unset `raw` (vendor_specific/unknown) or empty `raw` (unparsed) is REFUSED (`residue_raw_unset`).
R-A7 Nuke: an unreadable/garbage --db file is IN THE WAY → the store removes it (and -wal/-shm siblings) and recreates, with the existing schema warn naming the cause; boot proceeds. A version mismatch nukes as today. Integration tests pin both.
R-A8 Bash run frames and the detached row: StoreAgentBash.run is the ORIGIN UNIT (AgentActivityId). A run frame locates its detached_work row by origin_unit == run.value first, then by work_id == run.value; if neither exists (the file plane observed the spool before the stream plane announced it) it creates the row keyed by run.value with origin_unit = run.value, and a later announcement whose origin_unit == run.value UPSERTS that row (never a second row). GetLiveWork lists one entry per row. Test with distinct DetachedWorkId and AgentActivityId values.
R-A9 assertNoDatabaseTouch: the validation subjects launch the store with AGENT_REPL_LOG_VERBOSE=1 so db statement records persist, and assert no `statement` record carries the refused request's request_id; db emits a verbose record per statement family carrying request_id when the server passes one (thread the request id through the context, not a new logger).
R-A10 Overflow exactly at the bound is not black-box deterministic; do not pretend — assert the burst test and the filtered overflow test only.
R-A11 Every critique's proposed test is added unless a ruling above changes its shape; every helper defect is fixed; the two dead helpers become live through their tests.
