# Investigation notes: `<synthetic>` model, doom + glimmer-intensity-boost (2026-09-23)

Status: catalog only, nothing fixed. Evidence: `bin/logs.sh --workspace doom|glimmer-intensity-boost`,
`--central`, `--all`; the vendor transcript `6a1b0e3a-….jsonl`; shim source at master.

## A. doom's model is `<synthetic>` (CONFIRMED end to end)

1. 09-21 16:03 the session moved to `claude-fable-5-1`.
2. The vendor binary is 2.1.220; every fable call is refused by the API:
   "Claude Code 2.1.220 does not support this model; version 2.1.251 or newer is required".
3. The CLI writes each refusal into the transcript as an assistant record with
   `model:"<synthetic>"`, `isApiErrorMessage:true`, and a ZEROED `usage` block.
4. Keep-alive turns fire roughly every 52 minutes overnight; each one fails and writes another
   synthetic record, so the transcript's last assistant record is always synthetic.
5. On resume, `engine/cold.ts readTranscriptFacts` takes `lastModel` from the last record WITH
   `usage` — the synthetic one — and `engine/session.ts:2665` uses it as `requestedModel`,
   then `effectiveModel`, then the SDK `model` option (`session.ts:736/2451`). None of those
   paths call `normalizeModel`; only `noteReportedModel` guards the marker.
6. Result since 09-22 14:12: the CLI launches on model `<synthetic>`, every call 404s
   (`shim.vendor.model_missing`), the reply is the vendor's "issue with the selected model"
   notice, and keep-alives keep writing more synthetic records (self-perpetuating).
7. Side effect: the same zeroed usage makes `contextTokens = 0`, so the cold gate reads the
   conversation as under the 70k floor and never fires for a ~500k conversation.

## B. Switching doom back to "opus" is refused (CONFIRMED)

- 11:21:34 and 11:22:35: `SetSessionModel` refused, `"opus" is not in this session's model catalog`.
- Same unlanded arm as 09-21 item 8: goes out as HTTP 400, Emacs treats it as an outage and
  holds the prompt (`kind=:outage`, depth reached 3). The prompts never ran.
- Logging gap: the shim logs the catalog's COUNT (5), not its names, so why "opus" is absent
  cannot be read from the logs.

## C. Revival on select timed out and was torn down (CONFIRMED)

- 11:20:48 select revived doom; StartSession was still running at 11:20:58 when Emacs's 10s
  SelectWorkspace deadline cancelled it (`start_session shim call failed`, then a forced kill).
- The revival runs on the rpc's context — the same class as the bind bug fixed with
  `context.WithoutCancel` on 09-21. A second revival (prompt-triggered, 11:21:16) took 6s and
  succeeded, which puts the first one's duration just past the 10s bound.

## D. Another forced restart ended every session (CONFIRMED)

- 11:23:11 `elisp.daemon.restart` → `UpdateShutdownSchedule{now}` → every shim killed.
- This is a second caller of the forced path (`agent-repl-daemon-restart`), separate from
  `deploy-all.sh`; the 09-21 rollout fix did not cover it.
- Emacs then logged `call-on-closed-connection` for a select and deferred 3 held prompts.

## E. glimmer's "pay and resume" does nothing (PARTLY DETERMINED)

- glimmer has been parked at its cold gate since 09-22 14:12 and again after 11:23 today.
- The daemon has NEVER received a cold-gate answer for glimmer: the only `answer_cold_gate`
  records in 48h are doom's, 09-21 12:45. So the click never reached the daemon.
- Why it did not is NOT determinable: see F.

## F. No webapp log record has reached disk since 09-21 (CONFIRMED gap)

- doom's last webapp record is 09-21 14:35:50; glimmer's is 09-21 16:04:13; no workspace has
  written one since. No new `*-webapp-*.log` target has been created since 09-21 16:04, and
  each workspace's `.claude/emacs/webapp.log` symlink still points at the 09-21 file.
- The pages are alive (the daemon logs their `AdoptWebWorkspace` calls), so log forwarding is
  what stopped — which is why E cannot be traced past "the click never reached the daemon".

## G. Recurring noise already catalogued on 09-21

- 92 `detached_unknown_unit`, 4 `subagent_without_start`, 1 `row_without_identity` in doom —
  the 200-entry replay (09-21 item 2), still unfixed.

---

## Follow-ups found while fixing (2026-09-23, after the fixes landed)

- A handover's successor never checks whether the shims it adopted are on the deployed build:
  it records a shim's build only at StartSession, so an adopted shim reports none and is left
  alone. The bounce added in `becomeIncumbent` therefore relaunches nothing today.
- The staleness check compares the shim's reported build with the DAEMON's `.built-sha`, not
  the shim bundle's, so a daemon-only deploy would read every shim as stale.
- The sessions were moved to the new SDK by hand with RestartWorkspace. glimmer's old shim
  (pid 99409) was left running beside its parked replacement.

## Log audit, all backends, 09-15 → 09-23 (8001 warn/error records)

Still happening after 11:50 today:
- Store refuses sidecar batches: a row's kind would change "keepalive" → "page_line" (202 total,
  14 today); the sidecar then parks the whole transcript file for the process's life.
- Replay noise: `detached_unknown_unit` (723), `subagent_without_start` (185),
  `row_without_identity` (110).
- Every handover: Emacs logs WatchDaemon/WatchWorkspaceRoster "producer closed without an end
  frame" at ERROR, and the relaunch force-kills old shims that miss the stand-down window.
- ClientLog refusals during a handover: `transferring_away` (663) and `not_yet_adopted` (667)
  are unlanded arms, logged as WARN floods. The not_yet_adopted half is fixed.

Historical, since fixed or not seen for days:
- Store unreachable 09-15 to 09-18 (sidecar panic `makeslice: cap out of range` twice; shim
  store writes DROPPED 366, cursor recovery, suspended production).
- `store.rpc.watch-bash-run` refusals (1833) plus `shimclient.watch_bash` ERROR (48) for a
  refusal the daemon calls expected; last seen 09-21.
- Emacs log routing "owned target vanished" (195 push-invalid, 09-17 to 09-18).
- The select/bind revival cancelled by the caller's 10s deadline (SelectWorkspace 11, bind 7).
- ReadTranscripts BigInt float (fixed 09-21), synthesized-title splice (09-15/16), the
  frameUndecodable card (fixed today), the quarantined merge (fixed), classifier interrupt refusals.
- Smaller: 33 slow write_batch queries, context-panel category color "warning", KillWorkspace on
  a missing session row, a held-prompt verdict written after tombstone, `ListAgents` unmodeled,
  7 absent log sinks for deleted worktrees and test directories.
