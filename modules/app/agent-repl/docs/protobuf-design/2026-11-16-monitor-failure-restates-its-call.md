# Every producer's monitor failure restates its call

`conversation.v1.AgentMonitorFailure.call` already existed, and the shim
filled it (`src/convert/tools/monitor.ts`). The sidecar's transcript plane did
not (`internal/convert/settled_items.go`, the `kindMonitor` failure arm). Both
planes upsert one unit under one key, so the sidecar's bare failure, landing
after the shim's, replaced the restating row in the store.

Observed on 2026-11-12 in the workspace `agent-prompt-bubble-collapse`. A
`Monitor` call made before its schema was loaded (input
`{"query":"select:Monitor"}`) failed with an `InputValidationError`. The
store's row for `activity:toolu_01DpoAeL3uTVHuKZPLP32N7J` holds the monitor's
`failure` arm with no `call`, written from a transcript source offset. The
daemon logged `daemon.feed.settle_not_restated` live, and then
`daemon.feed.activity_undrawable` at ERROR on every boot that replayed the row,
the deploy of 2026-11-16T00:12:20Z among them.

Owner rulings: contract changes are decided and carried through by the
implementing agent (2026-10-13).

## Landed changes

### Contract revision 3

- WHAT: `conversation.v1.AgentActivityContract` gains
  `MONITOR_FAILURE_RESTATES_ITS_CALL = 3`.
- WHY A NEW REVISION: the call the stored row lost is not in the store
  anywhere (the failure replaced the start under the same key), so no producer
  can rewrite it. The daemon grades a missing restatement by the producer's
  stamped revision. A monitor failure stamped below 3 without its call is
  expected old data (INFO). One stamped 3 without it is a producer defect
  (ERROR).
- Consequences: the sidecar's monitor failure restates the call through
  `monitorStart`, the start's own reader. The shim and the sidecar both stamp
  revision 3. The daemon's `drawMonitor` grades a bare failure by revision 3,
  and a bare `ended` still by revision 1.
