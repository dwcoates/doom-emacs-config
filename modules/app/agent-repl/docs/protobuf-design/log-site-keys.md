# Log site keys and the window query

Every record agent-repl writes is to name the log call that wrote it, by a key
that is the same for every record that call writes and survives every commit
that leaves the call alone, with the call's unrendered template as the site's
summary. The production checks planned on top of this compare which sites
fired since the last deploy with which fired in the deploy before, so the
daemon must keep, per deploy, every site that fired, independently of log
retention, and answer a window of time from what it kept.

Design mode: autonomous (the owner handed the change over without asking to
co-design it).

## Assumptions (step 1)

- OBJECTIVE: a stable key and summary on every log record, and a query of the
  sites that fired in a window, as the foundation of the production checks.
- WHAT IS BUILT: a site key in every runtime's logging helper, a per-deploy
  tally of fired sites in the daemon's store, and one agent API rpc.
- FRONTEND: none. The rpc is an agent API read, answered as JSON on the CLI;
  figma→idl does not apply.
- SYSTEMS: the daemon (the store, the pass, the rpc), the webapp and the
  sidecar (each forwards its records through `ClientLog`, so each must send
  its site), and every other runtime's logging helper (no contract: those
  write their records to their own sinks).
- NON-ADDITIVE CHANGES: `ClientLogRecord` gains two REQUIRED fields, so a
  forwarder built before the change has its records refused. The webapp page
  is reloaded by every deploy and the sidecar is redeployed with the daemon,
  so the skew lasts one rollout; the refusal is loud (InvalidArgument), never
  a silently uncounted record.

## Iteration sequence

The default sequence, walked by the orchestrator alone: transport (Connect,
as every agent API rpc is), the inventory (one new rpc, `ListLogSites`, in the
AGENT API section, and two fields on an existing message), the conventions
(the package's `oneof result { success; error; }`), then each shape.

## Landed changes

### `ClientLogRecord.site` and `ClientLogRecord.template`

- WHAT: two required strings on the forwarded record. The client derives the
  key from its runtime, the call's scope (a TypeScript module names its
  logger's scope statically, since no caller is recoverable from a minified
  bundle) and its template; the daemon checks only the key's form (12
  lowercase hex), because it cannot see the client's scope.
- WHY: a forwarded record must carry a site like every other record, or the
  production checks miss everything the webapp and the sidecar log.
- CONSEQUENCE, ACCEPTED: the daemon cannot verify a client's key, only its
  form; a client that derives keys wrongly is caught by that client's own
  scan test, not by the daemon.
- `message` stays the rendered text: the daemon persists what the client
  rendered rather than re-rendering, since the client holds the values.

### `log_level.proto`

- WHAT: the four empty level arms (`ClientLogLevelDebug` through
  `ClientLogLevelError`) move out of `endpoint_client_log.proto` into a shared
  file, unchanged in name and wire, because `ListLogSites` needs them too
  (the file model keeps an endpoint's private shapes in its own file and
  anything two endpoints need in a shared one).
- Their names keep the `ClientLog` prefix: renaming them would change every
  generated binding the daemon and the webapp already use, for no change in
  meaning.

### `ListLogSites`

- WHAT: the request is a window (`since_ms` required, `until_ms` defaulting to
  now) and an optional minimum level; the success answers one `LogSite` per
  site and level (key, template, runtime, level, latest operation, count,
  first and last seen, and the latest record as a sample) and states the
  hours it covered.
- WHY HOUR GRANULARITY: the daemon tallies into (deploy, site, level, hour)
  rows. Per-deploy rows alone could not answer a window that starts or ends
  mid-deploy; per-record rows would be the logs again. Hours split exactly at
  each deploy's start (a row is per deploy), so a per-deploy read is exact and
  a time window is exact to the hour. The answer says which hours it covered
  (`ListLogSitesCovered.whole`), so no reader mistakes the widening.
- WHY A COMPLETENESS ARM: the store keeps the rows of its most recent deploys
  (at least three), so a window older than that is read only from where the
  rows begin, and says so (`partial`); one wholly older is the
  `unrecorded_window` error, distinct from a window in which nothing fired
  (a success with no sites).
- WHY ONE ENTRY PER SITE AND LEVEL: a site's key names a call, and a helper
  that forwards its caller's message (`dlog.Logger.Log`) chooses the level at
  run time, so one site can write at two levels.
- THE SAMPLE'S CONTEXT IS A `google.protobuf.Struct`, the same accepted cost
  as `ClientLogRecord.context`: a record's context has no schema ahead of its
  call sites.
- WHY NOT JSON TEXT FOR THE SAMPLE: the record's fixed fields are typed
  (instant, message, workspace), and only its free-shape context is untyped.

## Consequences recap

- An old webapp page or sidecar's records are refused for one rollout.
- The daemon checks a forwarded key's form only.
- A time window is answered at hour granularity and says which hours.
- The store keeps at least three deploys of rows; older windows answer
  partial or unrecorded.
