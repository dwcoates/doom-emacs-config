# Elisp implementation planning

## Dead code to remove
- The `:hibernated` render state, whole: its decode, teal color
  (`agent-repl--color-hibernated-teal`), tab palette row, and face in
  lisp/status.el — referenced across ~29 elisp files. Hibernation left the
  contract; a parked workspace presents as live with shim_attached=false.
- The keep-alive origin machinery: `agent-repl--context-cost-keep-alive-origin`
  and `--keep-alive-p` in lisp/context-cost.el match a retired enum value and
  now silently downgrade the keep-alive alarm — LIVE correctness issue, remove
  with the alarm's re-derivation from the new contract.
- The UDS frame/command envelope tables in lisp/frontend-uds.el
  (`--uds-known-frame-fields`, `--uds-known-command-fields`,
  `--uds-ignored-frame-fields`, ~25 arms): the whole push/command envelope has
  no successor; the transport is re-pointed at the agentrepl.v1 Connect rpcs.
- The `FailureKind` triage lists (`agent-repl-failure-machinery-kinds` /
  `-vendor-kinds` / `-client-kinds`, ~62 arms vs the contract's 17): the
  entry-correlated arms moved into feed.proto per-entry error arms; re-derive
  the coloring from the frozen vocabulary.
- `proto/vocab/render-colors.json`'s 24 RENDER_STATE_* values (CROSS-SYSTEM:
  coordinate with webapp+daemon orchestrators; the palette contracts to the
  five live colors, teal dies).

## Replacement integration-test specs
(Seeded from reconciliation's deleted pins; unit specs deliberately absent.)
- The agentrepl.v1 protojson round-trip: elisp encodes each host-section
  request (RegisterWorkspace, SelectWorkspace, WatchHostWorkspace) and decodes
  responses/pushes against the real frozen schema, including the
  WatchHostWorkspace push oneof {host | notification}.
- Roster status decode covers EVERY RosterRow.status arm the frozen contract
  declares and REFUSES unknown arms loudly (replaces the deleted
  hibernated-inclusive pin).
