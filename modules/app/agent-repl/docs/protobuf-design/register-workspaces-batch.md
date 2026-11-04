# RegisterWorkspaces: one batched re-announcement at link-up and promotion

On 2026-11-03 a backend bounce ended every session, and Emacs's link-up edge
re-registered all ~70 workspaces at once, as ~70 concurrent RegisterWorkspace
calls. The booting daemon's state database has one connection, and every
registration also republished the roster, whose `ReadArrangement` dominated
the connection's holders (`daemon.wsm.conn_pressure` 16:21:04-14, holders led
by `daemon.wsm.read_arrangement` and `daemon.wsm.workspace`). The calls outlived
Emacs's 10s unary budget (`elisp.connect.unary-timeout method="RegisterWorkspace"`
at 16:21:13.96), and every workspace whose walk failed was left detached with
nothing armed to walk it again. The owner asked for the flood to be removed
structurally (pre-approved proto change).

Design mode: autonomous.

## Assumptions (step 1)

- Objective: re-announcing every workspace at a link edge can never end in a
  timeout from contention on the daemon's state database.
- Built: one rpc, `agentrepl.v1.RegisterWorkspaces`, plus its daemon handler,
  the fake daemon's handler, and the Emacs codec and caller.
- No frontend component.
- Systems: daemon (`internal/server`, `internal/workspace`), Emacs
  (`lisp/wire-host.el`, `lisp/rpc.el`, `lisp/host.el`), the ERT fake daemon
  (`lisp/testsupport/fakedaemon`). The webapp and shim do not call it.
- Non-additive: none. RegisterWorkspace stays for single registrations
  (onboarding, a lost stream, a dead connection, a handover's serving address).

## Iteration sequence

Single increment: the endpoint's request and response, landed in
`endpoint_register_workspaces.proto` and signed in `service.proto`'s HOST
section beside RegisterWorkspace.

## Landed changes

### RegisterWorkspaces (endpoint_register_workspaces.proto)

- WHAT: request `dirs` (repeated path); response `outcomes`, one per dir in
  request order, each echoing its `dir` and carrying a oneof of `registered`
  (RegisterWorkspace's own success), `refused` (RegisterWorkspace's own error
  causes) and `failed` (a daemon-side fault, with its cause as free text).
- WHY: one call is the only shape in which N registrations cannot race each
  other for the one connection; the daemon registers serially and republishes
  the roster once.
- Why not a daemon readiness latch: the daemon already serves only after its
  boot has published the opening views (`daemon.cmd.serve` "serving" at
  16:21:03.87 preceded the flood). The contention was the N registrations'
  own N roster republications, which a latch would not remove.
- Why not one SQL transaction for the batch: a registration runs git between
  its store calls; holding the one connection across git would starve every
  other operation of the daemon for the batch's whole length. Serial
  registrations, each taking the connection as it does today, plus one
  republication, remove the contention without that.
- Why no batch-level failure arm: every failure is one directory's, and the
  fixed rule is that one directory's refusal or fault never costs the others.
  A call the daemon cannot serve at all is a Connect or transport error. The
  same plain-response precedent exists (`GetSidebarArrangementResponse`).
- Why a per-dir `failed` arm: RegisterWorkspace answers a fault as a Connect
  error for the whole call; a batch must not let one fault fail every dir.
- Consequences:
  - Emacs calls it with NO unary timer (`:timeout :none`): the answer's length
    scales with the workspace count, and the connection dying is the definite
    failure edge (the link's down then up edges walk every workspace again).
  - The fake daemon answers it by composing its RegisterWorkspace default per
    dir, so integration tests script it like any other unary.
