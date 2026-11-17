# A keep-alive's failed answer on the session stream

The daemon stands an idle session down only once its keep-alive is answered,
and the only answer it could recognize was the restated cache lapse. A
keep-alive whose request failed (the API unreachable, the vendor answering an
error) moves no lapse, so the daemon waited out its two-minute bound and
recorded "the keep-alive went unanswered" at WARN, although the shim had the
failed answer within a second (2026-11-17 13:19:20, storage-retention-tighten:
the keep-alive turn closed 0.3s after its send on ENOTFOUND; the daemon warned
at 13:21:19). Nineteen such records in one outage.

Mode: autonomous (the deploy fixer's dispatch; no co-design asked).

## Assumptions

- Objective: a keep-alive's failure reaches the daemon as its answer, so no
  answer that already came is waited out and reported as missing.
- Systems: the shim (producer) and the daemon's session watcher and cache
  keeper (consumer). No frontend surface.
- Non-additive changes: none. The daemon's two-minute bound stays for a
  keep-alive whose vendor never answers at all, which remains a WARN.

## Iteration sequence

One increment: a new arm of conversation.v1.SessionUpdate. The transport,
inventory and conventions stages are the existing session stream's and are not
reopened.

## Landed changes

### conversation.v1.SessionUpdate.keepalive_failed (tag 43), conversation.v1.SessionKeepaliveFailed

- What: an event arm the shim states once for every keep-alive turn whose
  terminal is a failure, carrying the vendor's own account in `detail`.
- Why: the keep-alive's answer has two outcomes and only one was on the wire.
- Consequences:
  - The shim pushes it directly from its message loop when a keep-alive-tagged
    result is folded, because the push plane drops every keep-alive-tagged
    session fact (keepaliveScope.attribute); it is not one of those facts.
  - Ordering is structural: a lapse the failed request still moved is restated
    from the assistant record, before the result, on the same ordered stream,
    so a daemon that took that lapse as the answer is no longer waiting and
    ignores the failure.
  - The daemon records the failure at INFO; the failure itself is the shim's
    to record (shim.vendor.unreachable, the network fault).
  - Not stated for a skipped keep-alive or a failed submission, which
    shim.v1.SendKeepalive answers itself, nor for a parked keep-alive, whose
    `sent` already means answered.
- Rejected: reusing conversation.v1.SessionFault.keepalive_failed. A fault is a
  standing condition drawn on the topbar's warning strip until resolved; a
  keep-alive's answer is an event, and drawing every failed keep-alive during a
  network outage would add a warning beside the network fault already shown.
- Sweep: nothing obviated.
