# Composer selections: completion order by recency of selection, owned by the daemon

The composer's completions (slash commands, workspace names, Slack channels)
offer the candidate the reader took most recently first, so after `/compact`
is taken, `/co` offers `/compact` before `/autocompact`. The record of what was
taken must be persisted and managed by the daemon, not held in Emacs's memory.

## Assumptions (autonomous design)

- The record is the user's, not a workspace's: a command taken in one
  composer is offered first in every other's (the behavior Emacs already had,
  in memory only).
- Every completion kind that already used the in-memory record moves to the
  daemon's record: commands, workspaces and channels alike. Keeping two
  records for one ordering rule would leave two owners.
- Systems involved: the protos, the daemon (keeper, state store, server, host
  stream), and Emacs (completion-order.el, host.el, the wire codec, rpc.el).
  The webapp and the shim are untouched.
- Non-additive change: Emacs's in-memory selection table
  (`agent-repl--completion-order-selected`) is removed.

## Landed changes

### ReportComposerSelection and the host stream's composer_selections arm

- WHAT: a new unary verb carrying one selection whose arm is its kind
  (command, workspace, channel) and its name; a new host-stream arm restating
  the whole record, newest first per kind.
- WHY: the daemon is the one owner of the record, so Emacs never orders from a
  shadow copy. The record rides the host stream as state, exactly like the
  workspace mentions, so a restart of either side hydrates Emacs first.
- The kind is a oneof rather than an enum, per the state-enum rule; the three
  kinds are separate namespaces (a command and a workspace may share a name).
- The request names no workspace, because the record is daemon-global;
  every host watch carries the same record.
- A store failure is the response's `not_recorded` error arm: the record and
  the push are left unchanged (no partial state).
- Consequence: the daemon stamps each selection itself, strictly later than
  every earlier stamp (max of now and the last stamp plus one), so the order
  never ties and never depends on Emacs's clock.
- Consequence: Emacs's order updates one round trip after a selection rather
  than instantly; the next completion is the earliest reader of it.
- Sweep: nothing in the package is obviated by this change.
