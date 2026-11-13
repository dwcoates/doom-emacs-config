# A reload generation on the web view host's page stream

In the 2026-11-13 14:42 deploy (0.427.1 to 0.427.2) the daemon logged one
`daemon.deploy.webhost` ERROR: the host's two documents (the host and the
sidebar) attached in the same millisecond on the old webapp; the first one's
attach pushed the reload, and the second was judged "attached after its
reload was pushed" although the host simply had not reloaded yet.
`deploy.ReconcileWebhost` had only the stale build to go on, which cannot tell
"has not reloaded yet" from "reloaded and ignored the push". The owner
approved a contract change: a per-host reload generation on the page stream.

Mode: autonomous (the owner handed the change over; nothing was co-designed).

## Assumptions

- The web view host is reloaded only by Emacs (`lisp/webhost.el`), which can
  count its loads; the webapp cannot tell a reload Emacs made from one WebKit
  made. Evidence tier: code (`agent-repl--webhost-navigate`,
  `agent-repl--webhost-take-back` are the only navigations of the host).
- The sidebar document always reloads with the host (`agent-repl--webhost-navigate`
  calls `agent-repl-sidebar-view-navigate`), so it states the host's generation.
- One Emacs owns one host. A second Emacs's host counts its own generations,
  which the daemon's one recorded generation does not tell apart; accepted.

## Iteration sequence

One increment: the field on the existing request message. No rpc, no new
message.

## Landed changes

### `agentrepl.v1.WebhostPage.reload_generation` (field 2)

- WHAT: `optional uint32 reload_generation = 2` on `agentrepl.v1.WebhostPage`,
  the message `agentrepl.v1.WatchPageRequest.webhost` carries.
- WHY: the daemon records, with each reload it pushes, the highest generation
  a stale host stood at (`webhostReload.pushedFor`); a stale host at or below
  it has not reloaded yet (DEBUG), one above it reloaded and ignored the push
  (the one ERROR, on a host's attach).
- Optional, not a bare scalar: an Emacs older than the field loads the host
  with no `reload` parameter, and its documents state none. Absence means "this
  host's reload cannot be judged", which the daemon takes as not reloaded yet.
  0 is a real value (the host's first load), so a zero default could not say it.
- Producer: Emacs counts (`agent-repl--webhost-reload-generation`, reset to 0
  when the host is created, raised by `agent-repl--webhost-load`) and hands each
  document its generation in its URL (`agent-repl-webhost-document-url`,
  `reload=<n>`); the webapp reads it (`readReloadGeneration`,
  `webapp/src/host/document.ts`) and states it (`startPageStreams`). A
  malformed parameter fails the document's start loudly.
- Consumer: `daemon/internal/server/page.go` holds the whole stated message per
  stream (`holdWebhostPage`), `server.WebhostPages` answers clones in attach
  order, and `deploy.ReconcileWebhost` judges (`judgeStaleWebhosts`). The
  deploy's `Clients` interface takes the proto message itself
  (`WebhostPages() []*agentreplv1.WebhostPage`) rather than a second spelling of it.
- Consequence accepted: a generation stated against a push recorded for hosts
  that stated none cannot be judged either, and is taken as not reloaded yet.

## Sweep

Nothing obviated: `WebhostPage.webapp_build` is still read; no other message
changed.
