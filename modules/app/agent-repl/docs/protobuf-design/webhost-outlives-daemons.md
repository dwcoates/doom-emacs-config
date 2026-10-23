# The web view host outlives the daemons it is served by

## Landed changes

### 1. The host states its build on its page stream, and a deploy reloads a stale host

- What:
  - `agentrepl.v1.WatchPageRequest` gains `optional WebhostPage webhost = 2`, present only on the web view host's own page stream (its sidebar's), carrying `webapp_build`.
  - `agentrepl.v1.WatchDaemonResponse` gains the push arm `DaemonReloadWebhost reload_webhost = 11`, sent only on Emacs streams, naming the build to reload the host onto.
  - The daemon holds each host stream's build while it stands; the deploy's webapp step pushes `reload_webhost` when a standing host's build is not the one it built.
- Why: a handover no longer reloads the host (2026-10-23, so the page on screen never blanks for a daemon change), and the host was the one document nothing else brought onto a new build: after the first handover-only deploy the live host kept its old code and lacked `setDaemon`, so its sidebar kept dialing the departed daemon.
- Why the push goes to Emacs: Emacs owns the host and every reload of it (`lisp/webhost.el`); the host only applies what Emacs tells it.
- Why on the page stream: the host has no workspace, so it has no `WatchWebWorkspace` to state a build on, and its page stream is the one stream it always holds.
- Consequences:
  - A host whose build predates a call Emacs makes answers `unknown-call` (the call script checks the method exists), and Emacs reloads it on the live daemon; this covers a host too old to state its build at all.
- Decided under the owner's delegation; additive.
