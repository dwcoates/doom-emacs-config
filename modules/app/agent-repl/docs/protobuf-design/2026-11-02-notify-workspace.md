# NotifyWorkspace: an agent-posted desktop banner attributed to a workspace

The owner asked for `agent-repld workspace notify`: any workspace sends a desktop notification on behalf of a target workspace (itself or another), and clicking it switches to Emacs and then to that workspace. The daemon already owns every desktop banner (`daemon/internal/desktopnotify`), whose macOS click runs the daemon's own `SelectWorkspace` and activates Emacs, so the change is one agent-API rpc over that notifier.

Designed autonomously (no co-design was asked for).

## Assumptions

- Objective: an agent can raise a desktop banner for any open workspace whose click selects that workspace in Emacs.
- Built: one rpc (`agentrepl.v1.NotifyWorkspace`), its CLI verb `workspace notify WS TEXT`, and a line in the workspaces skill.
- No frontend component: the banner is drawn by the platform's program, not by the webapp.
- Systems: the daemon (server, desktopnotify, CLI) only. No editor change: the click path (`-execute` running `agent-repld call SelectWorkspace`, `-activate org.gnu.Emacs`) already exists.
- No non-additive change to the contract.

## Iteration sequence

Protocol (ConnectRPC, the service's own) → inventory (one rpc in the AGENT API section) → conventions (the package's standing `result { success; error; }`) → the one endpoint's shapes.

## Landed changes

### NotifyWorkspace (endpoint_notify_workspace.proto, service.proto AGENT API section)

- WHAT: a request naming the target `workspace.v1.WorkspaceRef` and the banner's `message`; a success whose `delivery` arm is `posting` or `editor_focused`; an error whose cause is `unknown_workspace`, `workspace_ref_mismatch`, `workspace_closed`, `no_banner_program` or `standing_down`.
- WHY: the banner rides the notifier every other banner rides, so it is titled by the target's name, grouped with the target's banners, chimed, held for a focus claim, and clicked through to the target.
- The banner is titled by the target workspace's name, never the sender's. The sender writes whatever attribution it wants into the message. A separate title field was rejected: the title is what tells the user which workspace the click lands on, and letting a caller replace it would let a banner misstate where its click goes.
- `editor_focused` is a success arm, not an error: the package-wide rule is that a focused Emacs gets no banner (desktopnotify package doc), so the daemon declining is the answer to a well-formed request, and the caller learns the user is already at the editor.
- `no_banner_program` and `standing_down` are decided synchronously in `desktopnotify.Notifier.Post` before the banner's goroutine starts, so the caller is told instead of reading "posting" for a banner that cannot happen. The platform program's own failure happens after the answer and is recorded in the log only (`daemon.desktopnotify.post`), as the `posting` arm's comment says.
- `workspace_closed`: a closed workspace has no tab, so a click selecting it would lead nowhere useful; the request is refused rather than raising a misleading banner.
- The attention marker is deliberately not set: it means an unseen vendor notification, cleared by `AsksSettled` and selection, and an agent's banner is neither an ask nor owned by the target's session.
- Consequences: `desktopnotify.Notifier.Post` now answers a decision (`Poster` interface changes with it), and the missing-program record moves from the banner's goroutine to `Post` itself, losing its `title` field (the banner is not composed yet when the program is known missing).
