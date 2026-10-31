# A cold-gated workspace's page is held back until the user asks

The owner asked (2026-10-30) that a cold-gated workspace's feed not be loaded
when the user switches to it. A subtle "load" button stands at the centre of
where the feed would be, and a click loads the feed. Switching rapidly past
cold-gated workspaces then costs the one web process no page per pass.

## Why the gate stands without the page

The webhost already draws the shown workspace's cold gate itself
(`webapp/src/host/gate.ts`), from the roster's `frontend.v1.RosterColdGate`,
whether or not the page is loaded. So holding the page back loses nothing the
user must answer.

## Why the editor loads the page

Emacs owns the host's desired state (`lisp/webhost.el`): which pages it holds,
at which URL, and the replay to a host that loads. A page the webhost loaded on
its own would be one Emacs does not know it holds. It would never be unloaded,
never announced unloading to the daemon, and lost on the next replay. So the
click goes to Emacs.

The webhost reaches Emacs only through the daemon, as the rail's row click
does (`agentrepl.v1.SelectInEditor`).

## Landed changes

### `agentrepl.v1.LoadPageInEditor` (new RPC)

- **What changed.** A new WEB-section RPC, `endpoint_load_page_in_editor.proto`.
  The request names the workspace. Success is empty. The error arms are the
  four the workspace resolution refuses with, and `no_editor` when no host
  stream holds the workspace.
- **Why `no_editor` and not a daemon fallback.** Only Emacs can load a page.
  The webhost draws the button only for a page Emacs held back, so a click
  nobody hears is a webhost out of step with its editor: it is refused and
  recorded at warn, and the button is given back.

### `agentrepl.v1.WatchHostWorkspaceResponse.load_page_requested` (new arm)

- **What changed.** A new event arm carrying the empty
  `agentrepl.v1.HostWorkspaceLoadPageRequested`. The stream it rides names the
  workspace.
- **Why an event.** A click belongs to the moment it was made, so it is never
  replayed to a late subscriber.

## The host calls

- `holdBack(id, dir)` records a held-back page. While it is shown with no
  frame, the webhost draws the load button. The dir completes the
  `workspace.v1.WorkspaceRef` the button's call names.
- `show(id)` of a held-back page hides the page shown before it and answers
  `held-back`.
- `ensure(id)` ends the hold, and the page loads in the button's place.
  `remove(id)` ends it too.
