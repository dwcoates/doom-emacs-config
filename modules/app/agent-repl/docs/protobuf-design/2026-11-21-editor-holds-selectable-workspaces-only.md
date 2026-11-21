# Emacs holds only the workspaces the user can select

The owner ruled (2026-11-21) that Emacs handles only selectable workspaces, as
a structural invariant. Emacs held a perspective and a `WatchHostWorkspace`
stream for every open and hidden workspace: 125 streams and 126 perspectives,
of which about 11 workspaces were drawn or current. Every deploy walked each
of those streams through the handover (a `transferred` push, an adopt, an
unsubscribe and a resubscribe, a fresh snapshot and its hooks). Over the 12
hours before the ruling, Emacs logged 5209 reattach walks, with a median of
11.9s and a p90 of 34.4s each, almost all of them for workspaces nobody could
see.

## Context (assumptions, autonomous design)

- **Objective.** A workspace Emacs cannot show costs Emacs nothing, on a
  deploy or otherwise.
- **What is built.** The daemon states, per roster row, whether the editor
  holds the workspace. Emacs holds a perspective and a host stream for exactly
  those rows. Every event that can target a workspace Emacs does not hold
  rides the editor's daemon-wide stream instead of the workspace's own.
- **Frontend.** No webapp change: the sidebar draws nothing new. The new roster
  field is read by Emacs alone.
- **Systems.** Proto, daemon, Emacs.
- **Non-additive changes.** Three host-stream arms (`notification_clicked`,
  `evaluate`, `select_requested`) leave `agentrepl.v1.WatchHostWorkspaceResponse`
  and move to `agentrepl.v1.WatchDaemonResponse`. `wsm.Workspace.Runs()`
  becomes defined over `wsm.Workspace.Holds()`.

## Iteration sequence

Default order, walked by the agent alone: the roster's held fact, then the
daemon stream's editor events, then the composer draft the daemon keeps.

## Core design principles

### One predicate says which workspaces the editor holds, and the daemon owns it

- **The principle (owner ruling 2026-11-21, question 4.1).** The daemon tells
  Emacs which workspaces to hold; Emacs never works it out.
- **Consequences.** `wsm.Workspace.Holds()` is the one definition: the
  workspace is current, or the tab bar draws it (an open workspace in an
  unfolded section, or one surfaced inside a fold). A thread is held only
  while it is current. `wsm.Workspace.Runs()` is `Holds()` plus the cases that
  run a workspace nobody can see (a merge in a queue, a deploy mediator, a fold
  spare, a thread). The roster row carries `Holds()` as a presence marker, and
  Emacs reconciles on that marker alone.
- **Reopens.** Emacs's own fold and hide predicates
  (`agent-repl-open-tabs-folded-p`, `agent-repl-open-tabs-withheld-p`) and
  page-unload's fold and hide paths, which decided what Emacs kept loaded from
  its own reading of the sections.
- **Does not claim.** Nothing about which workspaces RUN changes beyond what
  `Holds()` adds to `Runs()` (see the `Withheld` consequence below).

### An event for a workspace never depends on that workspace's stream

- **The principle (owner ruling 2026-11-21, question 4.3.1).** A banner click,
  a rail click and an agent's evaluation reach Emacs on its daemon-wide
  stream, for every workspace, held or not.
- **Consequences.** There is one path for each of the three events, so none can
  target a stream that does not exist.
- **Does not claim.** Events that only a held workspace's page can raise
  (`open_in_editor`, `link_followed`, `load_page_requested` and the composer
  events) stay on the host stream.

### Folding or switching away closes the workspace in Emacs

- **The ruling (owner, 2026-11-21, questions 4.4.1 and 3.1).** A workspace
  that stops being held loses its perspective and its stream. A folded
  workspace the user picks is held while it is current and closes again when
  the user switches away. Its section stays folded. The composer's draft
  survives the close (see the correction under the draft entry below).
- **Accepted cost.** The perspective's own window layout and the file buffers
  it held are no longer that perspective's when it reopens. Its panels are
  rebuilt by panels-on-arrival.

### A picked workspace opens through the daemon

- **The ruling (owner, 2026-11-21, question 4.2).** Picking a workspace Emacs
  does not hold asks the daemon to select it. The roster push that marks it
  held opens it, and the roster's follow lands the frame on it.
- **Accepted cost.** A switch to a folded workspace waits for that round trip
  (about 50ms), on top of the page boot such a switch already paid.

## Landed changes

### `frontend.v1.RosterRow.held` and `frontend.v1.RosterThreadRow.held`

- **What changed.** A new presence marker on each row,
  `frontend.v1.RosterRowHeld` (field 77) and `frontend.v1.RosterThreadHeld`
  (field 7).
- **Why.** The daemon states the one predicate (principle above), so Emacs's
  set of perspectives and the daemon's `Runs()` cannot disagree.
- **Why a marker and not the tab's fold.** The tab's folded arm
  (`frontend.v1.RosterOpenRowTab`) answers whether the bar draws a number. It
  says nothing about the hidden section, the current workspace there, or
  threads, so Emacs had to combine it with the section and the selection
  itself. That combination was the second predicate the ruling retires.
- **Consequences.**
  - `wsm.Workspace.Withheld()` now answers `Drawn` for a workspace a fold
    covers that is drawn anyway (surfaced or current), since the tab bar draws
    it. `wsm.Workspace.Runs()` therefore runs a surfaced workspace, whose turn
    already runs. Accepted.
  - A closed workspace is never held, even while it is the selection.

### `agentrepl.v1.WatchDaemonResponse` gains three editor events

- **What changed.** `notification_clicked` (field 14), `select_requested`
  (field 15) and `evaluate` (field 16), each carrying the workspace's
  `workspace.v1.WorkspaceRef`. The host stream's arms 7, 10 and 12 are
  retired, and with them `agentrepl.v1.HostWorkspaceNotificationClicked`,
  `agentrepl.v1.HostWorkspaceSelectRequested` and `agentrepl.v1.HostEvaluate`.
- **Why.** A workspace Emacs does not hold has no host stream (principle
  above).
- **Consequences.**
  - `SelectInEditor` relays to the editor's daemon stream and selects itself
    only when no Emacs daemon stream stands.
  - `EvaluateInEditor` refuses `no_editor` when no Emacs daemon stream stands.
    An evaluation for a workspace Emacs does not hold runs in a neutral buffer
    whose `default-directory` is the workspace's directory.
  - Emacs answers an evaluation on the connection that delivered it.
  - An evaluation the daemon pushed to several Emacs streams waits until the
    last of them closes before it answers `no_editor`.
  - A feed link's `link_followed` still rides the source page's host stream,
    while the switch it causes rides the daemon stream, so Emacs may see the
    two in either order. Accepted: counting the source viewed does not depend
    on the switch having happened, and `link_followed` must keep its order
    with the host-stream events an `OpenInEditor` click raises after it.
  - Emacs selects a workspace it does not hold through the same one select
    queue as a held one: the queue's slots carry `workspace.v1.WorkspaceRef`s,
    not names, so two selections are still sent in the order they were made.

### RETRACTED: `agentrepl.v1.ReportComposerDraft` and `agentrepl.v1.HostWorkspace.composer_draft`

- **Correction (owner ruling, 2026-11-21).** The rpc, the host field and the
  daemon's `composer_drafts` table below were landed and then removed. Emacs
  already keeps a composer's text across the composer's death: its shutdown
  save writes the text into the workspace's prompt history as the unsent head
  (`agent-repl--input-save-on-shutdown`), and a new composer puts it back
  (`agent-repl--input-restore-unsent`). Closing a workspace now runs that same
  save. One mechanism restores a composer, it survives an Emacs restart, and
  it keeps pasted images, which the daemon's plain text did not.
- **Root cause of the error.** The recommendation (question 4.4.1) was made
  from the composer's buffer alone, without reading how its contents already
  outlive an Emacs restart. The existing persistence was the place to look
  first.

The retracted entry, as it was landed:


- **What changed.** A new rpc, `endpoint_report_composer_draft.proto`, and a
  new host-state field, `agentrepl.v1.HostComposerDraft` (field 9).
- **Why.** Closing a workspace kills its composer, and the composer's text
  lived only in its buffer (ruling 4.4.1).
- **How it is used.** Emacs reports the composer's text when it closes the
  workspace. The daemon keeps it durably, so it survives a handover. The next
  snapshot after the workspace opens carries it. Emacs puts it into an empty
  composer, then reports the draft cleared. A crash between the restore and the
  clear restores the same draft again, which duplicates nothing because the
  restore only fills an empty composer.
- **What it does not carry.** A draft a question or a highlight set aside: the
  set-aside already enters the composer's persisted history, so the up arrow
  reaches it after a reopen.
