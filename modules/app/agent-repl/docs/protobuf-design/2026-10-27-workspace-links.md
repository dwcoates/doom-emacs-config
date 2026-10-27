# Workspace links

The owner asked to replace a workspace's single "principal link" with many
links. An agent stores each link through the agent API under a short label
(one sentence, two to eight words); clicking the purple summary in the topbar
drops a menu of the labels, each a hyperlink. Storage is unlimited, but the
menu shows at most five, and the agent is told when that cap is reached so it
chooses which five the menu shows, ranked by relevance to the developer working
with the agent and to the stakeholders the developer reports the work to.

Owner rulings, 2026-10-27:

- The stored principal links are not migrated: none exist (`wsm.db` held no
  `principal_link` value), and the owner said to start fresh.
- A summary with exactly one shown link opens it on click; two or more drop
  the menu.
- The menu lists links most recently set first.
- No cap on how many links are stored; a cap of five on how many are shown,
  and the agent chooses the five once the cap is reached.
- The owner delegated the contract and the implementation end to end ("just
  do the whole thing e2e yourself"), matching the standing ruling of
  2026-10-13 (decide contract changes and carry them through, report after),
  so this record is written without per-increment selections.

## Landed changes

### `agentrepl.v1.ChangeWorkspaceLinks` replaces `agentrepl.v1.SetWorkspacePrincipalLink`

- WHAT: `endpoint_set_workspace_principal_link.proto` and its rpc are deleted.
  `endpoint_change_workspace_links.proto` declares `ChangeWorkspaceLinks`,
  whose request names a workspace and one `change` arm: `set { url, label }`,
  `remove { url }`, `show { urls }` or `clear {}`. The success arm answers the
  workspace's whole `WorkspaceLinks`. The error arms are the four every
  workspace-subject rpc shares plus `invalid_link { url, reason }`,
  `invalid_label { label, reason }`, `unknown_link { url }` and
  `too_many_shown { named, limit }`.
- WHY one rpc with four arms rather than four rpcs: every arm is a change to
  the same stored list, answers the same `WorkspaceLinks`, and shares every
  refusal but its own; `SetWorkspaceCorrespondence` and the deleted rpc already
  spell "a change to one attribute" as an arm oneof.
- `set` on a stored url relabels it and moves it to the front of the
  most-recently-set order, keeping its placement. A new link is shown while
  fewer than five are shown, and stored unshown otherwise. THE CAP IS NEVER
  RESOLVED BY THE DAEMON EVICTING A SHOWN LINK: which links matter is the
  agent's judgment, so the daemon only reports the full menu
  (`WorkspaceLinksMenu.full`) and the agent answers with a `show`.
- `show` names the exact shown set (one to five stored urls, no repeats);
  every other link becomes unshown. The order of `urls` carries nothing: the
  menu always draws most recently set first, per the owner's ruling.
- The label rule (two to eight words, one sentence, no line break) is enforced
  by the daemon as a typed refusal, so a label the menu cannot draw is
  unrepresentable in storage rather than merely discouraged by the metaprompt.
- Alternatives rejected: a `bool shown` per link (the state-enum rule: the
  placement is a two-arm oneof); a daemon-side LRU eviction of the oldest
  shown link (it makes the daemon decide relevance, which the owner gave to
  the agent).

### `agentrepl.v1.WorkspaceLinks` (new shared file `workspace_links.proto`)

- WHAT: `WorkspaceLinks { repeated WorkspaceLink links; WorkspaceLinksMenu
  menu }`, `WorkspaceLink { url, label, WorkspaceLinkPlacement placement }`,
  placement arms `shown` / `unshown`, menu arms `room { free }` /
  `full { limit, unshown }`.
- WHY shared: `ChangeWorkspaceLinksSuccess` and `WorkspaceStatus` both carry
  it, so a change's answer and a later status read describe the links
  identically.
- `full` carries `unshown`, which may be zero (exactly five links, all shown):
  the agent acts only when `unshown` is non-zero.

### `agentrepl.v1.WorkspaceStatus.links` replaces `principal_link`

- WHAT: field 10 (`principal_link`) is reserved; field 11 is
  `WorkspaceLinks links`.

### `frontend.v1.TopbarTitle.links` replaces `TopbarTitle.link`

- WHAT: field 2 (`link`) is reserved; field 3 is
  `optional TopbarTitleLinks links`, a oneof `opens` of `single`
  (`TopbarTitleLink { label, url }`) or `menu`
  (`TopbarTitleLinkMenu { repeated TopbarTitleLinkMenuItem items }`, each
  item `{ label, url }`).
- WHY a oneof: under figma→idl the client never derives; whether the click
  opens a link or drops a menu is resolved by the daemon from how many links
  are shown, so the client never counts.
- WHY a new field number rather than retyping field 2: during a rolling
  handover a page of the previous build can read a push of the new one, and a
  retyped field 2 would decode as garbage rather than as an absent link.
- Only SHOWN links reach the topbar, most recently set first. With none shown
  (every link removed, or every shown one removed while unshown ones remain)
  `links` is unset and the summary is plain text.

Layout:

```text
+-TopbarTitle---------------------------------------+
| [summary text, underlined while links are shown]  |
+---------------------------------------------------+
  +-TopbarTitleLinkMenu (open after a click)--+
  | +-TopbarTitleLinkMenuItem---------------+ |
  | | Merge queue design doc                | |
  | +---------------------------------------+ |
  | +-TopbarTitleLinkMenuItem---------------+ |
  | | Upstream SDK issue                    | |
  | +---------------------------------------+ |
  +-------------------------------------------+
```

- Consequences:
  - The daemon's `wsm` store gains a `workspace_links` table (layout 85,
    additive). The `workspaces.principal_link` column is left in place,
    unread and unwritten, because dropping it is a breaking step that would
    turn the deploy into a restart; `wsm/migrate.go`'s own rule ships a drop
    as a later deploy once no running build reads the column. The drop is a
    row in `org/todo.md`.
  - The CLI's `workspace set -principal-link` / `-no-principal-link` flags
    are replaced by a `workspace link` verb.
  - The metaprompt's "Principal links" section becomes "Workspace links".
