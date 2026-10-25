# The sidebar's one-shot subsection

The owner (2026-10-24): "setting or unsetting a workspace as a one-shot in daemon should add that workspace to the one-shot subsection for the repository (creating if it doesnt exist) in the sidebar's OPEN section. when the one-shot subsection is empty, it should remove itself (it's special like that). the one-shot subsection should always be the last subsection of the workspace (its special like that)". "Last subsection of the workspace" means the last entry of the repository's section.

This change runs as a one-shot workspace with contract changes pre-approved by the owner, so the design was settled here and landed directly, without the interactive sequence.

## Core design principles

### One source of truth: the one-shot subsection is derived from the mark

- The principle: membership is never stored. The one-shot mark (`creation_jobs.one_shot`, read as `wsm.Workspace.OneShot`) is the only fact, and the subsection is computed from it on every roster publish.
- Consequences: every path that sets or clears the mark moves the row with no move code of its own. That includes `SetWorkspaceOneShot`, a create with `one_shot`, the merge queue's own `SET one_shot = 1`, and a daemon boot. The subsection exists exactly while it has rows, and it is drawn last by construction.
- What it does not claim: it does not change what the mark means or who may set it. `SetWorkspaceOneShot`'s refusals stand, including the one for a child its owner lands.

### A family never splits, and moves whole (owner rulings 2026-10-14 and 2026-10-20)

- The unit is the family. A family is drawn in the one-shot subsection while any OPEN member is marked. A closed or hidden member's mark does not count, since finished one-shots keep their mark and are closed.
- Placing a family elsewhere (`ArrangePlaceWorkspace`, `ArrangePlaceWorkspaces`) clears the mark of every member that is not closed, in the same write. Otherwise the family would stay in the one-shot subsection after being dropped elsewhere. A closed member keeps its mark, since a finished one-shot is still one.

## Landed changes

### 1. `frontend.v1.RosterSubsection` gains a kind arm

- What: `key` (field 1) is retired. A `oneof kind` holds `named` (`RosterSubsectionNamed { RosterSubsectionKey key }`) or `one_shot` (`RosterSubsectionOneShot {}`).
- Why: the owner's design brief asked for the special subsection to be an arm, not a magic title. Only a named subsection has a stored identity, so its key lives on that arm, and a key-less one-shot subsection cannot be represented with an empty key.
- Consequences:
  - Every reader of the subsection key moves to `kind.named.key`: the webapp sidebar (fold gesture, drag destination and anchor, the rename and delete menu), the Emacs roster decoder and `rosterwalk`.
  - The one-shot subsection cannot be renamed, moved, deleted, used as a destination or used as an anchor through `ArrangeSidebar`, because nothing in that verb can name it. The brief's "typed refusal" for those requests is replaced by unrepresentability, which is strictly stronger.

### 2. `RosterSubsectionExpanded.collapse` gains `never_collapses`

- What: a third arm, `RosterSubsectionNeverCollapses`. The one-shot subsection always carries `expanded { never_collapses }`.
- Why: a subsection collapse stands down its workspaces' sessions, and one-shots are working unattended. Folding the one-shot subsection would stop the very work it shows.
- Consequences:
  - The webapp's fold gesture does nothing on that arm, as on `holds_current`.
  - Its tabs are folded only when the repository itself is collapsed, as for any subsection.
  - `UpdateSidebarView` needs no new target.

### 3. `ArrangePlaceWorkspace(s)`: a placement clears the family's one-shot marks

- What: documented on both messages, with no shape change.
- Why: dragging a row out of the one-shot subsection is an ordinary placement. The placement must take the family out of the one-shot subsection, or the drop would not show.
- Consequences: `wsm`'s placement write clears `creation_jobs.one_shot` for every member of each placed family that is not closed, in the same transaction.

### Dragging INTO the one-shot subsection

- It is `SetWorkspaceOneShot(workspace, one_shot)` on the dropped workspace, with no `ArrangeSidebar` change. Marking one member brings its whole family in. A refusal, such as `owner_lands`, is drawn at the drop, as any refused drop is.

### 4. `agentrepl.v1.ArrangedEntry` gains `one_shot`

- What: a third arm, `ArrangedOneShot { repeated ArrangedWorkspace workspaces }`, listed last and only while it has families.
- Why: `GetSidebarArrangement` answers what the sidebar draws. Without the arm, an agent would read a marked family as an unsectioned one, at a place where the sidebar does not draw it.
- Consequences: an agent sees the one-shot subsection, and still cannot name it in `ArrangeSidebar`.

### 5. `agentrepl.v1.SetWorkspaceOneShotSuccess` gains `roster_revision`

- What: the revision of the roster resolved after the mark changed, as `ArrangeSidebarSuccess.roster_revision` carries for a placement.
- Why: a drag into the one-shot subsection is `SetWorkspaceOneShot`. The page holds its drawing until the push that shows the change, and without a revision it could only let go at once, so an older push would put the row back for an instant.
- Consequences: the daemon reads the revision after `SetOneShot` republished, as `ArrangeSidebar` does.

### 6. The user taking a workspace back clears its mark (owner addendum, 2026-10-24)

- What: documented on `SetWorkspaceOneShot`, with no shape change. A prompt the user submits (`promptorigin.UserSent`: the editor's and the webapp's user-typed origins) clears the mark before it is delivered, and so does reopening a closed one-shot. A prompt from another workspace or from the daemon does not.
- Why: no agent has to remember to unmark itself when the user steps in, whether the one-shot finished, failed its deploy and stayed open, or is mid-work.
- Consequences: the clear is `SetOneShot(false)` asked by the user, so the one-shot subsection updates with no code of its own. It is recorded at INFO under `daemon.workspace.take_back_one_shot`, naming the workspace and `cleared_by`.
