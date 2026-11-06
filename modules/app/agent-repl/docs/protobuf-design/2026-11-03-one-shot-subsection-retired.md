# The sidebar's one-shot subsection is retired

The owner (2026-11-03): "let's stop supporting the special one-shot subsection in the OPEN section of the feed. instead, oneshots should organize themselves into subsections like any other workspace does".

This change ran as a one-shot workspace with contract changes pre-approved by the owner, so the design was settled here and landed directly. It reverses `sidebar-one-shot-subsection.md`, whose changes 1 to 4 are undone below. Its changes 5 and 6 stand.

## Core design principle: the one-shot mark draws a glyph and nothing else

- The principle: the mark (`wsm.Workspace.OneShot`) says whether the workspace finishes unattended. It never decides where the workspace is drawn. A one-shot sits where it is arranged, like any other workspace, and its row draws the one-shot glyph (`frontend.v1.RosterRowOneShot`).
- Consequences:
  - A one-shot files itself through the same placement brief every new root workspace closes its first message with (`workspace-subsection-placement`).
  - Setting or clearing the mark moves no row.
  - A placement never touches the mark, so moving a one-shot never stops it finishing unattended.

## Landed changes

### 1. `frontend.v1.RosterSubsection.one_shot` and `RosterSubsectionOneShot` are retired

- What: field 8 is reserved with its name, and the message is deleted. `kind` keeps the `named` arm alone.
- Why: no subsection is derived from the mark any more.
- Consequences: the webapp and the Emacs roster decoder refuse `oneShot` as an unknown field. The webapp's drag into the one-shot subsection, which marked a workspace one-shot, is gone with it.

### 2. `frontend.v1.RosterSubsectionExpanded.never_collapses` and `RosterSubsectionNeverCollapses` are retired

- What: field 3 is reserved with its name, and the message is deleted.
- Why: only the one-shot subsection carried it.
- Consequences: a one-shot filed in a named subsection folds with it. A fold still spares a one-shot's session (`wsm.Workspace.FoldSpare`, `open-tabs.md`), so the unattended work is never stood down by a fold.

### 3. `agentrepl.v1.ArrangePlaceWorkspace(s).keep_one_shot` is retired

- What: field 7 of both messages is reserved with its name. `wsm.OneShotMarks` and the mark-clearing placement write are deleted.
- Why: the mark was cleared on a placement only so the drop would take the family out of the one-shot subsection. Without it, clearing the mark would silently stop a one-shot from finishing unattended.
- Consequences: `agent-repld workspace place` sends no flag, and every placement leaves every mark as it is.

### 4. `agentrepl.v1.ArrangedEntry.one_shot` and `ArrangedOneShot` are retired

- What: field 3 is reserved with its name, and the message is deleted.
- Why: `GetSidebarArrangement` answers what the sidebar draws, and the sidebar draws a one-shot where it is arranged.

### 5. Leaving the one-shot state no longer asks the workspace to file itself

- What: documented on `agentrepl.v1.SetWorkspaceOneShot`, with no shape change. The daemon no longer sends `conversation.v1.PromptOrigin.PROMPT_ORIGIN_SUBSECTION_PLACEMENT`.
- Why: the brief (owner request, 2026-10-26) existed because a workspace leaving the one-shot subsection landed back among the unsectioned rows. A one-shot now stays where it is arranged, so an unmark has nothing to file.
- Consequences:
  - `workspace.Verbs.TakeBackOneShot` answers only an error, and `PendingPlacement` and `SubmitPlacement` are deleted.
  - The enum value stays, so prompt rows already stored with it still decode and still draw as no row.

### 6. A workspace a fold spares runs under the fold at boot

- What: `wsm.Workspace.Runs` runs a workspace whose tab a fold withholds when the fold spares it (`wsm.Workspace.FoldSpare`: a one-shot, or a pending merge). No shape change.
- Why: the one-shot subsection never collapsed, so a one-shot's tab was never folded. Filed in a collapsed subsection, a one-shot would be withheld at the next daemon start, though a fold never stands a running one down.
- Consequences: boot and fold agree on which sessions a fold leaves running.
