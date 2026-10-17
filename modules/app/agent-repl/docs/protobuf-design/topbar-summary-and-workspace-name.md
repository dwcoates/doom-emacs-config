# The topbar's summary line and its workspace-name line

The topbar's purple title line showed the workspace's generated summary, but
whenever the summary was missing it fell back to the workspace's name, and
whenever the branch differed from the line shown it appended ` · <branch>`.
Worktree branches are minted from the workspace name, so once a summary
existed the line routinely read `<summary> · <workspace-name>`. The owner ruled
(2026-10-17) that the purple line is the generated summary and nothing else,
and that the workspace's name gets a line of its own beneath it.

## Landed changes

### `frontend.v1.TopbarView.title` is the summary alone, and `frontend.v1.TopbarView.workspace_name` is a new element

- WHAT: `frontend.v1.TopbarView.title` became `optional`, and
  `frontend.v1.TopbarTitle.text` carries only a generated summary: the
  daemon's own distillation, or the vendor's ai-title until the first
  distillation lands. No workspace name and no branch is ever composed into it,
  as a suffix or as a fallback; with no summary the field is UNSET. A new
  always-set element `frontend.v1.TopbarView.workspace_name`
  (`frontend.v1.TopbarWorkspaceName`) carries the registry's workspace name.
- WHY, in the owner's terms: "it should just be the generated summary. no
  workspace name or branch name should be added to the purple summary. even
  as a fallback, it shouldnt be used." The name instead sits on its own line:
  "the topbar should be slightly taller, with the extra space being downward,
  and in that extra space, in the middle should be the workspace's name ...
  centered under the purple text summary (not necessarily the topbar's width,
  specifically centered relative to the purple text summary)."
- LAYOUT (the drawing the shapes were checked against):

  ```text
  +-TopbarView-------------------------------------------------------------+
  | [left: account cell]   +-heading------------------+   [right: chips]   |
  |                        | TopbarTitle (purple)     |                    |
  |                        |   TopbarWorkspaceName    |                    |
  |                        +--------------------------+                    |
  +------------------------------------------------------------------------+
  ```

  The flanks and the summary keep the strip's original band; the name sits in
  the extra band below it. The heading box shrinks to its content and is
  centered in the space between the flanks, and both lines are centered in
  that box, so the name's midpoint is the summary's midpoint, not the strip's.
- CONSEQUENCES:
  - The branch no longer appears anywhere in the topbar. The sidebar row's
    detail panel still states it (`frontend.v1.RosterRowDetailBranch`).
  - A workspace that has never been summarized draws no purple line at all;
    its name line still identifies it. The session-line reveal therefore
    anchors on the whole heading box, so it stays clickable without a title.
  - `topbar.Naming.Branch` and `topbar.Naming.DefaultBranch` lose their only
    reader in the topbar resolver (the title's branch rule), and
    `topbar.Naming.Title` duplicated `topbar.Naming.Slug`; the resolver keeps
    one name.
  - `--topbar-h` grows, and because the sidebar head shares the token, the
    two header dividers stay flush.
  - The vendor ai-title still stands in until the first distillation: it is a
    generated summary, not a name, so it is inside the ruling's "just the
    generated summary".

### The name stands in the summary's place when there is no summary, bold and larger

- WHAT: with `frontend.v1.TopbarView.title` unset, the client draws
  `frontend.v1.TopbarView.workspace_name` in the summary's place on the top
  band instead of on the name line. The summary is italic (still purple). The
  name is bold, in the foreground color, and a step larger than the summary
  and every cell of the band. The comments on both fields say so; no field or
  tag changed.
- WHY, in the owner's terms: "it can show just the workspace name in place of
  the purple summary. the workspace name can always be white and bold text. the
  summary should be additionally italicized. the name should be slightly larger
  text than the summary (or anything else on the main first row of the top
  bar), and i consider this workspace name row like row 1.5 and just for the
  workspace name."
- CONSEQUENCES:
  - This narrows the earlier "nothing stands in for a summary": the title's
    TEXT is still never anything but a summary, but the NAME element now takes
    the title's SLOT when the title is absent. The placement is the client's
    stylesheet keyed on the title's absence, so the daemon states nothing new.
  - "White" is the theme's foreground color: white in the dark theme, and the
    light theme's dark foreground there, because literal white would vanish on
    the light background.
  - The name line grows to fit the larger text (`--topbar-name-h` 0.95rem).
