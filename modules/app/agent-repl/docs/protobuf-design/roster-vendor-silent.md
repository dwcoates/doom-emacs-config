# Roster row: the vendor-silent working step

The owner asked (2026-11-08) that a sidebar row whose footer shows the working
step `waiting` (`frontend.v1.FooterSubStatusWorkingWaiting`: a turn runs, but
the vendor has been silent past the daemon's 45s vendor-silence threshold)
draw a HYBRID: the BLUE status dot of an unusable workspace, while keeping the
working status's red name and its breathing. Their reasoning: the step is a
working substatus, so progress is still being attempted, but it is
semantically a disconnection, and the session may soon detach for good.

Design ran in autonomous mode.

## Assumptions

- Objective: the roster row distinguishes a vendor-silent turn from a thinking
  one, in the hybrid colors above.
- Built: one new roster status arm, its projection in the daemon, and its
  rendering in the webapp sidebar and the Emacs tab bar.
- Frontend: yes (the sidebar row). The row's layout does not change; only
  which status arm is set.
- Systems: daemon (`resolve/sidebar`, `resolve/ladder`, `vocab`), webapp
  (`sidebar/tones.ts`), editor lisp (`wire-roster.el`, `status.el`,
  `page-unload.el`, `workspace.el`), and the shared vocabulary file
  `proto/vocab/render-colors.json`.
- Non-additive changes: none. A row that drew `thinking` while the vendor was
  silent now draws `vendor_silent`.

## Iteration sequence

One increment: the roster status arm. No transport, endpoint or convention
stage applies, since the change adds one arm to an existing oneof.

## Landed changes

### 1. `frontend.v1.RosterRow.status` gains `vendor_silent` (tag 72)

- WHAT: a new empty arm message `frontend.v1.RosterRowStatusVendorSilent`,
  set when the footer's status is `working` with its `waiting` substatus.
- WHY: every roster status is a oneof arm, and the hybrid is a distinct thing
  the row draws, so it gets its own arm rather than a field on `thinking`
  (the roster arms are deliberately empty: the set arm is the whole
  assertion).
- One source of truth: the daemon's `projectArm` reads the footer status the
  footer resolver already produced, so the roster arm follows the exact
  vendor-silence decision (`resolver.vendorSilent`) the footer used. There is
  no second timer.
- Colors live in `render-colors.json`, not in the proto: `roster_status`
  names it `red` (the workspace's color: the row's name, the tab's ground),
  and `glyph_inks` inks its dot `blue`. This reuses the mechanism compacting
  already uses (a red workspace with an orange dot), so every renderer that
  reads the shared file draws the hybrid with no renderer-specific logic.
- Motion: the webapp's `armMotion` breathes it, as it does `thinking`.
- Ladder claim: `Thinking`, the same claim the footer's `working` status
  makes, so the sidebar's claim and the footer's stay one.
- Consequences:
  - The one-shot target glyph is NOT drawn on this arm (it is absent from
    `one_shot_glyph_arms`), so a silent one-shot still shows the blue dot.
    Hiding the warning behind the target would defeat the arm's purpose.
  - The Emacs tab bar inherits the hybrid through the same `glyph_inks` row:
    a red tab with a blue dot before its name.
  - Page unload holds the page as `working`, as `thinking` does.

Obviated-declaration sweep: nothing obviated; the change only adds.
