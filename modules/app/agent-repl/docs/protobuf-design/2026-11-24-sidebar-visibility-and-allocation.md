# Sidebar visibility and workspace resource allocation

The owner wants the sidebar's folds to follow one plain rule, and wants that
rule to decide which workspaces hold resources. A fold means "what is under
this is not pertinent right now": a row a fold covers is hidden unless it
works or holds an unread result, at every depth below the fold. Only the
NEAREST container's fold covers a row: an unfolded subsection inside a
collapsed repository section shows every one of its top-level rows. A
subsection with members always draws its header, because unfolding it is how
its hidden rows come back. Whether a row is shown then bounds whether its
workspace holds a page and a running shim: a workspace that is not shown never
has either.

Design mode: autonomous. Provenance: figma→idl (frontend.v1 is the sidebar's
server-driven UI).

## Core design principles

- **Not shown means no page and a parked shim.** In the owner's terms: if a
  workspace is not shown in the sidebar, it must necessarily and structurally
  be the case that its page is not loaded and its shim is parked. The inverse
  does not hold: a shown workspace need not hold either.
  - Consequences: the daemon derives one allocation value per workspace from
    which both the page and the shim are read, and a not-shown arm carries no
    page and no running shim by construction. The named exceptions (deploy
    mediators and workspaces in queue bypass mode, which are always shown; a
    sync merge's active phases and keep-alive beats; one keep-alive tick after
    a backlog or close) are stated at the derivation.
  - Reopens: the run rule `wsm.Workspace.Runs` (every visible workspace runs)
    and Emacs's own page-residency rules in `lisp/page-unload.el`.
  - Does not claim: that a shown workspace's shim runs, or that its page is
    loaded.
- **The daemon is the one source of truth for "viewed".** The viewed mark, the
  recency walk's order and every allocation clock read the same recorded
  interactions.

## Landed changes

### 1. `frontend.v1.RosterOpenRow.fold_cover` replaces `surfaced`

- WHAT: `RosterOpenRow.surfaced` (field 7, a bool) is retired and reserved.
  `RosterOpenRow.fold_cover` (field 9) is a `RosterOpenRowFoldCover` whose
  oneof states the row's standing under the folds: `uncovered`, `hidden` (a
  fold covers it and the sidebar does not draw it) or `surfaced` (a fold covers
  it and it is drawn anyway, because it works, holds an unread result, starts,
  has top priority, is selected, or stands above such a row).
- WHY: the webapp re-derived which rows a fold covers from the DOM, with
  descendant selectors that let a collapsed repository section reach into its
  subsections, which is the defect the owner reported. The daemon already
  decides coverage (`resolve/sidebar/taborder.go` `openRegion.fold`, and the
  children fold), so the row now carries the verdict and the stylesheet only
  draws it.
- Why a oneof and not a second bool beside `surfaced`: `surfaced` without
  coverage was a meaningless state the two bools could express; the three arms
  are the three states there are.
- Consequences: Emacs's strict decoder (`lisp/wire-roster.el`,
  `agent-repl-wire-decode-roster-open-row`) must learn `foldCover` and drop
  `surfaced` in the same change, or every roster push is refused. The webapp's
  fold rules in `webapp/src/styles.css` (the `.repo.folded .rows ...` block
  near line 1045 and the reduced-motion copy near line 5190) become rules
  keyed on the row's own class. `webapp/src/sidebar/row.ts`,
  `fold-fade.ts` and `open-count.ts` read the arm.
