# Sidebar section counts equal what the section shows

The owner's report (2026-10-08): a repository section's count, drawn while it
is folded, disagreed with the rows the section shows unfolded, and so did the
Recently Merged band's. The owner's ruling: a folded section's count equals
the number of entries the unfolded section shows. The Recently Merged band
also loses its fixed cap and shows as many merges as fit in the rail without
scrolling.

## Landed changes

### 1. `RosterSectionCount` counts the rows a client draws

- **What changed:** no field moved. The comments on
  `RosterSectionHeader.count` and `RosterSectionCount` now define the count as
  what the unfolded section shows.
  - A repository section counts every row of its tree, nested family rows
    included, that is not closed (`RosterRowClosed`).
  - The Recently Merged band counts its rows (each merged row is closed by
    design, and is exactly what the band shows).
- **Why the counts disagreed:** the daemon counted every row of the flattened
  tree, closed rows included. A closed, killed or nuked workspace still rides
  the wire, because Emacs reconciles its tabs from the flag, but the webapp
  never draws it (`expandVisibleRows`), so a repository holding one counted
  more workspaces than it showed.
- **Consequences:**
  - The daemon reads the count off the very rows the section carries, through
    the same `RosterRowClosed` fact the webapp drops a row by
    (`drawnLiveRowCount`, `resolve/sidebar/sections.go`).
  - The webapp draws a repository count verbatim, as before.

### 2. The Recently Merged band shows what fits, and counts that

- **What changed:** nothing on the wire. The daemon already sends every merged
  workspace, most recently merged first, with no cap. The webapp's ten-row
  scroller is gone.
- **Where "fits" is decided:** in the webapp, the only party that knows the
  rail's height. `fitMergedSection` (`webapp/src/sidebar/merged-fit.ts`)
  computes N, the merges that fit below everything else the pane draws, and in
  the same pass hides every row past N and writes N into the count. Nothing
  else writes either, so the two cannot drift.
- **Why folded and unfolded agree:** N is computed from the pane's height less
  the band's rows region, and a folded band's rows region takes no height, so
  both states compute the same N.
- **Re-fit:** after every roster push, and on every resize of the rail's
  scroller or of a pane (a section folding, the grouping switching, the window
  resizing).
- **Consequences:**
  - A band that is not laid out (the hidden grouping's pane) is left as drawn:
    every row under the daemon's count of every row, which is itself
    consistent. It is fitted when it lays out.
  - When the live sections already fill the rail, N is 0: the band's header
    stays and says "(0)", and unfolding it shows no rows.
