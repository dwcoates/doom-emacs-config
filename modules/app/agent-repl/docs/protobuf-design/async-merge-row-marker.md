# Async merge as a row marker

The owner wants a workspace whose own branch merges async (in a hidden merge
workspace) to keep showing its real status: the sidebar row's name in its
status color with only the status dot purple, and the Emacs tab with only its
`[N]` bracket purple. Sync merges stay exactly as they are.

## Landed changes

### 1. `frontend.v1.RosterRow.async_merging` replaces the `async_merging` status arm

- WHAT: the status arm `frontend.v1.RosterRow.status.async_merging` (tag 63,
  `RosterRowStatusAsyncMerging`) is retired and reserved. A new optional
  marker `frontend.v1.RosterRow.async_merging` (tag 64,
  `frontend.v1.RosterRowAsyncMerging`, empty) carries the fact beside the
  real status arm. `render-colors.json` gains `async_merge_mark`
  (`ink`, `motion`, `tab_bracket`), validated by the daemon's vocab loader and
  asserted by the webapp and ERT suites.
- WHY: the arm replaced the workspace's own status, so neither surface could
  draw the name in its status color. The owner asked for the merge to show
  only in the dot (sidebar) and the `[N]` bracket (tab bar). Both renderers must
  read one wire fact, so the fact is a marker the daemon sets, never a
  renderer inference.
- PRECEDENCE: the sidebar applies the async-merge ink only to a PLAIN dot
  (`dot` glyph, no `glyph_inks` row, no one-shot glyph). Gate glyphs, the
  one-shot glyph, merge glyphs and inked dots (compacting's orange) outrank
  it. The marker is set on ANY status arm, where the retired arm only ever
  replaced the idle rung's green/yellow arms; so a working (red) row now
  shows a purple dot too, per the owner's "overrides the plain idle/working
  dots".
- CONSEQUENCES:
  - `daemon/internal/resolve/sidebar/status.go`: `asyncMergeArm` is gone;
    `asyncMergingMarker` sets the marker in `rows.go`. `viewReadsResult` no
    longer special-cases the arm, since the real turn-end arm stands under
    the marker and reads as usual.
  - `daemon/internal/resolve/ladder/projection.go` drops the arm's claim.
  - `lisp/roster.el` drops `:async-merging` from the settled statuses;
    `lisp/workspace.el` drops its state icon. The tab bar draws no glyph for
    the marker.
