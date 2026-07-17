# Design: the workspace drawer as a native GUI sidebar

Status: **Phases 1 AND 2 implemented, plus every §8 optional follow-up.**
The bridge is on by default (`agent-repl-sidebar-enabled`, disable to
opt out); the drawer remains available and canonical while open. The
`C-S-*` chords route through `agent-repl-sidebar-global-*` wrappers: an
open drawer keeps its exact semantics, otherwise cursor ops dispatch
into the webview via the `window.agentReplSidebar` hook and `C-S-n`/`p`
follow-navigate Emacs-side in view-model order. Priority badges ship as
cached data-URI PNGs; width is the `agent-repl-sidebar-width-px` custom
shipped as `width_px`; standalone (non-embedded) pages label themselves
and gate Emacs-only actions. §8 landed as: one-click permission
decisions (new `POST /sessions/{id}/permission` + roster join),
turn-preview and cost chips from an enriched `GET /sessions`, hover
cards, search/filter, drag-to-reprioritize (`set-priority` action),
merge-queue commit click (`show-commit` action → magit), desktop
notifications, and CSS-only animation with `content-visibility`
(virtualization judged unnecessary at real fleet sizes).

As-built deltas from the proposal: marks/expanded stay owned by the
singleton drawer buffer (the sidebar reads and mutates them there —
same shared-set semantics as "promote to global", zero drawer.el
churn); `cmd/claude-repld/main.go` needed the `/workspaces/` mux mount
alongside the server-side routes; the daemon's pre-existing
`activeTurnText` held the user prompt, so turn previews ride a new
translator-side assistant-text accumulator.

The one open item left is Q4 (drawer retirement), which is the user's
call — everything else in §5.4, §8, and §10 is resolved or shipped.
Scope: `webapp/` (`index.html`, `styles.css`, new `sidebar.ts`), `daemon/internal/`
(new `workspaces` package + `workspacecmd` extension), `shared/protocol.md` (new
workspace-stream section), Emacs side (`drawer.el` view-model extraction,
`workspace-status-export.el` successor, action-file watcher).

---

## 1. Target design

- The Emacs drawer becomes a **left sidebar inside the browser-based agent-repl GUI**.
- The sidebar is **fixed width horizontally**: window resizes are absorbed entirely by
  the output area (the feed column scales preferentially).
- The sidebar keeps the drawer's **full north/south stretch**: it spans the full
  viewport height, beside the topbar, feed, and composer — not below the topbar.
- **Hard requirement: feature parity** with the current drawer. Section 3 is the
  authoritative inventory; section 5 maps every entry. Anything that cannot map is
  surfaced in §5.4 / §10, never silently dropped.

---

## 2. Ground truth from investigation

### 2.1 What the drawer is (Emacs side)

`drawer.el` (2851 lines) renders a read-only-ish side window listing every agent-repl
workspace, refreshed by the 1 Hz status poll (`status.el:1716-1722`) behind a
render-signature memo (`drawer.el:1415-1463`). Every fact it renders comes from
Emacs-owned state:

- `agent-repl--workspaces` hash (name → plist) in `workspace.el` — agent/repl state,
  priority, summaries, git-clean, project/source dirs, merge flags, detail caches.
- `agent-repl--ws-render-status` (`workspace.el:575-690`) — the **single source of
  truth** for visual status shared by drawer, tab-bar, and picker.
- `agent-repl--merge-queue` / `agent-repl--in-flight-merges` / merge progress
  (`worktree.el`) — the MERGE QUEUE section's commit stream.
- Repo fold state `agent-repl--folded-repos` (`workspace.el:447-517`) — global,
  shared with the tab-bar.

### 2.2 What the GUI is (webapp + daemon)

- The webapp is a **single-session SPA**: one page binds one daemon session
  (`?session=<id>`), one WebSocket `/sessions/{id}/stream` (`main.ts:328`). It has
  **no workspace list, no peer-session model, no navigation** — `GET /sessions` is
  used only as a liveness probe (`ws.ts:142-153`).
- Layout is a single flexbox **column**: `body {flex-direction: column}`
  (`styles.css:137-144`) stacking `#topbar` / `#compact-progress-slot` / `#feed`
  (`flex:1`, `styles.css:341-348`) / `#composer`. There is no horizontal split today.
- The Go daemon holds per-session state only (`session.Session`, `session.go:89-185`;
  aggregate one-shot `GET /sessions`, `server.go:1397-1462`). It knows **nothing** of
  workspaces, repos, branches, priorities, summaries, or the merge queue. Broadcast
  fan-out is per-session (`session.go:1076-1120`); no cross-session channel exists.
- Emacs talks to the daemon over plain HTTP (`frontend-client.el`), embeds the webapp
  per workspace as an xwidget webview (`frontend.el:500-509`, `?composer=0`), and
  receives daemon events via sentinel files (`protocol.md:1372-1437`). The daemon
  already has one daemon→Emacs command channel: `workspacecmd` request files
  (`workspacecmd.go:1-14`).
- Existing Emacs→file aggregate export `workspace-status-export.el` writes
  `~/.claude-emacs/workspace-status.json` (agent/repl state, priority, summary,
  git-clean, dirs) for the `/workspace-status` skill — it is not wired to the daemon
  and lacks merge-queue, repo grouping, detail fields, and ordering.

---

## 3. Drawer feature inventory

Every feature, behavior, and keybinding, with source anchors. IDs are referenced by
the parity map in §5.

### 3.1 Layout & window behavior

| ID | Feature | Source |
|----|---------|--------|
| L1 | Left side window, slot 0, full frame height | `drawer.el:2300-2307` |
| L2 | Width = `round(0.243 × frame-width)`, constant at runtime, re-applied on show and on persp switch | `drawer.el:28-41, 2328-2338, 2359-2380, 2781` |
| L3 | No mode-line; near-black `#0a0a0a` background; soft word-wrap with content-aligned `wrap-prefix`; fringes 0; cursor hidden at column 0 | `drawer.el:189-195, 535-544, 476-480, 2452-2457` |
| L4 | Global visibility flag: drawer follows across workspace switches, re-shown/hidden/width-reapplied by the persp-activated hook | `drawer.el:2382-2389, 2740-2791` |
| L5 | Keyboard-inaccessible by policy: keyboard-driven window selection bounces to the MRU non-drawer window; mouse clicks exempt | `drawer.el:2793-2840` |
| L6 | Dedicated + `no-other-window` + `no-delete-other-windows` so display-buffer machinery never repurposes it (Magit fix) | `drawer.el:2300-2326` |
| L7 | `SPC o d` toggles; `q` hides; showing never selects the drawer window | `keybindings.el:726`, `drawer.el:2391-2481, 2842-2848` |

### 3.2 Content model

| ID | Feature | Source |
|----|---------|--------|
| C1 | Section order: MERGE QUEUE (omitted when idle) → MAIN (n) → HIDDEN (n) (omitted when empty) → MERGING (n) → MERGED (n); counts in headers; `(none)` placeholder; header + `─` rule styling | `drawer.el:1377-1413, 1010-1018, 1151-1166` |
| C2 | Workspace filter: excludes project-dir-less stubs, the persp-nil sentinel, and tombstoned (`:nuked-at`) entries | `drawer.el:596-629` |
| C3 | Section bucketing from render-status: `:merged`/`:merge-failed`/`:merge-conflict` → MERGED; `:merging`/`:merge-queued` → MERGING; repl-state `:hidden` → HIDDEN; else MAIN | `drawer.el:665-712` |
| C4 | Repo grouping by git common-dir with fold glyphs (`▾`/`▸`); folds are global and also hide workspaces from the **tab-bar**; group label face is larger/white | `drawer.el:1039-1137, 2150-2159`, `workspace.el:447-517` |
| C5 | Parent/child tree from `:source-ws-dir`; MAIN/HIDDEN flatten through git-merged ancestors; MERGING/MERGED preserve topology; cycle cap 16; indent 2 cols/level | `drawer.el:795-888, 1139-1149, 43-49` |
| C6 | Sort: priority rank, then name — roots and siblings alike | `drawer.el:635-649` |
| C7 | Entry = 2 lines: header (gutter, state glyph, priority badge PNG with text fallback, bold name, dirty `●`) + summary line (aiTitle; `…` when pending; `—` when absent) | `drawer.el:905-1008, 941-949` |
| C8 | State glyph palette: ⏳ init, ⌛ thinking, ✅ done, 💤 idle, ❓ permission, ❗ stop-failed, 🚫 start-failed, ❌ dead, 🔀 merged, ⛔ merge-failed, 💥 merge-conflict, 🔄 merging, 🕒 merge-queued, `·` default | `drawer.el:56-100` |
| C9 | Name color by status: init blue, thinking red, done/permission green, idle orange, stop/start-failed magenta; merge/dead states keep the default face (glyph carries the signal) | `drawer.el:917-939` |
| C10 | HIDDEN section entries dimmed | `drawer.el:232-235, 1020-1035` |
| C11 | Expanded detail lines (per entry): merge status (`update in progress/queued · N commits`), branch, merged into, `merged: 30m 20s ago` compound clock, ahead-master (patch-id `--cherry-pick` count, not SHA ancestry), ahead-`<source-branch>` (suppressed when source is trunk), last commit + relative time, dirty file count, last prompt ago, pending prompt count, one `merged in:` line per merged-in workspace — each with a distinct face | `drawer.el:2187-2298, 2050-2115` |
| C12 | Auto-expand ONLY MERGING-section entries on drawer show and on workspace switch; manual TAB unaffected | `drawer.el:714-722, 2435-2440, 2723-2729` |
| C13 | MERGE QUEUE section: commit-level stream (in-flight picks first from current commit onward, then queued FIFO); every current/conflict commit always visible; lookahead budget 3 across project boundaries; per-row `⟳/💥/⛔` glyph, SHA, subject truncated at 34 cols, elapsed `M:SS` clock only after 3 s; conflict detail line (`N files unmerged · resolver: phase M:SS`); project separators styled like repo headers but non-navigable | `drawer.el:147-167, 1168-1375` |
| C14 | `help-echo` tooltips on entries (`Workspace: X (hidden)`) and repo headers (`Repo: X (TAB to fold)`) | `drawer.el:1003-1006, 1105-1109` |
| C15 | Marked entries render a red `●` gutter glyph; current entry renders a `▶` arrow overlay (mark takes precedence) | `drawer.el:169-187, 446-474` |
| C16 | Selection stays vertically centered (`recenter`, clamped at list edges) | `drawer.el:482-512` |

### 3.3 Refresh model

| ID | Feature | Source |
|----|---------|--------|
| R1 | 1 Hz poll refresh when visible; render-signature memo skips no-op builds; content diff avoids flicker; cursor restored by entry identity; unfocused window-point preserved | `status.el:1716-1722`, `drawer.el:1415-1540, 1967-1986` |
| R2 | Cursor auto-syncs (and recenters) to the newly-active workspace on every workspace switch | `drawer.el:2703-2738` |
| R3 | `g` re-fetches detail caches for expanded entries, then re-renders | `drawer.el:1685-1697` |
| R4 | MERGED section auto-clears after 8 h of Emacs idle: each entry is finished (worktree + hash entry removed) | `drawer.el:197-208, 1990-2028` |

### 3.4 In-drawer keybindings (after mouse-clicking in; Emacs + evil motion state)

| ID | Key | Behavior | Source |
|----|-----|----------|--------|
| K1 | `j` / `<down>` | Next entry (workspaces AND repo headers are stops; entry block is the unit) | `drawer.el:1573-1586` |
| K2 | `k` / `<up>` | Previous entry, snapping to the entry block start | `drawer.el:1588-1608` |
| K3 | `RET` | Visit: persp switch; a MERGED entry is **reactivated** (merge flags cleared, persp + session re-established; errors when project-dir is gone); leaves the side window first so persp restore can't clobber panels | `drawer.el:1643-1683, 1612-1641` |
| K4 | `TAB` | On a workspace: toggle expanded detail (expanding runs the synchronous git detail-cache refresh); on a repo header: fold/unfold (also repaints the tab-bar) | `drawer.el:2161-2185` |
| K5 | `g` | Manual refresh (R3) | `drawer.el:1685-1697` |
| K6 | `q` | Hide the drawer (clears the global visibility flag) | `drawer.el:2462-2481` |
| K7 | `x` | Nuke targets; a MERGED target prompts `y-or-n-p` then `--finish-workspace` (removes worktree + hash entry); others take `agent-repl-nuke-workspace` | `drawer.el:1770-1787` |
| K8 | `d` | Kill targets; **refuses** MERGED targets | `drawer.el:1789-1800` |
| K9 | `i` | Read a prompt in the minibuffer, send to each target via `agent-repl--send` (history + pending-summary `…` transition); refuses MERGED; empty prompt is a no-op | `drawer.el:1810-1828` |
| K10 | `M` | Merge each target into its source/master (`SPC TAB M` equivalent); **accepts** MERGED (retry path, reactivates first); temp-switch to each target and back | `drawer.el:1830-1873` |
| K11 | `m` | Merge a child into the entry at point (`SPC TAB m` equivalent); accepts MERGED as destination | `drawer.el:1875-1889` |
| K12 | `n` | New child worktree branched from the entry (`SPC TAB n` equivalent, prompts for preemptive prompt); refuses MERGED | `drawer.el:1891-1903` |
| K13 | `f` | Fork the entry's Claude session into a new worktree (`SPC TAB f` equivalent); refuses MERGED | `drawer.el:1955-1965` |
| K14 | `H` | Toggle hidden (`:hidden` ↔ active; entry moves MAIN ↔ HIDDEN) | `drawer.el:1941-1953` |
| K15 | `+` / `-` | Cycle priority through `p05 → p1 → p2 → p3 → nil`, wrapping | `drawer.el:1905-1939` |
| K16 | `t` | Toggle mark (red `●`), auto-advance to next entry | `drawer.el:1749-1761` |
| K17 | `u` | Clear all marks | `drawer.el:1763-1768` |
| K18 | `C-c C-k` | Interrupt Claude in each target | `drawer.el:1802-1808` |
| K19 | `<left>`/`<right>` (+ evil `h`/`l`) | Blocked — the entry is the unit of selection | `drawer.el:377-380, 583-587` |
| K20 | Evil hardening: initial state `motion`; insert-entry keys (`I a A o O s S c C R`) blocked | `drawer.el:549-592` |
| K21 | Marks-or-point targeting: `x`/`d`/`i`/`M`/`C-c C-k` act on the marked set when non-empty, else the entry at point | `drawer.el:1728-1735` |

### 3.5 Global chords (from any Emacs window; drawer must be open)

| ID | Key | Behavior | Source |
|----|-----|----------|--------|
| G1 | `C-S-n` / `C-S-p` | Move the drawer cursor next/prev **and follow**: the workspace under the cursor becomes active live; skips repo headers, the already-active workspace, and MERGED entries (never reactivates as a scroll side effect); uses focused dispatch so `hl-line` persists | `keybindings.el:662-663`, `drawer.el:2568-2644` |
| G2 | `C-S-<return>` | Visit the entry at the drawer cursor (installed in `general-override-mode-map` to beat vterm/panel maps) | `keybindings.el:690-711`, `drawer.el:2646-2649` |
| G3 | `C-S-x` / `C-S-d` / `C-S-i` / `C-S-m` / `C-S-h` / `C-S-t` / `C-S-u` / `C-S-+` / `C-S--` | Nuke / kill / send-prompt / merge-into-master / toggle-hidden / toggle-mark / clear-marks / priority-up / priority-down against the drawer cursor entry, preserving the cursor afterward; dispatched without selecting the drawer window | `keybindings.el:664-672`, `drawer.el:2485-2699` |
| G4 | `SPC o d` | Toggle the drawer | `keybindings.el:726` |

---

## 4. Architecture

### 4.1 Principle: Emacs stays the model owner and exports a **view-model**

Every parity-critical semantic (bucketing precedence, flatten-through-merged trees,
patch-id ahead counts, merge-stream ordering, lookahead budgeting, fold effects on the
tab-bar) is subtle, tested Emacs logic. Re-implementing it in TypeScript means ~500
lines of drift-prone duplication.

**Decision: Emacs computes the drawer's structure and ships it as data; the browser is
a dumb renderer.** Refactor `drawer.el`'s content pipeline (`--insert-content` and its
helpers) so the section/tree/merge-stream assembly emits a serializable view-model
(sections → repo groups → workspace trees with per-entry fields; plus the commit
stream) that both the existing buffer renderer and the new exporter consume. Parity is
then structural, not re-derived.

The view-model snapshot carries per entry: name, `render_status` keyword, name-color
class, priority, summary (+pending flag), dirty flag, depth, marked/expanded flags,
auto-expand flag, hidden-dim flag, help text, and the full detail-field set (C11) when
cached; per section: label, count, ordered groups/trees; plus the merge-queue stream
rows (C13) with `started_at`/resolver timestamps so clocks tick client-side; plus the
current workspace name, fold set, marks, and a sidebar-visible flag.

### 4.2 Transport: Emacs → daemon → all browsers

- **Ingest**: Emacs `POST /workspaces/status` with the snapshot, on exactly the
  drawer's existing refresh triggers (1 Hz poll gated by the render signature, persp
  switch, explicit refresh). Matches Emacs's HTTP-only posture (`frontend-client.el`).
- **Fan-out**: new daemon package `internal/workspaces` holding the latest snapshot
  and a client set; new WebSocket endpoint `GET /workspaces/stream` broadcasting each
  snapshot to every connected sidebar; `GET /workspaces/status` one-shot for initial
  paint. Deliberately **not** injected into per-session streams — those have
  per-session `seq`/replay semantics the snapshot must not pollute.
- **Protocol**: new section in `shared/protocol.md` (workspace stream, its own
  version field), snapshot schema mirroring the view-model.

Rejected alternative: daemon tails `workspace-status.json`. File-watch latency,
partial-write hazards, and the file lacks most fields anyway; the exporter needs
rewriting either way, so an explicit POST is strictly better.

### 4.3 Actions: browser → daemon → Emacs

Every drawer action mutates Emacs-owned state (persps, worktrees, merge queue), so
actions route back to Emacs:

- Browser sends `POST /workspaces/action` (`{action, targets[], args}`) — actions:
  `visit`, `nuke`, `kill`, `send-prompt` (carries the prompt text), `interrupt`,
  `merge-into-source`, `merge-child`, `new-child`, `new-fork`, `toggle-hidden`,
  `priority-up/down`, `toggle-mark`, `clear-marks`, `toggle-expand` (triggers the
  detail-cache git refresh), `toggle-fold`, `refresh`, `hide-sidebar`.
- Daemon writes an action request file via an extended `workspacecmd` (the existing
  daemon→Emacs channel, `workspacecmd.go`); Emacs's watcher executes the existing
  drawer functions and the next snapshot reflects the result.
- Confirmations move browser-side: `x` on a MERGED entry confirms in the sidebar
  (K7's `y-or-n-p` equivalent) and sends `confirmed: true`; `i` reads the prompt in a
  sidebar input. Refusal rules (K8/K9/K12/K13 vs MERGED) are enforced Emacs-side as
  today **and** pre-checked in the sidebar for immediate feedback.

### 4.4 State ownership matrix

| State | Owner today | Owner after | Notes |
|-------|-------------|-------------|-------|
| Workspace registry, statuses, priorities, summaries, git facts | Emacs | Emacs | Snapshot is a projection |
| Merge queue + progress | Emacs | Emacs | Rows shipped in snapshot |
| Repo fold set | Emacs (global, tab-bar-shared) | Emacs | Sidebar fold round-trips so the tab-bar effect (C4) is preserved |
| Marked set | Emacs, buffer-local in the singleton drawer buffer | **Emacs, promoted to global state** | Preserves today's one-shared-mark-set behavior across the N per-workspace webviews |
| Expanded set + detail caches | Emacs, buffer-local | Emacs, promoted to global state | TAB already round-trips through git; auto-expand (C12) stays Emacs-computed |
| Sidebar visibility | Emacs (`--global-visible-p`) | Emacs | `SPC o d` and sidebar `q` both flip it; carried in the snapshot |
| Cursor/selection | Emacs, buffer-local | **Browser, page-local** | Safe: the drawer cursor already snaps to the active workspace on every switch (R2), so page-local + snap-on-switch is behaviorally identical |
| Scroll position | Emacs window | Browser | Centered-selection (C16) via `scrollIntoView({block:"center"})` |
| Session transcripts, queue, permissions | Daemon | Daemon | Unchanged |

### 4.5 Global chords after the move

The webviews are inside Emacs, so `C-S-*` stays bound in Emacs and dispatches into the
**active workspace's webview** via the existing `execute-script` host-bridge pattern
(`window.agentReplParkAtTail`, `window.agentReplChessStep` precedents): a
`window.agentReplSidebar(op, …)` hook. `C-S-n`/`C-S-p` follow-navigation is computed
Emacs-side against the same view-model Emacs just built (Emacs knows the order),
switching persps directly; the sidebar arrow follows via the snapshot. In-sidebar keys
(K1-K21) bind on the sidebar container after a click focuses it — mirroring the
drawer's click-in-then-type model (L5).

---

## 5. Parity map

### 5.1 Direct mappings

| ID | Sidebar landing |
|----|-----------------|
| L1 | `<aside id="sidebar">` as first body child; full viewport height (§6) |
| L3 | CSS: `#0a0a0a` background, no chrome, `overflow-wrap`, hidden native cursor/caret |
| L4 | Snapshot-carried visibility flag; every webview honors it, so the sidebar "follows" across workspace switches automatically |
| L7/G4/K6 | `SPC o d` flips the Emacs-owned flag (via the same action channel or directly in Emacs); sidebar `q` sends `hide-sidebar` |
| C1-C3, C5-C13 | Rendered verbatim from the view-model (Emacs computes structure; §4.1); glyphs/colors become CSS classes keyed on `render_status` |
| C4 | Fold toggle round-trips through Emacs so the tab-bar effect is preserved |
| C7 priority badges | Serve the badge PNGs (e.g. via the existing `--widget-assets` mount) with the same text fallback |
| C14 | `title` attributes (or hover cards, §8) |
| C15 | CSS classes for `▶` arrow (page-local cursor) and red `●` (snapshot-carried marks) |
| C16 | `scrollIntoView({block:"center"})` on cursor moves |
| R1 | Snapshot pushes on the same signature-gated 1 Hz cadence; browser re-renders on frame receipt (reuse the coalescer pattern); merge-queue clocks (C13) and `ago` clocks (C11) tick client-side from timestamps |
| R2 | Sidebar cursor snaps to `current_ws` when it changes in the snapshot |
| R3/K5 | `g` → `refresh` action |
| R4 | Untouched — pure Emacs behavior; sidebar reflects the emptied section |
| K1/K2 | `j`/`k`/arrows over the flattened entry list (repo headers included as stops) |
| K3/G2 | `visit` action → Emacs persp switch / MERGED reactivation |
| K4 | `toggle-expand` / `toggle-fold` actions (detail git refresh stays Emacs-side) |
| K7-K18, K21 | Actions per §4.3, marks-or-point targeting resolved sidebar-side (it knows marks + cursor) and validated Emacs-side |
| K19 | No horizontal cursor concept in the DOM list — trivially satisfied |
| G1 | Emacs-computed follow-navigation (§4.5), same skip rules |
| G3 | `execute-script` dispatch into the active webview's page-local cursor + shared marks |

### 5.2 Mappings that change mechanism but keep behavior

- **L2 (width)**: the target design **overrides** the drawer's fraction-of-frame width
  with a fixed width — this is the one deliberate, requested deviation. Default
  `--sidebar-width` chosen to visually match today's 24.3 % at typical frame sizes
  (proposal: `340px`, configurable via CSS var; see Q1).
- **L5 (keyboard inaccessibility)**: becomes "the sidebar never takes focus except by
  click" — no tab-stops (`tabindex="-1"` management), focus returns to the page on
  action completion where appropriate.
- **L6 (window hardening)**: Emacs-window-machinery concerns (`no-other-window`,
  dedication, the Magit `display-buffer` bug) have **no DOM analogue and nothing to
  preserve** — the hazard class does not exist in the browser.
- **K20 (evil hardening)**: evil states don't exist in the DOM; the underlying intent
  (no accidental text entry, entry-unit navigation) is inherent to a non-editable
  list.
- **K9/K7 dialogs**: minibuffer prompt / `y-or-n-p` become sidebar-native input and
  confirm affordances (same gating, same refusal rules).

### 5.3 Emacs-internal machinery that intentionally has no sidebar counterpart

Perf/plumbing internals whose *effects* are preserved but whose mechanisms are
Emacs-specific: dir-map O(N) reverse-lookup caching, render-signature memo +
content-diff (replaced by snapshot gating + browser reconciliation), overlay
management, `wrap-prefix` bookkeeping, window-point restoration, side-window
leave-before-switch (persp-internal; stays in the Emacs action executors).

### 5.4 Parity findings to decide (nothing silently dropped)

1. **Marks scope** (C15/K16/K21): buffer-local today but the drawer buffer is a
   singleton, so effectively global. Recommendation: promote to Emacs-global state so
   all webviews share one mark set (§4.4). Page-local marks would be a real behavior
   change.
2. **Cursor scope** (G1/G3): page-local per webview vs today's singleton cursor.
   Recommendation: page-local is behaviorally equivalent because of R2's
   snap-on-switch; noted for sign-off.
3. **Standalone (non-xwidget) browser**: `visit`, follow-navigation, and every
   persp-touching action are meaningless without the Emacs frame. Recommendation:
   sidebar renders read-only-plus-session-safe actions (interrupt, send-prompt) when
   `parent_ws` is absent, with Emacs-only actions disabled and labeled; full behavior
   in the embedded webviews. Needs a decision (Q5).
4. **Width fixedness** (L2): requested deviation, see §5.2.

---

## 6. Sidebar DOM and the fixed-width layout

`index.html` restructure (the only structural file; `main.ts` only fills fixed ids):

```html
<body>                                   <!-- flex-direction: row -->
  <aside id="sidebar">…</aside>          <!-- flex: 0 0 var(--sidebar-width) -->
  <div id="main-col">                    <!-- flex: 1 1 auto; min-width: 0;
                                              display:flex; flex-direction: column;
                                              height: 100vh -->
    <header id="topbar">…</header>
    <div id="compact-progress-slot"></div>
    <main id="feed">…</main>             <!-- unchanged: flex:1; overflow-y:auto -->
    <footer id="composer">…</footer>
  </div>
  <!-- overlays (#account-menu, #login-overlay) are position:absolute/fixed;
       unaffected -->
</body>
```

- `body` flips to `flex-direction: row` (`styles.css:137-144`); existing vertical
  stacking moves onto `#main-col`.
- **Fixed width / output scales preferentially** is exactly flexbox semantics:
  `#sidebar { flex: 0 0 var(--sidebar-width); }` never grows or shrinks;
  `#main-col { flex: 1 1 auto; min-width: 0; }` absorbs 100 % of horizontal resize.
  `min-width: 0` is load-bearing — without it flex items refuse to shrink below
  content width and the sidebar would get squeezed.
- **Full north/south stretch**: as a direct body child in a row flex, the aside spans
  the full viewport height beside topbar/feed/composer — matching the drawer's
  full-frame-height placement (L1).
- `#sidebar` gets `overflow-y: auto`, `background: #0a0a0a`, `border-right: 1px solid
  var(--border)`; hidden state via `display: none` driven by the visibility flag.
- New webapp modules: `sidebar.ts` (render + keys + actions) and a small
  `sidebar-store.ts` (latest snapshot + page-local cursor), reusing `WsClient` for
  `/workspaces/stream` and the render-coalescer pattern.

---

## 7. Coexistence and transition

- **Phase 1 (additive)**: sidebar ships behind an opt-in (`?sidebar=1` URL param, off
  by default). The Emacs drawer is untouched and remains canonical. Both are readers
  of the same Emacs state and both dispatch into the same Emacs action functions, so
  they cannot disagree or conflict — dual display is safe indefinitely.
- **Phase 2 (default flip)**: after parity validation, `frontend.el` appends
  `sidebar=1` by default; `SPC o d` keeps working (it flips the shared visibility
  flag, which now also drives the sidebar). Drawer available as fallback.
- **Phase 3 (optional retirement)**: user decision (Q4). The view-model extraction
  (§4.1) means the drawer's logic core survives either way; only the buffer-rendering
  half would retire.
- The snapshot exporter is additive alongside `workspace-status-export.el` (whose
  file consumers — the `/workspace-status` skill — keep working unchanged).

---

## 8. OPTIONAL follow-ups (bonus only — never traded against parity)

GUI-native capabilities the Emacs drawer could not offer, each a separate follow-up:

1. One-click permission approve/deny per workspace row (daemon already tracks
   `pending_permissions` per session; `permission-decision` command exists).
2. Live streaming preview of each workspace's in-flight turn (daemon holds
   `activeTurnText`).
3. Queue badges per workspace from `GET /sessions` queue data.
4. Hover cards showing the full C11 detail set without a TAB round-trip (for cached
   fields).
5. Search/filter box over workspaces.
6. Drag-and-drop priority reordering.
7. Per-workspace cost/usage rollups from `usage` frames.
8. Clickable MERGE QUEUE commits opening a diff view.
9. Desktop notifications (permission requests, merge completion) via the Web
   Notifications API.
10. Smooth expand/collapse animation and list virtualization for large fleets.

---

## 9. Risks

1. **Action round-trip latency**: browser → daemon → file → Emacs watcher → snapshot.
   File-watch debounce may make actions feel slower than the drawer's synchronous
   keys; may need optimistic UI or a faster Emacs-notify path.
2. **`workspacecmd` generalization**: the channel is one-shot/create-oriented today;
   extending it needs idempotency and an ack/error story (an action that fails
   Emacs-side must surface in the sidebar, not vanish).
3. **Snapshot staleness**: if Emacs dies, the daemon keeps broadcasting the last
   snapshot; the sidebar needs an age stamp + stale indicator.
4. **xwidget keyboard focus**: click-to-focus then in-sidebar keys must be verified
   inside `xwidget-webkit` (focus handoff between Emacs and the webview is quirky).
5. **View-model refactor blast radius**: `--insert-content`'s helpers are heavily
   tested; extraction must keep every existing drawer test green while adding the
   serializer.
6. **Two WebSockets per page** (session + workspaces): reconnect/backoff duplication;
   mitigated by reusing `WsClient`.
7. **Emoji glyph fidelity**: the state palette (C8) renders differently across
   browser font stacks than in Emacs; may need an SVG/emoji-font pass.
8. **Protocol evolution**: the workspace stream needs its own version gate so old
   webapps fail loud, mirroring `Layer2Version` discipline.

---

## 10. Open questions (decisions requested)

1. **Q1 — width**: fixed default value (proposal `340px`) and whether it is
   user-tunable (CSS var / config / Emacs custom pushed in the snapshot)?
2. **Q2 — marks promotion**: agree marks become Emacs-global shared state (§5.4.1)?
3. **Q3 — detail expand semantics**: keep today's on-demand git refresh round-trip
   (recommended, preserves K4 semantics), or eagerly include detail fields for all
   entries in every snapshot (heavier git cost, instant hover data)?
4. **Q4 — drawer end-state**: keep the Emacs drawer permanently as an alternative
   surface, or retire it after Phase 2?
5. **Q5 — standalone browser**: accept the degraded read-only-ish mode (§5.4.3), or
   scope the sidebar to embedded webviews only?
6. **Q6 — transport sign-off**: confirm POST-ingest + `/workspaces/stream` broadcast
   (§4.2) over the file-tail alternative.
7. **Q7 — action channel**: extend `workspacecmd` files (§4.3) vs adding a dedicated
   Emacs-side HTTP poll/long-poll for actions; file extension is recommended but the
   ack story (risk 2) may tip this.
