# Design: MERGE QUEUE drawer section

Status: **design only — not implemented.**
Scope: `modules/app/agent-repl/` (`drawer.el`, `worktree.el`, `status.el`).

---

## 1. Motivation

The drawer's `MERGING` section (`drawer.el:1036-1037`) is a flat list of workspace
names. During an actual merge it tells the user nothing: no queue position, no
target, no progress, no idea whether git is picking commit 1 of 12 or whether a
`claude -p` resolver has been chewing on a conflict for ninety seconds.

Meanwhile the merge machinery is genuinely rich. It has per-target-dir FIFO
sub-queues that drain concurrently (`worktree.el:4576-4585`), an in-flight set with
start timestamps (`worktree.el:4690-4710`), a conflict/auto-resolve loop
(`worktree.el:3344-3366`), and a commit range whose size is computed and then thrown
away (`range-count`, `worktree.el:3324`).

**Goal:** a top-of-drawer section that renders merge activity at the *git action*
level — which commit, of how many, into which target, how long, and what git or the
resolver is doing right now.

---

## 2. Placement and section replacement

### 2.1 Position

New section `MERGE QUEUE`, inserted in `agent-repl-drawer--insert-content`
(`drawer.el:1013-1040`) **before** `MAIN`. Final order:

```
MERGE QUEUE   (new, top)
MAIN
HIDDEN        (omitted when empty)
MERGED
```

### 2.2 MERGING is replaced, not supplemented

`MERGING` is deleted. This is not an aesthetic call — it is required for
correctness.

Drawer rows are identified by a bare `agent-repl-drawer-workspace` text property
(`drawer.el:872-874`), and `--goto-workspace-line` (`drawer.el:1154-1166`) returns the
*first* match scanning from `point-min`. A workspace rendered in two sections would:

- break cursor restore after every redraw (point yanked to the top section),
- render marks and expansion in both places simultaneously
  (`drawer.el:1303-1306`, `1589-1592`).

Fortunately the swap is exactly 1:1. `--workspace-section` (`drawer.el:575-583`)
buckets into `:merging` precisely the workspaces whose `--ws-render-status` is
`:merging` or `:merge-queued` (`workspace.el:521-554`). `:merge-conflict` and
`:merge-failed` already bucket to `MERGED`. So `MERGE QUEUE` covers the same set
`MERGING` covered, with strictly more information, and no workspace is ever
rendered twice.

**Exception worth taking:** `:merge-conflict` workspaces should *move* into
`MERGE QUEUE` too. A conflict is a merge that is stuck, not a merge that is done —
burying it in `MERGED` is the current behavior's worst wart. `:merge-failed` and
`:merged` stay in `MERGED`.

So the section's membership predicate is:

```
render-status ∈ { :merging, :merge-queued, :merge-conflict }
```

### 2.3 Visibility when empty

Omit the section entirely when empty, following `HIDDEN`'s precedent
(`drawer.el:1032-1035`). Drawer width is a fixed 20% of the frame
(`drawer.el:28-41`) and vertical space is the scarce resource; a permanent
`MERGE QUEUE (0) / (none)` block costs three lines forever to communicate nothing.

---

## 3. Visual specification

### 3.1 Grouping

The queue is bucketed by `:target-dir` (`worktree.el:4770`), and buckets drain
concurrently. That maps exactly onto the existing repo-group label mechanism
(`--render-trees`, `drawer.el:962-982`; face `agent-repl-drawer-group-label`,
`drawer.el:185-191`). Reuse it, with the label extended to name the target branch:

```
 ▸ doom → master
```

Label text = `<repo-basename> → <target-branch>`, where the branch comes from the
already-cached `:merge-target-name` (`worktree.el:5193`).

### 3.2 Row anatomy

```
 MERGE QUEUE (5)
 ────────────
 ▸ doom → master
   ⟳ drawer-nil-guard              0:12
     cherry-pick  3/7  ▰▰▰▱▱▱▱
     a1b2c3d fix(drawer): guard nil ws
   💥 vterm-freeze-repro           1:47  ⚠
     conflict at 9f8e7d6 · 2 files unmerged
     resolver: verifying           0:31
   ⏸ merge-queue-design            #1
   ⏸ codex-backend-swap            #2  ⛔ halted
 ▸ services → master
   ⟳ ceac-timeout-fix              0:03
     computing base
```

Line-by-line:

| Line | Content | Shown when |
|---|---|---|
| **header** | glyph + ws name + elapsed-or-position + flags | always |
| **progress** | phase name + `M/N` + bar | in-flight |
| **detail** | short SHA + commit subject | in-flight, picking |
| **conflict** | conflicting SHA + unmerged file count | conflict |
| **resolver** | resolver sub-phase + its own elapsed | conflict, auto-resolving |

Queued rows are a single line: glyph, name, `#N` position, optional halt flag.
Only the *front* entry of each bucket is a candidate to start, so `#1` is
meaningful.

The header keeps the existing 2-line block's `wrap-prefix` conventions
(`drawer.el:863-870`) so soft-wrap still aligns. Sub-lines are indented one level
past the name.

### 3.3 Glyphs

Extend `agent-repl-drawer-state-icons` (`drawer.el:56-94`):

| Glyph | Meaning |
|---|---|
| `⟳` | in-flight (animated: `⟳ ⟲` alternating, see §6.2) |
| `⏸` | queued |
| `⛔` | queued + `:halt-until-human` |
| `💥` | conflict (existing) |

### 3.4 Progress bar

`▰`/`▱`, width 7, derived from `commit-index / commit-total`. It is a plain string,
so it costs nothing and survives the temp-buffer/string-compare render path
(`drawer.el:1131-1135`).

### 3.5 New faces

Follow the `agent-repl-drawer-detail-*` naming already in place
(`drawer.el:198-245`):

| Face | Style |
|---|---|
| `agent-repl-drawer-merge-phase` | cyan |
| `agent-repl-drawer-merge-progress` | spring green, bold |
| `agent-repl-drawer-merge-elapsed` | shadow |
| `agent-repl-drawer-merge-commit` | medium orchid |
| `agent-repl-drawer-merge-halted` | tomato, bold |

---

## 4. The observability gap

This is the crux. Almost none of the above is currently readable from outside the
merge worker thread.

### 4.1 What exists today

| Datum | Where |
|---|---|
| Queue membership + FIFO order | `agent-repl--merge-queue` (`worktree.el:4576`) |
| Per-bucket target dir | `:target-dir` on each entry |
| `:halt-until-human` | on re-enqueued entries (`worktree.el:1979`) |
| In-flight set | `agent-repl--in-flight-merges` (`worktree.el:4690`) |
| **Merge start time** | `:started-at` (`worktree.el:4731`) — the one free win |
| Target branch name | `:merge-target-name` on the ws plist (`worktree.el:5193`) |

### 4.2 What does not exist

| Datum | Currently |
|---|---|
| Total commits in range | local `range-count`, computed then discarded (`worktree.el:3324`) |
| **Current commit index** | *does not exist* — the pick is one range invocation (`worktree.el:3335`) |
| Current commit SHA / subject | never known to elisp |
| Current git subcommand | not tracked; `--git-exit-code` returns only an int (`worktree.el:174`) |
| git stdout/stderr | **discarded** — `start-process` with a nil buffer (`worktree.el:196`) |
| Conflicted file list | local `files` (`worktree.el:4007`) |
| Resolver sub-phase | implicit in control flow (`worktree.el:3988-4048`) |
| Conflict-loop iteration | local `cpc-iter` (`worktree.el:3343`) |

---

## 5. Proposed instrumentation

### 5.1 A merge-progress record

Do **not** scatter these onto the workspace plist. Merge progress is high-churn,
worker-thread-written, and ephemeral; the ws plist is snapshot-persisted on every
mutation (`worktree.el:4677`) and we do not want to serialize a progress bar.

New in `worktree.el`:

```elisp
(defvar agent-repl--merge-progress (make-hash-table :test 'equal)
  "ws-name → progress plist for merges currently in flight.")

(defvar agent-repl--merge-progress-seq 0
  "Monotonic counter bumped on every progress write.")
```

Progress plist:

| Key | Value |
|---|---|
| `:phase` | closed set (§5.2) |
| `:commit-index` | integer, commits picked so far |
| `:commit-total` | integer, `range-count` |
| `:commit-sha` | short SHA currently being applied |
| `:commit-subject` | its `%s` |
| `:conflict-files` | list of paths (`--diff-filter=U`) |
| `:resolver-phase` | `spawned` / `waiting` / `verifying` / `continuing` |
| `:resolver-started-at` | float-time |
| `:git-last-line` | last stderr line from the running git process |

Entries are cleared alongside `--clear-in-flight-merge` (`worktree.el:4738`), so the
hash never outlives the in-flight set.

### 5.2 Phase enum

Mirrors the actual code path in `--workspace-merge-do` (`worktree.el:4155`):

```
resolving-target → checking-clean → computing-base → picking
   → [conflict → resolving → verifying → continuing → picking]*
   → tagging → finalizing
```

Roughly ten `agent-repl--merge-progress-put` calls at the anchors already
enumerated in §4.2. These all run on the worker thread; that is safe, because the
progress hash is plain Lisp and only *UI* operations need
`--defer-to-main-thread` (`worktree.el:1833`).

### 5.3 Per-commit progress requires unrolling the range pick

`worktree.el:3335` is a single invocation:

```elisp
(agent-repl--git-exit-code root "cherry-pick" "-x" range)   ; range = "BASE..BRANCH"
```

Git applies the commits one at a time internally, but **elisp never sees the
boundaries**, so `3/7` is unobtainable without a change here. Two options:

**Option A — poll the target's HEAD (no merge-path change).**
On each drawer tick, for each in-flight merge, run
`git rev-list --count <base>..HEAD` in the target and derive the index.
- Pro: zero risk to the merge path.
- Con: a git subprocess per tick per in-flight merge, on the *main* thread. That is
  exactly the kind of synchronous git the drawer already confines to explicit
  `TAB`/`g` actions (`--refresh-detail-cache`, `drawer.el:1605-1646`). Also yields
  no SHA/subject and no `:git-last-line`.

**Option B — replace the range pick with a per-SHA loop (recommended).**

```
rev-list BASE..BRANCH  →  for each SHA (oldest first):
    progress-put :commit-sha/:commit-subject/:commit-index
    git cherry-pick -x <SHA>
    if CHERRY_PICK_HEAD → existing conflict/auto-resolve loop
```

- Semantically equivalent: `git cherry-pick -x A..B` *is* a per-commit apply that
  halts at the first conflict, and `-x` annotates each commit identically either way.
- The existing conflict loop (`worktree.el:3344-3366`) already handles "a subsequent
  commit in the range conflicts again", so re-entering it per commit is not new logic.
- One difference to handle deliberately: after a resolved conflict, `cherry-pick
  --continue` in the range case auto-proceeds through the remainder. In the unrolled
  loop, `--continue` finishes only the current commit and *we* drive the rest. That is
  strictly more observable and arguably more controllable.
- Cost: one extra `git` process per commit. Negligible against a `claude -p` resolver.
- The `unwind-protect` abort (`worktree.el:3384-3389`) is unchanged and still
  unconditional.

**Recommend B.** The entire point of the ask is git-action-level resolution, and A
cannot deliver the SHA, the subject, or the live git output.

### 5.4 Capturing git output

Add `agent-repl--git-exit-code-capturing`, a sibling of `--git-exit-code`
(`worktree.el:174`) that attaches a **process filter** retaining the last line of
stderr into `:git-last-line`. Used by the per-commit pick only.

This is a strict addition. `--git-exit-code` keeps its current signature and every
existing caller and error path is untouched — no error-handling coverage is removed
or weakened anywhere in this design.

---

## 6. Render integration

### 6.1 The render signature must sample progress

`agent-repl-drawer--render-signature` (`drawer.el:1050-1071`) is the load-bearing
short-circuit: when the signature is unchanged, the 1Hz poll skips the render
entirely (`drawer.el:1116`). It currently samples neither `:merging` nor
`:merge-completed`, and knows nothing of the queue lists.

Rather than enumerate every new field, append the two cheap globals:

```elisp
(list (agent-repl-drawer--current-ws)
      ws-sig
      agent-repl--merge-progress-seq        ; any progress write invalidates
      (length agent-repl--merge-queue)      ; enqueue/dequeue invalidates
      (length agent-repl--in-flight-merges))
```

The `seq` counter means a single `progress-put` forces exactly one redraw, with no
risk of a field being added later and silently failing to render — the failure mode
the current signature already has for `:merging`.

### 6.2 Cadence: a merge-only fast timer

The global poll is 1Hz (`status.el:67-70`, timer at `status.el:1748`). That is fine
for phase and commit-index changes, but too coarse for a spinner and a live clock,
and raising the global rate would multiply the per-workspace state work
(`status.el:1610-1643`) for no reason.

Add a dedicated timer, period `agent-repl-merge-progress-tick` (default 0.25s), that:

- runs **only** while `agent-repl--in-flight-merges` is non-empty,
- calls `agent-repl-drawer--refresh-if-visible` and nothing else,
- self-cancels when the in-flight set empties.

The spinner frame and the elapsed clock derive from `(float-time)` at render, so
they need no state.

### 6.3 Rendering is a pure function

`--insert-merge-queue-section` takes `(queue, in-flight, progress-hash, now)` and
returns text. No git, no I/O, no globals read at render time beyond those three.
This is what makes §8 tractable.

---

## 7. Row actions

Rows carry the standard `agent-repl-drawer-workspace` property, so `RET`, marks,
and navigation work with zero new keymap wiring (`drawer.el:255-285`). Three
overrides make sense inside this section:

| Key | Queued row | In-flight row | Conflict row |
|---|---|---|---|
| `RET` | visit workspace (existing) | `magit-status` on target dir | pop `*agent-repl-merge-resolver-<ws>*` (`worktree.el:3621`) |
| `d` | `agent-repl--dequeue-merge` (`worktree.el:4646`) | — | — |
| `D` | — | — | `agent-repl-drain-merge-queue` (`commands.el:2751`), clearing `:halt-until-human` |

The resolver buffer already exists and is already kept alive
(`:keep-buffer`, `worktree.el:3901`) — today nothing points the user at it. `RET` on
a conflict row is the single highest-value action in this design and is free.

---

## 8. Testing plan

Per `CLAUDE.md`: one test file per source module, one edge case per test.

**`test-drawer.el`** — the render is pure, so every case is a synthetic
`(queue, in-flight, progress)` triple asserted against buffer text:

- empty queue + empty in-flight → section omitted entirely
- one queued entry → `#1`, `⏸`, no progress line
- queued + `:halt-until-human` → `⛔ halted`
- in-flight, `:phase picking`, 3/7 → bar `▰▰▰▱▱▱▱`, SHA + subject line
- in-flight, `:phase computing-base` → phase line, **no** bar (no totals yet)
- conflict + resolver `verifying` → conflict line + resolver line with its own clock
- two target dirs → two group labels, correct bucketing
- a `:merging` workspace appears in `MERGE QUEUE` and **not** in `MAIN`/`MERGED`
- `--render-signature` changes when `--merge-progress-seq` is bumped

**`test-worktree.el`**:

- per-SHA loop picks commits oldest-first and writes `:commit-index` monotonically
- `:commit-total` equals `rev-list --count` of the range
- a conflict on commit 2 of 4 leaves `:phase conflict` and `:commit-index 1`
- progress entry is removed by `--clear-in-flight-merge`
- `--git-exit-code-capturing` returns the same exit code as `--git-exit-code`
- the `unwind-protect` abort still runs on the unrolled loop's error path

---

## 9. Open questions

1. **Should `:merge-conflict` move out of `MERGED` into `MERGE QUEUE`?**
   Recommended (§2.2), but it changes where users currently look for a stuck merge.

2. **Option A (poll HEAD) or Option B (unroll the range pick)?**
   Recommended B (§5.3). B touches the merge hot path; A cannot deliver SHA,
   subject, or live git output. This is the one decision that materially changes the
   implementation's blast radius.

3. **Should a completed merge linger for a few seconds with a `✓` before vanishing?**
   Otherwise a fast merge flashes and disappears, and the user learns nothing.

4. **Halted entries: surface a persistent banner?**
   A `⛔` on row `#2` of a collapsed bucket is easy to miss, and a halted bucket
   blocks every merge behind it (`worktree.el:4832`).
