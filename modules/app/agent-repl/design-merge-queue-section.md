# Design: MERGE QUEUE drawer section

Status: **design only — not implemented.**
Scope: `modules/app/agent-repl/` (`drawer.el`, `worktree.el`, `status.el`).

---

## 1. Motivation

The drawer today answers *which workspaces are merging* and nothing else. It cannot
answer *what git is doing right now*. During a real merge the user stares at a name
with a `⇄` next to it for a minute or more — commit hooks can make a single
cherry-pick take a long time — with no way to tell whether progress is being made,
which commit is stuck, or what is behind it in line.

**Goal:** a top-of-drawer section that renders the merge queue at the level of
*individual commits*: the commit git is applying right now, how long it has been
applying it, and the next few commits behind it.

---

## 2. Two sections, two ontologies

`MERGE QUEUE` is **in addition to** `MERGING`. They are not alternatives and they do
not overlap in what they say.

| Section | Unit of a row | Answers |
|---|---|---|
| `MERGING` (existing, unchanged) | **workspace** | *Which workspaces are queued or merging?* |
| `MERGE QUEUE` (new, top) | **commit** | *Which commit is being cherry-picked, for how long, and what is next?* |

Because `MERGE QUEUE` rows are commits, they carry a new
`agent-repl-drawer-commit` text property rather than
`agent-repl-drawer-workspace`. Nothing in the drawer's row-identity machinery
(`--goto-workspace-line`, `drawer.el:1154-1166`; `--entry-bounds-at-point`,
`drawer.el:300-318`; the marked/expanded sets, `drawer.el:1303-1306`) sees a
workspace twice, so the duplicate-row hazard that would exist between two
*workspace* sections simply does not arise. Workspace navigation (`j`/`k`) skips
commit rows exactly the way it already skips headers and rules.

### 2.1 Section order

```
MERGE QUEUE   (new, top — omitted entirely when the queue is idle)
MAIN
HIDDEN        (omitted when empty)
MERGING       (unchanged)
MERGED        (unchanged)
```

`MERGE QUEUE` is omitted, not stubbed with `(none)`, when nothing is in flight and
nothing is queued. Drawer width is a fixed 20% of the frame (`drawer.el:28-41`) and
vertical space is the scarce resource.

---

## 3. What the section renders

### 3.1 Content

- The commit currently being cherry-picked, with an **elapsed clock shown only once
  it exceeds a threshold** (`agent-repl-drawer-merge-slow-commit-threshold`,
  default **3.0s**). Below the threshold the clock is absent, so a fast queue stays
  quiet and a slow commit announces itself.
- The **next three commits** behind it.
- **Project separators interleaved into that stream**, emitted at every project
  boundary — not as a fixed outer grouping.

### 3.2 The interleaving rule (the user's example, verbatim)

Current pick is project A, the next commit is also project A, and the two after that
are project B:

```
 MERGE QUEUE
 ───────────
 ▸ doom
   ⟳ a1b2c3d  fix(drawer): guard nil ws          0:07
     9f8e7d6  feat(drawer): merge-queue section
 ▸ services
     3c4d5e6  fix(ceac): debounce the state poll
     7a8b9c0  test(ceac): cover the debounce
```

A separator is emitted when, and only when, the project of commit *N* differs from
the project of commit *N-1*. A run of commits in one project gets one header.

The same fast queue with a sub-threshold current commit and only one commit behind it:

```
 MERGE QUEUE
 ───────────
 ▸ doom
   ⟳ a1b2c3d  fix(drawer): guard nil ws
     9f8e7d6  feat(drawer): merge-queue section
```

Separator label reuses the existing repo-group mechanism
(`--group-label`, `drawer.el:933-940`; face `agent-repl-drawer-group-label`,
`drawer.el:185-191`), so it is visually identical to the repo headers the user
already reads elsewhere in the drawer.

### 3.3 Row anatomy

| Column | Current commit | Upcoming commit |
|---|---|---|
| glyph | `⟳` (spinner, animated) | none |
| sha | short, `agent-repl-drawer-merge-commit` | short, dimmed |
| subject | truncated to fit | truncated, dimmed |
| clock | `M:SS`, **only when > 3.0s** | none |

Conflict is a state of the *current* commit, so it renders in place rather than as
its own section:

```
 ▸ doom
   💥 a1b2c3d  fix(drawer): guard nil ws          1:42
      2 files unmerged · resolver: verifying      0:31
```

---

## 4. The commit stream (core model)

Everything in §3 is a rendering of one ordered list. Building that list is the
whole job.

```
stream := [ remaining commits of each in-flight pick ]
       ++ [ commits of each queued entry, in bucket FIFO order ]
```

Then: take the current commit(s) plus the next three, and walk the result emitting a
separator whenever `project` changes.

Each stream element:

| Field | Source |
|---|---|
| `:sha` / `:subject` | in-flight: `.git/sequencer/todo` (§5.2). queued: `rev-list` (§5.4) |
| `:project` | the entry's `:target-dir` bucket (`worktree.el:4770`) → repo label |
| `:source-ws` | the queue entry's `:source-ws` |
| `:state` | `current` / `pending` / `conflict` |
| `:started-at` | set only for `current`, feeds the >3s clock |

### 4.1 Concurrency

Buckets are per-`:target-dir` and drain **concurrently** (`worktree.el:4549-4574`),
so in the general case there is more than one current commit — one per active
project. The rule that keeps this consistent with §3.2:

- **Every in-flight project's current commit is always shown**, regardless of budget.
- The **lookahead budget of 3 is global**, consumed in project order (active projects
  first, then queued buckets in `--merge-queue-target-dirs` first-appearance order,
  `worktree.el:4770`).

In the single-merge case this degenerates to exactly the user's example.

---

## 5. Instrumentation: what has to be built

### 5.1 There is no blocker — git already streams the boundaries

An earlier draft of this design claimed the range cherry-pick made per-commit
progress unobtainable, and concluded the pick had to be unrolled into a per-SHA
loop. **That claim was wrong, and it was checked empirically rather than assumed.**

The current invocation (`worktree.el:3335`) is:

```elisp
(agent-repl--git-exit-code root "cherry-pick" "-x" range)   ; range = "BASE..BRANCH"
```

Probing real `git` (2.45.1) with a live pipe reader — exactly what an Emacs process
filter sees — shows git emits a per-commit line and **flushes it incrementally**:

```
  t(s)  | line arriving on the pipe
--------+--------------------------------------------------
   0.08 | [master ba94789] feat: commit number 1
   0.15 | [master a398aa4] feat: commit number 2
   0.22 | [master 8abdfaa] feat: commit number 3
   0.22 | <process exit 0>
```

The boundaries are staggered and arrive **before** process exit. On conflict, git
additionally emits, live:

```
stdout:  Auto-merging f.txt
stdout:  CONFLICT (content): Merge conflict in f.txt
stderr:  error: could not apply dec4a97... feat: one
```

So the conflicting SHA, its subject, and the conflicted file list are all in the
stream too.

**Elisp does not see any of this today for one reason only: we throw it away.**
`--git-exit-code--worker` calls `start-process` with a **nil** output destination
(`worktree.el:196-197`) and keeps only the integer exit code. The information was
always there.

**Therefore: do not unroll the pick.** Attach a **process filter** to the existing
range invocation. This is strictly additive — git's semantics, the commit sequence,
the `-x` annotations, the conflict loop, and the unconditional `unwind-protect`
abort (`worktree.el:3384-3389`) are all untouched, and no error path anywhere is
removed or weakened. Unrolling would have rewritten the merge hot path to obtain
something git was already handing us.

New in `worktree.el`, a sibling of `--git-exit-code` that leaves the original and
all its callers alone:

```elisp
(defun agent-repl--git-exit-code-streaming (root filter &rest args) ...)
```

The filter parses three patterns and does nothing else:

| Pattern | Effect |
|---|---|
| `^\[.+ \([0-9a-f]+\)\] \(.*\)$` | commit applied → advance `:commit-index`, reset `:commit-started-at` |
| `^error: could not apply \([0-9a-f]+\)\.\.\. \(.*\)$` | → `:conflict-sha`, `:conflict-subject` |
| `^CONFLICT (.*): .* in \(.*\)$` | → push onto `:conflict-files` |

**Thread note.** Process filters run on whichever thread pumps the event loop, not
on the merge worker. That is fine and in fact desirable: the filter only mutates the
progress hash (plain Lisp) and bumps a counter. It must never touch UI — the
existing `--defer-to-main-thread` discipline (`worktree.el:1833`) is unchanged.

### 5.2 The in-flight lookahead is free — `.git/sequencer/todo`

For a multi-commit pick, git maintains `.git/sequencer/todo`, which is exactly the
list of remaining picks, already carrying SHA and subject:

```
pick dec4a97 feat: one          <- current (still listed while it is being applied)
pick 4078f49 feat: two
pick 437e2f3 feat: three
```

This is a **plain file on disk**. Reading it costs no subprocess, no `rev-list`, and
no worker round-trip — it *is* the "next three commits" for the in-flight pick,
verbatim.

Two caveats, both benign:

- A single-commit pick does not use the sequencer, so `.git/sequencer/` is absent.
  Fall back to the filter's own `:commits` knowledge (there is only one).
- The directory is removed when the pick finishes, which is precisely the signal that
  the in-flight portion of the stream is empty.

### 5.3 Progress record

Progress is high-churn, worker-thread-written, and ephemeral. It must **not** go on
the workspace plist, which is snapshot-persisted on every mutation
(`worktree.el:4677`) — we are not serializing a spinner.

```elisp
(defvar agent-repl--merge-progress (make-hash-table :test 'equal)
  "ws-name → progress plist, for merges currently in flight.")

(defvar agent-repl--merge-progress-seq 0
  "Monotonic counter bumped on every progress write.")
```

Every field below is written **by the process filter of §5.1**, from git's own output.
Nothing is inferred and nothing needs a probe subprocess.

| Key | Written by | Meaning |
|---|---|---|
| `:commit-index` | filter, on each `[branch sha]` line | how many commits have landed |
| `:commit-started-at` | filter, reset on each boundary | float-time — **this is the >3s clock** |
| `:conflict-sha` / `:conflict-subject` | filter, on `error: could not apply` | the commit that is stuck |
| `:conflict-files` | filter, on each `CONFLICT (...)` line | conflicted paths |
| `:resolver-phase` | merge worker | `spawned` / `waiting` / `verifying` / `continuing` |
| `:resolver-started-at` | merge worker | float-time |

The **current commit** is `.git/sequencer/todo`'s head (§5.2); `:commit-index` orders
the stream and drives the clock reset. The two agree, and the todo file is
authoritative for identity.

Cleared alongside `--clear-in-flight-merge` (`worktree.el:4738`), so it never
outlives the in-flight set.

### 5.4 Lookahead for queued entries

The commits of a *queued* entry are not known either, and computing them means
running git. That must never happen on the main thread at poll cadence — the drawer
already confines synchronous git to explicit `TAB`/`g` actions
(`--refresh-detail-cache`, `drawer.el:1605-1646`).

```elisp
(defvar agent-repl--merge-lookahead (make-hash-table :test 'equal)
  "ws-name → (:target-head SHA :commits ((sha . subject) ...)).")
```

- Computed **on a worker thread**, one `git rev-list --reverse <target-branch>..<ws-branch>`
  per entry.
- Computed **only for as many front-of-bucket entries as the 3-commit budget needs** —
  never for the whole queue.
- Keyed by the target's HEAD SHA. When the target advances (i.e. a merge lands), the
  entry is stale and recomputed.
- It is an **estimate**: the real base is resolved by `--cherry-pick-base`
  (`worktree.el:3223`) against the target's HEAD at the moment the pick actually
  starts, which will have moved. Good enough for a lookahead; the current row is
  always exact.

---

## 6. Render integration

### 6.1 The render signature must sample this

`agent-repl-drawer--render-signature` (`drawer.el:1050-1071`) is the load-bearing
short-circuit: an unchanged signature skips the render entirely (`drawer.el:1116`).
It currently samples neither `:merging` nor the queue lists, so this is a
prerequisite, not polish.

```elisp
(list (agent-repl-drawer--current-ws)
      ws-sig
      agent-repl--merge-progress-seq             ; any progress write invalidates
      (length agent-repl--merge-queue)
      (length agent-repl--in-flight-merges)
      (when agent-repl--in-flight-merges         ; make the clock tick
        (floor (float-time))))
```

The `seq` counter means one `progress-put` forces exactly one redraw, and a field
added later cannot silently fail to render — which is the bug the current signature
already has for `:merging`.

### 6.2 Cadence

The global poll is 1Hz (`status.el:67-70`, timer at `status.el:1748`). That is
enough for the clock (rendered at `M:SS`) but not for a spinner. Add a dedicated
timer, `agent-repl-merge-progress-tick` (default 0.5s), that runs **only** while
`agent-repl--in-flight-merges` is non-empty, calls
`agent-repl-drawer--refresh-if-visible` and nothing else, and self-cancels when the
in-flight set empties. Raising the *global* rate instead would multiply the
per-workspace state work (`status.el:1610-1643`) for no reason.

### 6.3 The render is pure

`--insert-merge-queue-section` takes `(stream, now)` and returns text. No git, no
I/O. Stream construction takes `(queue, in-flight, progress-hash, lookahead-hash,
todo-lines)` and is likewise pure — the one impure step, reading
`.git/sequencer/todo`, is a single `insert-file-contents` hoisted to the caller. This
is what makes §8 tractable.

---

## 7. Actions on commit rows

Commit rows are informational, but two actions are nearly free and high-value:

| Key | Current commit | Upcoming commit |
|---|---|---|
| `RET` | `magit-show-commit` the SHA in the target repo | same |
| `RET` (conflict) | pop `*agent-repl-merge-resolver-<ws>*` (`worktree.el:3621`) | — |

The resolver buffer already exists and is already kept alive (`:keep-buffer`,
`worktree.el:3901`). Today nothing points the user at it.

---

## 8. Testing plan

Per `CLAUDE.md`: one test file per source module, one edge case per test.

**`test-drawer.el`** — the stream builder and the renderer are both pure, so every
case is a synthetic input asserted against output:

- idle queue → section omitted entirely
- current commit at 1.2s elapsed → **no** clock rendered
- current commit at 4.5s elapsed → clock rendered as `0:04`
- current + 1 same-project + 2 other-project → **exactly the §3.2 layout**
- a run of 4 commits in one project → exactly one separator
- fewer than 3 commits behind the current → renders what exists, no padding
- more than 3 behind → truncated at 3
- two concurrent in-flight projects → both current commits shown, lookahead budget split
- conflict on the current commit → conflict line + resolver line
- commit rows carry `agent-repl-drawer-commit`, never `agent-repl-drawer-workspace`
- `j`/`k` navigation skips commit rows
- a `:merging` workspace still renders in `MERGING`, unchanged
- `--render-signature` changes when `--merge-progress-seq` is bumped
- `--render-signature` ticks once per second while in-flight, and does not while idle

**`test-worktree.el`** — the filter is a pure string→plist transform, so it tests
without git at all:

- `[master ba94789] feat: one` → `:commit-index` advances, `:commit-started-at` resets
- `error: could not apply dec4a97... feat: one` → `:conflict-sha` / `:conflict-subject`
- `CONFLICT (content): Merge conflict in f.txt` → pushed onto `:conflict-files`
- a line split across two filter calls (git may deliver a partial line) is buffered,
  not dropped — **the classic process-filter bug, and the one that will bite**
- an unrecognized line is ignored rather than corrupting state
- `--git-exit-code-streaming` returns the same exit code as `--git-exit-code`
- the progress entry is removed by `--clear-in-flight-merge`
- `.git/sequencer/todo` parses to `((sha . subject) ...)`
- a **missing** `.git/sequencer/` (single-commit pick) degrades to the filter's own state
- lookahead is keyed by target HEAD and recomputed when the target advances
- lookahead is computed only for the entries the 3-commit budget reaches

---

## 9. Empirically established (git 2.45.1)

Everything in §5.1–5.2 was verified against real git, not assumed. Recorded here
because the first draft of this design got it wrong in the expensive direction.

1. **The range cherry-pick streams per-commit lines and flushes them incrementally.**
   Boundaries arrive staggered, before process exit. A process filter sees them live.
2. **Conflicts stream too**: the stuck SHA and subject on stderr, the conflicted files
   on stdout.
3. **`.git/sequencer/todo` exists during a multi-commit pick** and lists the remaining
   picks with SHA and subject. Free lookahead, no subprocess.
4. **`cherry-pick` runs `post-commit` but NOT `pre-commit`.**
   Worth flagging against the stated premise that slow cherry-picks are caused by
   "commit hooks": if the slow hook is a `pre-commit` hook, it does **not** run during
   a cherry-pick, and the slowness is coming from somewhere else. Note also that
   `~/.gitconfig` sets `core.hooksPath` to `~/.config/git/hooks` globally, so the
   hooks that do run are the ones there, not any repo's `.git/hooks`.

---

## 10. Open questions

1. **Should an upcoming commit show which workspace it comes from?**
   Within one project, consecutive queue entries are different workspaces, and the
   section would not distinguish them. A dimmed trailing `ws-name` is possible but
   the drawer is only 20% of the frame wide.

2. **Is the 3-commit lookahead a global budget or per-project?**
   §4.1 proposes global, which reproduces the user's example exactly. Per-project
   would show more during concurrent merges but grows unboundedly with project count.

3. **Should the section linger for a beat after the queue empties?**
   Otherwise the final commit of a fast merge vanishes the instant it lands and the
   user never sees it complete.

4. **`⛔ halted` entries** (`:halt-until-human`, `worktree.el:1979`) block their whole
   bucket (`worktree.el:4832`). Should their commits appear in the stream (greyed,
   flagged) or be omitted, given they are not actually "up next"?
