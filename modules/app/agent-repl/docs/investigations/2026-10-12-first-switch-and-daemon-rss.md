# First switch to a workspace, and the daemon's memory (2026-10-12)

Read-only diagnosis on the owner's live runtime: logs under `~/.claude-emacs/logs`, `footprint`/`vmmap` on the live `agent-repld`, and a `.backup` copy of `wsm.db` decoded offline. Nothing live was restarted or sent a request.

## 1. Switching to a workspace

### What "never shown" is today

- Emacs already PRELOADS a hidden page for every opened workspace: 27 pages logged in the last hour, each booting `hidden: true` (e.g. `main.boot` to `feed.conversation-drawn` in 78ms while hidden).
- So the slow case the owner sees is the FIRST SHOW of an already-loaded hidden page (`elisp.panels.webview-on-switch ... view=first`), not a page load. Only one switch today created its page (switch to `main.boot` 589ms, to `feed.conversation-drawn` 3,041ms).

### Breakdown (all switches since 2026-10-12 00:00, record pairs from the logs)

| Switch kind | n | request to `webview-on-switch` (Emacs) | `webview-on-switch` to `page.visibility` visible | visible to `page.repaint` | request to painted |
|---|---|---|---|---|---|
| `view=first` (first show of a hidden page) | 46-61 | p50 90ms, p90 150ms | p50 453ms, p90 1,320ms, max 2,356ms | p50 58ms | p50 679ms, p90 1,477ms, max 2,619ms |
| `view=new` | 51-59 | p50 176ms, p90 345ms | p50 ~0, p90 133ms | p50 50ms | p50 215ms, p90 578ms |
| `view=same` | 210 | p50 81ms, p90 112ms | (rarely re-shown) | p50 55ms | |

Pairs: `agent-repl.elisp-host-switch-requested-*` and `agent-repl.elisp-panels-webview-on-switch-*` (Emacs log), then `page.visibility` "the page became visible" and `page.repaint` "the page painted Nms after visible" (that workspace's webapp log).

### What blocks the first show (confirmed by record order)

- Between the show and `page.visibility`, the hidden page DRAINS A BACKLOG of daemon pushes it received while hidden, one record about every 27ms (median), before `sidebar.workspace-selected` and `page.visibility` fire.
- Slowest case (ws `0432f1a45cba4b7f`, 13:01:42): 2,356ms, during which the page drew 38 `sidebar.drawn-status` transitions (rows stepping none, init, ready, submitting, thinking, done, merging), 12 `rpc.stream-frame-forward-skew` skips and a footer change, then became visible.
- Across `view=first` shows: drained records p50 23, max 153; correlation with the lag 0.33 (the drain is a large part, not all of it).
- INFERRED (no record shows it): WebKit throttles a hidden page's JS, so pushes queue; the show resumes it and every queued push is drawn in turn at about frame rate, intermediate states included, before the visibility change is delivered.

### What "load only the most recent page" would save

- The initial feed page is already bounded by row count: `feed.page-placed` replace pages p50 34 rows, p90 50, max 491 (2,036 placements).
- What is NOT bounded is row SIZE: a merge bubble's `merge_tab/tests` row carries every suite's output inline (`FeedMergeTestSuite.output`); 219 such rows hold 300MB, average 1.4MB, max 9.6MB. A page whose tail holds merge bubbles ships megabytes. Page byte size is not logged (gap below), so the saving is not measured; bounding the tests row (below) bounds it.

### Fix candidates (first switch)

1. Do not replay intermediate states in a hidden page: coalesce pushes per row while `document.hidden` and draw only the latest state on show (or have the daemon pause streams to hidden pages and resync with one snapshot on visible). Expected: the 453ms p50 / 1.3s p90 show-to-visible drain drops toward the 50-60ms paint; also removes most hidden-page CPU (below).
2. Emacs pre-swap work: 80-180ms of elisp between the switch request and `webview-on-switch` on EVERY switch; in the slowest case ~118ms of it falls between `maybe-autoselect-input` and `switch-activation-won` with no record in between (gap below).
3. Bound the merge tests row (section 2, fix 1), which bounds feed page bytes.

### Preload design (owner direction) and its memory

- Preloading is already total, and that is the battery problem: 32 WebKit processes, 2.85GB resident, 28.5% CPU in one `ps` snapshot; ~89MB per page process. Every hidden page draws every workspace's roster transitions (1,995 `sidebar.drawn-status` records across pages in one hour).
- Proposed: keep a small LRU of live pages (current plus the likely-next 2-3: previous workspace, cycle neighbors, workspaces with an active turn), park the rest (no stream, or no page), and warm the next candidate one at a time from an idle timer after a switch settles, never more than one page booting at once. Cost: ~90MB per warm page instead of ~90MB times every workspace.

## 2. The daemon's memory

### Attribution (live pid 12880, up 59 minutes)

| Region | Dirty | Note |
|---|---|---|
| untagged VM_ALLOCATE (the Go heap) | 547MB (537.6MB of it swapped/compressed) | `footprint`: 551MB total, peak 682MB |
| `__TEXT` | 14MB clean | the binary, shared/clean |
| stacks, malloc, dyld | < 3MB | |

- `ps` RSS reads 43MB only because the heap is compressed/swapped; the footprint is the real cost.

### The retainer (confirmed offline, inferred for the live heap)

- `wsm.db` is 332MB, of which `durable_feed_rows` is ~327MB: 1,001 rows, and 300MB of it is 219 `merge_tab/tests` rows. It grew ~40MB during this investigation; the layout backups beside it hold another ~1GB.
- Decoding every durable row as `restoreDurable` does retains 341MB of Go heap (scratch probe over the copy: encoded 326.7MB, retained 341.0MB).
- The live daemon restored 27 workspaces since boot (`daemon.feed.durable` "drew this workspace's durable rows again" x27); those workspaces' rows are 155MB encoded, so ~160MB live. With Go's default GOGC=100 headroom and multi-MB transient marshals, that is consistent with the 551MB footprint and 682MB peak. Exact split needs a heap profile of the live process (gap below).

### Why the rows are huge and why they cost CPU and writes

- Every counted test line in a merge's gate run (`gate.go` `edge`) upserts the whole tests tab; a settled tab carries every suite's output spans.
- Each upsert re-marshals the row for durability (`recordDurable`, `proto.Marshal`), rewrites the blob in SQLite, and pushes it to every reader of that feed.
- Rows are never retired: one per merge round, kept forever.

### Fix candidates (memory, CPU, disk)

1. Keep suite output out of the feed row: store the test log once as an archive (the gate already writes `ArchivePath`) and carry only counts plus a tail and a link in the row. Expected: tests rows from ~1.4MB to a few KB; `durable_feed_rows` from ~327MB to a few MB; daemon live heap down ~150MB+; feed page bytes bounded.
2. Retire old merge bubbles' durable rows (settled merges older than N, or beyond the last K per workspace).
3. Lazy restore: draw a workspace's durable rows only when a reader opens that feed, not when its feed state is born.
4. Set a memory limit (`debug.SetMemoryLimit`) or lower GOGC for the daemon once the live set is small, to cap the headroom.
5. Expose runtime memory stats (heap in-use, released, GC count) in daemon health at INFO so this is measurable without a restart.

## Observability gaps

- No record of a hidden page resuming, of its queued-push depth at resume, or of per-push draw cost (the drain mechanism is inferred).
- No record between `maybe-autoselect-input` and `switch-activation-won` in Emacs (~118ms unexplained in the slowest case).
- Feed page bytes and per-row bytes are not logged at INFO (`feed.page-placed` gives rows only).
- The daemon exposes no Go memory stats; attributing the live heap needed an offline reproduction.
- Logs directory is 5.8GB with no visible retention policy evident in this sample.
