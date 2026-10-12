# Workspace switch latency: plan

Owner targets (2026-10-12): an Emacs tab switch shows no perceptible lag (low
tens of milliseconds); the switched-to page is on screen within the low hundreds
of milliseconds; every workspace's page is preloaded when its Emacs tab is
created, by any path, asynchronously and starting immediately.

## Measured baseline (2026-10-12, real log edges and a profile session)

- Emacs: request to webview placed 90-160ms (up to 390ms under load): daemon
  round trip 45-50ms (select 2ms, full 153-row roster republished at +15ms,
  Emacs decode), frame switch and redisplay ~35ms, webview placement ~15ms.
- Logging's per-record `file-directory-p` in `agent-repl--log-dir-truename`:
  844 of 3,201 Emacs CPU samples (26%).
- Pages: boot to painted 30-160ms, but WebKit suspends a hidden page ~0.5s
  after boot (main thread stalled ~97 of 97s); on show it resumes in up to
  ~350ms, then replays its backlog before turning visible: 0.7-2.6s.
- 36 WebContent processes, 11.2GB RSS.
- Experiment (isolated Emacs -Q, xwidget page with five iframes): in a VISIBLE
  page every iframe keeps full timers and animation frames whether shown,
  `visibility:hidden`, `display:none`, stacked underneath, or cross-origin, and
  the host plus all its iframes run in ONE WebContent process.

## Phases (each lands as atomic commits with tests on branch switch-latency)

A. Logging hot path: the log directory's canonical spelling is cached per
   directory and invalidated by the events that change it (the workspace's
   directory changing, its teardown) and by a failed write, which still reports
   loudly. No per-record stat.

B. Tab bar: tab labels are computed when the roster or a status changes, not
   on every redisplay.

C. Logging gaps: every switch path records `elisp.host.editor-switch` (tab
   clicks included); a page records how long it was suspended when it resumes.

D. One host page. One persistent xwidget ("the host") shows every workspace's
   page as an iframe at the workspace's own origin (per-host connection budget
   kept). The host page stays on screen, so iframes are never suspended.
   - D1 webapp: a host entry (host.html/host.ts): ensure/show/remove iframes,
     tell each iframe whether it is shown (postMessage), forward focus.
   - D2 webapp: one presence source (visible AND shown by the host) replaces
     direct `document.visibilityState` reads (unseen gate, prompt wave, expand,
     page presence, wall phase).
   - D3 daemon: serve the host entry.
   - D4 Emacs: the host buffer is displayed as every workspace's webview;
     tab creation (any path) asks the host to ensure the workspace's iframe at
     once, without blocking; a switch shows it; teardown removes it; per-page
     script calls (profiling reads) route through the host to the iframe.

E. Emacs-first switch (needs a contract addition; designed with the owner via
   create-or-update-protobufs): the keystroke moves the frame at once and tells
   the daemon; the daemon's roster `current` is followed only when it is newer
   than Emacs's own latest selection, decided by a revision, not by arrival.

F. Measure every interaction again with /profile-agent-repl; remediate what is
   still over budget (per-frame drawing coalescing of non-roster streams in an
   unshown iframe if measured necessary).

Then: rebase onto master, full tests, fix, cherry-pick onto master (owner
instruction 2026-10-12: bypass the merge queue for this work).

## Related work on master (folded in, 2026-10-12 afternoon)

From the perf program plan (PLAN-perf-program.md, D5-D7) and
investigations/2026-10-12-first-switch-and-daemon-rss.md:

- D5, hidden page replays its backlog: the roster part landed (55a22a48e,
  hidden pages hold the newest roster). With the host, iframes are never
  suspended, so no stream queues a backlog at all; the roster hold stays, now
  keyed on presence (shown by the host AND document visible).
- D6, a live page per workspace (32 processes, 2.85GB): the host replaces the
  proposed LRU of live pages. Every page lives in the host's one WebContent
  process; the host's resident size is measured live in phase F.
- D7, 80-180ms of Emacs elisp before every webview swap, ~118ms unrecorded
  between `maybe-autoselect-input' and `switch-activation-won': the same gap
  measured here (~270ms under load). Phase F records what fills it (a profile
  of the live switch) and cuts it; the roster-push logging volume that ran in
  that window is already cut (log-on-change commits).
- Merge `tests' rows average 1.4MB (max 9.6MB): page bytes on a cold load. A
  host reload (rollout) loads every page at once, so bounding that row matters
  more under the host. Daemon-side; not in this branch.
- "Load only the most recent feed page": already bounded by row count (p50 34
  rows); only row size is unbounded (above).

## Not doing

- The daemon's selection-only roster delta: the roster is "always whole, never
  a delta" by design, and once Emacs switches first its decode is off the
  critical path.
