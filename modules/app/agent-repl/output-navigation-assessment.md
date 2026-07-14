# Output navigation — design assessment

Assessment for adding keyboard-driven semantic navigation to the agent-repl
output window: jump between user prompts, agent final responses, all agent
responses, and pages, with keybindings driven from the input form. This is a
design document only; nothing here is implemented.

## TLDR

The output window is **not an Emacs text buffer** — it is an xwidget WKWebView
(`frontend.el`) hosting the vanilla-TS webapp, whose feed is DOM rendered by
`FeedRenderer` (`webapp/src/render.ts`). Emacs cannot move point in it; the
only Emacs→webview channel is JavaScript injection
(`agent-repl--frontend-webview-execute-script`, `frontend.el:154`), with one
existing precedent: the `agentReplParkAtTail` window-hook contract
(`frontend.el:167` ⇄ `webapp/src/host.ts:22`).

**Recommendation (Approach B):** extend that exact contract. The renderer
stamps a semantic `data-nav` attribute on each feed item; a new
`webapp/src/nav.ts` plants a `window.agentReplNavigate(kind, dir, count)` hook
that does a stateless, viewport-relative scan over those anchors and
`scrollIntoView`s the target; a new elisp module `output-nav.el` defines 8
interactive commands that build the JS call and inject it, bound in
`agent-repl-input-mode-map` (primary, per "form input") and mirrored in
`agent-repl-frontend-webview-mode-map`.

## (a) Current rendering architecture

### The window itself

- Buffer: `*agent-frontend-<ws>*` (`agent-repl-frontend-buffer-name-format`,
  `frontend.el:85`), an `xwidget-webkit-mode` buffer created by
  `agent-repl--frontend-make-webview-buffer` (`frontend.el:141`) and stored on
  the workspace plist as `:frontend-buffer`.
- Minor mode: `agent-repl-frontend-webview-mode` with keymap
  `agent-repl-frontend-webview-mode-map` (`frontend.el:240–265`), currently
  binding only the copy chords (`y`, `C-c`).
- Panel layout: the webview is panel kind `:view`, the input form `:input`
  (`window.el:60–95`); placement in `agent-repl--frontend-display-webview`
  (`frontend.el:337`).
- The webview URL carries `composer=0` (`frontend.el:443–452`): Emacs owns
  input; the webapp is output-only. Input is POSTed straight to the daemon
  (`frontend-client.el:448`), never through the webview.

### The feed and its message model

Layer-2 WS frames (`shared/protocol.md` §2) reduce into
`ConversationStore.items` (`webapp/src/store.ts:423–661`); `FeedRenderer`
(`render.ts:935–1187`) reconciles items into `#feed` as one
`<div class="feed-item" data-key="…">` per item (`itemKey`,
`render.ts:864–877`). The three semantic roles required by the motions:

| role | wire origin | store item | DOM today |
|---|---|---|---|
| user prompt | `user-turn` frame (§2.3) | `kind: "user-turn"` (`store.ts:27`) | `.bubble.user` (`UserTurn`, `render.ts:215`) |
| agent response (any) | `text-start/delta/end` (§2.4) | `kind: "text"` (`store.ts:34`) | `.bubble.assistant.md` (`TextStream`, `render.ts:294`) |
| agent FINAL response | **computed, not a wire type** | same `TextItem` | extra class `final-response` (`render.ts:296`) |

The load-bearing nuance: **"final" is derived at render time** by
`finalResponses()` (`render.ts:644–662`) — the last `text` block before a
`result` frame whose `subtype` is `"success"` (`isTurnComplete`,
`render.ts:616`). Consequences:

- An in-flight turn has no final yet; its last text block is intermediate
  until the `result` lands.
- Aborted/errored turns (`aborted`, `error_*` subtypes) have **no** final
  response at all — final-response motions skip those turns entirely.
- Thinking blocks (`<details class="thinking">`) and tool cards
  (`.tool-card`) are not "responses" for any of the required motions.

### Existing scroll machinery

- Webapp: `parkAtTail` (`scroll.ts:90`), pin detection `isPinnedToBottom`
  (`scroll.ts:72`), auto-repin on a new user turn (`repinsToTail`,
  `render.ts:902`), edge-gated wheel routing (`scroll.ts:199`). No keyboard
  feed navigation and no per-message DOM `id`s (only `data-key`).
- Emacs: exactly one reposition primitive — snap-to-tail via the
  `agentReplParkAtTail` hook (`agent-repl--frontend-snap-webview-to-tail`,
  `frontend.el:183`), called on workspace switch (`panels.el:393`). A removed
  scroll-chords feature left the breadcrumb
  `agent-repl--scroll-output-intercept-states` (`keybindings.el:674`).

## (b) Anchoring mechanism

Classic elisp anchors (text properties, overlays, markers) are inapplicable —
there is no elisp-side buffer text. Anchors must live in the webview DOM, and
the candidates are:

1. **CSS classes already emitted** (`.bubble.user`,
   `.bubble.assistant.md`, `.final-response`) — available today, but they are
   styling surface; a theming refactor silently breaks navigation.
2. **`data-key` on feed-item wrappers** (`render.ts:1057,1140`) — stable
   identity, but encodes item identity, not role, and final-ness is not in it.
3. **New semantic `data-nav` attribute** (recommended) — the renderer stamps
   each feed-item wrapper with `data-nav="prompt" | "response" | "final"`
   (final implies response; selector for motion 3 is
   `[data-nav="response"],[data-nav="final"]`). The value comes from a pure
   `navKindFor(item, chips)` helper beside `finalResponses()`, so the
   nav contract is decoupled from CSS and computed from the same source of
   truth the renderer already uses.

Anchor collection must skip **empty hidden items**: a `user-turn` whose
content is entirely harness-injected meta markers renders nothing and is
hidden by `.feed-item:empty{display:none}` (`styles.css:778`); restored
sessions also create empty shells that fill via rAF backfill
(`render.ts:1089–1104`, drained in milliseconds and flushed synchronously
before any live render).

## (c) Keybinding hookup

- **Commands** live in a new source module `output-nav.el` (one test file per
  module rule → `test-output-nav.el`). Each command resolves the active
  workspace (`agent-repl--ws-current-name`, `workspace.el:977`), fetches its
  `:frontend-buffer`, and signals `user-error` when there is no live webview
  (an expected condition, not an invariant violation).
- **Injection** funnels through the existing external-boundary wrapper
  `agent-repl--frontend-webview-execute-script` (`frontend.el:154`), with the
  same hook-guard script shape as `agent-repl--frontend-tail-script`
  (`frontend.el:172`): `window.agentReplNavigate && window.agentReplNavigate(…);`
  — a mid-navigation webview with no hook yet is an expected no-op.
- **Primary keymap** is the input form's `agent-repl-input-mode-map`
  (`input.el:107`), bound Doom-style via `map!` with `:ni` state prefixes
  exactly like the existing send/interrupt chords (`input.el:173–196`).
- **Mirror keymap** is `agent-repl-frontend-webview-mode-map` plus its evil
  auxiliary bindings, following the copy-selection pattern
  (`frontend.el:271–275`), so the same motions work when focus is on the
  webview panel.
- Focus never needs to move: keys fire in the input buffer, elisp injects JS,
  the webview scrolls. No `general-override-mode-map` chord is needed since
  both maps are buffer-local and uncontested.

## (d) Approaches compared

### A. Stateless CSS-selector scan (no render changes)

`nav.ts` queries the classes the renderer already emits and picks the next
anchor relative to `#feed.scrollTop`.

- Pro: zero renderer changes; smallest diff.
- Pro: stateless — immune to re-renders, replays, workspace-switch snaps.
- Con: couples navigation to styling classes (silent breakage on restyle).
- Con: `final-response` lives on the inner bubble while reconcile identity
  lives on the wrapper — selector logic straddles two DOM levels.

### B. Semantic `data-nav` attributes + stateless viewport scan (recommended)

Same stateless scan, but over renderer-stamped `data-nav` wrapper attributes
((b).3 above), planted as `window.agentReplNavigate` by an `installNavHook`
mirroring `installHostTailHook` (`host.ts:35`).

- Pro: explicit nav contract, decoupled from CSS, one attribute per wrapper.
- Pro: stateless viewport-relative semantics ("next" = first anchor below the
  viewport top) needs no cursor bookkeeping and survives repin-to-tail
  (`render.ts:902`), workspace-switch snaps (`panels.el:393`), and replays.
- Pro: pure helpers (`anchorsFor`, `nextAnchor`) are unit-testable in vitest
  without a real webview, matching `scroll.ts`'s pure-helper style.
- Con: small renderer change plus render tests.
- Con: no persistent "current anchor" highlight (see open questions).

### C. Stateful cursor over store items with highlight

`nav.ts` keeps a cursor index derived from `store.items`, highlights the
current anchor with a CSS class, and resolves it to DOM via `data-key`.

- Pro: exact semantics independent of scroll position; enables a visible
  "you are here" highlight and precise count-repeat.
- Con: real state to invalidate on new turns, replays, `hello` rebuilds,
  backfill, and tail snaps — the exact bookkeeping A/B avoid.
- Con: meaningfully more code in both `nav.ts` and `render.ts`.

### D. Elisp-maintained anchor index (rejected)

Emacs consumes no Layer-2 frames (its side channel is the sentinel files,
`shared/protocol.md` "Agent-state sentinels") and the webview exposes no
scroll state back to elisp. An elisp index would require attaching Emacs as a
WS client or widening the sentinel channel — a new data plane duplicating the
webapp store, for no UX gain over B. Not worth it.

**Verdict:** B now; C's highlight can layer on top of B later without
reworking the elisp surface, since the wire contract (`kind`, `dir`, `count`)
is identical.

## (e) Proposed command surface

### Elisp (`output-nav.el`)

```elisp
(defconst agent-repl-frontend-nav-hook "agentReplNavigate"
  "Webapp window global for feed navigation; must match NAV_HOOK in nav.ts.")

(defun agent-repl--output-nav-script (kind direction count) …)   ; pure builder
(defun agent-repl--output-navigate (kind direction count) …)      ; resolve ws → inject

(defun agent-repl-output-next-prompt (&optional count) (interactive "p") …)
(defun agent-repl-output-prev-prompt (&optional count) (interactive "p") …)
(defun agent-repl-output-next-final-response (&optional count) (interactive "p") …)
(defun agent-repl-output-prev-final-response (&optional count) (interactive "p") …)
(defun agent-repl-output-next-response (&optional count) (interactive "p") …)   ; final + intermediate
(defun agent-repl-output-prev-response (&optional count) (interactive "p") …)
(defun agent-repl-output-page-down (&optional count) (interactive "p") …)
(defun agent-repl-output-page-up (&optional count) (interactive "p") …)
```

`kind` ∈ `prompt | response | final | page`; `direction` ∈ `next | prev`;
`count` is the raw prefix arg clamped to ≥ 1. Emitted script:
`window.agentReplNavigate && window.agentReplNavigate('prompt','next',2);`.

### Webapp (`webapp/src/nav.ts`)

```ts
export type NavKind = "prompt" | "response" | "final" | "page";
export function anchorsFor(feed: HTMLElement, kind: NavKind): HTMLElement[];
export function nextAnchor(tops: number[], viewportTop: number,
                           dir: 1 | -1, count: number, epsilonPx: number): number | null;
export function navigate(feed: HTMLElement, kind: NavKind, dir: 1 | -1, count: number): void;
export function installNavHook(feed: HTMLElement): void;  // plants window.agentReplNavigate
```

Anchor jump: `scrollIntoView({block:"start"})` (instant, like `parkAtTail` —
repeat-press friendly). Paging: `feed.scrollBy(0, dir * count *
(feed.clientHeight - OVERLAP_PX))` with a small `OVERLAP_PX` for continuity;
clamped at the ends, no wrap. `epsilonPx` keeps an anchor sitting exactly at
the viewport top from being re-selected by a repeated `prev`/`next`.

Renderer change: `FeedRenderer` stamps `el.dataset.nav = navKindFor(item,
chips)` where it already stamps `data-key` (`render.ts:1057,1140`).

### Suggested keybindings

Input form (`map! :map agent-repl-input-mode-map`, states `:ni`; all verified
unbound there today):

| chord | motion |
|---|---|
| `M-p` / `M-n` | prev / next user prompt |
| `M-P` / `M-N` | prev / next final response |
| `C-M-p` / `C-M-n` | prev / next response (incl. intermediate) |
| `C-M-v` / `C-M-S-v` | page down / page up |

`C-M-v`/`C-M-S-v` deliberately reuse Emacs's scroll-other-window idiom — the
webview is "the other window" from the input form.

Webview panel (`agent-repl-frontend-webview-mode-map` + evil normal aux, the
copy-chord pattern): the same four chord pairs, plus vim-idiom brackets
`[[` / `]]` (prompts), `[r` / `]r` (responses), `[f` / `]f` (finals), and
`C-u` / `C-d` (paging).

## (f) Test coverage required

Per the module rules (one test file per source module, one edge case per
test):

- **`test-output-nav.el`** (new; batch ert, mock
  `agent-repl--frontend-webview-execute-script` via `cl-letf` per the
  external-boundary convention):
  - one test per command asserting the built script's `kind`/`direction`
    (8 tests);
  - script builder includes the `window.X && window.X(…)` hook guard;
  - prefix arg is passed through as `count`;
  - nil/zero/negative prefix clamps to 1;
  - dispatch targets the current workspace's `:frontend-buffer`;
  - no `:frontend-buffer` → `user-error`;
  - dead (killed) `:frontend-buffer` → `user-error`;
  - keybinding presence tests for the new chords in both maps, mirroring
    `test-keybindings.el`'s existing style.
- **`webapp/test/nav.test.ts`** (new; vitest, jsdom like `scroll.test.ts`):
  - `anchorsFor` selects prompts only / finals only / responses+finals;
  - `anchorsFor` skips empty hidden shells;
  - `nextAnchor`: first anchor below viewport top for `next`;
  - `nextAnchor`: first anchor above for `prev`;
  - `nextAnchor`: epsilon keeps the at-top anchor from re-matching;
  - `nextAnchor`: `count` folds (skips count−1 anchors);
  - `nextAnchor`: clamps at either end without wrapping;
  - paging distance = `count × (clientHeight − OVERLAP_PX)`;
  - `installNavHook` plants the window global (mirror `host.test.ts`).
- **`webapp/test/render.test.ts`** (additions):
  - `data-nav="prompt"` on user-turn wrappers;
  - `data-nav="response"` on intermediate assistant text;
  - `data-nav="final"` on the pre-success-result text block;
  - no `data-nav` on thinking/tool/permission/result items;
  - meta-only user turn gets no anchor.
- **`webapp/test/host.test.ts`** (addition): nav hook constant matches the
  elisp `agent-repl-frontend-nav-hook` string (the same one-contract test the
  tail hook has).

## Edge cases and gotchas

- **In-flight turns:** the streaming last text block is intermediate until
  `result:success`; `final` motions correctly ignore it, `response` motions
  see it.
- **Aborted/errored turns:** contribute no `final` anchor by design.
- **Repin on new prompt:** `repinsToTail` (`render.ts:902`) can yank the
  viewport to the tail when a new user turn arrives mid-navigation; stateless
  navigation just resumes from wherever the viewport lands.
- **Workspace switch:** `panels.el:393` snaps to tail on switch; stateless
  design has no cursor to invalidate.
- **Async injection:** `xwidget-webkit-execute-script` is fire-and-forget;
  the design never needs a return value, so elisp stays synchronous-free.
- **Granularity choice:** motion 3 is per text *block*, not per turn — a turn
  with three commentary blocks yields three stops (see open questions).

## Open questions

1. Should `response` motions stop per text block (proposed) or once per turn?
2. Is a transient "you are here" highlight on the jumped-to anchor wanted
   (a small C-style add-on atop B)?
3. Should the motions also get Doom leader bindings (e.g. under the existing
   agent-repl `SPC` prefix), or are the two buffer-local maps enough?
