# Design proposal: interactive chessboard widget in agent-repl response bubbles

Status: research complete, no implementation. Comparison of three candidate approaches with a recommendation, per the four investigation axes (chess.com embed, ChessCom internal widget, xwidget limitations, from-scratch).

## 0. Architectural ground truth (reframes the whole question)

- The agent-repl "GUI" is not an Emacs-rendered transcript. It is a TypeScript SPA (`webapp/src/`) served by the Go daemon (`claude-repld`) and displayed inside a single `xwidget-webkit` buffer (`frontend.el`, `frontends.el:6-12`).
- Response bubbles are DOM nodes built by `TextStream()` (`webapp/src/render.ts:315-340`) via `renderMarkdown()` (`webapp/src/markdown.ts:51-162`). Elisp renders no bubble content at all.
- Consequence: "a board inside the Emacs response bubble" necessarily means "a board component in `webapp/src/`". There is no elisp/SVG-in-buffer rendering path to attach to, and conversely a DOM board adds **zero new xwidget surface** — it rides the one webview the GUI already is.

```
Emacs (xwidget WS host)  ⇄  Go daemon (claude-repld)   [Layer 2: WebSocket]
Go daemon                ⇄  TS shim (per session)       [Layer 1: stdio NDJSON]
TS shim → Agent SDK → Claude
```

## 1. Axis: public chess.com embed — RULED OUT

- The public embed iframe is `https://www.chess.com/emboard?id=<ID>` and accepts **only a stored chess.com entity id**.
  - Confirmed in server source: `emboardDiagramAction` (`ChessCom/chess`, `src/Chess/WebBundle/Controller/WebController.php`) 404s without `id` and resolves it against the Diagram / DailyPuzzle repositories. No `pgn=` / `fen=` parameter, no postMessage or JS API exists.
  - Public-side research corroborates: arbitrary PGN is only reachable by uploading to `chess.com/analysis` first to mint an id (online, account-bound, not self-contained).
- Viewer move-playing does not exist on the embed (only a "solve as puzzle" mode replaying the saved solution). Variations render only for saved-analysis embeds.
- Reliability/licensing: external embeds broke repeatedly in 2024 (blank boards, missing annotations); ToS frames embedding as a website feature, an Emacs-hosted webview is a gray area.
- **Verdict: fails the arbitrary-PGN requirement and the interactive-move-playing requirement outright. Ruled out.**

## 2. Axis: ChessCom internal widget (vendored)

### `@chesscom/chessboard` (repo `ChessCom/chessboard`, v1.178.0)

- This is the production chess.com web board. It fully satisfies the functional requirements:
  - `game.load({ pgn })` accepts a full PGN (also `fen`, TCN `moves`); analysis mode supports variation trees; playing/setup/observing modes give interactive move-making; plugin system (move lists, premoves, clocks, threats); SSR support.
- Packaging: published to GitHub Packages (`npm.pkg.github.com`, `@chesscom` scope) — install requires a chess.com GitHub PAT with `read:packages` (`npm-setup.js`). Dist is compiled (Svelte→JS), mounts into a plain DOM element, so it would work inside the framework-free agent-repl webapp.
- Coupling: hard dep `@chesscom/web-ui@2.1.2`, peer `@chesscom/design-system>=0.14.0`, plus board/piece theme assets (CDN-oriented).
- Licensing: `"license": "UNLICENSED"` — proprietary internal code.
  - As an npm dep it never enters this repo (`webapp/.gitignore` excludes `node_modules/` and `dist/`), but `package.json` would reference it and **every build of the frontend would require chess.com-internal registry auth**.
  - Vendoring the source into the config outright is not acceptable if this config repo is (or ever becomes) public.

### `@chesscom/js-chess-engine` (JCE, repo `ChessCom/jsChessEngine`, v7.2.0)

- Dependency-free chess logic library underlying the board: `loadPgn()` with full variation support (`createVariation()`, `selectLine()`), legality, FEN, headers, plus UCI engine drivers.
- Same registry and same `UNLICENSED` status. Highly vendorable technically (single self-contained package), encumbered identically.

### Other ChessCom candidates (all non-fits)

- `chessboard-legacy` (old JS board), `chessboard-subtree` (read-only split), `swiftui-chessboard`/`swift-chessboard` (native iOS), `playwright-chessboard` (test driver), JCE's `board/` prototype (dev toy).

## 3. Axis: xwidget limitations

### Local ground truth

- Running build: GNU Emacs 30.2, hand-rolled from `/Users/dodgecoates/src/emacs` (`--with-ns --with-xwidgets`, WKWebView-backed). `(featurep 'xwidget-internal)` → `t` in the live daemon. ATS allows `http://` loads.

### NS/macOS failure modes (severity scored for THIS feature)

| Failure mode | Raw severity | Relevance here |
|---|---|---|
| Widget is an always-on-top NSView, repositioned per redisplay (scroll tearing, covers overlays) | high | **Already priced in** — the whole GUI is one such view today |
| One view per xwidget: window splits blank the second copy | high | Already applies to the GUI as a whole, a DOM board changes nothing |
| Wheel events over the widget are swallowed by the page | high inline | Irrelevant — the page IS the transcript, wheel-scrolls the feed as intended |
| Keyboard: async `xwHasFocus()` heuristic, IME broken, `perform-lispy-event` is a no-op on NS | high | Avoid: keep the board mouse-only, route any keys via `xwidget-webkit-execute-script` (`frontend.el:161-163`) |
| No `xwidget-webkit-load-html` on NS | medium | Irrelevant — the daemon already serves over `http://localhost` |
| Mouse click and drag reach the page natively (`mouseDown/Up` forwarded + passed to super, `mouseDragged` untouched) | — | **Enabler** — webapp buttons already work in daily use, board clicks/drags ride the same path |
| Subsystem near-unmaintained upstream (~8 trivial commits 2024-2025, no owner) | structural | Argues for adding no NEW xwidget objects, which the webapp-DOM approach satisfies |

- **Verdict: a NEW inline xwidget per board (in any Emacs text buffer) would be fragile — but that architecture doesn't exist here. A DOM board inside the existing webview inherits the GUI's current, evidently-tolerable xwidget risk and adds none.**

## 4. Axis: from-scratch board (as a webapp component)

- Rendering surface: inline SVG (or a plain DOM grid) emitted as an HTML string from a new `webapp/src/chessboard.ts`, exactly parallel to `metaprompt-tree.ts`. The webapp is framework-free TS + Vite, so no runtime is added. Piece glyphs: bundle an open piece set (e.g. the public-domain cburnett SVGs) or Unicode chess glyphs as a v0.
- Trigger/attach point: intercept a ```` ```pgn ```` (and optionally ```` ```fen ````) fenced block in `flushFence()` (`webapp/src/markdown.ts:83-100`) — the exact seam the metaprompt-tree custom renderer already uses (`markdown.ts:88-90`, `render.ts:332-338`).
- PGN parsing, variation tree, legality: in TypeScript inside the webapp (elisp never sees bubble content). Options, in descending preference:
  1. `chessops` + its PGN module (lichess, GPL-3): full RAV variation trees, legality, battle-tested; optionally with `chessground` (lichess's zero-dep interactive board, GPL-3) instead of hand-rolling drag/click.
  2. Vendored JCE (§2): functionally ideal, licensing/registry-encumbered.
  3. `chess.js` (BSD-2): no variation-tree support, we would hand-roll the tree — its main advantage is the laxer license.
  4. Fully hand-rolled: bounded but the largest effort (legality + RAV parser + promotion/castling/en-passant edge cases).
- Interactivity (clicks → moves): the established webapp patterns cover everything needed:
  - Delegated `data-*` click handling in the `FeedRenderer` constructor (`render.ts:1008-1030`), as QueuedCard/QuestionPrompt buttons do today.
  - Board UI state (selected square, current node in the variation tree, orientation) lives in the renderer beside `questionSelections` (`render.ts:998`) so it survives the per-delta `innerHTML` rewrites (`render.ts:1202-1211`), re-hydrating the SVG from state on each render.
  - Forward/backward/variation stepping as on-board buttons (or optional Emacs keybindings via the `xwidget-webkit-execute-script` bridge later).
- Scope of change: pure webapp — no daemon, shim, or protocol changes; ships through `bin/build-frontend.sh`'s webapp target.

## 5. Comparison

| Criterion | chess.com embed | Vendored `@chesscom/chessboard` | Bespoke webapp component |
|---|---|---|---|
| Arbitrary PGN | no (id-only) | yes | yes |
| Variations stepping | saved-analysis only | yes | yes (chessops or JCE) |
| User plays moves | no | yes | yes |
| New xwidget surface | none, but ruled out anyway | none | none |
| Self-contained / offline | no (chess.com servers) | no (registry auth + CDN assets) | fully |
| Licensing | ToS gray area | UNLICENSED internal | clean (GPL/BSD dep or clean-room) |
| Maintenance control | none | upstream churn (v1.178, fast-moving) | full |
| Effort | n/a | low-medium (integration + auth plumbing) | medium |

## 6. Recommendation

**Build the bespoke board as a webapp component (`webapp/src/chessboard.ts`), triggered by a ```` ```pgn ```` fence, with `chessops` (optionally + `chessground`) for logic/interaction — falling back to fully hand-rolled logic only if adding a GPL dep is unwanted.**

- It is the only option that satisfies all four requirements (arbitrary PGN, mainline+variation stepping, interactive move-playing, lives in the response bubble).
- It is also the most reliable with respect to xwidget flakiness: the key research finding is that the bubble is already webview DOM, so the bespoke path adds no new xwidget objects while the interactivity rides the mouse-event path the GUI already exercises daily.
- The component boundary (load PGN → render → emit/accept moves) mirrors `@chesscom/chessboard`'s API shape, keeping a later swap to the real chess.com board possible if the registry/licensing coupling ever becomes acceptable.

## 7. Open questions for discussion

1. Logic dependency: `chessops`/`chessground` (GPL-3, most proven), hand-rolled (dependency-free), `chess.js` (BSD-2 but no variation trees), or vendored JCE (proprietary)?
   - ~~DECIDED: hand-rolled~~ **SUPERSEDED — DECIDED: CEE via the WASM TypeScript SDK, as a STRICT requirement** (see §8). No hand-rolled or third-party chess logic anywhere; without the CEE capability configured, the widget does not exist.
2. Trigger surface: ```` ```pgn ```` fence only, or also auto-detect bare PGN/FEN in assistant prose?
3. Interaction model: click-click, drag-and-drop, or both (chessground gives both for free)?
4. Is this config repo public or ever intended to be (hard constraint on any `@chesscom/*` dependency)?

## 8. Implementation plan (CEE-WASM logic, webapp component)

**CEE is a STRICT requirement for this feature.** All chess semantics — PGN parsing, game tree, legality, move application — flow exclusively through the explanation engine compiled to WASM, driven by the TypeScript SDK (`@chesscom/cee-wrapper`, `explanation-engine/sdks/lang/typescript`, ISC). There is no hand-rolled or third-party chess-logic fallback of any kind: when the CEE capability is not configured, the widget does not activate and ```` ```pgn ````/```` ```fen ```` fences render as ordinary code blocks.

Widget code is plain TS in `webapp/src/` (flat, matching the existing layout), each module with a `webapp/test/<module>.test.ts` vitest file, one commit per completed step. No daemon/shim/protocol changes.

### Capability gate

- One env var (working name `AGENT_REPL_CEE_DIR`) points at the explanation-engine checkout/build.
- `bin/build-frontend.sh` reads it to (a) resolve `@chesscom/cee-wrapper` and the `@chesscom/proto_*` peer-deps from the local checkout (vite aliases / `file:` deps — never the internal registry, keeping registry auth out of the build), and (b) copy `explanation-engine.wasm` + the worker JS into the webapp's served assets.
- The widget module loads via dynamic import behind a build-time flag, so ungated builds contain no CEE code and behave exactly as today.
- Elisp surfaces the gate as a defcustom mirroring the env var (precedent: `$CEEPYGNPATH` in `modules/app/chess/config.el`), used only for diagnostics (`doctor.el` check) — the webapp build is the real consumer.

### Phase 0 — SDK readiness (in the explanation-engine repo)

1. Verify the wrapper's generated client exposes the widget-essential features; regenerate (`npm run generate:types`) or fall back to the generic transport for any missing ones:
   - `get_ergonomic_game` (PGN → `chess.Game` tree, comments/NAGs preserved), `get_ergonomic_position`/`get_fen`, `legal_moves`, `make_moves`/`unmake_moves`, `validate_pgn`.
2. Confirm the wasm artifact + worker JS build outputs and their load config (`createWasmClient(config)` asset URLs) work when served by `claude-repld`'s static file server.

### Phase 1 — CEE client integration

3. `src/cee.ts` — worker/client lifecycle:
   - Bootstrap `createWasmClient` lazily on first chess fence; one CEE game instance per board widget (mirroring the one-game-per-session model), LRU-capped with disposal for old bubbles.
   - Parse results cached by PGN-text hash so `innerHTML` re-renders never re-hit the worker.
   - Every `CeeError` surfaces visibly in the widget frame (never swallowed, per error-handling policy).
   - Tests: fake transport implementing the used features; lifecycle, caching, error surfacing.

### Phase 2 — rendering

4. `src/chessboard.ts` — widget HTML:
   - `chessboardHtml(state): string` — inline SVG board (squares, coordinates, last-move highlight, selected-square + legal-target markers) with Unicode piece glyphs for v1; per-node positions come from CEE (`get_ergonomic_position`/`get_fen`).
   - Nav strip (`|<` `<` `>` `>|`, flip) and a variation indicator when the current node has siblings, all as `data-board-*` buttons.
   - Async hydration: the fence renders a placeholder frame synchronously, then hydrates when the worker returns the parsed game.
   - Tests: SVG assertions on piece placement, highlight classes, nav-button presence per tree position.
5. `src/markdown.ts` — trigger:
   - Intercept ```` ```pgn ```` and ```` ```fen ```` fences in `flushFence()` (`markdown.ts:83-100`), mirroring the `isMetapromptTree` branch; on CEE parse rejection (`validate_pgn`) render the plain `<pre>` plus the visible error.
   - Tests: fence dispatch, ungated passthrough, invalid-PGN error path.

### Phase 3 — interactivity

6. `src/render.ts` — state + events:
   - Per-item board state in `FeedRenderer` beside `questionSelections` (`render.ts:998`), keyed by `itemKey` + fence index: current game point, selected square, orientation. Re-hydrate after each `innerHTML` rewrite (`render.ts:1202-1211`).
   - Delegated `data-board-*` click handling in the constructor's existing listener block (`render.ts:1008-1030`): square click = select / play / deselect with `legal_moves` supplying the target dots, nav clicks walk game points, promotion via a 4-choice mini-picker.
   - User moves via `make_moves` at the current game point: stepping into an existing child when the move matches, otherwise creating the new variation node — CEE's game tree is the single source of truth.
   - Tests: state survival across re-render, legality filtering, variation insertion vs step-into.

### Phase 4 — integration & polish

7. `styles.css` — board sizing inside `.bubble-body` (fixed max width, responsive), theme-consistent colors.
8. End-to-end verify: `npm test` + `npm run typecheck` in `webapp/` (both gated and ungated builds), `bin/build-frontend.sh` with `AGENT_REPL_CEE_DIR` set, hot-reload the live Emacs xwidget session, and exercise a real ```` ```pgn ```` bubble (variations + user moves) before final commit.

### Explicitly out of scope for v1

- Drag-and-drop (click-click only), SVG piece art, move-list pane, search/eval/threats/coach features (natural CEE-powered follow-ups), bare-PGN auto-detection in prose, Emacs-side keybindings via `xwidget-webkit-execute-script`.
