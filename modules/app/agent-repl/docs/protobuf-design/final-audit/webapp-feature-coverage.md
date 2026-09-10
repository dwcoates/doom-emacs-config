# Webapp feature-coverage audit — what `docs/overhaul/webapp.md` does not account for

Method: every module under `webapp/src/` was read for the USER-FACING feature it
carries, then checked against `docs/overhaul/webapp.md` for accounting — a port,
a named removal / dead-code entry, or a deliberate ignore. Implicit accounting
counts (e.g. "no client-side derivation" covers `permission-preview.ts`; "type-out
pacing is the client's" in `feed.proto` covers `smooth.ts`). Contract files under
`proto/src/` were consulted to decide whether an implicit home exists. Paths are
relative to `modules/app/agent-repl/`.

A FINDING is a real, live feature with NO accounting either way.

## Findings (most significant first)

1. **The Claude account LOGIN flow — a full-screen xterm.js TUI over a daemon pty.**
   - Evidence: `webapp/src/login.ts` (whole module), `webapp/src/login-terminal.ts`,
     `webapp/src/main.ts:103`, `main.ts:2466` (`requestLogin(httpBase, activeSessionId)`),
     `main.ts:2475` (`attachLoginTerminal(loginTermEl, wsBase, …)`), `main.ts:912`
     (`login-overlay`), `main.ts:2466` (`accountEl.disabled`).
   - Why unaccounted: the plan's topbar section covers only the DISPLAY —
     "the account label draws the email, or 'logged out' as a warning state"
     (`webapp.md:106`) — and `topbar.proto:75-85` has `TopbarAccountLoggedIn` /
     `TopbarAccountLoggedOut` arms. Nothing in the plan, and no rpc in
     `proto/src/agentrepl/v1/`, lets the user ACT on "logged out": there is no
     `Login`/`OpenLoginTerminal` verb, no pty stream, and no ruling that the
     login TUI is dropped. The current flow also rides an HTTP side-channel
     (`httpBase`) plus a raw ws pty, both of which the Connect-only port deletes.
   - Confidence: HIGH.

2. **The chess-board widget rendered inline in a response bubble.**
   - Evidence: `webapp/src/chess-game.ts:23` (`CHESS_GAME_MARKER`), `:72`
     (`splitChessGameSegments`), `:112` (`ChessWidgetHandle` mount contract),
     `webapp/src/render.ts:815` (`chessGameContainerHtml(seg.path)`),
     `main.ts:688` (`configureChessGames`), `main.ts:696` (`installChessNavHook`),
     plus the search module explicitly skipping it (`search.ts:88`
     `SKIP_SELECTOR = ".chess-game, .chess-game-error"`).
   - Why unaccounted: the plan's feed row taxonomy (`webapp.md:180-206`) and
     `frontend/v1/feed.proto` have no chess arm, and the response bubble is
     "markdown, rendered by the client's prose renderer" — so the skill's marker
     line would render as literal text after the port. The plan neither ports
     the widget (no `FeedChessGame`, no artifact-style bubble named for it) nor
     lists it under "Dead code to remove". The `/show-chess-game` skill is a
     live, installed skill, so this is a silent regression.
   - Confidence: HIGH.

3. **The host JS-eval hook surface — the only way Emacs drives the webview.**
   - Evidence: `webapp/src/host.ts:22` (`agentReplParkAtTail`), `:56`
     (`agentReplCloseTopbarMenus`), `:79` (`agentReplRecoverNow`), `:109`
     (`agentReplAdjustTextScale`); `nav.ts:49` (`agentReplNavigate`);
     `search.ts:67` (`agentReplSearch`); `recovery-probe.ts:161`
     (`agentReplRecoveryProbe`); installed at `main.ts:711, 715, 1417, 1426,
     1673, 750, 696`.
   - Why unaccounted: the plan settles the daemon-facing transport wholesale but
     never mentions the Emacs↔webview channel. It becomes MORE load-bearing after
     the port, not less: the plan makes the webview composer-less
     (`webapp.md:281`), which removes the keyboard driver these hooks duplicate,
     leaving `window.agentRepl*` as the sole input path. Nothing says which hooks
     survive, who re-installs them, or what replaces them.
   - Confidence: HIGH.

4. **Incremental in-feed search (Emacs-style isearch), including fold/tab reveal.**
   - Evidence: `webapp/src/search.ts` (857 lines) — `:67` host hook, `:70-85`
     match/current/reveal classes, `:91` `CLOSED_FOLD_SELECTOR` (search opens
     closed folds), `:94` `INACTIVE_TAB_SELECTOR` (searches inactive tab strips),
     `main.ts:746, 750`.
   - Why unaccounted: no mention anywhere in `webapp.md`. Its reveal behavior is
     also entangled with two things the plan actively redesigns — sub-feed
     collapse (`OpenFeed`/abandon token) and the merge tab strip — so a
     rendered-but-collapsed row is no longer even in the DOM to be found.
     Neither ported nor ruled out.
   - Confidence: HIGH.

5. **Bubble navigation — cycling prompts / final responses / tool cards.**
   - Evidence: `webapp/src/nav.ts:40` (`data-nav` tokens), `:58`
     (`NAV_CLASSES = prompt|final|tool`), `:78` `navTokensForItem`, emitted per
     item by `render.ts:83`; `main.ts:1426` (host hook), `main.ts:2337`
     (composer chords).
   - Why unaccounted: nav tokens are derived per RENDERED ITEM from the old
     conversation-item vocabulary, which the FeedRow oneof replaces wholesale.
     The plan says nothing about jump/cycle affordances (the only navigation it
     names is footer panel rows jumping to a `FeedId`, `webapp.md:210`, and the
     shared jump-to-file link, `:112`).
   - Confidence: HIGH.

6. **Copy-the-selection key chords (`C-c` / `y`) — the webview has no Cmd-C.**
   - Evidence: `webapp/src/copy.ts:37` (`isCopyChord`), `:79`
     (`writeSelection`), `:101` (`installCopyKeys`), wired at `main.ts:704`; the
     module header states WKWebView provides no native copy path.
   - Why unaccounted: not mentioned. This is an environment limitation, not a
     transport concern, so the port does not fix it incidentally; losing it makes
     feed text unextractable inside Emacs.
   - Confidence: HIGH.

7. **Permission-mode selection (pending-mode picker + ungated-session banner).**
   - Evidence: `webapp/src/pending-mode.ts:16` (`PendingPermissionMode` — a pick
     is pending until a prompt carries it), `main.ts:559, 590, 1097, 2359`;
     `webapp/src/ungated.ts:43` (`UNGATED_PERMISSION_MODES`), `:101`
     (`ungatedBannerHtml`), `:124` (`unswitchableModeOptionHtml`), painted at
     `main.ts:1116` and `main.ts:1103`.
   - Why unaccounted: `agentrepl/v1` has no set-permission-mode rpc (only
     `SetModel`), `endpoint_submit_prompt.proto` carries no mode field, and
     `topbar.proto`'s warning arms (`:150-159`) have no ungated/bypass arm. The
     plan's topbar spec lists "model selector, context chip, warning chip"
     (`webapp.md:100`) and no mode control. So both the CONTROL and the
     "this session has no permission gate at all" WARNING vanish, unnamed.
   - Confidence: HIGH (the mode control), MEDIUM-HIGH (the ungated banner —
     conceivably intended as a `TopbarWarning`, but no arm exists for it).

8. **The topbar counter chips: subagent roster and task roster, with turn-aged retention.**
   - Evidence: `webapp/src/counter-menu.ts` (shared facade), `agents.ts`
     (`agentsMenuHtml`, `SUBAGENT_TOOLS`), `tasks.ts` (`tasksMenuHtml`),
     `turn-clock.ts:55` (the retention clock), rendered at `topbar.ts:128, 130`
     and mirrored into the footer at `progress-footer.ts:28, 44`.
   - Why unaccounted: the new topbar is explicitly enumerated —
     "left tight (account, connectivity dot), centered flexing title, right tight
     (model selector, context chip, warning chip)" (`webapp.md:99-101`) — which
     silently deletes both counters, but the "Dead code to remove" section never
     names them. Partial implicit home: the footer's "live-work chips"
     (`webapp.md:212`) and `agents_panel.proto` / `todos_panel.proto` (which are
     SLASH-COMMAND answers, not a standing roster). The turn-scoped retention
     rule is nowhere.
   - Confidence: MEDIUM-HIGH (removal is implied by an exhaustive layout spec,
     but never ruled, and the retention semantics have no successor).

9. **The client-side prompt queue that absorbs prompts across a BACKEND OUTAGE.**
   - Evidence: `webapp/src/prompt-queue.ts` (whole module), `main.ts:626`
     (construction), `main.ts:658` (`promptQueue.offer(...)` intercepts a submit),
     `main.ts:1922` (`promptQueue.drain(...)` on recovery).
   - Why unaccounted: the plan discusses the DAEMON-hold tray
     (`WatchDaemonHolds`, `UpdateHeldPrompt`, `webapp.md:216-219, 256-257`) and
     the `heldPromptUnsentFailure` card (`webapp.md:44-49`) — both of which
     presuppose a REACHABLE daemon. This queue exists precisely for the window in
     which the daemon is gone (store+sidecar+daemon+shim roll), where no rpc can
     be made at all. Not ported, not ruled out.
   - Confidence: MEDIUM-HIGH (its host may be the Emacs composer post-port, but
     the plan never says so and the elisp plan does not claim it either).

10. **Unsupported-slash-command detection, and the "engineer support for it" offer.**
    - Evidence: `webapp/src/unsupported.ts:43` (`parseUnsupportedCommand`), `:61`
      (`requestSupportWorkspace`), used at `render.ts:2168, 2600` and
      `main.ts:804` (`addSupport: (command) => requestSupportWorkspace(...)` —
      it SPAWNS A WORKSPACE to implement the missing command).
    - Why unaccounted: the plan covers the daemon-answered command panels
      (`webapp.md:224-227`) but never the refusal case; `agentrepl/v1` has no
      verb for "create a workspace to add support for command X", and the current
      path is another `httpBase` side-call the Connect-only port deletes.
    - Confidence: MEDIUM-HIGH.

11. **The gns-sockets bridge fold — swallowing Slack-bridge upkeep under the final response.**
    - Evidence: `webapp/src/gns.ts:47` (`BRIDGE_SUBAGENT_TYPE = "sockets-listener"`),
      `:50` (`isBridgeSpawn`), `:122` (`gnsFolds`), consumed at `render.ts:94, 884`.
    - Why unaccounted: this is client-side derivation from a subagent TYPE STRING,
      which the "no client-side derivation" philosophy forbids and which the
      contract cannot express (vendor identities and per-tool knowledge do not
      reach the client). The correct successor would be a daemon-side decision
      surfaced on the feed row — but the plan neither asks for one nor lists this
      as a removal.
    - Confidence: MEDIUM-HIGH.

12. **Metaprompt TLDR-tree re-rendering (ASCII tree → non-shearing HTML).**
    - Evidence: `webapp/src/metaprompt-tree.ts` (297 lines), used at
      `render.ts:80, 823, 827` (`findTreeRegion` / `looksLikeIntendedTree` /
      `renderTreeHtml` applied to final-response text).
    - Why unaccounted: like #11, a client-side content sniff over response prose.
      The plan's response bubble is "markdown, rendered by the client's prose
      renderer" (`feed.proto:433-449`); a tree region would revert to sheared
      `<pre>`. Neither a daemon-composed successor nor a removal is stated.
    - Confidence: MEDIUM.

13. **Capped sections: click-to-expand, edge-gated inner scroll, and tail-follow.**
    - Evidence: `webapp/src/expand.ts` (click-to-expand for every N-line preview),
      `webapp/src/scroll.ts` (edge-gated wheel + `TailFollow`),
      `webapp/src/dom.ts` (the shared ancestor walk), installed at
      `main.ts:683, 699, 702`, and `installHostTailHook` at `main.ts:711`.
    - Why unaccounted: the plan's only use of "expand"/"collapse" is the sub-feed
      bubble lifecycle (`webapp.md:67, 277`), a different thing. The output-form
      oneof (`FeedSimpleToolCall` text/code/diff/lines/links) says nothing about
      height caps, and the plan never states whether capping/expansion/scroll
      gating survives or is dropped. Purely webview-local behavior, hence easy to
      lose silently.
    - Confidence: MEDIUM.

14. **The global scheduled-shutdown DRAIN banner (and the `draining` body class).**
    - Evidence: `webapp/src/drain.ts:29` (`DRAINING_BODY_CLASS`), `:91`
      (`drainHeadline`), `:101` (`drainShimNote`), `:116` (`drainBannerHtml`),
      painted at `main.ts:1123-1124`.
    - Why unaccounted: partial implicit home only — `daemon_hold.proto:118-120`
      (`HeldPromptShutdownHold`) surfaces a shutdown hold PER HELD PROMPT in the
      tray, and `UpdateShutdownSchedule` is called out as operator tooling
      (`webapp.md:258`). Neither gives the drain a STANDING, page-wide,
      daemon-global announcement ("a bounce is scheduled and waiting"), which is
      the point of `drain.ts`'s header (the lease is the daemon's, not a
      workspace's). Not named as a removal either.
    - Confidence: MEDIUM.

15. **The standalone element catalogue page (`catalogue.html`).**
    - Evidence: `webapp/catalogue.html`, `webapp/src/catalogue.ts` (761 lines),
      `webapp/test/catalogue.test.ts`, `catalogue.ts:758` (its own document-title
      gate).
    - Why unaccounted: a second built page that mocks every streaming element in
      the taxonomy. It is entirely built from the OLD item vocabulary, so the port
      breaks it wholesale; the plan neither ports it nor lists it as dead code.
      Plausibly a deliberate ignore — but "deliberate" is exactly what is missing.
    - Confidence: MEDIUM (low user-facing weight, high certainty of no accounting).

16. **Stripping the host's injected metaprompt spans out of the user's own prompt.**
    - Evidence: `webapp/src/meta.ts:18-21` (`META_OPEN` / `META_CLOSE`
      sentinels), `:30` (`stripMetaSpans`), consumed by `turn.ts:23`
      (`userTurnText`) and `turn-clock.ts:55`.
    - Why unaccounted: `conversation.v1.UserSaid` carries plain `TextBlock`s with
      no meta-span notion (tag 2, prompt provenance, is RETIRED), and the plan
      never says who strips the Emacs-injected read-directive / autonomous-execution
      preamble. Under server-driven UI the daemon plausibly composes the drawn
      prompt already — that would be implicit coverage — but nothing states it, and
      the sentinel convention is a webapp↔Emacs agreement with no successor named.
    - Confidence: MEDIUM-LOW (most likely resolved daemon-side, but unstated).

17. **Minor, unstated but low-weight:** the browser/tab title tracking the model
    (`main.ts:1212`, `document.title = "claude-repl · <model>"`) and the
    `localStorage` verbose-logging toggle (`wslog.ts:212, 306`). Neither is
    mentioned; both are trivially re-creatable.
    - Confidence: MEDIUM-LOW significance, HIGH certainty of no accounting.

## Checked and accounted (no finding)

Explicitly accounted (ported, replaced, or named for removal):

- `frontend-proto.ts`, `frontend-command.ts`, `proto-names.ts`, `proto-scalars.ts`,
  `protocol.ts`, `ws.ts` transport framing — "Dead code to remove", the whole
  hand-decoded FrontendFrame/FrontendCommand transport (`webapp.md:16-24`).
- `state-adapter.ts` (the adapter seam), `store.ts` accumulation — superseded by
  whole-view pushes + upserts by `FeedId`.
- Every `UNANCHORED` decode table in `async-bubble.ts` / `agent-emission.ts` —
  named at `webapp.md:20-23`.
- `async-bubble.ts` detached-work vocabulary and the 3-tier identity ladder —
  named at `webapp.md:27-29` (→ `FeedDetachedShell`/`FeedDetachedSubagent`).
- `progress-footer.ts` cold-keep-alive warning row — named at `webapp.md:25-26`.
- `response-usage-stamp.ts`, `tokens.ts`, `token-breakdown-view.ts` token
  arithmetic — named at `webapp.md:30-32` and the context-chip/breakdown spec
  (`webapp.md:103-104`).
- `local-failure.ts`'s three loud-throw stubs — `webapp.md:38-46`.
- `failure-card.ts` / `FailureKind` vendor band — `webapp.md:47-49`.
- `hibernation.ts` — ruled out outright: "No hibernation exists anywhere in the
  contract… Build no such UI state" (`webapp.md:279-281`).
- `merge-status.ts`, `merge-gate.ts`, `merge-dequeue.ts` — the merge-bubble
  section (`webapp.md:65-90`), the composer-refusal treatment (`webapp.md:10-15`),
  and `AnswerHeldOffer` / `HeldOfferMergeDequeue`.
- `version-skew.ts` — replaced by daemon-pushed reload/`transferred` signals on
  `WatchWebWorkspace` (`webapp.md:262-266`), "never a client heuristic".
- `connect-resync.ts`, `conversation-pager.ts`, `history-pager.ts`,
  `resync-snapshot.ts`, `fence.ts`, `session-rebase.ts`, `session-identity.ts` —
  superseded by `OpenFeed`/`WatchFeed`/`GetFeedPage`, no cursors, no fences,
  no epochs; reconnect = re-open (`webapp.md:157-161, 246-250`).
- `load-more.ts` — `FeedPage` `has_more` / `at_start` edges.
- `partition.ts`, `subfeed.ts`, `async-routing.ts`, `async-render.ts`,
  `async-stream.ts`, `async-teal.ts`, `stream-member.ts`, `watchers.ts`,
  `watcher-poll.ts` — self-similar feeds + `FeedId` routing; the poller's job is
  daemon-pushed now.
- `sidebar.ts` — `sidebar.proto`, the one global roster stream, with the blink
  cadence pinned (`webapp.md:114-117`).
- `topbar.ts` / `topbar-view.ts` / `account.ts` — the topbar section
  (`webapp.md:98-106`) and `topbar.proto`.
- `progress-footer.ts` / `footer-liveness.ts` / `breathing.ts` — `footer.proto`
  Status→SubStatus→StatusActivity, fully-resolved panels.
- `status.ts` and the panel renderers — `status_panel.proto` et al.
  (`webapp.md:224-227`).
- `clear-compact.ts` — `separation` rows, ONE renderer subroutine
  (`webapp.md:109-111`).
- `permission-preview.ts` — superseded by `FeedPermissionArguments` (composed
  daemon-side) and the no-derivation rule.
- `timer.ts`, `agent-clock.ts`, `duration.ts` — "Clocks tick client-side"
  (`webapp.md:166-169`).
- `smooth.ts` — `feed.proto:433` "type-out pacing is the client's".
- `markdown.ts`, `highlight.ts`, `fence.ts` code rendering, `prompt-body.ts`,
  `skill-body.ts` — the client's prose renderer over `markdown` fields; test
  output as paint-class spans (`webapp.md:75-79`).
- `wslog.ts` / `clientlog-throttle.ts` — `ClientLog` (`webapp.md:257`).
- `address.ts` — per-workspace addressing, implicit in the workspace-scoped
  `Watch*` streams and `WatchWebWorkspace`.
- `background-recovery.ts`, `restart-window.ts`, `recovery-probe.ts` (the
  RECOVERY logic itself; its host HOOK is finding #3) — reconnect-is-re-open plus
  daemon-pushed rollout signals.
- `lazy-item.ts`, `coalesce.ts`, `html-slot.ts`, `fold.ts`, `dom.ts` (as
  infrastructure) — rendering strategy, invariant under the transport port.
- `external-link.ts` — partially implicit in the ONE shared link component
  (`webapp.md:112`); flagged here rather than as a finding because the
  open-externally behavior is a rendering-layer concern the link component must
  carry regardless. Worth confirming the shared component keeps it.
