/**
 * Webapp bootstrap: session creation/join, WebSocket wiring, composer.
 *
 * URL parameters:
 *   ?daemon=host:port   daemon address (default: current host)
 *   ?session=<id>       join an existing session (else one is created)
 *   ?fake=1             create the session against the offline fake SDK
 *   ?parent_ws=<name>   parent workspace basename shown in the topbar
 */
import {
  TOPBAR_AGENT_ATTR,
  nextCounterMenu,
  runningAgentClocks,
  sessionTopbarDatapoints,
  topbarClickAction,
  topbarInfoHtml,
} from "./topbar.js";
import { AgentClock } from "./agent-clock.js";
import { AGENTS_SPEC } from "./agents.js";
import { TASKS_SPEC } from "./tasks.js";
import {
  type CounterSpec,
  MISSING_BUBBLE_NOTICE_MS,
  missingBubbleNotice,
} from "./counter-menu.js";
import { configureChessGames, installChessNavHook } from "./chess-game.js";
import { RenderCoalescer, windowFrameHost } from "./coalesce.js";
import { SmoothReveal } from "./smooth.js";
import { installCopyKeys } from "./copy.js";
import { installClickExpand } from "./expand.js";
import {
  HostGlobal,
  installHostCloseMenusHook,
  installHostTailHook,
  installHostTextScaleHook,
} from "./host.js";
import { FeedNav, installNavHook, installNavKeys } from "./nav.js";
import {
  type Account,
  type AccountMenuEntry,
  type RosterEntry,
  accountIsLoggedOut,
  accountLabel,
  accountMenuEntries,
  accountModeLabel,
  fetchAccount,
  fetchAccountMenuEntries,
  fetchAccounts,
  switchAccount,
} from "./account.js";
import { attachLoginTerminal, type LoginTerminal } from "./login-terminal.js";
import { closeLogin, loginNotice, requestLogin } from "./login.js";
import { PermissionMode } from "./protocol.js";
import { decodeFrontendFrame } from "./frontend-proto.js";
import { StateAdapter, type DegradedBanner } from "./state-adapter.js";
import { CommandDispatcher } from "./command-dispatch.js";
import type { CommandStruct } from "./frontend-command.js";
import { PendingPermissionMode } from "./pending-mode.js";
import { rebindSession, rememberResumeKeys } from "./rebind.js";
import { hiddenContinueMessage, rememberMidTask, shouldAutoContinue } from "./resume-continue.js";
import { remediationNotice, requestRemediation } from "./remediation.js";
import { requestSupportWorkspace } from "./unsupported.js";
import { statusSnapshotFromInit } from "./status.js";
import { compactionBannerHtml, FeedRenderer, lastUserTurnId, modelOptionsHtml } from "./render.js";
import { installEdgeScroll, isPinnedToBottom, parkAtTail } from "./scroll.js";
import { FeedSearch, type SearchHost, installSearchHook } from "./search.js";
import {
  WorkspaceSidebar,
  installWorkspaceExpandHook,
  installWorkspaceRosterHook,
} from "./sidebar.js";
import { ConversationStore } from "./store.js";
import { TIMER_SLOT, TaskTimer, windowHost } from "./timer.js";
import { WsClient, composerEnabled, makeSessionExistsProbe } from "./ws.js";
import {
  ForwardingLogger,
  setLogger,
  type ClientLogContext,
  type ClientLogLevel,
} from "./wslog.js";
import { fetchTaskTail } from "./watcher-poll.js";
import "./styles.css";

function must<T extends HTMLElement>(id: string): T {
  const el = document.getElementById(id);
  if (!el) throw new Error(`missing #${id}`);
  return el as T;
}

async function boot(): Promise<void> {
  const params = new URLSearchParams(location.search);
  const daemon = params.get("daemon") ?? location.host;
  const httpBase = `${location.protocol === "https:" ? "https" : "http"}://${daemon}`;
  const wsBase = `${location.protocol === "https:" ? "wss" : "ws"}://${daemon}`;

  // The session id to join. When ?session is unset it is created over the WS
  // (CreateSessionCmd) once the command dispatcher exists — see below.
  const joinParam = params.get("session");
  // Mutable on purpose: the "session gone" rebind swaps the live view
  // onto a successor session id; every closure below reads the current
  // binding.
  let activeSessionId: string = joinParam ?? "";
  let ws: WsClient;

  // Resume/rebind + auto-continue tracking, re-fed by the SessionView plane
  // (via the store) now that the frontend.v1 cutover routes it through the
  // adapter. Reset on a rebind so a successor session starts fresh.
  //   - rememberedClaudeId: last durable CLI uuid persisted for rebind.
  //   - midTaskActive: last turn-in-flight value written to the mid-task marker.
  //   - autoContinueChecked: whether this connection's fresh-join nudge was
  //     already evaluated (checked once, before the marker is rewritten).
  let rememberedClaudeId = "";
  let midTaskActive = false;
  let autoContinueChecked = false;

  // Delivery-path diagnostics (§2.15). The webapp→daemon log forward rode the
  // legacy `client-log` ClientCommand, which the S8/S9 outbound cutover
  // deleted; E4's additive `client_log` FrontendCommand arm restores it on the
  // protobuf channel rather than reviving a second transport.
  //
  // The sink is assigned a few lines below rather than closed over directly,
  // because the dispatcher it needs is built AFTER this logger (the dispatcher
  // logs through it). Until then a line is console-only — exactly the pre-boot
  // behavior `setLogger` already documents, not a fallback hiding a failure.
  let clientLogSink:
    | ((level: ClientLogLevel, message: string, context?: ClientLogContext) => boolean)
    | null = null;
  const wslog = new ForwardingLogger(
    (cmd) => clientLogSink?.(cmd.level, cmd.message, cmd.context) ?? false,
  );
  const clog = wslog.log.bind(wslog);
  // Deep modules (render walk, pollers) log through the module-level
  // singleton; install the real forwarder before anything renders.
  setLogger(wslog);

  const store = new ConversationStore((level, message) => clog(level, message));
  // The one-change cutover seam: the daemon pushes `agentshim.frontend.v1`
  // protojson frames, which decode (frontend-proto.ts) into effects
  // (state-adapter.ts) the store ingests. The adapter's explicit-ignore path
  // logs once per unsupported shape at debug — mapped to `info` here since the
  // client-log channel has no debug level.
  const adapter = new StateAdapter((level, message) =>
    wslog.log(level === "debug" ? "info" : level, message),
  );
  // The frontend→daemon command plane (§task 4): every outbound command is a
  // FrontendCommand protojson frame over the CURRENT socket (read lazily, like
  // wslog, so a rebind's successor socket carries subsequent commands). The
  // dispatcher is fed every inbound decoded frame (`observe`) so it can
  // correlate CommandAcks by requestId and a createSession's pushed SessionView.
  const dispatcher = new CommandDispatcher({
    send: (raw) => (ws as WsClient | undefined)?.send(raw) ?? false,
    log: (level, message) => clog(level, message),
    // A REJECTED clientLog cannot be reported through `log`: that forwards
    // another clientLog, earns another rejection, and loops. It still has to be
    // SEEN, so it goes to the logger's local-only path — the same injected
    // console sink every other line uses, at error level because a forward that
    // the daemon refused is a real failure of the diagnostics channel, not a
    // quiet fallback. The forward itself still happened and still failed loudly.
    logLocal: (message) => wslog.logLocalOnly("error", message),
  });
  // The workspace a runtime command names — the live session's cwd, as the
  // pushed `SessionView` reports it. The daemon stamps the URL-scoped
  // workspace onto any command that omits one (see frontendCommandTranslator),
  // so this is advisory on the session socket and is legitimately "" until the
  // first SessionView lands.
  const cmdWorkspace = (): string => store.state.cwd;

  // Close the diagnostics loop declared above: from here every forwarded line
  // rides the protobuf command plane into the daemon's log.
  clientLogSink = (level, message, context) =>
    dispatcher.clientLog(cmdWorkspace(), level, message, context as CommandStruct | undefined);

  // The permission mode the user picked that no prompt has carried yet:
  // frontend.v1 has no standalone set-permission-mode command, so the mode
  // rides `SubmitPromptCmd` and settles only when a pushed SessionView
  // reports it in force (see pending-mode.ts and renderChrome).
  const pendingMode = new PendingPermissionMode();
  /**
   * Submit one prompt as a `SubmitPromptCmd`, carrying any pending permission
   * mode. The pending mode is NOT spent on send: it clears when a pushed
   * SessionView reports it in force, so a failed submit does not silently
   * drop the user's choice.
   */
  const submitPrompt = (text: string, what: string): void => {
    void dispatcher
      .submitPrompt(cmdWorkspace(), text, pendingMode.outbound)
      .catch((err: unknown) => clog("error", `${what} failed: ${String(err)}`));
  };
  const feedEl = must("feed");
  // The store/sidecar/shim degraded-state banner (design §11): a simple line
  // pinned above the feed, styled as a warning, shown while a component is
  // degraded and cleared on its recovery notice.
  const degradedBannerEl = must("degraded-banner");
  const showDegraded = (dn: DegradedBanner): void => {
    if (dn.recovered) {
      degradedBannerEl.hidden = true;
      degradedBannerEl.textContent = "";
      return;
    }
    degradedBannerEl.textContent = `degraded: ${dn.component} — ${dn.reason}`;
    degradedBannerEl.hidden = false;
  };
  // The search's echo area: isearch keeps its query out of the text being
  // searched, and so does this — the composer's draft stays untouched while
  // a search runs, and the query shows up here instead.
  const searchStatusEl = must("search-status");
  // Chess-game bubbles fetch their payload through the daemon and mount
  // the in-place-served widget; the session getter tracks rebinds, and
  // the pinning pair lets an async board mount restore the feed's tail.
  configureChessGames({
    base: httpBase,
    session: () => activeSessionId,
    isPinned: () => isPinnedToBottom(feedEl),
    parkFeed: () => parkAtTail(feedEl),
  });
  // The Emacs host's webview-buffer keys step the active board through
  // this hook (out-of-band: the xwidget cannot deliver keys into the page).
  installChessNavHook(window as unknown as Record<string, unknown>);
  // Sections only take the wheel in their left/right gutters, so wheeling
  // over one scrolls the feed past it instead of scrolling it.
  installEdgeScroll(feedEl);
  // A click on a capped section drops its N-line cap and lays it out at
  // full length; the next click on it restores the preview.
  installClickExpand(feedEl);
  // The webview has no menu bar, so `C-c` / `y` are what copy a highlight.
  installCopyKeys(document);
  // The Emacs host snaps the feed to its newest message through this hook
  // whenever the user switches to the workspace holding this webview.
  installHostTailHook(window as unknown as HostGlobal, feedEl);
  // The Emacs host dials the feed's text size up or down through this hook,
  // sizing the document root's font so every rem-based run of text scales
  // together (the interactive text-size commands in frontend.el fire it).
  installHostTextScaleHook(window as unknown as HostGlobal, document.documentElement);
  // The workspaces rail. Emacs pushes the whole roster through this hook on
  // every workspace-state change; the rail stays collapsed until the first
  // push, so a bare-browser session keeps the single-column layout. Roster
  // state is global to the editor, not per-session, so it lives beside the
  // store rather than in it. The pin/park pair lets the rail's first
  // reveal re-park the feed: the reveal narrows and reflows the feed, so a
  // feed the boot render parked at its tail is snapped back down once the
  // rail is on screen — the gui's two halves land at the newest message
  // together (same pattern as the async chess mount above).
  const sidebar = new WorkspaceSidebar(must("ws-sidebar"), {
    httpBase,
    isPinned: () => isPinnedToBottom(feedEl),
    parkFeed: () => parkAtTail(feedEl),
  });
  installWorkspaceRosterHook(window as unknown as HostGlobal, sidebar);
  // C-S-RET in the input window fires the expand hook to unfold the
  // cursor row's detail panel (openDirs is client-owned, off the roster).
  installWorkspaceExpandHook(window as unknown as HostGlobal, sidebar);
  // Incremental search over the feed (isearch semantics), driven from the
  // composer's keys below. Built before the renderer because the renderer
  // must announce every render to it: the marks live in item DOM that a
  // render rewrites wholesale, so they are re-derived rather than kept.
  //
  // The hook is planted whether or not this page has a composer. Emacs runs
  // the webview with `composer=0` and cannot deliver keys into the page at
  // all, so the hook is the host's only way in — same contract as the chess
  // nav hook, and it answers the status line for the host to echo.
  const search = new FeedSearch(feedEl, (text) => {
    searchStatusEl.textContent = text;
    searchStatusEl.classList.toggle("on", text !== "");
  });
  installSearchHook(window as unknown as SearchHost, search);
  // The live tail line's bottom-pinned slot (see `#tail-slot`): a flex sibling
  // between the scrolling feed and the composer, so the progress indicator +
  // running turn-stats stay stuck to the window's bottom rather than trailing
  // the last bubble as the feed grows.
  const tailSlotEl = must("tail-slot");
  const feed = new FeedRenderer(feedEl, {
    onRendered: () => search.refresh(),
    decidePermission: (requestId, behavior) => {
      // Answer the pending canUseTool via PermissionAnswerCmd (S8).
      void dispatcher
        .permissionAnswer(cmdWorkspace(), {
          permissionRequestId: requestId,
          allow: behavior === "allow",
          denyMessage: behavior === "allow" ? "" : "denied from webapp",
        })
        .catch((err: unknown) => clog("error", `permission answer failed: ${String(err)}`));
    },
    answerQuestions: (requestId, updatedInput) => {
      // AskUserQuestion contract: allow with the tool input echoed back
      // carrying the `answers` record the user picked (updated_input Struct).
      void dispatcher
        .permissionAnswer(cmdWorkspace(), {
          permissionRequestId: requestId,
          allow: true,
          updatedInput: updatedInput as Record<string, unknown>,
          denyMessage: "",
        })
        .catch((err: unknown) => clog("error", `question answer failed: ${String(err)}`));
    },
    // Held-prompt queue controls (E4). These were loud no-ops between the
    // cutover and the queue's return; they now drive the real command arms.
    cancelQueued: (entryId) => {
      void dispatcher
        .queueCancel(cmdWorkspace(), entryId)
        .catch((err: unknown) => clog("error", `queue cancel failed: ${String(err)}`));
    },
    runQueuedNow: (entryId) => {
      void dispatcher
        .queueForce(cmdWorkspace(), entryId)
        .catch((err: unknown) => clog("error", `queue run-now failed: ${String(err)}`));
    },
    acceptQueued: (entryId) => {
      void dispatcher
        .queueAccept(cmdWorkspace(), entryId)
        .catch((err: unknown) => clog("error", `queue accept failed: ${String(err)}`));
    },
    sendPrompt: (text) => {
      // Card controls (stop task) are prompt-mediated: the button sends an
      // ordinary user message through the same command the composer uses.
      submitPrompt(text, "card prompt");
    },
    // Watcher folds poll this while open (§ watcher-bubble expansion),
    // targeting the CURRENT session so a rebind moves the polls with it.
    fetchTaskTail: (taskId, offset) => fetchTaskTail(httpBase, activeSessionId, taskId, offset),
    // The unsupported-command card's button. Targets the CURRENT session
    // so a rebind opens the workspace against the checkout in view, and
    // resolves to the workspace name Emacs was asked for — Emacs, not the
    // daemon, decides what actually happens next.
    addSupport: (command) => requestSupportWorkspace(httpBase, activeSessionId, command),
    // The `/status` panel's data. The snapshot half is re-sourced from the
    // session's PUSHED SystemInit (no round trip, and never staler than the
    // daemon's own view, which is why the old GET /status and its
    // /status/refresh re-probe are both gone). Only the account half is
    // fetched, on the sanctioned account endpoint, targeting the CURRENT
    // session so a rebind reads the account of the checkout in view.
    getStatus: () =>
      fetchAccount(httpBase, activeSessionId).then((account) => ({
        snapshot: statusSnapshotFromInit(store.state.systemInit),
        account,
      })),
  }, tailSlotEl);

  const statusEl = must("conn-status");
  const infoEl = must("session-info");
  const summaryEl = must("task-summary");
  const modeEl = must<HTMLSelectElement>("mode-select");
  const modelEl = must<HTMLSelectElement>("model-select");
  const spinnerEl = must("spinner");
  const compactBarEl = must("compact-progress-slot");
  const remediationEl = must("remediation");
  const accountEl = must<HTMLButtonElement>("account");
  const accountMenuEl = must("account-menu");
  const loginOverlayEl = must("login-overlay");
  const loginAccountEl = must("login-account");
  const loginTermEl = must("login-term");
  const loginCloseEl = must<HTMLButtonElement>("login-close");
  const parentWs = params.get("parent_ws");

  // Which topbar dropdown is open (only one at a time): a counter roster
  // or the tokens breakdown. It lives HERE rather than in the DOM because
  // renderChrome rewrites the whole topbar on every frame, which would
  // otherwise collapse an overlay the user is reading mid-turn.
  let counterMenu: "agents" | "tasks" | "tokens" | null = null;

  // The running task's timer now paints the live feed-tail stats row, not a
  // topbar slot: its clock moved down beside the progress indicator (see
  // `turnStatsRowHtml`). Its tick writes just that one span rather than
  // re-rendering the feed — a whole-feed rewrite once a second, on top of the
  // per-frame rewrites a streaming turn already drives, would be churn in
  // service of a single changing digit. The FeedRenderer skips the write when
  // no row is mounted (off-turn, or a replay-only paint), and bakes the last
  // reading into every render so a fresh row never blinks empty.
  const timer = new TaskTimer(windowHost(window), (label) => feed.paintTurnTimer(label));

  // The agent bubbles' own elapsed tick, the header timer's twin (see
  // agent-clock.ts): one interval repaints every RUNNING agent's topbar
  // slot, so a quiet agent's clock still moves between frames. A slot the
  // feed does not currently hold — a bubble inside a closed activity
  // panel — legitimately has nowhere to paint and is skipped.
  const agentClock = new AgentClock(windowHost(window), (agentId, label) => {
    const slot = feedEl.querySelector(
      `[${TOPBAR_AGENT_ATTR}="${agentId}"] [${TIMER_SLOT}]`,
    );
    if (slot) slot.textContent = label;
  });

  const renderChrome = (): void => {
    const s = store.state;
    // topbarInfoHtml escapes every value it interpolates. The same strip
    // renderer draws the agent-scoped bubble topbars (see topbar.ts).
    infoEl.innerHTML = topbarInfoHtml(sessionTopbarDatapoints(s, parentWs, store.taskRoster), {
      agentsOpen: counterMenu === "agents",
      tasksOpen: counterMenu === "tasks",
      tokensOpen: counterMenu === "tokens",
    });
    // The idle-with-live-async signal breathes as the sidebar's amber dot on
    // this session's own row rather than as strip text. The flag is the feed
    // renderer's own gate reading (idle + live async), read back here so the
    // rail mirrors the feed the last render already partitioned.
    sidebar.setMonitoring(feed.isMonitoring());
    // After the strip exists, so the paint on a starting turn has a span to
    // land in. Only the edges of a turn touch the interval.
    timer.sync(s.turnStartedAt);
    // The bubble clocks reconcile on the same cadence: whichever agents are
    // running keep their topbar slots ticking between frames.
    agentClock.sync(runningAgentClocks(s.items));
    // Rebuilt only when the menu or the selection actually moved: this runs
    // on EVERY frame, and blowing the options away mid-turn would slam shut
    // a dropdown the user had open.
    const nextOptions = modelOptionsHtml(s.models, s.model);
    if (modelEl.innerHTML !== nextOptions) modelEl.innerHTML = nextOptions;
    // A held permission-mode pick keeps the picker on the user's choice until
    // the daemon reports that mode in force, at which point the pick is spent.
    const wantMode = pendingMode.settle(s.permissionMode);
    if (modeEl.value !== wantMode) modeEl.value = wantMode;
    spinnerEl.classList.toggle("on", s.turnInFlight);
    // The centered "current objective" label (§2.14): textContent (not
    // innerHTML) so the daemon's summary is inert text, and the full line
    // rides in the tooltip since the strip ellipsis-clips it. Empty until
    // the first completed turn produces one, which collapses the element.
    const summary = s.taskSummary ?? "";
    summaryEl.textContent = summary;
    summaryEl.title = summary;
    // Empty string when no compaction runs, which collapses the slot.
    compactBarEl.innerHTML = compactionBannerHtml(s.compacting);
    document.title = s.model ? `claude-repl · ${s.model}` : "claude-repl";
  };

  const setCounterMenu = (menu: "agents" | "tasks" | "tokens" | null): void => {
    if (counterMenu === menu) return;
    counterMenu = menu;
    renderChrome();
  };

  // The reveal half of a roster-row click: dismiss the roster either way so
  // a revealed card is unobscured, and when the entry's bubble was NOT
  // found, say so in #remediation (the topbar's one status-line slot) for a
  // few seconds instead of silently doing nothing. The timed clear checks
  // the slot still shows THIS notice, so it never wipes a login or
  // remediation notice that landed meanwhile.
  const settleRosterReveal = (spec: CounterSpec, revealed: boolean): void => {
    setCounterMenu(null);
    if (revealed) return;
    const notice = missingBubbleNotice(spec);
    remediationEl.textContent = notice;
    window.setTimeout(() => {
      if (remediationEl.textContent === notice) remediationEl.textContent = "";
    }, MISSING_BUBBLE_NOTICE_MS);
  };

  // The chips are re-created by every renderChrome, so the toggles are
  // delegated off the topbar rather than bound to nodes that will not
  // survive the turn. Opening one counter closes the other. The click
  // vocabulary is the strip's own (topbarClickAction), shared with the
  // agent bubbles' delegation in the FeedRenderer.
  infoEl.addEventListener("click", (e) => {
    const action = topbarClickAction(e.target as HTMLElement);
    if (!action) return;
    if (action.kind === "toggle") {
      setCounterMenu(nextCounterMenu(counterMenu, action.menu));
      return;
    }
    // A roster row jumps the feed to the entry's bubble — a subagent's
    // card, a task's TaskCreate card — and lays it open.
    if (action.kind === "reveal") {
      settleRosterReveal(AGENTS_SPEC, feed.revealAgent(action.agentId));
      return;
    }
    settleRosterReveal(TASKS_SPEC, feed.revealTask(action.taskId));
  });
  // An open overlay closes the way every dropdown does: click off it, or
  // Escape. The agent bubbles' topbar overlays dismiss on the same
  // gestures, so both handlers close them alongside the header's.
  document.addEventListener("click", (e) => {
    const target = e.target as HTMLElement;
    if (
      !target.closest(".agents-menu") &&
      !target.closest(".tasks-menu") &&
      !target.closest(".tokens-menu")
    ) {
      setCounterMenu(null);
      feed.closeAgentMenus();
    }
  });
  document.addEventListener("keydown", (e) => {
    if (e.key === "Escape") {
      setCounterMenu(null);
      feed.closeAgentMenus();
    }
  });
  // The composer is a separate Emacs window the outside-click handler above
  // cannot see, so the host fires this hook when the user clicks into it —
  // closing the header and bubble dropdowns the same way a click-away would.
  installHostCloseMenusHook(window as unknown as HostGlobal, () => {
    setCounterMenu(null);
    feed.closeAgentMenus();
  });

  // Keyboard cycling of the feed, driven from whichever input box this
  // build has. The cursor is re-seated after every feed render (never
  // before: the wrappers it resolves against must already exist), so a
  // streaming turn cannot silently drop an in-flight cycle.
  const nav = new FeedNav(feedEl);
  // The Emacs GUI hides the composer below and owns input itself, so its
  // chords reach the cycle as an injected script rather than a key event.
  installNavHook(window as unknown as HostGlobal, nav);

  // The feed is fed a SMOOTHED view of the store: still-streaming text and
  // thinking blocks are revealed a few characters per frame rather than one
  // API chunk at a time, so a burst reads as a fast type-out instead of a
  // lurch. Only the feed sees the smoothed copy — nav and chrome key off turn
  // ids and stats, which the reveal does not touch, so they take the real
  // state. When the reveal is still catching up it asks for another frame,
  // and that self-driven loop stops the moment every block reaches its
  // frontier (see `SmoothReveal.reveal`).
  const smooth = new SmoothReveal({ now: () => performance.now() });
  const rerender = (): void => {
    const shown = smooth.reveal(store.state);
    feed.render(shown.state);
    nav.reconcile(lastUserTurnId(store.state.items));
    renderChrome();
    if (shown.pending) frames.schedule();
  };

  // Renders are coalesced onto animation frames: a burst of ingested effects
  // — the backlog draining when this webview's hidden workspace is switched
  // back to, a reconnect's snapshot, a fast delta stream — otherwise runs a
  // full feed render per effect, and that churn is the switch-in jitter. One
  // paint drains however many effects landed since it was scheduled.
  const frames = new RenderCoalescer(
    windowFrameHost(window),
    () => {
      rerender();
    },
    {
      // Stall watchdog: a webview whose WebKit process wrongly believes
      // it is occluded (xwidget reparenting) suspends rAF while CSS
      // animations keep breathing — the feed freezes with no error
      // anywhere. The visibility/focus snapshot is the evidence that
      // distinguishes that state from a genuinely hidden page.
      now: () => performance.now(),
      onStall: (ms) =>
        clog(
          "warn",
          `render stall: rAF pending ${Math.round(ms)}ms while frames keep arriving (visibility=${document.visibilityState} focus=${document.hasFocus()})`,
          // The same facts as fields, so the daemon's log can be read back by
          // something other than a human eye — this is the stall investigation's
          // primary evidence, and grepping a sentence for a number is not a plan.
          {
            pendingMs: Math.round(ms),
            visibility: document.visibilityState,
            focus: document.hasFocus(),
          },
        ),
      onStallRecover: (ms) =>
        clog("warn", `render stall recovered after ${Math.round(ms)}ms`, {
          stalledMs: Math.round(ms),
        }),
    },
  );
  // The wake anchors' countdowns tick on wall-clock time, not on frames,
  // so nothing would re-render them between deltas. A slow heartbeat
  // ask keeps them honest through the same coalescer every other render
  // rides; reconciliation no-ops every node whose HTML did not change.
  window.setInterval(() => frames.schedule(), 30_000);

  // swapTo rebinds the live view onto a successor session id (the
  // client-side twin of the Emacs sync-webview rebind): fresh store,
  // fresh socket, URL param updated so a reload lands on the successor.
  const swapTo = (next: string): void => {
    // Forwarded on the OLD socket in the instant before it closes; when
    // that loses the race the line still reaches the console.
    clog("warn", `session rebind: ${activeSessionId} -> ${next}`, {
      fromSessionId: activeSessionId,
      toSessionId: next,
    });
    ws.close();
    activeSessionId = next;
    // The successor is a fresh conversation view: its own claude session id,
    // its own mid-task marker, its own fresh-join auto-continue evaluation.
    rememberedClaudeId = "";
    midTaskActive = false;
    autoContinueChecked = false;
    // A paint scheduled against the dead session would render the
    // just-reset (empty) store; the successor's hello drives the next one.
    frames.cancel();
    // The successor's blocks are not this session's: drop every reveal cursor
    // so a reused block id cannot inherit a dead session's shown length.
    smooth.reset();
    store.reset();
    // The successor's turn is not this one's: stop the clock now rather than
    // letting it run on the dead session until the successor's hello lands.
    // The dead session's agents are not the successor's either.
    timer.stop();
    agentClock.stop();
    const url = new URL(location.href);
    url.searchParams.set("session", next);
    history.replaceState(null, "", url.toString());
    spinnerEl.classList.remove("alarm");
    remediationEl.textContent = "";
    // The successor is a fresh component set: any degraded banner belonged to
    // the dead session and must not carry over.
    degradedBannerEl.hidden = true;
    degradedBannerEl.textContent = "";
    ws = makeClient(next);
    ws.connect();
  };

  // remediate dispatches the headless analyst — the LAST resort, reached
  // only once daemon-side rehydration (the probe already failed) and the
  // client-side rebind above have both come up empty.
  const remediate = (sessionId: string): void => {
    remediationEl.textContent = remediationNotice("devising");
    void requestRemediation(httpBase, sessionId)
      .then((phase) => {
        remediationEl.textContent = remediationNotice(phase);
      })
      .catch((err: unknown) => {
        // A remediation that never launched must say so: silently
        // leaving "devising remediation plan" up would claim a recovery
        // effort that does not exist.
        remediationEl.textContent = remediationNotice("failed");
        clog("error", `remediation dispatch failed: ${String(err)}`);
      });
  };

  const makeClient = (sessionId: string): WsClient =>
    new WsClient({
      url: `${wsBase}/sessions/${sessionId}/stream`,
      log: (message) => clog("warn", message),
      onMessage: (data) => {
        // The one path: decode the protojson `frontend.v1` frame, let the
        // command dispatcher correlate its CommandAcks + a createSession's
        // pushed SessionView, map it to typed adapter effects, surface any
        // degraded banner, then ingest the effects into the store. A
        // malformed/unknown frame hard-errors (the decoder and adapter never
        // degrade) — evidence first, then re-throw, as the old raw path did.
        let effects;
        try {
          const decoded = decodeFrontendFrame(data);
          dispatcher.observe(decoded);
          effects = adapter.apply(decoded);
        } catch (err) {
          clog(
            "error",
            `frontend frame decode/adapt threw: ${String(err)} — frame head: ${data.slice(0, 200)}`,
          );
          throw err;
        }
        for (const effect of effects) {
          if (effect.kind === "degraded") showDegraded(effect.value);
        }
        const result = store.ingest(effects);
        // Resume/rebind + auto-continue, re-fed from the SessionView plane the
        // store now populates (claude_session_id/cwd). Skipped entirely until a
        // durable CLI uuid is known (pre-init frames carry none).
        const claudeId = store.state.claudeSessionId;
        if (claudeId !== "") {
          if (claudeId !== rememberedClaudeId) {
            // The SessionView supplied (or updated) the durable CLI uuid:
            // persist it + cwd so a future "session gone" can rebind this
            // conversation instead of dead-ending at remediation.
            rememberedClaudeId = claudeId;
            try {
              rememberResumeKeys(localStorage, activeSessionId, {
                claudeSessionId: claudeId,
                cwd: store.state.cwd,
              });
            } catch (err) {
              clog("error", `rememberResumeKeys failed: ${String(err)}`);
              throw err;
            }
          }
          if (!autoContinueChecked) {
            // Fresh-join auto-resume: the first time this connection learns the
            // claude session id, a mid-task marker that outlived a killed turn
            // (with the rehydrated turn NOT live) means the task was stopped
            // mid-flight — nudge it to continue without drawing a bubble. Read
            // BEFORE the marker rewrite below so it sees the pre-boot value.
            autoContinueChecked = true;
            if (shouldAutoContinue(localStorage, claudeId, store.state.turnInFlight)) {
              submitPrompt(hiddenContinueMessage(), "auto-continue");
            }
          }
          // Track turn-in-flight transitions so a kill mid-task leaves the
          // marker set for the next boot's auto-resume. Written only at a turn
          // boundary, not on every delta.
          if (store.state.turnInFlight !== midTaskActive) {
            midTaskActive = store.state.turnInFlight;
            try {
              rememberMidTask(localStorage, claudeId, store.state.turnInFlight);
            } catch (err) {
              clog("error", `rememberMidTask failed: ${String(err)}`);
              throw err;
            }
          }
        }
        if (result.changed) {
          // One paint per animation frame, however many effects land before
          // it. The coalescer decides chrome-only vs full feed when it fires.
          frames.schedule();
        }
        // The push channel carries no client-command replies now; the daemon
        // resyncs via `StateSnapshot`, not a client replay-request.
        return undefined;
      },
      onStatusChange: (connected) => {
        statusEl.textContent = connected ? "connected" : "disconnected";
        statusEl.classList.toggle("ok", connected);
        // Socket lifecycle in the daemon log: pairs with the daemon's
        // own attach/detach lines to show WHICH side went quiet.
        clog(connected ? "info" : "warn", `ws: ${connected ? "connected" : "disconnected"}`);
      },
      sessionExists: makeSessionExistsProbe(httpBase, sessionId),
      onGone: () => {
        statusEl.textContent = "session gone";
        statusEl.classList.remove("ok");
        // The turn-in-flight tick becomes a red/orange alarm: a lost session
        // is not a quiet state, and the dot is what the eye lands on.
        spinnerEl.classList.add("alarm");
        remediationEl.textContent = "rebinding session";
        void rebindSession(sessionId, localStorage, createSessionViaWs)
          .then((next) => {
            if (next !== null) {
              swapTo(next);
              return;
            }
            // Nothing durable was ever stored for this id: remediate.
            remediate(sessionId);
          })
          .catch((err: unknown) => {
            clog("error", `session rebind failed: ${String(err)}`);
            remediate(sessionId);
          });
      },
    });
  // Session creation (replaces POST /sessions), used both to open the first
  // session and to rebind a gone one: a short-lived connection to the unscoped
  // /frontend WS — the daemon has no session-scoped socket to offer before the
  // session exists. It feeds ONLY its own dispatcher's correlation (never the
  // render store), sends CreateSessionCmd once the initial snapshot lands (so
  // the correlation's known-session set is populated first, and a pre-existing
  // same-cwd session cannot masquerade as the new one), and resolves with the
  // new id from the pushed SessionView.
  const createSessionViaWs = (
    args: { cwd: string; resumeClaudeSessionId: string } = { cwd: "", resumeClaudeSessionId: "" },
  ): Promise<string> =>
    new Promise<string>((resolve, reject) => {
      let created = false;
      let settled = false;
      const finish = (fn: () => void): void => {
        if (settled) return;
        settled = true;
        clearTimeout(timeout);
        bootWs.close();
        fn();
      };
      const bootWs = new WsClient({
        url: `${wsBase}/frontend`,
        onMessage: (data) => {
          let decoded;
          try {
            decoded = decodeFrontendFrame(data);
          } catch (err) {
            clog("warn", `bootstrap frame decode failed: ${String(err)}`);
            return;
          }
          bootDispatcher.observe(decoded);
          if (decoded.frame.case === "snapshot" && !created) {
            created = true;
            void bootDispatcher
              .createSession({
                cwd: args.cwd,
                model: "",
                permissionMode: "",
                configDir: "",
                resumeClaudeSessionId: args.resumeClaudeSessionId,
                fake: params.get("fake") === "1",
              })
              .then((id) => finish(() => resolve(id)))
              .catch((err: unknown) =>
                finish(() => reject(err instanceof Error ? err : new Error(String(err)))),
              );
          }
        },
        log: (message) => clog("warn", message),
      });
      // Its OWN dispatcher, bound to this socket: the live session's
      // dispatcher must not be re-pointed at a socket that is about to close
      // (a rebind runs this while the session socket is still the one every
      // other command rides).
      const bootDispatcher = new CommandDispatcher({
        send: (raw) => bootWs.send(raw),
        log: (level, message) => clog(level, message),
      });
      const timeout = setTimeout(
        () => finish(() => reject(new Error("create session: no daemon snapshot within 15s"))),
        15_000,
      );
      bootWs.connect();
    });

  if (activeSessionId === "") {
    activeSessionId = await createSessionViaWs();
    const url = new URL(location.href);
    url.searchParams.set("session", activeSessionId);
    history.replaceState(null, "", url.toString());
  }
  ws = makeClient(activeSessionId);
  ws.connect();

  if (composerEnabled(params)) {
    const input = must<HTMLTextAreaElement>("composer-input");
    const submit = (): void => {
      const text = input.value.trim();
      if (text === "") return;
      submitPrompt(text, "submit prompt");
      input.value = "";
    };
    must<HTMLButtonElement>("send-btn").addEventListener("click", submit);
    // Cycle the output feed without ever leaving the composer: the chords
    // are swallowed here, every other key still types. Armed before the
    // send handler so neither can shadow the other — they share no chord.
    installNavKeys(input, nav);
    input.addEventListener("keydown", (e) => {
      // The search gets first refusal, and the order is the whole point:
      // `RET` accepts a running search and must NOT also send the draft.
      // One handler asking in order, rather than two racing listeners, is
      // what makes that precedence a fact you can read here.
      if (search.handleKey(e)) return;
      if (e.key === "Enter" && !e.shiftKey) {
        e.preventDefault();
        submit();
      }
    });
  } else {
    // Host-owned input (Emacs hybrid UI): hide the composer entirely.
    must("composer").style.display = "none";
  }

  // Picking a mode ASKS for the switch; it does not assert it. There is no
  // standalone set-permission-mode command in frontend.v1 — the mode is a
  // `SubmitPromptCmd` field — so the pick is HELD and applied by the next
  // prompt, and the picker only settles when a pushed SessionView reports it.
  modeEl.addEventListener("change", () => {
    pendingMode.pick(modeEl.value as PermissionMode);
    clog("info", `permission mode "${modeEl.value}" will ride the next prompt`);
  });

  // GAP (flagged for the coordinator): frontend.v1 carries a model ONLY on
  // CreateSessionCmd, so there is no mid-session model switch to send, and
  // SessionView carries no model catalog (store.models is therefore always
  // empty, leaving this control with nothing to offer in the first place).
  // Refuse loudly and put the picker back on the model actually in force,
  // rather than leave it displaying one the session never adopted.
  modelEl.addEventListener("change", () => {
    clog(
      "warn",
      `model switch to "${modelEl.value}" not sent: frontend.v1 has no set-model command`,
    );
    modelEl.value = store.state.model;
  });

  // Which account this session runs as, plus the roster that names its root.
  // Refreshed after every login and switch, since those are the two things
  // that change the answer. The last-fetched pair seeds both the chip and the
  // menu's re-auth entry.
  let account: Account | null = null;
  let roster: RosterEntry[] = [];
  // Paint the chip as "email (mode)": the email is WHO, the parenthesized
  // roster label is WHICH root. With two roots logged into the same email the
  // mode is the only disambiguator, so it rides on the chip itself rather than
  // hiding in the menu. The mode is a dimmed, italic annotation (see the
  // `.account-mode` rule) and is dropped entirely when the roster does not
  // name the current root, leaving the bare email.
  const renderAccountChip = (): void => {
    accountEl.classList.toggle("logged-out", accountIsLoggedOut(account));
    const email = accountLabel(account);
    const mode = accountModeLabel(account, roster);
    if (mode === "") {
      accountEl.textContent = email;
      return;
    }
    const modeEl = document.createElement("span");
    modeEl.className = "account-mode";
    modeEl.textContent = `(${mode})`;
    accountEl.replaceChildren(document.createTextNode(`${email} `), modeEl);
  };
  const refreshAccount = (): Promise<void> =>
    Promise.all([
      fetchAccount(httpBase, activeSessionId),
      // The roster only annotates the chip with a mode label; a failure drops
      // that annotation but must not blank the account, so it degrades to the
      // last-known roster rather than rejecting the pair.
      fetchAccounts(httpBase).catch((err: unknown) => {
        clog("warn", `account roster lookup failed: ${String(err)}`);
        return roster;
      }),
    ])
      .then(([fetched, fetchedRoster]) => {
        account = fetched;
        roster = fetchedRoster;
        renderAccountChip();
      })
      .catch((err: unknown) => {
        // Not fatal: the session may still be perfectly usable. Leave the slot
        // blank rather than assert an account we failed to read.
        account = null;
        renderAccountChip();
        clog("warn", `account lookup failed: ${String(err)}`);
      });
  void refreshAccount();

  // Re-auth is the one account verb that does not talk to the SDK session.
  // The daemon runs the login on a pty it owns and streams it here, where it
  // renders as a real terminal: the flow is a full-screen TUI gated behind
  // stateful prompts before it ever reaches OAuth, so a human reads it and
  // nothing parses it. The chip is disabled for the round trip so a
  // double-click cannot ask for two terminals.
  //
  // The notice reuses #remediation, the topbar's one status-line slot.
  let terminal: LoginTerminal | null = null;

  const closeOverlay = (): void => {
    terminal?.dispose();
    terminal = null;
    loginOverlayEl.hidden = true;
    remediationEl.textContent = "";
    // A finished login may have changed the account, so this is where the
    // topbar learns the user's answer to it.
    void refreshAccount();
  };

  const openLoginTerminal = (): void => {
    accountEl.disabled = true;
    void requestLogin(httpBase, activeSessionId)
      .then((opened) => {
        remediationEl.textContent = loginNotice("open");
        // Name the account being logged INTO. With two in play, logging into
        // the wrong one would leave the real problem exactly where it was.
        loginAccountEl.textContent =
          opened.account === "" ? "default account" : opened.account;
        loginOverlayEl.hidden = false;
        terminal = attachLoginTerminal(loginTermEl, wsBase, activeSessionId, {
          onClosed: () => {
            remediationEl.textContent = loginNotice("closed");
          },
        });
      })
      .catch((err: unknown) => {
        // A login that never opened must say so: leaving the topbar silent
        // would send the user off to look for a terminal that is not coming.
        remediationEl.textContent = loginNotice("failed");
        clog("warn", `login request failed: ${String(err)}`);
      })
      .finally(() => {
        accountEl.disabled = false;
      });
  };

  // Switch never touches OAuth: the daemon migrates the transcript and
  // bounces the shim under the target root, keeping the session id, so the
  // stream reconnect below finds the same conversation. The chip is
  // disabled for the round trip — a second switch racing the first would
  // bounce a shim already mid-bounce.
  const doSwitch = (configDir: string): void => {
    accountEl.disabled = true;
    remediationEl.textContent = "switching account…";
    void switchAccount(httpBase, activeSessionId, configDir)
      .then((outcome) => {
        remediationEl.textContent = outcome.switched
          ? `switched to ${outcome.account.email === "" ? outcome.account.label : outcome.account.email}`
          : "";
        void refreshAccount();
      })
      .catch((err: unknown) => {
        // A switch that did not happen must say so: the user is about to
        // spend tokens as whichever account they BELIEVE is active.
        remediationEl.textContent = "account switch failed";
        clog("error", `account switch failed: ${String(err)}`);
      })
      .finally(() => {
        accountEl.disabled = false;
      });
  };

  // The chip's dropdown. Rebuilt on every open from a live read of BOTH the
  // roster AND the account this session runs as, so the switch entry the menu
  // filters out is decided by the CURRENT root rather than a cached one — a
  // just-completed switch the chip's `account` has not caught up to would
  // otherwise re-offer the very root the session moved to. A daemon without
  // -accounts (or an unreachable one) degrades to the re-auth entry alone,
  // built from the last account known.
  const hideMenu = (): void => {
    accountMenuEl.hidden = true;
  };
  const openMenu = async (): Promise<void> => {
    let entries: AccountMenuEntry[];
    try {
      const menu = await fetchAccountMenuEntries(httpBase, activeSessionId);
      // The fresh read the menu was filtered against is also the truest value
      // for the chip, so adopt it (account AND roster) and repaint rather than
      // leave the two out of step.
      account = menu.current;
      roster = menu.roster;
      renderAccountChip();
      entries = menu.entries;
    } catch (err: unknown) {
      // A failed live read must still leave re-auth reachable: fall back to
      // the last account known and a roster-less menu rather than an empty one.
      clog("warn", `account menu lookup failed: ${String(err)}`);
      entries = accountMenuEntries(account, []);
    }
    accountMenuEl.replaceChildren(
      ...entries.map((entry) => {
        const item = document.createElement("button");
        item.textContent = entry.text;
        item.addEventListener("click", () => {
          hideMenu();
          if (entry.kind === "reauth") {
            openLoginTerminal();
          } else {
            doSwitch(entry.configDir);
          }
        });
        return item;
      }),
    );
    accountMenuEl.style.left = `${accountEl.offsetLeft}px`;
    accountMenuEl.hidden = false;
  };
  accountEl.addEventListener("click", (e) => {
    e.stopPropagation();
    if (accountMenuEl.hidden) {
      void openMenu();
    } else {
      hideMenu();
    }
  });
  // Click-away dismissal, so an abandoned menu does not sit over the feed.
  document.addEventListener("click", (e) => {
    if (!accountMenuEl.hidden && !accountMenuEl.contains(e.target as Node)) {
      hideMenu();
    }
  });

  loginCloseEl.addEventListener("click", () => {
    // Kill the child too, not just the view. A login left running on a pty
    // nobody is reading is an orphaned OAuth flow.
    void closeLogin(httpBase, activeSessionId).catch((err: unknown) => {
      clog("error", `closing the login terminal failed: ${String(err)}`);
    });
    closeOverlay();
  });
}

boot().catch((err: unknown) => {
  const feed = document.getElementById("feed");
  if (feed) {
    feed.innerHTML = `<div class="error-banner">boot failed: ${String(err)}</div>`;
  }
});
