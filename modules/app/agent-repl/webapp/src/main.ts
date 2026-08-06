/**
 * Webapp bootstrap: session creation/join, WebSocket wiring, composer.
 *
 * URL parameters:
 *   ?daemon=host:port   daemon address (default: current host)
 *   ?workspace=<dir>    render this workspace (absolute path, URL-encoded)
 *   ?session=<id>       render this one session (else one is created)
 *   ?fake=1             create the session against the offline fake SDK
 *   ?parent_ws=<name>   parent workspace basename shown in the topbar
 *
 * `?workspace` and `?session` are the two page ADDRESSES; address.ts owns
 * reading them and the scoped daemon socket each one opens. The URL is read
 * only — nothing here ever writes an address back into it, so a page attaches
 * to the same thing for every reload, bookmark, restored tab and remount.
 */
import {
  TOPBAR_AGENT_ATTR,
  runningAgentClocks,
  sessionAccountingLabel,
  sessionTopbarDatapoints,
  topbarClickAction,
  topbarInfoHtml,
} from "./topbar.js";
import { addressLabel, pageAddress, scopedStreamUrl, type PageAddress } from "./address.js";
import { AgentClock } from "./agent-clock.js";
import { AGENTS_SPEC, sessionSubagents } from "./agents.js";
import { TASKS_SPEC } from "./tasks.js";
import {
  ProgressFooter,
  alreadyCompletePhaseViolation,
  footerClickAction,
} from "./progress-footer.js";
import {
  type CounterSpec,
  MISSING_BUBBLE_NOTICE_MS,
  missingBubbleNotice,
} from "./counter-menu.js";
import {
  mergeGateBlockedLog,
  mergeGateNoticeHtml,
  mergeGateSendTitle,
  submitBlocked,
} from "./merge-gate.js";
import {
  HIBERNATED_BODY_CLASS,
  REVIVE_COMPACT_ATTR,
  REVIVE_DIRECT_ATTR,
  hibernateRefusedNotice,
  hibernationBlocked,
  hibernationBlockedLog,
  hibernationNoticeHtml,
  hibernationSendTitle,
  REVIVE_FAILED_TEXT,
  ReviveWatch,
  revivalGateHtml,
  reviveFailedLog,
  reviveRefusedLog,
  type RevivePending,
} from "./hibernation.js";
import { mergeStatusLogValue } from "./merge-status.js";
import { PromptOrigin } from "./frontend-command.js";
import { configureChessGames, installChessNavHook } from "./chess-game.js";
import { RenderCoalescer, windowEagerHost, windowFrameHost } from "./coalesce.js";
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
import { escapeHtml } from "./highlight.js";
import {
  controlPlaneFailure,
  daemonReachableFailure,
  daemonUnreachableFailure,
  frameUndecodableFailure,
  sessionGoneFailure,
  staleBundleFailure,
} from "./local-failure.js";
import { StateAdapter, systemFailureFrom, userTurnReceipt } from "./state-adapter.js";
import { CommandDispatcher, ModelSelectionRejectedError } from "./command-dispatch.js";
import { ConnectResync } from "./connect-resync.js";
import { captureResyncSnapshot } from "./resync-snapshot.js";
import type { CommandStruct } from "./frontend-command.js";
import { PendingPermissionMode } from "./pending-mode.js";
import { ungatedBannerHtml, ungatedModeOf, unswitchableModeOptionHtml } from "./ungated.js";
import { DRAINING_BODY_CLASS, drainBannerHtml } from "./drain.js";
import { SessionRebase, claudeSessionIdOf } from "./session-rebase.js";
import { requestSupportWorkspace } from "./unsupported.js";
import { statusSnapshotFromInit } from "./status.js";
import { compactionBannerHtml, FeedRenderer, lastUserTurnId, modelOptionsHtml } from "./render.js";
import { installEdgeScroll, isPinnedToBottom, parkAtTail } from "./scroll.js";
import { FeedSearch, type SearchHost, installSearchHook } from "./search.js";
import {
  WorkspaceSidebar,
  installWorkspaceExpandHook,
  workspaceStatusFromRenderState,
} from "./sidebar.js";
import { ConversationStore } from "./store.js";
import { IDLE_LABEL, TIMER_SLOT, TaskTimer, windowHost } from "./timer.js";
import { VersionSkewGuard } from "./version-skew.js";
import { WsClient, composerEnabled, makeSessionExistsProbe, type WsStateFreshness } from "./ws.js";
import {
  bindLogContext,
  ForwardingLogger,
  log,
  logVerbose,
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

  // What this page renders (address.ts). A malformed address throws here,
  // before anything connects, rather than surfacing as a failed handshake.
  const address = pageAddress(params);

  // The session the page's HTTP side-calls target (account, task tail, chess
  // payloads, login). It is INTERNAL STATE, never an address: an unaddressed
  // page creates it over the WS (CreateSessionCmd) once the command dispatcher
  // exists, and a workspace-addressed page learns it from the SessionView plane
  // the daemon pushes — the daemon rules on which session a workspace owns, and
  // a rotation there re-reads through the same channel.
  let activeSessionId: string = address.kind === "session" ? address.sessionId : "";
  let ws: WsClient;

  // Resume/rebind tracking, re-fed by the SessionView plane now that the
  // frontend.v1 cutover routes it through the adapter. It rules on every
  // announced vendor session uuid: the first is ADOPTED (persist the resume
  // keys), a changed one is a ROTATION — the conversation retired its store seq
  // space, and this end rebases onto the new one (see session-rebase.ts).
  const sessionRebase = new SessionRebase({ log: (level, message) => clog(level, message) });

  // Delivery-path diagnostics (§2.15). The webapp→daemon log forward rode the
  // legacy `client-log` ClientCommand, which the S8/S9 outbound cutover
  // deleted; E4's additive `client_log` FrontendCommand arm restores it on the
  // protobuf channel rather than reviving a second transport.
  //
  // The sink is assigned after the dispatcher exists. Logger bootstrap happens
  // first, so every normal runtime emission has the canonical delivery path.
  let clientLogSink:
    | ((level: ClientLogLevel, message: string, context?: ClientLogContext) => boolean)
    | null = null;
  const wslog = new ForwardingLogger(
    (cmd) => {
      // Startup and reconnect legitimately precede an open command socket.
      // Returning false keeps the record in ForwardingLogger's bounded queue.
      if (clientLogSink === null) return false;
      return clientLogSink(cmd.level, cmd.message, cmd.context);
    },
  );
  // A page instance exists before the first socket. Binding its initial
  // generation now guarantees even bootstrap diagnostics meet the webapp
  // record identity contract; each session socket advances the generation.
  const pageLogInstance = crypto.randomUUID();
  const loggerFor = (operation: string) =>
    (level: ClientLogLevel, message: string, context: ClientLogContext = {}): void =>
      log(level, message, { operation, context });
  const clog = loggerFor("webapp.main");
  const storeLog = (level: ClientLogLevel, message: string, context: ClientLogContext = {}, verbose = false): void => {
    const { operation = "conversation-store", ...evidence } = context;
    if (typeof operation !== "string") throw new Error("conversation-store log operation must be a string");
    (verbose ? logVerbose : log)(level, message, { operation, context: evidence });
  };
  const adapterLog = loggerFor("state-adapter");
  // Deep modules (render walk, pollers) log through the module-level
  // singleton; install the real forwarder before anything renders.
  setLogger(wslog);
  bindLogContext({
    connection_id: `${pageLogInstance}:0`,
    ...(activeSessionId !== "" ? { agent_repl_session_id: activeSessionId } : {}),
  });

  const store = new ConversationStore(storeLog);
  // The one-change cutover seam: the daemon pushes `agentshim.frontend.v1`
  // protojson frames, which decode (frontend-proto.ts) into effects
  // (state-adapter.ts) the store ingests. The adapter's explicit-ignore path
  // logs once per unsupported shape at debug — mapped to `info` here since the
  // client-log channel has no debug level.
  const adapter = new StateAdapter((level, message) =>
    adapterLog(level === "debug" ? "info" : level, message),
  );
  // The frontend→daemon command plane (§task 4): every outbound command is a
  // FrontendCommand protojson frame over the current socket (read lazily, like
  // wslog, so startup construction order does not capture an unset client). The
  // dispatcher is fed every inbound decoded frame (`observe`) so it can
  // correlate CommandAcks by requestId and a createSession's pushed SessionView.
  const dispatcher = new CommandDispatcher({
    send: (raw) => (ws as WsClient | undefined)?.send(raw) ?? false,
    // A REJECTED clientLog cannot be reported through `log`: that forwards
    // another clientLog, earns another rejection, and loops. It still has to be
    // SEEN, so it goes to the logger's local-only path — the same injected
    // console sink every other line uses, at error level because a forward that
    // the daemon refused is a real failure of the diagnostics channel, not a
    // quiet failure. The forward itself still happened and still failed loudly.
    logLocal: (message) => log("error", message, { operation: "webapp.client-log-rejected", localOnly: true }),
    // A refused command lands as a failure card IN THE FEED (F4). Before
    // this it reached a human through nothing at all: the ack's text went to
    // a local log and the promise it rejected was swallowed by every caller,
    // so a rejected prompt looked exactly like a prompt that was never sent.
    onFailure: (failure) => {
      if (store.addFailure(systemFailureFrom(failure))) frames.schedule();
    },
  });
  // The workspace a runtime command names — the live session's cwd, as the
  // pushed `SessionView` reports it. The daemon stamps the URL-scoped
  // workspace onto any command that omits one (see frontendCommandTranslator),
  // so this is advisory on the session socket and is legitimately "" until the
  // first SessionView lands.
  const cmdWorkspace = (): string => store.state.cwd;
  // Both resync senders MUST capture the same revisioned WorkspaceState facts
  // at their dispatch edge. A later state push cannot rewrite an already-built
  // command, so the daemon can classify a delayed old-client request instead
  // of replaying against whichever controller replaced it.
  const currentResyncSnapshot = (fromSeq: number) => captureResyncSnapshot(store.state, fromSeq);

  // The conversation-history request. `StateSnapshot` carries no conversation,
  // and the deltas that carry it were pushed before this page existed, so a
  // fresh mount asks for them: one `resync(workspace, lastSeq)` per connection,
  // fired once the snapshot has landed and a workspace is known. See
  // connect-resync.ts for why it must be exactly once per socket.
  const connectResync = new ConnectResync({
    resync: (snapshot) => dispatcher.resync(snapshot.workspace, snapshot),
    log: (level, message) => clog(level, message),
  });

  // Close the diagnostics loop declared above: from here every forwarded line
  // rides the protobuf command plane into the daemon's log.
  clientLogSink = (level, message, context) =>
    dispatcher.clientLog(cmdWorkspace(), level, message, context as CommandStruct | undefined);
  // CommandDispatcher owns and canonically logs every command rejection before
  // rejecting its Promise, AND surfaces every rejection shape through
  // `onFailure` above — a classified failure, a bare error string, or a send
  // the socket refused all land as a card in the feed. UI call sites consume
  // that already-owned rejection without writing a duplicate record.
  //
  // NOT used by hibernate and revive: those two have UI state of their own to
  // unwind (the gate) and a place the user is already looking (the gate card,
  // the topbar status line), so they carry real handlers.
  const consumeOwnedDispatchFailure = (_err: unknown): void => {};

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
  const submitPrompt = (text: string, promptOrigin: PromptOrigin): void => {
    void dispatcher
      .submitPrompt(cmdWorkspace(), text, promptOrigin, pendingMode.outbound)
      .catch(consumeOwnedDispatchFailure);
  };
  const feedEl = must("feed");
  // RETIRED (F4): the degraded-state banner. It was chrome that scrolled
  // away, carried no correlation between a report and its all-clear, and
  // showed a raw component/reason pair the daemon had already classified.
  // Degradation now arrives as a self-resolving failure card in the feed,
  // where a user whose workspace changed color can actually find it.
  // The search's echo area: isearch keeps its query out of the text being
  // searched, and so does this — the composer's draft stays untouched while
  // a search runs, and the query shows up here instead.
  const searchStatusEl = must("search-status");
  // Chess-game bubbles fetch their payload through the daemon and mount
  // the in-place-served widget; the session getter observes startup creation, and
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
  // No roster hook is planted: the rail's only ingress is the roster FRAME off
  // the websocket (WorkspaceSidebar.adoptRosterFrame), which is the sole path
  // that can carry the revision the gate ranks by.
  //
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
        .catch(consumeOwnedDispatchFailure);
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
        .catch(consumeOwnedDispatchFailure);
    },
    // Held-prompt queue controls (E4). These were loud no-ops between the
    // cutover and the queue's return; they now drive the real command arms.
    cancelQueued: (entryId) => {
      void dispatcher
        .queueCancel(cmdWorkspace(), entryId)
        .catch(consumeOwnedDispatchFailure);
    },
    runQueuedNow: (entryId) => {
      void dispatcher
        .queueForce(cmdWorkspace(), entryId)
        .catch(consumeOwnedDispatchFailure);
    },
    acceptQueued: (entryId) => {
      void dispatcher
        .queueAccept(cmdWorkspace(), entryId)
        .catch(consumeOwnedDispatchFailure);
    },
    sendPrompt: (text) => {
      // Card controls (stop task) are prompt-mediated: the button sends an
      // ordinary user message through the same command the composer uses.
      submitPrompt(text, PromptOrigin.WEBAPP_CARD_ACTION);
    },
    // Watcher folds poll this while open (§ watcher-bubble expansion),
    // targeting the current session.
    fetchTaskTail: (taskId, offset) => fetchTaskTail(httpBase, activeSessionId, taskId, offset),
    // The unsupported-command card's button. Targets the CURRENT session
    // so the workspace opens against the checkout in view, and
    // resolves to the workspace name Emacs was asked for — Emacs, not the
    // daemon, decides what actually happens next.
    addSupport: (command) => requestSupportWorkspace(httpBase, activeSessionId, command),
    // The `/status` panel's data. The snapshot half is re-sourced from the
    // session's PUSHED SystemInit (no round trip, and never staler than the
    // daemon's own view, which is why the old GET /status and its
    // /status/refresh re-probe are both gone). Only the account half is
    // fetched, on the sanctioned account endpoint, targeting the CURRENT
    // session so the account belongs to the checkout in view.
    getStatus: () =>
      fetchAccount(httpBase, activeSessionId).then((account) => ({
        snapshot: statusSnapshotFromInit(store.state.systemInit),
        account,
      })),
  });
  // The feed defers the heavy render of replayed history it has not scrolled
  // to (lazy-item.ts), and a placeholder carries less than the item it stands
  // for. A starting search therefore drains that first, so its walk covers the
  // same DOM it would have before deferral existed rather than only whatever
  // the reader happens to have scrolled past.
  search.setPrepare(() => feed.upgradeAll());

  // THE consolidated progress footer (F1): the raised dock between the feed
  // and the composer that replaced every scattered in-flight indicator. Its
  // whole input is the daemon's resolved ProgressView, so nothing here derives
  // a progress fact.
  const footerEl = must("progress-footer");
  const footer = new ProgressFooter(footerEl);

  const statusEl = must("conn-status");
  const infoEl = must("session-info");
  const summaryEl = must("task-summary");
  const modeEl = must<HTMLSelectElement>("mode-select");
  const modelEl = must<HTMLSelectElement>("model-select");
  const spinnerEl = must("spinner");
  const compactBarEl = must("compact-progress-slot");
  const ungatedBannerEl = must("ungated-banner");
  const drainBannerEl = must("drain-banner");
  const revivalGateEl = must("revival-gate");
  const hibernateEl = must<HTMLButtonElement>("hibernate-btn");
  /**
   * The revival decision this page has SENT and the daemon has not yet
   * answered, or null.
   *
   * It is browser-local view state and nothing else: the authority on whether
   * the session is awake is the pushed `SessionView`, and this only keeps the
   * gate from re-offering two buttons while one of them is in flight. It clears
   * on the ack's rejection (the decision did not happen), on the pushed view
   * that drops the hibernation field (it did), and on a pushed view that STILL
   * carries hibernation after an accepted decision (the bring-up failed) — the
   * exit `reviveWatch` below exists to give it.
   */
  let revivePending: RevivePending = null;
  /**
   * The one-shot expectation an ACCEPTED revival ack arms (hibernation.ts).
   *
   * The ack only means the daemon took the decision; the bring-up follows. Its
   * failure used to have no signal at all — the gate sat on "Waking the
   * session…" with both buttons gone, forever. The next pushed `SessionView`
   * for this workspace now settles it either way.
   */
  const reviveWatch = new ReviveWatch();
  /**
   * The gate's failure line, or "" — set when an accepted decision left the
   * session asleep, and cleared the moment another decision is sent, because a
   * stale complaint beside a fresh "waking…" line would describe the wrong
   * attempt.
   */
  let reviveFailure = "";
  // The composer's own elements, resolved HERE rather than in the wiring block
  // below because `renderChrome` repaints the merge gate on them every frame.
  // Null when the host owns input (Emacs's `composer=0`), which is also the one
  // case where there is no gate to paint.
  const composerEls = composerEnabled(params)
    ? {
        input: must<HTMLTextAreaElement>("composer-input"),
        send: must<HTMLButtonElement>("send-btn"),
        notice: must("merge-gate-notice"),
      }
    : null;
  // The picker's own vocabulary, captured BEFORE any live-mode option is
  // appended, so an ungated session's disabled marker never gets mistaken for
  // a switchable mode on the next frame.
  const switchableModes = Array.from(modeEl.options, (o) => o.value);
  const baseModeOptions = modeEl.innerHTML;
  const remediationEl = must("remediation");
  const accountEl = must<HTMLButtonElement>("account");
  const accountMenuEl = must("account-menu");
  const loginOverlayEl = must("login-overlay");
  const loginAccountEl = must("login-account");
  const loginTermEl = must("login-term");
  const loginCloseEl = must<HTMLButtonElement>("login-close");
  const parentWs = params.get("parent_ws");

  // Whether the topbar's tokens breakdown is open. It lives HERE rather than
  // in the DOM because renderChrome rewrites the whole topbar on every frame,
  // which would otherwise collapse an overlay the user is reading mid-turn.
  // The roster overlays are the FOOTER's disclosure now (see `footer`), which
  // owns its own for exactly the same reason.
  let tokensMenuOpen = false;

  // The running turn's timer paints the footer's clock cell. Its tick writes
  // just that one span rather than re-rendering the dock — and emphatically not
  // the FEED, which is what the nuked stats row's ancestor once cost. The
  // footer skips the write when no span is mounted (before the first
  // ProgressView, or during a replay-only paint), and bakes the last reading
  // into every render so a fresh dock never blinks empty.
  const timer = new TaskTimer(windowHost(window), (label) => {
    timerLabel = label;
    footer.paintTurnTimer(label);
  });
  // The tick's latest reading, so the next footer render bakes it in.
  let timerLabel = IDLE_LABEL;

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

  let lastFooterStateSignature = "";
  const renderChrome = (): void => {
    const s = store.state;
    // topbarInfoHtml escapes every value it interpolates. The same strip
    // renderer draws the agent-scoped bubble topbars (see topbar.ts).
    infoEl.innerHTML = topbarInfoHtml(sessionTopbarDatapoints(s, parentWs), {
      // The header strip carries NO roster chips any more (they relocated into
      // the footer's counters cluster), so only the tokens disclosure can open.
      agentsOpen: false,
      tasksOpen: false,
      tokensOpen: tokensMenuOpen,
    });
    const accounting = sessionAccountingLabel(s);
    if (accounting !== null) {
      infoEl.innerHTML += `<span class="session-accounting">${escapeHtml(accounting)}</span>`;
    }
    // The idle-with-live-async signal breathes as the sidebar's amber dot on
    // this session's own row rather than as strip text. The flag is the feed
    // renderer's own gate reading (idle + live async), read back here so the
    // rail mirrors the feed the last render already partitioned.
    sidebar.setMonitoring(feed.isMonitoring());
    // THE RAIL LEADS ON MERGES, so it gets the structured status too: the
    // recycle glyph said only "a merge is happening", identically for a run on
    // its last commit and a run parked on a conflict.
    sidebar.setMergeStatus(s.mergeStatus);
    // THE progress footer. Rendered on the chrome cadence (not the feed's), so
    // a dock rewrite never rides a feed reconcile. It reads the daemon's
    // resolved view plus the two rosters and the feed items the activity cell
    // needs; it derives no progress fact of its own.
    footer.render({
      progress: store.progress,
      // THE phase, read off the workspace's one authoritative state rather
      // than off a copy carried in a second message (F5).
      renderState: s.renderState,
      // THE structured status, on the same revisioned message as the phase
      // above and the only merge input the footer takes.
      mergeStatus: s.mergeStatus,
      agents: sessionSubagents(s.items),
      tasks: store.taskRoster,
      items: s.items,
      timerLabel,
    });
    const interruptOutcome = store.progress?.interrupt?.outcome ?? "none";
    const footerStateSignature =
      `${s.renderState ?? "none"}|${s.sessionConnectivity ?? "none"}|` +
      `${s.sessionStatus ?? "none"}|${interruptOutcome}|${s.mergeLeaseHeld}|` +
      mergeStatusLogValue(s.mergeStatus);
    if (footerStateSignature !== lastFooterStateSignature) {
      clog(
        "info",
        `footer state rendered phase=${s.renderState ?? "none"} ` +
          `connectivity=${s.sessionConnectivity ?? "none"} ` +
          `status=${s.sessionStatus ?? "none"} ` +
          `generation=${s.controllerGenerationId || "none"} ` +
          `faults=${s.activeFaults.map((fault) => `${fault.component}/${fault.faultType}`).join(",") || "none"} ` +
          `interrupt_outcome=${interruptOutcome} ` +
          `merge_lease_held=${s.mergeLeaseHeld} ` +
          `merge_status=${mergeStatusLogValue(s.mergeStatus)} session=${s.sessionId}`,
        {
          phase: s.renderState ?? "none",
          connectivity: s.sessionConnectivity ?? "none",
          session_status: s.sessionStatus ?? "none",
          controller_generation_id: s.controllerGenerationId || "none",
          active_faults: s.activeFaults.map((fault) => `${fault.component}/${fault.faultType}`),
          interrupt_outcome: interruptOutcome,
          merge_lease_held: s.mergeLeaseHeld,
          merge_status: mergeStatusLogValue(s.mergeStatus),
          session_id: s.sessionId,
        },
      );
      lastFooterStateSignature = footerStateSignature;
    }
    const activeViolation = alreadyCompletePhaseViolation(
      s.renderState,
      store.progress?.interrupt?.outcome ?? null,
    );
    if (activeViolation !== null) {
      clog(
        "error",
        `INVARIANT VIOLATION: footer rendered already_complete beside active phase=${activeViolation} ` +
          `session=${s.sessionId}`,
        {
          phase: activeViolation,
          interrupt_outcome: interruptOutcome,
          session_id: s.sessionId,
        },
      );
    }
    // After the footer exists, so the paint on a starting turn has a span to
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
    // The picker offers only SWITCHABLE modes, so a launch-only mode
    // (bypassPermissions) matches nothing and would render the select BLANK —
    // an ungated session looking like the most ordinary thing on screen. Carry
    // the live mode as a disabled option instead, so the picker always names
    // what is actually running.
    const liveOption = unswitchableModeOptionHtml(switchableModes, wantMode);
    const nextModeOptions = baseModeOptions + liveOption;
    if (modeEl.innerHTML !== nextModeOptions) modeEl.innerHTML = nextModeOptions;
    if (modeEl.value !== wantMode) modeEl.value = wantMode;
    // THE ungated-session surface: a session whose mode shadows canUseTool in
    // the fail-open direction has no daemon permission gate at all, so it gets
    // a permanent banner plus a document-wide marker the chrome paints against
    // (styles.css `.ungated`). Both are pure functions of the live mode — there
    // is nothing to dismiss while the mode is in force.
    //
    // Read off `s.permissionMode`, NOT the settled `wantMode`: a pending pick
    // is a UI intent, and a warning that a click can clear before the daemon
    // has honored anything is a warning that can be clicked away.
    const ungatedMode = ungatedModeOf({
      requestedMode: s.permissionMode,
      systemInit: s.systemInit,
    });
    ungatedBannerEl.innerHTML = ungatedBannerHtml(ungatedMode);
    document.body.classList.toggle("ungated", ungatedMode !== "");
    // THE DRAIN LEASE (drain.ts): a daemon-global banner, repainted on the
    // chrome cadence so its elapsed clock advances with every frame and so a
    // cancelled or completed drain takes it down the moment the daemon says
    // `idle`. Read straight off the adopted lease — the webapp derives no
    // drain fact of its own.
    drainBannerEl.innerHTML = drainBannerHtml(s.shutdownSchedule, Date.now());
    document.body.classList.toggle(DRAINING_BODY_CLASS, s.shutdownSchedule !== null);
    // THE MERGE GATE (merge-gate.ts). Both halves are pure functions of the
    // revisioned `WorkspaceState` lease, repainted on the chrome cadence, so
    // the composer un-gates the moment the merge releases without any local
    // state to unwind. Skipped entirely when the host owns input (Emacs runs
    // the webview with composer=0 and there are no controls to gate).
    // THE REVIVAL GATE (hibernation.ts). Same discipline as the two banners
    // above: a pure function of the daemon's live state, repainted every chrome
    // frame, so it goes up and comes down with the pushed SessionView and there
    // is no local lifetime to unwind. The in-flight decision clears itself here
    // the moment the daemon reports the session awake — the ONE authority on
    // that — so a revive that landed can never leave a stale "waking…" line.
    if (s.hibernation === null) {
      revivePending = null;
      // An awake session has no failed revival to complain about, and the gate
      // it would have been drawn in is gone.
      reviveFailure = "";
    }
    revivalGateEl.innerHTML = revivalGateHtml(
      s.hibernation,
      revivePending,
      Date.now(),
      reviveFailure,
    );
    document.body.classList.toggle(HIBERNATED_BODY_CLASS, s.hibernation !== null);
    // The sleep verb is offered only on an awake session: there is nothing to
    // hibernate on one already asleep, and the gate above is what that session
    // is asking for instead.
    hibernateEl.hidden = s.hibernation !== null;
    if (composerEls !== null) {
      // TWO INDEPENDENT GATES, and the composer is blocked by EITHER. They are
      // separate facts with separate causes (a merge owns the shim; the session
      // has no shim at all), so neither is folded into the other — but the
      // hibernation notice wins the shared slot when both stand, because a
      // sleeping session cannot be prompted even once the merge releases.
      const mergeHeld = submitBlocked(s.mergeLeaseHeld);
      const asleep = hibernationBlocked(s.hibernation);
      composerEls.notice.innerHTML = asleep
        ? hibernationNoticeHtml(s.hibernation)
        : mergeGateNoticeHtml(s.mergeLeaseHeld, s.mergeStatus);
      composerEls.send.disabled = mergeHeld || asleep;
      composerEls.send.title = asleep
        ? hibernationSendTitle(s.hibernation)
        : mergeGateSendTitle(s.mergeLeaseHeld, s.mergeStatus);
    }
    spinnerEl.classList.toggle("on", s.turnInFlight);
    // The centered "current objective" label (§2.14): textContent (not
    // innerHTML) so the daemon's summary is inert text, and the full line
    // rides in the tooltip since the strip ellipsis-clips it. Empty until
    // the first completed turn produces one, which collapses the element.
    const summary = s.taskSummary ?? "";
    summaryEl.textContent = summary;
    summaryEl.title = summary;
    // Empty string when no compaction runs, which collapses the slot. The
    // window is read straight off the retained `ProgressView` — the same
    // authority the footer's `compacting…` row reads — rather than off a
    // store copy of it that could disagree with the row beside it.
    compactBarEl.innerHTML = compactionBannerHtml(store.progress?.compacting != null);
    document.title = s.model ? `claude-repl · ${s.model}` : "claude-repl";
  };

  // The chip is re-created by every renderChrome, so the toggle is delegated
  // off the topbar rather than bound to a node that will not survive the turn.
  // The click vocabulary is the strip's own (topbarClickAction), shared with
  // the agent bubbles' delegation in the FeedRenderer — the HEADER strip now
  // only ever sees the tokens toggle, since its roster chips relocated into
  // the progress footer.
  infoEl.addEventListener("click", (e) => {
    const action = topbarClickAction(e.target as HTMLElement);
    if (action?.kind !== "toggle" || action.menu !== "tokens") return;
    tokensMenuOpen = !tokensMenuOpen;
    renderChrome();
  });

  // THE REVIVAL GATE's two verbs. Delegated off the slot rather than bound to
  // the buttons, which every renderChrome rewrites.
  //
  // The pending mark is set BEFORE the send and cleared on a rejection, so the
  // gate reports "waking…" only while a decision is genuinely outstanding. It
  // is NOT cleared on success: success is the daemon dropping the hibernation
  // field on a pushed SessionView, and renderChrome clears it there. Resolving
  // the promise means the daemon ACCEPTED the decision, not that the session is
  // up — the bring-up follows, and taking the gate down on the ack would put a
  // live composer in front of a session that has no shim yet.
  const sendRevive = (mode: "compactFirst" | "direct"): void => {
    const workspace = cmdWorkspace();
    revivePending = mode;
    // A previous attempt's complaint is not this attempt's news.
    reviveFailure = "";
    renderChrome();
    void dispatcher.reviveSession(workspace, mode).then(() => {
      // ACCEPTED, which means the decision was taken and NOT that the session
      // is up. Arm the one-shot expectation: the next pushed SessionView for
      // this workspace either drops the hibernation field (the bring-up
      // landed) or still carries it (the bring-up failed, and the gate must
      // come back). Bounded by a wire fact, with no timer anywhere.
      reviveWatch.arm(workspace, mode);
    }).catch((err: unknown) => {
      // The dispatcher owns the canonical rejection record and the failure
      // card; this unwinds the view state, because a refused decision leaves
      // the session exactly as asleep as it was and the user has to be able to
      // choose again — and says so on the workspace's own log, which is where
      // a gate that came back up is explained.
      revivePending = null;
      // A REJECTED decision never reached the revival path, so no view is
      // owed a verdict on it.
      reviveWatch.disarm();
      renderChrome();
      clog("error", reviveRefusedLog(mode, err));
    });
  };
  revivalGateEl.addEventListener("click", (e) => {
    const el = (e.target as HTMLElement).closest(
      `[${REVIVE_COMPACT_ATTR}], [${REVIVE_DIRECT_ATTR}]`,
    );
    if (el === null) return;
    sendRevive(el.hasAttribute(REVIVE_COMPACT_ATTR) ? "compactFirst" : "direct");
  });

  // The reveal half of a roster-row click: dismiss the roster either way so
  // a revealed card is unobscured, and when the entry's bubble was NOT
  // found, say so in #remediation (the topbar's one status-line slot) for a
  // few seconds instead of silently doing nothing. The timed clear checks
  // the slot still shows THIS notice, so it never wipes a login or
  // remediation notice that landed meanwhile.
  const notify = (notice: string): void => {
    remediationEl.textContent = notice;
    window.setTimeout(() => {
      if (remediationEl.textContent === notice) remediationEl.textContent = "";
    }, MISSING_BUBBLE_NOTICE_MS);
  };

  // The session-level sleep verb. It ASKS; the daemon refuses while a turn is
  // live or the merge lease is held, and that nack rides the ordinary
  // CommandAck failure path into a classified card — so nothing here pre-judges
  // settledness, which this end cannot resolve.
  //
  // The refusal is ALSO said in the topbar's status line: the button hides
  // itself the moment the session sleeps, so a refusal rendered only in the
  // feed would leave the click looking like it did nothing at all. Bound below
  // `notify` because that is the slot it writes to.
  hibernateEl.addEventListener("click", () => {
    void dispatcher.hibernateWorkspace(cmdWorkspace()).catch((err: unknown) => {
      notify(hibernateRefusedNotice(err));
    });
  });

  // The footer's own delegation. Its dock is rewritten by every renderChrome,
  // so the verbs are delegated off the slot rather than bound to nodes that
  // will not survive the turn. Its vocabulary is the footer's own
  // (footerClickAction), which checks the roster verbs BEFORE the strip's
  // expansion toggle — a counter chip lives INSIDE the clickable strip.
  const setFooterMenu = (menu: "agents" | "tasks" | null): void => {
    footer.setMenu(menu);
    renderChrome();
  };
  const settleFooterReveal = (spec: CounterSpec, revealed: boolean): void => {
    setFooterMenu(null);
    if (!revealed) notify(missingBubbleNotice(spec));
  };
  footerEl.addEventListener("click", (e) => {
    const action = footerClickAction(e.target as HTMLElement);
    if (!action) return;
    switch (action.kind) {
      case "toggle-menu": {
        const open = footer.disclosure();
        const current = open.agentsOpen ? "agents" : open.tasksOpen ? "tasks" : null;
        setFooterMenu(current === action.menu ? null : action.menu);
        return;
      }
      case "reveal-agent":
        settleFooterReveal(AGENTS_SPEC, feed.revealAgent(action.agentId));
        return;
      case "reveal-task":
        settleFooterReveal(TASKS_SPEC, feed.revealTask(action.taskId));
        return;
      case "reveal-error":
        // The error row's whole purpose: an error that has already scrolled
        // away is otherwise unfindable. A miss is reported in the topbar's
        // status slot rather than silently doing nothing.
        if (!feed.revealError(action.uuid)) {
          notify("the error's line is not in the current feed");
        }
        return;
      case "toggle-expand":
        footer.toggleExpanded();
        renderChrome();
        return;
    }
  });

  // Every open overlay closes the way every dropdown does: click off it, or
  // Escape. Three surfaces now hold one each — the header's tokens breakdown,
  // the footer's relocated rosters, and the agent bubbles' own strips — so all
  // three dismiss together on the same gestures.
  const closeAllMenus = (): void => {
    tokensMenuOpen = false;
    footer.closeMenus();
    feed.closeAgentMenus();
    renderChrome();
  };
  document.addEventListener("click", (e) => {
    const target = e.target as HTMLElement;
    if (
      !target.closest(".agents-menu") &&
      !target.closest(".tasks-menu") &&
      !target.closest(".tokens-menu")
    ) {
      closeAllMenus();
    }
  });
  document.addEventListener("keydown", (e) => {
    if (e.key === "Escape") closeAllMenus();
  });
  // The composer is a separate Emacs window the outside-click handler above
  // cannot see, so the host fires this hook when the user clicks into it —
  // closing the header, footer and bubble dropdowns the same way a click-away
  // would.
  installHostCloseMenusHook(window as unknown as HostGlobal, closeAllMenus);

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

  // Renders are coalesced: a burst of ingested effects — a reconnect's
  // snapshot, a fast delta stream — otherwise runs a full feed render per
  // effect, and that churn is the switch-in jitter. One render drains
  // however many effects landed since it was scheduled. While the page is
  // VISIBLE that ride is an animation frame, as it has always been; while
  // it is HIDDEN, rAF does not run at all under WKWebView, so the ride is
  // an eager timer instead — otherwise a background workspace banks the
  // whole interval's renders and pays for them at the moment it is
  // switched back to.
  const frames = new RenderCoalescer(
    windowFrameHost(window),
    () => {
      rerender();
    },
    {
      isHidden: () => document.visibilityState === "hidden",
      eagerHost: windowEagerHost(window),
      // Stall watchdog: a webview whose WebKit process wrongly believes
      // it is occluded (xwidget reparenting) suspends rAF while CSS
      // animations keep breathing — the feed freezes with no error
      // anywhere. A genuinely hidden page no longer reaches this path at
      // the frame threshold, because it no longer schedules on rAF; the
      // eager ride keeps its own, slacker threshold so that a hidden host
      // which has stopped running timers entirely still reports.
      now: () => performance.now(),
      onStall: (ms, kind) =>
        clog(
          "warn",
          `render stall: ${kind === "eager" ? "hidden-page timer" : "rAF"} pending ${Math.round(ms)}ms while frames keep arriving (visibility=${document.visibilityState} focus=${document.hasFocus()})`,
          // The same facts as fields, so the daemon's log can be read back by
          // something other than a human eye — this is the stall investigation's
          // primary evidence, and grepping a sentence for a number is not a plan.
          {
            pendingMs: Math.round(ms),
            scheduler: kind,
            visibility: document.visibilityState,
            focus: document.hasFocus(),
          },
        ),
      onStallRecover: (ms, kind) =>
        clog("warn", `render stall recovered after ${Math.round(ms)}ms`, {
          stalledMs: Math.round(ms),
          scheduler: kind,
        }),
    },
  );
  // VERSION SKEW (see version-skew.ts). This bundle is loaded once into a
  // long-lived xwidget webview and outlives daemon redeploys; when the code it
  // is running can no longer ingest the daemon's frames, no amount of
  // reconnecting fixes it and the page must fetch itself again.
  const versionSkew = new VersionSkewGuard({
    reload: () => location.reload(),
    storage: sessionStorage,
    onReloadRefused: (detail) => {
      // The fresh bundle cannot adopt either, so this is a real defect that
      // must be visible rather than hidden behind more reload churn.
      if (store.addFailure(staleBundleFailure(detail))) frames.schedule();
      statusEl.textContent = "state unreadable";
      statusEl.classList.remove("ok");
    },
    log: (level, message) => clog(level, message),
  });

  // The wake anchors' countdowns tick on wall-clock time, not on frames,
  // so nothing would re-render them between deltas. A slow heartbeat
  // ask keeps them honest through the same coalescer every other render
  // rides; reconciliation no-ops every node whose HTML did not change.
  window.setInterval(() => frames.schedule(), 30_000);

  // REPAINT ON SHOW. Whatever is on screen is at best the last hidden tick's
  // render while the store behind it has moved on — the socket keeps
  // delivering either way. Becoming visible therefore repaints from the
  // CURRENT snapshot rather than waiting for the next arriving frame.
  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "hidden") return;
    // The rail's roster is PUSHED by Emacs rather than streamed, so it is the
    // one surface the feed render would not refresh on its own.
    sidebar.repaint();
    // A pending render is flushed synchronously so the FIRST visible frame is
    // current; with nothing pending, an ordinary scheduled render picks up the
    // repainted rail on the next frame.
    if (!frames.flush()) frames.schedule();
  });

  let logConnectionGeneration = 0;
  // The live socket, opened against the page's address. A workspace-addressed
  // connection has no session id of its own; `activeSessionId` is whatever the
  // pushed plane has said so far, and it is the log attribution rather than the
  // routing key.
  const makeClient = (connectionAddress: PageAddress): WsClient => {
    logConnectionGeneration++;
    bindLogContext({
      agent_repl_session_id: activeSessionId,
      connection_id: `${pageLogInstance}:${logConnectionGeneration}`,
      ...(connectionAddress.kind === "workspace" ? { workspace: connectionAddress.workspace } : {}),
    });
    const client = new WsClient({
      url: scopedStreamUrl(wsBase, connectionAddress),
      onMessage: (data) => {
        // The one path: decode the protojson `frontend.v1` frame, let the
        // command dispatcher correlate its CommandAcks + a createSession's
        // pushed SessionView, map it to typed adapter effects, then ingest
        // them into the store. A
        // malformed/unknown frame hard-errors (the decoder and adapter never
        // degrade) — evidence first, then re-throw, as the old raw path did.
        let effects;
        // Whether THIS frame was the connect StateSnapshot — the signal the
        // history request waits for (it carries no conversation of its own).
        let isSnapshot = false;
        // The daemon PROCESS identity this snapshot came from, which the
        // version-skew guard compares against the one this page pinned.
        let daemonBootId = "";
        try {
          const decoded = decodeFrontendFrame(data);
          isSnapshot = decoded.frame.case === "snapshot";
          if (decoded.frame.case === "snapshot") {
            daemonBootId = decoded.frame.value.daemon?.bootId ?? "";
          }
          dispatcher.observe(decoded);
          effects = adapter.apply(decoded);
        } catch (err) {
          clog(
            "error",
            `frontend frame decode/adapt threw: ${String(err)} — frame head: ${data.slice(0, 200)}`,
          );
          throw err;
        }
        // THE CONVERSATION REBASE, ruled on BEFORE the ingest. A rotated vendor
        // session uuid means the seqs this end holds count in a retired store
        // seq space, and the new space starts again at 1 — so the retired
        // space's items and marks are dropped BEFORE the new space's items land,
        // or those items would rank above the history they follow and the feed
        // would draw the clear at the top of a conversation it discarded.
        const verdict = sessionRebase.observe(claudeSessionIdOf(effects));
        if (verdict === "rotated") {
          // The retired conversation's blocks are not this one's: drop every
          // reveal cursor so a reused block id cannot inherit a shown length.
          smooth.reset();
          store.rebaseSeqSpace();
        }
        // Receipt for an arriving prompt, stamped at INGEST. The feed logs a
        // turn only when the rAF-coalesced render draws it, so on its own that
        // line cannot separate a suspended animation frame from a delta that
        // simply came late. Read `lastSeq` before ingest, which advances it; a
        // resync replay lands at or below it and is forwarded to the daemon by
        // nobody — it is a re-delivery, and a whole replayed history would eat
        // the forward budget the live line needs.
        const receipt = userTurnReceipt(effects, store.state.lastSeq);
        if (receipt !== null) {
          const line = `feed: user turn received request_id=${receipt.requestId} seq=${receipt.seq} len=${receipt.len} live=${receipt.live}`;
          if (receipt.live) clog("info", line);
          else log("info", line, { operation: "webapp.main.user-turn-receipt", localOnly: true });
        }
        const result = store.ingest(effects);
        // WHICH SESSION THE HTTP SIDE-CALLS TARGET, re-read from the pushed
        // plane the store just took. A workspace-addressed connection carries
        // no session id of its own, so the daemon's ruling on which session the
        // workspace owns is the only source for it — and a session that rotates
        // under a live view arrives through exactly this channel, which is why
        // it is re-read on every ingest rather than pinned at connect.
        if (store.state.sessionId !== "" && store.state.sessionId !== activeSessionId) {
          clog(
            "info",
            `page session rebound ${activeSessionId || "none"} -> ${store.state.sessionId} ` +
              `(address=${addressLabel(connectionAddress)})`,
          );
          activeSessionId = store.state.sessionId;
          bindLogContext({ agent_repl_session_id: activeSessionId });
        }
        // THE REVIVAL VERDICT, ruled on against the batch the store just took.
        // `revived` needs nothing here — renderChrome clears the pending line
        // off the cleared hibernation field, which is the same authority. What
        // only this can do is close the other case: a decision the daemon
        // ACCEPTED whose next pushed view still reports the session asleep. The
        // gate's buttons come back (the user has a choice to make again), the
        // card says why, and the workspace log carries the one record of it.
        const reviveVerdict = reviveWatch.observe(effects);
        if (reviveVerdict.kind === "failed") {
          revivePending = null;
          reviveFailure = REVIVE_FAILED_TEXT;
          clog("error", reviveFailedLog(reviveVerdict.mode, reviveVerdict.hibernation));
          renderChrome();
        }
        if (isSnapshot) {
          if (store.state.renderState === null || store.state.workspaceStateAtMs <= 0) {
            const err = new Error(
              `scoped snapshot omitted a revisioned WorkspaceState ` +
                `address=${addressLabel(connectionAddress)} ` +
                `state=${store.state.renderState ?? "none"} at_ms=${store.state.workspaceStateAtMs}`,
            );
            clog("error", err.message, {
              session_id: activeSessionId,
              workspace: store.state.cwd,
              render_state: store.state.renderState ?? "none",
              revision_at_ms: store.state.workspaceStateAtMs,
            });
            throw err;
          }
          client.adoptSnapshot({
            workspace: store.state.cwd,
            session_id: store.state.sessionId,
            controller_generation_id: store.state.controllerGenerationId,
            revision_at_ms: store.state.workspaceStateAtMs,
            cause_seq: store.state.workspaceStateCauseSeq,
            render_state: store.state.renderState,
          });
          // AFTER adoption, never before: a snapshot this page failed to ingest
          // proves nothing about which daemon build this bundle can talk to,
          // and pinning its boot id would let a wedged page believe it is
          // current. A CHANGED boot id means the daemon restarted (possibly
          // redeployed) under a page that cannot be redeployed in place, so the
          // page fetches itself again.
          versionSkew.observeSnapshotAdoption(daemonBootId);
        }
        // The rail, painted from the SAME burst the feed and the footer are
        // ingesting: on a connect snapshot the roster frame rides in with the
        // rest, so the gui's three surfaces finish appearing together instead
        // of the rail waiting on a separate script injection. Adopted
        // synchronously (like the current-status lease below) — the sidebar's
        // own revision gate decides whether a replayed roster is stale.
        for (const effect of effects) {
          if (effect.kind === "workspace-roster") sidebar.adoptRosterFrame(effect.value);
        }
        if (effects.some((effect) => effect.kind === "workspace-state")) {
          if (store.state.renderState === null) {
            throw new Error("workspace-state ingestion completed without a render state");
          }
          sidebar.setAuthoritativeCurrentStatus(workspaceStatusFromRenderState(store.state.renderState));
        }
        // Ask for the conversation history this connection has not been told.
        // Read AFTER ingest so the snapshot's own SessionView has supplied the
        // workspace key the daemon routes a resync by.
        connectResync.observe(isSnapshot, currentResyncSnapshot(store.state.lastSeq));
        // Logging context, re-fed from the SessionView plane. A first adoption
        // and a rotation both re-bind it: the attribution on every log record
        // must name the conversation that is live RIGHT NOW, which is exactly
        // why the uuid is read from the pushed plane and never stored.
        if (verdict !== "unchanged") {
          bindLogContext({ claude_session_id: sessionRebase.claudeSessionId });
        }
        // The rebased view holds no history at all, and the new space's items
        // may have been pushed before this end learned the rotation. Asking from
        // zero has the daemon serve the new space from its own replay floor —
        // the clear that caused the rotation, and everything since — so the
        // recovery never depends on which frame arrived first.
        if (verdict === "rotated") {
          const resyncSnapshot = currentResyncSnapshot(0);
          void dispatcher
            .resync(resyncSnapshot.workspace, resyncSnapshot)
            .catch(consumeOwnedDispatchFailure);
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
      onFreshnessChange: (freshness: WsStateFreshness) => {
        const current = freshness === "current";
        if (current) {
          wslog.flush();
          connectResync.onConnect();
          if (store.state.renderState === null) {
            throw new Error("websocket reported current before WorkspaceState adoption");
          }
          sidebar.setAuthoritativeCurrentStatus(workspaceStatusFromRenderState(store.state.renderState));
        } else {
          connectResync.onDisconnect();
          // A lease that expired is a connection that was served a snapshot and
          // still never became current. Enough of those in a row condemn this
          // bundle as stale code rather than the daemon as unreachable.
          if (freshness === "expired") versionSkew.observeLeaseExpiry();
          const changed = store.invalidateFrontendState(`websocket_${freshness}`);
          sidebar.setAuthoritativeCurrentStatus(
            freshness === "connecting" || freshness === "awaiting_snapshot" ? "init" : "degraded",
          );
          if (changed) frames.schedule();
        }
        const label: Record<WsStateFreshness, string> = {
          connecting: "connecting",
          awaiting_snapshot: "synchronizing",
          current: "connected",
          disconnected: "disconnected",
          expired: "state stale",
        };
        statusEl.textContent = label[freshness];
        statusEl.classList.toggle("ok", current);
      },
      // The daemon-unreachable window (F4). It is the ONE fact the daemon
      // definitionally cannot report about itself, so it is one of the very
      // few things this end classifies for itself — and it now says WHICH
      // fault it was, from the close code, instead of "reconnecting…" for
      // every transport failure alike.
      onUnreachable: (code, reason) => {
        if (store.addFailure(daemonUnreachableFailure(code, reason))) frames.schedule();
      },
      onReachable: () => {
        if (store.addFailure(daemonReachableFailure(Date.now()))) frames.schedule();
      },
      // The session-existence probe and the terminal verdict it feeds belong to
      // a SESSION-ADDRESSED page: its address names one session, so that
      // session disappearing ends the page. A workspace-addressed page outlives
      // the sessions its workspace runs — the daemon rules on which one the
      // workspace owns and re-pushes the answer — so there is no such verdict
      // for it to reach, and a socket that will not open keeps reporting
      // through the unreachable card above.
      ...(connectionAddress.kind === "session"
        ? {
            sessionExists: makeSessionExistsProbe(httpBase, connectionAddress.sessionId),
            onGone: (): void => {
              if (store.addFailure(sessionGoneFailure(connectionAddress.sessionId))) frames.schedule();
              statusEl.textContent = "session gone";
              statusEl.classList.remove("ok");
              // A vanished session is terminal for this page. Keep the failure
              // visible and stop here rather than synthesizing a successor
              // session or submitting any agent-authored recovery prompt.
              spinnerEl.classList.add("alarm");
              remediationEl.textContent = "session unavailable";
              clog(
                "error",
                `session ${connectionAddress.sessionId} is gone; automatic recovery is disabled`,
              );
            },
          }
        : {}),
    });
    return client;
  };
  // Session creation (replaces POST /sessions) uses a short-lived connection to the unscoped
  // /frontend WS — the daemon has no session-scoped socket to offer before the
  // session exists. It feeds ONLY its own dispatcher's correlation (never the
  // render store), sends CreateSessionCmd once the initial snapshot lands (so
  // the correlation's known-session set is populated first, and a pre-existing
  // same-cwd session cannot masquerade as the new one), and resolves with the
  // new id from the pushed SessionView.
  const createSessionViaWs = (args: { cwd: string } = { cwd: "" }): Promise<string> =>
    new Promise<string>((resolve, reject) => {
      let created = false;
      let settled = false;
      const previousClientLogSink = clientLogSink;
      let bootClientLogSink: typeof clientLogSink = null;
      const finish = (fn: () => void): void => {
        if (settled) return;
        settled = true;
        clearTimeout(timeout);
        bootWs.close();
        if (clientLogSink === bootClientLogSink) clientLogSink = previousClientLogSink;
        fn();
      };
      const bootWs = new WsClient({
        url: `${wsBase}/frontend`,
        onMessage: (data) => {
          let decoded;
          try {
            decoded = decodeFrontendFrame(data);
          } catch (err) {
            // Skipping the frame is legitimate — this socket only waits for a
            // snapshot to hang `createSession` off, so one unreadable frame
            // need not abort a boot the next frame can complete. Skipping it
            // QUIETLY is not: bootstrap frames carry StateSnapshots, progress
            // views included, so this drop is state the user no longer has.
            //
            // So the refusal is reported at the same volume and with the same
            // evidence as the session socket's (which re-throws, because there
            // a bad frame means the store is already wrong):
            //   - ERROR to the daemon log via `clientLog`, frame head included,
            //     which is what the reader greps after the fact;
            //   - a durable failure card in the feed, which is the only half a
            //     user sees without opening a log file.
            // Then, and only then, the boot continues.
            clog(
              "error",
              `bootstrap frame decode failed: ${String(err)} — frame head: ${data.slice(0, 200)}`,
            );
            if (store.addFailure(frameUndecodableFailure(err, data.slice(0, 200))))
              frames.schedule();
            return;
          }
          bootDispatcher.observe(decoded);
          if (decoded.frame.case === "snapshot" && !created) {
            bootWs.adoptSnapshot({ bootstrap: true });
            wslog.flush();
            created = true;
            void bootDispatcher
              .createSession({
                cwd: args.cwd,
                permissionMode: "",
                configDir: "",
                // CONTINUE, always: the browser names no conversation. The
                // daemon resolves which one this cwd owns (see ResumeMode).
                resumeMode: "RESUME_MODE_CONTINUE",
                fake: params.get("fake") === "1",
              })
              .then((id) => finish(() => resolve(id)))
              .catch((err: unknown) =>
                finish(() => reject(err instanceof Error ? err : new Error(String(err)))),
              );
          }
        },
      });
      // Its OWN dispatcher is bound to this bootstrap socket so the live
      // session's dispatcher is never repointed at a socket about to close.
      const bootDispatcher = new CommandDispatcher({
        send: (raw) => bootWs.send(raw),
        logLocal: (message) => log("error", message, { operation: "webapp.bootstrap-client-log-rejected", localOnly: true }),
      });
      // Before a session socket exists, the bootstrap socket is the canonical
      // forwarding route. Installing it before connect lets connection-attempt
      // records queue, then the open transition flushes them in order.
      bootClientLogSink = (level, message, context) =>
        bootDispatcher.clientLog(args.cwd, level, message, context as CommandStruct | undefined);
      clientLogSink = bootClientLogSink;
      const timeout = setTimeout(
        () => finish(() => reject(new Error("create session: no daemon snapshot within 15s"))),
        15_000,
      );
      bootWs.connect();
    });

  // The address the live socket opens against. An unaddressed page has to make
  // something to render first; every other page already names what it renders.
  //
  // The created id is held as internal state and NEVER written back into the
  // URL. A page that rewrote itself into a session address would turn its own
  // reload, bookmark, restored tab and remount into an attach against whatever
  // session that URL used to name.
  let connectionAddress: PageAddress = address;
  if (connectionAddress.kind === "unaddressed") {
    activeSessionId = await createSessionViaWs();
    connectionAddress = { kind: "session", sessionId: activeSessionId };
  }
  ws = makeClient(connectionAddress);
  ws.connect();

  if (composerEls !== null) {
    const input = composerEls.input;
    const submit = (): void => {
      const text = input.value.trim();
      if (text === "") return;
      // THE MERGE GATE, on the one path every submit goes through. The daemon
      // would refuse this prompt anyway (it holds the lease); refusing it here
      // is what turns a vanished draft and a delayed failure card into an
      // immediate explanation. The draft is deliberately KEPT — the user
      // will want to send it once the merge finishes.
      // THE REVIVAL GATE, on the same one path. Checked FIRST: the daemon nacks
      // a prompt on a hibernated session regardless of any lease, and the gate
      // card is what the user has to answer before a prompt can go anywhere. As
      // with the merge gate, the draft is deliberately KEPT.
      const asleep = store.state.hibernation;
      if (asleep !== null) {
        clog("warn", hibernationBlockedLog(text.length, asleep));
        // Re-assert the notice in case this attempt raced a frame that had not
        // painted it, so an attempted send NEVER reads as nothing happening.
        composerEls.notice.innerHTML = hibernationNoticeHtml(asleep);
        return;
      }
      if (submitBlocked(store.state.mergeLeaseHeld)) {
        clog("warn", mergeGateBlockedLog(text.length, store.state.mergeStatus));
        // The standing notice is already the explanation; re-assert it in case
        // this attempt raced a frame that had not painted it yet, so an
        // attempted send NEVER reads as nothing having happened.
        composerEls.notice.innerHTML = mergeGateNoticeHtml(true, store.state.mergeStatus);
        return;
      }
      submitPrompt(text, PromptOrigin.WEBAPP_USER_SENT);
      input.value = "";
    };
    composerEls.send.addEventListener("click", submit);
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

  // The selector never owns model state. Browser select controls move their
  // visual value before this handler runs, so restore the daemon-rendered
  // selection immediately. Only the daemon's correlated receipt can paint a
  // new selection; the subsequent SessionView remains the durable snapshot.
  modelEl.addEventListener("change", () => {
    const requested = modelEl.value;
    modelEl.value = store.state.model;
    if (requested === store.state.model) return;
    void dispatcher.setModel(store.state.cwd, requested).then(
      (selected) => {
        if (store.applyAcknowledgedModel(selected)) rerender();
        modelEl.value = selected;
        clog("info", `model switch acknowledged request=${requested} selected=${selected}`);
      },
      (err: unknown) => {
        if (err instanceof ModelSelectionRejectedError) {
          if (store.applyAcknowledgedModel(err.selectedModel)) rerender();
          modelEl.value = err.selectedModel;
          clog("error", `model switch rejected request=${requested} selected=${err.selectedModel}: ${err.message}`);
          return;
        }
        clog("error", `model switch rejected request=${requested}: ${String(err)}`);
      },
    );
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
        if (store.addFailure(controlPlaneFailure("the login request", err)))
          frames.schedule();
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
        if (store.addFailure(controlPlaneFailure("the account switch", err)))
          frames.schedule();
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
    // The boot failure is the one card the store cannot carry — boot is what
    // builds the store. It is rendered directly, but through the SAME classes
    // every other failure uses, so the one surface a user hits when nothing
    // else works does not look like a different application.
    // It previously emitted `.error-banner`, a class the stylesheet no longer
    // defines: unstyled black text on the feed background.
    feed.innerHTML =
      `<div class="failure-card failure-internal" data-error-type="client.boot_failed">` +
      `<div class="failure-head"><span class="failure-mark">✕</span>` +
      `<span class="failure-message">agent-repl could not start</span></div>` +
      `<div class="failure-detail">${escapeHtml(String(err))}</div></div>`;
  }
});
