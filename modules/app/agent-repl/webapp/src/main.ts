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
import { HostGlobal, installHostCloseMenusHook, installHostTailHook } from "./host.js";
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
import { rebindSession, rememberResumeKeys } from "./rebind.js";
import { hiddenContinueMessage, rememberMidTask, shouldAutoContinue } from "./resume-continue.js";
import { remediationNotice, requestRemediation } from "./remediation.js";
import { requestSupportWorkspace } from "./unsupported.js";
import { fetchStatus, refreshStatus } from "./status.js";
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
import { fetchTaskTail } from "./watcher-poll.js";
import "./styles.css";

function must<T extends HTMLElement>(id: string): T {
  const el = document.getElementById(id);
  if (!el) throw new Error(`missing #${id}`);
  return el as T;
}

async function createSession(base: string, fake: boolean): Promise<string> {
  const resp = await fetch(`${base}/sessions`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ fake }),
  });
  if (!resp.ok) {
    throw new Error(`POST /sessions failed: ${resp.status} ${await resp.text()}`);
  }
  const body = (await resp.json()) as { session_id: string };
  return body.session_id;
}

async function boot(): Promise<void> {
  const params = new URLSearchParams(location.search);
  const daemon = params.get("daemon") ?? location.host;
  const httpBase = `${location.protocol === "https:" ? "https" : "http"}://${daemon}`;
  const wsBase = `${location.protocol === "https:" ? "wss" : "ws"}://${daemon}`;

  let joined = params.get("session");
  if (!joined) {
    joined = await createSession(httpBase, params.get("fake") === "1");
    const url = new URL(location.href);
    url.searchParams.set("session", joined);
    history.replaceState(null, "", url.toString());
  }
  // Mutable on purpose: the "session gone" rebind swaps the live view
  // onto a successor session id; every closure below reads the current
  // binding.
  let activeSessionId: string = joined;
  // The claude_session_id already persisted for activeSessionId, so the
  // per-frame hook below only touches localStorage on actual change.
  let rememberedClaudeId = "";
  // The last mid-task marker value written for the live session, so the
  // per-frame sync below only touches localStorage when a turn actually
  // starts or ends rather than on every streaming delta. Reset by swapTo
  // alongside rememberedClaudeId when the view rebinds to a successor.
  let midTaskActive: boolean | null = null;
  let ws: WsClient;

  const store = new ConversationStore();
  const feedEl = must("feed");
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
  // The workspaces rail. Emacs pushes the whole roster through this hook on
  // every workspace-state change; the rail stays collapsed until the first
  // push, so a bare-browser session keeps the single-column layout. Roster
  // state is global to the editor, not per-session, so it lives beside the
  // store rather than in it.
  const sidebar = new WorkspaceSidebar(must("ws-sidebar"), { httpBase });
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
  const feed = new FeedRenderer(feedEl, {
    onRendered: () => search.refresh(),
    decidePermission: (requestId, behavior) => {
      ws.send(
        behavior === "allow"
          ? { type: "permission-decision", request_id: requestId, decision: { behavior: "allow" } }
          : {
              type: "permission-decision",
              request_id: requestId,
              decision: { behavior: "deny", message: "denied from webapp" },
            },
      );
    },
    answerQuestions: (requestId, updatedInput) => {
      // AskUserQuestion contract: allow with the tool input echoed back
      // carrying the `answers` record the user picked.
      ws.send({
        type: "permission-decision",
        request_id: requestId,
        decision: { behavior: "allow", updated_input: updatedInput },
      });
    },
    // §2.13 queued-card controls: both are daemon-handled Layer-2 commands,
    // never forwarded to the shim, reusing the request_id convention.
    cancelQueued: (queueId) => {
      ws.send({ type: "queue-cancel", request_id: crypto.randomUUID(), queue_id: queueId });
    },
    runQueuedNow: (queueId) => {
      ws.send({ type: "queue-run-now", request_id: crypto.randomUUID(), queue_id: queueId });
    },
    sendPrompt: (text) => {
      // Card controls (stop task) are prompt-mediated: the button sends
      // an ordinary user message through the same channel the composer
      // uses, so it spends a turn and queues like any other prompt.
      ws.send({
        type: "user-message",
        request_id: crypto.randomUUID(),
        content: text,
      });
    },
    // Watcher folds poll this while open (§ watcher-bubble expansion),
    // targeting the CURRENT session so a rebind moves the polls with it.
    fetchTaskTail: (taskId, offset) => fetchTaskTail(httpBase, activeSessionId, taskId, offset),
    // The unsupported-command card's button. Targets the CURRENT session
    // so a rebind opens the workspace against the checkout in view, and
    // resolves to the workspace name Emacs was asked for — Emacs, not the
    // daemon, decides what actually happens next.
    addSupport: (command) => requestSupportWorkspace(httpBase, activeSessionId, command),
    // The `/status` panel's data, targeting the CURRENT session so a rebind
    // reads the status of the checkout in view. `getStatus` seeds the panel
    // (snapshot + account); `refreshStatus` re-probes so the snapshot reflects
    // any mid-session change, arriving back as a `status` frame.
    getStatus: () => fetchStatus(httpBase, activeSessionId),
    refreshStatus: () => refreshStatus(httpBase, activeSessionId),
  });

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
    // renderer draws the agent-scoped bubble topbars (see topbar.ts). The
    // monitoring flag is the feed renderer's own gate reading (idle + live
    // async), read back here so the strip's left-most datapoint mirrors the
    // feed the last render already partitioned rather than re-deriving it.
    infoEl.innerHTML = topbarInfoHtml(sessionTopbarDatapoints(s, parentWs, feed.isMonitoring()), {
      agentsOpen: counterMenu === "agents",
      tasksOpen: counterMenu === "tasks",
      tokensOpen: counterMenu === "tokens",
    });
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
    if (modeEl.value !== s.permissionMode) modeEl.value = s.permissionMode;
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

  // Renders are coalesced onto animation frames: a message burst — the
  // backlog draining when this webview's hidden workspace is switched back
  // to, a reconnect's replay, a fast delta stream — otherwise runs a full
  // feed render per message, and that churn is the switch-in jitter. The
  // paint decides chrome-only vs full feed when it FIRES, not when it was
  // asked for: a replay that completes between the two must paint the feed,
  // not just the chrome.
  const frames = new RenderCoalescer(windowFrameHost(window), () => {
    if (store.replaying) {
      renderChrome();
    } else {
      rerender();
    }
  });
  // The wake anchors' countdowns tick on wall-clock time, not on frames,
  // so nothing would re-render them between deltas. A slow heartbeat
  // ask keeps them honest through the same coalescer every other render
  // rides; reconciliation no-ops every node whose HTML did not change.
  window.setInterval(() => frames.schedule(), 30_000);

  // swapTo rebinds the live view onto a successor session id (the
  // client-side twin of the Emacs sync-webview rebind): fresh store,
  // fresh socket, URL param updated so a reload lands on the successor.
  const swapTo = (next: string): void => {
    console.warn(`session rebind: ${activeSessionId} -> ${next}`);
    ws.close();
    activeSessionId = next;
    rememberedClaudeId = "";
    midTaskActive = null;
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
        console.error("remediation dispatch failed", err);
      });
  };

  const makeClient = (sessionId: string): WsClient =>
    new WsClient({
      url: `${wsBase}/sessions/${sessionId}/stream`,
      onMessage: (data) => {
        const result = store.applyRaw(data);
        if (store.state.claudeSessionId !== "" && store.state.claudeSessionId !== rememberedClaudeId) {
          // The hello supplied (or updated) the durable CLI uuid: persist
          // it so a future "session gone" can rebind this conversation.
          rememberedClaudeId = store.state.claudeSessionId;
          rememberResumeKeys(localStorage, sessionId, {
            claudeSessionId: store.state.claudeSessionId,
            cwd: store.state.cwd,
          });
        }
        if (result.restored) {
          // A session stopped mid-task auto-resumes here: the mid-task
          // marker outlived the killed turn, and the rehydrated turn is not
          // live, so nudge the agent to continue without drawing a bubble
          // (the nudge is meta-wrapped). Checked before the sync below so it
          // reads the pre-boot marker, not the one this frame would write.
          if (shouldAutoContinue(localStorage, store.state.claudeSessionId, store.state.turnInFlight)) {
            ws.send({
              type: "user-message",
              request_id: crypto.randomUUID(),
              content: hiddenContinueMessage(),
            });
          }
          // Fresh-join replay complete: tail-first backfill render, so
          // the newest message is on screen immediately with no scroll.
          // Supersedes any coalesced paint — left pending, its rAF would
          // fire next and flush the staged backfill in one gulp.
          frames.cancel();
          feed.renderRestored(store.state);
          nav.reconcile(lastUserTurnId(store.state.items));
          renderChrome();
          // The restored render drew every block in full: mark them shown so
          // a reconnect mid-stream types out only the growth that arrives
          // AFTER the join, never re-typing prose already on screen.
          smooth.markShown(store.state);
        } else if (result.changed) {
          // One paint per animation frame, however many messages land
          // before it. During a replay the paint keeps the chrome live
          // and leaves the feed to the completion render.
          frames.schedule();
        }
        // Track whether a turn is running so a kill mid-task leaves the
        // marker set for the next boot's auto-resume above. The restored
        // frame is skipped so its reconciled turnInFlight cannot clear the
        // marker the auto-resume just consulted; replay frames are skipped
        // so a transcript's turn toggles never thrash it. The guard writes
        // localStorage only at a turn boundary, not on every delta.
        if (
          !result.restored &&
          !store.replaying &&
          store.state.claudeSessionId !== "" &&
          store.state.turnInFlight !== midTaskActive
        ) {
          midTaskActive = store.state.turnInFlight;
          rememberMidTask(localStorage, store.state.claudeSessionId, store.state.turnInFlight);
        }
        return result.send;
      },
      onStatusChange: (connected) => {
        statusEl.textContent = connected ? "connected" : "disconnected";
        statusEl.classList.toggle("ok", connected);
      },
      sessionExists: makeSessionExistsProbe(httpBase, sessionId),
      onGone: () => {
        statusEl.textContent = "session gone";
        statusEl.classList.remove("ok");
        // The turn-in-flight tick becomes a red/orange alarm: a lost session
        // is not a quiet state, and the dot is what the eye lands on.
        spinnerEl.classList.add("alarm");
        remediationEl.textContent = "rebinding session";
        void rebindSession(httpBase, sessionId, localStorage)
          .then((next) => {
            if (next !== null) {
              swapTo(next);
              return;
            }
            // Nothing durable was ever stored for this id: remediate.
            remediate(sessionId);
          })
          .catch((err: unknown) => {
            console.error("session rebind failed", err);
            remediate(sessionId);
          });
      },
    });
  ws = makeClient(activeSessionId);
  ws.connect();

  if (composerEnabled(params)) {
    const input = must<HTMLTextAreaElement>("composer-input");
    const submit = (): void => {
      const text = input.value.trim();
      if (text === "") return;
      ws.send({
        type: "user-message",
        request_id: crypto.randomUUID(),
        content: text,
      });
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

  modeEl.addEventListener("change", () => {
    ws.send({
      type: "set-permission-mode",
      request_id: crypto.randomUUID(),
      mode: modeEl.value as PermissionMode,
    });
  });

  // Picking a model ASKS for the switch; it does not assert it. The topbar
  // moves only when the `model-changed` frame comes back, so a switch the
  // SDK rejects leaves the picker showing the model still in force rather
  // than one the session never adopted.
  modelEl.addEventListener("change", () => {
    ws.send({
      type: "set-model",
      request_id: crypto.randomUUID(),
      model: modelEl.value,
    });
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
        console.error("account roster lookup failed", err);
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
        console.error("account lookup failed", err);
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
        console.error("login request failed", err);
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
        console.error("account switch failed", err);
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
      console.error("account menu lookup failed", err);
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
      console.error("closing the login terminal failed", err);
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
