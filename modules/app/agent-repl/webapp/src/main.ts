/**
 * Webapp bootstrap: session creation/join, WebSocket wiring, composer.
 *
 * URL parameters:
 *   ?daemon=host:port   daemon address (default: current host)
 *   ?session=<id>       join an existing session (else one is created)
 *   ?fake=1             create the session against the offline fake SDK
 *   ?parent_ws=<name>   parent workspace basename shown in the topbar
 */
import { sessionSubagents } from "./agents.js";
import { sessionTasks } from "./tasks.js";
import { countedTurns } from "./turn-clock.js";
import { configureChessGames, installChessNavHook } from "./chess-game.js";
import { RenderCoalescer, windowFrameHost } from "./coalesce.js";
import { installCopyKeys } from "./copy.js";
import { installClickExpand } from "./expand.js";
import { HostGlobal, installHostTailHook } from "./host.js";
import {
  type Account,
  type RosterEntry,
  accountIsLoggedOut,
  accountLabel,
  accountMenuEntries,
  fetchAccount,
  fetchAccounts,
  switchAccount,
} from "./account.js";
import { attachLoginTerminal, type LoginTerminal } from "./login-terminal.js";
import { closeLogin, loginNotice, requestLogin } from "./login.js";
import { PermissionMode } from "./protocol.js";
import { rebindSession, rememberResumeKeys } from "./rebind.js";
import { remediationNotice, requestRemediation } from "./remediation.js";
import { requestSupportWorkspace } from "./unsupported.js";
import { compactionBannerHtml, FeedRenderer, modelOptionsHtml, sessionInfoHtml } from "./render.js";
import { installEdgeScroll, isPinnedToBottom, parkAtTail } from "./scroll.js";
import { ConversationStore } from "./store.js";
import { IDLE_LABEL, TIMER_SLOT, TaskTimer, windowHost } from "./timer.js";
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
  let ws: WsClient;

  const store = new ConversationStore();
  const feedEl = must("feed");
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
  const feed = new FeedRenderer(feedEl, {
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
  });

  const statusEl = must("conn-status");
  const infoEl = must("session-info");
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

  // Which topbar counter dropdown is open (only one at a time). It lives
  // HERE rather than in the DOM because renderChrome rewrites the whole
  // topbar on every frame, which would otherwise collapse an overlay the
  // user is reading mid-turn.
  let counterMenu: "agents" | "tasks" | null = null;

  // The running task's timer. Its tick writes the one span it owns rather
  // than re-rendering the topbar: a whole-strip rewrite once a second, on
  // top of the per-frame rewrites a streaming turn already drives, would be
  // churn in service of a single changing digit.
  //
  // The span is re-created by every renderChrome, so the tick looks it up
  // each time instead of holding a node that will not survive the turn.
  let timerLabel = IDLE_LABEL;
  const timer = new TaskTimer(windowHost(window), (label) => {
    timerLabel = label;
    const slot = infoEl.querySelector(`[${TIMER_SLOT}]`);
    if (!slot) throw new Error(`topbar has no ${TIMER_SLOT} span`);
    slot.textContent = label;
  });

  const renderChrome = (): void => {
    const s = store.state;
    // sessionInfoHtml escapes every value it interpolates.
    infoEl.innerHTML = sessionInfoHtml(
      parentWs,
      s.contextTokens,
      sessionSubagents(s.items),
      sessionTasks(s.items),
      countedTurns(s.items),
      counterMenu === "agents",
      counterMenu === "tasks",
      timerLabel,
    );
    // After the strip exists, so the paint on a starting turn has a span to
    // land in. Only the edges of a turn touch the interval.
    timer.sync(s.turnStartedAt);
    // Rebuilt only when the menu or the selection actually moved: this runs
    // on EVERY frame, and blowing the options away mid-turn would slam shut
    // a dropdown the user had open.
    const nextOptions = modelOptionsHtml(s.models, s.model);
    if (modelEl.innerHTML !== nextOptions) modelEl.innerHTML = nextOptions;
    if (modeEl.value !== s.permissionMode) modeEl.value = s.permissionMode;
    spinnerEl.classList.toggle("on", s.turnInFlight);
    // Empty string when no compaction runs, which collapses the slot.
    compactBarEl.innerHTML = compactionBannerHtml(s.compacting);
    document.title = s.model ? `claude-repl · ${s.model}` : "claude-repl";
  };

  const setCounterMenu = (menu: "agents" | "tasks" | null): void => {
    if (counterMenu === menu) return;
    counterMenu = menu;
    renderChrome();
  };

  // The chips are re-created by every renderChrome, so the toggles are
  // delegated off the topbar rather than bound to nodes that will not
  // survive the turn. Opening one counter closes the other.
  infoEl.addEventListener("click", (e) => {
    const target = e.target as HTMLElement;
    if (target.closest("[data-agents-toggle]")) {
      setCounterMenu(counterMenu === "agents" ? null : "agents");
      return;
    }
    if (target.closest("[data-tasks-toggle]")) {
      setCounterMenu(counterMenu === "tasks" ? null : "tasks");
      return;
    }
    // A click on a subagent row jumps the feed to that agent's bubble and
    // opens it, then dismisses the roster so the revealed card is unobscured.
    const agentId = target.closest(".agent-row")?.getAttribute("data-agent-id");
    if (agentId) {
      feed.revealAgent(agentId);
      setCounterMenu(null);
    }
  });
  // An open overlay closes the way every dropdown does: click off it, or Escape.
  document.addEventListener("click", (e) => {
    const target = e.target as HTMLElement;
    if (!target.closest(".agents-menu") && !target.closest(".tasks-menu")) {
      setCounterMenu(null);
    }
  });
  document.addEventListener("keydown", (e) => {
    if (e.key === "Escape") setCounterMenu(null);
  });

  const rerender = (): void => {
    feed.render(store.state);
    renderChrome();
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
    // A paint scheduled against the dead session would render the
    // just-reset (empty) store; the successor's hello drives the next one.
    frames.cancel();
    store.reset();
    // The successor's turn is not this one's: stop the clock now rather than
    // letting it run on the dead session until the successor's hello lands.
    timer.stop();
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
          // Fresh-join replay complete: tail-first backfill render, so
          // the newest message is on screen immediately with no scroll.
          // Supersedes any coalesced paint — left pending, its rAF would
          // fire next and flush the staged backfill in one gulp.
          frames.cancel();
          feed.renderRestored(store.state);
          renderChrome();
        } else if (result.changed) {
          // One paint per animation frame, however many messages land
          // before it. During a replay the paint keeps the chrome live
          // and leaves the feed to the completion render.
          frames.schedule();
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
    input.addEventListener("keydown", (e) => {
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

  // Which account this session runs as. Refreshed after every login and
  // switch, since those are the two things that change the answer. The
  // last-fetched value seeds the menu's re-auth entry.
  let account: Account | null = null;
  const refreshAccount = (): void => {
    void fetchAccount(httpBase, activeSessionId)
      .then((fetched) => {
        account = fetched;
        accountEl.textContent = accountLabel(account);
        accountEl.classList.toggle("logged-out", accountIsLoggedOut(account));
      })
      .catch((err: unknown) => {
        // Not fatal: the session may still be perfectly usable. Leave the slot
        // blank rather than assert an account we failed to read.
        account = null;
        accountEl.textContent = "";
        accountEl.classList.remove("logged-out");
        console.error("account lookup failed", err);
      });
  };
  refreshAccount();

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
    refreshAccount();
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
        refreshAccount();
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

  // The chip's dropdown. Rebuilt on every open from the daemon's live
  // roster, so the identities shown are current rather than boot-time
  // stale; a daemon without -accounts degrades to the re-auth entry alone.
  const hideMenu = (): void => {
    accountMenuEl.hidden = true;
  };
  const openMenu = async (): Promise<void> => {
    let roster: RosterEntry[] = [];
    try {
      roster = await fetchAccounts(httpBase);
    } catch (err: unknown) {
      console.error("account roster lookup failed", err);
    }
    accountMenuEl.replaceChildren(
      ...accountMenuEntries(account, roster).map((entry) => {
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
