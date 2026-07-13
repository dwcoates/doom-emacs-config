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
import { installCopyKeys } from "./copy.js";
import { installClickExpand } from "./expand.js";
import { HostGlobal, installHostTailHook } from "./host.js";
import { loginNotice, requestLogin } from "./login.js";
import { PermissionMode } from "./protocol.js";
import { rebindSession, rememberResumeKeys } from "./rebind.js";
import { remediationNotice, requestRemediation } from "./remediation.js";
import { FeedRenderer, sessionInfoHtml } from "./render.js";
import { installEdgeScroll } from "./scroll.js";
import { ConversationStore } from "./store.js";
import { WsClient, composerEnabled, makeSessionExistsProbe } from "./ws.js";
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
  });

  const statusEl = must("conn-status");
  const infoEl = must("session-info");
  const modeEl = must<HTMLSelectElement>("mode-select");
  const loginEl = must<HTMLButtonElement>("login-btn");
  const spinnerEl = must("spinner");
  const remediationEl = must("remediation");
  const parentWs = params.get("parent_ws");

  // The subagent roster's disclosure state. It lives HERE rather than in the
  // DOM because renderChrome rewrites the whole topbar on every frame, which
  // would otherwise collapse an overlay the user is reading mid-turn.
  let agentsOpen = false;

  const renderChrome = (): void => {
    const s = store.state;
    // sessionInfoHtml escapes every value it interpolates.
    infoEl.innerHTML = sessionInfoHtml(
      parentWs,
      s.model,
      s.usage,
      sessionSubagents(s.items),
      agentsOpen,
    );
    if (modeEl.value !== s.permissionMode) modeEl.value = s.permissionMode;
    spinnerEl.classList.toggle("on", s.turnInFlight);
    document.title = s.model ? `claude-repl · ${s.model}` : "claude-repl";
  };

  const setAgentsOpen = (open: boolean): void => {
    if (agentsOpen === open) return;
    agentsOpen = open;
    renderChrome();
  };

  // The chip is re-created by every renderChrome, so the toggle is delegated
  // off the topbar rather than bound to a node that will not survive the turn.
  infoEl.addEventListener("click", (e) => {
    if ((e.target as HTMLElement).closest("[data-agents-toggle]")) {
      setAgentsOpen(!agentsOpen);
    }
  });
  // An open overlay closes the way every dropdown does: click off it, or Escape.
  document.addEventListener("click", (e) => {
    if (!(e.target as HTMLElement).closest(".agents-menu")) setAgentsOpen(false);
  });
  document.addEventListener("keydown", (e) => {
    if (e.key === "Escape") setAgentsOpen(false);
  });

  const rerender = (): void => {
    feed.render(store.state);
    renderChrome();
  };

  // swapTo rebinds the live view onto a successor session id (the
  // client-side twin of the Emacs sync-webview rebind): fresh store,
  // fresh socket, URL param updated so a reload lands on the successor.
  const swapTo = (next: string): void => {
    console.warn(`session rebind: ${activeSessionId} -> ${next}`);
    ws.close();
    activeSessionId = next;
    rememberedClaudeId = "";
    store.reset();
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
          feed.renderRestored(store.state);
          renderChrome();
        } else if (result.changed && store.replaying) {
          // Replay still streaming: defer the feed, keep the chrome live.
          renderChrome();
        } else if (result.changed) {
          rerender();
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

  // Login is the one topbar control that does not talk to the SDK session:
  // the OAuth flow needs a TTY, so it goes over HTTP to the daemon, which
  // hands it to Emacs to run in a vterm. The button is disabled for the
  // round trip so a double-click cannot ask for two login terminals.
  //
  // The notice reuses #remediation, the topbar's one status-line slot.
  loginEl.addEventListener("click", () => {
    loginEl.disabled = true;
    void requestLogin(httpBase, activeSessionId)
      .then((phase) => {
        remediationEl.textContent = loginNotice(phase);
      })
      .catch((err: unknown) => {
        // A login that never opened must say so: leaving the topbar silent
        // would send the user off to look for a terminal that is not coming.
        remediationEl.textContent = loginNotice("failed");
        console.error("login request failed", err);
      })
      .finally(() => {
        loginEl.disabled = false;
      });
  });
}

boot().catch((err: unknown) => {
  const feed = document.getElementById("feed");
  if (feed) {
    feed.innerHTML = `<div class="error-banner">boot failed: ${String(err)}</div>`;
  }
});
