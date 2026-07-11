/**
 * Webapp bootstrap: session creation/join, WebSocket wiring, composer.
 *
 * URL parameters:
 *   ?daemon=host:port   daemon address (default: current host)
 *   ?session=<id>       join an existing session (else one is created)
 *   ?fake=1             create the session against the offline fake SDK
 */
import { PermissionMode } from "./protocol.js";
import { FeedRenderer } from "./render.js";
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

  let sessionId = params.get("session");
  if (!sessionId) {
    sessionId = await createSession(httpBase, params.get("fake") === "1");
    const url = new URL(location.href);
    url.searchParams.set("session", sessionId);
    history.replaceState(null, "", url.toString());
  }

  const store = new ConversationStore();
  const feed = new FeedRenderer(must("feed"), {
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
  const chipEl = must("usage-chip");
  const modeEl = must<HTMLSelectElement>("mode-select");
  const spinnerEl = must("spinner");

  const renderChrome = (): void => {
    const s = store.state;
    chipEl.textContent = s.usage
      ? `${s.usage.input_tokens}in/${s.usage.output_tokens}out${
          s.costUsd !== null ? ` · $${s.costUsd.toFixed(4)}` : ""
        }`
      : "";
    if (modeEl.value !== s.permissionMode) modeEl.value = s.permissionMode;
    spinnerEl.classList.toggle("on", s.turnInFlight);
    document.title = s.model ? `claude-repl · ${s.model}` : "claude-repl";
  };

  const rerender = (): void => {
    feed.render(store.state);
    renderChrome();
  };

  const ws = new WsClient({
    url: `${wsBase}/sessions/${sessionId}/stream`,
    onMessage: (data) => {
      const result = store.applyRaw(data);
      if (result.changed) rerender();
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
    },
  });
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

  must<HTMLButtonElement>("interrupt-btn").addEventListener("click", () => {
    ws.send({ type: "interrupt", request_id: crypto.randomUUID() });
  });

  modeEl.addEventListener("change", () => {
    ws.send({
      type: "set-permission-mode",
      request_id: crypto.randomUUID(),
      mode: modeEl.value as PermissionMode,
    });
  });
}

boot().catch((err: unknown) => {
  const feed = document.getElementById("feed");
  if (feed) {
    feed.innerHTML = `<div class="error-banner">boot failed: ${String(err)}</div>`;
  }
});
