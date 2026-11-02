/**
 * The host page's start: build the host and hand it to Emacs.
 *
 * The host is the webapp's own bundle loaded at `?host=1` (main.ts), so the
 * page arrives with the workspace page's shell markup; the host clears it and
 * marks the root, which is what its stylesheet keys on.
 *
 * Emacs reaches the host through `window.agentReplHost` (lisp/webhost.el), the
 * one global this page defines besides `xwHasFocus`. A call Emacs makes before
 * the host has started is queued on `window.__agentReplHostPending` by the
 * call's own script, and run here, in order, the moment the host starts: the
 * host's code arrives by a dynamic import, so no call may depend on timing.
 *
 * `xwHasFocus` IS EMACS'S OWN QUESTION, ANSWERED FOR THE SHOWN PAGE. On the NS
 * port the web view asks the page `xwHasFocus()` on every key and forwards the
 * key to Emacs unless an INPUT or TEXTAREA holds the focus (src/nsxwidget.m,
 * which injects its own definition into this top frame). With the workspace
 * pages in iframes, the top frame's focused element is the iframe, so Emacs's
 * definition would answer no to every field of every page and take every key
 * away from it. The answer comes instead from the shown page, which reports
 * whether one of its text fields holds the focus (embed.ts).
 */
import { createHost, hostHasFocus, type Host } from "./host.js";
import { startWebhostServices } from "./services.js";
import { installPopupLayer, openPopupChannel, type PopupChannel, type PopupLayerEnv } from "./sidebar-popups.js";
import { WEBHOST_GATE_ID } from "./gate.js";
import { WEBHOST_LOAD_ID } from "./load-button.js";
import { installColorValues } from "../vocab.js";
import { runQueuedCalls } from "./answers.js";
import type { WebhostRuntime } from "./document.js";
import "./host.css";

/** A call Emacs made before the host started, queued on the page. */
type PendingCall = (host: Host) => unknown;

declare global {
  interface Window {
    agentReplHost?: Host;
    xwHasFocus?: () => boolean;
    __agentReplHostPending?: PendingCall[];
  }
}

/** The class the host page's root wears. */
export const HOST_ROOT_CLASS = "agent-repl-host";

/** The class of the element the workspace pages are stacked in. */
export const WEBHOST_FRAMES_CLASS = "webhost-frames";

/** What the start may have injected, for a suite that is not about the services. */
export interface StartHostDeps {
  /** The webhost's connection and logger, made before its code loaded (boot.ts). */
  runtime: WebhostRuntime;
  /** Start the webhost's services; the real one dials the daemon. */
  startServices?: (
    doc: Document,
    win: Window,
    host: Host,
    runtime: WebhostRuntime,
    deps: { gate: HTMLElement; load: HTMLElement },
  ) => unknown;
  /** Install the layer the sidebar's popups are drawn in; the real one listens on the channel. */
  installPopups?: (env: PopupLayerEnv) => unknown;
  /** Open the channel the sidebar's popups arrive on; the host origin's own when absent. */
  popupChannel?: () => PopupChannel;
}

/** Turn the page in DOC and WIN into the host, and hand it to Emacs. */
export function startHost(doc: Document, win: Window, deps: StartHostDeps): Host {
  const { runtime } = deps;
  doc.body.replaceChildren();
  doc.documentElement.classList.add(HOST_ROOT_CLASS);
  // THE STATUS COLORS' VALUES come from the shared vocabulary, never from
  // styles.css, and the webhost draws the cold gate's card in them.
  installColorValues(doc);
  // THE STACK OF WORKSPACE PAGES. The sidebar is not here: it is a web view of
  // its own beside the host (host/sidebar-page.ts).
  const frames = doc.createElement("div");
  frames.className = WEBHOST_FRAMES_CLASS;
  // THE SHOWN WORKSPACE'S COLD GATE, drawn by the webhost (host/gate.ts).
  const gate = doc.createElement("div");
  gate.id = WEBHOST_GATE_ID;
  gate.hidden = true;
  // THE LOAD BUTTON OF A HELD-BACK PAGE, over the frames (host/load-button.ts).
  const load = doc.createElement("div");
  load.id = WEBHOST_LOAD_ID;
  load.hidden = true;
  doc.body.append(frames, load, gate);
  // THE WEBHOST'S OWN LOG LEVEL, raised with every page's by a profiling
  // session (`Host.setLogLevel`), and ITS DAEMON, which starts as the one that
  // served it and moves with its workspaces at a handover (`Host.setDaemon`):
  // both made with its logger, before its code loaded (boot.ts).
  const { level, daemon } = runtime;
  const host = createHost({ doc, win, frames, level, daemon });
  // THE SERVICES START BEFORE EMACS'S QUEUED CALLS RUN, so every listener
  // they add is told the state the queued calls then set, or replays it. (A
  // queued call that logs once threw here for want of a logger, which the
  // services installed: 2026-10-31. The logger is now the runtime's, made
  // before the host's code loaded.)
  (deps.startServices ?? startWebhostServices)(doc, win, host, runtime, { gate, load });
  // THE SIDEBAR'S HOVER POPUPS, drawn over the pages (sidebar-popups.ts).
  (deps.installPopups ?? installPopupLayer)({
    doc,
    win,
    channel: (deps.popupChannel ?? openPopupChannel)(),
    onOffset: (listener) => host.onSidebarOffset(listener),
    onPagePressed: (listener) => host.onPagePressed(() => listener()),
  });
  win.agentReplHost = host;
  const pending = win.__agentReplHostPending;
  delete win.__agentReplHostPending;
  runQueuedCalls(pending, host);
  const answerFocus = (): boolean => hostHasFocus(doc, host);
  win.xwHasFocus = answerFocus;
  // Emacs injects its own definition as a user script, which can run after
  // this module; the host's own answer is put back once the page has loaded.
  win.addEventListener("load", () => {
    win.xwHasFocus = answerFocus;
  });
  return host;
}
