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
import { LevelWindow } from "../../../agent-shim/logging/ts/level-window.js";
import { createHost, hostHasFocus, type Host } from "./host.js";
import { WEBHOST_SIDEBAR_ID, startWebhostSidebar } from "./sidebar.js";
import { installColorValues } from "../vocab.js";
import { createDaemonOrigin, type DaemonOrigin } from "../rpc/daemon-origin.js";
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

/** Turn the page in DOC and WIN into the host, and hand it to Emacs. */
/** What the start may have injected, for a suite that is not about the sidebar. */
export interface StartHostDeps {
  /** Start the webhost's sidebar; the real one dials the daemon. */
  startSidebar?: (
    doc: Document,
    win: Window,
    host: Host,
    nav: HTMLElement,
    deps: { level: LevelWindow; daemon: DaemonOrigin },
  ) => unknown;
}

export function startHost(doc: Document, win: Window, deps: StartHostDeps = {}): Host {
  doc.body.replaceChildren();
  doc.documentElement.classList.add(HOST_ROOT_CLASS);
  // THE STATUS COLORS' VALUES come from the shared vocabulary, never from
  // styles.css, and the webhost draws the sidebar every status dot is in: a
  // host that did not install them drew every dot in the text's off-white
  // (2026-10-17, the pastel landing).
  installColorValues(doc);
  // THE SIDEBAR OVER THE PAGES: one nav, drawn over every page's reserved
  // sidebar column (host/sidebar.ts), and the stack of workspace pages under it.
  const nav = doc.createElement("nav");
  nav.id = WEBHOST_SIDEBAR_ID;
  nav.setAttribute("data-component", "sidebar");
  nav.hidden = true;
  const frames = doc.createElement("div");
  frames.className = WEBHOST_FRAMES_CLASS;
  doc.body.append(frames, nav);
  // THE WEBHOST'S OWN LOG LEVEL, raised with every page's by a profiling
  // session (`Host.setLogLevel`); its logger is the sidebar's.
  const level = LevelWindow.fixed("info");
  // THE WEBHOST'S DAEMON starts as the one that served it, and moves with
  // its workspaces at a handover (`Host.setDaemon`).
  const daemon = createDaemonOrigin(win.location.origin);
  const host = createHost({ doc, win, frames, level, daemon });
  win.agentReplHost = host;
  const pending = win.__agentReplHostPending ?? [];
  delete win.__agentReplHostPending;
  for (const call of pending) call(host);
  (deps.startSidebar ?? startWebhostSidebar)(doc, win, host, nav, { level, daemon });
  const answerFocus = (): boolean => hostHasFocus(doc, host);
  win.xwHasFocus = answerFocus;
  // Emacs injects its own definition as a user script, which can run after
  // this module; the host's own answer is put back once the page has loaded.
  win.addEventListener("load", () => {
    win.xwHasFocus = answerFocus;
  });
  return host;
}
