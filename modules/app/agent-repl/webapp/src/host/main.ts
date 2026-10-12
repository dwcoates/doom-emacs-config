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

/** Turn the page in DOC and WIN into the host, and hand it to Emacs. */
export function startHost(doc: Document, win: Window): Host {
  doc.body.replaceChildren();
  doc.documentElement.classList.add(HOST_ROOT_CLASS);
  const host = createHost({ doc, win });
  win.agentReplHost = host;
  const pending = win.__agentReplHostPending ?? [];
  delete win.__agentReplHostPending;
  for (const call of pending) call(host);
  const answerFocus = (): boolean => hostHasFocus(doc, host);
  win.xwHasFocus = answerFocus;
  // Emacs injects its own definition as a user script, which can run after
  // this module; the host's own answer is put back once the page has loaded.
  win.addEventListener("load", () => {
    win.xwHasFocus = answerFocus;
  });
  return host;
}
