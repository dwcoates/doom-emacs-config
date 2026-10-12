/**
 * The host page's start: build the host and hand it to Emacs.
 *
 * The host is the webapp's own bundle loaded at `?host=1` (main.ts), so the
 * page arrives with the workspace page's shell markup; the host clears it and
 * marks the root, which is what its stylesheet keys on.
 *
 * Emacs reaches the host through `window.agentReplHost` (lisp/webhost.el), the
 * one global this page defines besides `xwHasFocus`.
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

declare global {
  interface Window {
    agentReplHost?: Host;
    xwHasFocus?: () => boolean;
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
  const answerFocus = (): boolean => hostHasFocus(doc, host);
  win.xwHasFocus = answerFocus;
  // Emacs injects its own definition as a user script, which can run after
  // this module; the host's own answer is put back once the page has loaded.
  win.addEventListener("load", () => {
    win.xwHasFocus = answerFocus;
  });
  return host;
}
