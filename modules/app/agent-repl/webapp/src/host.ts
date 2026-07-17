/**
 * The Emacs host's bridge into the webapp.
 *
 * The gui frontend is this page mounted inside Emacs as an xwidget, and
 * the only channel Emacs has into it is evaluating JavaScript against the
 * live document. So everything the host must be able to trigger is planted
 * on `window` under a name both sides agree on, and nowhere else — the
 * lisp end of the contract is `agent-repl--frontend-webview-execute-script'
 * (frontend.el), which calls the hook by exactly the name exported here.
 *
 * Keep the surface a single named global per host action: a host script is
 * written as a string in lisp, so a DOM query written there instead would
 * couple frontend.el to this page's markup and drift silently the moment a
 * class or id changes.
 */
import { ScrollTail, parkAtTail } from "./scroll.js";

/**
 * Name of the global that snaps the feed to its newest message.
 * `agent-repl-frontend-tail-hook' (frontend.el) MUST match this string.
 */
export const TAIL_HOOK = "agentReplParkAtTail";

/** What a hook is planted on: `window`, or a plain object under test. */
export type HostGlobal = Record<string, unknown>;

/**
 * Plant the tail hook on TARGET, parking FEED at its newest message.
 *
 * Emacs fires it on every switch TO the workspace, so a feed the user
 * left scrolled up in history is back at the newest message the instant
 * the workspace is on screen, rather than showing stale middle-of-history
 * content until the next turn arrives.
 */
export function installHostTailHook(target: HostGlobal, feed: ScrollTail): void {
  target[TAIL_HOOK] = (): void => {
    parkAtTail(feed);
  };
}

/**
 * Name of the global that closes every open topbar dropdown.
 * `agent-repl-frontend-close-menus-hook' (frontend.el) MUST match this
 * string.
 */
export const CLOSE_MENUS_HOOK = "agentReplCloseTopbarMenus";

/**
 * Plant the close-menus hook on TARGET, dismissing every open topbar
 * overlay through CLOSE when the host fires it.
 *
 * The page's own outside-click handler already closes its dropdowns on a
 * click anywhere INSIDE the document, but the composer the GUI puts focus
 * into is a separate Emacs window the webview cannot see — so Emacs fires
 * this hook when the user clicks that input window, closing the header and
 * bubble dropdowns that would otherwise hang open behind it.
 */
export function installHostCloseMenusHook(target: HostGlobal, close: () => void): void {
  target[CLOSE_MENUS_HOOK] = (): void => {
    close();
  };
}
