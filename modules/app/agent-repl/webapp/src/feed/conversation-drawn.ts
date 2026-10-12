/**
 * conversation-drawn — THE PAGE SAYS WHEN ITS CONVERSATION IS ON SCREEN.
 *
 * Emacs's startup opens a workspace's tab only once the restored conversation
 * is drawn, not once the HTML loaded (xwidget `load-changed` fires before any
 * feed page exists). The page marks its root element with
 * `data-conversation-drawn` once the root feed's opening page is applied and
 * painted, and Emacs reads the mark through its webview read probe
 * (lisp/startup.el). The mark is set once per page and never cleared.
 *
 * PAINTED means two animation frames after the page was applied: the first
 * runs before the frame that shows the rows, the second after it. A page NOT
 * SEEN (its document hidden, or the host holding it behind another page:
 * `pagePresence().seen()`) paints nothing, and WebKit throttles its animation
 * frames into bursts many seconds apart (2026-10-12: pages applied by +13s
 * were marked at +15s and +27s), so there the applied page is the whole truth
 * and the mark is set at once. Every page loaded for a workspace being opened
 * is exactly such a page.
 *
 * A refused open marks the page too (`refused`): the refusal is what the
 * conversation slot shows, and a tab held for a page that will never draw
 * would never open.
 */
import { log } from "../log.js";
import { pagePresence } from "../presence.js";

/** The attribute Emacs reads. */
export const CONVERSATION_DRAWN_ATTRIBUTE = "data-conversation-drawn";

/** What the mark records: an opening page drawn, or a refused open. */
export type ConversationDrawn = "page" | "refused";

/** The environment the mark is set in; tests substitute the frame clock. */
export interface ConversationDrawnEnv {
  readonly doc: Document;
  readonly frame: (callback: () => void) => void;
  /** Whether the page is seen: shown by its host and its document visible. */
  readonly seen: () => boolean;
}

/** The page itself. */
export function conversationDrawnEnv(): ConversationDrawnEnv {
  return {
    doc: document,
    frame: (callback) => {
      requestAnimationFrame(() => callback());
    },
    seen: () => pagePresence().seen(),
  };
}

/** Mark the page drawn as WHAT once the applied page has painted. */
export function markConversationDrawn(
  what: ConversationDrawn,
  env: ConversationDrawnEnv = conversationDrawnEnv(),
): void {
  const root = env.doc.documentElement;
  if (root.hasAttribute(CONVERSATION_DRAWN_ATTRIBUTE)) return;
  const mark = (): void => {
    if (root.hasAttribute(CONVERSATION_DRAWN_ATTRIBUTE)) return;
    root.setAttribute(CONVERSATION_DRAWN_ATTRIBUTE, what);
    log.info("the conversation is on screen", {
      operation: "feed.conversation-drawn",
      context: { what, seen },
    });
  };
  const seen = env.seen();
  if (!seen) {
    mark();
    return;
  }
  env.frame(() => env.frame(mark));
}
