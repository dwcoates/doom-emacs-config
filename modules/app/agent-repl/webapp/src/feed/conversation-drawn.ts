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
 * runs before the frame that shows the rows, the second after it. A HIDDEN
 * document paints nothing and WebKit suspends its animation frames, so there
 * the applied page is the whole truth and the mark is set at once; a held
 * startup tab is exactly such a hidden page.
 *
 * A refused open marks the page too (`refused`): the refusal is what the
 * conversation slot shows, and a tab held for a page that will never draw
 * would never open.
 */
import { log } from "../log.js";

/** The attribute Emacs reads. */
export const CONVERSATION_DRAWN_ATTRIBUTE = "data-conversation-drawn";

/** What the mark records: an opening page drawn, or a refused open. */
export type ConversationDrawn = "page" | "refused";

/** The environment the mark is set in; tests substitute the frame clock. */
export interface ConversationDrawnEnv {
  readonly doc: Document;
  readonly frame: (callback: () => void) => void;
}

/** The page itself. */
export function conversationDrawnEnv(): ConversationDrawnEnv {
  return {
    doc: document,
    frame: (callback) => {
      requestAnimationFrame(() => callback());
    },
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
      context: { what, hidden: env.doc.hidden },
    });
  };
  if (env.doc.hidden) {
    mark();
    return;
  }
  env.frame(() => env.frame(mark));
}
