/**
 * focus-left — THE ONE PLACE A PAGE HEARS THAT THE USER'S FOCUS LEFT IT, so
 * every notice the pointer raised on the page can let go of it at once (owner
 * request, 2026-10-26: the hovered response's line in the footer clears
 * whenever the focus changes, always).
 *
 * THE FOCUS LEAVES THE PAGE when
 *   - the page's window loses the focus (`blur`): a click into another Emacs
 *     window, into the webhost's sidebar, or Emacs leaving the screen's front;
 *   - the page stops being seen (presence.ts): the host hid it for another
 *     workspace's, or Emacs left the screen;
 *   - Emacs moved its own focus (`left`, from the host's `focusMoved`): the
 *     selected Emacs window changed, or Emacs's frame lost the focus. A key
 *     the web view forwards to Emacs (`C-h`, `C-l`) selects another Emacs
 *     window without the web view ever losing the focus, so the page cannot
 *     see this one for itself.
 */
import { log } from "./log.js";
import { perDocument } from "./per-document.js";
import { pagePresence, type Presence } from "./presence.js";

/** Told why the focus left the page. */
type FocusLeftListener = (why: string) => void;

/** The page's focus-left signal, as its readers see it. */
export interface FocusLeft {
  /** Tell LISTENER every time the focus leaves the page; answers the unsubscribe. */
  onLeft(listener: FocusLeftListener): () => void;
}

/** The signal plus the one writer the host link holds. */
export interface FocusLeftSource extends FocusLeft {
  /** The focus left the page for a reason the page cannot see, WHY. */
  left(why: string): void;
  /** Stop listening to the window and the presence. */
  dispose(): void;
}

/** What the focus left the page for, as its listeners are told. */
export const FOCUS_LEFT_BLUR = "the page lost focus";
export const FOCUS_LEFT_HIDDEN = "the page was hidden";

/** The focus-left signal of the page in WIN, whose presence is PRESENCE. */
export function createFocusLeft(win: Window, presence: Presence): FocusLeftSource {
  const listeners = new Set<FocusLeftListener>();
  const tell = (why: string): void => {
    log.debug("the focus left the page; letting go of what the pointer raised", {
      operation: "page.focus-left",
      context: { why, listeners: listeners.size },
    });
    for (const listener of [...listeners]) listener(why);
  };
  const onBlur = (): void => tell(FOCUS_LEFT_BLUR);
  win.addEventListener("blur", onBlur);
  const unsubscribe = presence.onChange((seen) => {
    if (!seen) tell(FOCUS_LEFT_HIDDEN);
  });
  return {
    onLeft(listener) {
      listeners.add(listener);
      return () => {
        listeners.delete(listener);
      };
    },
    left: tell,
    dispose() {
      win.removeEventListener("blur", onBlur);
      unsubscribe();
      listeners.clear();
    },
  };
}

/** The focus-left signal of each page, keyed by its document: one per page. */
const focusLeftOf = perDocument(() => createFocusLeft(window, pagePresence()));

/** The focus-left signal of this page: one per page (per document), made on first use. */
export function pageFocusLeft(): FocusLeftSource {
  return focusLeftOf(document);
}
