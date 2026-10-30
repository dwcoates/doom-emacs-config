/**
 * webhost-pressed — THE ONE PLACE A PAGE HEARS THAT THE READER PRESSED THE
 * WEBHOST AROUND IT: its sidebar, or anything else the webhost draws outside
 * the workspace pages (owner request, 2026-10-30: a click on the sidebar ends
 * a final response's highlight, as a click anywhere else in the webapp does).
 *
 * A press in the webhost's own document never reaches a page's document (each
 * page is a cross-origin iframe), and the page cannot tell it from the focus
 * leaving for an Emacs window, which must NOT end the highlight. So the
 * webhost says so (`webhostPressed`, host/protocol.ts), and this is where the
 * page's readers hear it.
 */
import { createListeners } from "./listeners.js";
import { log } from "./log.js";
import { perDocument } from "./per-document.js";

/** The page's webhost-press signal, as its readers see it. */
export interface WebhostPresses {
  /** Tell LISTENER every time the reader presses the webhost; answers the unsubscribe. */
  onPressed(listener: () => void): () => void;
}

/** The signal plus the one writer the host link holds. */
export interface WebhostPressSource extends WebhostPresses {
  /** The reader pressed the webhost around the page. */
  pressed(): void;
}

/** A fresh webhost-press signal. */
export function createWebhostPresses(): WebhostPressSource {
  const listeners = createListeners();
  return {
    onPressed(listener) {
      return listeners.add(listener);
    },
    pressed() {
      log.debug("the reader pressed the webhost around the page", {
        operation: "page.webhost-pressed",
        context: { listeners: listeners.size },
      });
      listeners.notify();
    },
  };
}

/** The webhost-press signal of each page, keyed by its document: one per page. */
const webhostPressesOf = perDocument(() => createWebhostPresses());

/** The webhost-press signal of DOC's page: one per page (per document), made on first use. */
export function pageWebhostPresses(doc: Document = document): WebhostPressSource {
  return webhostPressesOf(doc);
}
