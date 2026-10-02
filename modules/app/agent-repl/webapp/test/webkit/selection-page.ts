/**
 * THE PAGE THE WEBKIT SELECTION TEST DRIVES (selection.webkit.test.ts).
 *
 * Bundled by that test into one script and run in headless WebKit over the
 * REAL stylesheet, it installs the REAL click guard (src/selection.ts) and
 * counts the clicks that reach the markup the test draws.
 */
import { installSelectionClickGuard } from "../../src/selection.js";

/** What the test reads back from the page. */
export interface SelectionPage {
  /** Install the guard, and start counting clicks on the host. */
  install(): void;
  /** Clicks that reached the host since the last reset, then reset. */
  takeClicks(): number;
  /** The page's selected text, then clear the selection. */
  takeSelection(): string;
}

declare global {
  interface Window {
    selectionPage: SelectionPage;
  }
}

let clicks = 0;

window.selectionPage = {
  install(): void {
    installSelectionClickGuard(document);
    document.getElementById("host")?.addEventListener("click", () => clicks++);
  },
  takeClicks(): number {
    const taken = clicks;
    clicks = 0;
    return taken;
  },
  takeSelection(): string {
    const selection = document.getSelection();
    const text = selection === null ? "" : selection.toString();
    selection?.removeAllRanges();
    return text;
  },
};
