/**
 * THE PAGE THE WEBKIT SELECTION TEST DRIVES (selection.webkit.test.ts).
 *
 * Bundled by that test into one script and run in headless WebKit over the
 * REAL stylesheet, it installs the REAL click guard (src/selection.ts), builds
 * REAL controls (src/control.ts) between two runs of prose, and counts the
 * clicks that reach each control.
 */
import { createControl } from "../../src/control.js";
import { installSelectionClickGuard } from "../../src/selection.js";

/** What the test reads back from the page. */
export interface SelectionPage {
  /** Install the guard, and draw the prose with an enabled and a disabled control. */
  install(): void;
  /** Clicks that reached the control with ID since the last take, then reset. */
  takeClicks(id: string): number;
  /** The page's selected text, then clear the selection. */
  takeSelection(): string;
  /** Focus the control with ID, as a Tab would. */
  focus(id: string): void;
}

declare global {
  interface Window {
    selectionPage: SelectionPage;
  }
}

const clicks = new Map<string, number>();

/** A control with ID and LABEL, counting the clicks that reach it. */
function control(id: string, label: string, disabled: boolean): HTMLElement {
  const el = createControl();
  el.id = id;
  el.textContent = label;
  el.disabled = disabled;
  clicks.set(id, 0);
  el.addEventListener("click", () => clicks.set(id, (clicks.get(id) ?? 0) + 1));
  return el;
}

/** A span of prose with ID. */
function prose(id: string, text: string): HTMLElement {
  const el = document.createElement("span");
  el.id = id;
  el.textContent = text;
  return el;
}

window.selectionPage = {
  install(): void {
    installSelectionClickGuard(document);
    const p = document.createElement("p");
    p.style.fontSize = "20px";
    p.append(
      prose("before", "alpha alpha"),
      " ",
      control("control", "Send now", false),
      " ",
      prose("after", "charlie charlie"),
      " ",
      control("off", "Cancel", true),
    );
    document.getElementById("host")?.append(p);
  },
  takeClicks(id: string): number {
    const taken = clicks.get(id) ?? 0;
    clicks.set(id, 0);
    return taken;
  },
  takeSelection(): string {
    const selection = document.getSelection();
    const text = selection === null ? "" : selection.toString();
    selection?.removeAllRanges();
    return text;
  },
  focus(id: string): void {
    document.getElementById(id)?.focus();
  },
};
