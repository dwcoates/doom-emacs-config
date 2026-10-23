/**
 * THE PAGE THE WEBKIT SIDEBAR DRAG TEST DRIVES (sidebar-drag.webkit.test.ts).
 *
 * Bundled by that test into one script and run in headless WebKit over the
 * REAL stylesheet, it draws a scrolling list of text rows and hands a press
 * on any of them to the REAL drag (src/sidebar/drag.ts).
 */
import { createArrangeHold } from "../../src/sidebar/arrange.js";
import { pressToDrag } from "../../src/sidebar/drag.js";
import { installPageLogger } from "./page-log";
import { takePageSelection } from "./page-selection";

/** What the test reads back from the page. */
export interface SidebarDragPage {
  /** Draw the rows and arm the drag on them. */
  install(): void;
  /** The page's selected text, then clear the selection. */
  takeSelection(): string;
  /** Where, in the viewport, the middle of row ID's character AT sits. */
  charMiddle(id: string, at: number): { x: number; y: number };
  /** Whether the list has scrolled to its end. */
  scrolledToEnd(): boolean;
  /** The rows' ids in their current order. */
  order(): string[];
}

declare global {
  interface Window {
    sidebarDragPage: SidebarDragPage;
  }
}

window.sidebarDragPage = {
  install(): void {
    installPageLogger("sidebar-drag");
    const host = document.getElementById("host");
    if (host === null) throw new Error("the page has no host");
    host.style.height = "140px";
    host.style.overflowY = "auto";
    const list = document.createElement("div");
    list.style.width = "240px";
    for (const id of ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]) {
      const row = document.createElement("div");
      row.id = id;
      row.textContent = `workspace row ${id}`;
      row.style.height = "28px";
      row.style.fontSize = "14px";
      list.append(row);
    }
    // THE COPY'S LAYER stands outside the scrolling list, as the rail's does.
    const layer = document.createElement("div");
    host.append(list);
    document.body.append(layer);
    const arrange = { hold: createArrangeHold(() => {}), layer, status: layer };
    list.addEventListener("pointerdown", (event) => {
      const item = (event.target as HTMLElement).closest<HTMLElement>("#host > div > div");
      if (item === null) return;
      pressToDrag(
        arrange,
        {
          kind: "row",
          item,
          box: list,
          scroller: host,
          lists: () => [list],
          itemsOf: (l) => Array.from(l.children) as HTMLElement[],
          dropped: () => {},
        },
        event,
      );
    });
  },
  takeSelection(): string {
    return takePageSelection(document);
  },
  charMiddle(id: string, at: number): { x: number; y: number } {
    const text = document.getElementById(id)?.firstChild;
    if (!(text instanceof Text)) throw new Error(`row ${id} has no text`);
    const range = document.createRange();
    range.setStart(text, at);
    range.setEnd(text, at + 1);
    const box = range.getBoundingClientRect();
    return { x: box.left + box.width / 2, y: box.top + box.height / 2 };
  },
  scrolledToEnd(): boolean {
    const host = document.getElementById("host");
    if (host === null) throw new Error("the page has no host");
    return host.scrollTop + host.clientHeight >= host.scrollHeight - 1;
  },
  order(): string[] {
    return Array.from(document.querySelectorAll("#host > div > div"), (row) => row.id);
  },
};
