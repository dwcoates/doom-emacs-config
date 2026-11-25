/** takePageSelection (test/webkit/page-selection.ts) is what every webkit page reads its selection through. */
import { describe, expect, it } from "vitest";
import selectionPage from "../test/webkit/selection-page.ts?raw";
import sidebarDragPage from "../test/webkit/sidebar-drag-page.ts?raw";

describe("takePageSelection", () => {
  it("is what every webkit page reads its selection through", () => {
    // Arrange
    const pages = [selectionPage, sidebarDragPage];

    // Assert: each delegates, and none reads a selection by hand.
    expect(pages.map((src) => [src.includes("takePageSelection(document)"), src.includes("getSelection(")])).toEqual([
      [true, false],
      [true, false],
    ]);
  });
});
