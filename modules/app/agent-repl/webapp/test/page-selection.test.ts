// @vitest-environment jsdom
// The selection a webkit page hands back. See test/webkit/page-selection.ts.
import { afterEach, describe, expect, it } from "vitest";
import { takePageSelection } from "./webkit/page-selection";
import selectionPage from "./webkit/selection-page.ts?raw";
import sidebarDragPage from "./webkit/sidebar-drag-page.ts?raw";

/** Select the text of a paragraph reading TEXT. */
function selectParagraph(text: string): void {
  const p = document.createElement("p");
  p.textContent = text;
  document.body.append(p);
  document.getSelection()?.selectAllChildren(p);
}

afterEach(() => {
  document.getSelection()?.removeAllRanges();
  document.body.replaceChildren();
});

describe("takePageSelection", () => {
  it("answers the selected text", () => {
    // Arrange
    selectParagraph("workspace row");

    // Act / Assert
    expect(takePageSelection(document)).toBe("workspace row");
  });

  it("clears the selection it took, so the next gesture starts from none", () => {
    // Arrange
    selectParagraph("workspace row");

    // Act
    takePageSelection(document);

    // Assert
    expect(takePageSelection(document)).toBe("");
  });

  it("answers nothing when nothing is selected", () => {
    // Act / Assert
    expect(takePageSelection(document)).toBe("");
  });

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
