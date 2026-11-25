// @vitest-environment jsdom
// The selection a webkit page hands back. See test/webkit/page-selection.ts.
import { afterEach, describe, expect, it } from "vitest";
import { takePageSelection } from "./webkit/page-selection";

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
});
