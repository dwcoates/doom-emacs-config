// @vitest-environment jsdom
/** The one reader of the selection's text, and the proof every reader uses it. */
import { readdirSync, readFileSync, statSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it } from "vitest";
import { selectedText, type SelectionSource } from "../src/selection.js";
import { codeOf } from "./source-text.js";

const here = path.dirname(fileURLToPath(import.meta.url));
const SRC = path.join(here, "../src");

afterEach(() => {
  document.body.innerHTML = "";
  document.getSelection()?.removeAllRanges();
});

/** Select the whole of a fresh paragraph holding TEXT. */
function selectParagraph(text: string): void {
  const host = document.createElement("p");
  host.textContent = text;
  document.body.append(host);
  const range = document.createRange();
  range.selectNodeContents(host);
  document.getSelection()?.addRange(range);
}

/** Every TypeScript module under DIR, recursively. */
function modules(dir: string): string[] {
  return readdirSync(dir).flatMap((name) => {
    const full = path.join(dir, name);
    if (statSync(full).isDirectory()) return modules(full);
    return full.endsWith(".ts") ? [full] : [];
  });
}

describe("selectedText", () => {
  it("answers the selected text", () => {
    // Arrange
    selectParagraph("boot_failed");
    // Act / Assert
    expect(selectedText()).toBe("boot_failed");
  });

  it("answers the empty string when the selection is collapsed", () => {
    // Arrange
    selectParagraph("boot_failed");
    document.getSelection()?.collapseToStart();
    // Act / Assert
    expect(selectedText()).toBe("");
  });

  it("answers the empty string when the source has no selection object", () => {
    // Arrange
    const source: SelectionSource = { getSelection: () => null };
    // Act / Assert
    expect(selectedText(source)).toBe("");
  });

  it("reads a document's selection as well as a window's", () => {
    // Arrange
    selectParagraph("frame unreadable");
    // Act / Assert
    expect(selectedText(document)).toBe("frame unreadable");
  });

  it("is the one reader of a selection's text in src", () => {
    // Arrange
    const readers = modules(SRC).filter((file) => !file.endsWith(`${path.sep}selection.ts`));
    // Act
    const handRolled = readers
      .filter((file) => /getSelection\(\)\??\.toString\(/.test(codeOf(readFileSync(file, "utf8"))))
      .map((file) => path.relative(SRC, file));
    // Assert
    expect(handRolled).toEqual([]);
  });
});
