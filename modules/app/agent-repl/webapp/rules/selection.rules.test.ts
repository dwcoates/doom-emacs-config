/** selectedText (src/selection.ts) is the one reader of a selection's text in src. */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { codeUnder } from "./files-under.js";

const here = path.dirname(fileURLToPath(import.meta.url));
const SRC = path.join(here, "../src");

describe("selectedText", () => {
  it("is the one reader of a selection's text in src", () => {
    // Arrange
    const readers = [...codeUnder(SRC)].filter(([file]) => !`${path.sep}${file}`.endsWith(`${path.sep}selection.ts`));
    // Act
    const handRolled = readers
      .filter(([, code]) => /getSelection\(\)\??\.toString\(/.test(code))
      .map(([file]) => file);
    // Assert
    expect(handRolled).toEqual([]);
  });
});
