/** No module but src/code-block.ts draws the code block's shape. */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { codeUnder } from "./files-under.js";

const here = path.dirname(fileURLToPath(import.meta.url));
const SRC = path.join(here, "../src");

describe("codeBlockHtml", () => {

  it("is the only place src writes the code block's shape", () => {
    // Arrange
    const files = [...codeUnder(SRC)];
    // Act
    const handRolled = files
      .filter(([file]) => file !== "code-block.ts")
      .filter(([, code]) => /class="md-code"/.test(code))
      .map(([file]) => file);
    // Assert
    expect(handRolled).toEqual([]);
  });
});
