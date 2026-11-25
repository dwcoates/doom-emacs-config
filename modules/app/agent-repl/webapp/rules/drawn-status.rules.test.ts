/** Every drawn-status record is written through createDrawnStatusLog (src/drawn-status.ts). */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { filesUnder } from "./files-under.js";

describe("the drawn-status records' one shape", () => {
  it("is written only through createDrawnStatusLog", () => {
    // Arrange: every source file that names a drawn-status operation.
    const root = join(__dirname, "..", "src");
    const files = filesUnder(root);
    // Act
    const handRolled = files.filter((file) => {
      // A logger's scope (createLogger("webapp.drawn-status")) names no operation.
      const source = readFileSync(file, "utf8").replace(/createLogger\("[^"]*"\)/g, "");
      return /"[a-z-]+\.drawn-status"/.test(source) && !/createDrawnStatusLog\([^)]*"[a-z-]+\.drawn-status"\)/.test(source);
    });
    // Assert
    expect(handRolled).toEqual([]);
  });
});
