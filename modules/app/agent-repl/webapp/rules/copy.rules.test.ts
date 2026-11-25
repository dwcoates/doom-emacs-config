/** writeCopyEvent is the one place src/copy.ts writes onto a copy event's clipboard. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { codeOf } from "../test/source-text.js";

describe("writeCopyEvent", () => {
  it("is the one place copy.ts writes onto a copy event's clipboard", () => {
    // Arrange
    const here = path.dirname(fileURLToPath(import.meta.url));
    const code = codeOf(readFileSync(path.join(here, "../src/copy.ts"), "utf8"));
    // Act
    const writes = code.match(/\.setData\(/g) ?? [];
    const callers = code.match(/writeCopyEvent\(event,/g) ?? [];
    // Assert
    expect([writes.length, callers.length]).toEqual([1, 2]);
  });
});
