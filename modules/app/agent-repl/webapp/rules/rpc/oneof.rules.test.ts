/**
 * THE SOURCE READS A ONEOF'S ARMS IN ONE PLACE (`src/rpc/oneof.ts`).
 */
import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import { dirname, join, relative } from "node:path";
import { fileURLToPath } from "node:url";
import { filesUnder } from "../files-under.js";

describe("the source reads a oneof's arms in one place", () => {
  it("finds no oneof lookup outside the reader", () => {
    // Arrange: every source file under src.
    const src = join(dirname(fileURLToPath(import.meta.url)), "../../src");
    const files = filesUnder(src);

    // Act
    const lookups = files.filter((f) => readFileSync(f, "utf8").includes(".oneofs.find(")).map((f) => relative(src, f));

    // Assert: only the reader's own module.
    expect(lookups).toEqual(["rpc/oneof.ts"]);
  });
});
