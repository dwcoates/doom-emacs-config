/** mintConnectionId (src/connection-id.ts) is the one minter: no document hand-rolls its own. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

describe("mintConnectionId", () => {
  it("is the one minter: no document hand-rolls its own", () => {
    // Arrange
    const here = path.dirname(fileURLToPath(import.meta.url));
    const read = (file: string) => readFileSync(path.join(here, "..", "src", file), "utf8");

    // Act
    // Every document begins through document-runtime.ts `startPageRuntime`,
    // which mints the connection: the workspace page, and Emacs's own two web
    // views, which both start through host/document.ts.
    const runtime = read("document-runtime.ts");
    const documents = ["main.ts", path.join("host", "document.ts")].map(read);

    // Assert
    expect([
      [runtime.includes("mintConnectionId()"), runtime.includes("randomUUID")],
      ...documents.map((source) => [source.includes("startPageRuntime("), source.includes("randomUUID")]),
    ]).toEqual([
      [true, false],
      [true, false],
      [true, false],
    ]);
  });
});
