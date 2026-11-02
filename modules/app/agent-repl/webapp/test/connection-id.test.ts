// A loaded document's own identity. See src/connection-id.ts.
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it, vi } from "vitest";

import { mintConnectionId } from "../src/connection-id.js";

afterEach(() => {
  vi.unstubAllGlobals();
});

describe("mintConnectionId", () => {
  it("is the platform's random UUID where there is one", () => {
    // Arrange
    vi.stubGlobal("crypto", { randomUUID: () => "11111111-2222-3333-4444-555555555555" });

    // Act
    const id = mintConnectionId();

    // Assert
    expect(id).toBe("11111111-2222-3333-4444-555555555555");
  });

  it("falls back to time and entropy where there is none", () => {
    // Arrange
    vi.stubGlobal("crypto", undefined);

    // Act
    const id = mintConnectionId();

    // Assert
    expect(id).toMatch(/^web-[0-9a-z]+-[0-9a-z]+$/);
  });

  it("mints a different id each time", () => {
    // Arrange / Act
    const ids = new Set([mintConnectionId(), mintConnectionId()]);

    // Assert
    expect(ids.size).toBe(2);
  });

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
