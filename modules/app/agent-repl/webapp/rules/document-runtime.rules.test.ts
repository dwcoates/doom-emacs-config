/** Both of the webapp's documents set themselves up through src/document-runtime.ts, and hand-roll none of it. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

describe("installTextAffordances", () => {
  it("is what both documents set themselves up with", () => {
    // Arrange
    const here = path.dirname(fileURLToPath(import.meta.url));

    // Act
    // The workspace page, and Emacs's own two web views, which both start
    // through host/document.ts (the host's services and the sidebar page).
    const sources = ["main.ts", path.join("host", "document.ts")].map((file) =>
      readFileSync(path.join(here, "..", "src", file), "utf8"),
    );

    // Assert
    expect(
      sources.map((s) => [
        s.includes("startPageRuntime("),
        s.includes("installTextAffordances("),
        s.includes("installForwardingLogger("),
        s.includes("new ForwardingLogger("),
        s.includes("mintConnectionId("),
        s.includes("installCopyFallback("),
        s.includes("installCopyChords("),
        s.includes("installCodeCopy("),
      ]),
    ).toEqual([
      [true, true, false, false, false, false, false, false],
      [true, true, false, false, false, false, false, false],
    ]);
  });
});
