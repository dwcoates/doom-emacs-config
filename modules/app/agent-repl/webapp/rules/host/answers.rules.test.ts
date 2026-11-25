/**
 * EVERY DOCUMENT EMACS CALLS ANSWERS AND RUNS ITS QUEUED CALLS THROUGH THE
 * SHARED HELPERS (`src/host/answers.ts`), never by hand.
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { codeOf } from "../../test/source-text.js";

describe("the documents Emacs calls", () => {
  it.each(["host/host.ts", "host/sidebar-page.ts"])("%s answers through createCallAnswers, never by hand", (path) => {
    // Arrange
    const code = codeOf(readFileSync(join(__dirname, "..", "..", "src", path), "utf8"));

    // Act
    const shape = { shared: code.includes("createCallAnswers("), handRolled: /JSON\.stringify\(\{\s*op\b/.test(code) };

    // Assert
    expect(shape).toEqual({ shared: true, handRolled: false });
  });

  it.each(["host/main.ts", "host/sidebar-page.ts"])("%s runs its queued calls through runQueuedCalls, never by hand", (path) => {
    // Arrange
    const code = codeOf(readFileSync(join(__dirname, "..", "..", "src", path), "utf8"));

    // Act
    const shape = { shared: code.includes("runQueuedCalls("), handRolled: /for \(const \w+ of pending\)/.test(code) };

    // Assert
    expect(shape).toEqual({ shared: true, handRolled: false });
  });
});
