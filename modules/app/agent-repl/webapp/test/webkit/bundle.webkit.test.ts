/**
 * The shared page bundler (bundle.ts): one classic script that assigns the
 * entry's exports to `window[name]`, which is what every webkit suite driving
 * the real modules relies on.
 */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { runInNewContext } from "node:vm";
import { describe, expect, it } from "vitest";
import { bundlePage } from "./bundle";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("bundlePage", () => {
  it("assigns the entry's exports to the named global", async () => {
    // Arrange
    const code = await bundlePage(path.join(here, "fixtures/bundle-entry.ts"), "bundleEntry");
    const sandbox: Record<string, unknown> = {};

    // Act
    runInNewContext(code, sandbox);

    // Assert
    expect((sandbox.bundleEntry as { answer: number }).answer).toBe(42);
  });

  it("refuses an entry that does not exist", async () => {
    await expect(bundlePage(path.join(here, "fixtures/no-such-entry.ts"), "missing")).rejects.toThrow();
  });
});
