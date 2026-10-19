/**
 * The shared page bundler (bundle.ts): one classic script that assigns the
 * entry's exports to `window[name]`, which is what every webkit suite driving
 * the real modules relies on.
 */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { runInNewContext } from "node:vm";
import { beforeAll, describe, expect, it } from "vitest";
import { bundlePage } from "./bundle";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("bundlePage", () => {
  // Each bundle is a real vite build, paid in the hook: a test body only reads
  // what the hook produced (vitest.webkit.config.ts).
  let code: string;
  let refusal: unknown;

  beforeAll(async () => {
    code = await bundlePage(path.join(here, "fixtures/bundle-entry.ts"), "bundleEntry");
    refusal = await bundlePage(path.join(here, "fixtures/no-such-entry.ts"), "missing").then(
      () => null,
      (err: unknown) => err,
    );
  });

  it("assigns the entry's exports to the named global", () => {
    // Arrange
    const sandbox: Record<string, unknown> = {};

    // Act
    runInNewContext(code, sandbox);

    // Assert
    expect((sandbox.bundleEntry as { answer: number }).answer).toBe(42);
  });

  it("refuses an entry that does not exist", () => {
    // Arrange + Act: the hook's bundle of a missing entry. Assert
    expect(refusal).toBeInstanceOf(Error);
  });
});
