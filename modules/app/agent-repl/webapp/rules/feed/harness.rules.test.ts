/**
 * THE FEED SUITE'S SETUP GOES THROUGH THE HARNESS (test/feed/harness.ts): a
 * scan of test/feed/feed.test.ts.
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";

/**
 * THE FEED SUITE SHARES THE SETUP: a hand-rolled root channel or scroll box is
 * the drift the helpers exist to prevent.
 */
describe("the feed suite's setup call sites", () => {
  const suite = readFileSync(join(process.cwd(), "test/feed/feed.test.ts"), "utf8");

  it("registers no root channel by hand", () => {
    // Arrange / Act
    const handRolled = /channels\.set\(\s*"tok:root"/.test(suite);
    // Assert
    expect([handRolled, suite.includes("liveRootHarness(")]).toEqual([false, true]);
  });

  it("builds no scroll box around a host by hand", () => {
    // Arrange / Act
    const handRolled = /const scroll = document\.createElement\("div"\);\s*const host = document\.createElement\("div"\);\s*scroll\.append\(host\);/.test(suite);
    // Assert
    expect([handRolled, suite.includes("scrollHost()")]).toEqual([false, true]);
  });
});
