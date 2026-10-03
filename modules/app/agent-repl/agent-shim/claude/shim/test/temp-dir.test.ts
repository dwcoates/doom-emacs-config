import { existsSync, readFileSync, readdirSync, statSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, relative } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

import { testTempDir } from "./temp-dir.js";

describe("testTempDir", () => {
  let made = "";

  it("makes the directory under the run's temp root", () => {
    // Act
    made = testTempDir("temp-dir-test-");

    // Assert
    expect([existsSync(made), dirname(made)]).toEqual([true, tmpdir()]);
  });

  // ORDER-DEPENDENT ON PURPOSE: removal happens after a test finishes, so only
  // the test after it can see it. Tests in one block run in declared order.
  it("has removed the previous test's directory once that test finished", () => {
    // Assert
    expect([made !== "", existsSync(made)]).toEqual([true, false]);
  });
});

/**
 * EVERY TEMP DIRECTORY A TEST MAKES IS MADE UNDER `os.tmpdir()`, which a run
 * points at the root it owns and removes (test/run-tmp-root.ts). A `mkdtemp`
 * anywhere else would escape that removal and leak into a directory the run
 * never cleans, which is how the shared user temp directory reached 856,127
 * entries.
 */
describe("the temp directories tests make", () => {
  const here = dirname(fileURLToPath(import.meta.url));
  const pkg = dirname(here);
  const ROOTED = /mkdtemp(?:Sync)?\(\s*(?:path\.)?join\(\s*(?:os\.)?tmpdir\(\)/;
  const CALL = /mkdtemp(?:Sync)?\(/;
  /** The file that MAKES the run's root, and this file, whose patterns name the call. */
  const EXEMPT = new Set([join(here, "run-tmp-root.ts"), fileURLToPath(import.meta.url)]);

  function sources(dir: string): string[] {
    return readdirSync(dir).flatMap((name) => {
      const full = join(dir, name);
      if (name === "node_modules") return [];
      if (statSync(full).isDirectory()) return sources(full);
      return /\.(ts|mjs)$/.test(name) ? [full] : [];
    });
  }

  it("roots every mkdtemp in test/ and scripts/ at os.tmpdir()", () => {
    // Arrange
    const files = [...sources(here), ...sources(join(pkg, "scripts"))].filter((f) => !EXEMPT.has(f));

    // Act
    const unrooted = files.flatMap((file) =>
      readFileSync(file, "utf8")
        .split("\n")
        .map((line, index) => ({ line, at: `${relative(pkg, file)}:${String(index + 1)}` }))
        .filter(({ line }) => !/^\s*(\*|\/\/)/.test(line) && CALL.test(line) && !ROOTED.test(line))
        .map(({ at }) => at),
    );

    // Assert
    expect(unrooted).toEqual([]);
  });
});
