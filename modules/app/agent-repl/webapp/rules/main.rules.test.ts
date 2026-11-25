/** Every block of test/main.test.ts runs its tests under the warm-boot bound. */
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { describe, expect, test } from "vitest";

describe("the harness", () => {
  test("every block in this file runs its tests under the warm-boot bound", () => {
    // EVERY TEST HERE BOOTS, so every block carries BOOT_TIMEOUT_MS. One block
    // that left it off ran its warm boots under the 850ms unit global, which
    // is under the 1.45s warm boot measured on the contended host, and timed
    // out only when the full suite set loaded the machine.
    const source = readFileSync(resolve(process.cwd(), "test/main.test.ts"), "utf8");
    const blocks = source.split("\n").filter((line) => line.startsWith("describe("));

    expect(blocks.filter((line) => !line.includes("{ timeout: BOOT_TIMEOUT_MS }"))).toEqual([]);
  });
});
