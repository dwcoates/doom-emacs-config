/**
 * WHO PAYS THE COLD BOOT (`test/integration/harness.ts`): every integration
 * file that boots the app pays its first boot in `bootColdOnce`'s
 * `beforeAll`, never in a test body.
 */
import { readFileSync, readdirSync } from "node:fs";
import { resolve } from "node:path";
import { describe, expect, it } from "vitest";

/** test/integration's test files that boot the app, read off disk. */
function bootingFiles(): { file: string; source: string }[] {
  const dir = resolve(process.cwd(), "test/integration");
  return readdirSync(dir)
    .filter((file) => file.endsWith(".test.ts"))
    .map((file) => ({ file, source: readFileSync(resolve(dir, file), "utf8") }))
    .filter(({ source }) => source.includes("startHarness("));
}

describe("the cold boot", () => {
  it("finds the files that boot the app", () => {
    // Arrange / Act: the scan the per-file check below runs over.
    const files = bootingFiles().map(({ file }) => file);

    // Assert: a scan that found nothing would pass every file vacuously.
    expect(files).toContain("harness.self.test.ts");
  });

  it.each(bootingFiles())("is paid before the first test of $file", ({ source }) => {
    // Arrange / Act: the file's own top-level statements.
    const calls = source.match(/^bootColdOnce\(\);$/gm) ?? [];

    // Assert: exactly one top-level call, so the file's first boot is warm.
    expect(calls).toHaveLength(1);
  });
});
