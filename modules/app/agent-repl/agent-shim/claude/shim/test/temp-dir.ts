/**
 * test/temp-dir.ts — a temp directory owned by the test that made it.
 *
 * Removed when that test finishes, passed or failed, so a file making one per
 * test (a session rig, a fake vendor drive) holds only the current test's
 * directory rather than every one it ever made. The run's own root
 * (test/run-tmp-root.ts) still removes anything left behind; this keeps the
 * root small while the run is going.
 *
 * Must be called inside a running test: `onTestFinished` refuses anywhere
 * else, which is the point, since nothing else would ever remove it.
 */
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { onTestFinished } from "vitest";

export function testTempDir(prefix: string): string {
  const dir = mkdtempSync(join(tmpdir(), prefix));
  // `force` forgives only a directory the test already removed itself.
  onTestFinished(() => rmSync(dir, { recursive: true, force: true }));
  return dir;
}
