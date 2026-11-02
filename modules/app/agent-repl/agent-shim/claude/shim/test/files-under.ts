/**
 * test/files-under.ts — the shim tests' one recursive walk of a directory.
 */
import { readdirSync, statSync } from "node:fs";
import { join } from "node:path";

/** Every file under DIR, recursively, as paths under DIR, in sorted order. */
export function filesUnder(dir: string): string[] {
  return readdirSync(dir)
    .sort()
    .flatMap((name) => {
      const path = join(dir, name);
      return statSync(path).isDirectory() ? filesUnder(path) : [path];
    });
}
