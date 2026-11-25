/**
 * THE ONE RECURSIVE FILE WALK the rule suite's source scans share. Every scan
 * that asks "which files under this directory say X" lists them here, so the
 * walk (which files it keeps, in what order, read how) cannot drift between
 * scans. It reads synchronously: a scan awaiting each of some two hundred
 * reads in turn outran the suite's timeout on a busy merge gate.
 */
import { readFileSync, readdirSync, statSync } from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { codeOf } from "../test/source-text.js";

/** The webapp package's root, the directory `src/`, `test/` and `rules/` sit in. */
export const webappRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");

/** The webapp's sources. */
export const srcDir = join(webappRoot, "src");

/** The webapp's behavior tests and their helpers. */
export const testDir = join(webappRoot, "test");

/** The webapp's rule tests and their helpers: this directory. */
export const rulesDir = join(webappRoot, "rules");

/**
 * Every file of the webapp's own test code whose path ends with one of
 * SUFFIXES: `test/` first, then `rules/`, each in `filesUnder` order. A scan
 * that holds the TEST code to a rule walks both, since the rule tests moved
 * out of `test/` are test code all the same.
 */
export function testCodeFiles(suffixes: readonly string[] = [".ts"]): string[] {
  return [...filesUnder(testDir, suffixes), ...filesUnder(rulesDir, suffixes)];
}

/**
 * Every file under DIR whose path ends with one of SUFFIXES, recursively, as
 * full paths: each directory's entries in `readdirSync` order, a
 * subdirectory's files in place of its name.
 */
export function filesUnder(dir: string, suffixes: readonly string[] = [".ts"]): string[] {
  return readdirSync(dir).flatMap((name) => {
    const full = join(dir, name);
    if (statSync(full).isDirectory()) return filesUnder(full, suffixes);
    return suffixes.some((suffix) => full.endsWith(suffix)) ? [full] : [];
  });
}

/**
 * Every .ts file under DIR as code, keyed by its path under DIR, comments
 * stripped (`codeOf`): what a scan reads when it asks which sources CALL
 * something, as opposed to which merely mention it.
 */
export function codeUnder(dir: string): ReadonlyMap<string, string> {
  return new Map(filesUnder(dir).map((path) => [relative(dir, path), codeOf(readFileSync(path, "utf8"))]));
}
