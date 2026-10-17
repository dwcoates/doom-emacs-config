/**
 * THE ONE RECURSIVE FILE WALK the test suite's source scans share. Every scan
 * that asks "which files under this directory say X" lists them here, so the
 * walk (which files it keeps, in what order, read how) cannot drift between
 * scans. It reads synchronously: a scan awaiting each of some two hundred
 * reads in turn outran the suite's timeout on a busy merge gate.
 */
import { readFileSync, readdirSync, statSync } from "node:fs";
import { join, relative } from "node:path";
import { codeOf } from "./source-text.js";

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
