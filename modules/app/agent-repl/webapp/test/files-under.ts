/**
 * THE ONE RECURSIVE FILE WALK the test suite's source scans share. Every scan
 * that asks "which files under this directory say X" lists them here, so the
 * walk (which files it keeps, in what order, read how) cannot drift between
 * scans. It reads synchronously: a scan awaiting each of some two hundred
 * reads in turn outran the suite's timeout on a busy merge gate.
 */
import { readdirSync, statSync } from "node:fs";
import { join } from "node:path";

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
