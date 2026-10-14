/**
 * Vendor transcripts for the fork suites: a parent session written where the
 * vendor keeps it, and the ids a fork mints, made deterministic.
 */
import { mkdirSync, writeFileSync } from "node:fs";
import path from "node:path";
import { transcriptPath } from "../src/engine/vendor-paths.js";

/**
 * Write `lines` as the vendor's JSONL transcript of `sessionId` run in `cwd`
 * under `configDir`, creating its project directory, and answer its path.
 */
export function writeTranscript(
  configDir: string,
  cwd: string,
  sessionId: string,
  lines: ReadonlyArray<Record<string, unknown>>,
): string {
  const file = transcriptPath(configDir, cwd, sessionId);
  mkdirSync(path.dirname(file), { recursive: true });
  writeFileSync(file, `${lines.map((line) => JSON.stringify(line)).join("\n")}\n`);
  return file;
}

/** A minter answering `id-1`, `id-2`, ... in order, so a fork's ids are known. */
export function sequentialIds(): () => string {
  let next = 0;
  return () => `id-${String(++next)}`;
}
