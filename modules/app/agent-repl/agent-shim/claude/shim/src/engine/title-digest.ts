/**
 * engine/title-digest.ts — reading the transcript's records so the digest can
 * be computed from them.
 *
 * The one place transcript BYTES become records for the title digest. It reads
 * the whole file (the digest is a fact about the whole conversation since the
 * last boundary, not a tail), splits on newlines, and parses each line, then
 * hands the records to the pure `computeTitleDigest`. This is the I/O half a
 * converter does not do; the boundary logic is all in `convert/title-digest.ts`.
 *
 * A LINE THAT DOES NOT PARSE IS SKIPPED, not fatal — the transcript is appended
 * to by a live process, so the last line can be a partial write, and one torn
 * byte must not cost the digest the prompts before it. This mirrors
 * `engine/cold.ts`'s `readTranscriptFacts`, which reads the same file the same
 * way for a different fact.
 */
import { readFileSync } from "node:fs";
import { bindLog } from "../log.js";
import {
  computeTitleDigest,
  type TitleDigest,
  type TitleDigestRecord,
} from "../convert/title-digest.js";

const LOGGER = bindLog({ component: "shim-engine-title-digest", operation: "shim.engine.title-digest" });

/** The outcome of reading a transcript for its title digest. THE ARM IS WHY. */
export type TitleDigestRead =
  | { readonly kind: "ok"; readonly digest: TitleDigest }
  /** No transcript exists yet — the vendor has written no record. */
  | { readonly kind: "no_transcript" }
  /** The transcript exists but could not be read from disk. */
  | { readonly kind: "unreadable"; readonly detail: string };

/**
 * The title digest the transcript at `file` states.
 *
 * ABSENCE AND UNREADABILITY ARE DISTINCT ARMS, because the daemon acts on them
 * differently: no transcript is the ordinary fresh-session case and is not a
 * fault, while an unreadable one is a deployment fault the daemon should see.
 */
export function readTitleDigest(file: string): TitleDigestRead {
  let contents: string;
  try {
    contents = readFileSync(file, "utf8");
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === "ENOENT") {
      LOGGER.debug({ file }, "no transcript to gather a title digest from yet");
      return { kind: "no_transcript" };
    }
    const detail = err instanceof Error ? err.message : String(err);
    LOGGER.error({ file, cause: detail }, "the transcript could not be read for a title digest");
    return { kind: "unreadable", detail };
  }

  const records: TitleDigestRecord[] = [];
  let skipped = 0;
  for (const raw of contents.split("\n")) {
    const line = raw.trim();
    if (line === "") continue;
    try {
      records.push(JSON.parse(line) as TitleDigestRecord);
    } catch {
      skipped++;
    }
  }
  if (skipped > 0) {
    LOGGER.debug({ file, skipped }, "skipped unparsable transcript lines while gathering a title digest");
  }
  return { kind: "ok", digest: computeTitleDigest(records) };
}
