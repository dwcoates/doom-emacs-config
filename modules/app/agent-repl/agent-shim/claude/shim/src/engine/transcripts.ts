/**
 * engine/transcripts.ts — every vendor conversation filed under THIS shim's
 * working directory, read from the transcripts' own lines.
 *
 * WHY THE SHIM. Where the vendor files a conversation, how the file is named,
 * and what its lines mean are the vendor adapter's knowledge — the same
 * knowledge `engine/cold.ts` already holds for the one conversation this shim
 * is bound to. ReadTranscripts is that knowledge applied to the DIRECTORY, so
 * a person can choose which of its conversations a workspace runs.
 *
 * IT SPENDS NOTHING. No query is started and no model is called: every figure
 * comes from {@link readTranscriptFacts}, which is the reader `cold.ts` uses
 * for the bound conversation. There is ONE reader of a transcript's facts, so
 * the chooser's list and the cold gate can never state different things about
 * one conversation.
 *
 * ONE CORRUPT TRANSCRIPT MUST NOT DENY THE CHOICE. A file that cannot be read
 * is LEFT OUT of the list with a record in the log, never a failure of the
 * whole verb — the proto says so, and the alternative is that one torn file in
 * a directory of twenty makes the other nineteen unreachable.
 */
import { readdirSync, statSync } from "node:fs";
import path from "node:path";
import { bindLog } from "../log.js";
import { cwdSlug, readTranscriptFacts, type TranscriptFacts } from "./cold.js";

const LOGGER = bindLog({
  component: "shim-engine-transcripts",
  operation: "shim.engine.transcripts",
});

/**
 * THE QUIET WINDOW: two minutes.
 *
 * A transcript appended to more recently than this is reported ACTIVE, which
 * the daemon refuses a bind on, because two writers on one conversation is
 * data loss. The shim knows no process table — the file's mtime is the only
 * evidence there is — so the window is chosen to err toward refusing:
 *
 *   - a live agent can sit silent for a while inside one turn (a long tool
 *     call writes nothing), so a window of seconds would call a conversation
 *     someone is in the middle of "quiet";
 *   - a conversation a person actually wants to bind to was abandoned minutes
 *     or hours ago, so two minutes costs them nothing.
 *
 * It is reported on the wire (`quiet_after_ms`) so the daemon's refusal can
 * state the rule it applied rather than a bare verdict.
 */
export const TRANSCRIPT_QUIET_AFTER_MS = 2 * 60 * 1000;

/** The vendor's suffix for a session transcript. */
const TRANSCRIPT_SUFFIX = ".jsonl";

/** One conversation found in the directory, as its own file states it. */
export interface TranscriptSummary {
  /** The vendor's identity for the conversation — the file's basename. */
  readonly vendorSessionId: string;
  /** What the transcript's lines state. */
  readonly facts: TranscriptFacts;
  /** When the file was last appended to, in epoch milliseconds. */
  readonly mtimeMs: number;
  /**
   * When something is writing to it right now, the instant of that write.
   * Absent when the file has been quiet for {@link TRANSCRIPT_QUIET_AFTER_MS}.
   */
  readonly activeAtMs?: number;
  /** Whether this is the conversation THIS shim is bound to. */
  readonly bound: boolean;
}

/** The outcome of reading the directory. THE ARM IS WHY. */
export type TranscriptsRead =
  | { readonly kind: "ok"; readonly transcripts: readonly TranscriptSummary[] }
  /** Nothing has ever run in this working directory. */
  | { readonly kind: "no_project_dir"; readonly searchedPath: string }
  /** The directory exists and could not be listed. */
  | {
      readonly kind: "unreadable";
      readonly searchedPath: string;
      readonly detail: string;
    };

/** The vendor's project directory for one working directory. */
function projectDir(configDir: string, cwd: string): string {
  return path.join(configDir, "projects", cwdSlug(cwd));
}

/**
 * Every conversation filed under `cwd`, newest activity first.
 *
 * AN EMPTY LIST IS A SUCCESS and is distinct from {@link TranscriptsRead}'s
 * `no_project_dir`: a directory that exists and holds nothing is an answer,
 * while a directory that was never created says nothing has ever run here.
 *
 * `boundVendorSessionId` is this shim's own conversation, which is INCLUDED
 * and flagged rather than omitted — the point of the verb is to find the
 * others, and a list that silently dropped the current one would make the
 * workspace's own conversation the one thing a person cannot see.
 */
export function readTranscripts(
  configDir: string,
  cwd: string,
  boundVendorSessionId: string | undefined,
  nowMs: number,
): TranscriptsRead {
  const searchedPath = projectDir(configDir, cwd);
  let names: string[];
  try {
    names = readdirSync(searchedPath);
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === "ENOENT") {
      LOGGER.debug(
        { searched_path: searchedPath },
        "no vendor project directory for this working directory",
      );
      return { kind: "no_project_dir", searchedPath };
    }
    const detail = err instanceof Error ? err.message : String(err);
    // error: the directory exists and the shim may not list it, which is a
    // deployment fault the daemon has to be able to name.
    LOGGER.error(
      { searched_path: searchedPath, cause: detail },
      "the vendor project directory could not be listed",
    );
    return { kind: "unreadable", searchedPath, detail };
  }

  const transcripts: TranscriptSummary[] = [];
  let skipped = 0;
  for (const name of names) {
    if (!name.endsWith(TRANSCRIPT_SUFFIX)) continue;
    const vendorSessionId = name.slice(0, -TRANSCRIPT_SUFFIX.length);
    if (vendorSessionId === "") continue;
    const file = path.join(searchedPath, name);
    const summary = summarize(file, vendorSessionId, boundVendorSessionId, nowMs);
    if (summary === undefined) {
      skipped++;
      continue;
    }
    transcripts.push(summary);
  }
  if (skipped > 0) {
    // warn: a defect because a conversation the person could have chosen is missing from their list.
    LOGGER.warn(
      { searched_path: searchedPath, skipped },
      "left unreadable transcripts out of the conversation list",
    );
  }
  transcripts.sort((a, b) => activityAtMs(b) - activityAtMs(a));
  LOGGER.debug(
    { searched_path: searchedPath, found: transcripts.length, skipped },
    "read the conversations filed under this working directory",
  );
  return { kind: "ok", transcripts };
}

/**
 * One file's summary, or absence when it could not be read.
 *
 * ABSENCE IS RECORDED BY THE CALLER, once, with a count: a directory whose
 * every file is unreadable would otherwise emit one warning per file and bury
 * the fact that the list came back empty.
 */
function summarize(
  file: string,
  vendorSessionId: string,
  boundVendorSessionId: string | undefined,
  nowMs: number,
): TranscriptSummary | undefined {
  let mtimeMs: number;
  try {
    // TRUNCATED, BECAUSE THE WIRE FIELD IS AN int64. `mtimeMs` is a FLOAT —
    // a filesystem records sub-millisecond precision — and protobuf-es
    // refuses a non-integer outright ("cannot be converted to a BigInt"),
    // which failed the whole verb on the first transcript it stat'd.
    mtimeMs = Math.trunc(statSync(file).mtimeMs);
  } catch (err) {
    LOGGER.info(
      { file, cause: err instanceof Error ? err.message : String(err) },
      "left a transcript out of the conversation list: it could not be stat'd",
    );
    return undefined;
  }
  let facts: TranscriptFacts | undefined;
  try {
    facts = readTranscriptFacts(file);
  } catch (err) {
    LOGGER.info(
      { file, cause: err instanceof Error ? err.message : String(err) },
      "left a transcript out of the conversation list: it could not be read",
    );
    return undefined;
  }
  if (facts === undefined) {
    // The file was listed and then vanished before it was read — a clear
    // rotating a conversation while this verb runs does exactly that.
    LOGGER.info({ file }, "left a transcript out of the conversation list: it is gone");
    return undefined;
  }
  const active = nowMs - mtimeMs < TRANSCRIPT_QUIET_AFTER_MS;
  return {
    vendorSessionId,
    facts,
    mtimeMs,
    ...(active ? { activeAtMs: mtimeMs } : {}),
    bound: boundVendorSessionId !== undefined && boundVendorSessionId === vendorSessionId,
  };
}

/**
 * The instant a conversation was last ACTIVE, for ordering.
 *
 * The last request is what a person means by "the one I was just in", and the
 * mtime is the fallback for a conversation that never reached the model —
 * which would otherwise sort as if it were from 1970.
 */
function activityAtMs(summary: TranscriptSummary): number {
  return summary.facts.lastRequestAtMs === 0 ? summary.mtimeMs : summary.facts.lastRequestAtMs;
}
