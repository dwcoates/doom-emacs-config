/**
 * engine/compaction.ts — the throwaway session that rewrites the transcript.
 *
 * RESPONSIBILITY. Compaction summarizes a conversation and extends its
 * transcript with the two records the vendor's own loader understands, and it
 * is driven by a SEPARATE, throwaway query rather than the live one: the live
 * query owns the session's identity, its locks and its in-flight turn, and
 * handing it a summarization prompt would put harness work inside the user's
 * conversation.
 *
 * WHO ASKS FOR IT. Only `SessionColdCompact`, the cold gate's remediation for
 * a cold context. Hibernation never compacts.
 *
 * # The two records, mimicked from OBSERVED lines
 *
 * The vendor's per-line field union is internal and undocumented, so the writer
 * copies the shape of real lines rather than inventing one
 * (`testdata/corpus/transcript-lines/system-compact_boundary.jsonl` and the
 * `isCompactSummary` user line that follows it in real transcripts):
 *
 *   1. a `system`/`compact_boundary` line whose `parentUuid` is NULL and whose
 *      `logicalParentUuid` names the last pre-compaction record — that pair is
 *      what tells the loader the chain restarts here;
 *   2. a `user` line carrying the summary, marked `isCompactSummary: true` and
 *      `isVisibleInTranscriptOnly: true`, parented to the boundary.
 *
 * IT APPENDS; IT NEVER TRUNCATES. That is what the observed lines say
 * compaction is — the boundary is a marker the loader splices at, not a
 * deletion — and it means a compaction that goes wrong costs nothing the
 * transcript backup could not already restore.
 *
 * THE AMBIENT FIELDS ARE COPIED, NOT COMPOSED. `cwd`, `version`, `gitBranch`,
 * `userType`, `entrypoint` and `slug` are taken from the transcript's own last
 * line. Composing them here would mean guessing at a union the vendor has not
 * published, and a guessed field is how a written line becomes an unloadable
 * transcript.
 */
import { randomUUID } from "node:crypto";
import { appendFileSync, readFileSync } from "node:fs";
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";

const LOGGER = bindLog({ component: "shim-engine-compaction", operation: "shim.engine.compaction" });

/** The sentence the vendor's own compact summary begins with. */
export const COMPACT_SUMMARY_PREFIX =
  "This session is being continued from a previous conversation that ran out of context. " +
  "The summary below covers the earlier portion of the conversation.\n\nSummary:\n";

/** The `content` of the boundary line, verbatim from the observed record. */
export const COMPACT_BOUNDARY_CONTENT = "Conversation compacted";

/** The instruction the throwaway session is given, per requested scope. */
export function compactionPrompt(scope: conversationv1.SessionCompactScope): string {
  const focus =
    scope === conversationv1.SessionCompactScope.PROMPTS
      ? "Summarize ONLY what the user asked for across this conversation: every request, constraint and correction, in order."
      : scope === conversationv1.SessionCompactScope.RESPONSES
        ? "Summarize ONLY what the assistant did and produced across this conversation: decisions taken, files changed, results reached."
        : "Summarize this conversation in full: what was asked, what was decided, what was done, what remains.";
  return (
    `${focus}\n\n` +
    "Write the summary as the continuation notes a fresh session needs in order to pick the work up " +
    "without re-reading the transcript. Be specific about file paths, identifiers and outstanding work. " +
    "Output the summary and nothing else."
  );
}

/** The ambient fields every line of one transcript shares. */
interface TranscriptAmbient {
  readonly sessionId: string;
  readonly cwd?: string;
  readonly version?: string;
  readonly gitBranch?: string;
  readonly userType?: string;
  readonly entrypoint?: string;
  readonly slug?: string;
  /** The uuid of the transcript's last record — the boundary's logical parent. */
  readonly lastUuid?: string;
}

/**
 * Read the ambient fields off the transcript, each from the LAST line to state
 * it.
 *
 * The LAST value, not the first: `gitBranch` and `slug` change over a
 * conversation's life, and the records this writer appends belong at its end.
 *
 * PER FIELD, AND NOT PER LINE. A transcript's last line is very often not a
 * conversation record at all — the CLI writes `last-prompt`, `queue-operation`
 * and `summary` bookkeeping lines carrying `sessionId` and nothing else — and
 * while this function rebuilt the whole accumulator from each line, ONE such
 * line at the end erased every field the conversation had already stated.
 *
 * That is not hypothetical: all 63 compactions on the owner's
 * chess960-review-failures-enm transcript (2026-09-14) landed right after a
 * `last-prompt` line, and every boundary the shim wrote there carries
 * `sessionId` alone — no `cwd`, `version`, `gitBranch`, `userType`,
 * `entrypoint` or `slug`, and, worst of the set, no `logicalParentUuid`, which
 * is the field the vendor's own boundary uses to say where the chain restarts.
 * The observed vendor boundary
 * (`testdata/corpus/transcript-lines/system-compact_boundary.jsonl`) carries
 * all seven.
 */
export function readAmbient(file: string): TranscriptAmbient {
  const contents = readFileSync(file, "utf8");
  let ambient: TranscriptAmbient = { sessionId: "" };
  for (const raw of contents.split("\n")) {
    const line = raw.trim();
    if (line === "") continue;
    let record: Record<string, unknown>;
    try {
      record = JSON.parse(line) as Record<string, unknown>;
    } catch {
      continue;
    }
    const pick = (key: string): string | undefined =>
      typeof record[key] === "string" ? (record[key]) : undefined;
    // MERGED ONTO WHAT IS ALREADY KNOWN. A line that does not state a field
    // says nothing about it; only a line that DOES may change it.
    ambient = {
      ...ambient,
      sessionId: pick("sessionId") ?? ambient.sessionId,
      ...(pick("cwd") === undefined ? {} : { cwd: pick("cwd") }),
      ...(pick("version") === undefined ? {} : { version: pick("version") }),
      ...(pick("gitBranch") === undefined ? {} : { gitBranch: pick("gitBranch") }),
      ...(pick("userType") === undefined ? {} : { userType: pick("userType") }),
      ...(pick("entrypoint") === undefined ? {} : { entrypoint: pick("entrypoint") }),
      ...(pick("slug") === undefined ? {} : { slug: pick("slug") }),
      // THE LAST RECORD THAT IS A CHAIN NODE, which is the last one carrying a
      // `uuid`. The bookkeeping lines have none, and the boundary's
      // `logicalParentUuid` has to name a record the loader can find.
      ...(pick("uuid") === undefined ? {} : { lastUuid: pick("uuid") }),
    };
  }
  return ambient;
}

/** Everything the two written lines need that the transcript does not state. */
interface CompactionLinesSpec {
  readonly ambient: TranscriptAmbient;
  readonly summary: string;
  readonly preTokens: number;
  readonly postTokens: number;
  readonly durationMs: number;
  readonly trigger: "manual" | "auto";
  readonly atMs: number;
  /**
   * The permission mode THE SESSION is in, in the vendor's own spelling.
   *
   * It is written onto the summary `user` line, and that is a correction rather
   * than a decoration. The summarizing query runs under `plan` so it can take
   * no tools, it RESUMES the user's own vendor session id, and the vendor
   * records `permissionMode` on every `user` record it writes — so the
   * throwaway leaves `plan` as the last mode the transcript states. A resume
   * restores the conversation's posture from exactly that field
   * (`engine/cold.ts`), so a session resumed straight after a compaction
   * came back in PLAN MODE, which nobody chose: observed on a headless sandbox
   * run's own revival, where the topbar read `plan` and the revived turn ran
   * under it. Writing the session's own mode on the last record the compaction
   * appends puts the transcript's final word back in the user's hands.
   */
  readonly permissionMode: string;
  readonly newUuid?: () => string;
}

/** The two records, as objects — the base function the shape tests assert. */
export function compactionLines(spec: CompactionLinesSpec): {
  boundary: Record<string, unknown>;
  summary: Record<string, unknown>;
} {
  const mint = spec.newUuid ?? randomUUID;
  const timestamp = new Date(spec.atMs).toISOString();
  const boundaryUuid = mint();
  const ambientFields = {
    ...(spec.ambient.userType === undefined ? {} : { userType: spec.ambient.userType }),
    ...(spec.ambient.entrypoint === undefined ? {} : { entrypoint: spec.ambient.entrypoint }),
    ...(spec.ambient.cwd === undefined ? {} : { cwd: spec.ambient.cwd }),
    sessionId: spec.ambient.sessionId,
    ...(spec.ambient.version === undefined ? {} : { version: spec.ambient.version }),
    ...(spec.ambient.gitBranch === undefined ? {} : { gitBranch: spec.ambient.gitBranch }),
    ...(spec.ambient.slug === undefined ? {} : { slug: spec.ambient.slug }),
  };
  const boundary: Record<string, unknown> = {
    parentUuid: null,
    ...(spec.ambient.lastUuid === undefined ? {} : { logicalParentUuid: spec.ambient.lastUuid }),
    isSidechain: false,
    type: "system",
    subtype: "compact_boundary",
    content: COMPACT_BOUNDARY_CONTENT,
    isMeta: false,
    timestamp,
    uuid: boundaryUuid,
    level: "info",
    compactMetadata: {
      trigger: spec.trigger,
      preTokens: spec.preTokens,
      postTokens: spec.postTokens,
      durationMs: spec.durationMs,
    },
    ...ambientFields,
  };
  const summary: Record<string, unknown> = {
    parentUuid: boundaryUuid,
    isSidechain: false,
    promptId: mint(),
    type: "user",
    message: { role: "user", content: `${COMPACT_SUMMARY_PREFIX}${spec.summary}` },
    isVisibleInTranscriptOnly: true,
    isCompactSummary: true,
    permissionMode: spec.permissionMode,
    uuid: mint(),
    timestamp,
    ...ambientFields,
  };
  return { boundary, summary };
}

/** Append the two records to the transcript. */
export function appendCompactionLines(
  file: string,
  lines: { boundary: Record<string, unknown>; summary: Record<string, unknown> },
): void {
  appendFileSync(file, `${JSON.stringify(lines.boundary)}\n${JSON.stringify(lines.summary)}\n`, "utf8");
  LOGGER.info(
    { transcript: file, boundary_uuid: lines.boundary.uuid, summary_uuid: lines.summary.uuid },
    "appended the compact_boundary and summary records the vendor's loader splices at",
  );
}

// ---------------------------------------------------------------------------
// the page line
// ---------------------------------------------------------------------------

/** The `context_cut` page line for a completed compaction. */
export function contextCutCompacted(options: {
  readonly summary: string;
  readonly tokensBefore: number;
  readonly tokensAfter: number;
  readonly durationMs: number;
  readonly requested: boolean;
}): conversationv1.ContextCut {
  return create(conversationv1.ContextCutSchema, {
    cut: {
      case: "compacted",
      value: create(conversationv1.ContextCompactedSchema, {
        summary: create(conversationv1.AgentResponseProseSchema, { markdown: options.summary }),
        tokens: create(conversationv1.ContextTokenDeltaSchema, {
          tokensBefore: BigInt(options.tokensBefore),
          tokensAfter: BigInt(options.tokensAfter),
        }),
        durationMs: BigInt(options.durationMs),
        trigger: options.requested
          ? { case: "requested", value: create(conversationv1.ContextCompactionRequestedSchema, {}) }
          : { case: "automatic", value: create(conversationv1.ContextCompactionAutomaticSchema, {}) },
      }),
    },
  });
}

/** The `context_cut` page line for a compaction that failed, with the vendor's wording. */
export function contextCutFailed(error: string): conversationv1.ContextCut {
  return create(conversationv1.ContextCutSchema, {
    cut: {
      case: "compactionFailed",
      value: create(conversationv1.ContextCompactionFailedSchema, { error }),
    },
  });
}

/**
 * The `context_cut` page line for a `/clear`.
 *
 * `ContextCleared.tokens` is a KNOWN-OPEN arm: nothing in the declared surface
 * states how many tokens a clear discarded, so it stays unset rather than being
 * filled with a number derived from somewhere else.
 */
export function contextCleared(): conversationv1.ContextCut {
  return create(conversationv1.ContextCutSchema, {
    cut: { case: "cleared", value: create(conversationv1.ContextClearedSchema, {}) },
  });
}

