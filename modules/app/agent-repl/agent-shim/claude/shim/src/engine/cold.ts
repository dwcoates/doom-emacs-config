/**
 * engine/cold.ts — the cold gate, read off the transcript before the SDK is
 * touched.
 *
 * RESPONSIBILITY. A resumed conversation whose prompt cache has lapsed costs
 * FULL PRICE to continue, and the user must be told BEFORE it is spent, not
 * after. This module reads the vendor transcript to answer four questions
 * WITHOUT starting a query or making a model call: how many context tokens the
 * conversation holds, when its last API request was (so cache lapse can be
 * judged), which cache tier that request bought, and which model and permission
 * mode it was last running under (so a resume restores the conversation's own
 * posture rather than a default).
 *
 * THE REFUSAL IS THE POINT. `StartSession` and `SetSessionModel` answer
 * `SessionCold` — with the cost and the reason — until the caller names a
 * remediation. It is a refusal and not a warning because a warning arrives
 * after the money is spent.
 *
 * WHERE EVERY FIGURE COMES FROM (corpus-verified, not inferred):
 *   - context tokens: the LAST assistant line's `message.usage`, summed as
 *     `cache_read_input_tokens + cache_creation_input_tokens + input_tokens` —
 *     the three parts of what that request actually sent.
 *   - the request instant: that same line's `timestamp`.
 *   - the cache tier: `message.usage.cache_creation.ephemeral_1h_input_tokens`
 *     — non-zero means the 1-hour tier was bought, zero means the 5-minute one.
 *     Both keys are present on every cached request in the corpus (7,310 of
 *     each across the sampled transcripts).
 *   - the model: that line's `message.model`.
 *   - the permission mode: the last `user` line's `permissionMode` (the vendor
 *     records it per user record and the SDK does not restore it on resume).
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";

const LOGGER = bindLog({ component: "shim-engine-cold", operation: "shim.engine.cold" });

/** The 5-minute ephemeral cache tier, in milliseconds. */
export const CACHE_TTL_5M_MS = 5 * 60 * 1000;
/** The 1-hour ephemeral cache tier, in milliseconds. */
export const CACHE_TTL_1H_MS = 60 * 60 * 1000;

/**
 * The vendor's project-directory slug: every byte outside `[A-Za-z0-9]`
 * becomes `-`.
 *
 * Underscores included, case preserved, existing dashes untouched — verified
 * against the live `~/.claude/projects` tree, where `/Users/x/.config/y`
 * appears as `-Users-x--config-y`.
 */
export function cwdSlug(cwd: string): string {
  return cwd.replace(/[^A-Za-z0-9]/g, "-");
}

/** The session transcript the vendor writes for one conversation. */
export function transcriptPath(configDir: string, cwd: string, vendorSessionId: string): string {
  return path.join(configDir, "projects", cwdSlug(cwd), `${vendorSessionId}.jsonl`);
}

/** What the transcript states about the conversation's last request. */
export interface TranscriptFacts {
  /** Tokens the last request carried: cache reads + cache writes + fresh input. */
  readonly contextTokens: number;
  /** When that request happened, in epoch milliseconds. */
  readonly lastRequestAtMs: number;
  /** The model that answered it, when the transcript states one. */
  readonly lastModel?: string;
  /** The permission mode the last user record ran under, when stated. */
  readonly lastPermissionMode?: string;
  /** Which ephemeral tier that request bought. */
  readonly cacheTtlMs: number;
}

interface TranscriptUsage {
  input_tokens?: number;
  cache_creation_input_tokens?: number;
  cache_read_input_tokens?: number;
  cache_creation?: { ephemeral_1h_input_tokens?: number; ephemeral_5m_input_tokens?: number };
}

interface TranscriptLine {
  type?: string;
  timestamp?: string;
  permissionMode?: string;
  message?: { model?: string; usage?: TranscriptUsage };
}

/**
 * Read the transcript's facts, or answer absence when there is no transcript.
 *
 * Absence is a legitimate answer, not an error: `StartSession{resume}` of an id
 * with no transcript is `StartSessionUnknownSession`, and the caller decides
 * that — this module only reports what it found.
 *
 * A LINE THAT DOES NOT PARSE IS SKIPPED, not fatal. The transcript is appended
 * to by a live process, so the last line can be a partial write; refusing to
 * start a session over one torn byte would be a worse failure than judging the
 * cache from the line before it.
 */
export function readTranscriptFacts(file: string): TranscriptFacts | undefined {
  let contents: string;
  try {
    contents = readFileSync(file, "utf8");
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === "ENOENT") {
      LOGGER.log({ file }, "no transcript for this vendor session id");
      return undefined;
    }
    throw err;
  }
  let contextTokens = 0;
  let lastRequestAtMs = 0;
  let lastModel: string | undefined;
  let lastPermissionMode: string | undefined;
  let cacheTtlMs = CACHE_TTL_5M_MS;
  let skipped = 0;
  for (const raw of contents.split("\n")) {
    const line = raw.trim();
    if (line === "") continue;
    let record: TranscriptLine;
    try {
      record = JSON.parse(line) as TranscriptLine;
    } catch {
      skipped++;
      continue;
    }
    if (record.type === "user" && typeof record.permissionMode === "string") {
      lastPermissionMode = record.permissionMode;
    }
    if (record.type !== "assistant") continue;
    const usage = record.message?.usage;
    if (usage === undefined) continue;
    contextTokens =
      (usage.cache_read_input_tokens ?? 0) +
      (usage.cache_creation_input_tokens ?? 0) +
      (usage.input_tokens ?? 0);
    cacheTtlMs =
      (usage.cache_creation?.ephemeral_1h_input_tokens ?? 0) > 0
        ? CACHE_TTL_1H_MS
        : CACHE_TTL_5M_MS;
    if (typeof record.message?.model === "string") lastModel = record.message.model;
    if (typeof record.timestamp === "string") {
      const at = Date.parse(record.timestamp);
      if (!Number.isNaN(at)) lastRequestAtMs = at;
    }
  }
  if (skipped > 0) {
    LOGGER.log(
      { level: "warn", file, skipped_lines: skipped },
      "skipped unparsable transcript lines while reading the cold-gate facts",
    );
  }
  const facts: TranscriptFacts = {
    contextTokens,
    lastRequestAtMs,
    cacheTtlMs,
    ...(lastModel === undefined ? {} : { lastModel }),
    ...(lastPermissionMode === undefined ? {} : { lastPermissionMode }),
  };
  LOGGER.log({ file, ...facts }, "read the cold-gate facts from the transcript");
  return facts;
}

/** Why a continuation would be cold, or absence when it would be warm. */
type ColdReason = "lapsed" | "model_switch";

/**
 * Judge the cache.
 *
 * A model switch is cold UNCONDITIONALLY when the models differ, because the
 * prompt cache is per model: continuing under a different model reads nothing
 * back no matter how recent the last request was.
 */
export function judgeCold(
  facts: TranscriptFacts,
  nowMs: number,
  requestedModel: string | undefined,
): ColdReason | undefined {
  if (
    requestedModel !== undefined &&
    requestedModel !== "" &&
    facts.lastModel !== undefined &&
    facts.lastModel !== requestedModel
  ) {
    return "model_switch";
  }
  if (facts.lastRequestAtMs === 0) return undefined;
  return nowMs - facts.lastRequestAtMs > facts.cacheTtlMs ? "lapsed" : undefined;
}

/** The refusal message, fully populated — every field the proto declares. */
export function sessionCold(
  facts: TranscriptFacts,
  reason: ColdReason,
  requestedModel: string,
): conversationv1.SessionCold {
  return create(conversationv1.SessionColdSchema, {
    contextTokens: BigInt(facts.contextTokens),
    lastRequestAtMs: BigInt(facts.lastRequestAtMs),
    requestedModel: create(conversationv1.AgentModelSchema, { name: requestedModel }),
    reason:
      reason === "lapsed"
        ? {
            case: "lapsed",
            value: create(conversationv1.SessionColdLapsedSchema, {
              cacheTtlMs: BigInt(facts.cacheTtlMs),
            }),
          }
        : { case: "modelSwitch", value: create(conversationv1.SessionColdModelSwitchSchema, {}) },
  });
}
