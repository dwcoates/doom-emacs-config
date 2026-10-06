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
 *     — non-zero means the 1-hour tier was bought, a non-zero
 *     `ephemeral_5m_input_tokens` the 5-minute one, and neither (a request that
 *     only read the cache) keeps the tier the last write bought
 *     ({@link cacheRequestOf}). Both keys are present on every cached request
 *     in the corpus (7,310 of each across the sampled transcripts).
 *   - the model: that line's `message.model`.
 *   - the permission mode: the last `user` line's `permissionMode` (the vendor
 *     records it per user record and the SDK does not restore it on resume).
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { SYNTHETIC_MODEL } from "../model.js";
import { conversationv1 } from "../proto.js";
import {
  isClearEnvelope,
  isCompactBoundary,
  promptText,
  type TitleDigestRecord,
} from "../convert/title-digest.js";

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

/**
 * The OPENING WORDS cap: 120 characters.
 *
 * A chooser reads a LIST, one line per conversation, so the opening has to fit
 * beside an age and a size on one line of a minibuffer completion — and a
 * prompt's first sentence is what distinguishes it from its neighbours. 120 is
 * wide enough for that sentence and narrow enough that a pasted stack trace
 * cannot push the rest of the line off the screen.
 */
export const TRANSCRIPT_OPENING_MAX_CHARS = 120;

/** What the transcript states about the conversation's last request. */
export interface TranscriptFacts {
  /** Tokens the last request carried: cache reads + cache writes + fresh input. */
  readonly contextTokens: number;
  /**
   * Whether the transcript stated ANY usage at all.
   *
   * `contextTokens` is 0 both for a conversation that has never reached the
   * model and for one whose last request carried nothing, and a chooser that
   * cannot tell those apart ranks an unread conversation as the cheapest one.
   * The cold gate keeps reading `contextTokens` alone — 0 is the right floor
   * answer for it either way — and ReadTranscripts reads this to decide
   * whether to SET the field at all.
   */
  readonly sawUsage: boolean;
  /** When that request happened, in epoch milliseconds. 0 when it states none. */
  readonly lastRequestAtMs: number;
  /** The model that answered it, when the transcript states one. */
  readonly lastModel?: string;
  /** The permission mode the last user record ran under, when stated. */
  readonly lastPermissionMode?: string;
  /** Which ephemeral tier that request bought. */
  readonly cacheTtlMs: number;
  /**
   * The conversation's opening words: the beginning of its FIRST user prompt,
   * truncated to {@link TRANSCRIPT_OPENING_MAX_CHARS}. Absent when the
   * transcript holds no user prompt.
   */
  readonly opening?: string;
  /** How many user prompts the transcript holds. */
  readonly prompts: number;
  /**
   * When the most recent context boundary was a CLEAR, the instant of that
   * clear in epoch milliseconds. Absent when the most recent boundary is a
   * compaction, or when there is no boundary at all — the same last-writer-wins
   * rule `computeTitleDigest` applies to the same records.
   */
  readonly clearedAtMs?: number;
}

interface TranscriptUsage {
  input_tokens?: number;
  cache_creation_input_tokens?: number;
  cache_read_input_tokens?: number;
  cache_creation?: { ephemeral_1h_input_tokens?: number; ephemeral_5m_input_tokens?: number };
}

export interface TranscriptLine {
  type?: string;
  timestamp?: string;
  permissionMode?: string;
  isApiErrorMessage?: boolean;
  message?: { model?: string; usage?: TranscriptUsage };
}

/**
 * Whether an assistant record is one the CLI WROTE ITSELF rather than one the
 * API answered.
 *
 * A failed request, a session-limit notice, a user stop: the CLI records each
 * as an assistant message whose `model` is the `<synthetic>` marker, flagged
 * `isApiErrorMessage` when an API call failed, and carrying a ZEROED usage
 * block. None of that is a fact about the conversation. Read as one, the
 * marker became the model the next resume launched on — which the API then
 * refused, which wrote another such record, forever — and the zeroed usage read
 * a 500k-token conversation as empty, so the cold gate never fired.
 */
function isCliSynthesized(record: TranscriptLine): boolean {
  return record.isApiErrorMessage === true || record.message?.model?.trim() === SYNTHETIC_MODEL;
}

/**
 * What one assistant message states about the API request behind it: the
 * cache tier that request WROTE, or absence when it wrote none.
 *
 * ONE READING FOR THE TRANSCRIPT AND THE LIVE STREAM. The vendor's transcript
 * records and the SDK's streamed assistant messages are the same objects (the
 * captures' `stream.jsonl` carries the same `message.usage.cache_creation`
 * split the transcript does), so the cold gate reading a transcript and the
 * keep-alive cadence watching a live session judge a request by this one rule.
 *
 * Answers absence -- NOT A REQUEST -- for a record the CLI wrote itself
 * ({@link isCliSynthesized}) and for one that states no usage.
 *
 * A REQUEST THAT WROTE NOTHING STATES NO TIER (`writtenTtlMs` absent). It read
 * the whole prompt back from a cache an EARLIER request wrote, so the tier that
 * governs it is that earlier write's, and the caller carries it forward. Read
 * as the 5-minute tier, such a record (121 of ~48,700 assistant records in the
 * owner's transcripts on 2026-10-06, every one a cache read) judged
 * a 1-hour cache lapsed after five minutes. A usage that states cache writes
 * without the per-tier split is the 5-minute tier, the API's only tier before
 * the split existed.
 */
export interface CacheRequest {
  readonly writtenTtlMs: number | undefined;
}

export function cacheRequestOf(record: TranscriptLine): CacheRequest | undefined {
  if (isCliSynthesized(record)) return undefined;
  const usage = record.message?.usage;
  if (usage === undefined) return undefined;
  return { writtenTtlMs: writtenCacheTtlMs(usage) };
}

function writtenCacheTtlMs(usage: TranscriptUsage): number | undefined {
  const split = usage.cache_creation;
  if (split === undefined) return (usage.cache_creation_input_tokens ?? 0) > 0 ? CACHE_TTL_5M_MS : undefined;
  if ((split.ephemeral_1h_input_tokens ?? 0) > 0) return CACHE_TTL_1H_MS;
  if ((split.ephemeral_5m_input_tokens ?? 0) > 0) return CACHE_TTL_5M_MS;
  return undefined;
}

/**
 * One parsed transcript record, read for BOTH the cold-gate facts and the
 * chooser's facts in the same pass. The two views of a record are different
 * fields of one line, never two reads of one file.
 */
type TranscriptRecord = TranscriptLine & TitleDigestRecord;

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
      LOGGER.debug({ file }, "no transcript for this vendor session id");
      return undefined;
    }
    throw err;
  }
  let contextTokens = 0;
  let sawUsage = false;
  let lastRequestAtMs = 0;
  let lastModel: string | undefined;
  let lastPermissionMode: string | undefined;
  let cacheTtlMs = CACHE_TTL_5M_MS;
  let opening: string | undefined;
  let prompts = 0;
  let clearedAtMs: number | undefined;
  let skipped = 0;
  for (const raw of contents.split("\n")) {
    const line = raw.trim();
    if (line === "") continue;
    let record: TranscriptRecord;
    try {
      record = JSON.parse(line) as TranscriptRecord;
    } catch {
      skipped++;
      continue;
    }
    if (record.type === "user" && typeof record.permissionMode === "string") {
      lastPermissionMode = record.permissionMode;
    }
    // The boundary is LAST-WRITER-WINS in file order, exactly as the title
    // digest reads it: a compaction after a clear means the conversation is no
    // longer resuming empty.
    if (isCompactBoundary(record)) {
      clearedAtMs = undefined;
    } else if (isClearEnvelope(record)) {
      clearedAtMs = recordAtMs(record) ?? 0;
    }
    const prompt = promptText(record);
    if (prompt !== undefined) {
      prompts++;
      opening ??= prompt.slice(0, TRANSCRIPT_OPENING_MAX_CHARS);
    }
    if (record.type !== "assistant") continue;
    const request = cacheRequestOf(record);
    if (request === undefined) continue;
    const usage = record.message?.usage ?? {};
    sawUsage = true;
    contextTokens =
      (usage.cache_read_input_tokens ?? 0) +
      (usage.cache_creation_input_tokens ?? 0) +
      (usage.input_tokens ?? 0);
    // A request that wrote nothing keeps the tier the last write bought.
    cacheTtlMs = request.writtenTtlMs ?? cacheTtlMs;
    if (typeof record.message?.model === "string") lastModel = record.message.model;
    if (typeof record.timestamp === "string") {
      const at = Date.parse(record.timestamp);
      if (!Number.isNaN(at)) lastRequestAtMs = at;
    }
  }
  if (skipped > 0) {
    // warn: a defect because malformed transcript rows were omitted from cold-cache judgment.
    LOGGER.warn(
      { file, skipped_lines: skipped },
      "skipped unparsable transcript lines while reading the cold-gate facts",
    );
  }
  const facts: TranscriptFacts = {
    contextTokens,
    sawUsage,
    lastRequestAtMs,
    cacheTtlMs,
    prompts,
    ...(lastModel === undefined ? {} : { lastModel }),
    ...(lastPermissionMode === undefined ? {} : { lastPermissionMode }),
    ...(opening === undefined ? {} : { opening }),
    ...(clearedAtMs === undefined ? {} : { clearedAtMs }),
  };
  LOGGER.debug({ file, ...facts }, "read the cold-gate facts from the transcript");
  return facts;
}

/** A record's own timestamp in epoch milliseconds, when it states a legible one. */
function recordAtMs(record: TranscriptRecord): number | undefined {
  if (typeof record.timestamp !== "string") return undefined;
  const at = Date.parse(record.timestamp);
  return Number.isNaN(at) ? undefined : at;
}

/** Why a continuation would be cold, or absence when it would be warm. */
export type ColdReason = "lapsed" | "model_switch";

/**
 * THE COLD GATE HAS A FLOOR.
 *
 * Owner ruling, 2026-09-13 ("Owner ruling: the cold gate has a floor" in
 * `docs/REALTEST-JUDGEMENT-CALLS.md`): no cold gate when the context a cold
 * read would re-read is under 70,000 tokens — the session continues
 * automatically. At or above 70,000 the gate asks as before. The ruling covers
 * every lapse (hibernation revival, daemon restart, resume after the TTL) AND
 * a model change, whose cache is per model and so re-reads everything
 * regardless.
 *
 * It is a constant and not a knob because the shim has no configuration
 * surface for thresholds — every environment variable it reads is a refusal
 * rather than a default — and inventing one would put the owner's ruling
 * behind a setting nothing sets.
 */
export const COLD_GATE_FLOOR_TOKENS = 70_000;

/**
 * Whether a cold read this small falls under the floor, recorded once when it
 * does.
 *
 * INFO AND NOT SILENCE: the session continuing without asking is an action the
 * user did not authorize turn by turn, so the log has to be able to answer
 * "why was I not asked" with the size, the lapse and the rule that decided it.
 */
export function underColdGateFloor(
  facts: TranscriptFacts,
  nowMs: number,
  reason: ColdReason,
): boolean {
  if (facts.contextTokens >= COLD_GATE_FLOOR_TOKENS) return false;
  LOGGER.info(
    {
      context_tokens: facts.contextTokens,
      floor_tokens: COLD_GATE_FLOOR_TOKENS,
      lapse_ms: facts.lastRequestAtMs === 0 ? 0 : nowMs - facts.lastRequestAtMs,
      reason,
    },
    "continuing without the cold gate: the cold read is under the cold-gate floor",
  );
  return true;
}

/**
 * Judge the cache.
 *
 * A model switch is cold UNCONDITIONALLY when the models differ, because the
 * prompt cache is per model: continuing under a different model reads nothing
 * back no matter how recent the last request was.
 *
 * The floor is applied AFTER the reason is settled, so a conversation that was
 * warm anyway is never recorded as having been let through by the floor.
 */
export function judgeCold(
  facts: TranscriptFacts,
  nowMs: number,
  requestedModel: string | undefined,
): ColdReason | undefined {
  const reason = coldReason(facts, nowMs, requestedModel);
  if (reason === undefined) return undefined;
  return underColdGateFloor(facts, nowMs, reason) ? undefined : reason;
}

function coldReason(
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
