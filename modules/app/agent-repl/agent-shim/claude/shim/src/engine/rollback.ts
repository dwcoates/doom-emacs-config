/**
 * engine/rollback.ts — the FILE-PLANE half of `RollBackSession`
 * (endpoint_roll_back_session.proto): reading the vendor transcript's chain,
 * planning the cut on it, and recognizing the vendor's refusal of a cut.
 *
 * WHY THE TRANSCRIPT AND NOT MEMORY. A prompt is sent under a vendor uuid
 * DERIVED from its turn id (`convert/ids.ts`, `promptVendorUuid`), so a shim
 * that restarted since the prompt was sent finds the prompt's record by
 * deriving the same uuid. Nothing about the cut is stored.
 *
 * THE CHAIN IS THE CONVERSATION, NOT THE FILE ORDER. The vendor never rewrites
 * its transcript: a resume at a fork point appends the next record as the fork
 * point's child, so a file that was rolled back before still holds the dropped
 * branch. The conversation is the chain walked from its head back through
 * `parentUuid` (and across a compaction through `logicalParentUuid`), and every
 * question here is asked of that chain.
 *
 * THE HEAD. The vendor's head is the last chained record in the file. Right
 * after a rollback nothing new has been appended yet, so the file's last record
 * is still on the dropped branch; the session engine names the fork point as
 * the head until its next send appends past it.
 *
 * Pure apart from the one read in {@link readTranscriptChain}. The mocked vendor
 * (`src/fake/`) reads its own files through the same reader, so both sides of
 * an offline test agree on what the chain is.
 */
import { readFileSync } from "node:fs";
import { bindLog } from "../log.js";
import { promptText, type TitleDigestRecord } from "../convert/title-digest.js";

const LOGGER = bindLog({ component: "shim-engine-rollback", operation: "shim.engine.rollback" });

/**
 * The vendor's words when `resumeDropsTurn`'s guard refuses a truncating resume
 * (sdk.d.ts, `Options.resumeDropsTurn`; the CLI's constant is this exact
 * string). Its own boot path writes it as an `error_during_execution` result
 * and on stderr, then exits 1.
 */
export const RESUME_DROPS_TURN_REFUSAL_PREFIX = "Resume rejected by --resume-drops-turn:";

/**
 * What the vendor says when it will not resume at the uuid it was handed.
 *
 * Grounded on the owner's two dead sessions of 2026-09-14: the child exits 1
 * having printed `No message found with message.uuid of: <uuid>`.
 */
export const REWIND_REFUSAL_PHRASE = "No message found with message.uuid";

/** Every sentence the vendor refuses a truncating resume with. */
const CUT_REFUSAL_PHRASES = [REWIND_REFUSAL_PHRASE, RESUME_DROPS_TURN_REFUSAL_PREFIX];

/**
 * THE ONE READING of "the vendor refused to resume at this cut".
 *
 * Answers the vendor's refusing line, verbatim, or absence when the evidence
 * holds none. Either a known refusal sentence appears, or — for the keep-alive
 * rewind, whose refusal can name only the anchor — the anchor uuid itself.
 * Both are looked for because the sentence reaches the shim on stderr and the
 * uuid in an error result's `errors`.
 */
export function vendorCutRefusal(evidence: string, anchorUuid?: string): string | undefined {
  const lines = evidence.split("\n").map((line) => line.trim());
  for (const phrase of CUT_REFUSAL_PHRASES) {
    const line = lines.find((candidate) => candidate.includes(phrase));
    if (line !== undefined) return line.slice(line.indexOf(phrase));
  }
  if (anchorUuid === undefined || anchorUuid === "") return undefined;
  return lines.find((candidate) => candidate.includes(anchorUuid));
}

/** One transcript record, in the slice the chain and the cut read. */
export interface ChainRecord extends TitleDigestRecord {
  readonly uuid: string;
  readonly parentUuid?: unknown;
  readonly logicalParentUuid?: unknown;
  readonly origin?: { readonly kind?: unknown };
}

/** The transcript as a conversation: its chain, oldest first. */
export interface TranscriptChain {
  /** Every chained record in the file, live or dropped, by uuid. */
  readonly byUuid: ReadonlyMap<string, ChainRecord>;
  /** The live conversation, oldest first: the head and its ancestry. */
  readonly chain: readonly ChainRecord[];
}

/** The outcome of reading a transcript's chain. THE ARM IS WHY. */
export type ChainRead =
  | { readonly kind: "ok"; readonly transcript: TranscriptChain }
  /** No transcript exists: the vendor has recorded nothing. */
  | { readonly kind: "no_transcript" }
  /** The transcript exists and could not be read, or its chain is broken. */
  | { readonly kind: "unreadable"; readonly detail: string };

/**
 * The live chain of the transcript at `file`, walked from `head` — or, when no
 * head is named, from the last chained main-thread record, which is the head
 * the vendor itself resumes from.
 *
 * A LINE THAT DOES NOT PARSE IS SKIPPED, as every transcript reader here skips
 * one: a live process appends to the file, so the last line can be torn. A
 * CYCLE, or a named head the file does not hold, is a broken chain and answers
 * `unreadable`: the cut would be planned on a conversation that does not exist.
 */
export function readTranscriptChain(file: string, head?: string): ChainRead {
  let contents: string;
  try {
    contents = readFileSync(file, "utf8");
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === "ENOENT") return { kind: "no_transcript" };
    const detail = err instanceof Error ? err.message : String(err);
    LOGGER.error({ file, cause: detail }, "the vendor transcript could not be read for its chain");
    return { kind: "unreadable", detail };
  }
  const byUuid = new Map<string, ChainRecord>();
  let last: string | undefined;
  for (const raw of contents.split("\n")) {
    const line = raw.trim();
    if (line === "") continue;
    let parsed: unknown;
    try {
      parsed = JSON.parse(line);
    } catch {
      continue;
    }
    if (typeof parsed !== "object" || parsed === null) continue;
    const record = parsed as Partial<ChainRecord>;
    if (typeof record.uuid !== "string" || record.uuid === "") continue;
    byUuid.set(record.uuid, record as ChainRecord);
    if (record.isSidechain !== true) last = record.uuid;
  }
  const start = head ?? last;
  const chain: ChainRecord[] = [];
  const seen = new Set<string>();
  let at = start;
  while (at !== undefined) {
    const record = byUuid.get(at);
    if (record === undefined) {
      const detail = `the chain names ${at}, which the transcript does not hold`;
      LOGGER.error({ file, head: start ?? "", missing: at, detail }, "the vendor transcript's chain is broken");
      return { kind: "unreadable", detail };
    }
    if (seen.has(at)) {
      const detail = `the chain loops back to ${at}`;
      LOGGER.error({ file, head: start ?? "", detail }, "the vendor transcript's chain is broken");
      return { kind: "unreadable", detail };
    }
    seen.add(at);
    chain.push(record);
    at = parentOf(record);
  }
  chain.reverse();
  return { kind: "ok", transcript: { byUuid, chain } };
}

/** A record's predecessor in the conversation, across a compaction too. */
function parentOf(record: ChainRecord): string | undefined {
  if (typeof record.parentUuid === "string" && record.parentUuid !== "") return record.parentUuid;
  if (typeof record.logicalParentUuid === "string" && record.logicalParentUuid !== "") {
    return record.logicalParentUuid;
  }
  return undefined;
}

/** The prompt-shaped records the vendor writes that no person typed. */
const INTERRUPT_MARKER_PREFIX = "[Request interrupted by user";
const TASK_NOTIFICATION_OPEN = "<task-notification>";
const LOCAL_COMMAND_STDERR_OPEN = "<local-command-stderr>";

/**
 * Whether user text is the vendor's interrupt marker
 * (`[Request interrupted by user…]`), the record a stop leaves in the
 * conversation: one bracketed line, never something a person typed.
 */
export function isInterruptMarker(text: string): boolean {
  const trimmed = text.trim();
  return trimmed.startsWith(INTERRUPT_MARKER_PREFIX) && trimmed.endsWith("]") && !trimmed.includes("\n");
}

/**
 * WHETHER A RECORD IS A PROMPT THAT OPENED OR JOINED A TURN: a `type:"user"`
 * record a sender authored. Not a tool-result carrier, not a harness record
 * (`isMeta`, a compaction summary, a sidechain), not a command artifact or the
 * shim's own keep-alive ({@link promptText} already refuses those), and not the
 * vendor's own bookkeeping that wears a prompt's shape: the interrupt marker, a
 * background task's notification, a local command's stderr, or anything whose
 * `origin` names a source other than a person. The sidecar draws the same line
 * (`shim-sidecar/internal/convert/bookkeeping.go`, `notTypedByPerson`).
 */
export function isPromptRecord(record: ChainRecord): boolean {
  const text = promptText(record);
  if (text === undefined) return false;
  const origin = record.origin?.kind;
  if (origin !== undefined && origin !== "human") return false;
  if (isInterruptMarker(text)) return false;
  if (text.startsWith(TASK_NOTIFICATION_OPEN)) return false;
  if (text.startsWith(LOCAL_COMMAND_STDERR_OPEN)) return false;
  return true;
}

/** Where the conversation is cut, or why it cannot be. THE ARM IS THE OUTCOME. */
export type CutPlan =
  | { readonly kind: "cut"; readonly promptUuid: string; readonly forkPoint: string }
  | { readonly kind: "promptNotRecorded"; readonly detail: string }
  | { readonly kind: "firstPrompt" }
  | { readonly kind: "unseenPrompt"; readonly vendorPromptUuid: string };

/**
 * Plan the cut before `promptUuid` on `transcript`, for a caller that named
 * the prompts of `droppedPromptUuids` (the derived uuids of `dropped_turns`).
 *
 *   - the prompt must be a `type:"user"` record ON THE LIVE CHAIN: one on a
 *     branch an earlier rollback dropped is no longer in the conversation;
 *   - with no predecessor it is the conversation's first, and nothing precedes
 *     it to resume at;
 *   - every later prompt on the chain must be one the caller named.
 */
export function planCut(
  transcript: TranscriptChain,
  promptUuid: string,
  droppedPromptUuids: readonly string[],
): CutPlan {
  const record = transcript.byUuid.get(promptUuid);
  if (record === undefined || record.type !== "user") {
    return { kind: "promptNotRecorded", detail: `the vendor transcript holds no user record ${promptUuid}` };
  }
  const at = transcript.chain.indexOf(record);
  if (at < 0) {
    return {
      kind: "promptNotRecorded",
      detail: `user record ${promptUuid} is on a branch the conversation no longer holds`,
    };
  }
  const forkPoint = parentOf(record);
  if (forkPoint === undefined) return { kind: "firstPrompt" };
  const named = new Set(droppedPromptUuids);
  const unseen = transcript.chain.slice(at + 1).find((later) => isPromptRecord(later) && !named.has(later.uuid));
  if (unseen !== undefined) return { kind: "unseenPrompt", vendorPromptUuid: unseen.uuid };
  return { kind: "cut", promptUuid, forkPoint };
}
