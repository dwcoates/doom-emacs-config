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
 * THE HEAD IS ONE RULE ({@link readLiveChain}). The vendor's head is the last
 * chained record in the file. Right after a rollback nothing new has been
 * appended yet, so the file's last record is still on the dropped branch; so
 * whenever that record's chain holds a rolled-back turn's prompt, the
 * conversation ends just before the earliest such prompt. The same rule picks
 * where a resume starts (`StartSessionResume.rolled_back_turns`) and where the
 * next rollback is planned, so the two can never disagree.
 *
 * Pure apart from the one read in {@link readTranscriptChain}. The mocked vendor
 * (`src/fake/`) reads its own files through the same reader, so both sides of
 * an offline test agree on what the chain is.
 */
import { readFileSync } from "node:fs";
import { bindLog } from "../log.js";
import { promptText, userRecordText, type TitleDigestRecord } from "../convert/title-digest.js";
import { isKeepalivePrompt } from "./keepalive.js";

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

/**
 * Whether a record is the shim's own keep-alive prompt: the keep-alive marker
 * on the file plane (engine/keepalive.ts). The shim knows every keep-alive it
 * sent, so one past a fork point is never an unseen prompt; the vendor's guard
 * cannot know it, and refuses it as unattributable.
 */
export function isKeepaliveRecord(record: ChainRecord): boolean {
  const text = userRecordText(record);
  return text !== undefined && isKeepalivePrompt(text);
}

/**
 * THE SHIM'S READING OF `resumeDropsTurn`'S GUARD (sdk.d.ts,
 * `Options.resumeDropsTurn`): whether the guard would let a discarded entry go
 * as the dropped turns' own. The turns' prompts are, and so is anything that
 * is not a user message of its own: an answer, a tool-result carrier, a
 * harness meta record, a compaction summary, the interrupt marker. Any OTHER
 * user message (an unnamed prompt, a task notification, a keep-alive) is not.
 *
 * ONE READING for both sides: the shim plans with it, and the mocked vendor
 * (`src/fake/rollback.ts`) judges its truncating resume with it.
 */
export function guardAttributes(record: ChainRecord, droppedPromptUuids: ReadonlySet<string>): boolean {
  if (droppedPromptUuids.has(record.uuid)) return true;
  if (record.type !== "user" || record.isMeta === true || record.isCompactSummary === true) return true;
  const content = record.message?.content;
  if (Array.isArray(content) && content.some((block) => (block as { type?: unknown }).type === "tool_result")) {
    return true;
  }
  return typeof content === "string" && isInterruptMarker(content);
}

/** Whether the vendor's guard is armed for a cut, and why not when it is not. */
export type GuardArming =
  /** One dropped turn, and every entry past the fork point is its own. */
  | { readonly kind: "armed"; readonly resumeDropsTurn: string }
  /** The guard validates one dropped turn; the plan's own check stands in. */
  | { readonly kind: "severalTurns" }
  /**
   * An entry past the fork point the guard would refuse though the shim knows
   * it (a keep-alive; a dropped turn's task notification): arming it would
   * only have the vendor refuse a cut the shim's own check allowed.
   */
  | { readonly kind: "unattributable"; readonly recordUuid: string };

/** Why the plan refuses `unseen_prompt`. */
export type UnseenWhy =
  /** A prompt no dropped turn names sits after the cut. */
  | "unnamedPrompt"
  /**
   * Restoring files: a user message the guard would refuse sits after the
   * cut, so the vendor could refuse the cut after the files were restored.
   */
  | "guardWouldRefuse";

/** Where the conversation is cut, or why it cannot be. THE ARM IS THE OUTCOME. */
export type CutPlan =
  | { readonly kind: "cut"; readonly promptUuid: string; readonly forkPoint: string; readonly guard: GuardArming }
  | { readonly kind: "promptNotRecorded"; readonly detail: string }
  | { readonly kind: "firstPrompt" }
  | { readonly kind: "unseenPrompt"; readonly vendorPromptUuid: string; readonly why: UnseenWhy };

/** Whether a rollback keeps the workspace's files or restores them. */
export type RollbackFiles = "keep" | "restore";

/**
 * Plan the cut before `promptUuid` on `transcript`, for a caller that named
 * the prompts of `droppedPromptUuids` (the derived uuids of `dropped_turns`).
 *
 *   - the prompt must be a `type:"user"` record ON THE LIVE CHAIN: one on a
 *     branch an earlier rollback dropped is no longer in the conversation;
 *   - with no predecessor it is the conversation's first, and nothing precedes
 *     it to resume at;
 *   - every later prompt on the chain must be one the caller named (the
 *     shim's own keep-alives are known, never unseen);
 *   - restoring files, every later user message must be one the guard lets go
 *     (keep-alives aside, which unarm it): the files are restored BEFORE the
 *     vendor judges the cut, so a refusal must be found here, up front.
 *
 * The guard is armed only for one dropped turn whose entries past the fork
 * point the guard would all let go.
 */
export function planCut(
  transcript: TranscriptChain,
  promptUuid: string,
  droppedPromptUuids: readonly string[],
  files: RollbackFiles,
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
  const later = transcript.chain.slice(at + 1);
  const unseen = later.find((entry) => isPromptRecord(entry) && !named.has(entry.uuid));
  if (unseen !== undefined) return { kind: "unseenPrompt", vendorPromptUuid: unseen.uuid, why: "unnamedPrompt" };
  if (files === "restore") {
    const refused = later.find((entry) => !guardAttributes(entry, named) && !isKeepaliveRecord(entry));
    if (refused !== undefined) return { kind: "unseenPrompt", vendorPromptUuid: refused.uuid, why: "guardWouldRefuse" };
  }
  return { kind: "cut", promptUuid, forkPoint, guard: guardArming(later, promptUuid, droppedPromptUuids.length) };
}

/** Whether to arm the guard for a cut whose later entries are `later`. */
function guardArming(later: readonly ChainRecord[], promptUuid: string, droppedTurns: number): GuardArming {
  if (droppedTurns !== 1) return { kind: "severalTurns" };
  const own = new Set([promptUuid]);
  const foreign = later.find((entry) => !guardAttributes(entry, own));
  if (foreign !== undefined) return { kind: "unattributable", recordUuid: foreign.uuid };
  return { kind: "armed", resumeDropsTurn: promptUuid };
}

/** Where the live conversation ends after its rollbacks. */
export type LiveEnd =
  /** No rolled-back prompt is on the newest record's chain: it ends there. */
  | { readonly kind: "whole" }
  /** It ends at `forkPoint`, just before the rolled-back prompt `promptUuid`. */
  | { readonly kind: "cut"; readonly promptUuid: string; readonly forkPoint: string }
  /** A rolled-back prompt opens the chain, so nothing precedes it to end at. */
  | { readonly kind: "firstPrompt"; readonly promptUuid: string };

/**
 * THE ONE RULE for where the conversation ends after its rollbacks
 * (`StartSessionResume.rolled_back_turns`): the newest record's chain, unless
 * it holds the prompt of a rolled-back turn; then the entry just before the
 * EARLIEST such prompt on it. A branch the next prompt started holds none of
 * them, so the rule never needs clearing.
 */
export function liveEnd(transcript: TranscriptChain, rolledBackPromptUuids: ReadonlySet<string>): LiveEnd {
  const earliest = transcript.chain.find((entry) => entry.type === "user" && rolledBackPromptUuids.has(entry.uuid));
  if (earliest === undefined) return { kind: "whole" };
  const forkPoint = parentOf(earliest);
  if (forkPoint === undefined) return { kind: "firstPrompt", promptUuid: earliest.uuid };
  return { kind: "cut", promptUuid: earliest.uuid, forkPoint };
}

/** The live conversation: the transcript's chain ended by {@link liveEnd}. */
export type LiveChainRead =
  | { readonly kind: "ok"; readonly transcript: TranscriptChain; readonly end: LiveEnd }
  | { readonly kind: "no_transcript" }
  | { readonly kind: "unreadable"; readonly detail: string };

/**
 * The live conversation at `file`, after the rollbacks whose prompts are
 * `rolledBackPromptUuids`: the chain from the newest record, ended by
 * {@link liveEnd}. A rolled-back prompt that opens the chain is a broken
 * invariant (a rollback never cuts before the first prompt) and answers
 * `unreadable`.
 */
export function readLiveChain(file: string, rolledBackPromptUuids: ReadonlySet<string>): LiveChainRead {
  const read = readTranscriptChain(file);
  if (read.kind !== "ok") return read;
  const end = liveEnd(read.transcript, rolledBackPromptUuids);
  switch (end.kind) {
    case "whole":
      return { kind: "ok", transcript: read.transcript, end };
    case "firstPrompt": {
      const detail = `rolled-back prompt ${end.promptUuid} opens the conversation; nothing precedes it to resume at`;
      LOGGER.error({ file, prompt_uuid: end.promptUuid, detail }, "the vendor transcript's live end cannot be found");
      return { kind: "unreadable", detail };
    }
    case "cut": {
      const chain = read.transcript.chain;
      const at = chain.findIndex((entry) => entry.uuid === end.forkPoint);
      LOGGER.debug(
        { file, prompt_uuid: end.promptUuid, fork_point: end.forkPoint },
        "the conversation ends before a rolled-back prompt still at the transcript's tail",
      );
      return { kind: "ok", transcript: { byUuid: read.transcript.byUuid, chain: chain.slice(0, at + 1) }, end };
    }
  }
}
