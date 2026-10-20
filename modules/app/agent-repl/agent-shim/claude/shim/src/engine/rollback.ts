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
 * `parentUuid`, and every question here is asked of that chain.
 *
 * A COMPACTION IS WHERE THE CHAIN STARTS. Its `compact_boundary` has a null
 * `parentUuid`, and the walk ends there exactly as the vendor's own loader
 * does. It never crosses through the boundary's `logicalParentUuid`: nothing
 * before the newest compaction or clear can be rolled back to (owner ruling,
 * 2026-10-13; the daemon's `rollbackPrompts` offers none), so the history
 * behind a boundary is never a question asked here. Crossing it was also how a
 * real vendor boundary broke resumes: Claude Code 2.1.296 wrote the last
 * PRESERVED record after the summary, under the anchor, and named it as the
 * boundary's logical parent, so the crossing walked back into its own chain
 * and every resume of that transcript answered `unreadable`.
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
import { closeSync, fstatSync, openSync, readSync } from "node:fs";
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
  readonly origin?: { readonly kind?: unknown };
}

/** The transcript as a conversation: its chain, oldest first. */
export interface TranscriptChain {
  /** Every chained record in the file, live or dropped, by uuid. */
  readonly byUuid: ReadonlyMap<string, ChainRecord>;
  /** The live conversation, oldest first: the head and its ancestry. */
  readonly chain: readonly ChainRecord[];
  /**
   * The whole chain the read walked, oldest first. It is `chain` unless
   * {@link readLiveChain} ended the conversation before a rolled-back prompt:
   * then it still runs on to the file's newest record, through the branch an
   * earlier rollback dropped. That is the chain the VENDOR loads, so a
   * truncating resume discards every entry on it past the fork point, and its
   * guard judges them all ({@link planCut}).
   */
  readonly walked: readonly ChainRecord[];
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
 * one: a live process appends to the file, so the last line can be torn. The
 * walk ends at the newest compaction's boundary. A CYCLE, or a named head the file does not hold, is a broken chain and answers
 * `unreadable`: the cut would be planned on a conversation that does not exist.
 */
export function readTranscriptChain(file: string, head?: string): ChainRead {
  return createTranscriptIndex(file, wholeRecord).read(head);
}

/** A parsed record as it stands: what a one-shot read holds only briefly. */
function wholeRecord(record: Partial<ChainRecord>): ChainRecord {
  return record as ChainRecord;
}

/**
 * One transcript, read once and then only as it grows.
 *
 * WHY IT EXISTS. The rollback plans its cut on the transcript's chain, and the
 * transcript of a long conversation is tens of megabytes: reading and parsing
 * it whole put ~170ms between the stop key and the cut (36 MB, measured
 * 2026-10-16). The chain changes only by appending, so the index keeps what it
 * parsed and reads only the bytes written since; a turn's end refreshes it
 * (session.ts), so a stop parses only the turn it stops.
 *
 * THE SAME READING AS A WHOLE READ, line for line: a record parsed earlier is
 * the record a whole read would parse, and a later line with the same uuid
 * replaces it as it would. A file that was replaced (another inode) or that
 * shrank is read again from its start.
 *
 * AN UNTERMINATED LAST LINE IS READ, NEVER KEPT. It may be a line still being
 * written, so it is parsed for this read alone (a whole read would parse it
 * too) and read again, whole, once its newline lands.
 *
 * WHAT IT HOLDS is what `project` keeps of each record: by default the slice
 * the chain and the cut read ({@link chainRecordOf}), since a kept tool result
 * or answer's text would hold the whole conversation in the shim's memory. A
 * one-shot read ({@link readTranscriptChain}) keeps whole records: the mocked
 * vendor reads an answer's tool calls off them.
 *
 * THE MAPS ONLY GROW. A read adds to the map an earlier read answered, and a
 * restart replaces it with a new one, so a chain a caller holds stays the one
 * it was read as.
 */
export interface TranscriptIndex {
  /** The transcript's path. */
  readonly file: string;
  /** The chain walked from `head`, or from the newest main-thread record. */
  read(head?: string): ChainRead;
  /** Take in what the file gained since the last read, without walking a chain. */
  refresh(): void;
}

/** What the index has taken in from its file. */
interface Ingested {
  readonly byUuid: Map<string, ChainRecord>;
  last: string | undefined;
}

/** RECORD's slice that the chain and the cut read, from a parsed line. */
export function chainRecordOf(record: Partial<ChainRecord>): ChainRecord {
  // ASSIGNED, NOT SPREAD: this runs once per line of a transcript tens of
  // megabytes long, and building each slice through a chain of spreads made a
  // whole read seven times slower than the parse itself.
  const slice: { -readonly [K in keyof ChainRecord]?: ChainRecord[K] } = { uuid: record.uuid };
  if (record.parentUuid !== undefined) slice.parentUuid = record.parentUuid;
  if (record.type !== undefined) slice.type = record.type;
  if (record.subtype !== undefined) slice.subtype = record.subtype;
  if (record.isMeta !== undefined) slice.isMeta = record.isMeta;
  if (record.isSidechain !== undefined) slice.isSidechain = record.isSidechain;
  if (record.isCompactSummary !== undefined) slice.isCompactSummary = record.isCompactSummary;
  if (record.origin !== undefined) slice.origin = { kind: (record.origin as { kind?: unknown } | null)?.kind };
  const message = record.message as { role?: unknown; content?: unknown } | null | undefined;
  if (message !== undefined && message !== null) {
    const content = message.content;
    if (typeof content === "string") {
      slice.message = { role: message.role, content };
    } else if (Array.isArray(content)) {
      const blocks: unknown[] = [];
      for (const block of content as unknown[]) {
        if (typeof block !== "object" || block === null) {
          blocks.push(block);
          continue;
        }
        const { type, text } = block as { type?: unknown; text?: unknown };
        blocks.push(type === "text" ? { type, text } : { type });
      }
      slice.message = { role: message.role, content: blocks };
    } else {
      slice.message = { role: message.role };
    }
  }
  return slice as ChainRecord;
}

/** Parse the complete lines of TEXT into INTO, each record as PROJECT keeps it. */
function ingest(text: string, into: Ingested, project: (record: Partial<ChainRecord>) => ChainRecord): void {
  for (const raw of text.split("\n")) {
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
    into.byUuid.set(record.uuid, project(record));
    if (record.isSidechain !== true) into.last = record.uuid;
  }
}

/** The index of the transcript at FILE, read on its first use. */
export function createTranscriptIndex(
  file: string,
  project: (record: Partial<ChainRecord>) => ChainRecord = chainRecordOf,
): TranscriptIndex {
  let identity: { readonly dev: number; readonly ino: number } | undefined;
  let offset = 0;
  let taken: Ingested = { byUuid: new Map(), last: undefined };

  /**
   * Take in what FILE gained, answering its unterminated tail, `absent` when
   * there is no file, or the read's failure.
   */
  const takeIn = (): { kind: "read"; tail: string } | { kind: "absent" } | { kind: "failed"; detail: string } => {
    let fd: number;
    try {
      fd = openSync(file, "r");
    } catch (err) {
      if ((err as NodeJS.ErrnoException).code === "ENOENT") {
        identity = undefined;
        offset = 0;
        taken = { byUuid: new Map(), last: undefined };
        return { kind: "absent" };
      }
      return { kind: "failed", detail: err instanceof Error ? err.message : String(err) };
    }
    try {
      const stat = fstatSync(fd);
      if (identity === undefined || stat.ino !== identity.ino || stat.dev !== identity.dev || stat.size < offset) {
        identity = { dev: stat.dev, ino: stat.ino };
        offset = 0;
        taken = { byUuid: new Map(), last: undefined };
      }
      if (stat.size === offset) return { kind: "read", tail: "" };
      const grown = Buffer.alloc(stat.size - offset);
      const got = readSync(fd, grown, 0, grown.length, offset);
      const bytes = grown.subarray(0, got);
      const newline = bytes.lastIndexOf(0x0a);
      if (newline >= 0) {
        ingest(bytes.toString("utf8", 0, newline), taken, project);
        offset += newline + 1;
      }
      return { kind: "read", tail: bytes.toString("utf8", newline + 1) };
    } catch (err) {
      return { kind: "failed", detail: err instanceof Error ? err.message : String(err) };
    } finally {
      closeSync(fd);
    }
  };

  return {
    file,
    read(head) {
      const result = takeIn();
      if (result.kind === "absent") return { kind: "no_transcript" };
      if (result.kind === "failed") {
        LOGGER.error({ file, cause: result.detail }, "the vendor transcript could not be read for its chain");
        return { kind: "unreadable", detail: result.detail };
      }
      let view: Ingested = taken;
      if (result.tail.trim() !== "") {
        view = { byUuid: new Map(taken.byUuid), last: taken.last };
        ingest(result.tail, view, project);
      }
      return walkChain(file, view.byUuid, head ?? view.last);
    },
    refresh() {
      const result = takeIn();
      if (result.kind === "failed") {
        LOGGER.error({ file, cause: result.detail }, "the vendor transcript could not be read to refresh its index");
      }
    },
  };
}

/** The chain of BYUUID walked from START, oldest first. */
function walkChain(file: string, byUuid: ReadonlyMap<string, ChainRecord>, start: string | undefined): ChainRead {
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
  const opening = chain[0];
  if (opening?.type === "system" && opening.subtype === "compact_boundary") {
    LOGGER.debug(
      { file, head: start ?? "", boundary: opening.uuid, length: chain.length },
      "the conversation starts at its newest compaction",
    );
  }
  return { kind: "ok", transcript: { byUuid, chain, walked: chain } };
}

/**
 * A record's predecessor in the conversation. A compaction's boundary has
 * none: the chain starts there (see the module comment).
 */
function parentOf(record: ChainRecord): string | undefined {
  if (typeof record.parentUuid === "string" && record.parentUuid !== "") return record.parentUuid;
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
   * it (a keep-alive; a dropped turn's task notification; a prompt an earlier
   * rollback dropped, still on the chain the vendor loads): arming it would
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
 * point the guard would all let go. Those are the entries of the chain the
 * vendor loads (`TranscriptChain.walked`), not only the live ones: right after
 * an earlier rollback the file's newest record is still on the branch it
 * dropped, so a resume at the fork point discards that branch too, and the
 * guard refuses its prompt as another turn's.
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
  const discarded = transcript.walked.slice(transcript.walked.indexOf(record) + 1);
  return { kind: "cut", promptUuid, forkPoint, guard: guardArming(discarded, promptUuid, droppedPromptUuids.length) };
}

/** Whether to arm the guard for a cut whose resume discards `discarded`. */
function guardArming(discarded: readonly ChainRecord[], promptUuid: string, droppedTurns: number): GuardArming {
  if (droppedTurns !== 1) return { kind: "severalTurns" };
  const own = new Set([promptUuid]);
  const foreign = discarded.find((entry) => !guardAttributes(entry, own));
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
export function readLiveChain(index: TranscriptIndex, rolledBackPromptUuids: ReadonlySet<string>): LiveChainRead {
  const file = index.file;
  const read = index.read();
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
      return { kind: "ok", transcript: { ...read.transcript, chain: chain.slice(0, at + 1) }, end };
    }
  }
}
