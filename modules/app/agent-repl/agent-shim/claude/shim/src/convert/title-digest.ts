/**
 * convert/title-digest.ts — the material the workspace summary is made of,
 * computed from the transcript's own records.
 *
 * WHAT THE DIGEST IS. The daemon distills the user's PROMPTS since the last
 * /clear into the one-sentence workspace summary the topbar draws. ONLY A
 * /clear CUTS THE DIGEST (owner ruling, 2026-10-14: every prompt of the
 * session, including pre-compaction, but not pre-clear):
 *
 *   - a /clear — recognized by the `<command-name>/clear</command-name>`
 *     command envelope the vendor writes at the TOP of the NEW transcript a
 *     clear rotates to. Since a clear rotates the file, "the prompts in this
 *     transcript" is already "the prompts since the clear", so the envelope is
 *     reported as the boundary but does not change which prompts are collected;
 *   - nothing — the conversation has never been cleared, so every prompt counts.
 *
 * A /compact stays IN the same file and leaves the user's earlier prompt
 * records in it, so a compaction cuts nothing: the prompts before it are read
 * exactly as the prompts after it are, and its summary is not a prompt.
 *
 * WHAT IS NOT A PROMPT. The transcript carries much under `type:"user"` that a
 * person never typed: the SDK writes tool results as user records (array
 * content of `tool_result` blocks), the vendor writes slash-command envelopes
 * (`<command-name>…`) and their caveats (`isMeta`) and stdout, a compaction
 * writes its summary (`isCompactSummary`), and a subagent's prompt rides
 * `isSidechain`. None of those is a prompt, and each is excluded.
 *
 * PURE, SO IT IS TESTABLE. This module does NO I/O: it takes already-parsed
 * records and returns the digest. `engine/title-digest.ts` owns reading and
 * parsing the transcript bytes — a converter reads none.
 */
import { isKeepalivePrompt } from "../engine/keepalive.js";

/** The kind of context boundary a digest is measured from. */
export type TitleDigestBoundary = "none" | "clear";

/** The material the workspace summary is made of. */
export interface TitleDigest {
  /** Which boundary the prompts are measured from. */
  readonly boundary: TitleDigestBoundary;
  /** The user's prompts since the last /clear, across every compaction, oldest first. */
  readonly prompts: string[];
}

/** One content block of a `type:"user"` record's array content. */
interface ContentBlock {
  readonly type?: unknown;
  readonly text?: unknown;
}

/**
 * One transcript record, in the slice this digest reads. The transcript is
 * append-only JSONL and carries far more than this; every field here is
 * optional because a torn or foreign record must be judged, not assumed.
 */
export interface TitleDigestRecord {
  readonly type?: unknown;
  readonly subtype?: unknown;
  readonly isMeta?: unknown;
  readonly isSidechain?: unknown;
  readonly isCompactSummary?: unknown;
  readonly message?: { readonly role?: unknown; readonly content?: unknown };
}

/** The command envelope a /clear writes at the head of the new transcript. */
const CLEAR_ENVELOPE = "<command-name>/clear</command-name>";

/**
 * The prefixes that mark a `type:"user"` string as a command ARTIFACT rather
 * than a prompt: the slash-command envelope, its caveat, and its stdout. A
 * person's prompt never opens with one.
 */
const COMMAND_ARTIFACT_PREFIXES = ["<command-name>", "<local-command-stdout>", "<local-command-caveat>"];

/**
 * Whether a record is a compaction boundary (the marker, not the summary).
 *
 * The digest no longer cuts at a compaction, but `engine/cold.ts` decides this
 * question about the same records while reading a transcript for
 * ReadTranscripts, and the record shapes this module reads are spelled here
 * once, so the predicate stays beside its siblings.
 */
export function isCompactBoundary(r: TitleDigestRecord): boolean {
  return r.type === "system" && r.subtype === "compact_boundary";
}

/** The string content of a `type:"user"` record, or undefined when it is not a bare string. */
function userString(r: TitleDigestRecord): string | undefined {
  if (r.type !== "user") return undefined;
  const content = r.message?.content;
  return typeof content === "string" ? content : undefined;
}

/**
 * Whether a user string is a /clear command envelope.
 *
 * EXPORTED because ReadTranscripts (`engine/cold.ts`) decides the same
 * question: one spelling of the boundary rule, shared by the digest and by it.
 */
export function isClearEnvelope(r: TitleDigestRecord): boolean {
  const s = userString(r);
  return s !== undefined && s.includes(CLEAR_ENVELOPE);
}

/**
 * The prompt text a record states, or undefined when it is not a prompt.
 *
 * A prompt is a `type:"user"` record the PERSON authored: not a tool result
 * (array of `tool_result` blocks), not a command artifact, not a compaction
 * summary, not a subagent's sidechain prompt, not a caveat. Array content is
 * reduced to its text blocks alone, so a prompt that carried an image beside
 * its words still contributes the words.
 */
export function promptText(r: TitleDigestRecord): string | undefined {
  const trimmed = userRecordText(r);
  if (trimmed === undefined) return undefined;
  if (COMMAND_ARTIFACT_PREFIXES.some((prefix) => trimmed.startsWith(prefix))) return undefined;
  // THE SHIM'S OWN KEEP-ALIVE IS NOBODY'S PROMPT. It is never served, so it
  // must not name a conversation, open its listing, or count toward it either.
  // On the FILE plane the marker is the ruled contract (engine/keepalive.ts):
  // the transcript line carries nothing else that says whose send it was.
  if (isKeepalivePrompt(trimmed)) return undefined;
  return trimmed;
}

/**
 * The trimmed text a main-thread user message states, whoever sent it, or
 * undefined when the record is no such message: not `type:"user"`, a harness
 * record (`isMeta`, a sidechain, a compaction summary), not `role:"user"`, or
 * without text. Array content contributes its text blocks alone.
 *
 * EXPORTED because the rollback (engine/rollback.ts) recognizes the shim's own
 * keep-alive on the file plane by this same text, which {@link promptText}
 * refuses to answer for a keep-alive.
 */
export function userRecordText(r: TitleDigestRecord): string | undefined {
  if (r.type !== "user") return undefined;
  if (r.isMeta === true || r.isSidechain === true || r.isCompactSummary === true) return undefined;
  if (r.message?.role !== "user") return undefined;

  const content = r.message?.content;
  let text: string;
  if (typeof content === "string") {
    text = content;
  } else if (Array.isArray(content)) {
    text = content
      .filter((b): b is ContentBlock => typeof b === "object" && b !== null)
      .filter((b) => b.type === "text" && typeof b.text === "string")
      .map((b) => b.text as string)
      .join("");
  } else {
    return undefined;
  }

  const trimmed = text.trim();
  return trimmed === "" ? undefined : trimmed;
}

/**
 * The digest the records state.
 *
 * The last /clear is found in ONE forward pass, last-writer-wins. The prompts
 * are then every prompt strictly after it (or every prompt at all when there
 * was none). A compaction boundary is passed over like any other non-prompt
 * record, so the prompts before it still count.
 */
export function computeTitleDigest(records: readonly TitleDigestRecord[]): TitleDigest {
  let boundaryIndex = -1;
  for (let i = 0; i < records.length; i++) {
    if (isClearEnvelope(records[i])) boundaryIndex = i;
  }

  const prompts: string[] = [];
  for (let i = boundaryIndex + 1; i < records.length; i++) {
    const text = promptText(records[i]);
    if (text !== undefined) prompts.push(text);
  }

  return { boundary: boundaryIndex < 0 ? "none" : "clear", prompts };
}
