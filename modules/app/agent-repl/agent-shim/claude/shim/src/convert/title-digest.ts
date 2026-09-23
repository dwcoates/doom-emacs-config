/**
 * convert/title-digest.ts — the material a synthesized workspace title is made
 * of, computed from the transcript's own records.
 *
 * WHAT THE DIGEST IS. The daemon writes its own one-line title when the vendor
 * has stated no `ai-title`, and it summarizes the user's PROMPTS since the last
 * context boundary. The boundary is whichever of these was most recent in the
 * CURRENT transcript:
 *
 *   - a /compact — a `system:compact_boundary` record followed by an
 *     `isCompactSummary` user record. The digest then carries that summary
 *     (the vendor's own compression of the discarded history) plus the prompts
 *     that followed it;
 *   - a /clear — recognized by the `<command-name>/clear</command-name>`
 *     command envelope the vendor writes at the TOP of the NEW transcript a
 *     clear rotates to. Since a clear rotates the file, "the prompts in this
 *     transcript" is already "the prompts since the clear", so the envelope is
 *     reported as the boundary but does not change which prompts are collected;
 *   - nothing — the conversation has never been cut, so every prompt counts.
 *
 * A /compact stays IN the same file (it never rotates the id), so a compaction
 * is always found by scanning the current transcript; a /clear only ever leaves
 * its envelope at the head of the file it rotated to. The most recent boundary
 * in file order wins.
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
export type TitleDigestBoundary = "none" | "clear" | "compact";

/** The material a synthesized title is made of. */
export interface TitleDigest {
  /** Which boundary the prompts are measured from. */
  readonly boundary: TitleDigestBoundary;
  /** The most recent compaction's summary, verbatim. Present iff boundary is "compact". */
  readonly lastCompactSummary?: string;
  /** The user's prompts since the boundary, oldest first. */
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
 * EXPORTED because `engine/cold.ts` decides the same question about the same
 * records while reading a transcript for ReadTranscripts. A second spelling of
 * "what is a boundary" is how two surfaces come to disagree about one
 * conversation, so there is one.
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
 * EXPORTED for the same reason `isCompactBoundary` is: one spelling of the
 * boundary rule, shared by the digest and by ReadTranscripts.
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
  if (trimmed === "") return undefined;
  if (COMMAND_ARTIFACT_PREFIXES.some((prefix) => trimmed.startsWith(prefix))) return undefined;
  // THE SHIM'S OWN KEEP-ALIVE IS NOBODY'S PROMPT. It is never served, so it
  // must not name a conversation, open its listing, or count toward it either.
  // On the FILE plane the marker is the ruled contract (engine/keepalive.ts):
  // the transcript line carries nothing else that says whose send it was.
  if (isKeepalivePrompt(trimmed)) return undefined;
  return trimmed;
}

/** The summary of the compaction whose boundary is at `boundaryIndex`, or undefined. */
function compactSummaryAfter(records: readonly TitleDigestRecord[], boundaryIndex: number): string | undefined {
  for (let i = boundaryIndex + 1; i < records.length; i++) {
    const r = records[i];
    if (r.isCompactSummary === true) {
      const content = r.message?.content;
      if (typeof content === "string") {
        const trimmed = content.trim();
        return trimmed === "" ? undefined : trimmed;
      }
      return undefined;
    }
  }
  return undefined;
}

/**
 * The digest the records state.
 *
 * The boundary is found in ONE forward pass, last-writer-wins, so the most
 * recent cut in file order is the one the prompts are measured from. The
 * prompts are then every prompt strictly after that boundary (or every prompt
 * at all when there was none).
 */
export function computeTitleDigest(records: readonly TitleDigestRecord[]): TitleDigest {
  let boundaryIndex = -1;
  let boundary: TitleDigestBoundary = "none";
  let lastCompactSummary: string | undefined;

  for (let i = 0; i < records.length; i++) {
    const r = records[i];
    if (isCompactBoundary(r)) {
      boundaryIndex = i;
      boundary = "compact";
      lastCompactSummary = compactSummaryAfter(records, i);
    } else if (isClearEnvelope(r)) {
      boundaryIndex = i;
      boundary = "clear";
      lastCompactSummary = undefined;
    }
  }

  const prompts: string[] = [];
  for (let i = boundaryIndex + 1; i < records.length; i++) {
    const text = promptText(records[i]);
    if (text !== undefined) prompts.push(text);
  }

  return boundary === "compact"
    ? { boundary, lastCompactSummary, prompts }
    : { boundary, prompts };
}
