/**
 * convert/transcript-prompt.ts — which of a transcript's records are the
 * person's own prompts, and which mark a context boundary.
 *
 * WHAT IS NOT A PROMPT. The transcript carries much under `type:"user"` that a
 * person never typed: the SDK writes tool results as user records (array
 * content of `tool_result` blocks), the vendor writes slash-command envelopes
 * (`<command-name>…`) and their caveats (`isMeta`) and stdout, a compaction
 * writes its summary (`isCompactSummary`), and a subagent's prompt rides
 * `isSidechain`. None of those is a prompt, and each is excluded.
 *
 * ONE SPELLING OF THE RECORD SHAPES. ReadTranscripts (`engine/cold.ts`) and
 * the rollback (`engine/rollback.ts`) both judge the same records, so the
 * predicates are spelled here once.
 *
 * PURE, SO IT IS TESTABLE. This module does NO I/O: it judges already-parsed
 * records, and its callers own reading the transcript bytes.
 */
import { isKeepalivePrompt } from "../engine/keepalive.js";

/** One content block of a `type:"user"` record's array content. */
interface ContentBlock {
  readonly type?: unknown;
  readonly text?: unknown;
}

/**
 * One transcript record, in the slice these predicates read. The transcript is
 * append-only JSONL and carries far more than this; every field here is
 * optional because a torn or foreign record must be judged, not assumed.
 */
export interface PromptRecord {
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

/** Whether a record is a compaction boundary (the marker, not the summary). */
export function isCompactBoundary(r: PromptRecord): boolean {
  return r.type === "system" && r.subtype === "compact_boundary";
}

/** The string content of a `type:"user"` record, or undefined when it is not a bare string. */
function userString(r: PromptRecord): string | undefined {
  if (r.type !== "user") return undefined;
  const content = r.message?.content;
  return typeof content === "string" ? content : undefined;
}

/**
 * Whether a user string is a /clear command envelope.
 *
 * A /clear rotates to a new transcript and writes this envelope at its head.
 */
export function isClearEnvelope(r: PromptRecord): boolean {
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
export function promptText(r: PromptRecord): string | undefined {
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
export function userRecordText(r: PromptRecord): string | undefined {
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
