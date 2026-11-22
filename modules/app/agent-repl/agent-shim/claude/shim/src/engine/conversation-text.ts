/**
 * engine/conversation-text.ts — a conversation, read off its transcript and
 * rendered as plain text, for the headless summarizer.
 *
 * WHY TEXT, AND NOT A RESUME. The summarizer used to resume the workspace's own
 * vendor session id, so the vendor recorded its prompt, its answer and its
 * `plan` posture as turns of the user's conversation: a summary cut short left
 * `plan` as the transcript's last mode, and the resumed session came back in
 * it (2026-11-15). The summarizer is now a bare inference run that holds no
 * session of the workspace's at all (sdk/real-query.ts, `realInferenceOptions`),
 * so the conversation reaches it the only way left: rendered here, as the text
 * of its prompt.
 *
 * WHAT IS RENDERED is the live chain {@link readLiveConversation} reads: from
 * the newest compaction's boundary (whose summary is the conversation before
 * it) to the live end, rolled-back turns excluded, exactly what a resume
 * loads. Each record is one piece: a person's prompt, the assistant's prose, a
 * tool call with its input, a tool's result. Thinking is the model's own
 * scratch and is left out, as are the CLI's meta records and bookkeeping.
 *
 * A CONVERSATION LARGER THAN THE MODEL'S WINDOW is read in parts
 * ({@link chunkConversation}), each asked with the summary of the parts before
 * it ({@link summarizerPrompt}), so the summarizer never meets "Prompt is too
 * long" (the deploy mediator's 1.49M-token conversation, 2026-11-20).
 */
import { conversationv1 } from "../proto.js";
import { compactionPrompt } from "./compaction.js";

/** A record's slice that the renderer reads, as the transcript states it. */
interface RenderedRecord {
  readonly type?: unknown;
  readonly isMeta?: unknown;
  readonly isCompactSummary?: unknown;
  readonly message?: { readonly content?: unknown };
}

/** One content block, as the vendor's records hold them. */
interface Block {
  readonly type?: unknown;
  readonly text?: unknown;
  readonly name?: unknown;
  readonly id?: unknown;
  readonly input?: unknown;
  readonly tool_use_id?: unknown;
  readonly content?: unknown;
  readonly is_error?: unknown;
}

/**
 * Every record of CHAIN that says something, rendered as one piece of text
 * each, oldest first. A record that says nothing (a boundary, a meta record,
 * an answer that was only thinking) renders no piece.
 */
export function renderConversation(chain: readonly RenderedRecord[]): string[] {
  const pieces: string[] = [];
  for (const record of chain) {
    const piece = renderRecord(record);
    if (piece !== "") pieces.push(piece);
  }
  return pieces;
}

/** One record as text, or "" when it says nothing a summary needs. */
function renderRecord(record: RenderedRecord): string {
  if (record.isMeta === true) return "";
  const content = record.message?.content;
  if (record.type === "user") {
    if (record.isCompactSummary === true) {
      return `[The conversation before this point, as an earlier compaction summarized it]\n${textOf(content)}`;
    }
    return typeof content === "string" ? labeled("User", content) : renderBlocks("User", content);
  }
  if (record.type === "assistant") return renderBlocks("Assistant", content);
  return "";
}

/** A labeled piece, or "" for one with nothing in it. */
function labeled(label: string, text: string): string {
  const trimmed = text.trim();
  return trimmed === "" ? "" : `${label}:\n${trimmed}`;
}

/** A record's content blocks, each a paragraph of its piece. */
function renderBlocks(speaker: string, content: unknown): string {
  if (!Array.isArray(content)) return "";
  const parts: string[] = [];
  for (const block of content as Block[]) {
    const part = renderBlock(speaker, block);
    if (part !== "") parts.push(part);
  }
  return parts.join("\n\n");
}

/** One content block as text, or "" for one the summary does not read. */
function renderBlock(speaker: string, block: Block): string {
  switch (block.type) {
    case "text":
      return labeled(speaker, typeof block.text === "string" ? block.text : "");
    case "tool_use":
      return `${speaker} called the tool ${String(block.name)} (${String(block.id)}) with:\n${JSON.stringify(block.input ?? {})}`;
    case "tool_result":
      return (
        `Tool result for ${String(block.tool_use_id)}${block.is_error === true ? " (an error)" : ""}:\n` +
        textOf(block.content)
      );
    case "image":
      return `${speaker} attached an image.`;
    case "document":
      return `${speaker} attached a document.`;
    default:
      // Thinking, redacted thinking and every other block: the model's own
      // scratch or the vendor's bookkeeping, never part of what was said.
      return "";
  }
}

/** The text a string or a block list carries; images are named, not drawn. */
function textOf(content: unknown): string {
  if (typeof content === "string") return content;
  if (!Array.isArray(content)) return "";
  return (content as Block[])
    .map((block) =>
      block.type === "text" && typeof block.text === "string" ? block.text : block.type === "image" ? "[an image]" : "",
    )
    .filter((text) => text !== "")
    .join("\n");
}

/**
 * The most characters of conversation one summarizer call reads.
 *
 * SIZED FROM THE SMALLEST WINDOW A SUMMARIZER RUNS ON: 200,000 tokens (Haiku,
 * the cold gate's usual summarizer), less up to 32,000 for the answer and
 * 8,000 for the running summary and the prompt, leaves 160,000 tokens of
 * conversation. A rendered transcript is mostly code, paths and JSON, which
 * tokenize densely, so it is read at 2.25 characters a token: 360,000
 * characters stay inside the window at that density.
 */
export const SUMMARY_PART_CHARS = 360_000;

/**
 * PIECES joined into parts of at most BUDGET characters each, oldest first.
 * Pieces are never reordered and never split unless one alone exceeds the
 * budget, which is then cut at the budget. An empty conversation is no parts.
 */
export function chunkConversation(pieces: readonly string[], budget: number = SUMMARY_PART_CHARS): string[] {
  if (budget <= 0) throw new Error(`conversation-text: a part's budget must be positive, not ${budget}`);
  const separator = "\n\n";
  const parts: string[] = [];
  let current = "";
  const flush = (): void => {
    if (current !== "") parts.push(current);
    current = "";
  };
  for (const piece of pieces) {
    for (let at = 0; at < piece.length; at += budget) {
      const slice = piece.slice(at, at + budget);
      const joined = current === "" ? slice : `${current}${separator}${slice}`;
      if (joined.length > budget) {
        flush();
        current = slice;
      } else {
        current = joined;
      }
    }
  }
  flush();
  return parts;
}

/** Which part of how many a summarizer call reads, and what came before it. */
export interface SummarizerPart {
  /** The part's text, as {@link chunkConversation} cut it. */
  readonly text: string;
  /** Its 1-based place among the parts. */
  readonly index: number;
  /** How many parts the conversation was cut into. */
  readonly count: number;
  /** The summary of every part before this one; absent for the first. */
  readonly summarySoFar?: string;
}

/**
 * The whole prompt one summarizer call is asked: the conversation's PART, and
 * then the compaction's own instruction for SCOPE ({@link compactionPrompt}),
 * unchanged. A conversation in several parts says which part this is and
 * carries the summary of the parts before it, to be folded into one.
 */
export function summarizerPrompt(part: SummarizerPart, scope: conversationv1.SessionCompactScope): string {
  const instruction = compactionPrompt(scope);
  if (part.count === 1) {
    return `The conversation to summarize:\n\n<conversation>\n${part.text}\n</conversation>\n\n${instruction}`;
  }
  const opening =
    `The conversation to summarize is too long to read at once, so it arrives in ${part.count} parts, ` +
    `oldest first. This is part ${part.index}.`;
  const before =
    part.summarySoFar === undefined
      ? ""
      : `\n\nThe summary of parts 1 to ${part.index - 1}:\n\n<summary_so_far>\n${part.summarySoFar}\n</summary_so_far>`;
  const fold =
    part.summarySoFar === undefined
      ? ""
      : "\n\nFold the summary so far and this part into ONE summary of the conversation through this part.";
  return (
    `${opening}${before}\n\nPart ${part.index}:\n\n<conversation>\n${part.text}\n</conversation>\n\n` +
    `${instruction}${fold}`
  );
}

/**
 * The permission mode the conversation's own records last state, in the
 * vendor's spelling: the last `user` record on CHAIN that names one, or
 * absence when none does. Read off the LIVE chain, so a rolled-back turn's
 * mode or a dropped branch's is never the session's.
 */
export function chainPermissionMode(chain: readonly { readonly type?: unknown; readonly permissionMode?: unknown }[]): string | undefined {
  for (let at = chain.length - 1; at >= 0; at--) {
    const record = chain[at];
    if (record?.type === "user" && typeof record.permissionMode === "string") return record.permissionMode;
  }
  return undefined;
}
