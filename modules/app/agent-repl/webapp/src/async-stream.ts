/**
 * Async-stream parsers — turning a detached source's raw stream into
 * something the feed can render (§2.6 `stream.format`).
 *
 * This is the module that makes the async fold GENERAL. A background
 * agent's output file is its full JSONL transcript — the same shape the
 * daemon's own replay path parses — so it decodes back into
 * ConversationItems and renders through the very `renderItem` the top-level
 * feed uses. A detached agent is not "like" the Agent case; it IS the Agent
 * case, differing only in that its stream arrives as a file rather than as
 * parent_tool_use_id-tagged frames. Before this it reached the browser and
 * was painted as an opaque `<pre>`.
 *
 * One parser per `format`, and nothing is forced into a shape its data does
 * not have: a workflow journal is a record log, so it yields rows, and a
 * shell's spool is bytes, so it has no parser here at all and stays a
 * `<pre>`.
 */
import { ConversationItem, TextItem, ThinkingItem, ToolItem } from "./store.js";

/**
 * Most items rendered from one parsed stream. A settled agent's transcript
 * runs to thousands of entries, and a fold is a glance at what the work is
 * doing, not a second feed — so the TAIL is kept (the newest work is the
 * interesting part) and the drop is reported rather than silent.
 */
export const STREAM_ITEM_CAP = 200;

/** A parsed stream, plus what the cap dropped so the fold can say so. */
export interface ParsedTranscript {
  items: ConversationItem[];
  /** Entries dropped by the cap; 0 when the whole stream fits. */
  dropped: number;
}

/** One entry of a workflow run's journal.jsonl. */
export interface JournalRow {
  label: string;
  detail: string;
  status: "running" | "done" | "error";
}

/**
 * Split JSONL into parsed objects, skipping what will not parse.
 *
 * A tail read starts and ends mid-file, so the first and last lines are
 * routinely truncated — a partial line is the normal case here, not a
 * failure worth surfacing.
 */
function jsonlObjects(text: string): Record<string, unknown>[] {
  const out: Record<string, unknown>[] = [];
  for (const line of text.split("\n")) {
    const trimmed = line.trim();
    if (trimmed === "") continue;
    try {
      const parsed: unknown = JSON.parse(trimmed);
      if (typeof parsed === "object" && parsed !== null && !Array.isArray(parsed)) {
        out.push(parsed as Record<string, unknown>);
      }
    } catch {
      // A truncated head or tail line; the next poll re-reads from the
      // same offset, so nothing is lost by skipping it here.
    }
  }
  return out;
}

function str(o: Record<string, unknown> | undefined, key: string): string {
  const v = o?.[key];
  return typeof v === "string" ? v : "";
}

function obj(o: Record<string, unknown> | undefined, key: string): Record<string, unknown> | undefined {
  const v = o?.[key];
  return typeof v === "object" && v !== null && !Array.isArray(v)
    ? (v as Record<string, unknown>)
    : undefined;
}

function arr(o: Record<string, unknown> | undefined, key: string): Record<string, unknown>[] {
  const v = o?.[key];
  return Array.isArray(v) ? (v as Record<string, unknown>[]) : [];
}

/** The text of a tool_result's `content`, string or block-array. */
function blockText(content: unknown): string {
  if (typeof content === "string") return content;
  if (!Array.isArray(content)) return "";
  return (content as Record<string, unknown>[])
    .filter((b) => b.type === "text")
    .map((b) => str(b, "text"))
    .join("\n");
}

/**
 * Parse a subagent's JSONL transcript into feed items.
 *
 * Deliberately a REDUCED model of the daemon's replay parsing: text,
 * thinking, tool calls, and their results. A transcript entry has no block
 * ids of its own (those are the streaming layer's), so ids are synthesized
 * per entry — they only have to be unique within this fold, since nothing
 * outside it addresses them.
 *
 * Every item comes back `done`: a transcript entry is by definition a
 * finished block, so a cursor or "thinking…" spinner on one would lie.
 */
export function parseTranscript(text: string, cap = STREAM_ITEM_CAP): ParsedTranscript {
  const items: ConversationItem[] = [];
  const tools = new Map<string, ToolItem>();
  let n = 0;

  for (const entry of jsonlObjects(text)) {
    const kind = str(entry, "type");
    const message = obj(entry, "message");
    if (message === undefined) continue;
    const messageId = str(message, "id") || `x${n}`;

    for (const block of arr(message, "content")) {
      const blockType = str(block, "type");
      if (kind === "assistant" && blockType === "text") {
        const t: TextItem = {
          kind: "text",
          blockId: `s${n++}`,
          messageId,
          text: str(block, "text"),
          done: true,
          ts: str(entry, "timestamp"),
        };
        if (t.text !== "") items.push(t);
      } else if (kind === "assistant" && blockType === "thinking") {
        const t: ThinkingItem = {
          kind: "thinking",
          blockId: `s${n++}`,
          messageId,
          text: str(block, "thinking"),
          done: true,
        };
        if (t.text !== "") items.push(t);
      } else if (kind === "assistant" && blockType === "tool_use") {
        const input = obj(block, "input") ?? {};
        const t: ToolItem = {
          kind: "tool",
          toolUseId: str(block, "id"),
          toolName: str(block, "name"),
          messageId,
          input,
          inputJson: JSON.stringify(input, null, 2),
          inputDone: true,
          ts: str(entry, "timestamp"),
        };
        tools.set(t.toolUseId, t);
        items.push(t);
      } else if (kind === "user" && blockType === "tool_result") {
        // Land the result on the call it answers, exactly as the live
        // store does — a result whose call this tail never saw (the read
        // started mid-transcript) is simply skipped.
        const target = tools.get(str(block, "tool_use_id"));
        if (target) {
          target.result = {
            isError: block.is_error === true,
            content: blockText(block.content),
          };
        }
      }
    }
  }

  if (items.length <= cap) return { items, dropped: 0 };
  return { items: items.slice(items.length - cap), dropped: items.length - cap };
}

/** What a transcript's own records say about the run's fate and spend. */
export interface TranscriptStats {
  /** The stream's terminal `result` record has landed: the run is over. */
  finished: boolean;
  /** The terminal record read as a failure rather than a success. */
  error: boolean;
  /**
   * Output tokens spent so far: the terminal record's authoritative total
   * once it lands, else the sum over the assistant messages seen — a LIVE
   * figure that grows with the tail. Undefined when no record carried one.
   */
  outputTokens: number | undefined;
}

function num(o: Record<string, unknown> | undefined, key: string): number | undefined {
  const v = o?.[key];
  return typeof v === "number" && Number.isFinite(v) ? v : undefined;
}

/**
 * Scan a transcript tail for the run's FATE, independent of the bubble
 * parse: the terminal `{"type":"result"}` record is the stream's own
 * completion signal — the one settling source that cannot be lost in
 * transit, because it arrives with the very bytes the fold reads — and
 * the per-message `usage` records are the badge's token figure. A tail
 * cut mid-line simply reports unfinished until the next poll completes
 * the record.
 */
export function transcriptStats(text: string): TranscriptStats {
  let finished = false;
  let error = false;
  let summed: number | undefined;
  let terminal: number | undefined;
  for (const entry of jsonlObjects(text)) {
    if (str(entry, "type") === "result") {
      finished = true;
      const subtype = str(entry, "subtype");
      error = entry.is_error === true || (subtype !== "" && subtype !== "success");
      terminal = num(obj(entry, "usage"), "output_tokens");
    } else {
      const tokens = num(obj(obj(entry, "message"), "usage"), "output_tokens");
      if (tokens !== undefined) summed = (summed ?? 0) + tokens;
    }
  }
  return { finished, error, outputTokens: terminal ?? summed };
}

/**
 * transcriptStats memoized per source on the tail's LENGTH: the poller
 * only ever appends, so an unchanged length is an unchanged scan, and a
 * badge re-rendering every second does not re-walk a megabyte transcript
 * that grew by nothing.
 */
const statsCache = new Map<string, { len: number; stats: TranscriptStats }>();

export function transcriptStatsCached(sourceId: string, text: string): TranscriptStats {
  const hit = statsCache.get(sourceId);
  if (hit && hit.len === text.length) return hit.stats;
  const stats = transcriptStats(text);
  statsCache.set(sourceId, { len: text.length, stats });
  return stats;
}

/**
 * Parse a workflow run's journal.jsonl into rows.
 *
 * A journal is a record log of the run's agent() calls, NOT a conversation,
 * so it renders as rows rather than bubbles. Forcing it into bubbles would
 * mean inventing a speaker for each record.
 */
export function parseJournal(text: string, cap = STREAM_ITEM_CAP): { rows: JournalRow[]; dropped: number } {
  const rows: JournalRow[] = [];
  for (const entry of jsonlObjects(text)) {
    const label = str(entry, "label") || str(entry, "agent") || str(entry, "phase");
    if (label === "") continue;
    const error = str(entry, "error");
    rows.push({
      label,
      detail: error !== "" ? error : str(entry, "result") || str(entry, "prompt"),
      status: error !== "" ? "error" : entry.result !== undefined ? "done" : "running",
    });
  }
  if (rows.length <= cap) return { rows, dropped: 0 };
  return { rows: rows.slice(rows.length - cap), dropped: rows.length - cap };
}
