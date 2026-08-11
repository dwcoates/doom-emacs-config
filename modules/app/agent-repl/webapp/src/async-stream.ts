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
import { AsyncSource } from "./protocol.js";
import { ConversationItem, TextItem, ThinkingItem, ToolItem } from "./store.js";
import { log } from "./wslog.js";

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
 * failure worth surfacing. A skip anywhere else in the stream is NOT
 * explained by truncation: a mostly-corrupt tail would otherwise render as
 * a short or empty fold with zero signal, so an interior skip logs one
 * dedup'd summary naming CONTEXT (the call site) with the count.
 */
function jsonlObjects(text: string, context: string): Record<string, unknown>[] {
  const out: Record<string, unknown>[] = [];
  const lines = text
    .split("\n")
    .map((line) => line.trim())
    .filter((line) => line !== "");
  let skipped = 0;
  let interiorSkipped = 0;
  lines.forEach((trimmed, i) => {
    try {
      const parsed: unknown = JSON.parse(trimmed);
      if (typeof parsed === "object" && parsed !== null && !Array.isArray(parsed)) {
        out.push(parsed as Record<string, unknown>);
      }
    } catch {
      // A truncated head or tail line is the normal case and stays
      // silent; the next poll re-reads from the same offset, so nothing
      // is lost by skipping it here.
      skipped++;
      if (i !== 0 && i !== lines.length - 1) interiorSkipped++;
    }
  });
  if (interiorSkipped > 0) {
    log("warn", `${context}: ${skipped}/${lines.length} JSONL line(s) failed to parse (${interiorSkipped} interior) — more than a truncated edge explains`, { operation: "async-stream.jsonl-parse", dedupKey: `jsonlObjects:${context}`, context: { parser_context: context, skipped, line_count: lines.length, interior_skipped: interiorSkipped } });
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
 * The harness's open-ended per-tool status vocabulary, as it appears in a
 * tool_result's structured `toolUseResult` sidecar, narrowed onto the
 * closed `AsyncSource["status"]` enum. Anything unrecognized reads as
 * running, because a fold wrongly saying done hides live output while one
 * wrongly saying running only spins a beat too long.
 */
function asyncStatus(raw: string): AsyncSource["status"] {
  switch (raw) {
    case "completed":
    case "done":
    case "success":
      return "done";
    case "failed":
    case "error":
      return "error";
    case "killed":
    case "cancelled":
    case "canceled":
      return "killed";
    default:
      return "running";
  }
}

/**
 * The stream shape a spawn's TOOL implies — the ONE tool-to-(kind, format)
 * table the classifier arms below and subfeed's pre-structured synthesis
 * both read, so the two cannot drift. Transport stays per-site on purpose:
 * a classified live shell is ws-tailed while everything synthesized or
 * polled reads the poll route.
 */
export function asyncShape(toolName: string): {
  kind: AsyncSource["kind"];
  format: "jsonl-transcript" | "jsonl-journal" | "text";
} {
  switch (toolName) {
    case "Task":
    case "Agent":
      return { kind: "agent", format: "jsonl-transcript" };
    case "Workflow":
      return { kind: "workflow", format: "jsonl-journal" };
    default:
      return { kind: "shell", format: "text" };
  }
}

/**
 * Derive an `AsyncSource` from a tool_result's structured `toolUseResult`
 * sidecar, for cards no live async-source frame ever reaches: a
 * transcript-parsed spawn at depth two and deeper, whose sidecar rides the
 * very JSONL this module already parses. Structural evidence rather than
 * prose, which is what retires regex spawn detection everywhere except
 * pre-structured history.
 */
export function classifyAsyncSource(
  toolName: string,
  structured: Record<string, unknown> | undefined,
  isError: boolean,
): AsyncSource | undefined {
  if (isError || structured === undefined) return undefined;
  switch (toolName) {
    case "Task":
    case "Agent": {
      // A synchronous subagent is NOT an async source: its stream already
      // arrived inline. Only a detached one owns a stream that outlives
      // the call.
      const agentId = str(structured, "agentId");
      if (structured.isAsync !== true || agentId === "") return undefined;
      const description = str(structured, "description");
      const { kind, format } = asyncShape(toolName);
      const src: AsyncSource = {
        source_id: agentId,
        kind,
        label: description !== "" ? description : undefined,
        status: asyncStatus(str(structured, "status")),
      };
      if (str(structured, "outputFile") !== "" && structured.canReadOutputFile === true) {
        src.stream = { transport: "poll", format };
      }
      return src;
    }
    case "Bash": {
      const taskId = str(structured, "backgroundTaskId");
      if (taskId === "") return undefined;
      const { kind, format } = asyncShape(toolName);
      return {
        source_id: taskId,
        kind,
        status: asyncStatus(str(structured, "status")),
        stream: { transport: "ws", format },
      };
    }
    case "Workflow": {
      const taskId = str(structured, "taskId");
      if (taskId === "") return undefined;
      const name = str(structured, "workflowName");
      const { kind, format } = asyncShape(toolName);
      const src: AsyncSource = {
        source_id: taskId,
        kind,
        label: name !== "" ? name : undefined,
        status: asyncStatus(str(structured, "status")),
      };
      if (str(structured, "transcriptDir") !== "") {
        src.stream = { transport: "poll", format };
      }
      return src;
    }
    default:
      return undefined;
  }
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

  for (const entry of jsonlObjects(text, "parseTranscript")) {
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
          // The entry's structured toolUseResult sidecar is the only
          // async-source evidence a transcript-parsed result carries:
          // classifying it here gives a depth-two spawn card a structural
          // verdict, retiring prose detection for it.
          const sidecar = obj(entry, "toolUseResult");
          const src = classifyAsyncSource(target.toolName, sidecar, block.is_error === true);
          if (src) target.asyncSource = src;
        }
      }
    }
  }

  if (items.length <= cap) return { items, dropped: 0 };
  return { items: items.slice(items.length - cap), dropped: items.length - cap };
}

/**
 * What a transcript's own records say about the run's FATE.
 *
 * SPEND IS NOT HERE, deliberately. This scan used to also sum the tail's
 * `output_tokens` for the catalog badge, which made a renderer-side summation a
 * second owner of the session's economics — reporting a different measure, from
 * a different source, than the footer reported for the same invocation. Every
 * token figure now comes from the daemon's own attribution
 * (`agentUncachedInput`), so the only thing a transcript is read for is whether
 * its run has ended and how.
 */
export interface TranscriptStats {
  /** The stream's terminal `result` record has landed: the run is over. */
  finished: boolean;
  /** The terminal record read as a failure rather than a success. */
  error: boolean;
}

/**
 * Scan a transcript tail for the run's FATE, independent of the bubble
 * parse: the terminal `{"type":"result"}` record is the stream's own
 * completion signal — the one settling source that cannot be lost in
 * transit, because it arrives with the very bytes the fold reads. A tail
 * cut mid-line simply reports unfinished until the next poll completes
 * the record.
 */
export function transcriptStats(text: string): TranscriptStats {
  let finished = false;
  let error = false;
  for (const entry of jsonlObjects(text, "transcriptStats")) {
    if (str(entry, "type") !== "result") continue;
    finished = true;
    const subtype = str(entry, "subtype");
    error = entry.is_error === true || (subtype !== "" && subtype !== "success");
  }
  return { finished, error };
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
  for (const entry of jsonlObjects(text, "parseJournal")) {
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
