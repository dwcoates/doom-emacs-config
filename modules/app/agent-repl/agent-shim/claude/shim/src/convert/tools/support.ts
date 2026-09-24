/**
 * convert/tools/support.ts — what every per-tool converter needs.
 *
 * # Reading the vendor's typed output
 *
 * A tool result carries TWO things: the string the MODEL was shown, and the
 * tool's own full Output object. Converters read the second — that is what the
 * typed outputs are for, and parsing the first is the mistake this file exists
 * to make unnecessary. The readers here are deliberately narrow: they answer
 * `undefined` for a field that is absent or of the wrong type, so a converter
 * can decide between "the vendor said nothing" (leave the field unset, per
 * PRESENCE NEVER SENTINELS) and "the vendor said something we must carry".
 *
 * # Hunks
 *
 * `FilePatchHunk` is STATED, not computed by a consumer: the producer diffs at
 * the moment of the change, so line numbers describe the file as it actually
 * was. The vendor hands an edit its `structuredPatch` already; a write is handed
 * `originalFile` and `content`, so the shim diffs those two itself — the
 * PRODUCER diffs, and does it once.
 */
import { create, isMessage, toJson, type JsonObject } from "@bufbuild/protobuf";
import { StructSchema } from "@bufbuild/protobuf/wkt";
import { conversationv1 } from "../../proto.js";
import { settledAt, toolFailure } from "../entries.js";
import { rawStruct } from "../residue.js";
import type { PendingCall, ToolOutcome } from "../tool-calls.js";

// ---------------------------------------------------------------------------
// Reading loosely-typed vendor objects
// ---------------------------------------------------------------------------

/** The vendor's Output object as a plain record, or `undefined` when it is not one. */
export function asRecord(value: unknown): Record<string, unknown> | undefined {
  if (typeof value !== "object" || value === null || Array.isArray(value)) return undefined;
  return value as Record<string, unknown>;
}

/** A string field, or `undefined` when the vendor stated none. */
export function str(record: Record<string, unknown> | undefined, key: string): string | undefined {
  const value = record?.[key];
  return typeof value === "string" ? value : undefined;
}

/** A string field, or the empty string — for a NON-OPTIONAL proto field only. */
export function strOr(record: Record<string, unknown> | undefined, key: string, fallback = ""): string {
  return str(record, key) ?? fallback;
}

/** A finite number field, or `undefined`. */
export function num(record: Record<string, unknown> | undefined, key: string): number | undefined {
  const value = record?.[key];
  return typeof value === "number" && Number.isFinite(value) ? value : undefined;
}

/** A non-negative integer field as a uint, or `undefined`. */
export function uint(record: Record<string, unknown> | undefined, key: string): number | undefined {
  const value = num(record, key);
  if (value === undefined) return undefined;
  const rounded = Math.trunc(value);
  return rounded < 0 ? undefined : rounded;
}

/** A non-negative integer field as a uint64, or `undefined`. */
export function big(record: Record<string, unknown> | undefined, key: string): bigint | undefined {
  const value = uint(record, key);
  return value === undefined ? undefined : BigInt(value);
}

/** A boolean field, or `undefined` when the vendor stated none. */
export function bool(record: Record<string, unknown> | undefined, key: string): boolean | undefined {
  const value = record?.[key];
  return typeof value === "boolean" ? value : undefined;
}

/** An array field, or `undefined`. */
export function arr(record: Record<string, unknown> | undefined, key: string): unknown[] | undefined {
  const value = record?.[key];
  return Array.isArray(value) ? value : undefined;
}

/** A nested object field, or `undefined`. */
export function obj(
  record: Record<string, unknown> | undefined,
  key: string,
): Record<string, unknown> | undefined {
  return asRecord(record?.[key]);
}

// ---------------------------------------------------------------------------
// Reading the text a result returned to the model
// ---------------------------------------------------------------------------

/**
 * The text a tool result returned, flattened out of the two shapes it takes.
 *
 * Converters read the TYPED output first and this second: a fact the vendor
 * states in a structured field is never mined out of prose. But some results
 * carry no typed output at all — a nonzero Bash exit arrives as a bare string —
 * and then the returned text is the vendor's ONLY statement of what happened.
 */
export function resultText(content: conversationv1.ToolResultContent | undefined): string {
  if (content === undefined) return "";
  return content.blocks
    .map((block) => (block.block.case === "text" ? block.block.value.text : ""))
    .join("\n");
}

// ---------------------------------------------------------------------------
// Untyped calls: a tool whose schema the producer does not hold
// ---------------------------------------------------------------------------

/**
 * The call's input as the untyped `Struct` an untyped tool's arm carries, or
 * `undefined` when it cannot be represented as one (the caller records that).
 *
 * `rawStruct` is the one place a vendor record is checked for JSON
 * representability, and its answer is normalized to the generated field's JSON
 * form here — a `Struct` and its JSON object are the same value in two
 * spellings, and this accepts whichever the helper hands back.
 */
export function untypedArguments(call: PendingCall): JsonObject | undefined {
  const raw = rawStruct(call.input);
  if (raw === undefined) return undefined;
  return isMessage(raw, StructSchema) ? toJson(StructSchema, raw) : raw;
}

/**
 * What an untyped tool returned, for an arm whose content is NON-OPTIONAL: when
 * the vendor returned nothing at all an EMPTY content is the honest value — the
 * call did settle, and the frame says the tool answered with no blocks rather
 * than pretending it never answered. `returned` says which it was, so the
 * caller can record an empty answer.
 */
export function returnedContent(outcome: ToolOutcome): {
  readonly content: conversationv1.ToolResultContent;
  readonly returned: boolean;
} {
  if (outcome.content !== undefined) return { content: outcome.content, returned: true };
  return { content: create(conversationv1.ToolResultContentSchema, {}), returned: false };
}

// ---------------------------------------------------------------------------
// The shared failure shape
// ---------------------------------------------------------------------------

/**
 * The shared account of a failed call, from the outcome the vendor gave, with
 * the call's own start restated beside the settle instant.
 */
export function failureOf(call: PendingCall, outcome: ToolOutcome): conversationv1.AgentToolFailure {
  return toolFailure(outcome.content, outcome.settledAtMs, call.startedAtMs);
}

/**
 * When the call settled, as every terminal arm carries it, with the call's own
 * start restated so the settled frame alone states its runtime.
 */
export function settle(call: PendingCall, outcome: ToolOutcome): conversationv1.AgentActivitySettledAt {
  return settledAt(outcome.settledAtMs, call.startedAtMs);
}

// ---------------------------------------------------------------------------
// Hunks
// ---------------------------------------------------------------------------

/** One hunk of the vendor's own structured patch. */
function hunkOf(entry: unknown): conversationv1.FilePatchHunk | undefined {
  const record = asRecord(entry);
  if (record === undefined) return undefined;
  const lines = arr(record, "lines");
  const oldStart = uint(record, "oldStart");
  const oldLines = uint(record, "oldLines");
  const newStart = uint(record, "newStart");
  const newLines = uint(record, "newLines");
  if (
    lines === undefined ||
    oldStart === undefined ||
    oldLines === undefined ||
    newStart === undefined ||
    newLines === undefined
  ) {
    // A HUNK MISSING A RANGE IS NOT A HUNK. Producing one with zeroes would
    // draw a diff at the top of the file that never happened.
    return undefined;
  }
  return create(conversationv1.FilePatchHunkSchema, {
    oldRange: create(conversationv1.FilePatchHunkRangeSchema, { start: oldStart, lines: oldLines }),
    newRange: create(conversationv1.FilePatchHunkRangeSchema, { start: newStart, lines: newLines }),
    lines: lines.filter((line): line is string => typeof line === "string"),
  });
}

/** The vendor's whole structured patch. */
export function hunksOf(structuredPatch: unknown): conversationv1.FilePatchHunk[] {
  const entries = Array.isArray(structuredPatch) ? structuredPatch : [];
  const hunks: conversationv1.FilePatchHunk[] = [];
  for (const entry of entries) {
    const hunk = hunkOf(entry);
    if (hunk !== undefined) hunks.push(hunk);
  }
  return hunks;
}

/**
 * The hunks between two whole versions of a file — THE PRODUCER DIFFS.
 *
 * A write hands over the file's whole new contents, so nothing upstream states
 * what CHANGED; a card that showed the whole file instead of the change would be
 * unreadable for a one-line edit. The algorithm is a longest-common-prefix and
 * suffix trim rather than a full diff: it is exact for the shape a write
 * actually takes (a replaced region), it is linear, and it never invents an
 * alignment inside the changed region that a reader would take for real.
 */
/**
 * A file version's lines. An EMPTY version has no lines at all, rather than
 * one empty line a diff would draw as a change.
 *
 * A FILE'S TERMINATING NEWLINE IS NOT A LINE. Text files end with one, so a
 * bare split leaves a final empty element that is the terminator rather than
 * any content — and a creation then drew a one-line file as TWO additions, the
 * second of them blank, and stated "+1,2" for it. `diff` itself counts "one\n"
 * as one line, and so does the card now. A version that genuinely ends in a
 * blank line is "a\n\n", which keeps its blank line here because only ONE
 * trailing empty element is dropped.
 *
 * The transcript plane's own `splitLines` (shim-sidecar/internal/convert/
 * diff.go) reads a version exactly this way, because the two planes must mint
 * the identical patch for one write.
 */
function splitLines(text: string): string[] {
  if (text === "") return [];
  const lines = text.split("\n");
  if (lines[lines.length - 1] === "") lines.pop();
  return lines;
}

export function diffHunks(before: string, after: string): conversationv1.FilePatchHunk[] {
  if (before === after) return [];
  const oldLines = splitLines(before);
  const newLines = splitLines(after);

  let prefix = 0;
  while (prefix < oldLines.length && prefix < newLines.length && oldLines[prefix] === newLines[prefix]) {
    prefix += 1;
  }
  let suffix = 0;
  while (
    suffix < oldLines.length - prefix &&
    suffix < newLines.length - prefix &&
    oldLines[oldLines.length - 1 - suffix] === newLines[newLines.length - 1 - suffix]
  ) {
    suffix += 1;
  }

  const removed = oldLines.slice(prefix, oldLines.length - suffix);
  const added = newLines.slice(prefix, newLines.length - suffix);
  const context = 3;
  const contextBefore = oldLines.slice(Math.max(0, prefix - context), prefix);
  const contextAfter = oldLines.slice(
    oldLines.length - suffix,
    Math.min(oldLines.length, oldLines.length - suffix + context),
  );

  const lines = [
    ...contextBefore.map((line) => ` ${line}`),
    ...removed.map((line) => `-${line}`),
    ...added.map((line) => `+${line}`),
    ...contextAfter.map((line) => ` ${line}`),
  ];
  const oldStart = Math.max(1, prefix - contextBefore.length + 1);
  const newStart = oldStart;
  return [
    create(conversationv1.FilePatchHunkSchema, {
      oldRange: create(conversationv1.FilePatchHunkRangeSchema, {
        start: oldStart,
        lines: contextBefore.length + removed.length + contextAfter.length,
      }),
      newRange: create(conversationv1.FilePatchHunkRangeSchema, {
        start: newStart,
        lines: contextBefore.length + added.length + contextAfter.length,
      }),
      lines,
    }),
  ];
}
