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
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../../proto.js";
import { settledAt, toolFailure } from "../entries.js";
import type { ToolOutcome } from "../tool-calls.js";

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
// The shared failure shape
// ---------------------------------------------------------------------------

/** The shared account of a failed call, from the outcome the vendor gave. */
export function failureOf(outcome: ToolOutcome): conversationv1.AgentToolFailure {
  return toolFailure(outcome.content, outcome.settledAtMs);
}

/** When the call settled, as every terminal arm carries it. */
export function settle(outcome: ToolOutcome): conversationv1.AgentActivitySettledAt {
  return settledAt(outcome.settledAtMs);
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
export function diffHunks(before: string, after: string): conversationv1.FilePatchHunk[] {
  if (before === after) return [];
  const oldLines = before === "" ? [] : before.split("\n");
  const newLines = after === "" ? [] : after.split("\n");

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
