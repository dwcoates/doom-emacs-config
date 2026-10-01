/**
 * convert/place.ts — WHERE EACH ROW SITS IN ITS CONVERSATION, stated from the
 * vendor record that produced it, by the same rule the file plane states it.
 *
 * TWO PRODUCERS WRITE ONE ROW, and the store keeps the place the first of them
 * stated. The sidecar places a row from its transcript record's bytes
 * (shim-sidecar internal/convert/place.go): `at_ms` is the earliest start
 * instant the entry states for its unit, else the record's own `timestamp`,
 * and `ordinal` is the entry's index among the entries that record produced.
 * This shim used to place every row at the instant it observed it, so the two
 * planes stated two places from two clocks for one row, and which one stood
 * depended on which write landed first: an activity drew ahead of its own
 * prompt when the sidecar won (TestColdGate, 2026-09-30).
 *
 * A row converted from a vendor record that carries a parsable `timestamp`
 * is therefore placed HERE, by the file plane's rule, so both planes state the
 * same instant whichever writes first. A row with no such record — a stream
 * delta, the shim's own prompt row — keeps the writer's observation clock
 * (store/writer.ts PlaceClock), as the file plane has no copy of it to race.
 */
import type { SdkMessage } from "../sdk/types.js";
import type { PersistEntry, RecordPlace } from "../store/persistence.js";

/** The record's own `timestamp` in epoch milliseconds, or undefined. */
export function recordTimestampMs(message: SdkMessage): number | undefined {
  // Observed shapes beat declared types: every real record carries a
  // top-level `timestamp`, but the SDK's own union does not declare it on
  // every arm (see stream-events.ts recordSettledAt, which reads it the same
  // way).
  const raw = (message as unknown as { readonly timestamp?: unknown }).timestamp;
  if (typeof raw !== "string" || raw === "") return undefined;
  const ms = Date.parse(raw);
  return Number.isNaN(ms) || ms <= 0 ? undefined : ms;
}

/**
 * The earliest start instant the value states for its unit, or 0 when it
 * states none. It reads EVERY `conversation.v1.AgentActivityStartedAt` in the
 * value, as the file plane does: a start arm carries its started_at and a
 * settle restates it, and they all name the one call that opened the unit.
 */
export function openingInstantMs(value: unknown): number {
  let earliest = 0;
  const walk = (node: unknown): void => {
    if (node === null || typeof node !== "object") return;
    if (Array.isArray(node)) {
      for (const item of node) walk(item);
      return;
    }
    const message = node as { readonly $typeName?: unknown; readonly atMs?: unknown };
    // A residue arm's verbatim record holds no instant of ours and can be
    // large; it is never walked.
    if (message.$typeName === "google.protobuf.Struct") return;
    if (message.$typeName === "conversation.v1.AgentActivityStartedAt") {
      const at = typeof message.atMs === "bigint" ? Number(message.atMs) : 0;
      if (at > 0 && (earliest === 0 || at < earliest)) earliest = at;
      return;
    }
    for (const child of Object.values(node)) walk(child);
  };
  walk(value);
  return earliest;
}

/**
 * Every entry one record produced, placed by the file plane's rule: the
 * entry's earliest start instant, else the record's timestamp, at its index
 * among the record's entries. A record with no parsable timestamp, or no
 * entries, answers the very array it was given, so a caller can tell nothing
 * was placed; its entries keep the writer's observation clock.
 */
export function placeRecordEntries(message: SdkMessage, entries: readonly PersistEntry[]): readonly PersistEntry[] {
  const recordAtMs = recordTimestampMs(message);
  if (recordAtMs === undefined || entries.length === 0) return entries;
  return entries.map((entry, ordinal) => {
    const opening = entry.item.kind === "shell_run_claim" ? 0 : openingInstantMs(entry.item);
    const place: RecordPlace = { atMs: opening > 0 ? opening : recordAtMs, ordinal };
    return { ...entry, recordPlace: place };
  });
}
