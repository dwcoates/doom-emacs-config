/**
 * proto-scalars — the loud protojson primitive readers every hand-typed
 * decoder in this webapp shares.
 *
 * These lived inside `frontend-proto.ts` while it was the only hand-typed
 * decoder. It is not any more: `async-bubble.ts` decodes the detached-work
 * surface, and a second private copy of "what a protojson string is" would be
 * two contracts that can drift — one of them silently accepting a shape the
 * other rejects. They live here so there is exactly ONE answer.
 *
 * Every reader here FAILS rather than degrades: a wrong JSON type, an
 * unrecognized field, an unparseable numeric string all throw. Nothing in this
 * module has a fallback value, because a decoder that guesses is how a frontend
 * starts rendering a shape no producer promised.
 */

/** A decoded JSON object, before any field has been claimed. */
export type Obj = Record<string, unknown>;

/** The allowed-key set for a message with no fields at all. */
export const EMPTY_KEY_SET: ReadonlySet<string> = new Set<string>();

/**
 * Exact generated field lists: omission and invention are both type errors.
 *
 * THE ANCHOR (invariant I5). A decode table built with this cannot name a field
 * the generated stub does not have, and cannot omit one it does — a proto that
 * adds, renames or drops a field breaks THIS build rather than producing a
 * decoder that silently stops recognizing part of the wire.
 */
export function generatedFieldSet<Fields extends string>() {
  return <const Keys extends readonly Fields[]>(
    ...keys: Keys & (Exclude<Fields, Keys[number]> extends never ? unknown : ["missing generated field", Exclude<Fields, Keys[number]>])
  ): ReadonlySet<string> => new Set<string>(keys);
}

export function ensureObject(v: unknown, ctx: string): Obj {
  if (typeof v !== "object" || v === null || Array.isArray(v)) {
    throw new Error(`frontend-proto: ${ctx} must be a JSON object`);
  }
  return v as Obj;
}

export function ensureArray(v: unknown, ctx: string): unknown[] {
  if (!Array.isArray(v)) {
    throw new Error(`frontend-proto: ${ctx} must be a JSON array`);
  }
  return v;
}

export function rejectUnknown(o: Obj, allowed: ReadonlySet<string>, ctx: string): void {
  const bad = Object.keys(o).filter((k) => !allowed.has(k));
  if (bad.length > 0) {
    throw new Error(`frontend-proto: ${ctx} has unrecognized field(s): ${bad.join(", ")}`);
  }
}

export function str(o: Obj, key: string, ctx: string): string {
  const v = o[key];
  if (v === undefined || v === null) return "";
  if (typeof v !== "string") {
    throw new Error(`frontend-proto: ${ctx}.${key} must be a string (got ${typeof v})`);
  }
  return v;
}

/** A proto3 numeric scalar: a JSON number, or (int64/uint64) a numeric string. */
export function num(o: Obj, key: string, ctx: string): number {
  const v = o[key];
  if (v === undefined || v === null) return 0;
  if (typeof v === "number") return v;
  if (typeof v === "string") {
    const n = Number(v);
    if (!Number.isFinite(n)) {
      throw new Error(`frontend-proto: ${ctx}.${key} is not a numeric string ('${v}')`);
    }
    return n;
  }
  throw new Error(`frontend-proto: ${ctx}.${key} must be a number or numeric string (got ${typeof v})`);
}

export function bool(o: Obj, key: string, ctx: string): boolean {
  const v = o[key];
  if (v === undefined || v === null) return false;
  if (typeof v !== "boolean") {
    throw new Error(`frontend-proto: ${ctx}.${key} must be a boolean (got ${typeof v})`);
  }
  return v;
}

export function int64(o: Obj, key: string, where: string): number {
  const value = o[key];
  if (typeof value !== "string" && typeof value !== "number") throw new Error(`frontend-proto: ${where}.${key} must be an int64 string`);
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed)) throw new Error(`frontend-proto: ${where}.${key} must be a safe integer`);
  return parsed;
}

/**
 * An int64 that may be ABSENT because canonical protojson omits proto3 scalars
 * at their default value.
 *
 * Distinct from {@link int64}, which demands presence: for a REQUIRED stamp an
 * absent field is a malformed frame, while for an ordinary proto3 int64 an
 * absent field means zero and nothing else. The range check is the same in both
 * — a value JavaScript cannot hold exactly is refused rather than rounded.
 */
export function int64OrZero(o: Obj, key: string, where: string): number {
  const value = o[key];
  if (value === undefined || value === null) return 0;
  return int64(o, key, where);
}

export function oneof(o: Obj, keys: readonly string[], where: string): string {
  const found = keys.filter((key) => o[key] !== undefined);
  if (found.length !== 1) throw new Error(`frontend-proto: ${where} requires exactly one of ${keys.join(", ")}`);
  return found[0];
}

/**
 * A `uint64` spool offset. Byte counts are never negative and never
 * fractional, and an offset that is either is a producer fault that would
 * otherwise poison every later continuity check (invariant I4).
 */
export function offset(o: Obj, key: string, ctx: string): number {
  const parsed = num(o, key, ctx);
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new Error(`frontend-proto: ${ctx}.${key} must be a non-negative safe integer offset`);
  }
  return parsed;
}
