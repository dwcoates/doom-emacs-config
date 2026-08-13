/**
 * The conversion & validation contract, converter-agnostic half.
 *
 * THE PRODUCER CONVERTS AT THE EDGE NOW. What leaves this shim is already
 * vendor-neutral (`conversation.v1` / `protocol.v1`), and the vendor material
 * that cannot be carried into a neutral feed does not travel at all: it lands
 * on `agentshim.v1`'s `InternalEntry.unconverted`, which has no external half
 * and therefore no path to the daemon. That is what makes eager conversion safe
 * rather than lossy — a record we could not place is stored WHOLE, so the
 * decision not to model it stays reversible from stored data.
 *
 * THREE UNCONVERTED ARMS, and they are not interchangeable:
 *
 * 1. {@link vendorSpecificEntry} — UNDERSTOOD, and deliberately not carried. We
 *    know exactly what the record is and have decided a vendor-agnostic feed
 *    cannot show it. The follow-up is a CONVERTER.
 * 2. {@link unknownEntry} — PARSED but not MODELED: a discriminator no arm
 *    matched. The SDK union grew 11 -> 38 members across 0.1.77 -> 0.3.220, so
 *    this is the expected steady state, not an error. The follow-up is a MODEL.
 * 3. {@link unparsedEntry} — could not be READ at all. A FAILURE, not a gap.
 *
 * UNKNOWN NEW FIELDS still get their loud once-per-path log (see {@link
 * Reader}), and on a family this shim DOES convert they are additionally
 * carried whole into a companion {@link vendorSpecificEntry} — see {@link
 * unknownFieldsEntry}. The retired `Event.extras` Struct was the old carrier and
 * has no successor on any of the five surfaces; a companion internal record is
 * how "nothing is ever dropped" stays true without inventing a field.
 *
 * EXTRAS SCOPING (deliberate, documented, unchanged): unknown-field diffing
 * operates at the TOP LEVEL of each SDK stream message — the granularity at
 * which the converter dispatches. Nested variance is either absorbed by a
 * `Struct` field or bounded by the typed model.
 */
import { create } from "@bufbuild/protobuf";
import type { JsonObject } from "@bufbuild/protobuf";
import { bindLog } from "../uds/log.js";
import {
  EntrySchema,
  InternalEntrySchema,
  PlaneSchema,
  PlaneStreamSchema,
  UnknownEntrySchema,
  UnparsedEntrySchema,
  VendorSpecificEntrySchema,
  type Entry,
  type InternalEntry,
} from "../uds/proto.js";

/** Producer identity stamped on every claude-shim-produced record. */
export const PRODUCER = "claude-shim";

/** `UnparsedEntry.raw` is bounded; the store caps producers at 64 KiB. */
export const RAW_CAP_BYTES = 64 * 1024;

/**
 * `UnparsedEntry.source` for everything this shim reads. The shim observes the
 * SDK stream and nothing else; the sidecar, which reads files, states a path.
 */
export const STREAM_SOURCE = "claude-sdk-stream";

const COMPONENT = "claude-shim-convert";
const LOGGER = bindLog({ component: COMPONENT, operation: "shim.extras.capture" });

/**
 * Distinct `<type>.<field>` paths already loud-logged, so the "once per
 * distinct field path" contract holds across the whole process lifetime.
 * Exposed for reset only through {@link __resetExtrasSeen} (tests).
 */
const seenFieldPaths = new Set<string>();

/**
 * Distinct unknown DISCRIMINATORS already loud-logged, so the passthrough
 * path logs once per new family rather than once per message — a family that
 * arrives on every turn would otherwise drown the log.
 */
const seenDiscriminators = new Set<string>();

/** Test-only: clear the once-per-path log dedup so cases stay independent. */
export function __resetExtrasSeen(): void {
  seenFieldPaths.clear();
  seenDiscriminators.clear();
}

/**
 * Loud-log an unrecognized union discriminator ONCE per distinct value, and
 * report whether this call was the one that logged it.
 *
 * Deliberately NOT a warning about broken data: an unmodeled discriminator
 * means the vendor shipped something new, which is expected on a format
 * documented as version-unstable. What matters is that it is visible and that
 * the record survives whole in `UnknownEntry.raw`.
 */
export function logUnknownDiscriminator(
  discriminator: string,
  parentType: string,
): boolean {
  const key = parentType === "" ? discriminator : `${parentType}:${discriminator}`;
  if (seenDiscriminators.has(key)) return false;
  seenDiscriminators.add(key);
  LOGGER.log(
    { discriminator: key },
    `unmodeled discriminator captured verbatim into UnknownEntry: ${key}`,
  );
  return true;
}

/**
 * Thrown by a sub-converter when an EXPECTED field is missing or a
 * discriminator is unusable. {@link import("./convert.js")} catches it and
 * turns the record into an {@link unparsedEntry}, never a zero value.
 */
export class MissingFieldError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "MissingFieldError";
  }
}

/** The outcome of finalizing a {@link Reader}: leftover fields + new logs. */
export interface ExtrasOutcome {
  /** Populated iff any field went unmapped; undefined when everything mapped. */
  extras?: JsonObject;
  /** `<type>.<field>` paths newly loud-logged by this finalize (for tests). */
  logged: string[];
}

/**
 * A single SDK stream message under conversion, tracking which top-level keys
 * the converter consumed so the leftover set can be captured.
 *
 * Getters coerce defensively (a wrong-typed source value yields the type's
 * zero, not a throw) — a genuinely malformed record is the sub-converter's
 * job to reject via {@link MissingFieldError}, not the reader's.
 */
export class Reader {
  private readonly consumed = new Set<string>();
  private readonly extras: Record<string, unknown> = {};

  constructor(private readonly raw: Record<string, unknown>) {}

  /** Raw value under the first present of `keys`; marks every alias consumed. */
  val(...keys: string[]): unknown {
    let found: unknown;
    let has = false;
    for (const k of keys) {
      this.consumed.add(k);
      if (!has && Object.prototype.hasOwnProperty.call(this.raw, k)) {
        found = this.raw[k];
        has = true;
      }
    }
    return found;
  }

  /** True iff any alias is present (regardless of value). */
  has(...keys: string[]): boolean {
    return keys.some((k) => Object.prototype.hasOwnProperty.call(this.raw, k));
  }

  str(...keys: string[]): string {
    const v = this.val(...keys);
    return typeof v === "string" ? v : "";
  }

  /** Preserve optional-string presence while rejecting an unusable supplied id. */
  optionalNonBlankStr(...keys: string[]): string | undefined {
    const present = this.has(...keys);
    const v = this.val(...keys);
    if (!present) return undefined;
    if (typeof v !== "string" || v === "") {
      throw new MissingFieldError(`present ${keys.map((key) => `\`${key}\``).join("/")} must be a non-empty string`);
    }
    return v;
  }

  num(...keys: string[]): number {
    const v = this.val(...keys);
    return typeof v === "number" && Number.isFinite(v) ? v : 0;
  }

  /** int64/int32 fields want a bigint; NaN/absent → 0n, floats truncate. */
  big(...keys: string[]): bigint {
    const v = this.val(...keys);
    return typeof v === "number" && Number.isFinite(v) ? BigInt(Math.trunc(v)) : 0n;
  }

  bool(...keys: string[]): boolean {
    return this.val(...keys) === true;
  }

  obj(...keys: string[]): Record<string, unknown> | undefined {
    const v = this.val(...keys);
    return isObject(v) ? v : undefined;
  }

  arr(...keys: string[]): unknown[] | undefined {
    const v = this.val(...keys);
    return Array.isArray(v) ? v : undefined;
  }

  strList(...keys: string[]): string[] {
    const v = this.val(...keys);
    return Array.isArray(v) ? v.filter((x): x is string => typeof x === "string") : [];
  }

  /** A JSON object suitable for a Struct field; wrong type → undefined. */
  struct(...keys: string[]): JsonObject | undefined {
    const v = this.val(...keys);
    return isObject(v) ? (v as JsonObject) : undefined;
  }

  /**
   * Mark purely-structural keys consumed WITHOUT carrying them (the `type` /
   * `subtype` discriminators the converter dispatches on).
   */
  ignore(...keys: string[]): void {
    for (const k of keys) this.consumed.add(k);
  }

  /**
   * Mark EVERY remaining key consumed, for the passthrough paths only.
   *
   * `VendorSpecificEntry.raw` and `UnknownEntry.raw` already store the record
   * whole, so letting {@link finish} also collect each field would duplicate the
   * payload AND — worse — log every field of an unplaced family as a "new
   * field", which is noise that hides the genuine unknown-field signal on
   * families we DO convert.
   */
  consumeAll(): void {
    for (const k of Object.keys(this.raw)) this.consumed.add(k);
  }

  /**
   * Mark a top-level field RECOGNIZED-but-unmodeled: consumed (so it is not a
   * loud "unknown field"), yet preserved into the leftover set when present so
   * NOTHING is dropped. Documented per call site.
   */
  carry(...keys: string[]): void {
    for (const k of keys) {
      this.consumed.add(k);
      if (Object.prototype.hasOwnProperty.call(this.raw, k)) {
        this.extras[k] = this.raw[k];
      }
    }
  }

  /**
   * Finalize: every top-level key not yet consumed is a genuinely UNKNOWN new
   * field — collected (never dropped) and loud-logged once per
   * `<typeLabel>.<field>`. Returns the leftover object (undefined when empty)
   * plus the paths newly logged by this call.
   */
  finish(typeLabel: string): ExtrasOutcome {
    const logged: string[] = [];
    for (const k of Object.keys(this.raw)) {
      if (this.consumed.has(k)) continue;
      this.extras[k] = this.raw[k];
      const path = `${typeLabel}.${k}`;
      if (!seenFieldPaths.has(path)) {
        seenFieldPaths.add(path);
        logged.push(path);
        LOGGER.log({ type: typeLabel, field: k }, `unknown field captured into a companion VendorSpecificEntry: ${path}`);
      }
    }
    const keys = Object.keys(this.extras);
    return { extras: keys.length > 0 ? (this.extras as JsonObject) : undefined, logged };
  }
}

// ---------------------------------------------------------------------------
// The unconverted arms
// ---------------------------------------------------------------------------

/**
 * Wrap one `InternalEntry.unconverted` arm as a whole {@link Entry}.
 *
 * NO EXTERNAL HALF, and that absence is the contract rather than an omission: a
 * record we could not place has nothing to hand the daemon, so it has no field
 * a forwarder could read. "An unconvertible record cannot reach a page" is
 * therefore a fact about the record's SHAPE, not a rule a query applies.
 *
 * The plane is always `stream`: this shim observes the SDK live and never reads
 * a file. `write_id` is deliberately left empty here and minted once by the
 * store client when the record is first handed to a write, because a replay
 * must re-present the SAME identity it was first delivered under.
 */
function internalOnly(unconverted: InternalEntry["unconverted"]): Entry {
  return create(EntrySchema, {
    internal: create(InternalEntrySchema, {
      plane: create(PlaneSchema, { plane: { case: "stream", value: create(PlaneStreamSchema, {}) } }),
      unconverted,
    }),
  });
}

/**
 * A fact specific to one vendor, UNDERSTOOD and deliberately not carried into a
 * vendor-agnostic feed.
 *
 * This is where the bulk of the SDK stream now lands. The old converter
 * transliterated all 38 SDK families into vendor-shaped protos and shipped them
 * across the wire for the daemon to interpret; the neutral model has a home for
 * the LIFECYCLE facts (bookkeeping) and for CONVERSATION content (which the
 * file plane produces, because only it can resolve a parent). Everything else
 * is understood, unrenderable in a neutral feed, and stored whole here.
 */
export function vendorSpecificEntry(kind: string, raw: JsonObject): Entry {
  return internalOnly({
    case: "vendorSpecific",
    value: create(VendorSpecificEntrySchema, { kind, raw }),
  });
}

/**
 * The companion record carrying the top-level fields a CONVERTED family grew
 * that this converter does not read.
 *
 * `Event.extras` used to carry these, and nothing on the five surfaces
 * replaces it. Emitting a companion internal record keeps the never-drop
 * contract literally true without inventing a field: the fields are durable,
 * they are attributable to the family that grew them, and they cannot reach a
 * consumer that might act on a value nobody modeled.
 *
 * Only ever built when {@link Reader.finish} found something; an ordinary
 * record produces none.
 */
export function unknownFieldsEntry(typeLabel: string, extras: JsonObject): Entry {
  return vendorSpecificEntry(`${typeLabel}.unknown-fields`, extras);
}

/**
 * A record we READ but do not MODEL — a discriminator no arm matched.
 *
 * `discriminatorField` names WHERE the value was read from, because a producer
 * that looked in the wrong place and a record with a genuinely new kind produce
 * the same entry otherwise.
 */
export function unknownEntry(
  discriminator: string,
  discriminatorField: string,
  raw: JsonObject,
): Entry {
  return internalOnly({
    case: "unknown",
    value: create(UnknownEntrySchema, { discriminator, discriminatorField, raw }),
  });
}

/**
 * A record we could not read at all. `raw` is capped at 64 KiB of UTF-8.
 * Always loud-logged unless the caller states it already emitted the causal
 * record: a record that fails to parse is an anomaly, never noise.
 */
export function unparsedEntry(
  raw: string,
  parseError: string,
  fields: {
    /** Which conversation it belonged to, when the record said. */
    sessionId?: string;
    /** Where in the stream it came from; defaults to {@link STREAM_SOURCE}. */
    source?: string;
    /** Byte offset within that source, 0 for a stream with no addressable one. */
    offset?: bigint;
    /** False when the converter already emitted the owning causal record. */
    log?: boolean;
  } = {},
): Entry {
  const capped = capUtf8(raw, RAW_CAP_BYTES);
  if (fields.log !== false) {
    LOGGER.log({
      level: "error",
      ...(fields.sessionId === undefined ? {} : { claude_session_id: fields.sessionId }),
      producer: PRODUCER,
      parse_error: parseError,
      raw_bytes: byteLength(raw),
    }, `UNPARSED record (${parseError}); ${byteLength(raw)} raw bytes preserved`);
  }
  return internalOnly({
    case: "unparsed",
    value: create(UnparsedEntrySchema, {
      source: fields.source ?? STREAM_SOURCE,
      offset: fields.offset ?? 0n,
      parseError,
      raw: capped,
    }),
  });
}

/** UTF-8 byte length of `s`. */
function byteLength(s: string): number {
  return new TextEncoder().encode(s).length;
}

/**
 * Truncate `s` to at most `cap` UTF-8 bytes WITHOUT splitting a code point.
 *
 * `UnparsedEntry.raw` is a proto3 `string`, which must be valid UTF-8: capping
 * at a raw byte index could cut a multi-byte sequence in half and produce a
 * value the wire rejects. `TextDecoder` with `fatal: false` would silently
 * substitute U+FFFD instead, which is a corruption of the very bytes this field
 * exists to preserve — so the cut is walked back to a boundary instead.
 */
function capUtf8(s: string, cap: number): string {
  const bytes = new TextEncoder().encode(s);
  if (bytes.length <= cap) return s;
  let end = cap;
  // A UTF-8 continuation byte is 0b10xxxxxx; walk back off any partial sequence.
  while (end > 0 && (bytes[end] & 0xc0) === 0x80) end--;
  return new TextDecoder().decode(bytes.subarray(0, end));
}

function isObject(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}
