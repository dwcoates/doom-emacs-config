/**
 * The §5.1 conversion & validation contract, converter-agnostic half.
 *
 * Two obligations, both LOUD and never silent (metaprompt no-fallbacks rule):
 *
 * 1. UNKNOWN NEW FIELD → captured, never dropped. Any top-level field of an
 *    SDK stream message that the converter did not consume is copied verbatim
 *    into the containing `Event.extras` (a `google.protobuf.Struct`, surfaced
 *    by protobuf-es as a plain `JsonObject`) and loud-logged ONCE per distinct
 *    `<type>.<field>` path. See {@link Reader}.
 *
 * 2. MISSING EXPECTED FIELD → hard error. A record the converter cannot parse
 *    (no `type` discriminator, unknown `type`/`subtype`, structurally broken)
 *    NEVER becomes a zero value: it is turned into a core `UnparsedEvent`
 *    carrying the raw bytes (capped at 64 KiB), the verbatim error, and the
 *    producer identity. See {@link unparsedEvent} / {@link MissingFieldError}.
 *
 * EXTRAS SCOPING (deliberate, documented): unknown-field diffing operates at
 * the TOP LEVEL of each SDK stream message — the granularity at which the
 * converter dispatches. Nested objects (e.g. `message.usage`) are handed to
 * typed sub-converters that map the fields the proto models; nested variance
 * the proto does not type is either absorbed by the schema's designated
 * `Struct` catch fields (`cache_creation`, `server_tool_use`, …) or bounded by
 * the typed model. This matches the corpus MANIFEST's contract that every
 * fixture decodes with zero unknown-field logs while nested telemetry
 * (`usage.inference_geo`, `usage.iterations`, …) exists in the samples.
 */
import { create } from "@bufbuild/protobuf";
import type { JsonObject } from "@bufbuild/protobuf";
import { shimLog } from "../uds/log.js";
import {
  EventClass,
  EventSchema,
  Plane,
  UnparsedEventSchema,
  type Event,
} from "../uds/proto.js";

/** Producer identity stamped on every claude-shim-produced record (§5.2). */
export const PRODUCER = "claude-shim";

/** UnparsedEvent.raw is bounded; the store caps producers at 64 KiB (§5.2). */
export const RAW_CAP_BYTES = 64 * 1024;

const COMPONENT = "claude-shim-convert";

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
 * This is the passthrough arm's single log line. It is deliberately NOT a
 * warning about broken data: an unmodeled discriminator means the vendor
 * shipped something new, which is expected on a format documented as
 * version-unstable. What matters is that it is visible and that the record
 * survives in `UnknownRecord.raw`.
 */
export function logUnknownDiscriminator(
  discriminator: string,
  parentType: string,
): boolean {
  const key = parentType === "" ? discriminator : `${parentType}:${discriminator}`;
  if (seenDiscriminators.has(key)) return false;
  seenDiscriminators.add(key);
  shimLog(
    COMPONENT,
    { discriminator: key },
    `unmodeled discriminator captured verbatim into UnknownRecord: ${key}`,
  );
  return true;
}

/**
 * Thrown by a sub-converter when an EXPECTED field is missing or a
 * discriminator is unusable. {@link import("./convert.js")} catches it and
 * turns the record into an {@link unparsedEvent}, never a zero value.
 */
export class MissingFieldError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "MissingFieldError";
  }
}

/** The outcome of finalizing a {@link Reader}: the extras Struct + new logs. */
export interface ExtrasOutcome {
  /** Populated iff any field went unmapped; undefined leaves Event.extras unset. */
  extras?: JsonObject;
  /** `<type>.<field>` paths newly loud-logged by this finalize (for tests). */
  logged: string[];
}

/**
 * A single SDK stream message under conversion, tracking which top-level keys
 * the converter consumed so the leftover set can be captured into extras.
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
   * Mark EVERY remaining key consumed, for the passthrough (UnknownRecord)
   * path only.
   *
   * The passthrough arm already stores the record whole in `UnknownRecord.raw`,
   * so letting {@link finish} also copy each field into `Event.extras` would
   * duplicate the payload AND — worse — log every field of an unknown family
   * as a "new field", which is noise that hides the genuine unknown-field
   * signal on families we DO model. The one honest log for this record is the
   * unknown-discriminator log, emitted by {@link logUnknownDiscriminator}.
   */
  consumeAll(): void {
    for (const k of Object.keys(this.raw)) this.consumed.add(k);
  }

  /**
   * Mark a top-level field RECOGNIZED-but-unmodeled: consumed (so it is not a
   * loud "unknown field"), yet preserved into extras when present so NOTHING
   * is dropped. Used for known transport/context telemetry the proto's typed
   * model deliberately omits (e.g. `ttft_ms` on a stream_event, a subagent
   * user message's `subagent_type`). Documented per call site.
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
   * field — captured into extras (never dropped) and loud-logged once per
   * `<typeLabel>.<field>`. Returns the extras object (undefined when empty)
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
        shimLog(COMPONENT, { type: typeLabel, field: k }, `unknown field captured into Event.extras: ${path}`);
      }
    }
    const keys = Object.keys(this.extras);
    return { extras: keys.length > 0 ? (this.extras as JsonObject) : undefined, logged };
  }
}

/**
 * Build a core `UnparsedEvent` Event for a record conversion could not parse.
 * `raw` is the source line/object; it is UTF-8 encoded and capped at 64 KiB.
 * Always loud-logged: a record that fails to parse is an anomaly, never noise.
 */
export function unparsedEvent(
  raw: string | Uint8Array,
  error: string,
  fields: { sessionId?: string; requestId?: string; producedAtMs?: number } = {},
): Event {
  const bytes = typeof raw === "string" ? new TextEncoder().encode(raw) : raw;
  const capped = bytes.length > RAW_CAP_BYTES ? bytes.subarray(0, RAW_CAP_BYTES) : bytes;
  shimLog(COMPONENT, { session: fields.sessionId ?? "" }, `UNPARSED event (${error}); ${bytes.length} raw bytes preserved`);
  return create(EventSchema, {
    sessionId: fields.sessionId ?? "",
    seq: 0n,
    plane: Plane.STREAM,
    class: EventClass.PERSISTENT,
    requestId: fields.requestId ?? "",
    producedAtMs: BigInt(fields.producedAtMs ?? Date.now()),
    payload: {
      case: "unparsed",
      value: create(UnparsedEventSchema, {
        sourcePath: "",
        byteOffset: 0n,
        raw: capped,
        error,
        producer: PRODUCER,
      }),
    },
  });
}

function isObject(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}
