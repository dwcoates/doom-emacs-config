/**
 * convert/residue.ts — WHAT COULD NOT BE CONVERTED, kept whole.
 *
 * # Why residue exists at all
 *
 * Eager conversion at the edge is a bet: we decide what a vendor record means
 * and throw the rest away. Residue is what makes that bet REVERSIBLE — nothing
 * unconvertible is dropped, it lands durably and verbatim, and the follow-up is
 * a converter (for `vendor_specific`), a model (for `unknown`), or a fix (for
 * `unparsed`). Nothing is ever dropped silently except the exempt set, which is
 * a decision rather than a gap.
 *
 * # The three arms mean three different things
 *
 *   - `vendor_specific` — UNDERSTOOD and deliberately not carried: something one
 *     vendor does that no vendor-agnostic feed can show.
 *   - `unknown` — parsed, not modelled. The discriminator and the FIELD it was
 *     read from both ride along, because a producer that looked in the wrong
 *     place and a genuinely new kind otherwise produce the same entry.
 *   - `unparsed` — a FAILURE rather than a gap; the fields exist so it is
 *     investigable rather than merely counted.
 *
 * # The `kind` spelling is a CROSS-PLANE contract
 *
 * The sidecar mints the same spelling from the file plane, so both planes'
 * residue agrees and a later converter finds every instance under one name:
 * an attachment record is `attachment/<type>`, a system record is
 * `system/<subtype>`, and any other record kind is `<type>` verbatim.
 */
import { create } from "@bufbuild/protobuf";
import type { JsonObject } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { storev1 } from "../proto.js";
import type { PersistEntry } from "../store/persistence.js";
import type { FoldContext } from "./fold-context.js";
import { residueUpsertKey, streamResidueUpsertKey } from "../store/keys.js";

const LOGGER = bindLog({ component: "shim-convert-residue", operation: "shim.convert.residue" });

/**
 * The record entire and verbatim, as a Struct.
 *
 * A record that cannot even be represented as JSON (a cycle, a BigInt the SDK
 * put there) is not a reason to lose it: it degrades to the `unparsed` arm,
 * which carries the bytes as a string and says why.
 */
export function rawStruct(record: unknown): JsonObject | undefined {
  try {
    // protobuf-es types a `google.protobuf.Struct` FIELD as a plain JSON object
    // rather than as a Struct message, so the round-trip through JSON is both
    // the representability check and the conversion.
    const cloned: unknown = JSON.parse(JSON.stringify(record));
    if (typeof cloned !== "object" || cloned === null || Array.isArray(cloned)) {
      return { value: cloned as never };
    }
    return cloned as JsonObject;
  } catch (error) {
    // warn: a defect because a vendor record could not be represented even as structured residue.
    LOGGER.warn(
      { detail: error instanceof Error ? error.message : String(error) },
      "a vendor record could not be represented as a Struct; it lands unparsed instead",
    );
    return undefined;
  }
}

/** Something one vendor does that no vendor-agnostic feed can show. */
export function vendorSpecificResidue(kind: string, record: unknown): storev1.StoreUnservedItem {
  const raw = rawStruct(record);
  if (raw === undefined) return unparsedResidue(kind, "the record could not be represented", record);
  LOGGER.logVerbose({ kind }, "recording a vendor-specific record as residue");
  return create(storev1.StoreUnservedItemSchema, {
    unservedItem: {
      case: "vendorSpecific",
      value: create(storev1.StoreVendorSpecificSchema, { kind, raw }),
    },
  });
}

/** A record we read successfully and do not model. */
export function unknownResidue(
  discriminator: string,
  discriminatorField: string,
  record: unknown,
): storev1.StoreUnservedItem {
  const raw = rawStruct(record);
  if (raw === undefined) {
    return unparsedResidue(discriminator, "the record could not be represented", record);
  }
  LOGGER.debug(
    { discriminator, discriminator_field: discriminatorField },
    "recording an unmodelled record as residue",
  );
  return create(storev1.StoreUnservedItemSchema, {
    unservedItem: {
      case: "unknown",
      value: create(storev1.StoreUnknownSchema, { discriminator, discriminatorField, raw }),
    },
  });
}

/** A record we could not read at all. */
export function unparsedResidue(
  source: string,
  parseError: string,
  record: unknown,
): storev1.StoreUnservedItem {
  let raw: string;
  try {
    raw = JSON.stringify(record) ?? String(record);
  } catch {
    raw = String(record);
  }
  LOGGER.error({ source, parse_error: parseError }, "recording an unparsable record");
  return create(storev1.StoreUnservedItemSchema, {
    unservedItem: {
      case: "unparsed",
      value: create(storev1.StoreUnparsedSchema, {
        source,
        // THE SHIM READS A STREAM, NOT A FILE: there is no byte offset to name,
        // and stating one would be an invention. Zero here means "the stream has
        // no offset", which is the only value it can take on this plane.
        offset: 0n,
        parseError,
        raw,
      }),
    },
  });
}

/**
 * The residue one whole SDK message becomes.
 *
 * `parseError` is set only when the message was recognized and a converter then
 * FAILED on it; without one the message is simply a kind no converter owns.
 */
export function residueForMessage(message: unknown, parseError?: string): storev1.StoreUnservedItem {
  const record = message as { type?: string; subtype?: string };
  const kind = residueKind(record);
  if (parseError !== undefined) return unparsedResidue(kind, parseError, message);
  return unknownResidue(kind, record.subtype === undefined ? "type" : "subtype", message);
}

/**
 * The CROSS-PLANE `kind` spelling for one record.
 *
 * Shared verbatim with the sidecar so both planes name the same thing the same
 * way; a divergence here would make one plane's residue invisible to a search
 * over the other's.
 */
export function residueKind(record: { type?: string; subtype?: string }): string {
  const type = record.type ?? "unknown";
  if (type === "system" && record.subtype !== undefined) return `system/${record.subtype}`;
  if (type === "attachment" && record.subtype !== undefined) return `attachment/${record.subtype}`;
  if (record.subtype !== undefined) return `${type}/${record.subtype}`;
  return type;
}

/**
 * The per-process sequence behind a uuid-less record's residue key.
 *
 * MONOTONIC AND PROCESS-LOCAL: it names an occurrence, because a record with no
 * identity has nothing else to be named by, and a shared counter would make two
 * different unidentified records upsert over each other.
 */
let streamResidueSequence = 0;

function nextStreamResidue(): number {
  streamResidueSequence += 1;
  return streamResidueSequence;
}

/**
 * Wrap one residue as a row.
 *
 * Residue has NO BOOK — that is what makes it unserved — so `agentId` here is
 * only the row's attribution for a human reading the table, never a page.
 */
export function residueEntry(
  context: FoldContext,
  message: unknown,
  residue: storev1.StoreUnservedItem,
  discriminator: string,
): PersistEntry {
  const record = message as { uuid?: string; type?: string; subtype?: string };
  // THE KEY IS THE RECORD'S OWN UUID (ruling, landing 5): both planes convert
  // the same transcript line, and write_id dedup collapses them into one row
  // only when the key bytes match.
  const vendorUuid = record.uuid ?? "";
  const upsertKey =
    vendorUuid === "" ? streamResidueUpsertKey(nextStreamResidue()) : residueUpsertKey(vendorUuid);
  if (vendorUuid === "") {
    LOGGER.debug(
      { kind: residueKind(record), upsert_key: upsertKey },
      "the vendor stated no uuid for this record; its residue is keyed by this process's own stream sequence",
    );
  }
  return {
    agentId: context.mainAgentId,
    upsertKey,
    source: { vendorUuid: vendorUuid === "" ? upsertKey : vendorUuid, discriminator },
    // RESIDUE IS ALREADY UNSERVED. Marking it keep-alive as well would put it
    // under the keepalive arm and lose which of the four reasons it is unserved
    // for, which is the whole information the arm carries.
    keepalive: false,
    turn: context.turnId,
    item: { kind: "residue", residue },
  };
}
