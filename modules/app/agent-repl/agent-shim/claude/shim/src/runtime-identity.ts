/**
 * Canonical SHA-256 fingerprints for the query's runtime identity evidence.
 *
 * `QueryRuntimeIdentity` carries five `EvidenceFingerprint` fields whose whole
 * purpose is to let two turns be compared for "was this the same effective
 * configuration?" without persisting the configuration itself. A fingerprint is
 * only worth that if it is CANONICAL: the same configuration must hash to the
 * same digest regardless of the order a key happened to be inserted in, so the
 * encoder sorts object keys at every depth rather than trusting `JSON.stringify`
 * insertion order.
 *
 * Non-JSON values are encoded by SHAPE, not dropped. The SDK options object
 * legitimately carries a `canUseTool` callback and an `AbortController`; both
 * are part of the effective configuration, and silently omitting them would make
 * two genuinely different option sets fingerprint identically. They are encoded
 * as stable type markers instead, which is the most any hash can honestly say
 * about a live object.
 */

import { createHash } from "node:crypto";
import { create } from "@bufbuild/protobuf";
import {
  EvidenceFingerprintSchema,
  FingerprintUnavailableSchema,
  QueryRuntimeIdentitySchema,
} from "./uds/proto.js";
import type { QueryRuntimeIdentity } from "./uds/proto.js";

/** Encode a value so that equal configurations always produce equal text. */
export function canonicalEncode(value: unknown): string {
  if (value === null) return "null";
  if (value === undefined) return "undefined";
  switch (typeof value) {
    case "string":
      return JSON.stringify(value);
    case "number":
      return Number.isFinite(value) ? String(value) : `"${String(value)}"`;
    case "boolean":
      return value ? "true" : "false";
    case "bigint":
      return `"${value.toString()}n"`;
    case "function":
      return `"[function ${value.name || "anonymous"}]"`;
    case "symbol":
      return `"[symbol ${String(value)}]"`;
  }
  if (Array.isArray(value)) {
    return `[${value.map(canonicalEncode).join(",")}]`;
  }
  const object = value as Record<string, unknown>;
  // A class instance carries no enumerable configuration of its own worth
  // hashing (AbortController is the live example), so it is encoded by its
  // constructor name — different classes stay distinguishable, and the same
  // class never perturbs the digest between two otherwise identical queries.
  const prototype = Object.getPrototypeOf(object);
  if (prototype !== null && prototype !== Object.prototype) {
    return `"[object ${object.constructor?.name ?? "unknown"}]"`;
  }
  const keys = Object.keys(object).sort();
  return `{${keys.map((key) => `${JSON.stringify(key)}:${canonicalEncode(object[key])}`).join(",")}}`;
}

/** Lowercase hexadecimal SHA-256 of a value's canonical encoding. */
export function canonicalSha256(value: unknown): string {
  return createHash("sha256").update(canonicalEncode(value)).digest("hex");
}

/** What only the SHIM knows about the query, as opposed to what init reports. */
export interface ShimRuntimeFacts {
  /** Vendor session id confirmed for this query. */
  vendorSessionId: string;
  /** Model this session resolved from the SDK's own init authority. */
  effectiveModel: string;
  /** Claude Agent SDK package version loaded by this shim process. */
  sdkVersion: string;
  /** Shim build identity embedded in the running bundle. */
  shimBuildSha: string;
  /** Exact options object this shim passed to the SDK's query(). */
  effectiveQueryOptions?: Record<string, unknown> | undefined;
  /** System prompt this shim passed to the SDK's query(). */
  contextPrefix?: unknown;
}

/**
 * Build the query's durable runtime identity from the SDK's `system:init`
 * message and the facts only the shim holds.
 *
 * IT IS A PURE FUNCTION ON PURPOSE. This evidence is what the daemon's turn
 * accounting reconciles every settled turn against, and the whole class of
 * defect it has suffered — a field read under a key the SDK does not use, and so
 * silently recorded as "" — is invisible from the session's integration rig and
 * obvious from a unit test that hands it one real init payload.
 */
export function queryRuntimeIdentity(
  init: Record<string, unknown>,
  shim: ShimRuntimeFacts,
): QueryRuntimeIdentity {
  // THE SDK MIXES SPELLINGS IN ONE MESSAGE. `claude_code_version` and
  // `fast_mode_state` arrive snake_case while `apiKeySource` arrives camel, so a
  // snake-only reader produced "" for exactly the camel fields and every settled
  // turn was condemned for evidence the message was in fact carrying. Each field
  // names every spelling it is known by, the way proto/convert.ts already does
  // for this same payload.
  const text = (...fields: string[]): string => {
    for (const field of fields) {
      const value = init[field];
      if (typeof value === "string") return value;
    }
    return "";
  };
  const unavailable = (cause: string) =>
    create(EvidenceFingerprintSchema, {
      evidence: { case: "unavailable", value: create(FingerprintUnavailableSchema, { cause }) },
    });
  // A fingerprint over evidence that is not there would hash "absent" into a
  // digest indistinguishable from a real empty configuration, so an absent
  // source stays EXPLICITLY unavailable rather than silently hashed.
  const fingerprintOf = (value: unknown, cause: string) =>
    value === undefined
      ? unavailable(cause)
      : create(EvidenceFingerprintSchema, { evidence: { case: "sha256", value: canonicalSha256(value) } });
  return create(QueryRuntimeIdentitySchema, {
    vendorSessionId: shim.vendorSessionId,
    effectiveModel: shim.effectiveModel,
    sdkVersion: shim.sdkVersion,
    claudeCodeVersion: text("claude_code_version", "claudeCodeVersion"),
    shimBuildSha: shim.shimBuildSha,
    authSource: text("apiKeySource", "api_key_source"),
    // NOT INITIALIZATION EVIDENCE. The subscription type is reported by the
    // usage service, and the session records it on every AccountUsageObservation
    // it samples, so the accounting record already holds it at both of a turn's
    // boundaries. Blocking init on a usage round-trip to duplicate it here would
    // buy the ledger nothing.
    subscriptionType: "",
    fastModeState: text("fast_mode_state", "fastModeState"),
    // The EXPLANATION for the state, which the SDK emits only when fast mode is
    // disabled. Absent while fast mode is on is the correct reading, not a gap.
    fastModeReason: text("fast_mode_disabled_reason", "fastModeDisabledReason"),
    effectiveOptions: fingerprintOf(
      shim.effectiveQueryOptions,
      "the shim was constructed without the effective SDK options it passed to query()",
    ),
    settings: unavailable(
      "the shim declares its settingSources to the SDK but never reads the merged user/project/local settings, so it holds nothing to fingerprint",
    ),
    tools: fingerprintOf(init["tools"], "the Agent SDK initialization message carried no tools"),
    mcp: fingerprintOf(
      init["mcp_servers"] ?? init["mcpServers"],
      "the Agent SDK initialization message carried no mcp_servers",
    ),
    contextPrefix: fingerprintOf(
      shim.contextPrefix,
      "the shim was constructed without the system prompt it passed to query()",
    ),
  });
}
