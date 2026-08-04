/** Canonical validation and normalization for one assistant Messages API usage block. */
import type { JsonObject, JsonValue } from "@bufbuild/protobuf";

const MODELED_API_USAGE_FIELDS = new Set([
  "input_tokens", "inputTokens", "output_tokens", "outputTokens",
  "cache_read_input_tokens", "cacheReadInputTokens",
  "cache_creation_input_tokens", "cacheCreationInputTokens",
  "cache_creation", "cacheCreation", "server_tool_use", "serverToolUse",
  "service_tier", "serviceTier", "iterations", "speed",
  "inference_geo", "inferenceGeo", "fallback_credit", "fallbackCredit",
  "output_tokens_details", "outputTokensDetails", "unmodeled_usage", "unmodeledUsage",
  "cache_diagnostic", "cacheDiagnostic",
]);

/** A known usage field had a representation the typed wire cannot express faithfully. */
export class InvalidModeledUsageError extends Error {
  readonly fieldPath: string;

  constructor(
    readonly field: string,
    readonly rawValue: unknown,
    readonly rawUsage: Record<string, unknown>,
    detail: string,
  ) {
    const fieldPath = field === "usage" || field.startsWith("usage.")
      ? field
      : `usage.${field}`;
    super(`assistant usage field ${JSON.stringify(fieldPath)} ${detail}`);
    this.name = "InvalidModeledUsageError";
    this.fieldPath = fieldPath;
  }
}

/** One validated usage block shared by protobuf conversion and structured accounting logs. */
export interface NormalizedApiUsage {
  rawSdkUsage: JsonObject;
  inputTokens: number;
  outputTokens: number;
  cacheReadInputTokens: number;
  cacheCreationInputTokens: number;
  cacheCreation?: JsonObject;
  serverToolUse?: JsonObject;
  serviceTier: string;
  iterations?: JsonValue[];
  speed: string;
  inferenceGeo: string;
  fallbackCredit?: JsonObject;
  outputTokensDetails?: JsonObject;
  unmodeledUsage?: JsonObject;
  cacheDiagnostic?: JsonObject;
  cacheCreation5mInputTokens: number;
  cacheCreation1hInputTokens: number;
  unknownUsageFields: JsonObject;
  promptCache:
    | { case: "empty"; totalPromptInputTokens: 0 }
    | {
        case: "rates";
        totalPromptInputTokens: number;
        cacheHitRate: number;
        cacheWriteRate: number;
        uncachedInputRate: number;
      };
}

/** Validate one raw assistant API usage object without defaults for invalid shapes. */
export function normalizeApiUsage(rawValue: unknown): NormalizedApiUsage {
  if (!isObject(rawValue)) {
    throw new InvalidModeledUsageError("usage", rawValue, {}, "must be an object");
  }
  const raw = rawValue;
  // Capture the SDK object before any typed field is normalized. The clone is
  // also the single JSON-representability gate required by protobuf Struct.
  const rawSdkUsage = copyJsonObject(raw, raw, "usage", new WeakSet<object>());
  const inputTokens = requiredCounter(raw, "input_tokens", ["input_tokens", "inputTokens"]);
  const outputTokens = requiredCounter(raw, "output_tokens", ["output_tokens", "outputTokens"]);
  const cacheReadInputTokens = nullableCounter(raw, "cache_read_input_tokens", ["cache_read_input_tokens", "cacheReadInputTokens"]);
  const cacheCreationInputTokens = nullableCounter(raw, "cache_creation_input_tokens", ["cache_creation_input_tokens", "cacheCreationInputTokens"]);
  const totalPromptInputTokens = inputTokens + cacheReadInputTokens + cacheCreationInputTokens;
  const cacheCreation = nullableObject(raw, "cache_creation", ["cache_creation", "cacheCreation"]);
  const serverToolUse = nullableObject(raw, "server_tool_use", ["server_tool_use", "serverToolUse"]);
  const explicitUnmodeled = nullableObject(raw, "unmodeled_usage", ["unmodeled_usage", "unmodeledUsage"]);
  const unknownUsageFields = Object.fromEntries(
    Object.entries(raw).filter(([field]) => !MODELED_API_USAGE_FIELDS.has(field)),
  ) as JsonObject;
  const unmodeledUsage = {
    ...(explicitUnmodeled ?? {}),
    ...unknownUsageFields,
  };

  return {
    rawSdkUsage,
    inputTokens,
    outputTokens,
    cacheReadInputTokens,
    cacheCreationInputTokens,
    ...(cacheCreation === undefined ? {} : { cacheCreation }),
    ...(serverToolUse === undefined ? {} : { serverToolUse }),
    serviceTier: nullableString(raw, "service_tier", ["service_tier", "serviceTier"]),
    ...nullableArray(raw, "iterations", ["iterations"]),
    speed: nullableString(raw, "speed", ["speed"]),
    inferenceGeo: nullableString(raw, "inference_geo", ["inference_geo", "inferenceGeo"]),
    ...optionalObjectProperty(raw, "fallbackCredit", "fallback_credit", ["fallback_credit", "fallbackCredit"]),
    ...optionalObjectProperty(raw, "outputTokensDetails", "output_tokens_details", ["output_tokens_details", "outputTokensDetails"]),
    ...(Object.keys(unmodeledUsage).length === 0 ? {} : { unmodeledUsage }),
    ...optionalObjectProperty(raw, "cacheDiagnostic", "cache_diagnostic", ["cache_diagnostic", "cacheDiagnostic"]),
    cacheCreation5mInputTokens: nestedOptionalCounter(raw, cacheCreation, "cache_creation.ephemeral_5m_input_tokens", "ephemeral_5m_input_tokens"),
    cacheCreation1hInputTokens: nestedOptionalCounter(raw, cacheCreation, "cache_creation.ephemeral_1h_input_tokens", "ephemeral_1h_input_tokens"),
    unknownUsageFields,
    promptCache: totalPromptInputTokens === 0
      ? { case: "empty", totalPromptInputTokens: 0 }
      : {
          case: "rates",
            totalPromptInputTokens,
            cacheHitRate: cacheReadInputTokens / totalPromptInputTokens,
            cacheWriteRate: cacheCreationInputTokens / totalPromptInputTokens,
            uncachedInputRate: inputTokens / totalPromptInputTokens,
        },
  };
}

/** Clone one raw usage object while proving every source value is Struct-safe. */
function copyJsonObject(
  rawUsage: Record<string, unknown>,
  value: Record<string, unknown>,
  path: string,
  ancestors: WeakSet<object>,
): JsonObject {
  if (ancestors.has(value)) {
    throw new InvalidModeledUsageError(path, value, rawUsage, "contains a circular reference");
  }
  if (Object.getOwnPropertySymbols(value).length !== 0) {
    throw new InvalidModeledUsageError(path, value, rawUsage, "contains symbol-keyed data that JSON cannot represent");
  }
  ancestors.add(value);
  try {
    return Object.fromEntries(Object.entries(value).map(([key, entry]) => [
      key,
      copyJsonValue(rawUsage, entry, `${path}.${key}`, ancestors),
    ])) as JsonObject;
  } finally {
    ancestors.delete(value);
  }
}

/** Clone one raw JSON value without coercing nulls, numbers, or containers. */
function copyJsonValue(
  rawUsage: Record<string, unknown>,
  value: unknown,
  path: string,
  ancestors: WeakSet<object>,
): JsonValue {
  if (value === null || typeof value === "string" || typeof value === "boolean") return value;
  if (typeof value === "number") {
    if (!Number.isFinite(value)) {
      throw new InvalidModeledUsageError(path, value, rawUsage, "must be a finite JSON number");
    }
    return value;
  }
  if (Array.isArray(value)) {
    if (ancestors.has(value)) {
      throw new InvalidModeledUsageError(path, value, rawUsage, "contains a circular reference");
    }
    ancestors.add(value);
    try {
      return Array.from({ length: value.length }, (_unused, index) => {
        if (!Object.hasOwn(value, index)) {
          throw new InvalidModeledUsageError(`${path}[${index}]`, undefined, rawUsage, "is an array hole that JSON cannot preserve");
        }
        return copyJsonValue(rawUsage, value[index], `${path}[${index}]`, ancestors);
      });
    } finally {
      ancestors.delete(value);
    }
  }
  if (isObject(value)) return copyJsonObject(rawUsage, value, path, ancestors);
  throw new InvalidModeledUsageError(path, value, rawUsage, `has non-JSON type ${typeof value}`);
}

function isObject(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}

function aliasedValue(
  raw: Record<string, unknown>,
  field: string,
  aliases: readonly string[],
): { present: boolean; value: unknown } {
  const present = aliases.filter((alias) => Object.hasOwn(raw, alias));
  if (present.length > 1) {
    throw new InvalidModeledUsageError(field, Object.fromEntries(present.map((alias) => [alias, raw[alias]])), raw, `has conflicting aliases ${present.join(", ")}`);
  }
  return present.length === 0 ? { present: false, value: undefined } : { present: true, value: raw[present[0]!] };
}

function validCounter(raw: Record<string, unknown>, field: string, value: unknown): number {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new InvalidModeledUsageError(field, value, raw, "must be a non-negative safe integer");
  }
  return value;
}

function requiredCounter(raw: Record<string, unknown>, field: string, aliases: readonly string[]): number {
  const value = aliasedValue(raw, field, aliases);
  if (!value.present) throw new InvalidModeledUsageError(field, undefined, raw, "is required");
  return validCounter(raw, field, value.value);
}

function nullableCounter(raw: Record<string, unknown>, field: string, aliases: readonly string[]): number {
  const value = aliasedValue(raw, field, aliases);
  if (!value.present) throw new InvalidModeledUsageError(field, undefined, raw, "is required");
  if (value.value === null) return 0;
  return validCounter(raw, field, value.value);
}

function nullableObject(raw: Record<string, unknown>, field: string, aliases: readonly string[]): JsonObject | undefined {
  const value = aliasedValue(raw, field, aliases);
  if (!value.present || value.value === null) return undefined;
  if (!isObject(value.value)) throw new InvalidModeledUsageError(field, value.value, raw, "must be an object or null");
  return value.value as JsonObject;
}

function optionalObjectProperty<K extends string>(
  raw: Record<string, unknown>,
  property: K,
  field: string,
  aliases: readonly string[],
): { [P in K]?: JsonObject } {
  const value = nullableObject(raw, field, aliases);
  return value === undefined ? {} : { [property]: value } as { [P in K]?: JsonObject };
}

function nullableString(raw: Record<string, unknown>, field: string, aliases: readonly string[]): string {
  const value = aliasedValue(raw, field, aliases);
  if (!value.present || value.value === null) return "";
  if (typeof value.value !== "string") throw new InvalidModeledUsageError(field, value.value, raw, "must be a string or null");
  return value.value;
}

function nullableArray(
  raw: Record<string, unknown>,
  field: string,
  aliases: readonly string[],
): { iterations?: JsonValue[] } {
  const value = aliasedValue(raw, field, aliases);
  if (!value.present || value.value === null) return {};
  if (!Array.isArray(value.value)) throw new InvalidModeledUsageError(field, value.value, raw, "must be an array or null");
  return { iterations: value.value as JsonValue[] };
}

function nestedOptionalCounter(
  raw: Record<string, unknown>,
  parent: JsonObject | undefined,
  field: string,
  key: string,
): number {
  if (parent === undefined || !Object.hasOwn(parent, key)) return 0;
  return validCounter(raw, field, parent[key]);
}
