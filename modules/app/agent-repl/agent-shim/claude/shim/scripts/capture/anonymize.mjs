/**
 * The corpus anonymizer — the walker `testdata/corpus/MANIFEST.md` describes,
 * implemented once so a capture run and any later re-harvest apply the SAME
 * rules to the SAME shapes.
 *
 * MANIFEST.md states the contract this file implements, verbatim:
 *
 *   - Secret-looking tokens (Anthropic/GitHub/AWS/Slack keys, Bearer/JWT,
 *     `authorization`/`api-key`/`password`/`secret` values) -> `REDACTED`.
 *   - Strings > 900 chars truncated to first 400 +
 *     `…[TRUNCATED N chars for corpus]`.
 *   - Opaque blobs (`signature`, base64 image `data`) truncated to a short
 *     prefix + `…[TRUNCATED N chars]`.
 *   - ALL structural fields preserved verbatim: uuids, session ids, paths,
 *     timestamps, tool-use ids, keys.
 *
 * WHY THE RULES ARE IN THIS ORDER: key-based redaction runs FIRST, so a secret
 * that also happens to be long is redacted rather than truncated (a truncated
 * secret is still a leaked prefix). Blob truncation runs next, because a
 * signature under 900 chars must still be cut. Length truncation runs last, as
 * the general fallback.
 *
 * WHAT IS DELIBERATELY NOT TOUCHED: object KEYS, array ORDER, numbers,
 * booleans, null, and every string that is neither secret-looking, a declared
 * blob, nor over-long. The corpus's whole value is that its structure is real;
 * a walker that normalized ids or paths would destroy the joins the converters
 * are tested on.
 *
 * Node built-ins only (this file is loaded by `capture.mjs`, which runs under
 * bare `node` with no build step).
 */

/** The replacement every redacted value collapses to. */
export const REDACTED = "REDACTED";

/** Strings longer than this are truncated by the general length rule. */
export const MAX_STRING_CHARS = 900;

/** How much of an over-long string survives truncation. */
export const TRUNCATE_TO_CHARS = 400;

/** How much of a declared opaque blob survives truncation. */
export const BLOB_PREFIX_CHARS = 64;

/**
 * Object keys whose STRING value is a credential regardless of its shape.
 *
 * Matched case-insensitively against the key with separators ignored, so
 * `api-key`, `api_key` and `apiKey` are one rule rather than three.
 */
export const SECRET_KEY_NAMES = [
  "authorization",
  "apikey",
  "xapikey",
  "password",
  "passwd",
  "secret",
  "clientsecret",
  "accesstoken",
  "refreshtoken",
  "sessiontoken",
  "privatekey",
  "credential",
  "credentials",
];

/**
 * Value patterns that are credentials wherever they appear, whatever the key.
 *
 * These exist because a secret does not only arrive under a helpfully named
 * key — it turns up inside a bash command line, an env dump, a tool result's
 * prose, or a hook's stderr. Each pattern is anchored on a vendor-specific
 * prefix so it cannot fire on an ordinary identifier: a uuid, a vendor session
 * id and a `toolu_` id all fail every one of them.
 */
export const SECRET_VALUE_PATTERNS = [
  // Anthropic API keys.
  /sk-ant-[A-Za-z0-9_-]{16,}/g,
  // OpenAI-style keys (harness machines carry them too).
  /\bsk-[A-Za-z0-9]{32,}\b/g,
  // GitHub personal-access / OAuth / app tokens.
  /\bgh[pousr]_[A-Za-z0-9]{20,}\b/g,
  /\bgithub_pat_[A-Za-z0-9_]{20,}\b/g,
  // AWS access key ids and session tokens.
  /\bAKIA[0-9A-Z]{16}\b/g,
  /\bASIA[0-9A-Z]{16}\b/g,
  // Slack bot/user/app tokens.
  /\bxox[abprs]-[A-Za-z0-9-]{10,}\b/g,
  // Bearer credentials in a header line.
  /\bBearer\s+[A-Za-z0-9._~+/=-]{16,}/g,
  // JSON Web Tokens.
  /\beyJ[A-Za-z0-9_-]{8,}\.[A-Za-z0-9_-]{8,}\.[A-Za-z0-9_-]{8,}\b/g,
];

/**
 * Keys whose value is an opaque blob: meaningless to a reader, enormous, and
 * carrying no structure a converter joins on.
 *
 * `signature` is the thinking block's cryptographic signature; `data` is a
 * base64 image payload — the two the MANIFEST names. Both are truncated rather
 * than dropped, because their PRESENCE and type are part of the shape under
 * test even when their bytes are not.
 */
export const BLOB_KEYS = ["signature", "data", "base64", "thumbnail"];

/** Normalize a key for the secret/blob key rules (case and separators). */
function normalizeKey(key) {
  return String(key).toLowerCase().replace(/[^a-z0-9]/g, "");
}

/** Whether this object key names a credential. */
export function isSecretKey(key) {
  return SECRET_KEY_NAMES.includes(normalizeKey(key));
}

/** Whether this object key names an opaque blob. */
export function isBlobKey(key) {
  return BLOB_KEYS.includes(normalizeKey(key));
}

/**
 * Replace every credential-shaped run inside a string.
 *
 * Returns the string unchanged when nothing matched, so the caller can tell a
 * redaction happened without a second scan.
 */
export function redactSecretRuns(text) {
  let out = text;
  for (const pattern of SECRET_VALUE_PATTERNS) {
    // The patterns are module-level and /g, so reset lastIndex per use.
    pattern.lastIndex = 0;
    out = out.replace(pattern, REDACTED);
  }
  return out;
}

/** Truncate a declared opaque blob to a recognizable prefix. */
export function truncateBlob(text) {
  if (text.length <= BLOB_PREFIX_CHARS) return text;
  const dropped = text.length - BLOB_PREFIX_CHARS;
  return `${text.slice(0, BLOB_PREFIX_CHARS)}…[TRUNCATED ${dropped} chars]`;
}

/** Truncate an over-long ordinary string. */
export function truncateLong(text) {
  if (text.length <= MAX_STRING_CHARS) return text;
  const dropped = text.length - TRUNCATE_TO_CHARS;
  return `${text.slice(0, TRUNCATE_TO_CHARS)}…[TRUNCATED ${dropped} chars for corpus]`;
}

/**
 * Apply the three string rules in their settled order.
 *
 * `key` is the object key the string was found under, or `undefined` for a
 * bare array element or a root string — the key-driven rules simply do not
 * apply there, and the value-pattern and length rules still do.
 */
export function anonymizeString(text, key) {
  if (key !== undefined && isSecretKey(key)) return REDACTED;
  const redacted = redactSecretRuns(text);
  if (redacted !== text) return redacted;
  if (key !== undefined && isBlobKey(key)) return truncateBlob(text);
  return truncateLong(text);
}

/**
 * Walk any JSON value, returning an anonymized COPY.
 *
 * The input is never mutated: a capture run keeps the raw stream on disk only
 * long enough to anonymize it, and an in-place walker makes "did this file
 * already get processed" unanswerable.
 */
export function anonymize(value, key) {
  if (typeof value === "string") return anonymizeString(value, key);
  if (Array.isArray(value)) return value.map((item) => anonymize(item, key));
  if (value !== null && typeof value === "object") {
    const out = {};
    for (const [childKey, childValue] of Object.entries(value)) {
      out[childKey] = anonymize(childValue, childKey);
    }
    return out;
  }
  // Numbers, booleans, null, undefined: structural, kept verbatim.
  return value;
}

/**
 * Anonymize a JSONL document line by line.
 *
 * A line that does not parse as JSON is NOT dropped and NOT silently passed
 * through: it is anonymized as plain text and reported through `onUnparsed` so
 * the capture's report can name it. The vendor writes real JSONL, so an
 * unparsed line means either a partially flushed tail or a shape we have never
 * seen — both are findings, not noise.
 */
export function anonymizeJsonl(text, onUnparsed = () => {}) {
  const lines = text.split("\n");
  const out = [];
  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i];
    if (line === "") {
      out.push(line);
      continue;
    }
    let parsed;
    try {
      parsed = JSON.parse(line);
    } catch (err) {
      onUnparsed(i + 1, err);
      out.push(anonymizePlainText(line));
      continue;
    }
    out.push(JSON.stringify(anonymize(parsed)));
  }
  return out.join("\n");
}

/**
 * Anonymize a NON-JSON artifact (a shell spool, a hook's stderr capture).
 *
 * Only the credential rules apply. The length rules deliberately do NOT: a
 * spool's value to the sidecar tests is that it is a real byte stream ending
 * (or not) in its `EXIT=<code>` marker, and a walker that truncated it would
 * manufacture the very "caught mid-write" case the corpus keeps as a separate,
 * deliberately hand-truncated fixture.
 */
export function anonymizePlainText(text) {
  return redactSecretRuns(text);
}
