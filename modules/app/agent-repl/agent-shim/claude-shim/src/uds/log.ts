/**
 * Loud, structured logging for the shim UDS transport.
 *
 * The design's instrumentation contract (§12) mandates a shared prefix
 * `HH:MM:SS.mmm [component] {session=… ws=…} message` and "loud on every
 * anomaly, never silently swallow". Everything here goes to stderr, which is
 * free-form log output (stdout/UDS carry only protocol frames).
 */

/** Structured key/value fields rendered as `{k=v k=v}`. Empty → omitted. */
export type LogFields = Record<string, string | number | bigint | boolean>;

function stamp(): string {
  // HH:MM:SS.mmm slice of the ISO string (UTC; parity with the Go services'
  // millisecond stamps, which is all the contract asks for).
  return new Date().toISOString().slice(11, 23);
}

function renderFields(fields: LogFields | undefined): string {
  if (!fields) return "";
  const parts = Object.entries(fields).map(([k, v]) => `${k}=${String(v)}`);
  return parts.length > 0 ? ` {${parts.join(" ")}}` : "";
}

/** Emit one loud log line for `component`, tagged with structured fields. */
export function shimLog(component: string, fields: LogFields, message: string): void {
  process.stderr.write(`${stamp()} [${component}]${renderFields(fields)} ${message}\n`);
}
