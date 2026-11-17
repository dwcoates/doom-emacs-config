/**
 * WARN AND ERROR RECORDS LIVE IN THEIR OWN FILE.
 *
 * Every agent-repl runtime writes its warn and error records ONLY to a
 * warnings file beside its log, and its debug and info records only to the
 * log, so a reader of warnings opens the small warnings files alone.
 * `proto/vocab/log-warnings-sink.json` is the contract the Go
 * (`agent-shim/logging/go/warnings.go`), TypeScript and elisp copies each
 * assert.
 */

/** The four record levels. */
export type RecordLevel = "debug" | "info" | "warn" | "error";

/** Whether a record at LEVEL belongs in the warnings file. */
export function isWarningsLevel(level: RecordLevel): boolean {
  switch (level) {
    case "warn":
    case "error":
      return true;
    case "debug":
    case "info":
      return false;
    default:
      throw new Error(`logging: invalid level ${JSON.stringify(level)}`);
  }
}

/** The warnings file of the log at PATH: '.warn' before a final '.log', or '.warn.log' appended. */
export function warningsPath(path: string): string {
  return path.endsWith(".log") ? `${path.slice(0, -".log".length)}.warn.log` : `${path}.warn.log`;
}

/** The warnings twin of the per-workspace sink NAME (a canonical link NAME.warn.log). */
export function warningsSink(name: string): string {
  return `${name}.warn`;
}
