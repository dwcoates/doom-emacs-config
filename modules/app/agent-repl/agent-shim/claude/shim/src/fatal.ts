/**
 * The entrypoint's two log emitters, deliberately kept OFF the wiring graph.
 *
 * `reportFatal` is the reporter of last resort: it has to work at the very
 * first line of startup, before argv is parsed, before the log is configured,
 * and before a single engine, store or service module has been resolved. A
 * reporter that lived in `main.ts` could only be reached by pulling that whole
 * graph in, which makes the bootstrap path — and the tests that exercise it —
 * depend on modules the failure it reports may be about. `log.ts` is the only
 * thing it needs, so `log.ts` is the only thing it imports.
 *
 * `main.ts` re-exports both, so the entrypoint's public surface is unchanged.
 */
import { bindLog, emergencyStderr } from "./log.js";

/** Stable operation labels for entrypoint telemetry and tests. */
export const MAIN_LIFECYCLE_OPERATION = "shim.main.lifecycle";
export const MAIN_FATAL_OPERATION = "shim.main.fatal";

const LIFECYCLE_LOGGER = bindLog({ component: "shim-main", operation: MAIN_LIFECYCLE_OPERATION });
const FATAL_LOGGER = bindLog({ component: "shim-main", operation: MAIN_FATAL_OPERATION });

/** Emit a lifecycle record at info unless the caller identifies an error. */
export function logMainLifecycle(fields: Record<string, unknown>, message: string): void {
  LIFECYCLE_LOGGER.log({ level: "info", ...fields }, message);
}

function fatalCause(err: unknown): string {
  if (err instanceof Error) return err.name.length === 0 ? "Error" : err.name;
  return typeof err;
}

/** Log an unrecoverable entrypoint failure before ending the process. */
export function reportFatal(err: unknown): void {
  const message = `fatal: ${err instanceof Error ? (err.stack ?? err.message) : String(err)}`;
  try {
    FATAL_LOGGER.log(
      {
        level: "error",
        cause: err,
        cause_class: "unrecoverable_entrypoint_failure",
        cause_type: fatalCause(err),
        exit_outcome: "process_exit_1",
      },
      message,
    );
  } catch (logErr) {
    // Reached only before the logger is configured, or when its sink failed.
    emergencyStderr(
      `${message}; logger failure: ${logErr instanceof Error ? logErr.message : String(logErr)}`,
    );
  }
}
