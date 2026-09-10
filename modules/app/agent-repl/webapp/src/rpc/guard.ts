/**
 * guardMalformed — the one place a fire-and-forget click handler's
 * `MalformedView` stops.
 *
 * EVERY CLICK IS AN RPC, and a click handler cannot be awaited: the DOM
 * listener returns before the answer arrives, so the submission runs as a
 * detached promise. `callUnary` and the arm walks below it raise
 * `MalformedView` for an answer this build cannot read — correct, and exactly
 * what must NOT be defaulted away — but a rejection nothing awaits is an
 * unhandled rejection: the process-level report the runtime prints, the test
 * runner fails on, and no user ever sees.
 *
 * So the OWNING LAYER catches it here. An unreadable ANSWER is the same
 * condition an unreadable PUSH is (`src/rpc/streams.ts` files the identical
 * card), so it is logged once at error with the message tree path that refused,
 * and filed as `frame_undecodable` — the user is told conversation may be
 * missing rather than left with a control that silently did nothing.
 *
 * WHAT IT DOES NOT CATCH. Anything that is not a `MalformedView` travels on
 * untouched: a transport failure belongs to the call site (which renders it at
 * the clicked control), and a programming error is not this layer's to
 * interpret. Swallowing either here would hide a fault behind a card that names
 * the wrong thing.
 */
import { log } from "../log.js";
import { frameUndecodable, type FailureSink } from "../failure/sink.js";
import { isMalformedView } from "./malformed.js";

/** The slice of the context this guard needs; the whole AppContext fits. */
export interface GuardContext {
  readonly failures: FailureSink;
}

/**
 * Run WORK to completion, absorbing a `MalformedView` as a reported failure.
 *
 * OPERATION is the owning layer's own operation name ("composer.submit") and
 * rides the log record, so two call sites filing the same card are still told
 * apart in the log. Answers whether the work refused as malformed.
 */
export async function guardMalformed(
  ctx: GuardContext,
  operation: string,
  work: Promise<unknown>,
): Promise<boolean> {
  try {
    await work;
    return false;
  } catch (err) {
    if (!isMalformedView(err)) throw err;
    log.error(`the daemon's answer could not be read: ${err.message}`, {
      operation: `${operation}-undecodable`,
      context: { path: err.path, cause: err.detail },
    });
    ctx.failures.report(frameUndecodable(err.detail, err.path));
    return true;
  }
}
