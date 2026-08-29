/**
 * The webapp's diagnostic-log record shape.
 *
 * WHAT THIS FILE USED TO BE: the whole hand-written webapp<->daemon data
 * vocabulary -- the conversation-item model's shared data types and the
 * webapp->daemon command union. All of it went with the port to generated
 * Connect clients, which own the wire types now.
 *
 * WHY THE TWO TYPES BELOW REMAIN: `wslog.ts` (and `clientlog-throttle.ts`)
 * pass them around, and those two modules are being replaced wholesale by the
 * rpc core's `src/log.ts` in a concurrent change. Rather than edit a file
 * another agent is renaming out from under this one, the two types stay here
 * and this file shrinks to nothing else. It is a SEAM, not a design: once
 * `src/log.ts` lands and carries the level and context types itself, this file
 * has no callers and is deleted.
 */

/**
 * A webapp-side diagnostic line: ALWAYS written to the local console, and
 * mirrored into the daemon's log -- over the `ClientLog` rpc after the port.
 * This is the in-memory shape the ForwardingLogger passes around.
 */
export interface ClientLogCmd {
  type: "client-log";
  level: "info" | "warn" | "error";
  message: string;
  /**
   * Optional structured payload (ids, counters, timings) accompanying the
   * message, encoded onto the request's `context` (a `google.protobuf.Struct`).
   *
   * Schemaless on purpose, matching the proto: the shape is the reporting call
   * site's business, so adding a diagnostic never becomes a proto change. Call
   * sites that HAVE structured facts pass them here instead of only string-
   * interpolating them into `message`, so the daemon's log carries the ids in a
   * form something can read back.
   */
  context?: ClientLogContext;
}

/** A ClientLogCmd's structured payload: JSON values, as a Struct carries. */
export type ClientLogContext = Record<string, unknown>;
