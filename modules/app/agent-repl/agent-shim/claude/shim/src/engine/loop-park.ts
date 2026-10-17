/**
 * Where a vendor message loop is parked, for the record a stand-down writes
 * when the loop outlives its budget after the query was closed.
 *
 * A LOOP THAT NEVER ENDS IS ONE OF THREE WAITS, and each is remediated
 * differently: the vendor's iterator never finished (`vendor`), the shim's own
 * handling of a message never returned (`handling`), or the store writer's
 * backpressure never released the loop (`backpressure`). The overrun record
 * names which, the message involved, and how long it had been parked there.
 */
export interface LoopPark {
  /** The wait the loop is in. */
  at: "vendor" | "handling" | "backpressure";
  /**
   * The vendor's name for the message being handled, or the last one handled
   * while waiting on the vendor; empty before the first.
   */
  message: string;
  /** When the loop entered this wait, in the engine clock's milliseconds. */
  sinceMs: number;
}

/** The overrun record's fields for a park; none when no loop was running. */
export function loopParkFields(park: LoopPark | undefined, nowMs: number): Record<string, unknown> {
  if (park === undefined) return {};
  return { parked_at: park.at, parked_message: park.message, parked_ms: nowMs - park.sinceMs };
}
