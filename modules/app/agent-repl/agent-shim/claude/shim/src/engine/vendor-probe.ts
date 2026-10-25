/**
 * engine/vendor-probe.ts — one PULLED vendor fact, and the health of reading it.
 *
 * A session reads several facts from the live query on its own schedule
 * (context usage, the model catalog, mcp health, the slash-command catalog).
 * Each read shares one contract, stated once here:
 *
 * - NO QUERY, NO READ. Before StartSession and after the query is gone there
 *   is nothing to ask, and nothing is stated.
 * - A READ THAT ANSWERS IS THE RECOVERY of its own component: whatever fault
 *   the last refusal raised there is lifted, because the same operation
 *   succeeding proves that refusal was a moment, not a state.
 * - A READ THAT FAILS IS A `SessionFault` on its own component, kind
 *   `vendorQueryFailed`, whose detail names the call. It NEVER rejects: a
 *   probe that cannot answer does not take its caller down with it, and
 *   nothing is swallowed, because the fault is the record.
 * - HANDING THE ANSWER ON IS PART OF THE READ: a conversion that throws is
 *   the read failing, stated the same way.
 * - A READ THE SESSION CUT SHORT IS NOT A FAULT. When the session closed the
 *   very query the read was waiting on (a stand-down, a replaced query, a
 *   failed start), the vendor's "Query closed before response received" is
 *   the session's own act: it is recorded at INFO, and no fault is raised for
 *   a vendor that did nothing wrong (2026-10-25: every KillSession that landed
 *   while a turn-end context reading was in flight opened a fault).
 */
import { bindLog } from "../log.js";
import { sessionFault } from "../service/failures.js";
import type { QueryLike } from "../sdk/types.js";
import type { SessionPushes } from "./pushes.js";

const LOGGER = bindLog({ component: "shim-engine-vendor-probe", operation: "shim.engine.vendor_probe" });

/** Where a probe states its health: the session's fault table. */
export type ProbeHealth = Pick<SessionPushes, "resolveComponent" | "fault">;

/** One pulled fact: which component answers for it, the call, and its answer's use. */
export interface VendorProbe<T> {
  /** The fault component this read raises and lifts. */
  readonly component: string;
  /** The SDK call's name, as the fault's detail spells it. */
  readonly call: string;
  /** Ask the live query. */
  readonly read: (active: QueryLike) => Promise<T>;
  /** Hand the answer on (push it, keep it). */
  readonly take: (answer: T) => void;
}

/**
 * Read one vendor fact from ACTIVE, stating the read's health on HEALTH.
 * CLOSED_BY_SESSION answers whether the session itself closed a query.
 */
export async function probeVendorFact<T>(
  health: ProbeHealth,
  active: QueryLike | undefined,
  probe: VendorProbe<T>,
  closedBySession: (query: QueryLike) => boolean,
): Promise<void> {
  if (active === undefined) return;
  try {
    probe.take(await probe.read(active));
    health.resolveComponent(probe.component, 0);
  } catch (err) {
    if (closedBySession(active)) {
      LOGGER.info(
        { component_read: probe.component, call: probe.call, cause: err instanceof Error ? err.message : String(err) },
        "a vendor read was cut short by the session closing its own query; no fault is raised",
      );
      return;
    }
    health.fault(
      sessionFault(
        { kind: "vendorQueryFailed" },
        probe.component,
        `${probe.call} failed: ${err instanceof Error ? err.message : String(err)}`,
      ),
    );
  }
}
