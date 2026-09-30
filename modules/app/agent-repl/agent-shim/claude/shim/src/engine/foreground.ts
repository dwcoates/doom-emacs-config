/**
 * engine/foreground.ts — what the turn has in flight, by tool call.
 *
 * # Why the engine keeps this at all
 *
 * `DetachForeground` has FOUR distinct answers and the daemon acts on each
 * differently: the unit was never a unit (`unknown_unit`), it already finished
 * (`already_concluded`), its KIND cannot detach at all (`not_detachable`), or it
 * is perfectly detachable in kind but the vendor tracks no foreground task for
 * it, so `backgroundTasks` moved nothing (`not_in_foreground`). Without a table
 * of the calls in flight the engine can only tell "the vendor holds background
 * work for this id" from "it does not", so three of those four answers collapse
 * onto `unknown_unit` — which tells a consumer to stop offering an affordance
 * for work that backgrounds itself routinely.
 *
 * The live-work table cannot answer it: that one holds DETACHED work, which is
 * precisely what a foreground unit is not yet.
 *
 * # Bounded on purpose
 *
 * Only calls with no result yet are retained. A settled call is remembered just
 * long enough to answer `already_concluded` for it, under a cap — the store is
 * the history of the session, and this must never become a second one.
 */
import { bindLog } from "../log.js";

const LOGGER = bindLog({ component: "shim-engine-foreground", operation: "shim.engine.foreground" });

/**
 * How many settled calls are remembered, newest first.
 *
 * A consumer presses Ctrl-B on something it can still see, so the answer only
 * has to outlive the frame on screen; the bound is what keeps this table's size
 * a function of the UI's recency and not of the conversation's length.
 */
export const SETTLED_MEMORY = 256;

/**
 * The activity kinds that are detachable IN KIND.
 *
 * The four detachable kinds are subagent, bash, workflow and monitor. Anything
 * else — a read, an edit, the turn's own response — cannot be backgrounded at
 * all, which is a different refusal from "this one cannot be backgrounded right
 * now". Named by the `AgentActivity.item` arm, which is the vocabulary the
 * consumer addressed the unit in.
 */
const DETACHABLE_KINDS = new Set(["subagent", "bash", "workflow", "monitor"]);

/**
 * The kinds that are not tool calls at all.
 *
 * The agent's own prose and its reasoning are UNITS a consumer can point at,
 * and they are the only ones for which `already_concluded` is meaningless: they
 * settle per BLOCK, several times in one live turn, so "it already finished"
 * would be said of a turn that is still running. They are always
 * `not_detachable`, which is the fact about them that does not change.
 */
const NEVER_A_CALL = new Set(["response", "thinking"]);

/** What the table can say about one addressed unit. */
type ForegroundVerdict =
  | { readonly kind: "unknown" }
  | { readonly kind: "settled" }
  | { readonly kind: "live_detachable" }
  | { readonly kind: "not_detachable" };

interface Unit {
  readonly activityId: string;
  /** The `AgentActivity.item` arm: `bash`, `subagent`, `read`, `response`, … */
  readonly kind: string;
}

/** Every unit this session has seen, at the state it has reached. */
export class ForegroundUnitTable {
  private readonly inFlight = new Map<string, Unit>();
  private readonly settled = new Map<string, Unit>();
  /** Settled ids, oldest first, so the oldest is the one evicted. */
  private readonly order: string[] = [];

  /**
   * A unit reached a state.
   *
   * Fed from the FOLD's own frames rather than from the raw SDK blocks: the
   * consumer addresses a unit by the `AgentActivityId` it was shown, so the
   * table has to be keyed by exactly that, and the `item` arm is the kind in
   * the same vocabulary. `settled` is `false` while the unit is still running.
   */
  note(activityId: string, kind: string, settled: boolean): void {
    if (activityId === "") return;
    if (!settled) {
      this.inFlight.set(activityId, { activityId, kind });
      LOGGER.logVerbose({ activity_id: activityId, kind }, "a foreground unit is in flight");
      return;
    }
    this.inFlight.delete(activityId);
    if (this.settled.has(activityId)) return;
    this.settled.set(activityId, { activityId, kind });
    this.order.push(activityId);
    while (this.order.length > SETTLED_MEMORY) {
      const evicted = this.order.shift();
      if (evicted !== undefined) this.settled.delete(evicted);
    }
  }

  /**
   * What this id is, as far as the foreground is concerned.
   *
   * A CALL THAT FINISHED is `already_concluded` whatever its kind: that is the
   * fact the consumer needs, and it is what stops them retrying. A call still
   * running is judged on its KIND instead -- "nothing of this kind ever
   * detaches" tells them to stop offering the affordance. The agent's own prose
   * and reasoning are neither: they settle per block inside a live turn, so
   * they are always `not_detachable` and never `already_concluded`.
   */
  verdict(activityId: string): ForegroundVerdict {
    const unit = this.inFlight.get(activityId) ?? this.settled.get(activityId);
    if (unit === undefined) return { kind: "unknown" };
    if (NEVER_A_CALL.has(unit.kind)) return { kind: "not_detachable" };
    if (!this.inFlight.has(activityId)) return { kind: "settled" };
    return DETACHABLE_KINDS.has(unit.kind)
      ? { kind: "live_detachable" }
      : { kind: "not_detachable" };
  }

  /** How many units are in flight, for the table's own tests. */
  get inFlightCount(): number {
    return this.inFlight.size;
  }

  /** Nothing is in flight any more. */
  clear(): void {
    this.inFlight.clear();
  }
}
