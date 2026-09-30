/**
 * engine/pushes.ts — the WatchSession fan-out.
 *
 * RESPONSIBILITY. One standing stream per consumer, fed by every session-level
 * fact: the vendor's own (`identity_rotated`, `query_died`, `model_changed`,
 * `permission_mode_changed`, `fast_mode`, `mcp_server`, `account_usage`,
 * `context_budget_warning`, `compacting`) and the shim's own about itself
 * (`diagnostics`, `context_usage`, `network_resume_waits`,
 * `network_resume_outcome`).
 *
 * THE FIRST PUSH IS READINESS, AND IT IS SYNCHRONOUS WITH THE OPEN. After
 * `StartSession`, the first frame every WatchSession receives is `diagnostics` —
 * that push IS the daemon's readiness signal, and there is no other. It is
 * seeded into the subscriber's queue BEFORE the iterable is returned, because
 * connect-go surfaces a server-stream refusal only at the first Receive: a
 * consumer that blocks waiting for a first frame cannot tell "not ready" from
 * "refused", and a silent WatchSession stalls the daemon's whole bring-up.
 *
 * THEN THE CURRENT VIEW, THEN CHANGES ONLY. A joining subscriber also gets the
 * current `context_usage`, `model_changed`, `permission_mode_changed`,
 * `fast_mode`, `account_usage`, `title` and `network_resume_waits` — a
 * consumer that attached late is not entitled to a blank session — and after
 * that, an arm is pushed only when its value actually CHANGED. A periodic push
 * of an unchanged view is indistinguishable from a change at the consumer and
 * defeats every "on change" optimization above it; the client ticks locally
 * from the instants already shipped.
 *
 * SYNTHESIZED FACTS ARE NEVER WRITTEN. `diagnostics` and `context_usage` are
 * the shim's report about ITSELF, not vendor conversation, so they are pushed
 * and never landed in the store.
 */
import { create, toBinary } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";

const LOGGER = bindLog({ component: "shim-engine-pushes", operation: "shim.engine.pushes" });

/**
 * How many undelivered facts one subscriber may hold.
 *
 * Session facts are rare — a mode change, a usage sample, an mcp health flip —
 * so a subscriber this far behind is not slow, it is gone. The overflow is
 * reported as a degraded window rather than silently dropped, because a
 * consumer that missed a `query_died` and was never told it missed one is worse
 * off than one that saw the gap.
 */
export const SUBSCRIBER_QUEUE_LIMIT = 256;

/** Which arm an update carries — the key "on change only" is computed per. */
function armOf(update: conversationv1.SessionUpdate): string {
  return update.update.case ?? "";
}

function sameUpdate(a: conversationv1.SessionUpdate, b: conversationv1.SessionUpdate): boolean {
  const left = toBinary(conversationv1.SessionUpdateSchema, a);
  const right = toBinary(conversationv1.SessionUpdateSchema, b);
  if (left.length !== right.length) return false;
  return left.every((byte, index) => byte === right[index]);
}

class Subscriber {
  private readonly queue: conversationv1.SessionUpdate[] = [];
  private waiting: ((value: IteratorResult<conversationv1.SessionUpdate>) => void) | undefined;
  private closed = false;

  offer(update: conversationv1.SessionUpdate): boolean {
    if (this.closed) return false;
    if (this.waiting !== undefined) {
      const resolve = this.waiting;
      this.waiting = undefined;
      resolve({ value: update, done: false });
      return true;
    }
    if (this.queue.length >= SUBSCRIBER_QUEUE_LIMIT) return false;
    this.queue.push(update);
    return true;
  }

  close(): void {
    this.closed = true;
    if (this.waiting !== undefined) {
      const resolve = this.waiting;
      this.waiting = undefined;
      resolve({ value: undefined, done: true });
    }
  }

  iterator(): AsyncIterator<conversationv1.SessionUpdate> {
    return {
      next: (): Promise<IteratorResult<conversationv1.SessionUpdate>> => {
        const next = this.queue.shift();
        if (next !== undefined) return Promise.resolve({ value: next, done: false });
        if (this.closed) return Promise.resolve({ value: undefined, done: true });
        return new Promise((resolve) => {
          this.waiting = resolve;
        });
      },
      return: (): Promise<IteratorResult<conversationv1.SessionUpdate>> => {
        this.close();
        return Promise.resolve({ value: undefined, done: true });
      },
    };
  }
}

/** The fan-out, and the session's own diagnostic memory. */
export class SessionPushes {
  private readonly subscribers = new Set<Subscriber>();
  private readonly faults: conversationv1.SessionFault[] = [];
  /** How many times each standing (component, kind) fault has recurred, for the log record only. */
  private readonly faultRepeats = new Map<string, number>();
  private readonly degradedWindows: conversationv1.SessionDegradedWindow[] = [];
  /** The current value of each replayed arm, so a late subscriber is not blind. */
  private readonly current = new Map<string, conversationv1.SessionUpdate>();
  private standingDown = false;

  /**
   * `shimBuildSha` is REQUIRED on `SessionDiagnostics.shim_build` (every
   * frame, including the opening one of a session-less WatchSession), so it
   * is required here too rather than defaulted: `main.ts` refuses to start
   * without `SHIM_BUILD_SHA`, so production never reaches this with an empty
   * string, and a caller that does is a construction-site defect, loud rather
   * than a silently empty required field on the wire.
   */
  constructor(
    private readonly nowMs: () => number = () => Date.now(),
    private readonly shimBuildSha: string,
  ) {
    if (this.shimBuildSha === "") {
      throw new Error("SessionPushes: shimBuildSha is required and must not be empty");
    }
  }

  /** The arms a joining subscriber is caught up on, in the order it gets them. */
  //
  // `fastMode` is replayed for the same reason the model is: it is a LEVEL the
  // session is in, not an event, and a consumer that attached after the vendor
  // last stated it would otherwise draw the toggle from nothing.
  //
  // `accountUsage` is the same kind of level, and it was the one missing here.
  // The session PROBES it once at StartSession (session.ts, the `void
  // pushAccountUsage()` beside the keepalive cadence), while the daemon opens
  // its standing WatchSession only AFTER StartSession has answered — so that
  // first sample was fanned out to nobody and the footer held no allowance
  // figure at all until a turn closed and reprobed. MEASURED in a headless run
  // of the real editor: the sample went out at 37.494 to the bring-up's stream,
  // the daemon's own watch opened at 37.503, and the footer's first sighting of
  // any account usage was the turn-close reprobe 41ms later. The dedup above
  // suppresses nothing real: every sample carries its own `observed_at_ms`, so
  // two samples are never byte-identical.
  //
  // `title` is a level too, and the most consequential one to miss: the vendor
  // states it on the FILE plane and the shim reads it at the session's start,
  // which is BEFORE the daemon's standing WatchSession exists — so without a
  // replay the topbar would draw the workspace name until some later turn
  // happened to change the title.
  //
  // `networkResumeWaits` is a STANDING SET whose producer obligation is to be
  // stated on every open before any live frame (session.proto): a consumer
  // that (re)connects mid-outage learns the waits from its own stream. Like
  // every level here it is replayed once it has been stated, so a session
  // that never waited states nothing, and one whose last wait ended replays
  // the empty set it stated then.
  private static readonly REPLAYED = [
    "contextUsage",
    "modelChanged",
    "permissionModeChanged",
    "fastMode",
    "accountUsage",
    "title",
    "networkResumeWaits",
  ];

  /**
   * Open one standing stream.
   *
   * The diagnostics frame is queued HERE, synchronously, so the returned
   * iterable's first `next()` resolves without waiting on anything.
   */
  subscribe(): AsyncIterable<conversationv1.SessionUpdate> {
    const subscriber = new Subscriber();
    subscriber.offer(this.diagnostics());
    for (const arm of SessionPushes.REPLAYED) {
      const update = this.current.get(arm);
      if (update !== undefined) subscriber.offer(update);
    }
    this.subscribers.add(subscriber);
    LOGGER.debug({ subscribers: this.subscribers.size }, "opened a WatchSession stream; diagnostics pushed first");
    if (this.standingDown) subscriber.close();
    return {
      // An ARROW, not a method: the teardown below has to reach this table's
      // subscriber set, and a method's own `this` is the returned literal.
      [Symbol.asyncIterator]: (): AsyncIterator<conversationv1.SessionUpdate> => {
        const iterator = subscriber.iterator();
        return {
          next: () => iterator.next(),
          return: async () => {
            this.subscribers.delete(subscriber);
            LOGGER.debug({ subscribers: this.subscribers.size }, "a WatchSession consumer went away");
            return iterator.return === undefined
              ? { value: undefined, done: true as const }
              : iterator.return();
          },
        };
      },
    };
  }

  /** How many consumers are attached. */
  get subscriberCount(): number {
    return this.subscribers.size;
  }

  /**
   * Push a fact.
   *
   * ON CHANGE ONLY for the replayed arms: an identical value is dropped without
   * reaching anyone. Event arms (`identity_rotated`, `query_died`,
   * `compacting`, `account_usage`, `context_budget_warning`) always go out —
   * two identical rotations are two rotations.
   */
  push(update: conversationv1.SessionUpdate): boolean {
    const arm = armOf(update);
    if (SessionPushes.REPLAYED.includes(arm) || arm === "diagnostics") {
      const previous = this.current.get(arm);
      if (previous !== undefined && sameUpdate(previous, update)) {
        LOGGER.logVerbose({ arm }, "dropped an unchanged session fact");
        return false;
      }
    }
    if (SessionPushes.REPLAYED.includes(arm) || arm === "diagnostics") {
      this.current.set(arm, update);
    }
    this.fanOut(update, arm);
    return true;
  }

  private fanOut(update: conversationv1.SessionUpdate, arm: string): void {
    for (const subscriber of this.subscribers) {
      if (subscriber.offer(update)) continue;
      // A subscriber that cannot take a session fact is REPORTED, never
      // silently skipped: the consumer needs to know its view has a hole.
      // warn: a defect because a full subscriber queue lost a session fact.
      LOGGER.warn(
        { arm, queue_limit: SUBSCRIBER_QUEUE_LIMIT },
        "a WatchSession consumer's queue is full; the fact could not be delivered",
      );
      this.openDegradedWindow("shim-engine-pushes", `a WatchSession consumer could not take a ${arm} fact`);
    }
    LOGGER.logVerbose({ arm, subscribers: this.subscribers.size }, "pushed a session fact");
  }

  // -- diagnostics ----------------------------------------------------------

  /** The current verdict, with every fault and degraded window kept since start. */
  diagnostics(): conversationv1.SessionUpdate {
    return create(conversationv1.SessionUpdateSchema, {
      update: {
        case: "diagnostics",
        value: create(conversationv1.SessionDiagnosticsSchema, {
          degradedWindows: [...this.degradedWindows],
          shimBuild: this.shimBuildSha,
          health:
            this.faults.length === 0
              ? { case: "healthy", value: create(conversationv1.SessionHealthySchema, {}) }
              : {
                  case: "unhealthy",
                  value: create(conversationv1.SessionUnhealthySchema, { faults: [...this.faults] }),
                },
        }),
      },
    });
  }

  /**
   * Record a fault and restate the diagnostics.
   *
   * A fault whose (component, kind) matches a STANDING fault REPLACES it —
   * latest detail wins — rather than stacking a new entry beside it. A store
   * down for an hour with a watch every few minutes would otherwise pile up
   * identical faults until recovery clears them, and the diagnostics arm
   * would report a growing list of one repeated symptom rather than one
   * symptom that has recurred. The `repeats` count lives only on this
   * instance, for the log record — it is not carried into the SessionFault
   * message itself.
   */
  fault(fault: conversationv1.SessionFault): void {
    const kind = fault.kind.case ?? "";
    const key = `${fault.component} ${kind}`;
    const index = this.faults.findIndex(
      (standing) => standing.component === fault.component && (standing.kind.case ?? "") === kind,
    );
    const repeats = (this.faultRepeats.get(key) ?? 0) + 1;
    this.faultRepeats.set(key, repeats);
    if (index === -1) {
      this.faults.push(fault);
      LOGGER.error({ component: fault.component, kind, detail: fault.detail }, "recorded a session fault");
    } else {
      this.faults[index] = fault;
      LOGGER.debug({ component: fault.component, kind, detail: fault.detail, repeats }, "recorded a session fault");
    }
    this.push(this.diagnostics());
  }

  /** Record an OPEN degraded window and restate the diagnostics. */
  openDegradedWindow(component: string, reason: string): conversationv1.SessionDegradedWindow {
    const window = create(conversationv1.SessionDegradedWindowSchema, {
      component,
      reason,
      beganAtMs: BigInt(this.nowMs()),
      extent: { case: "open", value: create(conversationv1.SessionDegradedOpenSchema, {}) },
    });
    this.degradedWindows.push(window);
    return window;
  }

  /**
   * A COMPONENT RECOVERED: drop its standing faults and close its open windows.
   *
   * Recovery is a real fact, not the absence of one: a shim that only ever
   * appended faults would report a transient converter defect as a permanent
   * unhealthy session forever. The WINDOW survives the recovery (closed, with
   * what was lost) because a consumer that joined afterwards still needs to
   * know there was a hole.
   *
   * Answers whether anything actually changed, so a caller does not restate an
   * unchanged verdict.
   */
  resolveComponent(component: string, droppedCount: number): boolean {
    let changed = false;
    for (let index = this.faults.length - 1; index >= 0; index -= 1) {
      const standing = this.faults[index];
      if (standing?.component !== component) continue;
      this.faults.splice(index, 1);
      this.faultRepeats.delete(`${standing.component} ${standing.kind.case ?? ""}`);
      changed = true;
    }
    for (const window of this.degradedWindows) {
      if (window.component !== component || window.extent.case !== "open") continue;
      window.extent = {
        case: "closed",
        value: create(conversationv1.SessionDegradedClosedSchema, {
          endedAtMs: BigInt(this.nowMs()),
          droppedCount: BigInt(droppedCount),
        }),
      };
      changed = true;
    }
    if (!changed) return false;
    // INFO, NOT DEBUG. A recovery is the fact that lifts an unhealthy session,
    // and a shim whose faults cleared while its log said nothing about it is
    // indistinguishable, at default verbosity, from one that never recovered.
    LOGGER.info(
      { component, dropped_count: droppedCount },
      "a component recovered; its faults are cleared and its degraded window is closed",
    );
    this.push(this.diagnostics());
    return true;
  }

  /** Record a window the record plane already closed. */
  recordDegradedWindow(window: conversationv1.SessionDegradedWindow): void {
    this.degradedWindows.push(window);
    this.push(this.diagnostics());
  }

  /** Everything kept since start, for the SessionDiagnostics arm's own tests. */
  get faultCount(): number {
    return this.faults.length;
  }

  /** Every consumer's stream ends; nothing new is accepted. */
  standDown(): void {
    this.standingDown = true;
    for (const subscriber of this.subscribers) subscriber.close();
    this.subscribers.clear();
    LOGGER.debug({}, "closed every WatchSession stream");
  }
}
