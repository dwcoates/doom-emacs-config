/**
 * store/writer.ts — the WRITE half of the record plane, and the one place a
 * `PersistEntry` becomes a `store.v1` row.
 *
 * # The retry buffer: it NEVER DROPS, and it is bounded by BACKPRESSURE
 *
 * Dropping a store write is data loss: the store is the durable record every
 * consumer reads from, the daemon's turn endings included. So a failed batch is
 * HELD in its place and replayed — transiently on the retry schedule, then, once
 * the schedule is exhausted, every `heldRetryMs` for as long as the process lives.
 * A store that is GONE is not papered over: the first failure opens a degraded
 * window and raises `store_unreachable`, the exhausted schedule is an ERROR
 * naming the held keys, and the window stands until a write lands again.
 *
 * The buffer is bounded by pausing its one unbounded producer, never by
 * eviction. At the backlog's high-water mark (rows or bytes) the writer opens a
 * backlog episode — one WARN — and `whenWritable()` holds the vendor message
 * loop until the backlog drains to its low-water mark — one INFO. Why it had to
 * be this (2026-09-23, the owner's logs): the store took 1–15s per one-row
 * write under a host load of 281, the shim enqueued one batch per vendor
 * message (every streamed delta re-upserts its unit), the drain sent one batch
 * per round trip, and the 256-batch buffer filled and evicted 2,135 rows.
 *
 * There is deliberately NO durable producer-side spill: a spill would make the
 * shim a second durable copy of the record, which is exactly the statelessness
 * the architecture rests on not having. Pausing the stream keeps the one copy.
 *
 * # Bounded batches, in exactly the order the rows were produced
 *
 * The store commits an interactive batch as ONE transaction, so a batch's size
 * is the store's hold on its one writer (a 494-row interrupt batch held it for
 * 163s). Queued rows are cut into batches bounded by rows, payload bytes and a
 * time budget the row bound adapts to — merging a backlog of one-row writes and
 * splitting one huge write alike — and a batch ENDS at a turn edge (a prompt,
 * an agent terminal), so an edge's ack never waits on a row produced after it.
 *
 * THE ORDER IS NEVER CHANGED. The store receives every row in the order the
 * shim produced it, across every book: a turn's terminal is the turn's last
 * word (the fold puts even the calls a stop cut ahead of it), and the daemon's
 * watchers and the turn's consumers read "the terminal has landed" as "the whole
 * turn is recorded", subagent books included. A terminal that overtook queued
 * rows would break exactly that, so the only latency lever is the batch itself.
 * A batch the store refuses as malformed is re-sent one row at a time, so only
 * the rows the store will not carry are refused — named at ERROR and raised as
 * a `converter_defect` fault.
 *
 * # Why replay is safe
 *
 * `write_id` is `sha256(producer | source coordinates | discriminator)` — a
 * function of the frame's PROVENANCE, not of when it was sent. A replayed batch
 * mints the same ids and the store absorbs it as a no-op. That is the whole
 * reason the buffer can resend freely.
 *
 * # Routing (the CROSS-PLANE rule agreed with the store lead)
 *
 *   - a prompt, and an `update`/`success`/`failure` frame → a PAGE LINE of the
 *     frame's own book;
 *   - a `detached_work` announcement → also a page line, keyed by the work id.
 *     The proto comment says the announcement lands in "the lifecycle record for
 *     its kind" — but store.v1's lifecycle arms carry an `AgentBash` and an
 *     `AgentWorkflow`, neither of which can hold an `AgentDetachedWork`, and
 *     `GetLiveWork.live_detached` has no other source. Recorded as a deviation
 *     in the record-plane report; the shared fake store already reads liveness
 *     from exactly this row.
 *   - a detached shell run's `AgentBash` frames → the `bash` lifecycle arm;
 *   - a session fact → the `session_update` entry arm;
 *   - all residue → `unserved_item`;
 *   - anything belonging to a KEEP-ALIVE turn → NOTHING. {@link storedEntries}
 *     drops it at the door, before a batch is formed (see below).
 *
 * # A keep-alive is never stored
 *
 * Nothing needs the rows (2026-09-23). The keep-alive's send, its answer and
 * the rewind anchor all live in the engine's memory (`engine/keepalive.ts`), a
 * resume reads the vendor's own transcript, and nothing anywhere reads a
 * stored keep-alive row: the store serves page lines and bash runs only, and
 * the daemon has no store client. The sidecar skips the same records on the
 * file plane, so neither plane writes one. The TAG still rides every entry the
 * fold produces — it is what this door reads — and `write`/`writeDurable` are
 * the only doors, so a keep-alive entry from any producer (the fold, the
 * permission gate, a terminal the teardown writes) is dropped here and nowhere
 * needs to remember to ask.
 */
import { create, toBinary } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1, storev1 } from "../proto.js";
import {
  producerId,
  writeId,
} from "./keys.js";
import {
  DEFAULT_BATCH_POLICY,
  DEFAULT_RETRY_POLICY,
  PersistenceError,
  type AgentOpening,
  type AgentPageSession,
  type FlushOutcome,
  type PersistEntry,
  type Persistence,
  type PersistenceBatchPolicy,
  type PersistenceOptions,
  type PersistenceRetryPolicy,
} from "./persistence.js";
import { createReader } from "./reader.js";
import { lookupAgentByVendorTask, type VendorTaskAnswer } from "./locator.js";
import { createReconciler } from "./reconcile.js";

const LOGGER = bindLog({ component: "shim-store-writer", operation: "shim.store.writer" });

/** The component name every fault and degraded window from this half carries. */
const WRITER_COMPONENT = "store-writer";

// ---------------------------------------------------------------------------
// PersistEntry → StoreEntry: one function per arm (the proto→code mapping)
// ---------------------------------------------------------------------------

/** The write's deterministic identity, from its provenance alone. */
export function entryWriteId(producer: string, entry: PersistEntry): string {
  // NO TRANSLATION: `PersistEntry.source` IS the hashing module's
  // `SourceCoordinates` — one declaration, re-exported by persistence.ts.
  return writeId(producer, entry.source);
}

/** A prompt or a frame, wrapped as the store's servable item. */
function agentItem(entry: PersistEntry): storev1.StoreAgentItem | undefined {
  switch (entry.item.kind) {
    case "prompt":
      return create(storev1.StoreAgentItemSchema, {
        item: { case: "agentPrompt", value: entry.item.prompt },
      });
    case "peer":
      // A peer message is a servable page line in the recipient's book, exactly
      // like a prompt — never the agent's own words, never a turn terminal.
      return create(storev1.StoreAgentItemSchema, {
        item: { case: "peerMessage", value: entry.item.peer },
      });
    case "frame":
      return create(storev1.StoreAgentItemSchema, {
        item: { case: "agentFrame", value: entry.item.frame },
      });
    default:
      return undefined;
  }
}

/**
 * The page line one servable item renders as: in its own book, or -- for a row
 * whose owner this shim never observed -- filed `owner_unknown`, for the store
 * to place in the book already holding its upsert key.
 */
function pageLine(entry: PersistEntry, item: storev1.StoreAgentItem): storev1.StorePageLine {
  return create(storev1.StorePageLineSchema, {
    book:
      entry.ownerUnknown === true
        ? { case: "ownerUnknown", value: create(storev1.StorePageLineOwnerUnknownSchema, {}) }
        : { case: "pageAgentId", value: entry.agentId },
    agentItem: item,
  });
}

/** The `agent_info` arm one entry lands in. */
function agentInfo(entry: PersistEntry): storev1.StoreAgentUpdate["agentInfo"] {
  if (entry.item.kind === "residue") {
    return { case: "unservedItem", value: entry.item.residue };
  }
  if (entry.item.kind === "bash_run") {
    return {
      case: "bash",
      value: create(storev1.StoreAgentBashSchema, {
        run: entry.item.run,
        frame: entry.item.frame,
      }),
    };
  }
  const item = agentItem(entry);
  if (item === undefined) {
    throw new Error(`shim store writer: entry kind ${entry.item.kind} has no servable item`);
  }
  return { case: "serveableFrame", value: pageLine(entry, item) };
}

/**
 * One row as the writer holds it: what it says, and WHERE IN ITS CONVERSATION
 * it was placed — the instant this shim first observed it, stamped once at the
 * writer's door ({@link PlaceClock}) and carried unchanged through every retry.
 */
export interface PlacedEntry {
  readonly entry: PersistEntry;
  readonly place: conversationv1.ConversationPlace;
}

/**
 * THE OBSERVATION CLOCK a row's place is stamped from (store.v1
 * `StoreEntry.place`): the instant the shim first observed the fact, in
 * wall-clock milliseconds, and a monotonic count of the rows stamped within
 * that millisecond as the ordinal.
 *
 * IT NEVER RUNS BACKWARDS. A wall clock can step back (an NTP correction);
 * placing a later observation before an earlier one would draw it above what
 * the conversation said first, so a step back holds the last instant and keeps
 * counting ordinals in it. A clock reading that is not a positive instant is a
 * defect in the injected clock and is refused, never stamped: the store refuses
 * such a place, and a zero would order the row before the whole conversation.
 */
export class PlaceClock {
  private lastAtMs = 0;
  private nextOrdinal = 0;

  constructor(private readonly nowMs: () => number) {}

  /** The next place, strictly after every place this clock has stamped. */
  next(): conversationv1.ConversationPlace {
    const now = Math.floor(this.nowMs());
    if (!(now > 0)) {
      throw new Error(`shim store writer: the observation clock read ${now}, which names no instant`);
    }
    if (now > this.lastAtMs) {
      this.lastAtMs = now;
      this.nextOrdinal = 0;
    }
    const place = create(conversationv1.ConversationPlaceSchema, {
      atMs: BigInt(this.lastAtMs),
      ordinal: this.nextOrdinal,
    });
    this.nextOrdinal += 1;
    return place;
  }
}

/** One placed row as the store's own envelope. */
export function toStoreEntry(producer: string, placed: PlacedEntry): storev1.StoreEntry {
  const { entry, place } = placed;
  if (entry.keepalive) {
    // THE DOOR DROPS EVERY KEEP-ALIVE ENTRY ({@link storedEntries}), so one
    // reaching the envelope is a writer defect, refused like every other
    // entry that cannot be carried.
    throw new Error(
      `shim store writer: keep-alive entry ${entry.upsertKey} reached the envelope; nothing of a keep-alive is stored`,
    );
  }
  if (entry.upsertKey === "") {
    throw new Error("shim store writer: an entry with an empty upsert key would collide with every other");
  }
  // BUILT LAZILY, because a session fact has no agent update at all and
  // `agentInfo` refuses an entry with no servable item — eagerly building one
  // would turn every session row into a failed batch.
  const arm: storev1.StoreEntry["entry"] =
    entry.item.kind === "session_update"
      ? { case: "sessionUpdate", value: entry.item.update }
      : {
          case: "agentUpdate",
          value: create(storev1.StoreAgentUpdateSchema, {
            // THE BOOK IS THE TOP LEVEL here: every book this shim writes is a
            // non-sync agent's (the main agent, or a detached agent with its own
            // stream), which is exactly what `top_level` names.
            // An owner-unknown row states no top level either: the store keeps
            // the stored row's.
            topLevel: entry.item.kind === "residue" ? undefined : entry.agentId,
            agentInfo: agentInfo(entry),
          }),
        };
  return create(storev1.StoreEntrySchema, {
    plane: create(storev1.PlaneSchema, {
      plane: { case: "stream", value: create(storev1.PlaneStreamSchema, {}) },
    }),
    writeId: entryWriteId(producer, entry),
    upsertKey: entry.upsertKey,
    entry: arm,
    // THE ONE PLACE A ROW IS STAMPED WITH ITS TURN, for every arm alike.
    ...(entry.turn === undefined ? {} : { turn: entry.turn }),
    // AND WITH ITS PLACE: the instant the writer's door first observed it,
    // stamped once there, so a retried batch carries the original instant.
    place,
  });
}

/** The batch request one group of entries becomes. */
export function toWriteBatchRequest(
  producer: string,
  entries: readonly PlacedEntry[],
): storev1.WriteBatchRequest {
  // A CLAIM IS NOT A ROW: it joins a spool's task id to its run and lands in
  // the batch's own claim list, never as a StoreEntry.
  const rows: PlacedEntry[] = [];
  const shellRunClaims: storev1.ShellRunClaim[] = [];
  for (const placed of entries) {
    if (placed.entry.item.kind === "shell_run_claim") shellRunClaims.push(placed.entry.item.claim);
    else rows.push(placed);
  }
  return create(storev1.WriteBatchRequestSchema, {
    producer,
    // EVERY SHIM WRITE IS INTERACTIVE: it is live turn content somebody is
    // waiting to see, and the store takes it ahead of any queued bulk copy.
    // The store refuses a write that states no class, so this is not optional.
    writeClass: create(storev1.WriteClassSchema, {
      writeClass: { case: "interactive", value: create(storev1.WriteClassInteractiveSchema, {}) },
    }),
    batch: create(storev1.EntryBatchSchema, {
      entries: rows.map((entry) => toStoreEntry(producer, entry)),
      shellRunClaims,
      // A STREAM-PLANE PRODUCER HAS NO FILE to be positioned in, so no cursor
      // rides with its records. Only the sidecar advances one.
    }),
  });
}

// ---------------------------------------------------------------------------
// The buffered writer
// ---------------------------------------------------------------------------

/**
 * One failed attempt at a batch, and what KIND of failure it was.
 *
 * `terminal` says the failure is about the BATCH rather than about the store,
 * so replaying it just re-sends the same bytes. `converterDefect` says WHOSE
 * defect it is: the producer handed this writer a row that cannot be carried,
 * which is not a store outage and must never be reported as one.
 */
interface BatchFailure {
  readonly detail: string;
  readonly terminal: boolean;
  readonly converterDefect: boolean;
}

/**
 * How a durable write ended, from its caller's point of view.
 *
 * A RESOLUTION, NEVER A REJECTION: the writer settles a group from inside its
 * drain, long after the caller may have stopped listening (it is released the
 * moment the store is known to be down), and a rejected promise nobody awaits
 * any more is an unhandled rejection. The caller turns the outcome into its
 * own typed refusal.
 */
type DurableOutcome =
  | { readonly kind: "durable" }
  | { readonly kind: "degraded"; readonly reason: string }
  | { readonly kind: "refused"; readonly detail: string };

/** One durable write's rows, waiting for the last of them to land. */
interface DurableGroup {
  /** Rows of the group not yet durable. */
  pending: number;
  /** Settle the caller's wait; the FIRST settlement wins and the rest are no-ops. */
  readonly settle: (outcome: DurableOutcome) => void;
}

/** One row waiting to be acked. */
interface QueuedRow extends PlacedEntry {
  /** Its payload size, the unit the byte bounds are stated in. */
  readonly bytes: number;
  /** The durable write it belongs to, when a caller is waiting on it. */
  readonly durable?: DurableGroup;
}

/**
 * How long a one-shot read waits for a book's queued first row to land before
 * asking the store anyway. A healthy batch lands in milliseconds (measured
 * 5-16ms in the e2e worlds); the bound only matters for a store that is not
 * taking writes, where the read's own ask then reports the outage.
 */
export const FIRST_ROW_LANDING_BOUND_MS = 1_000;

/** The serialized size of what one row SAYS, before the store envelope. */
function payloadBytes(entry: PersistEntry): number {
  switch (entry.item.kind) {
    case "prompt":
      return toBinary(conversationv1.AgentPromptSchema, entry.item.prompt).length;
    case "peer":
      return toBinary(conversationv1.PeerMessageSchema, entry.item.peer).length;
    case "frame":
      return toBinary(conversationv1.AgentFrameSchema, entry.item.frame).length;
    case "session_update":
      return toBinary(conversationv1.SessionUpdateSchema, entry.item.update).length;
    case "bash_run":
      return toBinary(conversationv1.AgentBashSchema, entry.item.frame).length;
    case "residue":
      return toBinary(storev1.StoreUnservedItemSchema, entry.item.residue).length;
    case "shell_run_claim":
      return toBinary(storev1.ShellRunClaimSchema, entry.item.claim).length;
    default:
      // A kind the router does not know is refused when its batch is built,
      // loudly and as a converter defect; sizing it as nothing here keeps that
      // one refusal site the only place the defect is judged.
      return 0;
  }
}

/**
 * Whether a row is a TURN EDGE: the row a consumer is blocked on.
 *
 * A turn's prompt (the daemon's StartTurn waits on its ack, R15), and an agent
 * terminal (`success`/`failure` — the frame the daemon ends a turn on, and the
 * one an interrupt waits for). A keep-alive's rows are never edges: nobody is
 * waiting on them.
 */
function isTurnEdge(entry: PersistEntry): boolean {
  if (entry.keepalive) return false;
  if (entry.item.kind === "prompt") return true;
  if (entry.item.kind !== "frame") return false;
  const result = entry.item.frame.result.case;
  return result === "success" || result === "failure";
}

const defaultSleep = (ms: number): Promise<void> =>
  new Promise((resolve) => {
    const timer = setTimeout(resolve, ms);
    // Never hold the process open for a backoff: a shim standing down flushes
    // explicitly, and a pending timer must not be the reason it lingers.
    timer.unref?.();
  });

/**
 * The entries of `entries` the store is to be given: every one but a
 * keep-alive's, each of which is stated at DEBUG and dropped.
 *
 * IT ANNOUNCES NO ROW: the record names the arm and the book, never an upsert
 * key, because nothing was stored under one.
 */
function storedEntries(entries: readonly PersistEntry[]): PersistEntry[] {
  const stored: PersistEntry[] = [];
  for (const entry of entries) {
    if (!entry.keepalive) {
      stored.push(entry);
      continue;
    }
    LOGGER.debug(
      { item_kind: entry.item.kind, discriminator: entry.source.discriminator, agent_id: entry.agentId?.value ?? "(owner unknown)" },
      "a keep-alive turn's entry is never stored; dropped before the batch",
    );
  }
  return stored;
}

/**
 * The agents a batch REGISTERS a book for — not every agent it NAMES.
 *
 * THE STORE REGISTERS A BOOK ON A PAGE LINE AND ON NOTHING ELSE (its
 * `applyServeableFrameLifecycle` calls `ensureAgent` for a prompt and for a
 * frame, and for nothing else). A session update is a fact about the SESSION
 * that carries the main agent in its envelope purely so it has a book to be
 * filed under — it does not create the `agent` row, so a watcher woken on one
 * goes straight back into the refusal it was blocked on, and a caller that
 * concluded the absence was over from one would ask the store for a book that
 * still does not exist. (A keep-alive's entries never reach a batch at all.)
 */
export function booksRegisteredBy(entries: readonly PersistEntry[]): Set<string> {
  const books = new Set<string>();
  for (const entry of entries) {
    if (entry.item.kind !== "prompt" && entry.item.kind !== "frame") continue;
    // AN OWNER-UNKNOWN ROW REGISTERS NO BOOK: it can only land in a book that
    // already holds its key, which some earlier write registered.
    if (entry.ownerUnknown === true) continue;
    books.add(entry.agentId.value);
  }
  return books;
}

/**
 * Build the whole record plane: this write half, plus the reader and the
 * reconciler, behind the one {@link Persistence} seam.
 */
export function createPersistence(options: PersistenceOptions): Persistence {
  const retry: PersistenceRetryPolicy = options.retry ?? DEFAULT_RETRY_POLICY;
  const bounds: PersistenceBatchPolicy = options.batching ?? DEFAULT_BATCH_POLICY;
  const sleep = options.sleep ?? defaultSleep;
  /**
   * This writer's name — UNSET until StartSession names the conversation.
   *
   * A write before then is a LIFETIME-SEQUENCING DEFECT, not a condition to
   * survive: rows landed under a placeholder name would have write ids in a
   * namespace no later replay could ever absorb against, so they would double on
   * the first retry after the real name arrived.
   */
  let producer = options.producer;
  /**
   * Whether any row has been handed to the store under the current name.
   *
   * The one fact {@link Persistence.clearProducer} turns on: a name nothing has
   * written under is still free, and a name a row carries never is.
   */
  let wroteUnderProducer = false;
  const requireProducer = (): string => {
    if (producer === undefined || producer === "") {
      const message =
        "shim store writer: a row was produced before StartSession named the conversation; write ids would land in a namespace no replay can absorb";
      LOGGER.error({ detail: message }, message);
      throw new PersistenceError("store_unavailable", message);
    }
    return producer;
  };

  /** Where every row this writer enqueues is placed (see {@link PlaceClock}). */
  const placeClock = new PlaceClock(options.nowMs);

  const faultListeners = new Set<(fault: conversationv1.SessionFault) => void>();
  const windowListeners = new Set<(window: conversationv1.SessionDegradedWindow) => void>();

  /** Every row enqueued and not yet acked or refused, in produce order. */
  let queue: QueuedRow[] = [];
  /** The payload bytes {@link queue} holds. */
  let queuedBytes = 0;
  /** Set while the drain loop is running, so `write()` never starts a second. */
  let draining: Promise<void> | undefined;
  /** The row bound the NEXT batch is cut at; adapts to the time budget. */
  let rowLimit = bounds.maxBatchRows;
  /** Open degraded window: when it opened, and what it has lost so far. */
  let degradedSince: number | undefined;
  let degradedReason = "";
  let droppedWhileDegraded = 0n;
  /**
   * Set once a batch has failed through the whole retry schedule, until a
   * write lands again. While it stands, `flush()` returns at once.
   */
  let persistentFailure = false;
  /** Every row the store refused as malformed, over the process's whole life. */
  let refusedRows = 0;
  /** The open backlog episode's start, while the vendor stream is paused. */
  let backlogSince: number | undefined;
  /** Durable writes whose caller is still waiting. */
  const durableWaiting = new Set<DurableGroup>();
  /** Whoever is waiting for the backlog to drain (the vendor message loop). */
  let writableWaiters: (() => void)[] = [];
  /** Whoever is waiting for the drain to make progress (`flush()`). */
  let progressWaiters: (() => void)[] = [];

  const wake = (waiters: (() => void)[]): void => {
    for (const resolve of waiters) resolve();
  };
  const noteProgress = (): void => {
    const waiters = progressWaiters;
    progressWaiters = [];
    wake(waiters);
  };

  const emitFault = (kind: "store_unreachable" | "converter_defect", detail: string): void => {
    const fault = create(conversationv1.SessionFaultSchema, {
      component: WRITER_COMPONENT,
      detail,
      kind:
        kind === "store_unreachable"
          ? {
              case: "storeUnreachable",
              value: create(conversationv1.SessionFaultStoreUnreachableSchema, {}),
            }
          : {
              case: "converterDefect",
              value: create(conversationv1.SessionFaultConverterDefectSchema, {}),
            },
    });
    for (const listener of faultListeners) listener(fault);
  };

  const openDegraded = (reason: string): void => {
    if (degradedSince !== undefined) return;
    degradedSince = options.nowMs();
    degradedReason = reason;
    droppedWhileDegraded = 0n;
    // warn: a defect because the record plane is unavailable while writes continue to buffer.
    LOGGER.warn(
      { reason, backlog_rows: queue.length },
      "the store is unreachable; writes are held in order and a degraded window is open",
    );
    // THE WINDOW IS ANNOUNCED BEFORE THE FAULT. A fault restates the session's
    // diagnostics, and a consumer reading the first unhealthy diagnostics has
    // to see the window that explains it -- announcing the fault first would
    // publish an unhealthy session whose degraded windows were still empty.
    for (const listener of windowListeners) {
      listener(
        create(conversationv1.SessionDegradedWindowSchema, {
          component: WRITER_COMPONENT,
          reason,
          beganAtMs: BigInt(degradedSince),
          extent: { case: "open", value: create(conversationv1.SessionDegradedOpenSchema, {}) },
        }),
      );
    }
    // A DURABLE CALLER IS RELEASED THE INSTANT THE OUTAGE IS KNOWN: it holds an
    // RPC open, and the retry schedule is longer than that RPC's deadline. Its
    // rows stay exactly where they are in the buffer.
    const waiting = [...durableWaiting];
    for (const group of waiting) group.settle({ kind: "degraded", reason });
    emitFault("store_unreachable", reason);
  };

  const closeDegraded = (): void => {
    if (degradedSince === undefined) return;
    const beganAtMs = BigInt(degradedSince);
    const endedAtMs = BigInt(options.nowMs());
    const dropped = droppedWhileDegraded;
    const reason = degradedReason;
    degradedSince = undefined;
    degradedReason = "";
    droppedWhileDegraded = 0n;
    LOGGER.debug(
      { dropped_count: dropped.toString(), reason },
      "the store answered again; the degraded window is closed",
    );
    for (const listener of windowListeners) {
      listener(
        create(conversationv1.SessionDegradedWindowSchema, {
          component: WRITER_COMPONENT,
          reason,
          beganAtMs,
          extent: {
            case: "closed",
            value: create(conversationv1.SessionDegradedClosedSchema, {
              endedAtMs,
              droppedCount: dropped,
            }),
          },
        }),
      );
    }
  };

  /**
   * Open or close the backlog episode against the high- and low-water marks.
   *
   * ONE WARN PER EPISODE, ONE INFO TO CLOSE IT. The episode is the backpressure:
   * while it stands, {@link Persistence.whenWritable} holds the vendor message
   * loop, so the backlog is bounded by the marks rather than by eviction.
   */
  const noteBacklog = (): void => {
    if (backlogSince === undefined) {
      if (queue.length < bounds.backlogHighWaterRows && queuedBytes < bounds.backlogHighWaterBytes) return;
      backlogSince = options.nowMs();
      // warn: a defect because the store is taking rows slower than the vendor produces them.
      LOGGER.warn(
        {
          backlog_rows: queue.length,
          backlog_bytes: queuedBytes,
          high_water_rows: bounds.backlogHighWaterRows,
          high_water_bytes: bounds.backlogHighWaterBytes,
        },
        "the store writer's backlog reached its high-water mark; pausing the vendor stream until it drains",
      );
      return;
    }
    if (queue.length > bounds.backlogLowWaterRows || queuedBytes > bounds.backlogLowWaterBytes) return;
    const since = backlogSince;
    backlogSince = undefined;
    LOGGER.info(
      {
        backlog_rows: queue.length,
        backlog_bytes: queuedBytes,
        low_water_rows: bounds.backlogLowWaterRows,
        low_water_bytes: bounds.backlogLowWaterBytes,
        episode_ms: options.nowMs() - since,
      },
      "the store writer's backlog drained to its low-water mark; the vendor stream resumes",
    );
    const waiters = writableWaiters;
    writableWaiters = [];
    wake(waiters);
  };

  /** Hold these rows at the tail of the buffer, in the order given. */
  const enqueue = (entries: readonly PersistEntry[], durable?: DurableGroup): void => {
    for (const entry of entries) {
      const bytes = payloadBytes(entry);
      // THE ROW IS PLACED HERE, ONCE: the writer's door is where the shim
      // first holds the fact, and the place rides the queued row through every
      // retry rather than being re-read from the clock at each send. A row its
      // vendor record placed keeps that place, which the file plane states for
      // the same row (convert/place.ts); only a row no record placed reads the
      // observation clock.
      const place =
        entry.recordPlace === undefined
          ? placeClock.next()
          : create(conversationv1.ConversationPlaceSchema, {
              atMs: BigInt(Math.trunc(entry.recordPlace.atMs)),
              ordinal: entry.recordPlace.ordinal,
            });
      queue.push(durable === undefined ? { entry, place, bytes } : { entry, place, bytes, durable });
      queuedBytes += bytes;
    }
    noteBacklog();
  };

  /**
   * The next batch: the head of the buffer, in produce order, cut at the row
   * and byte bounds — and ENDED right after the first turn edge it takes, so
   * the edge's transaction never carries a row produced after it.
   */
  const nextBatch = (): QueuedRow[] => {
    const batch: QueuedRow[] = [];
    let bytes = 0;
    for (const row of queue) {
      if (batch.length >= rowLimit) break;
      // AT LEAST ONE ROW, ALWAYS: a single row larger than the byte bound is
      // still a row that must land, and a batch that could never form would
      // stall the whole buffer behind it.
      if (batch.length > 0 && bytes + row.bytes > bounds.maxBatchBytes) break;
      batch.push(row);
      bytes += row.bytes;
      if (isTurnEdge(row.entry)) break;
    }
    return batch;
  };

  /**
   * Adapt the row bound to how long the last batch took.
   *
   * THE TIME BOUND. Rows and bytes predict a batch's cost on a healthy store;
   * a loaded host, a cold cache or a checkpoint is what they cannot predict, and
   * it is exactly then that one long transaction holds the store's one writer.
   * A batch that overran the budget halves the next one's row bound; one that
   * finished inside a quarter of it doubles the bound back toward its ceiling.
   */
  const adaptRowLimit = (rows: number, durationMs: number): void => {
    if (durationMs > bounds.batchTimeBudgetMs && rows > 1) {
      // A batch never carries more rows than the bound, so half of it is
      // always below the bound it was cut at.
      rowLimit = Math.floor(rows / 2);
      LOGGER.debug(
        { rows, duration_ms: durationMs, budget_ms: bounds.batchTimeBudgetMs, row_limit: rowLimit },
        "a store batch overran its time budget; halving the next batch's row bound",
      );
      return;
    }
    if (durationMs * 4 <= bounds.batchTimeBudgetMs && rowLimit < bounds.maxBatchRows) {
      rowLimit = Math.min(bounds.maxBatchRows, rowLimit * 2);
      LOGGER.debug(
        { rows, duration_ms: durationMs, budget_ms: bounds.batchTimeBudgetMs, row_limit: rowLimit },
        "a store batch finished well inside its time budget; raising the next batch's row bound",
      );
    }
  };

  /**
   * State every entry the store SKIPPED as a book conflict.
   *
   * THE BATCH IS DURABLE AND THESE ROWS ARE NOT IN IT. The store keeps the row
   * an upsert key was first written under when a later write names a different
   * book, so a stream-plane row the file plane had already booked elsewhere
   * never reaches the book this shim writes and the daemon watches: the turn's
   * answer and its cleared cut went missing that way, with nothing on this side
   * saying so. The contract (`WriteBatchSuccess.skipped`) has the producer
   * speak up on a steady-state skip; this writer has no catch-up window, so
   * every skip is one, and it is a row lost from the book the daemon reads --
   * an ERROR, not a caution.
   */
  /**
   * State every OWNER-UNKNOWN entry the store could not place
   * (`WriteBatchSuccess.unplaced`): a fact this shim observed about a unit whose
   * owner it never saw, written before any row of that unit existed, so no book
   * holds it. The fact is not in the record -- an ERROR, never a caution.
   */
  const reportUnplaced = (unplaced: readonly storev1.WriteBatchUnplacedEntry[]): void => {
    for (const entry of unplaced) {
      LOGGER.error(
        { upsert_key: entry.upsertKey, detail: entry.detail },
        "the store did not place a row this shim wrote with no known owner: no stored row holds its upsert key, so it is not in the record",
      );
    }
  };

  const reportSkipped = (skipped: readonly storev1.WriteBatchSkippedEntry[]): void => {
    for (const skip of skipped) {
      LOGGER.error(
        {
          upsert_key: skip.upsertKey,
          from_book: skip.fromBook,
          to_book: skip.toBook,
          detail: `the store kept ${skip.upsertKey} in book ${skip.fromBook}; this write named ${skip.toBook}`,
        },
        "the store skipped a row this shim wrote: its upsert key already names a row in another book, so the book this shim writes does not carry it",
      );
    }
  };

  /**
   * Send one batch once.
   *
   * Resolves with the store's own refusal, or null when the batch is durable.
   * `terminal` says the refusal is about the BATCH rather than about the store:
   * an `invalid_request` names a malformed row, and replaying it just re-sends
   * the same malformed row while every later batch waits behind it in the one
   * ordered drain.
   */
  const attempt = async (entries: readonly PlacedEntry[]): Promise<BatchFailure | null> => {
    // THE ENVELOPE IS BUILT OUTSIDE THE TRANSPORT'S TRY, so a row this writer
    // cannot envelope at all is never mistaken for the store being down.
    // `toStoreEntry` refuses an entry with no servable item and one with an
    // empty upsert key, and `requireProducer` refuses a row produced before the
    // conversation was named -- every one of those is the PRODUCER's defect,
    // reported for years as `store_unreachable` and retried on a schedule that
    // could never make the same bytes acceptable.
    let request: storev1.WriteBatchRequest;
    try {
      request = toWriteBatchRequest(requireProducer(), entries);
    } catch (error) {
      return {
        detail: error instanceof Error ? error.message : String(error),
        terminal: true,
        converterDefect: true,
      };
    }
    let response: storev1.WriteBatchResponse;
    try {
      response = await options.client.writeBatch(request);
    } catch (error) {
      return {
        detail: error instanceof Error ? error.message : String(error),
        terminal: false,
        converterDefect: false,
      };
    }
    const result = response.result;
    if (result.case === "success") {
      reportSkipped(result.value.skipped);
      reportUnplaced(result.value.unplaced);
      return null;
    }
    if (result.case === "failure") {
      // AN `invalid_request` IS THE STORE ANSWERING, not the store failing: it
      // read the batch and named a malformed row. The store is reachable, so
      // the honest fault is the converter's.
      const invalid = result.value.kind.case === "invalidRequest";
      return { detail: result.value.detail, terminal: invalid, converterDefect: invalid };
    }
    // AN UNSET ONEOF IS ILLEGAL, immediately and loudly: a response that says
    // neither durable nor failed cannot be acted on either way.
    return {
      detail: "store answered a WriteBatch with no result arm set",
      terminal: false,
      converterDefect: false,
    };
  };

  /**
   * Report one failed attempt on the channel that names its real cause.
   *
   * A converter defect and a store outage are DIFFERENT DIAGNOSES and neither
   * may be reported as the other: the store is reachable when it refuses a
   * malformed row, and buffering does not exist for bytes that can never become
   * valid, so a converter defect opens no degraded window.
   */
  const reportFailure = (failure: BatchFailure): void => {
    if (failure.converterDefect) {
      LOGGER.error(
        { detail: failure.detail },
        "the converter produced a row the store plane cannot carry; the store is not at fault",
      );
      emitFault("converter_defect", failure.detail);
      return;
    }
    openDegraded(failure.detail);
  };

  /**
   * Announce a row the store refused as MALFORMED, which can never be written.
   *
   * NOT A DROP FOR AN OUTAGE: the store read the row and said it cannot carry
   * it, so no schedule can land it. It is named at ERROR, counted, and raised
   * (by {@link reportFailure}) as a `converter_defect` fault.
   */
  const refuseLoudly = (row: QueuedRow, attempts: number, detail: string): void => {
    droppedWhileDegraded += 1n;
    refusedRows += 1;
    LOGGER.error(
      { attempts, detail, lost_upsert_keys: [row.entry.upsertKey] },
      "the store refused a row as malformed; it can never be written and is removed from the buffer",
    );
    row.durable?.settle({ kind: "refused", detail });
  };

  /**
   * One-shot reads waiting for a book's first queued row to land, by agent
   * value (see `readFirstPage` below). Woken when a batch registering the
   * book lands; each waiter also carries its own bound.
   */
  const bookLandingWaiters = new Map<string, Set<() => void>>();

  /** Whether a row still queued would register AGENT's book on landing. */
  const bookPendingInQueue = (agent: string): boolean =>
    queue.some((row) => booksRegisteredBy([row.entry]).has(agent));

  /** Everything owed once a batch is durable. */
  const landed = (batch: readonly QueuedRow[], attempts: number, durationMs: number): void => {
    // WOKEN ONLY ONCE THE ROWS ARE DURABLE: a `WatchAgent` that opened before
    // this book existed is blocked on the store holding a row, so waking it on
    // the enqueue would send it back into the same refusal.
    const books = booksRegisteredBy(batch.map((row) => row.entry));
    reader.noteAgentRows(books);
    for (const book of books) {
      const waiting = bookLandingWaiters.get(book);
      if (waiting === undefined) continue;
      bookLandingWaiters.delete(book);
      for (const wake of waiting) wake();
    }
    closeDegraded();
    if (persistentFailure) {
      persistentFailure = false;
      LOGGER.info(
        { attempts, rows: batch.length, backlog_rows: queue.length },
        "the store took the held batch; the buffer is draining again and nothing was dropped",
      );
    }
    for (const row of batch) {
      const group = row.durable;
      if (group === undefined) continue;
      group.pending -= 1;
      if (group.pending === 0) group.settle({ kind: "durable" });
    }
    const bytes = batch.reduce((sum, row) => sum + row.bytes, 0);
    const timing = {
      rows: batch.length,
      bytes,
      attempts,
      duration_ms: durationMs,
      row_limit: rowLimit,
      backlog_rows: queue.length - batch.length,
    };
    // THE FLUSH TIMING IS VISIBLE WHILE THE WRITER IS BEHIND. An ordinary batch
    // is a hot, per-frame event and stays verbose; one landing inside a backlog
    // episode is the evidence of how fast the backlog is draining.
    if (backlogSince === undefined) LOGGER.logVerbose(timing, "batch is durable");
    else LOGGER.debug(timing, "a batch landed while the store writer is behind");
    adaptRowLimit(batch.length, durationMs);
  };

  /**
   * Send one batch until it lands or is refused as malformed.
   *
   * NEVER GIVES UP ON A STORE FAILURE. The first failure opens the degraded
   * window and raises `store_unreachable`; the `maxAttempts`-th declares the
   * failure PERSISTENT at ERROR and releases `flush()`; after that the batch is
   * retried every `heldRetryMs` for as long as the process lives, in
   * its place at the head of the buffer, while the backlog's backpressure
   * pauses the vendor stream. It never rejects: `write()` is fire-and-forget.
   */
  const deliver = async (batch: readonly QueuedRow[]): Promise<void> => {
    for (let attempts = 1; ; attempts += 1) {
      const startedAt = options.nowMs();
      const failure = await attempt(batch);
      if (failure === null) {
        landed(batch, attempts, options.nowMs() - startedAt);
        return;
      }
      if (failure.terminal) {
        if (batch.length > 1) {
          // ONE MALFORMED ROW MUST NOT COST ITS NEIGHBOURS THEIR PLACE. The
          // store refuses the whole transaction for one row, so the batch is
          // sent again ONE ROW AT A TIME, in order, and only the rows the store
          // refuses on their own are refused. Each of those is stated at ERROR
          // with its own fault, so the narrowing itself is an ordinary step.
          LOGGER.debug(
            { rows: batch.length, detail: failure.detail },
            "the store refused a multi-row batch as malformed; resending its rows one at a time to isolate the refusal",
          );
          for (const row of batch) await deliver([row]);
          return;
        }
        reportFailure(failure);
        // A batch is never empty, so this is its one row.
        for (const row of batch) refuseLoudly(row, attempts, failure.detail);
        return;
      }
      reportFailure(failure);
      const backoff =
        attempts >= retry.maxAttempts
          ? retry.heldRetryMs
          : (retry.backoffMs[Math.min(attempts - 1, retry.backoffMs.length - 1)] ?? 0);
      if (attempts === retry.maxAttempts) {
        persistentFailure = true;
        LOGGER.error(
          {
            attempts,
            detail: failure.detail,
            held_rows: queue.length,
            held_upsert_keys: batch.map((row) => row.entry.upsertKey),
            retry_every_ms: backoff,
          },
          "the store has failed this batch through the whole retry schedule; every row is HELD in order and retried, nothing is dropped",
        );
        noteProgress();
      } else if (attempts < retry.maxAttempts) {
        // warn: a defect because the store rejected a batch that must be replayed from memory.
        LOGGER.warn(
          { attempt: attempts, backoff_ms: backoff, detail: failure.detail },
          "the store refused a batch; replaying it from the retry buffer",
        );
      } else {
        LOGGER.debug(
          { attempt: attempts, backoff_ms: backoff, detail: failure.detail, held_rows: queue.length },
          "the store is still failing; retrying the held batch",
        );
        // Every confirmation of the persistent failure is progress a waiting
        // `flush()` answers on: it has seen the store fail once more.
        noteProgress();
      }
      await sleep(backoff);
    }
  };

  /** Drain the buffer, one bounded batch at a time. */
  const drain = async (): Promise<void> => {
    while (queue.length > 0) {
      const batch = nextBatch();
      await deliver(batch);
      const settled = new Set(batch);
      queue = queue.filter((row) => !settled.has(row));
      for (const row of batch) queuedBytes -= row.bytes;
      noteBacklog();
      noteProgress();
    }
    draining = undefined;
    noteProgress();
  };

  const startDraining = (): void => {
    if (draining !== undefined) return;
    draining = drain();
    // Nothing awaits the drain; `flush()` waits on its progress instead, and
    // `deliver()` is written never to reject.
    void draining;
  };

  // THE SAME SCHEDULE ON BOTH HALVES. A busy store is one condition, so the
  // read half backs off exactly as the write half replays.
  const reader = createReader({ client: options.client, retry, sleep });
  const reconciler = createReconciler({ client: options.client, retry, sleep });

  /**
   * Tell the reader which shell-run rows this batch wrote.
   *
   * The rows themselves are read back from the store — a run is served through
   * `WatchBashRun`, whether the shim or the sidecar wrote it — so this is only
   * the writer's observation point: a frame that never reached the store is
   * visible in the log rather than merely missing from a watcher.
   */
  const noteShellRuns = (entries: readonly PersistEntry[]): void => {
    for (const entry of entries) {
      if (entry.item.kind !== "bash_run") continue;
      reader.noteBashFrame(entry.item.run.value, entry.item.frame);
    }
  };

  return {
    setProducer(originalVendorSessionId: string): void {
      const next = producerId(originalVendorSessionId);
      if (producer !== undefined && producer !== next) {
        // THE ORIGINAL ID NEVER CHANGES. A second, different name means the
        // caller mistook a ROTATED id for the original one, which would split
        // this conversation's write-id namespace at the rotation.
        LOGGER.error(
          { producer, next, detail: "the producer identity cannot change after it is set" },
          "refusing to re-key the producer: a conversation has exactly one original vendor session id",
        );
        throw new PersistenceError(
          "store_unavailable",
          `the producer is already ${JSON.stringify(producer)} and cannot become ${JSON.stringify(next)}`,
        );
      }
      producer = next;
      LOGGER.debug({ producer: next }, "named this writer from the conversation's original vendor session id");
    },

    producerHasWrittenRows(): boolean {
      return wroteUnderProducer;
    },

    clearProducer(): void {
      if (producer === undefined) return;
      if (wroteUnderProducer) {
        // A name a row already carries cannot be taken back: the write ids are
        // derived from it, and a later name would put one conversation's rows
        // in two namespaces that can never absorb each other.
        LOGGER.error(
          { producer, detail: "the producer identity cannot be cleared after rows are written" },
          "refusing to un-name the producer: rows have already been written under it",
        );
        throw new PersistenceError(
          "store_unavailable",
          `the producer ${JSON.stringify(producer)} has already written rows and cannot be un-named`,
        );
      }
      LOGGER.debug({ producer }, "un-named the writer: the attempt that named it was abandoned before writing");
      producer = undefined;
    },

    async writeDurable(given: PersistEntry[]): Promise<void> {
      const entries = storedEntries(given);
      if (entries.length === 0) return;
      wroteUnderProducer = true;
      noteShellRuns(entries);
      // THE ROWS JOIN THE ONE ORDERED BUFFER, whatever the caller learns. A
      // durable write that jumped the buffer could land a turn's first activity
      // frame before the prompt row that R15 says precedes it, or the prompt
      // before the previous turn's own rows; behind them, as a turn edge, it
      // ends its batch, so its ack waits on nothing produced after it.
      if (degradedSince !== undefined) {
        // THE OUTAGE IS ALREADY KNOWN, so there is nothing to wait for: the
        // rows are held, and the caller is told at once.
        enqueue(entries);
        startDraining();
        throw new PersistenceError(
          "store_unavailable",
          `the store is unreachable (${degradedReason}); ${entries.length} durable row(s) are held on the retry buffer`,
        );
      }
      const outcome = await new Promise<DurableOutcome>((resolve) => {
        const group: DurableGroup = {
          pending: entries.length,
          settle: (settled) => {
            if (!durableWaiting.delete(group)) return;
            resolve(settled);
          },
        };
        durableWaiting.add(group);
        enqueue(entries, group);
        startDraining();
      });
      if (outcome.kind === "durable") return;
      if (outcome.kind === "degraded") {
        throw new PersistenceError(
          "store_unavailable",
          `the store is unreachable (${outcome.reason}); ${entries.length} durable row(s) are held on the retry buffer`,
        );
      }
      throw new PersistenceError(
        "invalid_request",
        `the store refused a durable row as malformed: ${outcome.detail}`,
      );
    },

    write(given: PersistEntry[]): void {
      const entries = storedEntries(given);
      if (entries.length === 0) return;
      wroteUnderProducer = true;
      noteShellRuns(entries);
      enqueue(entries);
      LOGGER.logVerbose({ entries: entries.length, queued: queue.length }, "rows enqueued");
      startDraining();
    },

    whenWritable(): Promise<void> {
      if (backlogSince === undefined) return Promise.resolve();
      return new Promise<void>((resolve) => {
        writableWaiters.push(resolve);
      });
    },

    async flush(): Promise<FlushOutcome> {
      // Counted ACROSS THIS FLUSH, not over the writer's life: the stand-down's
      // exit code answers "did the writes this flush waited for land", and a
      // lifetime count would report an outage the session already recovered
      // from as a dirty exit.
      //
      // A FLUSH ALWAYS WAITS FOR ONE MORE OUTCOME while rows are queued, even
      // under a standing persistent failure: the next attempt either lands them
      // (and the flush goes on until the buffer is empty) or confirms the
      // failure (and the flush returns, counting what is held). Returning at
      // once would report a store that recovered a moment ago as still down.
      const before = refusedRows;
      while (queue.length > 0) {
        await new Promise<void>((resolve) => {
          progressWaiters.push(resolve);
        });
        if (persistentFailure) break;
      }
      const held = persistentFailure ? queue.length : 0;
      if (held > 0) {
        // The persistent failure was stated at ERROR where it was declared, and
        // the caller states what the held count costs it; this is the branch.
        LOGGER.debug(
          { held_rows: held, detail: "the store is in a persistent failure" },
          "a flush returned with rows the store has not acked; they are still held and retried",
        );
      }
      return { lostRows: refusedRows - before + held };
    },

    openAgentPage(
      agent: conversationv1.AgentId,
      opening: AgentOpening,
      known?: () => boolean,
    ): Promise<AgentPageSession> {
      return reader.openAgentPage(agent, opening, known);
    },

    noteAgentMinted(agentValue: string): void {
      reader.noteAgentMinted(agentValue);
    },

    async readFirstPage(
      agent: conversationv1.AgentId,
      opening: AgentOpening,
      known?: () => boolean,
    ): Promise<conversationv1.HistoryPage> {
      // A BOOK WHOSE FIRST ROW IS STILL IN THIS WRITER'S QUEUE IS ASKED FOR
      // ONCE IT LANDS. The one-shot read always asks the store (reader.ts),
      // and asked a moment early — a reader's page opened while a fresh
      // session's first prompt is being written — the store refuses a book
      // that is about to exist, which is a refusal in its log on a bring-up
      // going exactly as it should. The wait is bounded: a row the store is
      // not taking leaves the read to ask, and to surface what it is told.
      if (bookPendingInQueue(agent.value)) {
        await new Promise<void>((resolve) => {
          const waiting = bookLandingWaiters.get(agent.value) ?? new Set<() => void>();
          const finish = (): void => {
            clearTimeout(timer);
            waiting.delete(finish);
            resolve();
          };
          const timer = setTimeout(() => {
            LOGGER.info(
              { agent: agent.value, bound_ms: FIRST_ROW_LANDING_BOUND_MS },
              "a book's first row did not land within the bound; the read asks the store as it stands",
            );
            finish();
          }, FIRST_ROW_LANDING_BOUND_MS);
          timer.unref?.();
          waiting.add(finish);
          bookLandingWaiters.set(agent.value, waiting);
        });
      }
      return reader.readFirstPage(agent, opening, known);
    },

    readAgentPage(
      agent: conversationv1.AgentId,
      after: conversationv1.HistoryPointer,
    ): Promise<conversationv1.HistoryPage> {
      return reader.readAgentPage(agent, after);
    },

    readPageThrough(
      agent: conversationv1.AgentId,
      through: conversationv1.ConversationThrough,
    ): Promise<conversationv1.HistoryPage> {
      return reader.readPageThrough(agent, through);
    },

    liveWork(session: conversationv1.AgentId): Promise<storev1.GetLiveWorkSuccess> {
      return reconciler.liveWork(session);
    },

    agentByVendorTask(session: conversationv1.AgentId, vendorTaskId: string): Promise<VendorTaskAnswer> {
      return lookupAgentByVendorTask({ client: options.client, retry, sleep }, session, vendorTaskId);
    },

    openBashRun(
      work: conversationv1.DetachedWorkId,
      options: { readonly awaitFirstRow: boolean },
    ): Promise<AsyncIterable<conversationv1.AgentBash>> {
      return reader.openBashRun(work, options);
    },

    onFault(listener: (fault: conversationv1.SessionFault) => void): () => void {
      faultListeners.add(listener);
      return () => faultListeners.delete(listener);
    },

    onDegradedWindow(
      listener: (window: conversationv1.SessionDegradedWindow) => void,
    ): () => void {
      windowListeners.add(listener);
      return () => windowListeners.delete(listener);
    },
  };
}
