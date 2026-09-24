/**
 * store/writer.ts — the WRITE half of the record plane, and the one place a
 * `PersistEntry` becomes a `store.v1` row.
 *
 * # The retry buffer, and why there is no spill
 *
 * A store that blips must not cost the conversation a frame, so a failed batch
 * holds in a BOUNDED in-memory buffer and replays. A store that is GONE must not
 * be papered over, so the buffer is bounded and exhaustion is LOUD: every lost
 * upsert key is named in the log, a degraded window records how many
 * observations were lost, and a `store_unreachable` fault stands until a write
 * succeeds again.
 *
 * There is deliberately NO durable producer-side spill. A persistent inability
 * to reach the store is a lifetime-sequencing defect to fix, not a condition to
 * survive with fallback persistence — and a spill would make the shim a second
 * durable copy of the record, which is exactly the statelessness the
 * architecture rests on not having.
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
 *   - anything belonging to a KEEP-ALIVE turn, and all residue → `unserved_item`.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1, storev1 } from "../proto.js";
import {
  producerId,
  writeId,
} from "./keys.js";
import {
  DEFAULT_RETRY_POLICY,
  PersistenceError,
  type AgentPageSession,
  type FlushOutcome,
  type PersistEntry,
  type Persistence,
  type PersistenceOptions,
  type PersistenceRetryPolicy,
} from "./persistence.js";
import { createReader } from "./reader.js";
import type { BashRunStanding } from "./reader.js";
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

/** The page line one servable item renders as, in its own book. */
function pageLine(entry: PersistEntry, item: storev1.StoreAgentItem): storev1.StorePageLine {
  return create(storev1.StorePageLineSchema, {
    pageAgentId: entry.agentId,
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
  if (entry.keepalive) {
    // A KEEP-ALIVE IS RECORDED AND NEVER SERVED. It made a real API call and
    // cost real tokens, so dropping it would lose accounting; it has no book,
    // so serving it would put a turn nobody asked for in the feed.
    return {
      case: "unservedItem",
      value: create(storev1.StoreUnservedItemSchema, { unservedItem: { case: "keepalive", value: item } }),
    };
  }
  return { case: "serveableFrame", value: pageLine(entry, item) };
}

/** One `PersistEntry` as the store's own envelope. */
export function toStoreEntry(producer: string, entry: PersistEntry): storev1.StoreEntry {
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
  });
}

/** The batch request one group of entries becomes. */
export function toWriteBatchRequest(
  producer: string,
  entries: readonly PersistEntry[],
): storev1.WriteBatchRequest {
  return create(storev1.WriteBatchRequestSchema, {
    producer,
    // EVERY SHIM WRITE IS INTERACTIVE: it is live turn content somebody is
    // waiting to see, and the store takes it ahead of any queued bulk copy.
    // The store refuses a write that states no class, so this is not optional.
    writeClass: create(storev1.WriteClassSchema, {
      writeClass: { case: "interactive", value: create(storev1.WriteClassInteractiveSchema, {}) },
    }),
    batch: create(storev1.EntryBatchSchema, {
      entries: entries.map((entry) => toStoreEntry(producer, entry)),
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

/** One batch waiting to be acked, and how many attempts it has had. */
interface PendingBatch {
  readonly entries: readonly PersistEntry[];
  attempts: number;
  /**
   * Settles once the batch has left the buffer: durable, dropped loudly, or
   * evicted. A durable write orders itself behind the batch that was last in
   * the buffer when it was made, and no further.
   */
  readonly settled: Promise<void>;
  readonly settle: () => void;
}

/** A batch for the buffer, carrying the promise its leaving settles. */
function pendingBatch(entries: readonly PersistEntry[]): PendingBatch {
  let settle = (): void => {};
  const settled = new Promise<void>((resolve) => {
    settle = resolve;
  });
  return { entries, attempts: 0, settled, settle };
}

const defaultSleep = (ms: number): Promise<void> =>
  new Promise((resolve) => {
    const timer = setTimeout(resolve, ms);
    // Never hold the process open for a backoff: a shim standing down flushes
    // explicitly, and a pending timer must not be the reason it lingers.
    timer.unref?.();
  });

/**
 * Build the whole record plane: this write half, plus the reader and the
 * reconciler, behind the one {@link Persistence} seam.
 */
/**
 * The agents a batch REGISTERS a book for — not every agent it NAMES.
 *
 * THE STORE REGISTERS A BOOK ON A PAGE LINE AND ON NOTHING ELSE (its
 * `applyServeableFrameLifecycle` calls `ensureAgent` for a prompt and for a
 * frame, and for nothing else). A session update is a fact about the SESSION
 * that carries the main agent in its envelope purely so it has a book to be
 * filed under, and a keep-alive lands as an unserved item — neither creates the
 * `agent` row, so a watcher woken on one goes straight back into the refusal it
 * was blocked on, and a caller that concluded the absence was over from one
 * would ask the store for a book that still does not exist.
 */
function booksRegisteredBy(entries: readonly PersistEntry[]): Set<string> {
  const books = new Set<string>();
  for (const entry of entries) {
    if (entry.keepalive) continue;
    if (entry.item.kind !== "prompt" && entry.item.kind !== "frame") continue;
    books.add(entry.agentId.value);
  }
  return books;
}

export function createPersistence(options: PersistenceOptions): Persistence {
  const retry: PersistenceRetryPolicy = options.retry ?? DEFAULT_RETRY_POLICY;
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

  const faultListeners = new Set<(fault: conversationv1.SessionFault) => void>();
  const windowListeners = new Set<(window: conversationv1.SessionDegradedWindow) => void>();

  /** The queue of batches enqueued by `write()` and not yet settled. */
  const queue: PendingBatch[] = [];
  /** Set while the drain loop is running, so `write()` never starts a second. */
  let draining: Promise<void> | undefined;
  /** Open degraded window: when it opened, and what it has lost so far. */
  let degradedSince: number | undefined;
  let degradedReason = "";
  let droppedWhileDegraded = 0n;
  /** Every row this writer has lost, over the process's whole life. */
  let lostRows = 0;

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

  /**
   * Whoever is waiting to learn that the store has GONE degraded.
   *
   * The durable path is the only waiter: it must abandon an inline wait the
   * instant the outage is known rather than sitting out a retry schedule that
   * is longer than its caller's RPC deadline.
   */
  let degradedWaiters: (() => void)[] = [];

  /** Resolves as soon as the writer is in an open degraded window. */
  const whenDegraded = (): Promise<void> =>
    degradedSince !== undefined
      ? Promise.resolve()
      : new Promise<void>((resolve) => {
          degradedWaiters.push(resolve);
        });

  const openDegraded = (reason: string): void => {
    if (degradedSince !== undefined) return;
    degradedSince = options.nowMs();
    degradedReason = reason;
    droppedWhileDegraded = 0n;
    // warn: a defect because the record plane is unavailable while writes continue to buffer.
    LOGGER.warn(
      { reason },
      "the store is unreachable; writes are buffering and a degraded window is open",
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
    const waiters = degradedWaiters;
    degradedWaiters = [];
    for (const wake of waiters) wake();
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
   * Send one batch once.
   *
   * Resolves with the store's own refusal, or null when the batch is durable.
   * `terminal` says the refusal is about the BATCH rather than about the store:
   * an `invalid_request` names a malformed row, and replaying it just re-sends
   * the same malformed row while every later batch waits behind it in the one
   * ordered drain. It is surfaced ONCE and dropped, so the queue keeps moving.
   */
  const attempt = async (entries: readonly PersistEntry[]): Promise<BatchFailure | null> => {
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
    if (result.case === "success") return null;
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

  /** Announce a batch that will never be written, naming every row it lost. */
  const dropLoudly = (batch: PendingBatch, detail: string): void => {
    const lost = batch.entries.map((entry) => entry.upsertKey);
    droppedWhileDegraded += BigInt(lost.length);
    lostRows += lost.length;
    LOGGER.error(
      { attempts: batch.attempts, detail, lost_upsert_keys: lost },
      "DROPPING store writes: the retry schedule is exhausted and there is no spill",
    );
  };

  /**
   * Send one batch, retrying on the schedule.
   *
   * Resolves when the batch is durable, or when it has been dropped loudly. It
   * never rejects: `write()` is fire-and-forget, and a rejection with no caller
   * is an unhandled rejection.
   */
  const deliver = async (batch: PendingBatch): Promise<boolean> => {
    for (;;) {
      batch.attempts += 1;
      const failure = await attempt(batch.entries);
      if (failure === null) {
        // WOKEN ONLY ONCE THE ROWS ARE DURABLE: a `WatchAgent` that opened
        // before this book existed is blocked on the store holding a row, so
        // waking it on the enqueue would send it back into the same refusal.
        reader.noteAgentRows(booksRegisteredBy(batch.entries));
        closeDegraded();
        LOGGER.logVerbose(
          { entries: batch.entries.length, attempts: batch.attempts },
          "batch is durable",
        );
        return true;
      }
      reportFailure(failure);
      // A REFUSED BATCH IS NOT A DEGRADED STORE: the store read this batch and
      // said it is malformed, so no schedule can make it acceptable. Surface it
      // once and drop it rather than blocking every later batch behind it.
      if (failure.terminal) {
        dropLoudly(batch, failure.detail);
        return false;
      }
      if (batch.attempts >= retry.maxAttempts) {
        dropLoudly(batch, failure.detail);
        return false;
      }
      const backoff = retry.backoffMs[Math.min(batch.attempts - 1, retry.backoffMs.length - 1)] ?? 0;
      // warn: a defect because the store rejected a batch that must be replayed from memory.
      LOGGER.warn(
        { attempt: batch.attempts, backoff_ms: backoff, detail: failure.detail },
        "the store refused a batch; replaying it from the retry buffer",
      );
      await sleep(backoff);
    }
  };

  /**
   * Send one batch ONCE, for a caller that is holding an RPC open.
   *
   * THE RETRY SCHEDULE BELONGS TO THE BUFFER, NOT TO A BLOCKED CALLER. The
   * schedule spans seconds by design, which is longer than the deadline the
   * daemon holds its `StartTurn` under -- so replaying inline turns a store
   * outage into a turn the daemon never sees accepted, which is exactly the
   * outcome the durable write's own contract forbids. One attempt is made; a
   * non-terminal failure opens the degraded window and is reported to the
   * caller, whose answer is to re-queue the row on the ordered retry buffer
   * that owns the outage. The write id is deterministic, so the replay the
   * buffer performs is absorbed if this attempt half-landed.
   */
  const deliverOnce = async (batch: PendingBatch): Promise<boolean> => {
    batch.attempts += 1;
    const failure = await attempt(batch.entries);
    if (failure === null) {
      reader.noteAgentRows(booksRegisteredBy(batch.entries));
      closeDegraded();
      LOGGER.logVerbose({ entries: batch.entries.length, attempts: batch.attempts }, "batch is durable");
      return true;
    }
    reportFailure(failure);
    if (failure.terminal) dropLoudly(batch, failure.detail);
    return false;
  };

  /** Drain the queue in order. One loop, so batches land in the order written. */
  const drain = async (): Promise<void> => {
    while (queue.length > 0) {
      const batch = queue[0];
      if (batch === undefined) break;
      await deliver(batch);
      // A full buffer evicts its head from under the drain, delivering or not;
      // shifting blindly then would remove the next batch, undelivered.
      if (queue[0] === batch) queue.shift();
      batch.settle();
    }
    draining = undefined;
  };

  const startDraining = (): void => {
    if (draining !== undefined) return;
    draining = drain();
    // Nothing awaits the drain except flush(); a rejection here would be
    // unhandled, and deliver() is written never to reject.
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

    async writeDurable(entries: PersistEntry[]): Promise<void> {
      if (entries.length === 0) return;
      wroteUnderProducer = true;
      noteShellRuns(entries);
      // ORDERED BEHIND WHATEVER IS BUFFERED: a durable write that jumped the
      // queue could land a turn's first activity frame before the prompt row
      // that R15 says precedes it. The wait is abandoned the moment the store
      // is known to be down, because the drain it is waiting on is then
      // replaying on a schedule measured in seconds and the caller is holding
      // an RPC open. Abandoning keeps the order: the caller's answer to the
      // refusal is to re-queue the row at the TAIL of the same buffer.
      //
      // BEHIND WHAT IS BUFFERED NOW, NEVER BEHIND AN IDLE BUFFER. Waiting for
      // the drain to go idle starved the write for as long as anything kept
      // enqueueing -- a keep-alive's rows, a detached shell's spool -- and a
      // StartTurn held its RPC open on it until the daemon's deadline (e2e
      // TestKeepAliveAnswerAfterVendorTurnNeverServed, 30 of 30 under load).
      // The buffer drains in order, so the batch last in it now settling means
      // every batch ahead of this write has.
      const ahead = queue[queue.length - 1];
      if (ahead !== undefined && degradedSince === undefined) {
        await Promise.race([ahead.settled, whenDegraded()]);
      }
      if (degradedSince !== undefined) {
        throw new PersistenceError(
          "store_unavailable",
          `the store is unreachable (${degradedReason}); ${entries.length} durable row(s) belong on the retry buffer`,
        );
      }
      const batch = pendingBatch(entries);
      const landed = await deliverOnce(batch);
      if (!landed) {
        throw new PersistenceError(
          "store_unavailable",
          `the store did not accept ${entries.length} durable row(s) after ${batch.attempts} attempt(s)`,
        );
      }
    },

    write(entries: PersistEntry[]): void {
      if (entries.length === 0) return;
      wroteUnderProducer = true;
      noteShellRuns(entries);
      if (queue.length >= retry.bufferCapacity) {
        const evicted = queue.shift();
        if (evicted !== undefined) {
          dropLoudly(evicted, `retry buffer is full at ${retry.bufferCapacity} batches`);
          evicted.settle();
        }
      }
      queue.push(pendingBatch(entries));
      LOGGER.logVerbose({ entries: entries.length, queued: queue.length }, "batch enqueued");
      startDraining();
    },

    async flush(): Promise<FlushOutcome> {
      // Counted ACROSS THIS FLUSH, not over the writer's life: the stand-down's
      // exit code answers "did the writes this flush waited for land", and a
      // lifetime count would report an outage the session already recovered
      // from as a dirty exit.
      const before = lostRows;
      while (draining !== undefined) await draining;
      return { lostRows: lostRows - before };
    },

    openAgentPage(
      agent: conversationv1.AgentId,
      pageSize: number,
      knownThrough?: conversationv1.HistoryPointer,
      known?: () => boolean,
    ): Promise<AgentPageSession> {
      return reader.openAgentPage(agent, pageSize, knownThrough, known);
    },

    noteAgentMinted(agentValue: string): void {
      reader.noteAgentMinted(agentValue);
    },

    readFirstPage(
      agent: conversationv1.AgentId,
      pageSize: number,
      knownThrough?: conversationv1.HistoryPointer,
      known?: () => boolean,
    ): Promise<conversationv1.HistoryPage> {
      return reader.readFirstPage(agent, pageSize, knownThrough, known);
    },

    readAgentPage(
      agent: conversationv1.AgentId,
      pageSize: number,
      after: conversationv1.HistoryPointer,
    ): Promise<conversationv1.HistoryPage> {
      return reader.readAgentPage(agent, pageSize, after);
    },

    liveWork(session: conversationv1.AgentId): Promise<storev1.GetLiveWorkSuccess> {
      return reconciler.liveWork(session);
    },

    openBashRun(
      work: conversationv1.DetachedWorkId,
      announcement?: () => BashRunStanding,
    ): Promise<AsyncIterable<conversationv1.AgentBash>> {
      return reader.openBashRun(work, announcement);
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
