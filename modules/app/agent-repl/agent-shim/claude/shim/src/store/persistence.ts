/**
 * store/persistence.ts — THE seam between the record plane and the engine.
 *
 * # What this file is
 *
 * The engine never speaks store.v1. It hands the fold's output here and reads
 * history back here, and everything between — batching, the retry buffer, the
 * upsert and write-id minting, the open/watch bifurcation, the reconciliation
 * of open obligations — lives behind {@link Persistence}. That is the whole
 * reason the seam is a declaration file of its own: the engine's tests
 * substitute an object literal, and the record plane's tests substitute a store,
 * and neither has to impersonate the other.
 *
 * # Why a PersistEntry rather than a bare frame
 *
 * A store row needs three things a `conversation.v1` frame does not carry:
 * WHICH BOOK it belongs to, WHICH ROW it replaces (the upsert key), and WHERE
 * IN THE VENDOR'S RECORD it came from (the write-id coordinates). Those are the
 * producer's facts, not the conversation model's, so they ride the envelope.
 * The fold mints all three — it is the only thing that has seen the vendor
 * record — and the writer never re-derives one.
 *
 * OWNER: the record-plane agent. The declarations here are STABLE: the engine
 * codes against these names.
 */
import type { conversationv1, storev1 } from "../proto.js";
import type { StoreClient } from "./client.js";

// ---------------------------------------------------------------------------
// What one write is
// ---------------------------------------------------------------------------

/**
 * WHERE a frame came from in the vendor's own record, plus what it says.
 *
 * The three fields together are the write's identity: `sha256("<producer>|<
 * vendorUuid[:blockIndex]>|<discriminator>")`. The discriminator is the frame's
 * ARM PATH, and it is what keeps two frames derived from ONE vendor record
 * (a tool call's start and the session fact the same record implied) from
 * hashing identically and having one silently absorbed as a duplicate.
 */
export interface SourceCoordinates {
  /** The SDK message's `uuid` — the vendor's own name for the record. */
  readonly vendorUuid: string;
  /** The 0-based content-block index, for a frame derived from one block. */
  readonly blockIndex?: number;
  /** The frame's arm path, e.g. `agent_frame.update.activity.read.start`. */
  readonly discriminator: string;
}

/**
 * One row to write: what it is, which book it belongs to, and which row it
 * replaces.
 */
export interface PersistEntry {
  /**
   * THE BOOK: the agent whose page this row renders in — the main agent, or a
   * subagent for its own frames. A session update is a fact about the session
   * rather than about any agent, so it carries the MAIN agent here; it lands as
   * a session row and never as a page line either way.
   */
  readonly agentId: conversationv1.AgentId;
  /**
   * The row this write REPLACES, minted by `store/keys.ts`. Every frame of one
   * unit carries the same key, which is what makes a unit that starts, streams
   * and terminates appear ONCE in the record.
   */
  readonly upsertKey: string;
  /** Where in the vendor's record this came from, and which arm it is. */
  readonly source: SourceCoordinates;
  /**
   * Whether this belongs to a KEEP-ALIVE turn. A keep-alive is real vendor
   * traffic with real cost, so it is recorded — but it has no book, so it lands
   * as `unserved_item.keepalive` and no page ever returns it.
   */
  readonly keepalive: boolean;
  /** What this row says. The arm decides the store arm it lands in. */
  readonly item:
    | { readonly kind: "prompt"; readonly prompt: conversationv1.AgentPrompt }
    | { readonly kind: "frame"; readonly frame: conversationv1.AgentFrame }
    | { readonly kind: "session_update"; readonly update: conversationv1.SessionUpdate }
    | {
        readonly kind: "bash_run";
        readonly run: conversationv1.AgentActivityId;
        readonly frame: conversationv1.AgentBash;
      }
    | { readonly kind: "residue"; readonly residue: storev1.StoreUnservedItem };
}

// ---------------------------------------------------------------------------
// Reading a book back
// ---------------------------------------------------------------------------

/**
 * One agent's book, opened: the page the caller repaints from, and the tail
 * that continues exactly after it.
 *
 * THE TWO ARE ONE ACT, deliberately. The store pins the tail at the instant of
 * the open, so nothing is missed or doubled between page and stream — which is
 * only true if the caller never opens the two separately. `close()` releases
 * the tail; it ends nothing on the agent's side.
 */
export interface AgentPageSession {
  /** The opening page, newest first. */
  readonly page: conversationv1.HistoryPage;
  /** Every entry written after the page, in order, each with its pointer. */
  readonly tail: AsyncIterable<conversationv1.HistoryEntryAt>;
  /** Stop following. Idempotent. */
  close(): void;
}

// ---------------------------------------------------------------------------
// Failures
// ---------------------------------------------------------------------------

/**
 * WHY a persistence call could not be served.
 *
 * A closed set rather than prose, because each leads somewhere different: an
 * unreachable store is waited out, an unknown agent is a caller error, a stale
 * pointer is a re-open, and unknown work is a refusal the caller reports.
 */
export type PersistenceFailureKind =
  | "store_unavailable"
  | "unknown_agent"
  | "stale_pointer"
  | "unknown_work";

/** A refusal from the record plane, carrying the kind a caller switches on. */
export class PersistenceError extends Error {
  constructor(
    public readonly kind: PersistenceFailureKind,
    message: string,
  ) {
    super(message);
    this.name = "PersistenceError";
  }
}

// ---------------------------------------------------------------------------
// The seam
// ---------------------------------------------------------------------------

/**
 * The record plane, as the engine uses it.
 *
 * # Two write verbs, on purpose
 *
 * {@link Persistence.writeDurable} resolves on the store's DURABLE ack, and
 * exists for the one write whose ordering is load-bearing: R15 says the turn's
 * `AgentPrompt` row is acked BEFORE the turn's first activity frame is written,
 * so a reader can never see a turn's work before the prompt that caused it.
 * Everything else uses {@link Persistence.write}, which enqueues and returns —
 * a fold that had to await the store per frame would make the shim's throughput
 * the store's latency.
 */
export interface Persistence {
  /**
   * Write these rows and resolve when the store says they are DURABLE.
   *
   * Rejects with a {@link PersistenceError} when the batch could not be landed
   * after the retry schedule. Nothing is committed on a failure, so a caller
   * that retries duplicates nothing.
   */
  writeDurable(entries: PersistEntry[]): Promise<void>;
  /**
   * Enqueue these rows. Returns at once.
   *
   * Transient store failures replay silently from the BOUNDED in-memory retry
   * buffer; an exhausted retry is a LOUD logged drop naming every lost upsert
   * key, a degraded window, and a `store_unreachable` fault. There is NO spill
   * to disk, ever.
   */
  write(entries: PersistEntry[]): void;
  /** Resolve once every buffered write has been acked or loudly dropped. */
  flush(): Promise<void>;
  /**
   * Open one agent's book: a page plus the tail pinned after it.
   *
   * `knownThrough` is the CALLER'S own high-water mark: unset repaints, set
   * returns only entries newer than it.
   */
  openAgentPage(
    agent: conversationv1.AgentId,
    pageSize: number,
    knownThrough?: conversationv1.HistoryPointer,
  ): Promise<AgentPageSession>;
  /** An OLDER page of one book, walking down from a pointer already served. */
  readAgentPage(
    agent: conversationv1.AgentId,
    pageSize: number,
    after: conversationv1.HistoryPointer,
  ): Promise<conversationv1.HistoryPage>;
  /** Everything the record holds a start for and no terminal. */
  liveWork(): Promise<storev1.GetLiveWorkSuccess>;
  /** One detached shell run's lifecycle frames: the announced start, then the tail. */
  openBashRun(work: conversationv1.DetachedWorkId): Promise<AsyncIterable<conversationv1.AgentBash>>;
  /** Observe faults the record plane raises. Returns an unsubscribe. */
  onFault(listener: (fault: conversationv1.SessionFault) => void): () => void;
  /** Observe degraded windows the record plane opens and closes. */
  onDegradedWindow(listener: (window: conversationv1.SessionDegradedWindow) => void): () => void;
}

// ---------------------------------------------------------------------------
// Construction
// ---------------------------------------------------------------------------

/**
 * The retry schedule, as constants rather than as behavior buried in a loop.
 *
 * IMPLEMENTATION DETAIL, deliberately overridable: what is contractual is that
 * the buffer is BOUNDED and that exhaustion is loud. The numbers are a shape
 * that absorbs a store restart without absorbing a store that is simply gone.
 */
export interface PersistenceRetryPolicy {
  /** How many batches may wait at once before the oldest is dropped LOUDLY. */
  readonly bufferCapacity: number;
  /** The delay before each attempt after the first, in milliseconds. */
  readonly backoffMs: readonly number[];
  /** How many attempts one batch gets in total, the first included. */
  readonly maxAttempts: number;
}

/** The default schedule: five attempts over roughly six seconds, 256 batches deep. */
export const DEFAULT_RETRY_POLICY: PersistenceRetryPolicy = {
  bufferCapacity: 256,
  backoffMs: [50, 200, 800, 3000],
  maxAttempts: 5,
};

/** What {@link createPersistence} needs to exist. */
export interface PersistenceOptions {
  /** The store, already dialed. */
  readonly client: StoreClient;
  /** This writer's name, from `store/keys.ts` `producerId()`. */
  readonly producer: string;
  /** The retry schedule. Defaults to {@link DEFAULT_RETRY_POLICY}. */
  readonly retry?: PersistenceRetryPolicy;
  /** The clock, injected so a test does not wait in real time. */
  readonly nowMs: () => number;
  /**
   * How a delay is taken between retry attempts. Injected for the same reason
   * as the clock: a suite that slept the real backoff would take minutes.
   */
  readonly sleep?: (ms: number) => Promise<void>;
}

export type { StoreClient };

/**
 * Build the record plane over a store client.
 *
 * Implemented in `store/writer.ts` (the write half), `store/reader.ts` (the
 * read half) and `store/reconcile.ts` (the open obligations); this file
 * declares the seam and re-exports the constructor so a caller has one import.
 */
export { createPersistence } from "./writer.js";
