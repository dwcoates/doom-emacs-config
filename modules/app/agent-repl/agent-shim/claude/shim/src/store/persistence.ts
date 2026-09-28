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
import type { SourceCoordinates } from "./keys.js";
import type { StoreClient } from "./client.js";
import type { VendorTaskAnswer } from "./locator.js";

// ---------------------------------------------------------------------------
// What one write is
// ---------------------------------------------------------------------------

/**
 * WHERE a frame came from in the vendor's own record, plus what it says.
 *
 * DECLARED ONCE, in `store/keys.ts`, and re-exported here so the engine keeps
 * coding against this module's names. The three fields together are the write's
 * identity: `sha256("<producer>|<vendorUuid[:blockIndex]>|<discriminator>")` —
 * which is why the hashing module owns the declaration and nothing translates
 * between two shapes of one fact.
 */
export type { SourceCoordinates } from "./keys.js";

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
   * Whether this belongs to a KEEP-ALIVE turn. Nothing of a keep-alive is
   * stored: the writer drops a tagged entry at its door (`store/writer.ts`),
   * so the tag is the whole decision and no producer has to ask.
   */
  readonly keepalive: boolean;
  /**
   * THE TURN THIS ROW WAS PRODUCED WITHIN: the turn the shim had open when the
   * vendor produced it (a keep-alive's own id for a keep-alive's rows), or
   * `undefined` for a row produced outside any turn. REQUIRED, so every place
   * that builds a row has to decide it; never guessed. The writer stamps it on
   * the store envelope (`StoreEntry.turn`).
   */
  readonly turn: conversationv1.TurnId | undefined;
  /** What this row says. The arm decides the store arm it lands in. */
  readonly item:
    | { readonly kind: "prompt"; readonly prompt: conversationv1.AgentPrompt }
    | { readonly kind: "peer"; readonly peer: conversationv1.PeerMessage }
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
 * One thing a standing tail tells its consumer, in `WatchAgentResponse.frame`'s
 * own arm names so the consumer relays it without a translation table.
 *
 * - `entry`: a line as written (or upserted), at its pointer.
 * - `retired`: a line the store RETIRED (store.v1 `StoreRetirement`): the
 *   record behind it no longer converts to it, so no page serves it again. It
 *   carries the line as last served, at its own pointer, converted exactly as a
 *   served line is, so the consumer can find and remove what it drew for it.
 *   The pointer stays a valid `known_through`.
 */
export type AgentTailFrame =
  | { readonly case: "entry"; readonly value: conversationv1.HistoryEntryAt }
  | { readonly case: "retired"; readonly value: conversationv1.HistoryEntryAt };

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
  /**
   * Every entry written after the page, in order, each with its pointer, and
   * every retirement of a line the consumer may have drawn.
   */
  readonly tail: AsyncIterable<AgentTailFrame>;
  /**
   * Serve everything up to `through`, then END the tail rather than standing.
   *
   * The session teardown needs this: a `WatchAgent` tail is a STANDING stream
   * by contract, so cutting it at the exit would reach the consumer as a
   * transport failure exactly when it is waiting for the interrupted terminal
   * the teardown just wrote. Concluding it through the book's head at that
   * moment delivers the terminal and then ends the stream honestly.
   *
   * `through` unset — or a pointer already served — ends the tail at once.
   * Idempotent.
   */
  concludeThrough(through?: conversationv1.HistoryPointer): void;
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
type PersistenceFailureKind =
  | "store_unavailable"
  | "unknown_agent"
  | "stale_pointer"
  | "unknown_work"
  // A request THIS PROCESS malformed, refused before it could be answered — a
  // shim defect, never waited out and never read as an empty answer. Today
  // only a live-work read naming no session reaches it.
  | "invalid_request";

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
export interface FlushOutcome {
  /**
   * How many rows this flush could NOT see land: the rows the store refused as
   * malformed while it waited, plus — when it returned because the store is in
   * a persistent failure — every row the writer still HOLDS unacknowledged.
   *
   * THE WRITER DROPS NOTHING FOR AN OUTAGE; a held row keeps being retried for
   * as long as the process lives. This count is what the stand-down's exit code
   * is decided by, because a process that exits with rows still held takes them
   * with it.
   *
   * Scoped to the flush and not to the writer's life: an outage the session
   * already recovered from is not a dirty exit.
   */
  readonly lostRows: number;
}

export interface Persistence {
  /**
   * Name this writer, once the vendor has named the conversation.
   *
   * THE PRODUCER IS KEYED BY THE ORIGINAL VENDOR SESSION ID (`claude-shim:<id>`)
   * and by nothing else. Write ids are `sha256(producer | coordinates | arm)`,
   * so the name decides which namespace a conversation's deterministic ids live
   * in — and a name that rotated with the vendor's current session id would make
   * one writer look like two, splitting a conversation's replay absorption in
   * half at the rotation.
   *
   * Called once, from StartSession, before any write happens. A write attempted
   * before it raises loudly rather than landing rows under a placeholder name
   * that nothing could ever absorb a replay against.
   */
  setProducer(originalVendorSessionId: string): void;
  /**
   * Un-name the writer, so a caller that named it and then FAILED can leave the
   * plane exactly as it found it.
   *
   * StartSession names the producer from the identity it just settled and then
   * starts the vendor query; when the query cannot be started, the whole attempt
   * is abandoned and the next `fresh` StartSession settles a DIFFERENT identity.
   * Without this the writer stayed named after the abandoned attempt and the
   * retry hit {@link Persistence.setProducer}'s re-key guard — a correct guard
   * answering a question nobody meant to ask, surfacing as an unhandled
   * `Internal` on a verb that has a typed refusal for every real condition.
   *
   * THE GUARD ITSELF IS UNTOUCHED, and this is not a way around it: clearing is
   * legal ONLY while nothing has been written under the current name. Once a row
   * exists the name is load-bearing — its write ids are derived from it — and
   * clearing THROWS rather than quietly splitting one conversation's namespace.
   */
  clearProducer(): void;
  /**
   * Whether any row has already been handed to the store under the current name.
   *
   * THE CALLER ASKS BEFORE IT ABANDONS. {@link Persistence.clearProducer} is
   * legal only while the name is still free, and a failed StartSession cannot
   * know from its own error whether the vendor emitted something that the
   * converter persisted before the start was refused — a SessionStart hook that
   * blocks the opening does exactly that. Asking first is what turns "the name
   * is load-bearing now" from an exception escaping a typed verb into the
   * decision it is: the identity STANDS, and the retry re-announces the same
   * one.
   */
  producerHasWrittenRows(): boolean;
  /**
   * Write these rows and resolve when the store says they are DURABLE.
   *
   * THE ROWS ARE HELD EITHER WAY. They join the one ordered retry buffer
   * behind every row produced before them (see {@link Persistence.write}), so
   * the answer never costs the record a row: a rejection says only that this
   * caller cannot wait for the ack, never that the rows are gone.
   *
   * Rejects with a {@link PersistenceError}: `store_unavailable` the moment the
   * store is known to be unreachable (the buffer keeps the rows and replays
   * them, in order, when it answers again), `invalid_request` when the store
   * refused a row as malformed (logged at ERROR and raised as a
   * `converter_defect` fault by the writer). A caller must NOT re-queue the rows
   * on either rejection: the buffer already holds everything that can land.
    * A keep-alive entry is never stored: it is dropped at the door, and a batch
    * of nothing else resolves at once.
   */
  writeDurable(entries: PersistEntry[]): Promise<void>;
  /**
   * Enqueue these rows. Returns at once, and NEVER DROPS A ROW.
   *
   * Rows land in BOUNDED batches (rows, bytes, and a time budget the row bound
   * adapts to), in EXACTLY the order they were produced, across every book; a
   * batch ends at a turn edge (a prompt, an agent terminal), so an edge's ack
   * never waits on a row produced after it. Transient store failures replay from the
   * in-memory buffer; a store that stays down past the retry schedule is an
   * ERROR, a degraded window and a `store_unreachable` fault, and the rows stay
   * HELD and keep being retried. The buffer is bounded by BACKPRESSURE, not by
   * eviction: see {@link Persistence.whenWritable}. There is NO spill to disk.
    * A keep-alive entry is never stored: it is dropped at the door, before any
    * batch is formed.
   */
  write(entries: PersistEntry[]): void;
  /**
   * Resolve when the writer can take more rows without its backlog growing
   * past its high-water mark — at once, unless a backlog episode is open.
   *
   * THIS IS THE BACKPRESSURE. The vendor message loop awaits it before it reads
   * the next message, so a store that falls behind pauses the one unbounded
   * producer instead of the buffer evicting what it holds. It resolves once the
   * backlog drains below its low-water mark.
   */
  whenWritable(): Promise<void>;
  /**
   * Resolve once every buffered write has been acked, or refused as malformed —
   * or, when the store is in a PERSISTENT failure (a batch has failed through
   * the whole retry schedule), as soon as an attempt has declared or confirmed
   * that failure, counting every row still held.
   *
   * The persistent-failure exit is what keeps a stand-down from waiting forever
   * on a store that is gone; the rows it counts are still held and still
   * retried, and the count decides the stand-down's exit code.
   */
  flush(): Promise<FlushOutcome>;
  /**
   * Open one agent's book: a page plus the tail pinned after it.
   *
   * `knownThrough` is the CALLER'S own high-water mark: unset repaints, set
   * returns only entries newer than it.
   *
   * `known` is the CALLER'S belief that the agent exists — the producer's own
   * answer. The store refuses `unknown_agent` for a book it holds no row for,
   * and the row is created by the agent's FIRST WRITE, so a watcher opened on a
   * fresh agent beats it there. With this predicate holding, that refusal is
   * waited out: the opening page comes back EMPTY and its tail stands until the
   * first row lands. Without it — or once it stops holding — the store's
   * refusal is surfaced as it stands.
   */
  openAgentPage(
    agent: conversationv1.AgentId,
    pageSize: number,
    knownThrough?: conversationv1.HistoryPointer,
    known?: () => boolean,
  ): Promise<AgentPageSession>;
  /**
   * Declare that this shim MINTED an agent id, so no book exists for it yet.
   *
   * THE REGISTRATION ORDER, DECLARED RATHER THAN DISCOVERED. A book comes into
   * existence when the first write names its agent; until then the store has
   * never heard of the id and refuses `OpenAgentSession` for it. A fresh
   * conversation's AgentId is minted here, so this session is the one authority
   * that can state the absence — and stating it is what keeps a contract-abiding
   * cold bring-up from probing the store for an answer it already has.
   */
  noteAgentMinted(agentValue: string): void;
  /**
   * The NEWEST page of one book, for a read that stands no tail.
   *
   * THE STORE IS ALWAYS ASKED. {@link Persistence.openAgentPage} may serve a
   * minted-but-unwritten book WITHOUT asking, because what the open is worth is
   * the tail it stands and the absence is this session's own fact. A one-shot
   * read stands no tail, so the store's answer is the whole of what it has to
   * say: asking is what separates the three outcomes such a read must tell
   * apart — a page, an announced book with nothing in it yet, and a store that
   * could not be reached or failed the read.
   *
   * `known` is the producer's own vouching, exactly as on `openAgentPage`: with
   * it holding, the store's `unknown_agent` becomes the EMPTY page that a fresh
   * session's book legitimately is. Every other refusal is surfaced as it
   * stands, so an unreachable store is never served as an empty history.
   */
  readFirstPage(
    agent: conversationv1.AgentId,
    pageSize: number,
    knownThrough?: conversationv1.HistoryPointer,
    known?: () => boolean,
  ): Promise<conversationv1.HistoryPage>;
  /** An OLDER page of one book, walking down from a pointer already served. */
  readAgentPage(
    agent: conversationv1.AgentId,
    pageSize: number,
    after: conversationv1.HistoryPointer,
  ): Promise<conversationv1.HistoryPage>;
  /**
   * The newest page of one book placed AT OR BEFORE an instant: the book as it
   * stood then (a fork's parent at the fork point). The store refuses a book it
   * has never heard of as `unknown_agent`; `more` walks older with
   * {@link Persistence.readAgentPage} as usual.
   */
  readPageThrough(
    agent: conversationv1.AgentId,
    pageSize: number,
    through: conversationv1.ConversationThrough,
  ): Promise<conversationv1.HistoryPage>;
  /**
   * Everything the record holds a start for and no terminal, WITHIN ONE
   * SESSION: the lineage of `session`, this conversation's main agent.
   *
   * The store is shared by every session on the host, and whatever this
   * answers the StartSession reconciliation closes when this vendor does not
   * hold it — so the read is scoped by construction and there is no unscoped
   * form. An empty `session` is refused as `invalid_request` before the store
   * is asked.
   */
  liveWork(session: conversationv1.AgentId): Promise<storev1.GetLiveWorkSuccess>;
  /**
   * WHICH AGENT a vendor task locator names, within the lineage of `session`
   * (store/locator.ts). Never rejects: a failure is the `failed` answer, and the
   * caller writes the one record for whatever came back.
   */
  agentByVendorTask(session: conversationv1.AgentId, vendorTaskId: string): Promise<VendorTaskAnswer>;
  /**
   * One detached shell run's lifecycle frames: its start, its tail, its
   * terminal.
   *
   * NEVER A WAIT. A run the store holds no row for is refused as
   * `unknown_work` at once: the shim writes a run's start ahead of its
   * announcement, so an announced run always has its first row, and there is
   * no producer left to wait for.
   */
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
 * exhaustion is LOUD and that it loses nothing. On the WRITE half, a batch that
 * has failed `maxAttempts` times is declared a persistent failure (an ERROR,
 * with the degraded window and `store_unreachable` fault already standing) and
 * is then retried every `heldRetryMs` for as long as the process lives.
 * On the READ half, `maxAttempts` is where a read gives up and raises.
 */
export interface PersistenceRetryPolicy {
  /** The delay before each attempt after the first, in milliseconds; the last step repeats. */
  readonly backoffMs: readonly number[];
  /** How many attempts before a failure is declared persistent, the first included. */
  readonly maxAttempts: number;
  /**
   * How often a HELD batch is retried once its failure is persistent.
   *
   * It bounds how long a store that came back waits for the shim to notice:
   * the held batch is the head of the one ordered buffer, so nothing lands
   * until it does. A second is a connect attempt a second against a store that
   * is gone, and at most a second of extra outage for one that returned.
   */
  readonly heldRetryMs: number;
}

/**
 * The default schedule: five attempts over roughly four seconds, then a held
 * batch every second.
 */
export const DEFAULT_RETRY_POLICY: PersistenceRetryPolicy = {
  backoffMs: [50, 200, 800, 3000],
  maxAttempts: 5,
  heldRetryMs: 1000,
};

/**
 * How big one write may be, and how big the backlog may grow before the
 * vendor stream is paused.
 *
 * THE STORE COMMITS AN INTERACTIVE BATCH AS ONE TRANSACTION, so the shim's
 * batch size IS the store's hold on its one writer: a 494-row interactive batch
 * held it for 163s (2026-09-23) and everything behind it waited. So a batch is
 * bounded three ways — rows, payload bytes, and a TIME budget the row bound
 * adapts to (a batch that overran it halves the next one's row bound; one that
 * finished well inside it doubles it back toward `maxBatchRows`) — and a batch
 * always carries at least one row, so a single oversized row still lands.
 *
 * The backlog marks are the BACKPRESSURE: at or past either high-water mark the
 * writer opens a backlog episode (one WARN) and {@link Persistence.whenWritable}
 * pauses the vendor message loop; the episode closes (one INFO) once the
 * backlog is at or below BOTH low-water marks.
 */
export interface PersistenceBatchPolicy {
  /** The most rows one WriteBatch carries. */
  readonly maxBatchRows: number;
  /** The most payload bytes one WriteBatch carries, unless one row alone is larger. */
  readonly maxBatchBytes: number;
  /** How long one WriteBatch may take before the next one's row bound is halved. */
  readonly batchTimeBudgetMs: number;
  /** Queued rows at which the backlog episode opens and the vendor stream pauses. */
  readonly backlogHighWaterRows: number;
  /** Queued rows at or below which (bytes permitting) the episode closes. */
  readonly backlogLowWaterRows: number;
  /** Queued payload bytes at which the backlog episode opens. */
  readonly backlogHighWaterBytes: number;
  /** Queued payload bytes at or below which (rows permitting) the episode closes. */
  readonly backlogLowWaterBytes: number;
}

/**
 * The default bounds: the store's own bulk split (64 rows, 1 MiB), a 500ms
 * budget (64 healthy rows cost ~220ms on the owner's largest database), and a
 * backlog of 1,024 rows or 16 MiB before the vendor stream is paused.
 */
export const DEFAULT_BATCH_POLICY: PersistenceBatchPolicy = {
  maxBatchRows: 64,
  maxBatchBytes: 1 << 20,
  batchTimeBudgetMs: 500,
  backlogHighWaterRows: 1024,
  backlogLowWaterRows: 256,
  backlogHighWaterBytes: 16 << 20,
  backlogLowWaterBytes: 4 << 20,
};

/** What {@link createPersistence} needs to exist. */
export interface PersistenceOptions {
  /** The store, already dialed. */
  readonly client: StoreClient;
  /**
   * This writer's name, when it is already known.
   *
   * UNSET at construction is the ordinary case: the shim is built before
   * StartSession, and only StartSession learns the conversation's original
   * vendor session id. {@link Persistence.setProducer} supplies it then.
   */
  readonly producer?: string;
  /** The retry schedule. Defaults to {@link DEFAULT_RETRY_POLICY}. */
  readonly retry?: PersistenceRetryPolicy;
  /** The batch and backlog bounds. Defaults to {@link DEFAULT_BATCH_POLICY}. */
  readonly batching?: PersistenceBatchPolicy;
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

/**
 * The persistence before there is a store to reach.
 *
 * KEPT FROM THE ENGINE'S SCAFFOLD because it is still the honest answer for a
 * build with no store socket: it REFUSES rather than pretending. A placeholder
 * that answered with an empty page would make a history read look like an empty
 * conversation, and one that swallowed writes would make a lost record look
 * like a written one. Every verb answers `store_unavailable`, which the engine
 * already maps onto a loud `SessionFault` and a `ReadHistoryStoreUnavailable`.
 */
export function unavailablePersistence(): Persistence {
  const refuse = (verb: string): PersistenceError =>
    new PersistenceError(
      "store_unavailable",
      `shim persistence: ${verb} has no store to reach in this build`,
    );
  return {
    setProducer: () => undefined,
    clearProducer: () => undefined,
    producerHasWrittenRows: () => false,
    writeDurable: () => Promise.reject(refuse("writeDurable")),
    write: () => {
      throw refuse("write");
    },
    // NOTHING IS EVER BUFFERED HERE -- `write` refuses -- so there is no
    // backlog for a caller to wait out.
    whenWritable: () => Promise.resolve(),
    flush: () => Promise.resolve({ lostRows: 0 }),
    openAgentPage: () => Promise.reject(refuse("openAgentPage")),
    noteAgentMinted: () => undefined,
    readFirstPage: () => Promise.reject(refuse("readFirstPage")),
    readAgentPage: () => Promise.reject(refuse("readAgentPage")),
    readPageThrough: () => Promise.reject(refuse("readPageThrough")),
    liveWork: () => Promise.reject(refuse("liveWork")),
    agentByVendorTask: () =>
      Promise.resolve({ kind: "failed", detail: "shim persistence: agentByVendorTask has no store to reach in this build" }),
    openBashRun: () => Promise.reject(refuse("openBashRun")),
    onFault: () => () => undefined,
    onDegradedWindow: () => () => undefined,
  };
}
