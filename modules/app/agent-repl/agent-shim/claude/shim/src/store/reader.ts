/**
 * store/reader.ts — the READ half of the record plane.
 *
 * THE SHIM SERVES HISTORY FROM THE STORE, NEVER FROM MEMORY. That is the whole
 * point of this file: a shim that answered `WatchAgent` out of its own
 * recollection would be a second, divergent copy of the record, and a bounce
 * would lose it. Everything here is a translation of store.v1's page vocabulary
 * into conversation.v1's, and nothing here remembers a conversation.
 *
 * # Open-then-watch is ONE act
 *
 * The store pins a watch token at the instant of the open, so the tail begins
 * exactly after the opening page — nothing missed, nothing doubled. A caller
 * that opened and watched separately would have a race, so this file never
 * exposes the two apart.
 *
 * # The refused-open convention
 *
 * A watch token can be unknown: consumed, or minted by a store that has since
 * restarted. A stream has no failure message to carry that (its response type is
 * the frame), so the refusal arrives as a Connect `NotFound` and the answer is
 * to RE-OPEN with `known_through` set to the last pointer actually served. That
 * is why the tail tracks its high-water mark: it is the only thing that makes
 * the re-open lossless.
 */
import { createHash } from "node:crypto";
import { create, toBinary } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import { bindLog } from "../log.js";
import { conversationv1, storev1 } from "../proto.js";
import type { StoreClient } from "./client.js";
import {
  PersistenceError,
  REPAINT,
  type AgentOpening,
  type AgentPageSession,
  type AgentTailFrame,
} from "./persistence.js";
import { isRetryableRead, readWithRetry, type ReadRetryOptions } from "./retry.js";

const LOGGER = bindLog({ component: "shim-store-reader", operation: "shim.store.reader" });

// ---------------------------------------------------------------------------
// store.v1 → conversation.v1: one function per message (the proto→code mapping)
// ---------------------------------------------------------------------------

/** The store's pointer as the history pointer the daemon echoes. */
function toHistoryPointer(pointer: storev1.StoreItemPointer): conversationv1.HistoryPointer {
  if (pointer.value === "") {
    throw new PersistenceError("stale_pointer", "the store served an empty item pointer");
  }
  return create(conversationv1.HistoryPointerSchema, { value: pointer.value });
}

/** The history pointer back as the store's own, for a request. */
export function toStorePointer(pointer: conversationv1.HistoryPointer): storev1.StoreItemPointer {
  if (pointer.value === "") {
    throw new PersistenceError("stale_pointer", "a history pointer is never the empty string");
  }
  return create(storev1.StoreItemPointerSchema, { value: pointer.value });
}

/** One stored line as the history entry it renders. */
/** An opening, as the store's `OpenAgentSessionRequest.opening` arm. UNSET is the repaint. */
function toStoreOpening(opening: AgentOpening): storev1.OpenAgentSessionRequest["opening"] {
  switch (opening.case) {
    case "knownThrough":
      return { case: "knownThrough", value: toStorePointer(opening.value) };
    case "tailOnly":
      return { case: "tailOnly", value: create(storev1.AgentSessionTailOnlySchema, {}) };
    case "repaint":
      return { case: undefined };
  }
}

export function toHistoryEntry(line: storev1.StorePageLine): conversationv1.HistoryEntry {
  const item = line.agentItem?.item;
  switch (item?.case) {
    case "agentPrompt":
      return create(conversationv1.HistoryEntrySchema, {
        entry: { case: "userPrompt", value: item.value },
      });
    case "agentFrame":
      return create(conversationv1.HistoryEntrySchema, {
        entry: { case: "agentFrame", value: item.value },
      });
    case "peerMessage":
      return create(conversationv1.HistoryEntrySchema, {
        entry: { case: "peerMessage", value: item.value },
      });
    default:
      // AN UNSET ONEOF IS ILLEGAL AT THE CONSUMER, loudly: a line that is
      // neither a prompt nor a frame cannot be drawn, and forwarding it as an
      // empty entry would put a blank row in someone's feed.
      throw new PersistenceError(
        "store_unavailable",
        "the store served a page line whose item arm is unset",
      );
  }
}

/**
 * A stored line's conversation place, on the arm naming who established it.
 *
 * THE ARM PASSES THROUGH UNCHANGED: the store's `recorded_place` is a place a
 * producer stated, its `received_place` the store's receipt instant standing
 * in, and `HistoryEntryAt` spells the same two arms. This is the ONE mapping
 * every serving path uses (a page, a catch-up, a live or retired frame), so no
 * path can serve an entry at a place another path would not.
 *
 * A LINE THE STORE SERVED UNPLACED is passed through unplaced — the proto's
 * "the serving side states no places at all", which a consumer orders by its
 * own receipt and records that it did. The store states a place on every line
 * it serves, so this is a store that predates places, and it is traced.
 */
function toHistoryPlace(line: storev1.StoreLineAt): conversationv1.HistoryEntryAt["place"] {
  switch (line.place.case) {
    case "recordedPlace":
      return { case: "recordedPlace", value: line.place.value };
    case "receivedPlace":
      return { case: "receivedPlace", value: line.place.value };
    default:
      LOGGER.debug(
        { pointer: line.at?.value },
        "the store served a line with no conversation place; it is served unplaced",
      );
      return { case: undefined };
  }
}

/** One stored line, its pointer, its turn and its place. */
function toHistoryEntryAt(line: storev1.StoreLineAt): conversationv1.HistoryEntryAt {
  if (line.at === undefined || line.line === undefined) {
    throw new PersistenceError(
      "store_unavailable",
      "the store served a line with no pointer or no content",
    );
  }
  return create(conversationv1.HistoryEntryAtSchema, {
    at: toHistoryPointer(line.at),
    entry: toHistoryEntry(line.line),
    // The row's turn as the store keeps it (its first stamp), passed through.
    ...(line.turn === undefined ? {} : { turn: line.turn }),
    place: toHistoryPlace(line),
  });
}

/**
 * What a served pointer's content is unknown as: the caller's own
 * `known_through`, which this session was never handed the line for. It matches
 * no fingerprint, so nothing is ever withheld against it.
 */
const CONTENT_UNKNOWN = "";

/**
 * What a RETIRED pointer's content is recorded as: the consumer was told to
 * remove the line, so it holds nothing there. Like {@link CONTENT_UNKNOWN} it
 * matches no fingerprint (a sha256 in base64 is never this string), so a row
 * later taken back at the same position is always served again.
 */
const RETIRED_CONTENT = "retired";

/**
 * The identity of one line's CONTENT. A pointer names a position and an upsert
 * keeps it, so "have I served this line as it now stands" is the pointer AND
 * this: the same pointer with a new fingerprint is new information.
 */
function lineFingerprint(line: storev1.StoreLineAt): string {
  const content = line.line === undefined ? new Uint8Array() : toBinary(storev1.StorePageLineSchema, line.line);
  return createHash("sha256").update(content).digest("base64");
}

/** The page's completeness arm, in the history vocabulary. */
function toHistoryBoundary(
  boundary: storev1.AgentSessionPage["boundary"] | storev1.ReadAgentPageSuccess["boundary"],
): conversationv1.HistoryPage["boundary"] {
  switch (boundary.case) {
    case "more":
      if (boundary.value.lastItem === undefined) {
        throw new PersistenceError(
          "store_unavailable",
          "the store said older lines remain but named no pointer to walk from",
        );
      }
      return {
        case: "more",
        value: create(conversationv1.HistoryMoreSchema, {
          lastEntry: toHistoryPointer(boundary.value.lastItem),
        }),
      };
    case "floor":
      return { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) };
    default:
      throw new PersistenceError(
        "store_unavailable",
        "the store served a page with no boundary arm set",
      );
  }
}

/** The opening page, whole. */
function toHistoryPage(page: storev1.AgentSessionPage): conversationv1.HistoryPage {
  return create(conversationv1.HistoryPageSchema, {
    entries: page.lines.map(toHistoryEntryAt),
    boundary: toHistoryBoundary(page.boundary),
  });
}

/**
 * How many times in a row the store may END a standing tail, delivering
 * nothing, before the tail stops re-opening and says so.
 *
 * A STORE THAT ENDS A STANDING WATCH IS NOT A CONCLUSION. `WatchAgentSession`
 * has no failure arm and no natural end: the store's own handler returns `nil`
 * — a clean, error-free end of stream — when it is SHUTTING DOWN or when it
 * sees the caller go away, and connect-node presents that to this side as an
 * async iterable that simply finished. This tail used to treat that as "the
 * store closed, so stop", and the whole chain above it was silent: the engine's
 * `for await` fell out, the `WatchAgent` generator returned normally, and the
 * route recorded `completed` at DEBUG. The daemon, on the other side of the
 * socket, saw a standing stream end while the session lived, opened a
 * `link_fault` and a health fault — and the shim's own log, the only place that
 * could say what ended it, held nothing at any level it was running at.
 *
 * So an unasked end is now the same class of event as the refused token beside
 * it: the store forgot the watch, the book still exists, and the recovery is to
 * re-open from the last pointer actually served. The budget bounds the one case
 * that recovery cannot fix — a store that accepts a token and immediately ends
 * the stream again — so a shim cannot spin re-opening against it. Three,
 * because a store restarting under a live shim ends each watch ONCE; a second
 * and third are the retry schedule's own attempts landing mid-restart, and a
 * fourth is no longer a restart.
 */
const UNASKED_END_BUDGET = 3;

// ---------------------------------------------------------------------------
// Failure translation
// ---------------------------------------------------------------------------

/**
 * Any store read refusal that carries a typed `kind`, narrowed to the arm.
 *
 * `detail` is the store's prose account and is NEVER switched on: it is the
 * driver's text, a field name, a sentence a store maintainer may reword at any
 * time, and classifying by substring made the reader's behavior depend on that
 * wording. An earlier version did exactly that — "unknown agent" and "pointer"
 * matched anywhere in the string — so a storage failure whose driver text
 * happened to say "pointer" was reported to the engine as a stale pointer and
 * the engine re-read a book that was actually unreachable.
 */
type TypedReadFailure = {
  readonly detail: string;
  readonly kind:
    | { readonly case: "invalidRequest" }
    | { readonly case: "unknownAgent" }
    | { readonly case: "stalePointer" }
    | { readonly case: "storageFailure" }
    | { readonly case: undefined };
};

/**
 * A store read refusal, as the kind the engine switches on.
 *
 * THE ARM DECIDES, never the detail:
 *   - `stale_pointer` → `stale_pointer`. The caller's mark names no line of
 *     this book; it re-reads from the floor.
 *   - `unknown_agent` → `unknown_agent`. The store's own refusal of a
 *     well-formed agent id that names no book (landing 7): the record plane,
 *     not the shim, is what knows whether a book exists.
 *   - `invalid_request` → `unknown_agent`. The store validates the book before
 *     anything else, so on a read the only request the shim can malform is the
 *     agent id — the pointer is minted by this process. Reported
 *     as the condition the engine can act on rather than as a generic refusal.
 *   - `storage_failure` → `store_unavailable`, and so is an UNSET arm: a
 *     refusal that names no reason is a store the shim cannot trust, and
 *     guessing a kinder arm would make the engine retry into a broken store.
 */
export function readFailure(failure: TypedReadFailure): PersistenceError {
  switch (failure.kind.case) {
    case "stalePointer":
      return new PersistenceError("stale_pointer", failure.detail);
    case "unknownAgent":
    case "invalidRequest":
      return new PersistenceError("unknown_agent", failure.detail);
    case "storageFailure":
      return new PersistenceError("store_unavailable", failure.detail);
    default:
      LOGGER.error(
        { detail: failure.detail },
        "the store refused a read and named no reason; treated as unavailable",
      );
      return new PersistenceError("store_unavailable", failure.detail);
  }
}

/** A thrown transport error, as the kind the engine switches on. */
export function transportFailure(error: unknown): PersistenceError {
  if (error instanceof PersistenceError) return error;
  const detail = error instanceof Error ? error.message : String(error);
  return new PersistenceError("store_unavailable", detail);
}

/** Whether a thrown error is the store's "I do not know this token" refusal. */
function isNotFound(error: unknown): boolean {
  return error instanceof ConnectError && error.code === Code.NotFound;
}

// ---------------------------------------------------------------------------
// The reader
// ---------------------------------------------------------------------------

/** Where a page read below the newest page begins: the store's two position arms. */
type PageFrom =
  | { readonly case: "after"; readonly value: conversationv1.HistoryPointer }
  | { readonly case: "through"; readonly value: conversationv1.ConversationThrough };

/** The read half, plus the two notes the write half feeds it about shell runs. */
interface Reader {
  openAgentPage(
    agent: conversationv1.AgentId,
    opening: AgentOpening,
    known?: () => boolean,
  ): Promise<AgentPageSession>;
  /**
   * The newest page of one book, for a read that stands no tail.
   *
   * Always asks the store, so an unreachable store is a refusal and never an
   * empty page. `known` is the producer's own vouching, and it turns the
   * store's `unknown_agent` into the empty page a fresh session's book is.
   */
  readFirstPage(
    agent: conversationv1.AgentId,
    opening: AgentOpening,
    known?: () => boolean,
  ): Promise<conversationv1.HistoryPage>;
  readAgentPage(
    agent: conversationv1.AgentId,
    after: conversationv1.HistoryPointer,
  ): Promise<conversationv1.HistoryPage>;
  readPageThrough(
    agent: conversationv1.AgentId,
    through: conversationv1.ConversationThrough,
  ): Promise<conversationv1.HistoryPage>;
  openBashRun(
    work: conversationv1.DetachedWorkId,
    options: { readonly awaitFirstRow: boolean },
  ): Promise<AsyncIterable<conversationv1.AgentBash>>;
  /** Observe one shell-run frame the writer took: the log line that says it was written. */
  noteBashFrame(runValue: string, frame: conversationv1.AgentBash): void;
  /**
   * Report that a batch LANDED, so a watcher waiting for one of its books wakes.
   *
   * Called after the store accepted the rows and never before: the waiter is
   * blocked on the book EXISTING, and waking it on the enqueue would send it
   * straight back into the same refusal.
   */
  noteAgentRows(agents: Iterable<string>): void;
  /**
   * Report that this shim MINTED an agent id, so the store has never heard of
   * it and cannot until this shim's first write under it lands.
   *
   * THE ORDER, STATED RATHER THAN PROBED. A book is registered by its first
   * write, so a consumer that opens the main agent's watch before the first
   * turn — which the endpoint contract tells the daemon to do — names a book
   * that provably does not exist yet. Asking the store anyway earned a
   * `unknown_agent` refusal on every healthy cold bring-up, which the store
   * rightly logged as the refusal it is. With this note the reader knows the
   * answer without asking and defers the book directly.
   *
   * ONLY FOR AN ID THIS PROCESS MINTED. A resumed conversation's id was minted
   * by an earlier session that may well have written under it, and claiming
   * absence there would serve an empty opening page over a book with history.
   */
  noteAgentMinted(agentValue: string): void;
}

/** What a reader needs to exist. */
interface ReaderOptions extends ReadRetryOptions {
  readonly client: StoreClient;
}

/**
 * How long a waiter sleeps before re-asking the store for an AGENT's book.
 *
 * The note from {@link Reader.noteAgentRows} is the real signal here, and it is
 * COMPLETE for one shim: every row that creates an agent row is written by this
 * process, so a waiter is woken by the very commit it was blocked on. The
 * recheck is the belt to that braces, and it is deliberately slow: an agent
 * wait stands for as long as a fresh session is idle, and re-asking the store
 * forty times a second meanwhile would be a busy loop against an external
 * resource.
 */
const AGENT_ROW_RECHECK_MS = 250;

/** One "wait for the first row" rendezvous, keyed by whatever names the thing. */
interface FirstRowGate {
  /** Wake everyone waiting on this key; the store now holds a row for it. */
  wake(key: string): void;
  /**
   * Wait for this key's first row to land, or for the recheck cadence (or
   * DELAYMS, for a caller backing its recheck off).
   */
  wait(key: string, delayMs?: number): Promise<void>;
}

/**
 * A first-row rendezvous on one recheck cadence.
 *
 * AN AGENT BOOK'S WAIT, AND NO LONGER A SHELL RUN'S. A shell run's first row is
 * its start, which this shim writes ahead of the run's announcement, so a watch
 * opened on an announced run finds it in the store and waits for nobody.
 */
function firstRowGate(recheckMs: number): FirstRowGate {
  const waiters = new Map<string, Set<() => void>>();
  return {
    wake(key) {
      const waiting = waiters.get(key);
      if (waiting === undefined) return;
      waiters.delete(key);
      for (const wake of waiting) wake();
    },
    wait(key, delayMs = recheckMs) {
      return new Promise<void>((resolve) => {
        let settled = false;
        const finish = (): void => {
          if (settled) return;
          settled = true;
          clearTimeout(timer);
          waiters.get(key)?.delete(finish);
          resolve();
        };
        const timer = setTimeout(finish, delayMs);
        // Never hold the process open for a wait nobody is blocked on.
        timer.unref?.();
        const waiting = waiters.get(key) ?? new Set<() => void>();
        waiting.add(finish);
        waiters.set(key, waiting);
      });
    },
  };
}

export function createReader(options: ReaderOptions): Reader {
  const client = options.client;

  /**
   * Who is waiting for a given AGENT's first stored row, by agent value.
   *
   * `OpenAgentSession` refuses `unknown_agent` until something has been written
   * under the id (landing 7), and the main agent's row is created by its first
   * write — so a daemon that opens `WatchAgent` on a fresh session, exactly as
   * the contract tells it to, arrives before the book exists.
   */
  const agentRows = firstRowGate(AGENT_ROW_RECHECK_MS);

  /**
   * Agents whose id THIS PROCESS MINTED and has written nothing under yet.
   *
   * A MINTED ID IS A FACT, NOT AN OBSERVATION. The store registers a book on
   * the first write that names its agent, and a minted id is a uuid this shim
   * made moments ago — so no book can exist for it, and none can come into
   * existence except through a write this shim makes and observes landing.
   * That is what lets a caller here SKIP the ask instead of making it: an ask
   * for a book known absent buys nothing but an `unknown_agent` refusal in the
   * store's log, and a contract-abiding cold bring-up made exactly two of them
   * per fresh agent — the open, and the deferred session's own re-open.
   *
   * IT IS NOT THE OBSERVED refusal. A store that answered `unknown_agent` for
   * an id this shim did not mint has told us about a book ANOTHER writer may
   * yet register, and that wait keeps its recheck cadence (below) as the belt
   * to a note that would never come. Only a minted id waits on the note alone.
   *
   * An id leaves the set the moment a batch naming it lands, or an open for it
   * succeeds — either way the book demonstrably exists.
   */
  const booksMinted = new Set<string>();

  /**
   * The open itself.
   *
   * `pageOnly` STATES THAT NO WATCH FOLLOWS, and it is the caller's to state
   * because the store cannot work it out: OpenAgentSession is unary and the
   * service has no close, so a token minted for a page that is then abandoned
   * can never be reclaimed and lives for the store's whole process lifetime.
   * A page-only open mints nothing and answers with `watch` unset.
   */
  const openSession = async (
    agent: conversationv1.AgentId,
    opening: AgentOpening,
    pageOnly = false,
  ): Promise<storev1.OpenAgentSessionSuccess> => {
    let response: storev1.OpenAgentSessionResponse;
    try {
      response = await client.openAgentSession(
        create(storev1.OpenAgentSessionRequestSchema, {
          agent,
          opening: toStoreOpening(opening),
          pageOnly,
        }),
      );
    } catch (error) {
      // A STORE RESTART IS THE ORDINARY SHAPE OF THIS FAILURE, NOT A FAULT.
      // Every caller here replays the open on {@link onReadRetrySchedule},
      // which is exactly what an ORDERED restart -- `launchctl kickstart` on
      // the store, under a live shim -- needs from this side: the socket is
      // down for as long as the store takes to come back, and the schedule is
      // there to absorb it. So the attempt is INFO, and the ERROR belongs to
      // the schedule being SPENT, which the wrapper says once.
      LOGGER.info(
        { agent: agent.value, detail: String(error) },
        "the store could not be reached to open an agent's book; replaying the open on the read retry schedule",
      );
      throw transportFailure(error);
    }
    const result = response.result;
    if (result.case === "success") return result.value;
    if (result.case === "failure") {
      LOGGER.debug(
        { agent: agent.value, detail: result.value.detail },
        "the store refused to open an agent's book",
      );
      throw readFailure(result.value);
    }
    throw new PersistenceError(
      "store_unavailable",
      "the store answered OpenAgentSession with no result arm set",
    );
  };

  /**
   * One store read, on the read half's retry schedule, with the EXHAUSTION said
   * once and loudly.
   *
   * THE ATTEMPTS ARE NOT THE FAULT; THE SCHEDULE RUNNING OUT IS. A store
   * restarting under a live shim -- the ordered `launchctl kickstart` a deploy
   * performs -- refuses every call it meets for as long as its socket is down,
   * and the read half exists to ride that out: the attempts are INFO at the
   * site that made them, and this is the one place that says the store never
   * came back. The error itself is re-thrown exactly as it stood, so the
   * caller's typed arm and the engine's fault are unchanged.
   */
  const onReadRetrySchedule = async <T>(
    what: string,
    agent: conversationv1.AgentId,
    read: () => Promise<T>,
  ): Promise<T> => {
    try {
      return await readWithRetry(what, read, options);
    } catch (error) {
      if (isRetryableRead(error)) {
        LOGGER.error(
          { agent: agent.value, read: what, detail: String(error) },
          "gave up reading an agent's book: the store stayed unreachable for the whole read retry schedule",
        );
      }
      throw error;
    }
  };

  /** One book, opened from the store as it stands. Refuses a book with no rows. */
  const openBookNow = async (
    agent: conversationv1.AgentId,
    opening: AgentOpening,
  ): Promise<AgentPageSession> => {
    const opened = await openSession(agent, opening);
    if (opened.page === undefined || opened.watch === undefined) {
      throw new PersistenceError(
        "store_unavailable",
        "the store opened a reading session with no page or no watch token",
      );
    }
    // THE STORE ANSWERED FOR THE BOOK, so it holds a row for it. A minted id
    // whose book demonstrably exists is no longer a certain absence, and the
    // belief must not outlive the answer that disproved it.
    booksMinted.delete(agent.value);
    const page = toHistoryPage(opened.page);
    const knownThrough = opening.case === "knownThrough" ? opening.value : undefined;
    /**
     * THE BOOK'S NEWEST LINE AS OF THE OPEN, as the store names it on every
     * open (unset: the book holds none).
     *
     * A tail-only page is empty by request, so this is the only thing such a
     * session stands on: the pointer a teardown concluding through the book's
     * head names (already "served", so the tail ends at once instead of
     * spending its conclusion budget on a line it will never carry), the mark
     * a lossless re-open passes as `known_through` (never a repaint, which
     * would replay history nobody asked for), and whether the book was empty.
     */
    const newest = opened.newest === undefined ? undefined : toHistoryPointer(opened.newest);
    LOGGER.debug(
      { agent: agent.value, opening: opening.case, entries: page.entries.length },
      "opened an agent's book and pinned its tail",
    );

    // The caller's high-water mark, kept so a refused re-open is lossless. It
    // is the LAST pointer served rather than the newest by position, because an
    // upsert of an old row is new information about a line already read past:
    // re-opening from the newer pointer would drop it. A tail-only open's mark
    // is the book's newest line as of the open.
    let servedThrough: conversationv1.HistoryPointer | undefined =
      page.entries[0]?.at ?? knownThrough ?? (opening.case === "tailOnly" ? newest : undefined);
    /**
     * EVERY pointer this session has handed the consumer.
     *
     * THE CONCLUSION ASKS "HAVE I SERVED THIS", WHICH THE LAST POINTER CANNOT
     * ANSWER. The store streams an upsert of an old row AT ITS ORIGINAL
     * POINTER, so `servedThrough` walks BACKWARD whenever a line already read
     * past is updated — and a teardown concluding through the book's HEAD then
     * names a row that was served earlier and will never be sent again. The
     * tail stood on it and the shim's `KillSession` spent its whole
     * `WATCHER_CONCLUSION_BUDGET_MS` on a stream that already owed nothing.
     *
     * A pointer names a POSITION, and an upsert reuses the position it already
     * had, so this set is bounded by the book's LINES rather than by the frames
     * written to them: a streaming unit upserts one row many times and adds one
     * member. The pointers are opaque values this shim never parses, so
     * membership is the only question it can ask of them.
     */
    const served = new Map<string, string>();
    if (knownThrough !== undefined) served.set(knownThrough.value, CONTENT_UNKNOWN);
    if (opening.case === "tailOnly" && newest !== undefined) served.set(newest.value, CONTENT_UNKNOWN);
    for (const line of opened.page.lines) {
      if (line.at !== undefined) served.set(line.at.value, lineFingerprint(line));
    }
    let token: storev1.AgentSessionToken = opened.watch;
    let stopped = false;
    /**
     * The pointer the tail concludes on, once the teardown set one.
     *
     * The tail keeps standing until it has SERVED this pointer, which is what
     * makes the conclusion lossless: the terminal row the teardown just wrote
     * reaches the consumer, and only then does the stream end.
     */
    let concludeAt: conversationv1.HistoryPointer | undefined;
    let concluding = false;
    /** Whether the consumer has been handed everything the conclusion named. */
    const settled = (): boolean =>
      concluding && (concludeAt === undefined || served.has(concludeAt.value));
    // CANCELLING THE CALL IS HOW A STANDING TAIL ENDS. Connect's stream close
    // drains the body, which on a standing stream never completes — so
    // `close()` aborts the call rather than merely leaving the loop.
    let abort = new AbortController();

    const tail: AsyncIterable<AgentTailFrame> = {
      async *[Symbol.asyncIterator]() {
        /**
         * Consecutive UNASKED ends of the store's stream that delivered
         * nothing. Any entry served resets it, because a tail that is still
         * carrying rows is not the wedged case {@link UNASKED_END_BUDGET}
         * bounds.
         */
        let barrenEnds = 0;
        for (;;) {
          if (stopped) return;
          /** Whether this attempt ended in the store's refused-token refusal. */
          let refused = false;
          try {
            for await (const push of client.watchAgentSession(
              create(storev1.WatchAgentSessionRequestSchema, { watch: token }),
              abort.signal,
            )) {
              if (stopped) return;
              let frame: AgentTailFrame;
              switch (push.frame.case) {
                case "line": {
                  const entry = toHistoryEntryAt(push.frame.value);
                  if (entry.at !== undefined) {
                    served.set(entry.at.value, lineFingerprint(push.frame.value));
                  }
                  frame = { case: "entry", value: entry };
                  break;
                }
                case "retired": {
                  const entry = toHistoryEntryAt(push.frame.value);
                  // A RETIREMENT IS SERVED, for the conclusion's purposes. The
                  // consumer has been handed the last word on this pointer --
                  // remove what you drew -- and the store will never serve the
                  // line again, so a conclusion naming it (the teardown's head
                  // was the retired row) must settle here rather than stand
                  // forever for a line that cannot come. It is recorded under
                  // RETIRED_CONTENT, not the line's fingerprint, so a later
                  // write that takes the row back at the same position is
                  // never withheld by the re-open's already-served check,
                  // whatever its content: the consumer removed it.
                  if (entry.at !== undefined) served.set(entry.at.value, RETIRED_CONTENT);
                  LOGGER.logVerbose(
                    { agent: agent.value, pointer: entry.at?.value },
                    "the store retired a line of an agent's book; relaying the retirement to the consumer",
                  );
                  frame = { case: "retired", value: entry };
                  break;
                }
                default:
                  // AN UNSET ARM IS ILLEGAL AT THE CONSUMER, loudly: a frame
                  // that is neither a line nor a retirement says nothing this
                  // tail can relay, and skipping it would hide a broken store.
                  throw new PersistenceError(
                    "store_unavailable",
                    "the store pushed a watch frame with no arm set",
                  );
              }
              // The retired pointer is as valid a high-water mark as a served
              // one: the store keeps the position in the book, so a re-open
              // from it is bounded exactly as from any other line.
              servedThrough = frame.value.at;
              yield frame;
              barrenEnds = 0;
              if (settled()) {
                stopped = true;
                abort.abort();
                return;
              }
            }
            if (stopped) return;
          } catch (error) {
            if (stopped) return;
            if (!isNotFound(error)) throw transportFailure(error);
            refused = true;
          }
          if (refused) {
            // THE REFUSED-OPEN CONVENTION: an unknown token means the store
            // forgot the session (a restart, a consumed token). Re-open from
            // the last pointer actually served — the only thing that makes
            // the recovery lossless — and carry on.
            // warn: a defect because the store invalidated a live watch token and forced a reopen.
            LOGGER.warn(
              { agent: agent.value, served_through: servedThrough?.value },
              "the store refused the watch token; re-opening the book from the last served pointer",
            );
          } else {
            // AN END NOBODY ASKED FOR TAKES THE SAME RECOVERY, AND SAYS SO.
            // Nothing on this side stopped the tail — `stopped` is false — so
            // the store ended a standing watch of its own accord. See
            // {@link UNASKED_END_BUDGET} for what that silence used to cost.
            barrenEnds += 1;
            if (barrenEnds > UNASKED_END_BUDGET) {
              LOGGER.error(
                {
                  agent: agent.value,
                  served_through: servedThrough?.value,
                  attempts: barrenEnds,
                  budget: UNASKED_END_BUDGET,
                  detail: "the store ended each re-opened watch without delivering a line",
                },
                "gave up re-opening an agent's tail: the store keeps ending a standing watch nothing asked it to end",
              );
              throw new PersistenceError(
                "store_unavailable",
                `the store ended the standing watch on ${agent.value} ${String(barrenEnds)} times in a row ` +
                  "without delivering a line; the tail cannot be kept standing",
              );
            }
            // info: an ordered store restart ends every standing watch once,
            // and this side recovers from it on its own budget. The record is
            // the lifecycle of that recovery -- the store went away, the book
            // is being re-opened from the last served pointer, attempt N of M
            // -- and it is a fault only once the budget above is spent, which
            // the ERROR there says.
            LOGGER.info(
              {
                agent: agent.value,
                served_through: servedThrough?.value,
                attempt: barrenEnds,
                budget: UNASKED_END_BUDGET,
              },
              "the store ended a standing watch that nothing asked it to end; re-opening the book from the last served pointer",
            );
          }
          // ON THE READ HALF'S OWN SCHEDULE. A store that ended every watch
          // because it is RESTARTING will refuse this open for as long as it is
          // down, and a single attempt would turn a restart the schedule is
          // there to absorb into a severed WatchAgent on every live session.
          //
          // WITH NO MARK THE RE-OPEN IS A REPAINT, and that is right only
          // because a mark is missing solely when the book held nothing at
          // the open: a tail-only open marks the store's `newest` as of the
          // open, so everything a markless book holds now was written since.
          const mark = servedThrough;
          const reopened = await onReadRetrySchedule("reopenAgentTail", agent, () =>
            openSession(agent, mark === undefined ? REPAINT : { case: "knownThrough", value: mark }),
          );
          if (reopened.watch === undefined) {
            throw new PersistenceError(
              "store_unavailable",
              "the store re-opened a reading session with no watch token",
            );
          }
          // The re-open's page is bounded by `known_through` and by the
          // store's own page size. A gap wider than one page is WALKED, page by
          // page, down to the mark — the store's own recovery contract — so
          // nothing written while the watch was down is skipped.
          const caughtUp = reopened.page === undefined ? [] : await walkToMark(agent, reopened.page, mark);
          // A LINE ALREADY SERVED UNCHANGED IS NOT SERVED AGAIN. The re-open's
          // lower bound is the LAST pointer served, which walks backward on an
          // upsert of an old row, so the catch-up page can carry rows the
          // consumer already holds -- a finished turn's prompt and terminal
          // among them. Those carry no turn of their own, and a consumer that
          // took them again ended the turn now running with the previous
          // turn's terminal. The bound is kept, so an update to an old row (a
          // new fingerprint at a served pointer) still passes.
          const fresh: { entry: conversationv1.HistoryEntryAt; fingerprint: string }[] = [];
          let replayed = 0;
          for (const line of [...caughtUp].reverse()) {
            const entry = toHistoryEntryAt(line);
            const fingerprint = lineFingerprint(line);
            if (entry.at !== undefined && served.get(entry.at.value) === fingerprint) {
              replayed += 1;
              continue;
            }
            fresh.push({ entry, fingerprint });
          }
          if (replayed > 0) {
            // info: part of the store-restart recovery the re-open records
            // above; the lines are withheld by design, and saying so keeps the
            // withholding visible.
            LOGGER.info(
              { agent: agent.value, replayed },
              "the re-opened book carried lines already served unchanged; they were not served again",
            );
          }
          for (const { entry, fingerprint } of fresh) {
            servedThrough = entry.at;
            if (entry.at !== undefined) served.set(entry.at.value, fingerprint);
            yield { case: "entry", value: entry };
            barrenEnds = 0;
            if (settled()) {
              stopped = true;
              abort.abort();
              return;
            }
          }
          token = reopened.watch;
          // A fresh controller per attempt: the aborted one stays aborted.
          abort = new AbortController();
        }
      },
    };

    return {
      page,
      foundNothing: newest === undefined,
      tail,
      concludeThrough: (through) => {
        if (stopped) return;
        concluding = true;
        concludeAt = through;
        // Nothing left to wait for: either no pointer was named, or the tail
        // has already served it. Ending now is the honest answer, and holding
        // the stream open for a row that will never come would wedge the exit.
        if (settled()) {
          stopped = true;
          abort.abort();
        }
      },
      close: () => {
        if (stopped) return;
        stopped = true;
        abort.abort();
      },
    };
  };

  /**
   * An ANNOUNCED book that the store holds no rows for yet: an empty opening
   * page now, and the tail stood the moment its first row lands.
   *
   * THE PRODUCER IS THE ARBITER (landing 7). The store refuses `unknown_agent`
   * until something has been written under an id, and the main agent's row is
   * created by its first write — so a daemon that opens `WatchAgent` on a fresh
   * session before any turn, which the endpoint contract tells it to do, races
   * that write and always loses. Refusing there closed the stream at the
   * transport and the daemon read it as a severed link on every bring-up.
   *
   * The wait is NOT open-ended: it holds only while `known` still says this
   * shim announced the agent, exactly as a shell run's wait holds only while
   * the run is still live. An id nobody announced is refused at once, and an
   * announcement withdrawn under a standing wait surfaces the store's own
   * refusal rather than waiting on for a row that can never come.
   */
  const deferredBook = (
    agent: conversationv1.AgentId,
    opening: AgentOpening,
    known: () => boolean,
  ): AgentPageSession => {
    /**
     * THE BOOK DID NOT EXIST AT THIS OPEN, so everything it holds once it does
     * was written since, and is owed. A tail-only opening therefore opens the
     * real book as a REPAINT: its page is entirely news, and a tail-only open
     * would pin the tail past the very rows this wait was for.
     */
    const innerOpening: AgentOpening = opening.case === "tailOnly" ? REPAINT : opening;
    const innerMark = opening.case === "knownThrough" ? opening.value : undefined;
    let closed = false;
    let concluded = false;
    let concludeAt: conversationv1.HistoryPointer | undefined;
    let inner: AgentPageSession | undefined;
    /**
     * EVERY pointer THIS WRAPPER has handed the consumer.
     *
     * IT IS TRACKED HERE BECAUSE THE INNER SESSION CANNOT SEE IT. The rows that
     * landed while this book was deferred are served out of the real session's
     * OPENING PAGE, by the loop below, so the inner session's own
     * `servedThrough` never learns about them — and its `concludeThrough` ends
     * the stream only when the pointer it is given is the one IT served. A
     * teardown concluding through the book's head therefore matched nothing,
     * the tail stood waiting for a row that had already gone out, and the
     * shim's `KillSession` spent its whole `WATCHER_CONCLUSION_BUDGET_MS` on
     * every session whose book was minted empty and then written to.
     * MEASURED against the real quartet: 1.02s from the host's stop to the
     * daemon's exit, against 5ms for the same stop on a session whose book was
     * never deferred, with `the WatchAgent tail on <agent> did not end within
     * its conclusion budget` at ERROR in the shim's log each time.
     *
     * IT IS A SET AND NOT THE LAST POINTER, for the reason the real session's
     * own `served` states: the store streams an upsert of an old row at its
     * ORIGINAL pointer, so the newest pointer handed over walks backward and
     * cannot answer "have I served the head".
     */
    const served = new Set<string>();

    /**
     * Whether the consumer has been handed everything the conclusion named.
     *
     * An unnamed pointer settles at once, exactly as the real session's own
     * `concludeThrough` reads it: there is nothing left to wait for.
     */
    const settled = (): boolean =>
      concluded && (concludeAt === undefined || served.has(concludeAt.value));

    /** The real session, once the book exists. `undefined` if it never will. */
    const openWhenWritten = async (): Promise<AgentPageSession | undefined> => {
      for (;;) {
        if (closed) return undefined;
        // A MINTED BOOK IS WAITED FOR, NEVER ASKED ABOUT. This session was
        // deferred BECAUSE no book can exist yet, so an ask here would earn the
        // refusal that built it a second time and learn nothing. The write that
        // ends the absence is this shim's own and wakes the wait, so there is
        // nothing for a recheck to discover in the meantime. A producer that no
        // longer vouches falls through to the ask deliberately: the store's own
        // refusal is what such a caller is owed.
        if (booksMinted.has(agent.value) && known()) {
          // A CONCLUDED TAIL WAITS FOR NOTHING. The teardown writes the terminal
          // it owes BEFORE concluding, so a book still absent here holds nothing
          // this consumer is owed and standing on would never end the stream.
          if (concluded) return undefined;
          await agentRows.wait(agent.value);
          continue;
        }
        try {
          // ON THE READ HALF'S SCHEDULE, for the reason the tail's re-open is:
          // a store restarting under a live shim refuses this open for as long
          // as its socket is down, and one attempt would turn a restart the
          // schedule absorbs into a deferred book that never opens.
          return await onReadRetrySchedule("openDeferredAgentBook", agent, () =>
            openBookNow(agent, innerOpening),
          );
        } catch (error) {
          if (!(error instanceof PersistenceError) || error.kind !== "unknown_agent") throw error;
          if (closed) return undefined;
          // A CONCLUDED TAIL WAITS FOR NOTHING, as above.
          if (concluded) return undefined;
          if (!known()) throw error;
          await agentRows.wait(agent.value);
        }
      }
    };

    return {
      // AN EMPTY BOOK IS AT ITS FLOOR: there are no older entries to walk to,
      // so `more` would point a consumer at a page that does not exist.
      page: create(conversationv1.HistoryPageSchema, {
        entries: [],
        boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
      }),
      foundNothing: true,
      tail: {
        async *[Symbol.asyncIterator]() {
          const session = await openWhenWritten();
          if (session === undefined) return;
          inner = session;
          if (closed) {
            session.close();
            return;
          }
          if (concluded) session.concludeThrough(concludeAt);
          // THE STORE TAIL IS DIALED BEFORE A SINGLE ENTRY GOES OUT. A consumer
          // that cancels must cancel the shim's own `WatchAgentSession` with
          // it, and a tail still unopened when entries are already flowing
          // would leave that subscription to be opened after the cancel.
          const rows = session.tail[Symbol.asyncIterator]();
          let pending = rows.next();
          // THE ROWS THAT LANDED WHILE WE WAITED ARE OWED AS TAIL ENTRIES: the
          // opening page this consumer already has was empty, so the real
          // session's page is entirely news — and so is every older store page
          // below it, down to the caller's own mark, which a backlog wider
          // than one page reaches only by walking. It is newest-first; the
          // tail is write order.
          let owed: conversationv1.HistoryEntryAt[];
          try {
            owed = await walkEntriesToMark(agent, session.page, innerMark);
          } catch (error) {
            session.close();
            throw error;
          }
          for (const entry of [...owed].reverse()) {
            if (entry.at !== undefined) served.add(entry.at.value);
            yield { case: "entry", value: entry };
            // THE CONCLUSION IS HONORED BY WHOEVER SERVED THE ROW. These
            // entries never pass through the inner session's tail, so only
            // this loop can know the named pointer has been handed over.
            if (settled()) {
              session.close();
              return;
            }
          }
          if (settled()) {
            session.close();
            return;
          }
          for (;;) {
            const next = await pending;
            if (next.done === true) {
              // THE INNER TAIL ENDS ONLY WHEN THIS WRAPPER ASKED IT TO. It
              // recovers from a store that ends a standing watch and throws
              // when it cannot, so a `done` here with neither a close nor a
              // conclusion behind it is an ending nobody ordered — the very
              // shape the daemon reads as a severed link, and the shape that
              // must never leave this process unrecorded again.
              if (!closed && !concluded) {
                LOGGER.error(
                  {
                    agent: agent.value,
                    detail: "the deferred book's inner tail finished with no close and no conclusion",
                  },
                  "an agent's tail ended without anything asking it to; the consumer's stream ends with the session still live",
                );
              }
              return;
            }
            pending = rows.next();
            // A retirement counts as served here exactly as in the inner
            // session (see its `retired` arm): the consumer has the last word
            // on that pointer, and the store will never serve it again.
            const at = next.value.value.at;
            if (at !== undefined) served.add(at.value);
            yield next.value;
            if (settled()) {
              session.close();
              return;
            }
          }
        },
      },
      concludeThrough(through) {
        concluded = true;
        concludeAt = through;
        // Wake the wait so a teardown does not sit out the recheck cadence.
        agentRows.wake(agent.value);
        // ALREADY SERVED IS ALREADY DONE, and the inner session cannot tell:
        // a conclusion naming a pointer this wrapper handed out of the opening
        // page leaves the inner tail parked on a row that will never come, so
        // it is ENDED here rather than concluded.
        if (settled()) {
          inner?.close();
          return;
        }
        inner?.concludeThrough(through);
      },
      close() {
        closed = true;
        agentRows.wake(agent.value);
        inner?.close();
      },
    };
  };

  /** One book, waiting out the first-write race when the producer vouches for it. */
  const openBook = async (
    agent: conversationv1.AgentId,
    opening: AgentOpening,
    known?: () => boolean,
  ): Promise<AgentPageSession> => {
    // THE BOOK CANNOT EXIST YET: this shim minted the id and has written
    // nothing under it. Asking anyway buys nothing but an `unknown_agent`
    // refusal in the store's log on a bring-up going exactly as the contract
    // says it should — the store is right to refuse, so it is not asked.
    if (known !== undefined && known() && booksMinted.has(agent.value)) {
      LOGGER.debug(
        { agent: agent.value },
        "this agent's id was minted here and nothing is written under it yet, so no book was asked for; serving an empty page and standing the tail on its first row",
      );
      return deferredBook(agent, opening, known);
    }
    try {
      // ON THE READ HALF'S SCHEDULE. This is the open a WatchAgent stands its
      // whole tail on, and a deploy that kickstarts the store under a live
      // shim used to fail it terminally on the first unreachable attempt.
      return await onReadRetrySchedule("openAgentBook", agent, () => openBookNow(agent, opening));
    } catch (error) {
      if (known === undefined || !(error instanceof PersistenceError)) throw error;
      if (error.kind !== "unknown_agent" || !known()) throw error;
      LOGGER.debug(
        { agent: agent.value },
        "the store holds no rows for this announced agent yet; serving an empty page and standing the tail on its first row",
      );
      return deferredBook(agent, opening, known);
    }
  };

  /**
   * The NEWEST page of one book, with no tail behind it.
   *
   * THE STORE IS ALWAYS ASKED, and that is the whole point. A watch defers a
   * minted-but-unwritten book without asking, because the answer is known and
   * what the open is worth is the TAIL it stands. A one-shot read stands no
   * tail: everything it has to say is the store's answer, so skipping the ask
   * saves no refusal — it invents one of the three outcomes this read must tell
   * apart. Asking separates them:
   *
   *   - the store answers → the page it served;
   *   - the store refuses the BOOK, while the producer still vouches for the
   *     agent → an empty page, because a session whose first row has not landed
   *     has an empty past and not an unknown one;
   *   - the store cannot be reached, or fails the read → that refusal, surfaced
   *     as it stands for the engine's typed arm.
   *
   * An `unknown_agent` in the store's log on a cold read is the cost, and it is
   * the right one: an empty page invented over an unreachable store tells a
   * consumer this conversation has no history. The write half spares the
   * common case: a read of a book whose first row is still queued waits for it
   * to land before it gets here (writer.ts `readFirstPage`), so only a book
   * with nothing written or queued is asked for early.
   */
  const readFirstPageOnce = async (
    agent: conversationv1.AgentId,
    opening: AgentOpening,
    known?: () => boolean,
  ): Promise<conversationv1.HistoryPage> => {
    let opened: storev1.OpenAgentSessionSuccess;
    try {
      // PAGE-ONLY: this read stands no tail, so it asks for no token. Nothing
      // is minted, so there is nothing to abandon — the reason the store's
      // registry no longer grows by one per one-shot read. A tail-only opening
      // is relayed as it stands: the store still answers for the book (and
      // for its own reachability) and serves no lines.
      opened = await openSession(agent, opening, true);
    } catch (error) {
      if (known === undefined || !(error instanceof PersistenceError)) throw error;
      if (error.kind !== "unknown_agent" || !known()) throw error;
      LOGGER.debug(
        { agent: agent.value },
        "the store holds no rows for this announced agent yet; serving an empty page for a read that stands no tail",
      );
      // AN EMPTY BOOK IS AT ITS FLOOR: there are no older entries to walk to.
      return create(conversationv1.HistoryPageSchema, {
        entries: [],
        boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
      });
    }
    if (opened.page === undefined) {
      throw new PersistenceError(
        "store_unavailable",
        "the store answered a page-only open with no page",
      );
    }
    // THE STORE ANSWERED FOR THE BOOK, so it holds a row for it, exactly as on
    // the watched open. A minted id whose book demonstrably exists is no longer
    // a certain absence.
    booksMinted.delete(agent.value);
    return toHistoryPage(opened.page);
  };

  /**
   * The one-shot read, ON THE RETRY SCHEDULE.
   *
   * A busy database is not an unreachable one. This read backs the history a
   * consumer opens with and the re-announcement a joining WatchSession gets, and
   * a single `SQLITE_BUSY` used to lose both.
   */
  const readFirstPage = (
    agent: conversationv1.AgentId,
    opening: AgentOpening,
    known?: () => boolean,
  ): Promise<conversationv1.HistoryPage> =>
    onReadRetrySchedule("readFirstPage", agent, () => readFirstPageOnce(agent, opening, known));

  /**
   * One older page, as the store answered it.
   *
   * Wrapped by the retried entry point below: an older page is as much a
   * one-shot read as the newest one, and a busy database is not an answer.
   */
  const readStorePageOnce = async (
    agent: conversationv1.AgentId,
    from: PageFrom,
  ): Promise<storev1.ReadAgentPageSuccess> => {
    // THE ARM IS THE POSITION, forwarded as the store spells it: `after` walks
    // below a served line, `through` reads the book as it stood at an instant.
    const position: storev1.ReadAgentPageRequest["position"] =
      from.case === "after"
        ? { case: "after", value: toStorePointer(from.value) }
        : { case: "through", value: from.value };
    let response: storev1.ReadAgentPageResponse;
    try {
      response = await client.readAgentPage(
        create(storev1.ReadAgentPageRequestSchema, {
          book: agent,
          position,
        }),
      );
    } catch (error) {
      // AS ON THE OPEN ABOVE: this read is replayed on the retry schedule, so
      // one unreachable attempt is the expected shape of a store restart and
      // the ERROR is the schedule running out.
      LOGGER.info(
        { agent: agent.value, detail: String(error) },
        "the store could not be reached to read an older page; replaying the read on the retry schedule",
      );
      throw transportFailure(error);
    }
    const result = response.result;
    if (result.case === "failure") {
      LOGGER.debug(
        { agent: agent.value, detail: result.value.detail },
        "the store refused an older page",
      );
      throw readFailure(result.value);
    }
    if (result.case !== "success") {
      throw new PersistenceError(
        "store_unavailable",
        "the store answered ReadAgentPage with no result arm set",
      );
    }
    LOGGER.debug(
      { agent: agent.value, entries: result.value.lines.length },
      "served an older page of an agent's book",
    );
    return result.value;
  };

  const readAgentPageOnce = async (
    agent: conversationv1.AgentId,
    from: PageFrom,
  ): Promise<conversationv1.HistoryPage> => {
    const read = await readStorePageOnce(agent, from);
    // EVERY LINE CARRIES ITS OWN POINTER (landing 3): a continuation page is
    // a reconnect mark like any other, so nothing here is minted and nothing
    // has to be refused if a caller echoes one back.
    return create(conversationv1.HistoryPageSchema, {
      entries: read.lines.map(toHistoryEntryAt),
      boundary: toHistoryBoundary(read.boundary),
    });
  };

  /**
   * The pointer a `more` boundary names, to walk on from. A `more` naming none
   * is a store contradicting itself, refused loudly.
   */
  const walkFrom = (lastItem: storev1.StoreItemPointer | undefined): conversationv1.HistoryPointer => {
    if (lastItem === undefined) {
      throw new PersistenceError(
        "store_unavailable",
        "the store said older lines remain but named no pointer to walk from",
      );
    }
    return toHistoryPointer(lastItem);
  };

  /**
   * Every store page older than `after`, newest first, down to — never
   * including — `mark`, the caller's own high-water mark; to the floor when
   * there is none.
   *
   * A PAGE IS THE STORE'S PAGE, so a catch-up wider than one page arrives with
   * `more` pointing into the gap. The store's contract for that is the walk:
   * older pages until the caller meets its own mark. Each page is read on the
   * read half's retry schedule, exactly as a single read is.
   */
  const walkOlder = async (
    agent: conversationv1.AgentId,
    after: conversationv1.HistoryPointer,
    mark: conversationv1.HistoryPointer | undefined,
  ): Promise<storev1.StoreLineAt[]> => {
    const lines: storev1.StoreLineAt[] = [];
    let from: conversationv1.HistoryPointer | undefined = after;
    let pages = 0;
    while (from !== undefined) {
      const position: conversationv1.HistoryPointer = from;
      const older: storev1.ReadAgentPageSuccess = await onReadRetrySchedule("walkAgentBook", agent, () =>
        readStorePageOnce(agent, { case: "after", value: position }),
      );
      pages += 1;
      if (older.lines.length === 0 && older.boundary.case === "more") {
        // A `more` THAT LEADS TO NOTHING is a store contradicting itself, and
        // walking on from it would spin; it is surfaced, never treated as the
        // floor it did not say.
        throw new PersistenceError(
          "store_unavailable",
          "the store said older lines remain and then served an empty page below them",
        );
      }
      for (const line of older.lines) {
        if (mark !== undefined && line.at?.value === mark.value) {
          LOGGER.debug(
            { agent: agent.value, pages, lines: lines.length },
            "walked an agent's book down to the caller's mark",
          );
          return lines;
        }
        lines.push(line);
      }
      from = older.boundary.case === "more" ? walkFrom(older.boundary.value.lastItem) : undefined;
    }
    LOGGER.debug({ agent: agent.value, pages, lines: lines.length }, "walked an agent's book down to its floor");
    return lines;
  };

  /** An opened store page's lines and every older page's, down to `mark` ({@link walkOlder}). */
  const walkToMark = async (
    agent: conversationv1.AgentId,
    opened: storev1.AgentSessionPage,
    mark: conversationv1.HistoryPointer | undefined,
  ): Promise<storev1.StoreLineAt[]> =>
    opened.boundary.case === "more"
      ? [...opened.lines, ...(await walkOlder(agent, walkFrom(opened.boundary.value.lastItem), mark))]
      : [...opened.lines];

  /** {@link walkToMark}, from a page already converted to history entries. */
  const walkEntriesToMark = async (
    agent: conversationv1.AgentId,
    opened: conversationv1.HistoryPage,
    mark: conversationv1.HistoryPointer | undefined,
  ): Promise<conversationv1.HistoryEntryAt[]> => {
    if (opened.boundary.case !== "more") return [...opened.entries];
    const after = opened.boundary.value.lastEntry;
    if (after === undefined) {
      throw new PersistenceError(
        "store_unavailable",
        "the store said older lines remain but named no pointer to walk from",
      );
    }
    return [...opened.entries, ...(await walkOlder(agent, after, mark)).map(toHistoryEntryAt)];
  };

  return {
    openAgentPage(agent, opening, known) {
      return openBook(agent, opening, known);
    },

    readFirstPage(agent, opening, known) {
      return readFirstPage(agent, opening, known);
    },

    readAgentPage(agent, after) {
      return onReadRetrySchedule("readAgentPage", agent, () =>
        readAgentPageOnce(agent, { case: "after", value: after }),
      );
    },

    readPageThrough(agent, through) {
      return onReadRetrySchedule("readPageThrough", agent, () =>
        readAgentPageOnce(agent, { case: "through", value: through }),
      );
    },
    async openBashRun(work, { awaitFirstRow }) {
      // THE HANDLE IS THE RUN (ruling, landing 3): `DetachedWorkId.value ==
      // AgentActivityId.value`, the spawning call's own `tool_use_id`. So there
      // is no side table to consult and no way for a lookup to go stale — and a
      // handle the store holds no row for is refused by the store itself.
      const runValue = work.value;
      if (runValue === "") {
        throw new PersistenceError("unknown_work", "a detached-work handle is never the empty string");
      }
      // ONE PATH FOR EVERY RUN, and it is the STORE's. A detached shell's output
      // is written by the SIDECAR as deltas — no SDK route carries a byte of it
      // — so serving the run from what this shim happened to observe would show
      // a command's start and its ending with the whole middle missing.
      // `WatchBashRun` replays every stored row in write order and then follows,
      // which is exactly what a watcher of a growing spool needs, and it is the
      // same path whether this shim wrote the rows or the sidecar did.
      const run = create(conversationv1.AgentActivityIdSchema, { value: runValue });
      const abort = new AbortController();
      LOGGER.debug({ run: runValue, work: work.value, await_first_row: awaitFirstRow }, "following a shell run's stored rows");
      let opened = false;
      return {
        async *[Symbol.asyncIterator]() {
          try {
            // A RUN THIS SHIM HOLDS LIVE IS WAITED ON; ANY OTHER IS REFUSED.
            // A live run's rows can all be the sidecar's, written once it reads
            // the run's spool — for a shell launched inside a backgrounded
            // subagent this shim never saw the call, so there is no start of
            // its own to write — and refusing it then left the run with no
            // watch able to see its terminal (2026-09-28). The caller vouches
            // (`awaitFirstRow`) only for a run in its live set, which the
            // vendor's own notification or level retires, so the wait is
            // bounded by the run's life. A run it does not hold live is refused
            // as the contract says: the store holding no row means none exists.
            for await (const push of client.watchBashRun(
              create(storev1.WatchBashRunRequestSchema, { run, awaitFirstRow }),
              abort.signal,
            )) {
              opened = true;
              const frame = push.row?.frame;
              if (frame === undefined) {
                throw new PersistenceError(
                  "store_unavailable",
                  "the store pushed a bash row with no frame",
                );
              }
              yield frame;
            }
          } catch (error) {
            if (isNotFound(error)) {
              // A REFUSED OPEN means the store holds no row for this run. It is
              // an `unknown_work` refusal, not a transport failure, so the
              // caller can say so rather than reporting the store as broken.
              LOGGER.debug(
                { run: runValue, work: work.value },
                "the store holds no rows for this shell run; the watch is refused",
              );
              throw new PersistenceError(
                "unknown_work",
                `the store holds no rows for shell run ${JSON.stringify(runValue)}`,
              );
            }
            throw transportFailure(error);
          } finally {
            // CANCELLING THE CALL IS HOW A STREAM ENDS EARLY: a consumer that
            // breaks out of the loop would otherwise leave the call draining a
            // body that has not finished.
            if (opened || !abort.signal.aborted) abort.abort();
          }
        },
      };
    },

    noteBashFrame(runValue, frame) {
      // NOTHING TO RELAY OR WAKE. A run is served from the store's own rows,
      // so a frame this shim wrote reaches a watcher the same way the sidecar's
      // do — through `WatchBashRun`. Kept as the writer's one observation point
      // so a frame that never reached the store is visible in the log.
      LOGGER.logVerbose(
        { run: runValue, arm: frame.result.case },
        "wrote a shell run's lifecycle row; watchers read it back from the store",
      );
    },

    noteAgentRows(agents) {
      // THE FIRST-ROW SIGNAL for a book. A watcher that opened before this
      // agent had any row is blocked on exactly this commit having LANDED, so
      // waking it here is what makes that wait a synchronization and not a poll.
      for (const agent of agents) {
        if (agent === "") continue;
        // THE ABSENCE IS OVER: this batch is the write that registers the book,
        // so the next open is an ask that can succeed rather than one that is
        // known to refuse.
        booksMinted.delete(agent);
        agentRows.wake(agent);
      }
    },

    noteAgentMinted(agentValue) {
      // An empty id names no agent and could only ever be an ErrInvalid at the
      // store; recording absence for it would let a caller defer forever on it.
      if (agentValue === "") return;
      booksMinted.add(agentValue);
      LOGGER.logVerbose(
        { agent: agentValue },
        "this shim minted the agent id; its book exists only once the first write under it lands",
      );
    },
  };
}
