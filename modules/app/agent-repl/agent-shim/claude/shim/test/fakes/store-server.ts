/**
 * test/fakes/store-server.ts — an IN-PROCESS store.v1 server.
 *
 * # What it is for
 *
 * The shim's store client, writer, reader and reconciler all need a store that
 * ANSWERS, and the real one is a Go process the shim's suite may not require
 * (an integration test involving another real system is an e2e test, which this
 * is not). This is the store reduced to its observable contract: in-memory
 * tables, the same seven rpcs, the same refusal shapes.
 *
 * It is deliberately shared by the store-client agent and the integration-tests
 * agent, so both are working against ONE notion of how the store behaves and a
 * disagreement between them is a disagreement about this file rather than a
 * mystery at merge time.
 *
 * # The semantics it reproduces, and why each matters
 *
 *   - UPSERT BY KEY. A row re-sent under the same `upsert_key` REPLACES the
 *     row in place and keeps its original pointer. That is what makes a unit
 *     that starts, updates and ends appear ONCE in a book, and it is the single
 *     behavior most shim code depends on.
 *   - EVERY LINE HAS A CONVERSATION PLACE, AND PAGES ARE IN DESCENDING PLACE.
 *     A row keeps its FIRST stated `StoreEntry.place` (served as
 *     `recorded_place`); a row stored with none is served at its receipt
 *     instant (`received_place`, ordinal 0) until a later write states one.
 *     This fake's receipt instant is the latest instant it has placed any row
 *     at, so an unplaced row sorts where it arrived. Ties break by pointer.
 *     `more.last_item` points at the page's last (lowest-placed) line — the
 *     pointer the next `ReadAgentPage` echoes to walk on.
 *   - `ReadAgentPage.after` serves the lines placed strictly before the named
 *     line's CURRENT place; `through` serves the newest lines placed at or
 *     before an instant, and refuses a book with no agent row as
 *     `unknown_agent`; an unset position is `invalid_request`.
 *   - `known_through` IS THE CALLER'S HIGH-WATER MARK. The store remembers
 *     nothing about what it served; a set pointer means "only items FIRST
 *     WRITTEN after this one", ordered by descending place.
 *   - THE TAIL IS PURE: it carries WRITES, never replays. Opening a reading
 *     session delivers nothing to the tail, so the opening page is never
 *     repeated and open-then-watch is race-free. But EVERY write is a tail
 *     line, INCLUDING an upsert of a row the opening page already carried: a
 *     write supersedes the row whole, and a unit that started before the
 *     reader opened and settles afterwards is a real change served at the
 *     row's own stable pointer. Dropping those was what made a mid-turn
 *     subscriber never learn how the turn's earlier units ended.
 *   - AN UNKNOWN WATCH TOKEN IS A CONNECT `NotFound`. A refused stream open has
 *     no failure message to live in — the response type is the frame it
 *     streams — so the refusal closes the stream at the transport.
 *   - `WatchBashRun` REPLAYS EVERY STORED ROW of a run in FIRST-INSERT order —
 *     one row per upsert key, holding its newest write, as the real store's
 *     `ON CONFLICT(upsert_key)` does — then follows every write, and ENDS
 *     after the terminal row. A run with no stored row is a
 *     refused open — closed at the transport, like every other watch here —
 *     unless the request sets `await_first_row`, when it waits for the first.
 *   - A WRITE ID IS APPLIED ONCE. A batch entry whose `write_id` already
 *     landed is ABSORBED — it lands nothing, supersedes nothing and reaches no
 *     watcher — as the real store's write ledger absorbs it. A producer that
 *     re-sends a write (the shim re-writes a shell run's start to make it
 *     durable) must not regress a row another write has since superseded.
 *   - A RUN, ONCE ENDED, STAYS ENDED. `GetLiveWork` stops listing detached
 *     work the moment any terminal bash row lands for its run, and a later
 *     non-terminal row (a sidecar tail landing late) does not list it again,
 *     as the real store's `detached_work.ended_at_ms` is only ever set.
 *   - WRITES CAN BE MADE TO FAIL ON DEMAND ({@link FakeStore.failWrites}), which
 *     is how the writer's retry buffer is testable at all.
 */
import { clone, create } from "@bufbuild/protobuf";
import { Code, ConnectError, type ConnectRouter } from "@connectrpc/connect";
import { connectNodeAdapter } from "@connectrpc/connect-node";
import { unlinkSync } from "node:fs";
import http from "node:http";
import { conversationv1, storev1 } from "../../src/proto.js";

/** One stored line, with the identity and position the store gave it. */
interface StoredRow {
  /** The store-minted pointer. Stable across upserts of the same key. */
  readonly pointer: string;
  /** The key the row upserts on. */
  readonly upsertKey: string;
  /** The line itself, replaced whole on every upsert. */
  line: storev1.StorePageLine;
  /** The row's FIRST turn stamp, kept across upserts as the real store keeps it. */
  turn: conversationv1.TurnId | undefined;
  /**
   * Where the row sits in its conversation, and who established it: the
   * row's first stated place, or its receipt instant standing in.
   */
  place: { atMs: bigint; ordinal: number; recorded: boolean };
}

/** An opened reading session: which book, and where its tail begins. */
interface WatchSessionState {
  readonly book: string;
  /** Lines waiting to be pulled by the tail, oldest first. */
  readonly pending: storev1.StoreLineAt[];
  /** Resolvers of pulls that arrived before any line did. */
  readonly waiters: Array<(line: storev1.StoreLineAt) => void>;
  /** Set when the store is closing, so a parked tail ends instead of hanging. */
  closed: boolean;
}

/** The running fake, and the levers a test pulls on it. */
export interface FakeStore {
  /** The unix socket it is listening on. */
  readonly socketPath: string;
  /** Stop serving, end every open tail, and remove the socket file. */
  close(): Promise<void>;
  /**
   * Make every subsequent WriteBatch answer a `failure` with this detail under
   * the `storage_failure` arm, or pass `null` to accept writes again. The retry
   * buffer has no other way to be exercised: a real store fails when it is
   * down, and a test cannot take one down that it did not start.
   *
   * `storage_failure` is the RETRYABLE arm and therefore the right default for
   * this lever; {@link FakeStore.failWritesWith} names the other one.
   */
  failWrites(detail: string | null): void;
  /**
   * Make every subsequent WriteBatch answer a `failure` under a NAMED arm.
   *
   * The two arms mean opposite things to the writer — `invalid_request` says
   * the same bytes can never be accepted and a retry is pointless, while
   * `storage_failure` says a retry may succeed — so a writer that treated them
   * alike would either spin forever on a malformed batch or drop a transient
   * one. Pass `null` to accept writes again.
   */
  failWritesWith(arm: StoreWriteFailureArm | null, detail: string): void;
  /**
   * Make every subsequent read of `verb` answer a `failure` under a NAMED arm,
   * or pass `null` for the arm to serve that verb again.
   *
   * THE FAKE MUST BE ABLE TO REFUSE. Every typed refusal the store declares is
   * a branch of the shim's reader, and a fake that only ever succeeds leaves
   * those branches asserted nowhere: `stale_pointer` could be declared and
   * never observed. `detail` is deliberately the caller's to choose so a test
   * can pair an arm with CONTRADICTING prose and catch a consumer that
   * classifies by substring.
   *
   * `unknown_agent` is NOT settable: the fake refuses it on its own for a book
   * it holds no rows for. Write a row to make an agent known.
   *
   * `GetLiveWork` declares `invalid_request` and `storage_failure`; any other
   * arm is refused here rather than fabricating a response shape the proto
   * forbids.
   */
  failReads(verb: StoreReadVerb, arm: StoreReadFailureArm | null, detail?: string): void;
  /**
   * The watch tokens whose `WatchAgentSession` tail is OPEN right now.
   *
   * A client that stops reading an agent must CANCEL its tail, not merely stop
   * pulling: a store holding a stream open for a reader that will never return
   * leaks a subscription per closed watch. Nothing on the client says whether
   * it cancelled, so the only place that fact is observable is here.
   */
  openTails(): readonly string[];
  /**
   * The token of an OPEN tail, awaiting one if none is open yet.
   *
   * The tail's open crosses a socket too: a client that has already been served
   * a row is not proof its `WatchAgentSession` generator has begun on this
   * side, so {@link openTails} read as a level races the store. This is the
   * edge — the only sound way to assert on a tail that is expected to exist.
   */
  tailOpened(): Promise<string>;
  /**
   * Resolves the moment the tail on `token` closes, or immediately if it is
   * already gone.
   *
   * The cancellation crosses a real socket, so the client returning from its
   * iterator and the server's generator unwinding are two different instants.
   * This is the second one, awaitable — a test that polled or slept instead
   * would be asserting on a schedule rather than on the event.
   */
  tailClosed(token: string): Promise<void>;
  /** Every WriteBatch request the store received, in order, failures included. */
  writes(): storev1.WriteBatchRequest[];
  /**
   * The same batches with the store's VERDICT on each, in order.
   *
   * `writes()` cannot tell a refused batch from an accepted one, so "a write id
   * repeats only because a refused batch was resent" was unassertable: every
   * duplicate looked alike. The verdict is the discriminator.
   */
  writeBatches(): readonly FakeStoreWrite[];
  /** Every shell run claim written, as `task id -> run`, in write order. */
  shellRunClaims(): readonly string[];
  /** One book's rows, oldest first — the store's own order, for assertions. */
  book(agentId: string): storev1.StoreLineAt[];
  /** Every session-update row written, in order. */
  sessionUpdates(): conversationv1.SessionUpdate[];
  /** Every unserved item written, in order. */
  unserved(): storev1.StoreUnservedItem[];
  /**
   * Resolves with the first LANDED entry `matches` accepts, or at once if one
   * already has — the awaitable form of "this row reached the store", for a
   * test that must then read which arm it landed on.
   */
  entryLanded(matches: (entry: storev1.StoreEntry) => boolean): Promise<storev1.StoreEntry>;
  /**
   * Every READ verb the store served, in order.
   *
   * `writes()` made the write plane observable and the read plane had no
   * counterpart, so "GetLiveWork is called exactly once, at session start"
   * could only be declared and never asserted. This is that lever.
   */
  reads(): FakeStoreRead[];
}

/** A read verb that can be made to refuse. */
export type StoreReadVerb = "OpenAgentSession" | "ReadAgentPage" | "GetLiveWork";

/**
 * The typed refusal arms a read can be MADE to carry, as the protos declare
 * them.
 *
 * `unknown_agent` is deliberately absent: the fake refuses it INTRINSICALLY,
 * for any agent it holds no row for, exactly as the real store does (landing
 * 7). Modeling it as a switch let a test open a book on a store that had never
 * heard of the agent and get a page back — the very state the production
 * store cannot be in, and the one the fresh-session `WatchAgent` bug lived in.
 */
export type StoreReadFailureArm = "invalid_request" | "stale_pointer" | "storage_failure";

/** The typed refusal arms `WriteBatch` declares. */
export type StoreWriteFailureArm = "invalid_request" | "storage_failure";

/** One batch the fake received, and whether it took it. */
export interface FakeStoreWrite {
  readonly request: storev1.WriteBatchRequest;
  readonly accepted: boolean;
}

/** One read the fake served: which verb, and what was asked. */
export interface FakeStoreRead {
  readonly rpc:
    | "GetLiveWork"
    | "GetAgentByVendorTask"
    | "OpenAgentSession"
    | "ReadAgentPage"
    | "WatchBashRun"
    | "GetSidecarCursors";
  readonly request: unknown;
}

/** The activity arms the real store's `activityIsTerminal` reads as a unit's end. */
const TERMINAL_ACTIVITY_ARMS: ReadonlySet<string> = new Set(["success", "failure", "ended", "blocking_error"]);

/**
 * The fake's page size when a test names none: the real store's (its
 * `db.PageSize`), so a suite that never thinks about pages sees the pages
 * production serves.
 */
export const FAKE_STORE_PAGE_SIZE = 50;

/** How a test shapes the fake. */
export interface FakeStoreOptions {
  /**
   * The lines one page holds. THE STORE OWNS THE PAGE SIZE and no request can
   * state one, so this is the fake's own setting, never a request field: a
   * test that must cross a page boundary shrinks it rather than seeding fifty
   * rows, and a shim that assumed any particular size breaks against it.
   */
  readonly pageSize?: number;
}

/** Start the fake store on `socketPath`. Resolves once it is accepting. */
export async function startFakeStore(socketPath: string, options: FakeStoreOptions = {}): Promise<FakeStore> {
  const pageSize = options.pageSize ?? FAKE_STORE_PAGE_SIZE;
  /** Every agent the fake holds an `agent` row for — the store's `agent` table. */
  const knownAgents = new Set<string>();
  const books = new Map<string, StoredRow[]>();
  const rowsByKey = new Map<string, StoredRow>();
  const watches = new Map<string, WatchSessionState>();
  const receivedWrites: storev1.WriteBatchRequest[] = [];
  /** Every entry an ACCEPTED batch landed, in order, and who is waiting on one. */
  const landedEntries: storev1.StoreEntry[] = [];
  const landedWaiters = new Set<(entry: storev1.StoreEntry) => void>();
  /** The same batches with the verdict — see {@link FakeStore.writeBatches}. */
  const writeVerdicts: FakeStoreWrite[] = [];
  const sessionUpdateRows: conversationv1.SessionUpdate[] = [];
  const unservedRows: storev1.StoreUnservedItem[] = [];
  /** Every read verb served, in order — see {@link FakeStore.reads}. */
  const servedReads: FakeStoreRead[] = [];
  const noteRead = (rpc: FakeStoreRead["rpc"], request: unknown): void => {
    servedReads.push({ rpc, request });
  };
  /** Detached work ids that were announced, mapped to the run they detached from. */
  const detachedAnnounced = new Map<string, string | undefined>();
  /**
   * The store's LINEAGE COLUMNS: which agent announced each detached work
   * (`detached_work.owner_agent`) and which agent spawned each created agent
   * (`agent.spawned_by_agent`), in first-sight order. `GetLiveWork` answers
   * one session's lineage from these and from nothing else, exactly as
   * `sessionLineageCTE` does in the real store.
   */
  const detachedOwner = new Map<string, string>();
  const spawnedBy = new Map<string, string>();
  /** The commission each spawned agent's recorded start states, by agent: the `agent` row's columns. */
  const commissions = new Map<string, conversationv1.AgentSubagentPrompt>();
  /**
   * The store's `vendor_task` table: each vendor task locator, with every agent
   * a batch's `agent_locators` paired it with. `GetAgentByVendorTask` answers
   * from these within one session's lineage, exactly as the real store does.
   */
  const vendorTasks = new Map<string, Set<string>>();
  /** Every shell run claim a batch stated, as `task id -> run`. */
  const runClaims: string[] = [];
  /**
   * Every bash row per run, ONE PER UPSERT KEY, in first-insert order, each
   * holding its newest write — the real store's row model. A run's output is
   * one rendered-tail row every write supersedes (owner ruling 2026-09-23:
   * output beyond what is rendered is not stored), so a replay serves the
   * newest window, never every window the run passed through.
   */
  const bashRowsByRun = new Map<string, Array<{ readonly key: string; row: storev1.StoreAgentBash }>>();
  /** Runs a terminal bash row has ENDED — what `GetLiveWork` reads; never cleared. */
  const bashEndedRuns = new Set<string>();
  /**
   * Detached work a unit's TERMINAL ACTIVITY ARM ended (`success`, `failure`,
   * `ended`, `blocking_error`, exactly the arms the real store's
   * `activityIsTerminal` reads). Its `closeDetachedByOrigin` ends every row
   * ALREADY announced with that unit as its origin, so a spawn's or a
   * monitor's own terminal ends its detached work; a row announced later is a
   * new live row. Never cleared.
   */
  const endedByUnit = new Set<string>();
  /** Every write id an accepted batch APPLIED: the store's write ledger. */
  const appliedWriteIds = new Set<string>();
  /** Tails following one run's rows. */
  const bashWatchers = new Set<{
    readonly run: string;
    readonly pending: storev1.StoreAgentBash[];
    readonly waiters: Array<(row: storev1.StoreAgentBash | null) => void>;
    closed: boolean;
  }>();
  let nextPointer = 1;
  let nextToken = 1;
  let writeFailure: { arm: StoreWriteFailureArm; detail: string } | null = null;
  /** Which read verbs are currently refusing, and under which arm. */
  const readFailures = new Map<StoreReadVerb, { arm: StoreReadFailureArm; detail: string }>();
  /** Watch tokens whose tail is streaming right now — see {@link FakeStore.openTails}. */
  const openTailTokens = new Set<string>();
  /** Whoever is waiting for a given tail to unwind — see {@link FakeStore.tailClosed}. */
  const tailClosedWaiters = new Map<string, Array<() => void>>();
  /** Whoever is waiting for ANY tail to open — see {@link FakeStore.tailOpened}. */
  const tailOpenWaiters: Array<(token: string) => void> = [];

  /**
   * The refusal standing against one verb, if any.
   *
   * Each verb's arms are DIFFERENT MESSAGE TYPES that merely share names, so
   * every rpc builds its own `kind` from this rather than sharing one builder —
   * there is no cast that would let a `ReadAgentPage` arm ride on an
   * `OpenAgentSession` failure.
   */
  const refusalFor = (
    verb: StoreReadVerb,
  ): { arm: StoreReadFailureArm; detail: string } | undefined => readFailures.get(verb);

  const pointerOf = (row: StoredRow): storev1.StoreItemPointer =>
    create(storev1.StoreItemPointerSchema, { value: row.pointer });

  const lineAt = (row: StoredRow): storev1.StoreLineAt => {
    const place = create(conversationv1.ConversationPlaceSchema, {
      atMs: row.place.atMs,
      ordinal: row.place.ordinal,
    });
    return create(storev1.StoreLineAtSchema, {
      at: pointerOf(row),
      line: row.line,
      ...(row.turn === undefined ? {} : { turn: row.turn }),
      place: row.place.recorded
        ? { case: "recordedPlace", value: place }
        : { case: "receivedPlace", value: place },
    });
  };

  /** The latest instant any row was placed at: this fake's receipt clock. */
  let latestPlacedAt = 1n;

  /** Ascending conversation order of two rows: place, then pointer. */
  const placeOrder = (a: StoredRow, b: StoredRow): number => {
    if (a.place.atMs !== b.place.atMs) return a.place.atMs < b.place.atMs ? -1 : 1;
    if (a.place.ordinal !== b.place.ordinal) return a.place.ordinal - b.place.ordinal;
    return Number(a.pointer) - Number(b.pointer);
  };

  /** A book's rows in ascending conversation place. */
  const placedRowsOf = (agentId: string): StoredRow[] => [...rowsOf(agentId)].sort(placeOrder);

  /**
   * Deliver a line to every tail watching its book.
   *
   * EVERY WRITE IS SERVED, INCLUDING AN UPSERT OF A ROW OLDER THAN THE WATCH.
   * A write supersedes the row WHOLE, so a unit that started before the reader
   * opened and settles afterwards is a genuine change the tail must carry —
   * served, per store.v1, at the row's own (stable, original) pointer. An
   * earlier version of this fake compared that pointer against a pin taken at
   * open and silently dropped exactly those settle frames, so a reader that
   * subscribed mid-turn never learned how the turn's earlier units ended.
   *
   * The tail is still PURE: nothing is delivered without a write, so the
   * opening page is never replayed and open-then-watch stays race-free.
   */
  const fanOut = (bookId: string, row: StoredRow): void => {
    for (const state of watches.values()) {
      if (state.book !== bookId) continue;
      const waiter = state.waiters.shift();
      if (waiter !== undefined) waiter(lineAt(row));
      else state.pending.push(lineAt(row));
    }
  };

  /** Land one page line, upserting by key and keeping first-insert position. */
  const upsertPageLine = (
    upsertKey: string,
    line: storev1.StorePageLine,
    turn: conversationv1.TurnId | undefined,
    stated: conversationv1.ConversationPlace | undefined,
  ): void => {
    const bookId = line.book.case === "pageAgentId" ? (line.book.value.value ?? "") : "";
    if (stated !== undefined && stated.atMs > latestPlacedAt) latestPlacedAt = stated.atMs;
    const existing = rowsByKey.get(upsertKey);
    if (existing !== undefined) {
      existing.line = line;
      existing.turn ??= turn;
      // THE FIRST STATED PLACE STANDS; a row with none takes the first stated.
      if (!existing.place.recorded && stated !== undefined) {
        existing.place = { atMs: stated.atMs, ordinal: stated.ordinal, recorded: true };
      }
      fanOut(bookId, existing);
      return;
    }
    const row: StoredRow = {
      pointer: String(nextPointer++),
      upsertKey,
      line,
      turn,
      place:
        stated === undefined
          ? { atMs: latestPlacedAt, ordinal: 0, recorded: false }
          : { atMs: stated.atMs, ordinal: stated.ordinal, recorded: true },
    };
    rowsByKey.set(upsertKey, row);
    const book = books.get(bookId) ?? [];
    book.push(row);
    books.set(bookId, book);
    fanOut(bookId, row);
  };

  /** Remember what an entry says about live work, for GetLiveWork. */
  const recordLiveness = (line: storev1.StorePageLine): void => {
    const item = line.agentItem?.item;
    if (item?.case !== "agentFrame") return;
    const frame = item.value;
    if (frame.result.case === "update" && frame.result.value.update.case === "activity") {
      const activity = frame.result.value.update.value;
      const unit = activity.activityId?.value;
      const arm = (activity.item.value as { result?: { case?: string } } | undefined)?.result?.case;
      if (unit !== undefined && unit !== "" && arm !== undefined && TERMINAL_ACTIVITY_ARMS.has(arm)) {
        for (const [workId, runId] of detachedAnnounced) {
          if (runId === unit) endedByUnit.add(workId);
        }
      }
      return;
    }
    if (frame.result.case !== "detachedWork") return;
    const detached = frame.result.value;
    const workId = detached.work?.value;
    if (workId === undefined || workId === "") return;
    // THE ORIGIN UNIT, as the real store's `announceDetachedWork` takes it: the
    // unit a `detached` origin names, or the created agent of a `created`
    // subagent start.
    const created =
      detached.origin.case === "created" && detached.origin.value.workCreated?.work.case === "subagent"
        ? detached.origin.value.workCreated.work.value.result
        : undefined;
    const runId =
      detached.origin.case === "detached"
        ? detached.origin.value.detachedFromId?.value
        : created?.case === "start"
          ? created.value.createdAgentId?.value
          : undefined;
    detachedAnnounced.set(workId, runId);
    const owner = frame.agentId?.value;
    if (owner !== undefined && owner !== "") detachedOwner.set(workId, owner);
  };

  /** Every agent in `session`'s lineage: the main agent and all it spawned, transitively. */
  const lineageOf = (session: string): Set<string> => {
    const lineage = new Set<string>([session]);
    let grew = true;
    while (grew) {
      grew = false;
      for (const [created, spawner] of spawnedBy) {
        if (lineage.has(spawner) && !lineage.has(created)) {
          lineage.add(created);
          grew = true;
        }
      }
    }
    return lineage;
  };

  /**
   * Record FIRST SIGHT of an agent, exactly where `db.ensureAgent` does.
   *
   * A prompt addressed to an agent, any frame the agent itself emitted, and the
   * `created_agent_id` a subagent start names — nothing else. A session update,
   * an unserved item and a bash row create no agent row in the real store, so
   * they create none here either.
   */
  const rememberAgent = (agentId: string | undefined): void => {
    if (agentId === undefined || agentId === "") return;
    knownAgents.add(agentId);
  };

  /** Every agent the fake has an `agent` row for, by id. */
  const registerFromLine = (line: storev1.StorePageLine): void => {
    const item = line.agentItem?.item;
    if (item?.case === "agentPrompt") {
      rememberAgent(item.value.agent?.value);
      return;
    }
    if (item?.case !== "agentFrame") return;
    const frame = item.value;
    rememberAgent(frame.agentId?.value);
    if (frame.result.case !== "update") return;
    const update = frame.result.value.update;
    if (update.case !== "activity") return;
    const activity = update.value.item;
    if (activity.case !== "subagent") return;
    const subagent = activity.value.result;
    if (subagent.case !== "start") return;
    // `createSpawnedAgent`: the created agent's book is addressable from the
    // spawn frame on, before a single frame of its own has arrived.
    const created = subagent.value.createdAgentId?.value;
    rememberAgent(created);
    if (created !== undefined && created !== "" && subagent.value.prompt !== undefined) {
      commissions.set(created, subagent.value.prompt);
    }
    const spawner = frame.agentId?.value;
    if (created !== undefined && created !== "" && spawner !== undefined && spawner !== "") {
      spawnedBy.set(created, spawner);
    }
  };

  /**
   * Place an OWNER-UNKNOWN line (store.v1 StorePageLineOwnerUnknown) as the real
   * store does: in the book of the row already holding its upsert key, with the
   * frame attributed to that book -- or not at all, answered unplaced.
   */
  const placeUnownedLine = (
    upsertKey: string,
    line: storev1.StorePageLine,
  ): storev1.StorePageLine | storev1.WriteBatchUnplacedEntry => {
    const held = rowsByKey.get(upsertKey);
    const book = held?.line.book.case === "pageAgentId" ? held.line.book.value : undefined;
    if (book === undefined) {
      return create(storev1.WriteBatchUnplacedEntrySchema, {
        upsertKey,
        detail: "no stored row holds this upsert_key, so an owner_unknown write names no book",
      });
    }
    const placed = clone(storev1.StorePageLineSchema, line);
    placed.book = { case: "pageAgentId", value: book };
    if (placed.agentItem?.item.case === "agentFrame") placed.agentItem.item.value.agentId = book;
    return placed;
  };

  const landEntry = (entry: storev1.StoreEntry, unplaced: storev1.WriteBatchUnplacedEntry[]): void => {
    const update = entry.entry;
    if (update.case === "sessionUpdate") {
      sessionUpdateRows.push(update.value);
      return;
    }
    if (update.case !== "agentUpdate") return;
    const info = update.value.agentInfo;
    switch (info.case) {
      case "serveableFrame": {
        let line = info.value;
        if (line.book.case === "ownerUnknown") {
          const placed = placeUnownedLine(entry.upsertKey, line);
          if (placed.$typeName === "store.v1.WriteBatchUnplacedEntry") {
            unplaced.push(placed);
            return;
          }
          line = placed;
        }
        registerFromLine(line);
        recordLiveness(line);
        upsertPageLine(entry.upsertKey, line, entry.turn, entry.place);
        return;
      }
      case "unservedItem": {
        unservedRows.push(info.value);
        return;
      }
      case "bash": {
        const run = info.value.run?.value;
        if (run === undefined || run === "" || info.value.frame === undefined) return;
        const rows = bashRowsByRun.get(run) ?? [];
        const held = rows.find((stored) => stored.key === entry.upsertKey);
        if (held !== undefined) held.row = info.value;
        else rows.push({ key: entry.upsertKey, row: info.value });
        bashRowsByRun.set(run, rows);
        const arm = info.value.frame.result.case;
        if (arm === "success" || arm === "failure") bashEndedRuns.add(run);
        for (const watcher of bashWatchers) {
          if (watcher.run !== run || watcher.closed) continue;
          const waiter = watcher.waiters.shift();
          if (waiter !== undefined) waiter(info.value);
          else watcher.pending.push(info.value);
        }
        return;
      }
      default:
        // `workflow` is never written this wave; an unset arm is the producer's bug.
        return;
    }
  };

  /** A book's rows, oldest first. */
  const rowsOf = (agentId: string): StoredRow[] => books.get(agentId) ?? [];

  const routes = (router: ConnectRouter): void => {
    router.service(storev1.ShimStore, {
      async openAgentSession(request) {
        noteRead("OpenAgentSession", request);
        const refusal = refusalFor("OpenAgentSession");
        if (refusal !== undefined) {
          return create(storev1.OpenAgentSessionResponseSchema, {
            result: {
              case: "failure",
              value: create(storev1.OpenAgentSessionFailureSchema, {
                detail: refusal.detail,
                kind:
                  refusal.arm === "invalid_request"
                    ? {
                        case: "invalidRequest",
                        value: create(storev1.OpenAgentSessionInvalidRequestSchema, {
                          field: "agent",
                        }),
                      }
                    : refusal.arm === "stale_pointer"
                      ? {
                          case: "stalePointer",
                          value: create(storev1.OpenAgentSessionStalePointerSchema, {}),
                        }
                      : {
                          case: "storageFailure",
                          value: create(storev1.OpenAgentSessionStorageFailureSchema, {}),
                        },
              }),
            },
          });
        }
        const bookId = request.agent?.value ?? "";
        if (!knownAgents.has(bookId)) {
          // THE STORE OWNS THE BOOK'S EXISTENCE (landing 7). An agent row is
          // created by the first write that names the agent, so a reader that
          // opens before then is refused — which is exactly the race a fresh
          // session's `WatchAgent` runs, and the fake must run it too.
          return create(storev1.OpenAgentSessionResponseSchema, {
            result: {
              case: "failure",
              value: create(storev1.OpenAgentSessionFailureSchema, {
                detail: `fake store holds no agent row for ${JSON.stringify(bookId)}`,
                kind: {
                  case: "unknownAgent",
                  value: create(storev1.OpenAgentSessionUnknownAgentSchema, {}),
                },
              }),
            },
          });
        }
        const all = placedRowsOf(bookId);
        const floorPointer =
          request.opening.case === "knownThrough" ? Number(request.opening.value.value) : -1;
        // CATCH-UP IS WRITE ORDER: first written after the mark, in place order.
        // TAIL-ONLY serves nothing: the tail (pure here, as always) carries
        // only what is written from now on, and the empty page is the floor.
        const eligible =
          request.opening.case === "tailOnly" ? [] : all.filter((row) => Number(row.pointer) > floorPointer);
        // Newest first: take from the end, then reverse.
        const window = eligible.slice(Math.max(0, eligible.length - pageSize));
        const lines = [...window].reverse().map(lineAt);
        const olderExist = eligible.length > window.length;
        const oldestInPage = window[0];
        // A PAGE-ONLY OPEN MINTS NOTHING, exactly as the real store does: the
        // caller said no watch follows, so there is no token to hand back and
        // none left behind. The fake must run this or a shim that stopped
        // reading `watch` would look correct against a fake that still sent one.
        const token = request.pageOnly ? undefined : `watch-${nextToken++}`;
        if (token !== undefined) {
          watches.set(token, {
            book: bookId,
            pending: [],
            waiters: [],
            closed: false,
          });
        }
        return create(storev1.OpenAgentSessionResponseSchema, {
          result: {
            case: "success",
            value: create(storev1.OpenAgentSessionSuccessSchema, {
              page: create(storev1.AgentSessionPageSchema, {
                lines,
                boundary:
                  olderExist && oldestInPage !== undefined
                    ? {
                        case: "more",
                        value: create(storev1.ReadAgentPageMoreSchema, {
                          lastItem: pointerOf(oldestInPage),
                        }),
                      }
                    : { case: "floor", value: create(storev1.ReadAgentPageFloorSchema, {}) },
              }),
              watch:
                token === undefined
                  ? undefined
                  : create(storev1.AgentSessionTokenSchema, { value: token }),
            }),
          },
        });
      },

      async *watchAgentSession(request, context) {
        const token = request.watch?.value ?? "";
        const state = watches.get(token);
        if (state === undefined) {
          // A refused stream OPEN closes at the transport: the response type is
          // the frame, so there is nowhere in the message to say "no".
          throw new ConnectError(
            `store: no reading session for watch token ${JSON.stringify(token)}`,
            Code.NotFound,
          );
        }
        // THE OPEN-TAIL LEDGER. `finally` runs when the generator is RETURNED
        // into — which is what a cancelled Connect stream does — so the entry
        // survives exactly as long as the subscription does, and a client that
        // walks away without cancelling leaves it behind for the test to see.
        openTailTokens.add(token);
        for (const resolve of tailOpenWaiters.splice(0)) resolve(token);
        try {
          while (!state.closed && !context.signal.aborted) {
            const next = state.pending.shift();
            if (next !== undefined) {
              yield create(storev1.WatchAgentSessionResponseSchema, { frame: { case: "line", value: next } });
              continue;
            }
            // PARKED, BUT STILL CANCELLABLE. A generator suspended at an await
            // does not run its `finally` when the handler returns into it — the
            // return is queued behind the await — so a tail parked only on the
            // next line would hold its ledger entry (and, in a real store, its
            // subscription) forever after the caller hung up. Racing the
            // request's own abort signal is what makes the cancellation land.
            const awaited = await new Promise<storev1.StoreLineAt | null>((resolve) => {
              const settle = (line: storev1.StoreLineAt | null): void => {
                context.signal.removeEventListener("abort", onAbort);
                resolve(line);
              };
              const onAbort = (): void => settle(null);
              state.waiters.push(settle);
              context.signal.addEventListener("abort", onAbort, { once: true });
              if (state.closed || context.signal.aborted) settle(null);
            });
            if (awaited === null) return;
            yield create(storev1.WatchAgentSessionResponseSchema, { frame: { case: "line", value: awaited } });
          }
        } finally {
          openTailTokens.delete(token);
          for (const resolve of tailClosedWaiters.get(token) ?? []) resolve();
          tailClosedWaiters.delete(token);
        }
      },

      async *watchBashRun(request) {
        noteRead("WatchBashRun", request);
        const run = request.run?.value ?? "";
        const stored = bashRowsByRun.get(run) ?? [];
        // A VOUCHED RUN WITH NO ROW YET IS WAITED ON, as the real store waits.
        if (stored.length === 0 && !request.awaitFirstRow) {
          // A REFUSED OPEN closes at the transport: the response type is the
          // row, so there is nowhere in the message to say "no such run".
          throw new ConnectError(
            `store: no stored rows for bash run ${JSON.stringify(run)}`,
            Code.NotFound,
          );
        }
        const watcher = {
          run,
          // REPLAY FIRST, in write order: a snapshot taken now, so a row written
          // while the replay is being consumed lands in `pending` behind it.
          pending: stored.map((stored) => stored.row),
          waiters: [] as Array<(row: storev1.StoreAgentBash | null) => void>,
          closed: false,
        };
        bashWatchers.add(watcher);
        try {
          for (;;) {
            const next = watcher.pending.shift();
            if (next !== undefined) {
              yield create(storev1.WatchBashRunResponseSchema, { row: next });
              const arm = next.frame?.result.case;
              // ENDS AFTER THE TERMINAL: the run's stream is bounded.
              if (arm === "success" || arm === "failure") return;
              continue;
            }
            if (watcher.closed) return;
            const awaited = await new Promise<storev1.StoreAgentBash | null>((resolve) => {
              watcher.waiters.push(resolve);
              if (watcher.closed) resolve(null);
            });
            if (awaited === null) return;
            yield create(storev1.WatchBashRunResponseSchema, { row: awaited });
            const arm = awaited.frame?.result.case;
            if (arm === "success" || arm === "failure") return;
          }
        } finally {
          watcher.closed = true;
          bashWatchers.delete(watcher);
        }
      },

      async readAgentPage(request) {
        noteRead("ReadAgentPage", request);
        const refusal = refusalFor("ReadAgentPage");
        if (refusal !== undefined) {
          return create(storev1.ReadAgentPageResponseSchema, {
            result: {
              case: "failure",
              value: create(storev1.ReadAgentPageFailureSchema, {
                detail: refusal.detail,
                kind:
                  refusal.arm === "invalid_request"
                    ? {
                        case: "invalidRequest",
                        value: create(storev1.ReadAgentPageInvalidRequestSchema, { field: "book" }),
                      }
                    : refusal.arm === "stale_pointer"
                      ? {
                          case: "stalePointer",
                          value: create(storev1.ReadAgentPageStalePointerSchema, {}),
                        }
                      : {
                          case: "storageFailure",
                          value: create(storev1.ReadAgentPageStorageFailureSchema, {}),
                        },
              }),
            },
          });
        }
        const bookId = request.book?.value ?? "";
        const refuseRead = (
          detail: string,
          kind: storev1.ReadAgentPageFailure["kind"],
        ): storev1.ReadAgentPageResponse =>
          create(storev1.ReadAgentPageResponseSchema, {
            result: {
              case: "failure",
              value: create(storev1.ReadAgentPageFailureSchema, { detail, kind }),
            },
          });
        let older: StoredRow[];
        switch (request.position.case) {
          case "after": {
            // Strictly BEFORE the named line's CURRENT place.
            const pointer = request.position.value.value;
            const named = rowsOf(bookId).find((row) => row.pointer === pointer);
            if (named === undefined) {
              return refuseRead(`fake store: pointer ${JSON.stringify(pointer)} names no line of ${JSON.stringify(bookId)}`, {
                case: "stalePointer",
                value: create(storev1.ReadAgentPageStalePointerSchema, {}),
              });
            }
            older = placedRowsOf(bookId).filter((row) => placeOrder(row, named) < 0);
            break;
          }
          case "through": {
            if (!knownAgents.has(bookId)) {
              return refuseRead(`fake store holds no agent row for ${JSON.stringify(bookId)}`, {
                case: "unknownAgent",
                value: create(storev1.ReadAgentPageUnknownAgentSchema, {}),
              });
            }
            const bound = request.position.value.atMs;
            older = placedRowsOf(bookId).filter((row) => row.place.atMs <= bound);
            break;
          }
          default:
            return refuseRead("fake store: ReadAgentPage names no position", {
              case: "invalidRequest",
              value: create(storev1.ReadAgentPageInvalidRequestSchema, { field: "position" }),
            });
        }
        const window = older.slice(Math.max(0, older.length - pageSize));
        // EVERY LINE CARRIES ITS OWN POINTER (landing 3): a continuation page
        // is a reconnect mark like any other.
        const lines = [...window].reverse().map(lineAt);
        const oldestInPage = window[0];
        const olderExist = older.length > window.length;
        return create(storev1.ReadAgentPageResponseSchema, {
          result: {
            case: "success",
            value: create(storev1.ReadAgentPageSuccessSchema, {
              lines,
              boundary:
                olderExist && oldestInPage !== undefined
                  ? {
                      case: "more",
                      value: create(storev1.ReadAgentPageMoreSchema, {
                        lastItem: pointerOf(oldestInPage),
                      }),
                    }
                  : { case: "floor", value: create(storev1.ReadAgentPageFloorSchema, {}) },
            }),
          },
        });
      },

      getWorkflow() {
        // WORKFLOW IS KICKED. The store's own verb answers the same way the
        // shim's does, so nothing in the stack can half-support it by accident.
        throw new ConnectError(
          "store.v1.GetWorkflow is not implemented in this wave (workflow is kicked)",
          Code.Unimplemented,
        );
      },

      async getSidecarCursors(request) {
        noteRead("GetSidecarCursors", request);
        // No sidecar is running behind this fake, so it has read no files and
        // holds no cursors. An empty SUCCESS, never a failure: "nothing yet" is
        // an answer, not an error.
        return create(storev1.GetSidecarCursorsResponseSchema, {
          result: {
            case: "success",
            value: create(storev1.GetSidecarCursorsSuccessSchema, { cursors: [] }),
          },
        });
      },

      async getLiveWork(request) {
        noteRead("GetLiveWork", request);
        const liveWorkFailure = (
          detail: string,
          kind: storev1.GetLiveWorkFailure["kind"],
        ): storev1.GetLiveWorkResponse =>
          create(storev1.GetLiveWorkResponseSchema, {
            result: {
              case: "failure",
              value: create(storev1.GetLiveWorkFailureSchema, { detail, kind }),
            },
          });
        // NEVER ANSWERED UNSCOPED, exactly as the real store refuses it: the
        // store is shared by every session, and an unscoped answer is what let
        // one session's start close another session's running work.
        const session = request.session?.value ?? "";
        if (session === "") {
          return liveWorkFailure("session: GetLiveWork names no session", {
            case: "invalidRequest",
            value: create(storev1.GetLiveWorkInvalidRequestSchema, { field: "session" }),
          });
        }
        const refusal = refusalFor("GetLiveWork");
        if (refusal !== undefined) {
          return liveWorkFailure(
            refusal.detail,
            refusal.arm === "invalid_request"
              ? {
                  case: "invalidRequest",
                  value: create(storev1.GetLiveWorkInvalidRequestSchema, { field: "session" }),
                }
              : { case: "storageFailure", value: create(storev1.GetLiveWorkStorageFailureSchema, {}) },
          );
        }
        const lineage = lineageOf(session);
        const concluded = (agent: string): boolean =>
          rowsOf(agent).some((row) => {
            const item = row.line.agentItem?.item;
            if (item?.case !== "agentFrame") return false;
            const arm = item.value.result.case;
            return arm === "success" || arm === "failure";
          });
        // THE MAIN AGENT IS NEVER LISTED: it is the lineage's root, and its
        // liveness is the session's own.
        const liveAgents: conversationv1.AgentId[] = [];
        for (const created of spawnedBy.keys()) {
          if (created === session || !lineage.has(created) || concluded(created)) continue;
          liveAgents.push(create(conversationv1.AgentIdSchema, { value: created }));
        }
        const liveDetached: conversationv1.DetachedWorkId[] = [];
        for (const [workId, runId] of detachedAnnounced) {
          const owner = detachedOwner.get(workId);
          if (owner === undefined || !lineage.has(owner)) continue;
          const ended = (runId !== undefined && bashEndedRuns.has(runId)) || endedByUnit.has(workId);
          if (!ended) {
            liveDetached.push(create(conversationv1.DetachedWorkIdSchema, { value: workId }));
          }
        }
        return create(storev1.GetLiveWorkResponseSchema, {
          result: {
            case: "success",
            value: create(storev1.GetLiveWorkSuccessSchema, {
              liveAgents,
              // Workflow is kicked, so nothing ever writes a workflow row and
              // the live set is empty by construction rather than by omission.
              liveWorkflows: [],
              liveDetached,
            }),
          },
        });
      },

      async getAgentByVendorTask(request) {
        noteRead("GetAgentByVendorTask", request);
        const session = request.session?.value ?? "";
        const invalid = (field: string): storev1.GetAgentByVendorTaskResponse =>
          create(storev1.GetAgentByVendorTaskResponseSchema, {
            result: {
              case: "failure",
              value: create(storev1.GetAgentByVendorTaskFailureSchema, {
                detail: `${field}: the lookup is incomplete`,
                kind: { case: "invalidRequest", value: create(storev1.GetAgentByVendorTaskInvalidRequestSchema, { field }) },
              }),
            },
          });
        if (session === "") return invalid("session");
        if (request.vendorTaskId === "") return invalid("vendor_task_id");
        const lineage = lineageOf(session);
        const agents = [...(vendorTasks.get(request.vendorTaskId) ?? [])].filter((agent) => lineage.has(agent));
        if (agents.length > 1) {
          return create(storev1.GetAgentByVendorTaskResponseSchema, {
            result: {
              case: "failure",
              value: create(storev1.GetAgentByVendorTaskFailureSchema, {
                detail: `vendor task ${request.vendorTaskId} is paired with ${agents.length} agents of one lineage`,
                kind: { case: "storageFailure", value: create(storev1.GetAgentByVendorTaskStorageFailureSchema, {}) },
              }),
            },
          });
        }
        const [agent] = agents;
        return agent === undefined
          ? create(storev1.GetAgentByVendorTaskResponseSchema, {
              result: { case: "notFound", value: create(storev1.GetAgentByVendorTaskNotFoundSchema, {}) },
            })
          : create(storev1.GetAgentByVendorTaskResponseSchema, {
              result: {
                case: "success",
                value: create(storev1.GetAgentByVendorTaskSuccessSchema, {
                  agent: create(conversationv1.AgentIdSchema, { value: agent }),
                  // THE COMMISSION, as the real store answers it from the
                  // agent's recorded spawn start: unset when none was recorded.
                  commission: commissions.get(agent),
                }),
              },
            });
      },

      async writeBatch(request) {
        receivedWrites.push(request);
        // AN UNCLASSIFIED WRITE IS REFUSED, as the real store refuses it: the
        // class decides which queue the write takes, and nothing guesses it.
        if (request.writeClass?.writeClass.case === undefined) {
          writeVerdicts.push({ request, accepted: false });
          return create(storev1.WriteBatchResponseSchema, {
            result: {
              case: "failure",
              value: create(storev1.WriteBatchFailureSchema, {
                detail: "write_class: the write states neither interactive nor bulk",
                kind: {
                  case: "invalidRequest",
                  value: create(storev1.WriteBatchInvalidRequestSchema, { field: "write_class" }),
                },
              }),
            },
          });
        }
        writeVerdicts.push({ request, accepted: writeFailure === null });
        if (writeFailure !== null) {
          // DURABLE OR NOTHING: a failed batch lands no entry at all, which is
          // what makes a whole-batch retry correct rather than duplicating.
          //
          // THE ARM IS PART OF THE ANSWER. `invalid_request` says these bytes
          // can never be accepted, `storage_failure` says a retry may succeed;
          // a failure with no arm at all would let a writer that ignores the
          // distinction pass here and spin forever in production.
          return create(storev1.WriteBatchResponseSchema, {
            result: {
              case: "failure",
              value: create(storev1.WriteBatchFailureSchema, {
                detail: writeFailure.detail,
                kind:
                  writeFailure.arm === "invalid_request"
                    ? {
                        case: "invalidRequest",
                        value: create(storev1.WriteBatchInvalidRequestSchema, { field: "batch" }),
                      }
                    : {
                        case: "storageFailure",
                        value: create(storev1.WriteBatchStorageFailureSchema, {}),
                      },
              }),
            },
          });
        }
        for (const claim of request.batch?.shellRunClaims ?? []) {
          const run = claim.run?.value ?? "";
          if (claim.vendorTaskId === "" || run === "") {
            throw new Error("fake store: a batch stated a shell run claim missing its task id or its run");
          }
          runClaims.push(`${claim.vendorTaskId} -> ${run}`);
        }
        for (const locator of request.batch?.agentLocators ?? []) {
          const agent = locator.agent?.value ?? "";
          if (locator.vendorTaskId === "" || agent === "") {
            throw new Error("fake store: a batch stated an agent locator missing its locator or its agent");
          }
          const paired = vendorTasks.get(locator.vendorTaskId) ?? new Set<string>();
          paired.add(agent);
          vendorTasks.set(locator.vendorTaskId, paired);
        }
        const unplaced: storev1.WriteBatchUnplacedEntry[] = [];
        for (const entry of request.batch?.entries ?? []) {
          // ABSORBED, as the real store's ledger absorbs it: nothing lands.
          if (entry.writeId !== "" && appliedWriteIds.has(entry.writeId)) continue;
          const before = unplaced.length;
          landEntry(entry, unplaced);
          // AN UNPLACED ENTRY LANDS NOTHING, not even its ledger row, so a
          // replay is answered unplaced again.
          if (unplaced.length > before) continue;
          if (entry.writeId !== "") appliedWriteIds.add(entry.writeId);
          landedEntries.push(entry);
          for (const wake of [...landedWaiters]) wake(entry);
        }
        return create(storev1.WriteBatchResponseSchema, {
          result: { case: "success", value: create(storev1.WriteBatchSuccessSchema, { unplaced }) },
        });
      },
    });
  };

  const server = http.createServer(connectNodeAdapter({ routes }));
  const open = new Set<import("node:net").Socket>();
  server.on("connection", (socket) => {
    open.add(socket);
    socket.once("close", () => open.delete(socket));
  });
  await new Promise<void>((resolve, reject) => {
    server.once("error", reject);
    server.listen({ path: socketPath }, () => resolve());
  });

  return {
    socketPath,
    failWrites: (detail) => {
      writeFailure = detail === null ? null : { arm: "storage_failure", detail };
    },
    failWritesWith: (arm, detail) => {
      writeFailure = arm === null ? null : { arm, detail };
    },
    failReads: (verb, arm, detail) => {
      if (arm === null) {
        readFailures.delete(verb);
        return;
      }
      if (verb === "GetLiveWork" && arm === "stale_pointer") {
        // REFUSED RATHER THAN FABRICATED: GetLiveWorkFailure declares
        // invalid_request and storage_failure, so serving another arm would put
        // a shape on the wire the proto forbids and let a consumer be tested
        // against a store that cannot exist.
        throw new Error(
          `fake store: GetLiveWork declares invalid_request and storage_failure, not ${arm}`,
        );
      }
      readFailures.set(verb, { arm, detail: detail ?? `fake store refuses ${verb}` });
    },
    writeBatches: () => [...writeVerdicts],
    shellRunClaims: () => [...runClaims],
    openTails: () => [...openTailTokens],
    tailOpened: async () => {
      const already = [...openTailTokens][0];
      if (already !== undefined) return already;
      return new Promise<string>((resolve) => {
        tailOpenWaiters.push(resolve);
      });
    },
    tailClosed: async (token) => {
      if (!openTailTokens.has(token)) return;
      await new Promise<void>((resolve) => {
        const waiting = tailClosedWaiters.get(token) ?? [];
        waiting.push(resolve);
        tailClosedWaiters.set(token, waiting);
      });
    },
    writes: () => [...receivedWrites],
    book: (agentId) => rowsOf(agentId).map(lineAt),
    sessionUpdates: () => [...sessionUpdateRows],
    unserved: () => [...unservedRows],
    entryLanded: (matches) => {
      const already = landedEntries.find(matches);
      if (already !== undefined) return Promise.resolve(already);
      return new Promise<storev1.StoreEntry>((resolve) => {
        const wake = (entry: storev1.StoreEntry): void => {
          if (!matches(entry)) return;
          landedWaiters.delete(wake);
          resolve(entry);
        };
        landedWaiters.add(wake);
      });
    },
    reads: () => [...servedReads],
    close: async () => {
      // End every parked tail first: a watcher blocked on its promise would
      // otherwise keep the server's close callback from ever firing.
      for (const state of watches.values()) {
        state.closed = true;
        for (const waiter of state.waiters.splice(0)) {
          (waiter as unknown as (line: storev1.StoreLineAt | null) => void)(null);
        }
      }
      for (const watcher of bashWatchers) {
        watcher.closed = true;
        for (const waiter of watcher.waiters.splice(0)) waiter(null);
      }
      await new Promise<void>((resolve) => {
        server.close(() => resolve());
        for (const socket of open) socket.destroy();
        open.clear();
      });
      try {
        unlinkSync(socketPath);
      } catch {
        // Already gone; nothing to clean up.
      }
    },
  };
}
