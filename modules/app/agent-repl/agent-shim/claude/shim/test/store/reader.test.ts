/**
 * The READ half: the page, the pinned tail, the refused-open re-open, and the
 * pointer pass-through.
 *
 * Driven against the in-process fake store for the same reason the writer suite
 * is: open-then-watch pinning and `known_through` bounding are the STORE's
 * semantics, and a double would be asserting our beliefs about them.
 */
import { afterEach, describe, expect, it } from "vitest";
import { logRecordsSince, logSinkMark } from "../log-records.js";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import { conversationv1, storev1 } from "../../src/proto.js";
import { createStoreClient, type StoreClient } from "../../src/store/client.js";
import {
  PersistenceError,
  type AgentPageSession,
  type AgentTailFrame,
} from "../../src/store/persistence.js";
import {
  createReader,
  readFailure,
  toHistoryEntry,
  toStorePointer,
  transportFailure,
} from "../../src/store/reader.js";
import { createPersistence } from "../../src/store/writer.js";
import { producerId } from "../../src/store/keys.js";
import { startFakeStore, type FakeStore } from "../fakes/store-server.js";
import {
  agent,
  bashTailEntry,
  bashStartEntry,
  bashTerminalEntry,
  promptEntry,
  readEntry,
  socketPathForTest,
} from "./persistence-fixtures.js";

/**
 * Resolve `"hung"` only after the event loop has turned enough times that a
 * sub-millisecond settle must already have happened.
 *
 * The budget is in TICKS, not milliseconds: every tick is loop progress, so the
 * guard is independent of how loaded the machine is and cannot lose a race to a
 * descheduled worker.
 */
async function hangGuard(): Promise<string> {
  for (let i = 0; i < 1_000; i++) await new Promise((resolve) => setImmediate(resolve));
  return "hung";
}

const PRODUCER = producerId("vendor-session-1");
const BOOK = agent("book-1");

let store: FakeStore | undefined;

afterEach(async () => {
  await store?.close();
  store = undefined;
});

/** A fake store with `count` rows already in `book-1`, and a live persistence. */
async function seeded(name: string, count: number) {
  const started = await startFakeStore(socketPathForTest(name));
  store = started;
  const client = createStoreClient(started.socketPath);
  const plane = createPersistence({
    client,
    producer: PRODUCER,
    nowMs: () => 1_000,
    sleep: async () => undefined,
  });
  for (let index = 0; index < count; index += 1) {
    plane.write([readEntry(BOOK, `unit-${index}`, `/tmp/${index}`)]);
  }
  await plane.flush();
  return { started, client, plane };
}

/**
 * `expect.objectContaining`, TYPED.
 *
 * The matcher's own declaration answers `any`, so nesting one inside an object
 * literal is an unsafe assignment — the linter's complaint is exactly right:
 * an `any` there would let a typo in the OUTER shape pass unchecked. Naming
 * the result `unknown` keeps the matcher and gives the literal a type.
 */
function containing(shape: Record<string, unknown>): unknown {
  return expect.objectContaining(shape);
}

describe("openAgentPage", () => {
  it("serves the newest entries first", async () => {
    const { plane } = await seeded("page-order", 3);

    const session = await plane.openAgentPage(BOOK, 10);
    session.close();

    expect(session.page.entries).toHaveLength(3);
    expect(unitOf(session.page.entries[0])).toBe("unit-2");
  });

  it("reports a floor when the page reached the oldest retained entry", async () => {
    const { plane } = await seeded("page-floor", 2);

    const session = await plane.openAgentPage(BOOK, 10);
    session.close();

    expect(session.page.boundary.case).toBe("floor");
  });

  it("reports more, with the pointer to walk older from, when the page is short", async () => {
    const { plane } = await seeded("page-more", 3);

    const session = await plane.openAgentPage(BOOK, 2);
    session.close();

    expect(session.page.boundary.case).toBe("more");
    const more = session.page.boundary.value as conversationv1.HistoryMore;
    expect(more.lastEntry?.value).not.toBe("");
  });

  it("serves each entry with the turn the store keeps for its row", async () => {
    const { plane } = await seeded("page-turn", 0);
    plane.write([promptEntry(BOOK, "turn-5", "hello")]);
    await plane.flush();

    const session = await plane.openAgentPage(BOOK, 10);
    session.close();

    expect(session.page.entries[0]?.turn?.value).toBe("turn-5");
  });

  it("serves an entry whose row no write stamped with no turn", async () => {
    const { plane } = await seeded("page-no-turn", 1);

    const session = await plane.openAgentPage(BOOK, 10);
    session.close();

    expect(session.page.entries[0]?.turn).toBeUndefined();
  });

  it("bounds the page by the caller's own high-water mark", async () => {
    const { plane } = await seeded("page-known-through", 3);
    const first = await plane.openAgentPage(BOOK, 10);
    first.close();
    const newest = first.page.entries[0]?.at;

    const second = await plane.openAgentPage(BOOK, 10, newest);
    second.close();

    expect(second.page.entries).toHaveLength(0);
  });

  it("passes the store's pointer through verbatim", async () => {
    const { started, plane } = await seeded("pointer-passthrough", 1);

    const session = await plane.openAgentPage(BOOK, 10);
    session.close();

    expect(session.page.entries[0]?.at?.value).toBe(started.book("book-1")[0]?.at?.value);
  });

  it("serves a page of zero entries when the caller asks for none", async () => {
    const { plane } = await seeded("page-size-zero", 2);

    const session = await plane.openAgentPage(BOOK, 0);
    session.close();

    expect(session.page.entries).toHaveLength(0);
    // A `more` boundary is UNBUILDABLE for an empty page: `HistoryMore.
    // last_entry` is not optional and an empty page names no entry to walk from.
    // So a page of zero reports the floor, and the caller follows the tail.
    expect(session.page.boundary.case).toBe("floor");
  });

  it("renders a prompt row as the history's own prompt entry", async () => {
    const started = await startFakeStore(socketPathForTest("prompt-entry"));
    store = started;
    const plane = createPersistence({
      client: createStoreClient(started.socketPath),
      producer: PRODUCER,
      nowMs: () => 1_000,
      sleep: async () => undefined,
    });
    plane.write([promptEntry(BOOK, "turn-1", "hello")]);
    await plane.flush();

    const session = await plane.openAgentPage(BOOK, 10);
    session.close();

    expect(session.page.entries[0]?.entry?.entry.case).toBe("userPrompt");
  });
});

describe("openAgentPage on a book with no rows yet", () => {
  /** A fake store holding NOTHING, and a live persistence pointed at it. */
  async function empty(name: string) {
    const started = await startFakeStore(socketPathForTest(name));
    store = started;
    const plane = createPersistence({
      client: createStoreClient(started.socketPath),
      producer: PRODUCER,
      nowMs: () => 1_000,
      sleep: async () => undefined,
    });
    return { started, plane };
  }

  it("serves an EMPTY page when the producer vouches for the agent", async () => {
    // Arrange.
    const { plane } = await empty("deferred-empty-page");

    // Act.
    const session = await plane.openAgentPage(BOOK, 10, undefined, () => true);
    session.close();

    // Assert.
    expect(session.page.entries).toHaveLength(0);
  });

  it("stands the tail on the book's first row", async () => {
    // Arrange.
    const { plane } = await empty("deferred-tail-stands");
    const session = await plane.openAgentPage(BOOK, 10, undefined, () => true);
    const pending = session.tail[Symbol.asyncIterator]().next();

    // Act.
    plane.write([readEntry(BOOK, "unit-first", "/tmp/first")]);
    await plane.flush();
    const first = await pending;
    session.close();

    // Assert.
    expect(unitOf(entryOf(first.value as AgentTailFrame))).toBe("unit-first");
  });

  it("refuses unknown_agent when the producer does not vouch for the agent", async () => {
    // Arrange.
    const { plane } = await empty("deferred-unknown");

    // Act, Assert.
    await expect(plane.openAgentPage(BOOK, 10, undefined, () => false)).rejects.toMatchObject({
      kind: "unknown_agent",
    });
  });

  it("refuses unknown_agent when the caller vouches for nothing at all", async () => {
    // Arrange.
    const { plane } = await empty("deferred-no-predicate");

    // Act, Assert.
    await expect(plane.openAgentPage(BOOK, 10)).rejects.toMatchObject({
      kind: "unknown_agent",
    });
  });

  it("surfaces a storage_failure even for a vouched-for agent", async () => {
    // A store that cannot be read is not a book waiting to be written.
    // Arrange.
    const { started, plane } = await empty("deferred-storage-failure");
    started.failReads("OpenAgentSession", "storage_failure", "sqlite: disk I/O error");

    // Act, Assert.
    await expect(plane.openAgentPage(BOOK, 10, undefined, () => true)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("ends a tail that was concluded before the book ever existed", async () => {
    // The teardown writes what it owes BEFORE concluding, so a book still
    // absent here holds nothing this consumer is owed.
    // Arrange.
    const { plane } = await empty("deferred-conclude");
    const session = await plane.openAgentPage(BOOK, 10, undefined, () => true);
    const pending = session.tail[Symbol.asyncIterator]().next();

    // Act.
    session.concludeThrough(undefined);
    const first = await Promise.race([pending.then(() => "ended"), hangGuard()]);
    session.close();

    // Assert.
    expect(first).toBe("ended");
  });

  it("stops waiting once the producer withdraws the announcement", async () => {
    // Arrange.
    const { plane } = await empty("deferred-withdrawn");
    let known = true;
    const session = await plane.openAgentPage(BOOK, 10, undefined, () => known);
    const pending = session.tail[Symbol.asyncIterator]().next();

    // Act.
    known = false;
    const outcome = await Promise.race([
      pending.then(
        () => "ended",
        (error: unknown) => (error instanceof PersistenceError ? error.kind : "other"),
      ),
      hangGuard(),
    ]);
    session.close();

    // Assert.
    expect(outcome).toBe("unknown_agent");
  });
});

describe("the tail", () => {
  it("delivers entries written after the page, and never replays the page", async () => {
    const { plane } = await seeded("tail-pin", 1);
    const session = await plane.openAgentPage(BOOK, 10);
    const iterator = session.tail[Symbol.asyncIterator]();
    const pending = iterator.next();

    plane.write([readEntry(BOOK, "unit-new", "/tmp/new")]);
    await plane.flush();
    const first = await pending;
    session.close();

    expect(unitOf(entryOf(first.value as AgentTailFrame))).toBe("unit-new");
  });

  it("stops promptly when the session is closed, rather than hanging on the drain", async () => {
    const { plane } = await seeded("tail-close", 1);
    const session = await plane.openAgentPage(BOOK, 10);
    const iterator = session.tail[Symbol.asyncIterator]();
    const pending = iterator.next();

    session.close();

    // CANCELLING THE CALL IS HOW A STANDING TAIL ENDS: Connect's stream close
    // drains the body, which on a standing stream never completes.
    await expect(
      Promise.race([
        pending.then(() => "settled"),
        // A HANG guard, not the mechanism of success. It is counted in
        // MACROTASK TICKS rather than milliseconds on purpose: a wall-clock
        // guard races the settle, so under machine load the guard can win and
        // report a hang that never happened. A tick budget cannot.
        hangGuard(),
      ]),
    ).resolves.toBe("settled");
  });

  it("can be opened again after a close", async () => {
    const { plane } = await seeded("tail-reopen", 1);
    const first = await plane.openAgentPage(BOOK, 10);
    first.close();

    const second = await plane.openAgentPage(BOOK, 10);
    second.close();

    expect(second.page.entries).toHaveLength(1);
  });

  it("ends at once when a catch-up open served the pointer the teardown concludes through", async () => {
    // A WATCH OPENED BEHIND THE HEAD IS CAUGHT UP BY ITS OWN OPENING PAGE, and
    // the teardown then concludes it through the book's HEAD. If the catch-up
    // rows did not count as served, that conclusion would name a row the tail
    // was still waiting for and `KillSession` would spend its whole
    // WATCHER_CONCLUSION_BUDGET_MS on a stream that had already delivered
    // everything the consumer was owed.
    // Arrange.
    const { plane } = await seeded("tail-conclude-catchup", 4);
    const walked = await plane.openAgentPage(BOOK, 10);
    walked.close();
    const behind = walked.page.entries[2]?.at;
    const head = walked.page.entries[0]?.at;
    const session = await plane.openAgentPage(BOOK, 10, behind);
    const iterator = session.tail[Symbol.asyncIterator]();

    // Act. The open's page is the catch-up, so the head is already served.
    expect(session.page.entries.map(unitOf)).toEqual(["unit-3", "unit-2"]);
    session.concludeThrough(head);

    // Assert.
    await expect(
      Promise.race([
        iterator.next().then(() => "settled"),
        // A HANG guard, not the mechanism of success. It is counted in
        // MACROTASK TICKS rather than milliseconds on purpose: a wall-clock
        // guard races the settle, so under machine load the guard can win and
        // report a hang that never happened. A tick budget cannot.
        hangGuard(),
      ]),
    ).resolves.toBe("settled");
  });

  it("ends when the head was served before an upsert of an older row", async () => {
    // AN UPSERT OF AN OLD ROW ARRIVES AT ITS ORIGINAL POINTER, which is older
    // than the head the tail already served -- so the newest pointer handed
    // over walks BACKWARD, and a conclusion through the head matched nothing.
    // The tail then stood for a row that will never be sent again and the
    // shim's KillSession spent its whole conclusion budget on it.
    // Arrange.
    const reader = readerOver({
      openAgentSession: async () =>
        opened(floorPage([storedLine("2", "unit-b"), storedLine("1", "unit-a")]), WATCH),
      watchAgentSession: () =>
        standingWatch([
          create(storev1.WatchAgentSessionResponseSchema, { frame: { case: "line", value: storedLine("1", "unit-a") } }),
        ]),
    });
    const session = await reader.openAgentPage(BOOK, 10, undefined, () => true);
    const iterator = session.tail[Symbol.asyncIterator]();
    const upsert = await iterator.next();
    expect(unitOf(entryOf(upsert.value as AgentTailFrame))).toBe("unit-a");

    // Act.
    session.concludeThrough(create(conversationv1.HistoryPointerSchema, { value: "2" }));

    // Assert.
    await expect(
      Promise.race([
        iterator.next().then(() => "settled"),
        // A HANG guard, not the mechanism of success. It is counted in
        // MACROTASK TICKS rather than milliseconds on purpose: a wall-clock
        // guard races the settle, so under machine load the guard can win and
        // report a hang that never happened. A tick budget cannot.
        hangGuard(),
      ]),
    ).resolves.toBe("settled");
  });

  it("concludeThrough(undefined) ends the tail at once, with nothing left to wait for", async () => {
    const { plane } = await seeded("tail-conclude-unbounded", 1);
    const session = await plane.openAgentPage(BOOK, 10);
    const iterator = session.tail[Symbol.asyncIterator]();

    session.concludeThrough(undefined);

    await expect(
      Promise.race([
        iterator.next().then(() => "settled"),
        // A HANG guard, not the mechanism of success. It is counted in
        // MACROTASK TICKS rather than milliseconds on purpose: a wall-clock
        // guard races the settle, so under machine load the guard can win and
        // report a hang that never happened. A tick budget cannot.
        hangGuard(),
      ]),
    ).resolves.toBe("settled");
  });
});

describe("the refused-open convention", () => {
  it("re-opens with the last served pointer when the store refuses the watch token", async () => {
    const started = await startFakeStore(socketPathForTest("notfound"));
    store = started;
    const real = createStoreClient(started.socketPath);
    let refusals = 0;
    const reopened: (storev1.StoreItemPointer | undefined)[] = [];
    const flaky: StoreClient = {
      ...real,
      openAgentSession: async (request) => {
        reopened.push(request.knownThrough);
        return real.openAgentSession(request);
      },
      watchAgentSession: (request, signal) => {
        if (refusals === 0) {
          refusals += 1;
          return {
            async *[Symbol.asyncIterator]() {
              throw new ConnectError("unknown token", Code.NotFound);
            },
          };
        }
        return real.watchAgentSession(request, signal);
      },
    };
    const plane = createPersistence({
      client: flaky,
      producer: PRODUCER,
      nowMs: () => 1_000,
      sleep: async () => undefined,
    });
    plane.write([readEntry(BOOK, "unit-0", "/tmp/0")]);
    await plane.flush();

    const session = await plane.openAgentPage(BOOK, 10);
    const iterator = session.tail[Symbol.asyncIterator]();
    const pending = iterator.next();
    plane.write([readEntry(BOOK, "unit-1", "/tmp/1")]);
    await plane.flush();
    const first = await pending;
    session.close();

    expect(unitOf(entryOf(first.value as AgentTailFrame))).toBe("unit-1");
    // The re-open states the caller's own high-water mark, which is what makes
    // the recovery lossless.
    expect(reopened[1]).toBeDefined();
  });
});

describe("readAgentPage", () => {
  it("walks older entries from a served pointer", async () => {
    const { plane } = await seeded("older-page", 3);
    const session = await plane.openAgentPage(BOOK, 1);
    session.close();
    const more = session.page.boundary.value as conversationv1.HistoryMore;

    const older = await plane.readAgentPage(BOOK, 10, more.lastEntry as conversationv1.HistoryPointer);

    expect(older.entries).toHaveLength(2);
  });

  it("carries the served pointers, so a continuation page is a reconnect mark too", async () => {
    const { started, plane } = await seeded("older-page-pointers", 2);
    const session = await plane.openAgentPage(BOOK, 1);
    session.close();
    const more = session.page.boundary.value as conversationv1.HistoryMore;

    const older = await plane.readAgentPage(BOOK, 10, more.lastEntry as conversationv1.HistoryPointer);

    expect(older.entries[0]?.at?.value).toBe(started.book("book-1")[0]?.at?.value);
  });

  it("refuses an empty pointer, which names no position at all", () => {
    const pointer = create(conversationv1.HistoryPointerSchema, { value: "" });

    expect(() => toStorePointer(pointer)).toThrow(PersistenceError);
  });
});

describe("failure translation", () => {
  // THE TYPED ARM DECIDES, never the detail prose. The detail is the driver's
  // text and a store maintainer may reword it at any time, so every case below
  // pairs the arm with a detail that CONTRADICTS it: a classification that
  // still read the string would fail here rather than in production.

  it("maps stale_pointer to stale_pointer", () => {
    // Arrange.
    const failure = create(storev1.OpenAgentSessionFailureSchema, {
      detail: "the disk is full",
      kind: {
        case: "stalePointer",
        value: create(storev1.OpenAgentSessionStalePointerSchema, {}),
      },
    });

    // Act, Assert.
    expect(readFailure(failure).kind).toBe("stale_pointer");
  });

  it("maps the store's own unknown_agent arm to unknown_agent", () => {
    // Landing 7: the record plane, not the shim, is what knows a book exists.
    // Arrange.
    const failure = create(storev1.OpenAgentSessionFailureSchema, {
      detail: "sqlite: database is locked",
      kind: {
        case: "unknownAgent",
        value: create(storev1.OpenAgentSessionUnknownAgentSchema, {}),
      },
    });

    // Act, Assert.
    expect(readFailure(failure).kind).toBe("unknown_agent");
  });

  it("maps invalid_request to unknown_agent, the condition the engine can act on", () => {
    // Arrange.
    const failure = create(storev1.OpenAgentSessionFailureSchema, {
      detail: "that pointer is not in this book",
      kind: {
        case: "invalidRequest",
        value: create(storev1.OpenAgentSessionInvalidRequestSchema, { field: "agent" }),
      },
    });

    // Act, Assert.
    expect(readFailure(failure).kind).toBe("unknown_agent");
  });

  it("maps storage_failure to store_unavailable", () => {
    // Arrange.
    const failure = create(storev1.OpenAgentSessionFailureSchema, {
      detail: "unknown agent book-9",
      kind: {
        case: "storageFailure",
        value: create(storev1.OpenAgentSessionStorageFailureSchema, {}),
      },
    });

    // Act, Assert.
    expect(readFailure(failure).kind).toBe("store_unavailable");
  });

  it("treats an UNSET arm as store_unavailable rather than guessing a kinder one", () => {
    // A refusal that names no reason is a store the shim cannot trust; a kinder
    // arm would make the engine retry into a broken store.
    // Arrange.
    const failure = create(storev1.OpenAgentSessionFailureSchema, {
      detail: "that pointer names no such agent",
    });

    // Act, Assert.
    expect(readFailure(failure).kind).toBe("store_unavailable");
  });

  it("carries the store's detail through onto the error", () => {
    // Arrange.
    const failure = create(storev1.ReadAgentPageFailureSchema, {
      detail: "sqlite: database is locked",
      kind: {
        case: "storageFailure",
        value: create(storev1.ReadAgentPageStorageFailureSchema, {}),
      },
    });

    // Act, Assert.
    expect(readFailure(failure).message).toBe("sqlite: database is locked");
  });

  it("maps GetLiveWork's only arm, storage_failure, to store_unavailable", () => {
    // Arrange.
    const failure = create(storev1.GetLiveWorkFailureSchema, {
      detail: "sqlite: no such table",
      kind: {
        case: "storageFailure",
        value: create(storev1.GetLiveWorkStorageFailureSchema, {}),
      },
    });

    // Act, Assert.
    expect(readFailure(failure).kind).toBe("store_unavailable");
  });

  it("raises loudly on a page line whose item arm is unset", () => {
    const line = create(storev1.StorePageLineSchema, { book: { case: "pageAgentId", value: BOOK } });

    expect(() => toHistoryEntry(line)).toThrow(PersistenceError);
  });

  it("renders a peer_message page line as a peerMessage history entry", () => {
    const line = create(storev1.StorePageLineSchema, {
      book: { case: "pageAgentId", value: BOOK },
      agentItem: create(storev1.StoreAgentItemSchema, {
        item: {
          case: "peerMessage",
          value: create(conversationv1.PeerMessageSchema, { agent: BOOK, sender: "Explore", body: "hi", id: "u1" }),
        },
      }),
    });

    expect(toHistoryEntry(line).entry.case).toBe("peerMessage");
  });

  it("transportFailure passes an existing PersistenceError through unchanged", () => {
    const original = new PersistenceError("stale_pointer", "already classified");

    expect(transportFailure(original)).toBe(original);
  });

  it("transportFailure wraps any other thrown value as store_unavailable", () => {
    const wrapped = transportFailure(new Error("the socket reset"));

    expect(wrapped).toBeInstanceOf(PersistenceError);
    expect(wrapped.kind).toBe("store_unavailable");
    expect(wrapped.message).toBe("the socket reset");
  });
});

/** An open for a run the caller does not hold live: refused if unstored. */
const NO_WAIT = { awaitFirstRow: false } as const;

describe("openBashRun", () => {
  it("refuses an empty handle, which names no run at all", async () => {
    const { plane } = await seeded("bash-unknown", 0);

    await expect(
      plane.openBashRun(create(conversationv1.DetachedWorkIdSchema, { value: "" }), NO_WAIT),
    ).rejects.toMatchObject({ kind: "unknown_work" });
  });

  it("replays the run's stored rows: its start, then its rendered tail", async () => {
    const { plane } = await seeded("bash-rows", 0);
    // No join to establish: the handle IS the run's own identity.
    plane.write([bashStartEntry(), bashTailEntry()]);
    await plane.flush();

    const run = await plane.openBashRun(
      create(conversationv1.DetachedWorkIdSchema, { value: "run-1" }),
      NO_WAIT,
    );
    const seen: string[] = [];
    for await (const frame of run) {
      seen.push(String(frame.result.case));
      if (seen.length === 2) break;
    }

    expect(seen).toEqual(["start", "tail"]);
  });

  it("ends the run's stream after the terminal row, as a bounded stream owes", async () => {
    const { plane } = await seeded("bash-terminal", 0);
    plane.write([bashStartEntry(), bashTerminalEntry()]);
    await plane.flush();

    const run = await plane.openBashRun(
      create(conversationv1.DetachedWorkIdSchema, { value: "run-1" }),
      NO_WAIT,
    );
    const arms: string[] = [];
    for await (const frame of run) arms.push(String(frame.result.case));

    expect(arms).toEqual(["start", "success"]);
  });

  it("refuses a run the store holds no row for", async () => {
    const { plane } = await seeded("bash-no-rows", 0);

    const run = await plane.openBashRun(
      create(conversationv1.DetachedWorkIdSchema, { value: "run-1" }),
      NO_WAIT,
    );

    await expect(
      (async () => {
        for await (const frame of run) void frame;
      })(),
    ).rejects.toMatchObject({ kind: "unknown_work" });
  });

  it("waits for the first row of a run the caller holds live", async () => {
    // Arrange: the run is announced, and its rows are the sidecar's, still to come.
    const { plane } = await seeded("bash-await", 0);
    const run = await plane.openBashRun(create(conversationv1.DetachedWorkIdSchema, { value: "run-1" }), {
      awaitFirstRow: true,
    });
    const first = run[Symbol.asyncIterator]().next();

    // Act.
    plane.write([bashStartEntry()]);
    await plane.flush();

    // Assert.
    const next = await first;
    if (next.done === true) throw new Error("the waiting run ended without a row");
    expect(next.value.result.case).toBe("start");
  });

  it("asks the store to wait exactly when the caller holds the run live", async () => {
    // Arrange.
    const asked: boolean[] = [];
    const reader = readerOver({
      watchBashRun: (request) => ({
        async *[Symbol.asyncIterator]() {
          asked.push(request.awaitFirstRow);
          yield* [];
        },
      }),
    });
    const work = create(conversationv1.DetachedWorkIdSchema, { value: "run-1" });

    // Act.
    for await (const frame of await reader.openBashRun(work, { awaitFirstRow: true })) void frame;
    for await (const frame of await reader.openBashRun(work, NO_WAIT)) void frame;

    // Assert.
    expect(asked).toEqual([true, false]);
  });

  it("does not wait for a first row: a run refused at the open stays refused when a row lands later", async () => {
    // NEVER A SILENT WAIT (2026-09-27). The shim writes an announced run's
    // start before announcing it, so a refusal is the answer and not a race.
    const { plane } = await seeded("bash-no-wait", 0);
    const run = await plane.openBashRun(
      create(conversationv1.DetachedWorkIdSchema, { value: "run-1" }),
      NO_WAIT,
    );
    // THE EXPECTATION IS ATTACHED BEFORE THE WRITE. The refusal lands while the
    // flush below is awaited, and a rejection with no handler yet attached is
    // reported by the runner as unhandled even though it is asserted later.
    const refused = expect(run[Symbol.asyncIterator]().next()).rejects.toMatchObject({
      kind: "unknown_work",
    });

    plane.write([bashStartEntry()]);
    await plane.flush();

    await refused;
  });
});

/** A tail frame's entry, when it is on the `entry` arm; a retirement is not one. */
function entryOf(frame: AgentTailFrame | undefined): conversationv1.HistoryEntryAt | undefined {
  return frame?.case === "entry" ? frame.value : undefined;
}

/** The unit id a history entry's activity frame names. */
function unitOf(entry: conversationv1.HistoryEntryAt | undefined): string | undefined {
  const frame = entry?.entry?.entry;
  if (frame?.case !== "agentFrame") return undefined;
  const result = frame.value.result;
  if (result.case !== "update") return undefined;
  const update = result.value.update;
  if (update.case !== "activity") return undefined;
  return update.value.activityId?.value;
}

// ---------------------------------------------------------------------------
// A store that answers with a MALFORMED message
//
// The fake store speaks the contract, so it can never produce these. They are
// driven against a hand-built `StoreClient` — the interface exists to be
// implementable by hand — because the reader's whole job on a malformed answer
// is to refuse it loudly rather than forward half a record.
// ---------------------------------------------------------------------------

/** A store client whose every verb is a defect until an override names it. */
function stubClient(overrides: Partial<StoreClient>): StoreClient {
  const refuse = (): never => {
    throw new Error("stub store client: this suite did not expect that call");
  };
  return {
    openAgentSession: refuse,
    watchAgentSession: refuse,
    watchBashRun: refuse,
    readAgentPage: refuse,
    getWorkflow: refuse,
    getSidecarCursors: refuse,
    getLiveWork: refuse,
    getAgentByVendorTask: refuse,
    writeBatch: refuse,
    ...overrides,
  };
}

/**
 * A reader over a hand-built store client.
 *
 * The backoff is taken instantly: the retry SCHEDULE is what
 * `test/store/retry.test.ts` asserts, and a suite that waited the real one out
 * would spend seconds per refusal.
 */
function readerOver(overrides: Partial<StoreClient>) {
  return createReader({ client: stubClient(overrides), sleep: () => Promise.resolve() });
}

/** A shell run's start frame, exactly as the shared fixture writes it. */
function bashStartFrame(): conversationv1.AgentBash {
  const item = bashStartEntry().item;
  if (item.kind !== "bash_run") throw new Error("the bash start fixture is no longer a run row");
  return item.frame;
}

/** One read unit's frame, exactly as the shared fixture writes it. */
function readFrame(unitValue: string): conversationv1.AgentFrame {
  const item = readEntry(BOOK, unitValue, `/tmp/${unitValue}`).item;
  if (item.kind !== "frame") throw new Error("the read fixture is no longer a frame");
  return item.frame;
}

/** One stored line at a pointer, carrying a read unit's frame. */
function storedLine(pointerValue: string, unitValue: string): storev1.StoreLineAt {
  return create(storev1.StoreLineAtSchema, {
    at: create(storev1.StoreItemPointerSchema, { value: pointerValue }),
    line: create(storev1.StorePageLineSchema, {
      book: { case: "pageAgentId", value: BOOK },
      agentItem: create(storev1.StoreAgentItemSchema, {
        item: { case: "agentFrame", value: readFrame(unitValue) },
      }),
    }),
  });
}

/** A page at its floor, holding `lines` newest-first. */
function floorPage(lines: storev1.StoreLineAt[]): storev1.AgentSessionPage {
  return create(storev1.AgentSessionPageSchema, {
    lines,
    boundary: { case: "floor", value: create(storev1.ReadAgentPageFloorSchema, {}) },
  });
}

/** The watch token every hand-built open pins its tail on. */
const WATCH = create(storev1.AgentSessionTokenSchema, { value: "watch-1" });

/**
 * An `OpenAgentSession` that succeeded, with whatever page and token are given.
 *
 * `watch` is REQUIRED rather than defaulted: a default would turn the very
 * `undefined` these tests are about back into a token.
 */
function opened(
  page: storev1.AgentSessionPage | undefined,
  watch: storev1.AgentSessionToken | undefined,
): storev1.OpenAgentSessionResponse {
  return create(storev1.OpenAgentSessionResponseSchema, {
    result: {
      case: "success",
      value: create(storev1.OpenAgentSessionSuccessSchema, { page, watch }),
    },
  });
}

/** A watch that pushes `frames` and then stands forever, as a real tail does. */
function standingWatch(
  frames: storev1.WatchAgentSessionResponse[],
): AsyncIterable<storev1.WatchAgentSessionResponse> {
  return {
    async *[Symbol.asyncIterator]() {
      for (const frame of frames) yield frame;
      await new Promise<never>(() => undefined);
    },
  };
}

describe("a malformed opening page", () => {
  it("refuses a line whose pointer is the empty string, which names no position", async () => {
    // Arrange.
    const page = floorPage([storedLine("", "unit-0")]);
    const reader = readerOver({ openAgentSession: async () => opened(page, WATCH) });

    // Act, Assert.
    await expect(reader.openAgentPage(BOOK, 10)).rejects.toMatchObject({
      kind: "stale_pointer",
    });
  });

  it("refuses a line that carries no pointer at all", async () => {
    // Arrange.
    const line = create(storev1.StoreLineAtSchema, {
      line: create(storev1.StorePageLineSchema, {
        book: { case: "pageAgentId", value: BOOK },
        agentItem: create(storev1.StoreAgentItemSchema, {
          item: { case: "agentFrame", value: readFrame("unit-0") },
        }),
      }),
    });
    const reader = readerOver({ openAgentSession: async () => opened(floorPage([line]), WATCH) });

    // Act, Assert.
    await expect(reader.openAgentPage(BOOK, 10)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("refuses a line that carries a pointer but no content", async () => {
    // Arrange.
    const line = create(storev1.StoreLineAtSchema, {
      at: create(storev1.StoreItemPointerSchema, { value: "1" }),
    });
    const reader = readerOver({ openAgentSession: async () => opened(floorPage([line]), WATCH) });

    // Act, Assert.
    await expect(reader.openAgentPage(BOOK, 10)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("refuses a page that says older lines remain but names no pointer to walk from", async () => {
    // Arrange.
    const page = create(storev1.AgentSessionPageSchema, {
      lines: [storedLine("2", "unit-1")],
      boundary: { case: "more", value: create(storev1.ReadAgentPageMoreSchema, {}) },
    });
    const reader = readerOver({ openAgentSession: async () => opened(page, WATCH) });

    // Act, Assert.
    await expect(reader.openAgentPage(BOOK, 1)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("refuses a page with no boundary arm set at all", async () => {
    // A page that says neither "more" nor "floor" cannot tell a consumer
    // whether it has reached the start of the book.
    // Arrange.
    const page = create(storev1.AgentSessionPageSchema, { lines: [storedLine("1", "unit-0")] });
    const reader = readerOver({ openAgentSession: async () => opened(page, WATCH) });

    // Act, Assert.
    await expect(reader.openAgentPage(BOOK, 10)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });
});

describe("a malformed OpenAgentSession answer", () => {
  it("refuses an answer with no result arm set", async () => {
    // Arrange.
    const reader = readerOver({
      openAgentSession: async () => create(storev1.OpenAgentSessionResponseSchema, {}),
    });

    // Act, Assert.
    await expect(reader.openAgentPage(BOOK, 10)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("refuses a success that carries no page", async () => {
    // Arrange.
    const reader = readerOver({ openAgentSession: async () => opened(undefined, WATCH) });

    // Act, Assert.
    await expect(reader.openAgentPage(BOOK, 10)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("refuses a success that carries no watch token, since the tail could not be pinned", async () => {
    // Arrange.
    const reader = readerOver({ openAgentSession: async () => opened(floorPage([]), undefined) });

    // Act, Assert.
    await expect(reader.openAgentPage(BOOK, 10)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("reports a store that cannot be reached at all as unavailable", async () => {
    // Arrange.
    const reader = readerOver({
      openAgentSession: () => Promise.reject(new Error("connect ECONNREFUSED")),
    });

    // Act, Assert.
    await expect(reader.openAgentPage(BOOK, 10)).rejects.toMatchObject({
      kind: "store_unavailable",
      message: "connect ECONNREFUSED",
    });
  });
});

describe("the tail against a malformed or ending watch", () => {
  it("refuses a pushed frame with no arm set, line or retired", async () => {
    // Arrange.
    const reader = readerOver({
      openAgentSession: async () => opened(floorPage([]), WATCH),
      watchAgentSession: () =>
        standingWatch([create(storev1.WatchAgentSessionResponseSchema, {})]),
    });
    const session = await reader.openAgentPage(BOOK, 10);

    // Act, Assert.
    await expect(session.tail[Symbol.asyncIterator]().next()).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("keeps standing when the store ends the watch without refusing it", async () => {
    // THE STORE ENDING A STANDING WATCH IS NOT A CONCLUSION. Its own handler
    // returns a clean end of stream when it shuts down, and this tail used to
    // stop there -- which ended the shim's WatchAgent while the session lived,
    // silently, and reached the daemon as a severed link. The recovery is the
    // refused token's: re-open from the last served pointer and carry on.
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens === 1 ? opened(floorPage([]), WATCH) : opened(floorPage([]), WATCH_2);
      },
      watchAgentSession: (request) =>
        request.watch?.value === "watch-1"
          ? { async *[Symbol.asyncIterator]() {} }
          : standingWatch([
              create(storev1.WatchAgentSessionResponseSchema, { frame: { case: "line", value: storedLine("1", "unit-a") } }),
            ]),
    });
    const session = await reader.openAgentPage(BOOK, 10);

    // Act.
    const served: (string | undefined)[] = [];
    for await (const entry of session.tail) {
      served.push(unitOf(entryOf(entry)));
      break;
    }
    session.close();

    // Assert.
    expect(served).toEqual(["unit-a"]);
  });

  it("re-opens from the last served pointer when the store ends the watch", async () => {
    // The recovery is lossless only if it states the caller's own high-water
    // mark, so the rows written during the gap come back in the catch-up page.
    // Arrange.
    const reopened: (storev1.StoreItemPointer | undefined)[] = [];
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async (request) => {
        reopened.push(request.knownThrough);
        opens += 1;
        return opens === 1 ? opened(floorPage([]), WATCH) : opened(floorPage([]), WATCH_2);
      },
      watchAgentSession: (request) =>
        request.watch?.value === "watch-1"
          ? {
              async *[Symbol.asyncIterator]() {
                yield create(storev1.WatchAgentSessionResponseSchema, {
                  frame: { case: "line", value: storedLine("7", "unit-a") },
                });
              },
            }
          : standingWatch([]),
    });
    const session = await reader.openAgentPage(BOOK, 10);
    const iterator = session.tail[Symbol.asyncIterator]();

    // Act. The first entry, then the end that forces the re-open.
    await iterator.next();
    const raced = await Promise.race([iterator.next().then(() => "served"), hangGuard()]);
    session.close();

    // Assert.
    expect(raced).toBe("hung");
    expect(reopened[1]?.value).toBe("7");
  });

  it("never re-serves a line the re-open carries that was already served unchanged", async () => {
    // THE RE-OPEN'S LOWER BOUND WALKS BACKWARD: it is the LAST pointer served,
    // and an upsert of an old row is served at that row's original pointer.
    // After a store restart the catch-up page then carried rows the consumer
    // already held -- the previous turn's terminal among them -- and the
    // daemon ended the turn now running with it (2026-09-23).
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens === 1
          ? opened(floorPage([storedLine("2", "unit-b"), storedLine("1", "unit-a")]), WATCH)
          : opened(floorPage([storedLine("3", "unit-c"), storedLine("2", "unit-b")]), WATCH_2);
      },
      watchAgentSession: (request) =>
        request.watch?.value === "watch-1"
          ? {
              async *[Symbol.asyncIterator]() {
                // The upsert of the oldest row walks the bound back to "1".
                yield create(storev1.WatchAgentSessionResponseSchema, {
                  frame: { case: "line", value: storedLine("1", "unit-a-updated") },
                });
              },
            }
          : standingWatch([]),
    });
    const session = await reader.openAgentPage(BOOK, 10);
    const iterator = session.tail[Symbol.asyncIterator]();
    await iterator.next();

    // Act. The watch ends; the re-open carries "2" again, unchanged, then "3".
    const next = await iterator.next();
    session.close();

    // Assert.
    expect(unitOf(entryOf(next.value as AgentTailFrame))).toBe("unit-c");
  });

  it("still serves a line the re-open carries at a served pointer when its content changed", async () => {
    // The bound is kept precisely so an update to an old row is not lost: the
    // same pointer with new content is new information.
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens === 1
          ? opened(floorPage([storedLine("2", "unit-b"), storedLine("1", "unit-a")]), WATCH)
          : opened(floorPage([storedLine("2", "unit-b-updated")]), WATCH_2);
      },
      watchAgentSession: (request) =>
        request.watch?.value === "watch-1"
          ? {
              async *[Symbol.asyncIterator]() {
                yield create(storev1.WatchAgentSessionResponseSchema, {
                  frame: { case: "line", value: storedLine("1", "unit-a-updated") },
                });
              },
            }
          : standingWatch([]),
    });
    const session = await reader.openAgentPage(BOOK, 10);
    const iterator = session.tail[Symbol.asyncIterator]();
    await iterator.next();

    // Act.
    const next = await iterator.next();
    session.close();

    // Assert.
    expect(unitOf(entryOf(next.value as AgentTailFrame))).toBe("unit-b-updated");
  });

  it("records the lines a re-open withheld as already served", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens === 1
          ? opened(floorPage([storedLine("1", "unit-a")]), WATCH)
          : opened(floorPage([storedLine("3", "unit-c"), storedLine("1", "unit-a")]), WATCH_2);
      },
      watchAgentSession: (request) =>
        request.watch?.value === "watch-1"
          ? { async *[Symbol.asyncIterator]() {} }
          : standingWatch([]),
    });
    const session = await reader.openAgentPage(BOOK, 10);
    const before = logSinkMark();

    // Act.
    await session.tail[Symbol.asyncIterator]().next();
    session.close();

    // Assert.
    expect(logRecordsSince(before)).toContainEqual(
      expect.objectContaining({
        level: "info",
        message:
          "the re-opened book carried lines already served unchanged; they were not served again",
        context: containing({ replayed: 1 }),
      }),
    );
  });

  it("records an unasked ending inside the budget as the recovery it is", async () => {
    // THE SILENCE WAS THE DEFECT, and so was the WARN that replaced it: an
    // ORDERED store restart ends every standing watch once and this side
    // re-opens on its own budget, so the record is lifecycle at INFO. The
    // fault is the BUDGET being spent, and that record is still ERROR.
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens === 1 ? opened(floorPage([]), WATCH) : opened(floorPage([]), WATCH_2);
      },
      watchAgentSession: (request) =>
        request.watch?.value === "watch-1"
          ? { async *[Symbol.asyncIterator]() {} }
          : standingWatch([]),
    });
    const session = await reader.openAgentPage(BOOK, 10);
    const before = logSinkMark();

    // Act.
    await Promise.race([session.tail[Symbol.asyncIterator]().next(), hangGuard()]);
    session.close();

    // Assert.
    expect(logRecordsSince(before)).toContainEqual(
      expect.objectContaining({
        level: "info",
        message:
          "the store ended a standing watch that nothing asked it to end; re-opening the book from the last served pointer",
        context: containing({ attempt: 1, budget: 3 }),
      }),
    );
  });

  it("gives up once the store has ended one more re-opened watch than the budget allows", async () => {
    // A store that accepts a token and ends the stream again cannot be
    // recovered from, and a tail that kept re-opening would spin against it.
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opened(floorPage([]), WATCH);
      },
      watchAgentSession: () => ({ async *[Symbol.asyncIterator]() {} }),
    });
    const session = await reader.openAgentPage(BOOK, 10);

    // Act, Assert.
    await expect(session.tail[Symbol.asyncIterator]().next()).rejects.toMatchObject({
      kind: "store_unavailable",
    });
    // The opening open, plus one re-open per end the budget admitted.
    expect(opens).toBe(1 + 3);
  });

  it("records giving up on a store that keeps ending the re-opened watch", async () => {
    // Arrange.
    const reader = readerOver({
      openAgentSession: async () => opened(floorPage([]), WATCH),
      watchAgentSession: () => ({ async *[Symbol.asyncIterator]() {} }),
    });
    const session = await reader.openAgentPage(BOOK, 10);
    const before = logSinkMark();

    // Act.
    await expect(session.tail[Symbol.asyncIterator]().next()).rejects.toBeInstanceOf(
      PersistenceError,
    );

    // Assert.
    expect(logRecordsSince(before)).toContainEqual(
      expect.objectContaining({
        level: "error",
        message:
          "gave up re-opening an agent's tail: the store keeps ending a standing watch nothing asked it to end",
      }),
    );
  });

  it("ends the tail on the very entry the teardown concluded it through", async () => {
    // Arrange.
    const reader = readerOver({
      openAgentSession: async () => opened(floorPage([]), WATCH),
      watchAgentSession: () =>
        standingWatch([
          create(storev1.WatchAgentSessionResponseSchema, { frame: { case: "line", value: storedLine("7", "unit-last") } }),
        ]),
    });
    const session = await reader.openAgentPage(BOOK, 10);
    session.concludeThrough(create(conversationv1.HistoryPointerSchema, { value: "7" }));

    // Act.
    const served: (string | undefined)[] = [];
    for await (const entry of session.tail) served.push(unitOf(entryOf(entry)));

    // Assert. The concluded entry is DELIVERED and then the stream ends.
    expect(served).toEqual(["unit-last"]);
  });

  it("refuses a re-open that answers with no watch token", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens === 1 ? opened(floorPage([]), WATCH) : opened(floorPage([]), undefined);
      },
      watchAgentSession: () => ({
        async *[Symbol.asyncIterator]() {
          throw new ConnectError("unknown token", Code.NotFound);
        },
      }),
    });
    const session = await reader.openAgentPage(BOOK, 10);

    // Act, Assert.
    await expect(session.tail[Symbol.asyncIterator]().next()).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("yields the re-open's own page in write order before the tail continues", async () => {
    // The re-open's page is bounded by the last pointer served, so everything
    // it carries is news the consumer is owed -- oldest first, as a tail is.
    // Arrange.
    let opens = 0;
    let refused = false;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens === 1
          ? opened(floorPage([]), WATCH)
          : opened(floorPage([storedLine("2", "unit-b"), storedLine("1", "unit-a")]), WATCH);
      },
      watchAgentSession: () => {
        if (!refused) {
          refused = true;
          return {
            async *[Symbol.asyncIterator]() {
              throw new ConnectError("unknown token", Code.NotFound);
            },
          };
        }
        return standingWatch([]);
      },
    });
    const session = await reader.openAgentPage(BOOK, 10);
    const iterator = session.tail[Symbol.asyncIterator]();

    // Act.
    const first = await iterator.next();
    const second = await iterator.next();
    session.close();

    // Assert.
    expect([
      unitOf(entryOf(first.value as AgentTailFrame)),
      unitOf(entryOf(second.value as AgentTailFrame)),
    ]).toEqual(["unit-a", "unit-b"]);
  });
});

describe("the deferred book, against a hand-built store", () => {
  /** An open that refuses `unknown_agent` until `writes` says otherwise. */
  function refusingOpen(): storev1.OpenAgentSessionResponse {
    return create(storev1.OpenAgentSessionResponseSchema, {
      result: {
        case: "failure",
        value: create(storev1.OpenAgentSessionFailureSchema, {
          detail: "no agent row",
          kind: {
            case: "unknownAgent",
            value: create(storev1.OpenAgentSessionUnknownAgentSchema, {}),
          },
        }),
      },
    });
  }

  it("serves the rows that landed while it waited, then follows the tail", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens <= 2 ? refusingOpen() : opened(floorPage([storedLine("1", "unit-a")]), WATCH);
      },
      watchAgentSession: () =>
        standingWatch([
          create(storev1.WatchAgentSessionResponseSchema, { frame: { case: "line", value: storedLine("2", "unit-b") } }),
        ]),
    });
    const session = await reader.openAgentPage(BOOK, 10, undefined, () => true);
    const iterator = session.tail[Symbol.asyncIterator]();
    const first = iterator.next();

    // Act.
    reader.noteAgentRows(["book-1"]);
    const a = await first;
    const b = await iterator.next();
    session.close();

    // Assert. The page it waited for comes first, then the live tail.
    expect([
      unitOf(entryOf(a.value as AgentTailFrame)),
      unitOf(entryOf(b.value as AgentTailFrame)),
    ]).toEqual(["unit-a", "unit-b"]);
  });

  it("ends the tail on a conclusion through a pointer it served out of the opening page", async () => {
    // Arrange: a book deferred until a write lands, whose rows then arrive in
    // the real session's OPENING PAGE rather than down its tail. That page is
    // served by the deferred wrapper, so the inner session never sees those
    // pointers go out — and a teardown concluding through the book's head used
    // to leave this tail standing for a row that had already been handed over.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens <= 2 ? refusingOpen() : opened(floorPage([storedLine("1", "unit-a")]), WATCH);
      },
      watchAgentSession: () => standingWatch([]),
    });
    const session = await reader.openAgentPage(BOOK, 10, undefined, () => true);
    const iterator = session.tail[Symbol.asyncIterator]();
    const first = iterator.next();
    reader.noteAgentRows(["book-1"]);
    const served = await first;
    expect(unitOf(entryOf(served.value as AgentTailFrame))).toEqual("unit-a");

    // Act: the teardown concludes through the head of the book, which is the
    // pointer just served.
    session.concludeThrough(create(conversationv1.HistoryPointerSchema, { value: "1" }));

    // Assert: the tail ENDS, rather than standing out the conclusion budget
    // the shim's KillSession bounds it with.
    expect((await iterator.next()).done).toBe(true);
  });

  it("ends the tail, serving nothing, when the session is closed while the book is being opened", async () => {
    // Arrange.
    let opens = 0;
    let session: AgentPageSession | undefined = undefined;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        if (opens <= 2) return refusingOpen();
        // The consumer walks away in the very window the open is in flight.
        session?.close();
        return opened(floorPage([storedLine("1", "unit-a")]), WATCH);
      },
      watchAgentSession: () => standingWatch([]),
    });
    session = await reader.openAgentPage(BOOK, 10, undefined, () => true);
    const iterator = session.tail[Symbol.asyncIterator]();
    const first = iterator.next();

    // Act.
    reader.noteAgentRows(["book-1"]);
    const next = await first;

    // Assert.
    expect(next.done).toBe(true);
  });
});

describe("readAgentPage against a store that misbehaves", () => {
  const AFTER = create(conversationv1.HistoryPointerSchema, { value: "9" });

  it("reports a store that cannot be reached as unavailable", async () => {
    // Arrange.
    const reader = readerOver({
      readAgentPage: () => Promise.reject(new Error("connect ECONNREFUSED")),
    });

    // Act, Assert.
    await expect(reader.readAgentPage(BOOK, 10, AFTER)).rejects.toMatchObject({
      kind: "store_unavailable",
      message: "connect ECONNREFUSED",
    });
  });

  it("raises the store's own refusal under the arm the store named", async () => {
    // Arrange.
    const started = await startFakeStore(socketPathForTest("older-page-refused"));
    store = started;
    const plane = createPersistence({
      client: createStoreClient(started.socketPath),
      producer: PRODUCER,
      nowMs: () => 1_000,
      sleep: async () => undefined,
    });
    started.failReads("ReadAgentPage", "stale_pointer", "that pointer is not in this book");

    // Act, Assert.
    await expect(plane.readAgentPage(BOOK, 10, AFTER)).rejects.toMatchObject({
      kind: "stale_pointer",
    });
  });

  it("refuses an answer with no result arm set", async () => {
    // Arrange.
    const reader = readerOver({
      readAgentPage: async () => create(storev1.ReadAgentPageResponseSchema, {}),
    });

    // Act, Assert.
    await expect(reader.readAgentPage(BOOK, 10, AFTER)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });
});

describe("openBashRun against a store that misbehaves", () => {
  const WORK = create(conversationv1.DetachedWorkIdSchema, { value: "run-1" });

  /** Drain a run's stream, so its refusal surfaces. */
  async function drain(run: AsyncIterable<conversationv1.AgentBash>): Promise<void> {
    for await (const frame of run) void frame;
  }

  it("refuses a pushed row that carries no frame", async () => {
    // Arrange.
    const reader = readerOver({
      watchBashRun: () => ({
        async *[Symbol.asyncIterator]() {
          yield create(storev1.WatchBashRunResponseSchema, {});
        },
      }),
    });

    // Act, Assert.
    await expect(drain(await reader.openBashRun(WORK, NO_WAIT))).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("reports a transport failure as unavailable rather than as an absent run", async () => {
    // Arrange.
    const reader = readerOver({
      watchBashRun: () => ({
        async *[Symbol.asyncIterator]() {
          throw new Error("the socket reset");
        },
      }),
    });

    // Act, Assert.
    await expect(drain(await reader.openBashRun(WORK, NO_WAIT))).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("asks the store exactly once for a run it refuses", async () => {
    // Arrange.
    let asks = 0;
    const reader = readerOver({
      watchBashRun: () => ({
        async *[Symbol.asyncIterator]() {
          asks += 1;
          throw new ConnectError("no such run", Code.NotFound);
        },
      }),
    });

    // Act.
    await drain(await reader.openBashRun(WORK, NO_WAIT)).catch(() => undefined);

    // Assert.
    expect(asks).toBe(1);
  });

  it("refuses as unknown_work a run the store refuses after serving it", async () => {
    // Arrange: a refusal is the store saying "no such run" however late it
    // comes; it is never re-read as a transport failure.
    const reader = readerOver({
      watchBashRun: () => ({
        async *[Symbol.asyncIterator]() {
          yield create(storev1.WatchBashRunResponseSchema, {
            row: create(storev1.StoreAgentBashSchema, { frame: bashStartFrame() }),
          });
          throw new ConnectError("no such run", Code.NotFound);
        },
      }),
    });

    // Act, Assert.
    await expect(drain(await reader.openBashRun(WORK, NO_WAIT))).rejects.toMatchObject({
      kind: "unknown_work",
    });
  });
});

describe("transportFailure on a thrown non-Error", () => {
  it("renders the thrown value as the detail", () => {
    // A rejected promise can carry anything at all; the detail must still say
    // something a reader of the log can act on.
    // Arrange, Act.
    const wrapped = transportFailure("the store said no");

    // Assert.
    expect(wrapped.message).toBe("the store said no");
  });
});

// ---------------------------------------------------------------------------
// The re-open's catch-up page, and what the tail does once it is drained
// ---------------------------------------------------------------------------

/** A page that says older lines remain, holding `lines` newest-first. */
function morePage(lines: storev1.StoreLineAt[]): storev1.AgentSessionPage {
  return create(storev1.AgentSessionPageSchema, {
    lines,
    boundary: {
      case: "more",
      value: create(storev1.ReadAgentPageMoreSchema, {
        lastItem: lines[lines.length - 1]?.at,
      }),
    },
  });
}

/** The second watch token, so a re-open can be told apart from the first open. */
const WATCH_2 = create(storev1.AgentSessionTokenSchema, { value: "watch-2" });

/** A watch that refuses its token once, then behaves as `then` says. */
function refusedOnce(
  then: () => AsyncIterable<storev1.WatchAgentSessionResponse>,
): () => AsyncIterable<storev1.WatchAgentSessionResponse> {
  let refused = false;
  return () => {
    if (refused) return then();
    refused = true;
    return {
      async *[Symbol.asyncIterator]() {
        throw new ConnectError("unknown token", Code.NotFound);
      },
    };
  };
}

describe("the re-open's catch-up page", () => {
  it("records that the gap exceeded the catch-up budget when the page says more remain", async () => {
    // A bounded re-open that still reports older lines means entries between
    // the last served pointer and this page were skipped -- a loss the record
    // is the only place to see.
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens === 1
          ? opened(floorPage([]), WATCH)
          : opened(morePage([storedLine("2", "unit-b")]), WATCH_2);
      },
      watchAgentSession: refusedOnce(() => standingWatch([])),
    });
    const session = await reader.openAgentPage(BOOK, 10);
    const before = logSinkMark();

    // Act.
    await session.tail[Symbol.asyncIterator]().next();
    session.close();

    // Assert.
    expect(logRecordsSince(before)).toContainEqual(
      expect.objectContaining({
        level: "error",
        message: "the gap since the last served pointer exceeds the catch-up budget; entries were skipped",
      }),
    );
  });

  it("serves nothing from a re-open that answers with no page at all", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens === 1 ? opened(floorPage([]), WATCH) : opened(undefined, WATCH_2);
      },
      // The re-opened watch STANDS and pushes nothing, so the tail's only
      // possible output would be the catch-up page the re-open failed to carry.
      watchAgentSession: refusedOnce(() => standingWatch([])),
    });
    const session = await reader.openAgentPage(BOOK, 10);

    // Act.
    const raced = await Promise.race([
      session.tail[Symbol.asyncIterator]()
        .next()
        .then(() => "served"),
      hangGuard(),
    ]);
    session.close();

    // Assert.
    expect(raced).toBe("hung");
  });

  it("follows the re-open's own watch token once the catch-up page is drained", async () => {
    // Arrange.
    let opens = 0;
    const watched: string[] = [];
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens === 1
          ? opened(floorPage([]), WATCH)
          : opened(floorPage([storedLine("2", "unit-b")]), WATCH_2);
      },
      watchAgentSession: (request) => {
        watched.push(request.watch?.value ?? "");
        if (watched.length === 1) {
          return {
            async *[Symbol.asyncIterator]() {
              throw new ConnectError("unknown token", Code.NotFound);
            },
          };
        }
        // The re-opened watch carries the next line. (It used to END at once,
        // which drove a further re-open whose page re-served "2" -- a line the
        // store never returns above a `known_through` of "2", and one the
        // reader now withholds as already served.)
        return standingWatch([
          create(storev1.WatchAgentSessionResponseSchema, { frame: { case: "line", value: storedLine("3", "unit-c") } }),
        ]);
      },
    });
    const session = await reader.openAgentPage(BOOK, 10);
    const iterator = session.tail[Symbol.asyncIterator]();

    // Act. The catch-up entry first, then the tail continues past it.
    await iterator.next();
    await iterator.next();

    // Assert.
    expect(watched).toEqual(["watch-1", "watch-2"]);
  });
});

describe("a tail closed under a push", () => {
  it("serves nothing more once the session is closed while a push is in flight", async () => {
    // Arrange.
    let release = (): void => undefined;
    const held = new Promise<void>((resolve) => {
      release = resolve;
    });
    const reader = readerOver({
      openAgentSession: async () => opened(floorPage([]), WATCH),
      watchAgentSession: () => ({
        async *[Symbol.asyncIterator]() {
          yield create(storev1.WatchAgentSessionResponseSchema, { frame: { case: "line", value: storedLine("1", "unit-a") } });
          await held;
          yield create(storev1.WatchAgentSessionResponseSchema, { frame: { case: "line", value: storedLine("2", "unit-b") } });
          await new Promise<never>(() => undefined);
        },
      }),
    });
    const session = await reader.openAgentPage(BOOK, 10);
    const iterator = session.tail[Symbol.asyncIterator]();
    const first = await iterator.next();

    // Act. The consumer walks away, and only then does the next push land.
    session.close();
    release();
    const second = await iterator.next();

    // Assert.
    expect([unitOf(entryOf(first.value as AgentTailFrame)), second.done]).toEqual([
      "unit-a",
      true,
    ]);
  });
});

describe("the deferred book's own waiting", () => {
  /** An open that refuses with `unknown_agent`, as a book with no rows is. */
  function refusingOpen(): storev1.OpenAgentSessionResponse {
    return create(storev1.OpenAgentSessionResponseSchema, {
      result: {
        case: "failure",
        value: create(storev1.OpenAgentSessionFailureSchema, {
          detail: "no agent row",
          kind: {
            case: "unknownAgent",
            value: create(storev1.OpenAgentSessionUnknownAgentSchema, {}),
          },
        }),
      },
    });
  }

  /** An open that refuses because the store itself is broken. */
  function brokenOpen(): storev1.OpenAgentSessionResponse {
    return create(storev1.OpenAgentSessionResponseSchema, {
      result: {
        case: "failure",
        value: create(storev1.OpenAgentSessionFailureSchema, {
          detail: "the store's disk is gone",
          kind: {
            case: "storageFailure",
            value: create(storev1.OpenAgentSessionStorageFailureSchema, {}),
          },
        }),
      },
    });
  }

  it("never asks the store again once the session was closed before its tail was read", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return refusingOpen();
      },
    });
    const session = await reader.openAgentPage(BOOK, 10, undefined, () => true);

    // Act.
    session.close();
    const next = await session.tail[Symbol.asyncIterator]().next();

    // Assert. Only the open that deferred the book was ever made.
    expect([next.done, opens]).toEqual([true, 1]);
  });

  it("surfaces a refusal that is not about the book being absent, rather than waiting on it", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens === 1 ? refusingOpen() : brokenOpen();
      },
    });
    const session = await reader.openAgentPage(BOOK, 10, undefined, () => true);

    // Act, Assert.
    await expect(session.tail[Symbol.asyncIterator]().next()).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("ends the tail when the session is closed while an open that refuses is in flight", async () => {
    // Arrange.
    let opens = 0;
    let session: AgentPageSession | undefined = undefined;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        // The consumer walks away in the very window this refusal is in flight.
        if (opens >= 2) session?.close();
        return refusingOpen();
      },
    });
    session = await reader.openAgentPage(BOOK, 10, undefined, () => true);

    // Act.
    const next = await session.tail[Symbol.asyncIterator]().next();

    // Assert. The wait was abandoned rather than re-armed for another round.
    expect([next.done, opens]).toEqual([true, 2]);
  });

  it("concludes the real session at once when the teardown concluded while it was opening", async () => {
    // Arrange.
    let opens = 0;
    let session: AgentPageSession | undefined = undefined;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        if (opens === 1) return refusingOpen();
        session?.concludeThrough(create(conversationv1.HistoryPointerSchema, { value: "1" }));
        return opened(floorPage([storedLine("1", "unit-a")]), WATCH);
      },
      watchAgentSession: () => standingWatch([]),
    });
    session = await reader.openAgentPage(BOOK, 10, undefined, () => true);

    // Act.
    const served: (string | undefined)[] = [];
    for await (const entry of session.tail) served.push(unitOf(entryOf(entry)));

    // Assert. The row it waited for is delivered, and then the stream ends.
    expect(served).toEqual(["unit-a"]);
  });

  it("ends a deferred book's tail on a head served before an upsert of an older row", async () => {
    // THE SAME BACKWARD WALK, through the wrapper. Its opening page is empty, so
    // the rows that landed while it waited go out of the REAL session's page --
    // and an upsert arriving after them names an OLDER position, which is what
    // a last-pointer mark mistakes for "the head has not been served".
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens === 1
          ? refusingOpen()
          : opened(floorPage([storedLine("2", "unit-b"), storedLine("1", "unit-a")]), WATCH);
      },
      watchAgentSession: () =>
        standingWatch([
          create(storev1.WatchAgentSessionResponseSchema, { frame: { case: "line", value: storedLine("1", "unit-a") } }),
        ]),
    });
    const session = await reader.openAgentPage(BOOK, 10, undefined, () => true);
    const iterator = session.tail[Symbol.asyncIterator]();
    await iterator.next();
    await iterator.next();
    await iterator.next();

    // Act. The head went out first; the upsert of the older row went out last.
    session.concludeThrough(create(conversationv1.HistoryPointerSchema, { value: "2" }));

    // Assert.
    await expect(
      Promise.race([
        iterator.next().then(() => "settled"),
        // A HANG guard, not the mechanism of success. It is counted in
        // MACROTASK TICKS rather than milliseconds on purpose: a wall-clock
        // guard races the settle, so under machine load the guard can win and
        // report a hang that never happened. A tick budget cannot.
        hangGuard(),
      ]),
    ).resolves.toBe("settled");
  });

  it("passes a later conclusion through to the session it already opened", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens === 1
          ? refusingOpen()
          : opened(floorPage([storedLine("1", "unit-a")]), WATCH);
      },
      watchAgentSession: () =>
        standingWatch([
          create(storev1.WatchAgentSessionResponseSchema, { frame: { case: "line", value: storedLine("2", "unit-b") } }),
        ]),
    });
    const session = await reader.openAgentPage(BOOK, 10, undefined, () => true);
    const iterator = session.tail[Symbol.asyncIterator]();
    const first = await iterator.next();

    // Act.
    session.concludeThrough(create(conversationv1.HistoryPointerSchema, { value: "2" }));
    const second = await iterator.next();
    const end = await iterator.next();

    // Assert. The conclusion lands on the inner tail, which ends on that entry.
    expect([
      unitOf(entryOf(first.value as AgentTailFrame)),
      unitOf(entryOf(second.value as AgentTailFrame)),
      end.done,
    ]).toEqual(["unit-a", "unit-b", true]);
  });

  it("wakes the agent it was told about even when the same batch names an empty agent", async () => {
    // An empty agent value names no book, so it is skipped -- and skipping it
    // must not cost the real agent in the same batch its wake-up.
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens <= 2 ? refusingOpen() : opened(floorPage([storedLine("1", "unit-a")]), WATCH);
      },
      watchAgentSession: () => standingWatch([]),
    });
    const session = await reader.openAgentPage(BOOK, 10, undefined, () => true);
    const iterator = session.tail[Symbol.asyncIterator]();
    const pending = iterator.next();

    // Act.
    reader.noteAgentRows(["", "book-1"]);
    const first = await pending;
    session.close();

    // Assert.
    expect(unitOf(entryOf(first.value as AgentTailFrame))).toBe("unit-a");
  });
});

describe("a book whose id this shim minted", () => {
  /**
   * An open that refuses `unknown_agent`, as the store does for a book it holds
   * no row for. A minted id must never reach it at all.
   */
  function refusingOpen(): storev1.OpenAgentSessionResponse {
    return create(storev1.OpenAgentSessionResponseSchema, {
      result: {
        case: "failure",
        value: create(storev1.OpenAgentSessionFailureSchema, {
          detail: "no agent row",
          kind: {
            case: "unknownAgent",
            value: create(storev1.OpenAgentSessionUnknownAgentSchema, {}),
          },
        }),
      },
    });
  }

  it("opens without asking the store at all", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return refusingOpen();
      },
    });
    reader.noteAgentMinted("book-1");

    // Act.
    await reader.openAgentPage(BOOK, 10, undefined, () => true);

    // Assert.
    expect(opens).toBe(0);
  });

  it("serves an empty opening page at the floor", async () => {
    // Arrange.
    const reader = readerOver({ openAgentSession: async () => refusingOpen() });
    reader.noteAgentMinted("book-1");

    // Act.
    const session = await reader.openAgentPage(BOOK, 10, undefined, () => true);
    session.close();

    // Assert.
    expect([session.page.entries.length, session.page.boundary.case]).toEqual([0, "floor"]);
  });

  it("stands its tail without asking the store while no row has landed", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return refusingOpen();
      },
    });
    reader.noteAgentMinted("book-1");
    const session = await reader.openAgentPage(BOOK, 10, undefined, () => true);

    // Act. The tail is pulled and left standing well past the recheck cadence.
    const outcome = await Promise.race([
      session.tail[Symbol.asyncIterator]().next().then(() => "served"),
      hangGuard(),
    ]);
    session.close();

    // Assert.
    expect([outcome, opens]).toEqual(["hung", 0]);
  });

  it("asks the store once the write that registers the book has landed", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opened(floorPage([storedLine("1", "unit-a")]), WATCH);
      },
      watchAgentSession: () => standingWatch([]),
    });
    reader.noteAgentMinted("book-1");
    const session = await reader.openAgentPage(BOOK, 10, undefined, () => true);
    const first = session.tail[Symbol.asyncIterator]().next();

    // Act.
    reader.noteAgentRows(["book-1"]);
    const served = await first;
    session.close();

    // Assert. The rows that landed while it waited come out as tail entries.
    expect([opens, unitOf(entryOf(served.value as AgentTailFrame))]).toEqual([1, "unit-a"]);
  });

  it("asks the store when the producer no longer vouches for the id", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return refusingOpen();
      },
    });
    reader.noteAgentMinted("book-1");

    // Act, Assert. A withdrawn announcement is owed the store's own refusal.
    await expect(reader.openAgentPage(BOOK, 10, undefined, () => false)).rejects.toMatchObject({
      kind: "unknown_agent",
    });
    expect(opens).toBe(1);
  });

  it("asks the store for an id it was never told was minted", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return refusingOpen();
      },
    });

    // Act.
    const session = await reader.openAgentPage(BOOK, 10, undefined, () => true);
    session.close();

    // Assert. Nothing declared this book absent, so the store is the authority.
    expect(opens).toBe(1);
  });

  it("records nothing for an empty agent id", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return refusingOpen();
      },
    });

    // Act.
    reader.noteAgentMinted("");
    const session = await reader.openAgentPage(agent(""), 10, undefined, () => true);
    session.close();

    // Assert. An empty id names no agent, so nothing was declared absent for it
    // and the store is still asked.
    expect(opens).toBe(1);
  });

  it("stops believing the book absent once an open for it succeeded", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opened(floorPage([storedLine("1", "unit-a")]), WATCH);
      },
      watchAgentSession: () => standingWatch([]),
    });
    reader.noteAgentMinted("book-1");
    const first = await reader.openAgentPage(BOOK, 10, undefined, () => true);
    // The deferred session only asks once its tail is pulled; the row landing is
    // what lets that ask happen.
    const pulled = first.tail[Symbol.asyncIterator]().next();
    reader.noteAgentRows(["book-1"]);
    await pulled;
    first.close();

    // Act.
    const second = await reader.openAgentPage(BOOK, 10, undefined, () => true);
    second.close();

    // Assert. The second open went straight to the store rather than deferring.
    expect([opens, second.page.entries.length]).toEqual([2, 1]);
  });
});

// ---------------------------------------------------------------------------
// The one-shot read. THREE OUTCOMES, TOLD APART BY ASKING: a page, an announced
// book with nothing in it yet, and a store that is down or failing reads. A
// watch may serve a minted book without asking because the value of that open
// is its tail; a read stands no tail, so an unasked store would be reported as
// an empty history the moment the store went away.
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
// page_only: WHO ASKS FOR A TOKEN.
//
// OpenAgentSession is unary and the store's service has no close, so a token
// minted for a page that is then abandoned can never be reclaimed: it lived
// for the store's whole process lifetime. The caller therefore states at the
// open whether a watch follows, and a read that stands no tail says so.
// ---------------------------------------------------------------------------
describe("a read that stands no tail asks for no watch token", () => {
  it("sends page_only on readFirstPage, so the store mints nothing", async () => {
    // Arrange.
    const requests: storev1.OpenAgentSessionRequest[] = [];
    const reader = readerOver({
      openAgentSession: async (request) => {
        requests.push(request);
        return opened(floorPage([storedLine("p1", "u1")]), undefined);
      },
    });

    // Act.
    await reader.readFirstPage(BOOK, 10);

    // Assert.
    expect(requests.map((request) => request.pageOnly)).toEqual([true]);
  });

  it("never reads the watch field of a page-only open", async () => {
    // Arrange. The store answers a page-only open with no token, as it must.
    let watches = 0;
    const reader = readerOver({
      openAgentSession: async () => opened(floorPage([storedLine("p1", "u1")]), undefined),
      watchAgentSession: () => {
        watches += 1;
        return standingWatch([]);
      },
    });

    // Act.
    const page = await reader.readFirstPage(BOOK, 10);

    // Assert.
    expect([page.entries.length, watches]).toEqual([1, 0]);
  });

  it("leaves page_only UNSET on the watched open, which does want a token", async () => {
    // Arrange.
    const requests: storev1.OpenAgentSessionRequest[] = [];
    const reader = readerOver({
      openAgentSession: async (request) => {
        requests.push(request);
        return opened(floorPage([storedLine("p1", "u1")]), WATCH);
      },
      watchAgentSession: () => standingWatch([]),
    });

    // Act.
    const session = await reader.openAgentPage(BOOK, 10);
    session.close();

    // Assert.
    expect(requests.map((request) => request.pageOnly)).toEqual([false]);
  });
});

describe("readFirstPage on a book whose id this shim minted", () => {
  /** The refusal the store gives for a book it holds no row for. */
  function noSuchBook(): storev1.OpenAgentSessionResponse {
    return create(storev1.OpenAgentSessionResponseSchema, {
      result: {
        case: "failure",
        value: create(storev1.OpenAgentSessionFailureSchema, {
          detail: "no agent row",
          kind: {
            case: "unknownAgent",
            value: create(storev1.OpenAgentSessionUnknownAgentSchema, {}),
          },
        }),
      },
    });
  }

  it("ASKS the store, which the watch-side open of the same book does not", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return noSuchBook();
      },
    });
    reader.noteAgentMinted("book-1");

    // Act.
    await reader.readFirstPage(BOOK, 10, undefined, () => true);

    // Assert.
    expect(opens).toBe(1);
  });

  it("serves an EMPTY page when a HEALTHY store refuses the book the producer vouches for", async () => {
    // Arrange.
    const reader = readerOver({ openAgentSession: async () => noSuchBook() });
    reader.noteAgentMinted("book-1");

    // Act.
    const page = await reader.readFirstPage(BOOK, 10, undefined, () => true);

    // Assert.
    expect([page.entries.length, page.boundary.case]).toEqual([0, "floor"]);
  });

  it("refuses store_unavailable when the store CANNOT BE REACHED", async () => {
    // Arrange.
    const reader = readerOver({
      openAgentSession: () => {
        throw new ConnectError("the store socket is gone", Code.Unavailable);
      },
    });
    reader.noteAgentMinted("book-1");

    // Act, Assert.
    await expect(reader.readFirstPage(BOOK, 10, undefined, () => true)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  // A BUSY DATABASE IS NOT AN UNREACHABLE ONE. This read backs the history a
  // consumer opens with and the membership a joining WatchSession is
  // re-announced, and one `SQLITE_BUSY` used to lose both outright.
  it("serves the page once a momentarily busy database lets go", async () => {
    // Arrange: the store is busy for its first answer only.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        if (opens > 1) return opened(floorPage([]), undefined);
        return create(storev1.OpenAgentSessionResponseSchema, {
          result: {
            case: "failure",
            value: create(storev1.OpenAgentSessionFailureSchema, {
              detail: "storage failure: begin read transaction: database is locked (5) (SQLITE_BUSY)",
              kind: {
                case: "storageFailure",
                value: create(storev1.OpenAgentSessionStorageFailureSchema, {}),
              },
            }),
          },
        });
      },
    });

    // Act.
    const page = await reader.readFirstPage(BOOK, 10);

    // Assert.
    expect(opens).toBe(2);
    expect(page.entries).toEqual([]);
  });

  it("refuses store_unavailable when the READ ITSELF FAILS", async () => {
    // Arrange.
    const reader = readerOver({
      openAgentSession: async () =>
        create(storev1.OpenAgentSessionResponseSchema, {
          result: {
            case: "failure",
            value: create(storev1.OpenAgentSessionFailureSchema, {
              // The prose says the opposite of the arm on purpose: the arm is
              // what decides, never the store's wording.
              detail: "that pointer names no such agent",
              kind: {
                case: "storageFailure",
                value: create(storev1.OpenAgentSessionStorageFailureSchema, {}),
              },
            }),
          },
        }),
    });
    reader.noteAgentMinted("book-1");

    // Act, Assert.
    await expect(reader.readFirstPage(BOOK, 10, undefined, () => true)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });
});

// ---------------------------------------------------------------------------
// The store, restarted under a live shim
//
// A deploy kickstarts the store's launchd service while every shim keeps
// running. The socket goes down, comes back, and the read half is built to
// ride it out -- so the ATTEMPTS are INFO and only the schedule running out is
// an ERROR.
// ---------------------------------------------------------------------------

/** The transport error a store whose socket is down answers with. */
function unreachable(): ConnectError {
  return new ConnectError("the store socket is not listening", Code.Unavailable);
}

describe("an open that meets a restarting store", () => {
  it("records the unreachable attempt at info and opens on the next one", async () => {
    // Arrange. The first open lands while the store's socket is down; the
    // second lands after it came back, exactly as a kickstart looks.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        if (opens === 1) throw unreachable();
        return opened(floorPage([storedLine("1", "unit-a")]), WATCH);
      },
      watchAgentSession: () => standingWatch([]),
    });
    const before = logSinkMark();

    // Act.
    const session = await reader.openAgentPage(BOOK, 10);
    const records = logRecordsSince(before);
    session.close();

    // Assert.
    expect(records).toContainEqual(
      expect.objectContaining({
        level: "info",
        message:
          "the store could not be reached to open an agent's book; replaying the open on the read retry schedule",
      }),
    );
  });

  it("serves the book the second attempt opened", async () => {
    // The re-open is not merely quiet: it has to produce the session the
    // caller asked for, or the restart still severs the WatchAgent.
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        if (opens === 1) throw unreachable();
        return opened(floorPage([storedLine("1", "unit-a")]), WATCH);
      },
      watchAgentSession: () => standingWatch([]),
    });

    // Act.
    const session = await reader.openAgentPage(BOOK, 10);
    session.close();

    // Assert.
    expect(session.page.entries.map(unitOf)).toEqual(["unit-a"]);
  });

  it("raises the store's own arm once the retry schedule is spent", async () => {
    // Arrange. A store that never comes back.
    const reader = readerOver({ openAgentSession: async () => Promise.reject(unreachable()) });

    // Act, Assert.
    await expect(reader.openAgentPage(BOOK, 10)).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("records giving up once the store stayed unreachable for the whole schedule", async () => {
    // Arrange.
    const reader = readerOver({ openAgentSession: async () => Promise.reject(unreachable()) });
    const before = logSinkMark();

    // Act.
    await expect(reader.openAgentPage(BOOK, 10)).rejects.toBeInstanceOf(PersistenceError);

    // Assert.
    expect(logRecordsSince(before)).toContainEqual(
      expect.objectContaining({
        level: "error",
        message:
          "gave up reading an agent's book: the store stayed unreachable for the whole read retry schedule",
        context: containing({ read: "openAgentBook" }),
      }),
    );
  });
});

describe("a line the store retired", () => {
  /** A watch push on the `retired` arm. */
  function retiredPush(line: storev1.StoreLineAt): storev1.WatchAgentSessionResponse {
    return create(storev1.WatchAgentSessionResponseSchema, { frame: { case: "retired", value: line } });
  }

  /** A watch push on the `line` arm. */
  function linePush(line: storev1.StoreLineAt): storev1.WatchAgentSessionResponse {
    return create(storev1.WatchAgentSessionResponseSchema, { frame: { case: "line", value: line } });
  }

  /** An open that refuses `unknown_agent`, as the store does for a book with no row. */
  function refusingOpen(): storev1.OpenAgentSessionResponse {
    return create(storev1.OpenAgentSessionResponseSchema, {
      result: {
        case: "failure",
        value: create(storev1.OpenAgentSessionFailureSchema, {
          detail: "no agent row",
          kind: {
            case: "unknownAgent",
            value: create(storev1.OpenAgentSessionUnknownAgentSchema, {}),
          },
        }),
      },
    });
  }

  it("relays a retired push on the retired arm, converted as a served line is", async () => {
    // Arrange.
    const reader = readerOver({
      openAgentSession: async () => opened(floorPage([]), WATCH),
      watchAgentSession: () => standingWatch([retiredPush(storedLine("4", "unit-gone"))]),
    });
    const session = await reader.openAgentPage(BOOK, 10);

    // Act.
    const next = await session.tail[Symbol.asyncIterator]().next();
    session.close();

    // Assert.
    const frame = next.value as AgentTailFrame;
    expect([frame.case, frame.value.at?.value, unitOf(frame.value)]).toEqual([
      "retired",
      "4",
      "unit-gone",
    ]);
  });

  it("relays a line push on the entry arm, unchanged", async () => {
    // Arrange.
    const reader = readerOver({
      openAgentSession: async () => opened(floorPage([]), WATCH),
      watchAgentSession: () => standingWatch([linePush(storedLine("4", "unit-here"))]),
    });
    const session = await reader.openAgentPage(BOOK, 10);

    // Act.
    const next = await session.tail[Symbol.asyncIterator]().next();
    session.close();

    // Assert.
    const frame = next.value as AgentTailFrame;
    expect([frame.case, frame.value.at?.value, unitOf(frame.value)]).toEqual([
      "entry",
      "4",
      "unit-here",
    ]);
  });

  it("refuses a retired push whose line carries no pointer", async () => {
    // Arrange.
    const reader = readerOver({
      openAgentSession: async () => opened(floorPage([]), WATCH),
      watchAgentSession: () =>
        standingWatch([
          retiredPush(create(storev1.StoreLineAtSchema, { line: storedLine("4", "unit-gone").line })),
        ]),
    });
    const session = await reader.openAgentPage(BOOK, 10);

    // Act, Assert.
    await expect(session.tail[Symbol.asyncIterator]().next()).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("ends a tail whose conclusion names the retired pointer", async () => {
    // THE RETIREMENT COUNTS AS SERVED: the store never serves the line again,
    // so a conclusion through it that waited for a line would stand forever.
    // Arrange.
    const reader = readerOver({
      openAgentSession: async () => opened(floorPage([storedLine("1", "unit-a")]), WATCH),
      watchAgentSession: () => standingWatch([retiredPush(storedLine("2", "unit-gone"))]),
    });
    const session = await reader.openAgentPage(BOOK, 10);
    const iterator = session.tail[Symbol.asyncIterator]();
    session.concludeThrough(create(conversationv1.HistoryPointerSchema, { value: "2" }));

    // Act.
    await iterator.next();
    const end = await Promise.race([iterator.next().then((next) => next.done), hangGuard()]);

    // Assert.
    expect(end).toBe(true);
  });

  it("ends a deferred book's tail whose conclusion names the retired pointer", async () => {
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens === 1 ? refusingOpen() : opened(floorPage([]), WATCH);
      },
      watchAgentSession: () => standingWatch([retiredPush(storedLine("2", "unit-gone"))]),
    });
    const session = await reader.openAgentPage(BOOK, 10, undefined, () => true);
    const iterator = session.tail[Symbol.asyncIterator]();
    const first = iterator.next();
    reader.noteAgentRows(["book-1"]);
    await first;

    // Act.
    session.concludeThrough(create(conversationv1.HistoryPointerSchema, { value: "2" }));
    const end = await Promise.race([iterator.next().then((next) => next.done), hangGuard()]);

    // Assert.
    expect(end).toBe(true);
  });

  it("re-opens from the retired pointer as the last one served", async () => {
    // Arrange.
    const reopened: (storev1.StoreItemPointer | undefined)[] = [];
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async (request) => {
        reopened.push(request.knownThrough);
        opens += 1;
        return opens === 1 ? opened(floorPage([]), WATCH) : opened(floorPage([]), WATCH_2);
      },
      watchAgentSession: (request) =>
        request.watch?.value === "watch-1"
          ? {
              async *[Symbol.asyncIterator]() {
                yield retiredPush(storedLine("7", "unit-gone"));
              },
            }
          : standingWatch([]),
    });
    const session = await reader.openAgentPage(BOOK, 10);
    const iterator = session.tail[Symbol.asyncIterator]();

    // Act. The retirement, then the end that forces the re-open.
    await iterator.next();
    await Promise.race([iterator.next(), hangGuard()]);
    session.close();

    // Assert.
    expect(reopened[1]?.value).toBe("7");
  });

  it("serves again a row the re-open carries back at a retired pointer, even unchanged", async () => {
    // A later write of a real record takes the row back at the same position.
    // The consumer removed it on the retirement, so the already-served check
    // must never withhold it, whatever its content.
    // Arrange.
    let opens = 0;
    const reader = readerOver({
      openAgentSession: async () => {
        opens += 1;
        return opens === 1
          ? opened(floorPage([]), WATCH)
          : opened(floorPage([storedLine("5", "unit-back")]), WATCH_2);
      },
      watchAgentSession: (request) =>
        request.watch?.value === "watch-1"
          ? {
              async *[Symbol.asyncIterator]() {
                yield retiredPush(storedLine("5", "unit-back"));
              },
            }
          : standingWatch([]),
    });
    const session = await reader.openAgentPage(BOOK, 10);
    const iterator = session.tail[Symbol.asyncIterator]();
    await iterator.next();

    // Act.
    const next = await Promise.race([iterator.next(), hangGuard()]);
    session.close();

    // Assert.
    expect(typeof next === "string" ? next : unitOf(entryOf(next.value as AgentTailFrame))).toBe(
      "unit-back",
    );
  });

  it("records each relayed retirement verbosely, with its agent and pointer", async () => {
    // Arrange.
    const reader = readerOver({
      openAgentSession: async () => opened(floorPage([]), WATCH),
      watchAgentSession: () => standingWatch([retiredPush(storedLine("4", "unit-gone"))]),
    });
    const session = await reader.openAgentPage(BOOK, 10);
    const before = logSinkMark();

    // Act.
    await session.tail[Symbol.asyncIterator]().next();
    session.close();

    // Assert.
    expect(logRecordsSince(before)).toContainEqual(
      expect.objectContaining({
        level: "debug",
        verbosity: "verbose",
        message: "the store retired a line of an agent's book; relaying the retirement to the consumer",
        context: containing({ agent: "book-1", pointer: "4" }),
      }),
    );
  });
});

describe("the conversation place", () => {
  /** A persistence over a fresh fake store whose clock the test moves. */
  async function clocked(name: string) {
    const started = await startFakeStore(socketPathForTest(name));
    store = started;
    const clock = { now: 1_000 };
    const plane = createPersistence({
      client: createStoreClient(started.socketPath),
      producer: PRODUCER,
      nowMs: () => clock.now,
      sleep: async () => undefined,
    });
    return { started, plane, clock };
  }

  /** One line of an older page, as a stub store serves it. */
  async function servedOlderLine(line: storev1.StoreLineAt): Promise<conversationv1.HistoryEntryAt | undefined> {
    const reader = readerOver({
      readAgentPage: async () =>
        create(storev1.ReadAgentPageResponseSchema, {
          result: {
            case: "success",
            value: create(storev1.ReadAgentPageSuccessSchema, {
              lines: [line],
              boundary: { case: "floor", value: create(storev1.ReadAgentPageFloorSchema, {}) },
            }),
          },
        }),
    });
    const page = await reader.readAgentPage(BOOK, 10, create(conversationv1.HistoryPointerSchema, { value: "9" }));
    return page.entries[0];
  }

  it("serves an entry at the place its writer stamped, on the recorded arm", async () => {
    // Arrange.
    const { plane } = await clocked("place-recorded");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/1")]);
    await plane.flush();

    // Act.
    const session = await plane.openAgentPage(BOOK, 10);
    session.close();

    // Assert.
    const place = session.page.entries[0]?.place;
    expect([place?.case, place?.value?.atMs, place?.value?.ordinal]).toEqual(["recordedPlace", 1_000n, 0]);
  });

  it("serves a line the store placed by receipt on the received arm", async () => {
    // Arrange.
    const line = storedLine("9", "unit-1");
    line.place = {
      case: "receivedPlace",
      value: create(conversationv1.ConversationPlaceSchema, { atMs: 4_000n, ordinal: 0 }),
    };

    // Act.
    const entry = await servedOlderLine(line);

    // Assert.
    expect([entry?.place.case, entry?.place.value?.atMs]).toEqual(["receivedPlace", 4_000n]);
  });

  it("serves a line the store placed nowhere unplaced", async () => {
    // Arrange.
    const line = storedLine("9", "unit-1");

    // Act.
    const entry = await servedOlderLine(line);

    // Assert.
    expect(entry?.place.case).toBeUndefined();
  });

  it("serves a book in descending place, not in the order it was written", async () => {
    // Arrange: the later-placed row is written first.
    const { started, clock } = await clocked("place-order");
    const client = createStoreClient(started.socketPath);
    const late = createPersistence({ client, producer: PRODUCER, nowMs: () => 5_000, sleep: async () => undefined });
    late.write([readEntry(BOOK, "unit-late", "/tmp/late")]);
    await late.flush();
    clock.now = 2_000;
    const early = createPersistence({ client, producer: PRODUCER, nowMs: () => clock.now, sleep: async () => undefined });
    early.write([readEntry(BOOK, "unit-early", "/tmp/early")]);
    await early.flush();

    // Act.
    const session = await early.openAgentPage(BOOK, 10);
    session.close();

    // Assert.
    expect(session.page.entries.map(unitOf)).toEqual(["unit-late", "unit-early"]);
  });

  it("forwards a through read as the store's own through arm", async () => {
    // Arrange.
    const requests: storev1.ReadAgentPageRequest[] = [];
    const reader = readerOver({
      readAgentPage: async (request) => {
        requests.push(request);
        return create(storev1.ReadAgentPageResponseSchema, {
          result: {
            case: "success",
            value: create(storev1.ReadAgentPageSuccessSchema, {
              boundary: { case: "floor", value: create(storev1.ReadAgentPageFloorSchema, {}) },
            }),
          },
        });
      },
    });

    // Act.
    await reader.readPageThrough(BOOK, 10, create(conversationv1.ConversationThroughSchema, { atMs: 2_500n }));

    // Assert.
    const position = requests[0]?.position;
    expect([position?.case, position?.case === "through" ? position.value.atMs : undefined]).toEqual([
      "through",
      2_500n,
    ]);
  });

  it("reads a book as it stood at an instant", async () => {
    // Arrange.
    const { plane, clock } = await clocked("place-through");
    for (const [at, unitValue] of [
      [1_000, "unit-1"],
      [2_000, "unit-2"],
      [3_000, "unit-3"],
    ] as const) {
      clock.now = at;
      plane.write([readEntry(BOOK, unitValue, `/tmp/${unitValue}`)]);
      await plane.flush();
    }

    // Act.
    const page = await plane.readPageThrough(BOOK, 10, create(conversationv1.ConversationThroughSchema, { atMs: 2_000n }));

    // Assert.
    expect(page.entries.map(unitOf)).toEqual(["unit-2", "unit-1"]);
  });

  it("refuses a through read of a book the store never heard of as unknown", async () => {
    // Arrange.
    const { plane } = await clocked("place-through-unknown");

    // Act, Assert.
    await expect(
      plane.readPageThrough(agent("nobody"), 10, create(conversationv1.ConversationThroughSchema, { atMs: 2_000n })),
    ).rejects.toMatchObject({ kind: "unknown_agent" });
  });
});
