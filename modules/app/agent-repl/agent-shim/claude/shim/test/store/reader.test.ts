/**
 * The READ half: the page, the pinned tail, the refused-open re-open, and the
 * pointer pass-through.
 *
 * Driven against the in-process fake store for the same reason the writer suite
 * is: open-then-watch pinning and `known_through` bounding are the STORE's
 * semantics, and a double would be asserting our beliefs about them.
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import { writeSync } from "node:fs";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import { conversationv1, storev1 } from "../../src/proto.js";
import { createStoreClient, type StoreClient } from "../../src/store/client.js";
import { PersistenceError, type AgentPageSession } from "../../src/store/persistence.js";
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
  bashDeltaEntry,
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
    expect(unitOf(first.value as conversationv1.HistoryEntryAt)).toBe("unit-first");
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

    expect(unitOf(first.value as conversationv1.HistoryEntryAt)).toBe("unit-new");
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

    expect(unitOf(first.value as conversationv1.HistoryEntryAt)).toBe("unit-1");
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
    const line = create(storev1.StorePageLineSchema, { pageAgentId: BOOK });

    expect(() => toHistoryEntry(line)).toThrow(PersistenceError);
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

describe("openBashRun", () => {
  it("refuses an empty handle, which names no run at all", async () => {
    const { plane } = await seeded("bash-unknown", 0);

    await expect(
      plane.openBashRun(create(conversationv1.DetachedWorkIdSchema, { value: "" })),
    ).rejects.toMatchObject({ kind: "unknown_work" });
  });

  it("replays the run's stored rows, so a growing spool has no hole in the middle", async () => {
    const { plane } = await seeded("bash-rows", 0);
    // No join to establish: the handle IS the run's own identity.
    plane.write([bashStartEntry(), bashDeltaEntry()]);
    await plane.flush();

    const run = await plane.openBashRun(
      create(conversationv1.DetachedWorkIdSchema, { value: "run-1" }),
    );
    const seen: string[] = [];
    for await (const frame of run) {
      seen.push(String(frame.result.case));
      if (seen.length === 2) break;
    }

    expect(seen).toEqual(["start", "update"]);
  });

  it("ends the run's stream after the terminal row, as a bounded stream owes", async () => {
    const { plane } = await seeded("bash-terminal", 0);
    plane.write([bashStartEntry(), bashTerminalEntry()]);
    await plane.flush();

    const run = await plane.openBashRun(
      create(conversationv1.DetachedWorkIdSchema, { value: "run-1" }),
    );
    const arms: string[] = [];
    for await (const frame of run) arms.push(String(frame.result.case));

    expect(arms).toEqual(["start", "success"]);
  });

  it("refuses a run the store holds no row for", async () => {
    const { plane } = await seeded("bash-no-rows", 0);

    const run = await plane.openBashRun(
      create(conversationv1.DetachedWorkIdSchema, { value: "run-1" }),
    );

    await expect(
      (async () => {
        for await (const frame of run) void frame;
      })(),
    ).rejects.toMatchObject({ kind: "unknown_work" });
  });

  it("waits out a refused open when the caller still believes the run is live, and is woken by its first row", async () => {
    const { plane } = await seeded("bash-still-live", 0);

    // Nothing has been written for run-1 yet, but the caller (the daemon's own
    // live table) says the announcement already reached it, so the refusal is
    // a race to wait out rather than a real "unknown_work".
    const run = await plane.openBashRun(
      create(conversationv1.DetachedWorkIdSchema, { value: "run-1" }),
      () => "live",
    );
    const iterator = run[Symbol.asyncIterator]();
    const pending = iterator.next();

    plane.write([bashStartEntry()]);
    await plane.flush();

    const first = await pending;
    expect((first.value as conversationv1.AgentBash | undefined)?.result.case).toBe("start");
  });

  it("waits out a refused open through the concluded-but-unwritten window", async () => {
    const { plane } = await seeded("bash-concluded", 0);

    // THE WINDOW e2e run 5 hit: the run left the live set before its first row
    // was committed, so the caller's standing is "concluded" rather than
    // "live" -- and an announced run whose rows are merely late is not a run
    // that does not exist.
    const run = await plane.openBashRun(
      create(conversationv1.DetachedWorkIdSchema, { value: "run-1" }),
      () => "concluded",
    );
    const iterator = run[Symbol.asyncIterator]();
    const pending = iterator.next();

    plane.write([bashStartEntry()]);
    await plane.flush();

    const first = await pending;
    expect((first.value as conversationv1.AgentBash | undefined)?.result.case).toBe("start");
  });

  it("refuses a run nothing was ever announced under", async () => {
    const { plane } = await seeded("bash-unknown-standing", 0);

    const run = await plane.openBashRun(
      create(conversationv1.DetachedWorkIdSchema, { value: "run-1" }),
      () => "unknown",
    );

    await expect(
      (async () => {
        for await (const frame of run) void frame;
      })(),
    ).rejects.toMatchObject({ kind: "unknown_work" });
  });
});

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
    writeBatch: refuse,
    ...overrides,
  };
}

/** A reader over a hand-built store client. */
function readerOver(overrides: Partial<StoreClient>) {
  return createReader({ client: stubClient(overrides) });
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
      pageAgentId: BOOK,
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
        pageAgentId: BOOK,
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
  it("refuses a pushed frame that carries no line", async () => {
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

  it("ends the tail when the store closes the watch without refusing it", async () => {
    // A standing stream concludes nothing on its own, so a watch that simply
    // ends is the store closing and the tail stops rather than re-opening.
    // Arrange.
    const reader = readerOver({
      openAgentSession: async () => opened(floorPage([]), WATCH),
      watchAgentSession: () => ({ async *[Symbol.asyncIterator]() {} }),
    });
    const session = await reader.openAgentPage(BOOK, 10);

    // Act.
    const next = await session.tail[Symbol.asyncIterator]().next();

    // Assert.
    expect(next.done).toBe(true);
  });

  it("ends the tail on the very entry the teardown concluded it through", async () => {
    // Arrange.
    const reader = readerOver({
      openAgentSession: async () => opened(floorPage([]), WATCH),
      watchAgentSession: () =>
        standingWatch([
          create(storev1.WatchAgentSessionResponseSchema, { line: storedLine("7", "unit-last") }),
        ]),
    });
    const session = await reader.openAgentPage(BOOK, 10);
    session.concludeThrough(create(conversationv1.HistoryPointerSchema, { value: "7" }));

    // Act.
    const served: (string | undefined)[] = [];
    for await (const entry of session.tail) served.push(unitOf(entry));

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
      unitOf(first.value as conversationv1.HistoryEntryAt),
      unitOf(second.value as conversationv1.HistoryEntryAt),
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
          create(storev1.WatchAgentSessionResponseSchema, { line: storedLine("2", "unit-b") }),
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
      unitOf(a.value as conversationv1.HistoryEntryAt),
      unitOf(b.value as conversationv1.HistoryEntryAt),
    ]).toEqual(["unit-a", "unit-b"]);
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
    await expect(drain(await reader.openBashRun(WORK))).rejects.toMatchObject({
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
    await expect(drain(await reader.openBashRun(WORK, () => "live"))).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("stops waiting on a concluded run once its terminal row has been written", async () => {
    // After the terminal write, a store that still holds no row is answering
    // about a run that genuinely is absent -- the refusal is the truth.
    // Arrange.
    const reader = readerOver({
      watchBashRun: () => ({
        async *[Symbol.asyncIterator]() {
          throw new ConnectError("no such run", Code.NotFound);
        },
      }),
    });
    const terminal = bashTerminalEntry().item;
    if (terminal.kind !== "bash_run") throw new Error("the terminal fixture is no longer a run row");
    reader.noteBashFrame("run-1", terminal.frame);

    // Act, Assert.
    await expect(drain(await reader.openBashRun(WORK, () => "concluded"))).rejects.toMatchObject({
      kind: "unknown_work",
    });
  });

  it("stops waiting on a concluded run that never wrote a terminal, at the backstop", async () => {
    // The one path that retires a run without a terminal of its own is bounded
    // by the backstop rather than by a write, so the WINDOW ITSELF is what is
    // under test here and the wait is the mechanism, not a synchronization.
    // Arrange.
    const reader = readerOver({
      watchBashRun: () => ({
        async *[Symbol.asyncIterator]() {
          throw new ConnectError("no such run", Code.NotFound);
        },
      }),
    });

    // Act, Assert.
    await expect(drain(await reader.openBashRun(WORK, () => "concluded"))).rejects.toMatchObject({
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

/** Every structured record the logger wrote since `before`. */
function recordsSince(before: number): Record<string, unknown>[] {
  const calls = vi.mocked(writeSync).mock.calls as unknown as [number, Buffer, number, number][];
  return calls.slice(before).map(([, bytes, offset, length]) => {
    return JSON.parse(bytes.subarray(offset, offset + length).toString("utf8")) as Record<
      string,
      unknown
    >;
  });
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
    const before = vi.mocked(writeSync).mock.calls.length;

    // Act.
    await session.tail[Symbol.asyncIterator]().next();
    session.close();

    // Assert.
    expect(recordsSince(before)).toContainEqual(
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
      // The re-opened watch ends at once, so the tail's only possible output
      // would be the catch-up page the re-open failed to carry.
      watchAgentSession: refusedOnce(() => ({ async *[Symbol.asyncIterator]() {} })),
    });
    const session = await reader.openAgentPage(BOOK, 10);

    // Act.
    const next = await session.tail[Symbol.asyncIterator]().next();

    // Assert.
    expect(next.done).toBe(true);
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
        return { async *[Symbol.asyncIterator]() {} };
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
          yield create(storev1.WatchAgentSessionResponseSchema, { line: storedLine("1", "unit-a") });
          await held;
          yield create(storev1.WatchAgentSessionResponseSchema, { line: storedLine("2", "unit-b") });
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
    expect([unitOf(first.value as conversationv1.HistoryEntryAt), second.done]).toEqual([
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
    for await (const entry of session.tail) served.push(unitOf(entry));

    // Assert. The row it waited for is delivered, and then the stream ends.
    expect(served).toEqual(["unit-a"]);
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
          create(storev1.WatchAgentSessionResponseSchema, { line: storedLine("2", "unit-b") }),
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
      unitOf(first.value as conversationv1.HistoryEntryAt),
      unitOf(second.value as conversationv1.HistoryEntryAt),
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
    expect(unitOf(first.value as conversationv1.HistoryEntryAt)).toBe("unit-a");
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
    expect([opens, unitOf(served.value as conversationv1.HistoryEntryAt)]).toEqual([1, "unit-a"]);
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
