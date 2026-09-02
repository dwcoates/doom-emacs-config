/**
 * The READ half: the page, the pinned tail, the refused-open re-open, and the
 * pointer pass-through.
 *
 * Driven against the in-process fake store for the same reason the writer suite
 * is: open-then-watch pinning and `known_through` bounding are the STORE's
 * semantics, and a double would be asserting our beliefs about them.
 */
import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import { conversationv1, storev1 } from "../../src/proto.js";
import { createStoreClient, type StoreClient } from "../../src/store/client.js";
import { PersistenceError } from "../../src/store/persistence.js";
import { readFailure, toHistoryEntry, toStorePointer, transportFailure } from "../../src/store/reader.js";
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
            // eslint-disable-next-line require-yield
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
      () => true,
    );
    const iterator = run[Symbol.asyncIterator]();
    const pending = iterator.next();

    plane.write([bashStartEntry()]);
    await plane.flush();

    const first = await pending;
    expect((first.value as conversationv1.AgentBash | undefined)?.result.case).toBe("start");
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
