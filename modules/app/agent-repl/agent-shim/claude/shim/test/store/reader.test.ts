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
import {
  createReader,
  readFailure,
  toHistoryEntry,
  toStorePointer,
} from "../../src/store/reader.js";
import { createPersistence } from "../../src/store/writer.js";
import { producerId } from "../../src/store/keys.js";
import { startFakeStore, type FakeStore } from "../fakes/store-server.js";
import { agent, promptEntry, readEntry, socketPathForTest } from "./persistence-fixtures.js";

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
        new Promise((resolve) => setTimeout(() => resolve("hung"), 2_000)),
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
  it("names an unknown agent", () => {
    expect(readFailure("unknown agent book-9").kind).toBe("unknown_agent");
  });

  it("names a stale pointer", () => {
    expect(readFailure("that pointer is not in this book").kind).toBe("stale_pointer");
  });

  it("falls back to store_unavailable for anything else", () => {
    expect(readFailure("the disk is full").kind).toBe("store_unavailable");
  });

  it("raises loudly on a page line whose item arm is unset", () => {
    const line = create(storev1.StorePageLineSchema, { pageAgentId: BOOK });

    expect(() => toHistoryEntry(line)).toThrow(PersistenceError);
  });
});

describe("openBashRun", () => {
  it("refuses a handle no announcement carries", async () => {
    const { plane } = await seeded("bash-unknown", 0);

    await expect(
      plane.openBashRun(create(conversationv1.DetachedWorkIdSchema, { value: "b-nope" })),
    ).rejects.toMatchObject({ kind: "unknown_work" });
  });

  it("serves the announced start, then the run's later frames", async () => {
    const started = await startFakeStore(socketPathForTest("bash-run"));
    store = started;
    const reader = createReader({ client: createStoreClient(started.socketPath) });
    reader.linkWork("work-1", "run-1");
    reader.noteBashFrame(
      "run-1",
      create(conversationv1.AgentBashSchema, {
        result: {
          case: "start",
          value: create(conversationv1.AgentBashStartSchema, {
            command: create(conversationv1.AgentBashCommandSchema, { line: "sleep 1" }),
            startedAt: create(conversationv1.AgentActivityStartedAtSchema, { atMs: 5n }),
          }),
        },
      }),
    );

    const run = await reader.openBashRun(
      create(conversationv1.DetachedWorkIdSchema, { value: "work-1" }),
    );
    const iterator = run[Symbol.asyncIterator]();
    const opening = await iterator.next();

    expect((opening.value as conversationv1.AgentBash).result.case).toBe("start");
  });

  it("ends the run's stream on its terminal frame, as a bounded stream owes", async () => {
    const started = await startFakeStore(socketPathForTest("bash-terminal"));
    store = started;
    const reader = createReader({ client: createStoreClient(started.socketPath) });
    reader.linkWork("work-1", "run-1");

    const run = await reader.openBashRun(
      create(conversationv1.DetachedWorkIdSchema, { value: "work-1" }),
    );
    const iterator = run[Symbol.asyncIterator]();
    const pending = iterator.next();
    reader.noteBashFrame(
      "run-1",
      create(conversationv1.AgentBashSchema, {
        result: {
          case: "failure",
          value: create(conversationv1.AgentBashFailureSchema, {}),
        },
      }),
    );
    await pending;

    await expect(iterator.next()).resolves.toMatchObject({ done: true });
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
