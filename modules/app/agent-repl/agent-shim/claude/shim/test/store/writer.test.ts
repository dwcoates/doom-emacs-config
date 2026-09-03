/**
 * The WRITE half: routing, the durable ack, replay absorption, and the loud drop.
 *
 * Every test drives the real writer against the in-process fake store, because
 * the behaviors that matter — upsert by key, durable-or-nothing, write-id
 * absorption — are the STORE's semantics, and a hand-rolled double would be
 * asserting our own beliefs about them rather than the contract.
 */
import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { conversationv1, storev1 } from "../../src/proto.js";
import { createStoreClient } from "../../src/store/client.js";
import { producerId } from "../../src/store/keys.js";
import { DEFAULT_RETRY_POLICY, type Persistence } from "../../src/store/persistence.js";
import { createPersistence, toStoreEntry, toWriteBatchRequest } from "../../src/store/writer.js";
import { startFakeStore, type FakeStore } from "../fakes/store-server.js";
import { agent, bashRunEntry, promptEntry, readEntry, socketPathForTest } from "./persistence-fixtures.js";

const PRODUCER = producerId("vendor-session-1");
const BOOK = agent("book-1");

let store: FakeStore | undefined;

afterEach(async () => {
  await store?.close();
  store = undefined;
});

/** A persistence over a fresh fake store, with a fast, deterministic backoff. */
async function persistence(name: string): Promise<{ store: FakeStore; persistence: Persistence }> {
  const started = await startFakeStore(socketPathForTest(name));
  store = started;
  return {
    store: started,
    persistence: createPersistence({
      client: createStoreClient(started.socketPath),
      producer: PRODUCER,
      nowMs: () => 1_000,
      // The schedule's SHAPE is what is under test, never its wall-clock cost.
      sleep: async () => undefined,
      retry: { ...DEFAULT_RETRY_POLICY, backoffMs: [0, 0, 0, 0] },
    }),
  };
}

describe("PersistEntry → StoreEntry routing", () => {
  it("routes a prompt to a page line of its own book", () => {
    const entry = toStoreEntry(PRODUCER, promptEntry(BOOK, "turn-1", "hello"));

    expect(entry.entry.case).toBe("agentUpdate");
    const update = entry.entry.value as storev1.StoreAgentUpdate;
    expect(update.agentInfo.case).toBe("serveableFrame");
    expect((update.agentInfo.value as storev1.StorePageLine).pageAgentId?.value).toBe("book-1");
  });

  it("routes a keep-alive turn's frame to the unserved keepalive arm", () => {
    const entry = toStoreEntry(PRODUCER, readEntry(BOOK, "unit-1", "/tmp/a", { keepalive: true }));

    const update = entry.entry.value as storev1.StoreAgentUpdate;
    expect(update.agentInfo.case).toBe("unservedItem");
    expect((update.agentInfo.value as storev1.StoreUnservedItem).unservedItem.case).toBe("keepalive");
  });

  it("routes a detached shell run's frame to the bash lifecycle arm, never a page line", () => {
    const entry = toStoreEntry(PRODUCER, bashRunEntry(BOOK, "run-1", "sleep 1"));

    const update = entry.entry.value as storev1.StoreAgentUpdate;
    expect(update.agentInfo.case).toBe("bash");
    expect((update.agentInfo.value as storev1.StoreAgentBash).run?.value).toBe("run-1");
  });

  it("stamps the stream plane on every row it writes", () => {
    const entry = toStoreEntry(PRODUCER, readEntry(BOOK, "unit-1", "/tmp/a"));

    expect(entry.plane?.plane.case).toBe("stream");
  });

  it("refuses an entry with an empty upsert key, which would collide with every other", () => {
    const broken = { ...readEntry(BOOK, "unit-1", "/tmp/a"), upsertKey: "" };

    expect(() => toStoreEntry(PRODUCER, broken)).toThrow(/empty upsert key/);
  });

  it("carries no cursor advance: a stream-plane producer has no file to be positioned in", () => {
    const request = toWriteBatchRequest(PRODUCER, [readEntry(BOOK, "unit-1", "/tmp/a")]);

    expect(request.batch?.cursorAdvance).toBeUndefined();
  });

  it("mints the same write id for the same frame re-sent, so the store absorbs the replay", () => {
    const first = toStoreEntry(PRODUCER, readEntry(BOOK, "unit-1", "/tmp/a"));
    const second = toStoreEntry(PRODUCER, readEntry(BOOK, "unit-1", "/tmp/a"));

    expect(second.writeId).toBe(first.writeId);
  });

  it("mints different write ids for two arms of one vendor record", () => {
    const start = toStoreEntry(PRODUCER, readEntry(BOOK, "unit-1", "/tmp/a", { vendorUuid: "u" }));
    const other = toStoreEntry(PRODUCER, {
      ...readEntry(BOOK, "unit-1", "/tmp/a", { vendorUuid: "u" }),
      source: { vendorUuid: "u", discriminator: "activity.read.success" },
    });

    expect(other.writeId).not.toBe(start.writeId);
  });
});

describe("writeDurable", () => {
  it("resolves only once the store says the batch is durable", async () => {
    const { store: fake, persistence: plane } = await persistence("durable");

    await plane.writeDurable([promptEntry(BOOK, "turn-1", "hello")]);

    expect(fake.book("book-1")).toHaveLength(1);
  });

  it("rejects with store_unavailable when the retry schedule is exhausted", async () => {
    const { store: fake, persistence: plane } = await persistence("durable-fail");
    fake.failWrites("the store is down");

    await expect(plane.writeDurable([promptEntry(BOOK, "turn-1", "hello")])).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });
});

describe("the bounded retry buffer", () => {
  it("replays a transient failure silently and lands the row", async () => {
    const { store: fake, persistence: plane } = await persistence("replay");
    fake.failWrites("transient");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    // The first attempt fails; the store recovers before the schedule runs out.
    await new Promise((resolve) => setImmediate(resolve));
    fake.failWrites(null);

    await plane.flush();

    expect(fake.book("book-1")).toHaveLength(1);
  });

  it("drops loudly and raises a store_unreachable fault when the store stays down", async () => {
    const { store: fake, persistence: plane } = await persistence("exhausted");
    const faults: conversationv1.SessionFault[] = [];
    plane.onFault((fault) => faults.push(fault));
    fake.failWrites("the store is down");

    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    expect(faults.map((fault) => fault.kind.case)).toContain("storeUnreachable");
    expect(fake.book("book-1")).toHaveLength(0);
  });

  it("does NOT replay a batch the store called invalid_request: the same bytes cannot become valid", async () => {
    // Arrange.
    const { store: fake, persistence: plane } = await persistence("invalid-once");
    fake.failWritesWith("invalid_request", "entry 0 sets no item arm");

    // Act.
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    // Assert. ONE attempt, not the whole schedule.
    expect(fake.writes()).toHaveLength(1);
  });

  it("keeps the queue draining after a refused batch, so a later batch still lands", async () => {
    // Arrange.
    const { store: fake, persistence: plane } = await persistence("invalid-drains");
    fake.failWritesWith("invalid_request", "entry 0 sets no item arm");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    // Act.
    fake.failWritesWith(null, "");
    plane.write([readEntry(BOOK, "unit-2", "/tmp/b")]);
    await plane.flush();

    // Assert.
    expect(fake.book("book-1")).toHaveLength(1);
  });

  it("raises the refusal as a fault, so a refused batch is surfaced rather than swallowed", async () => {
    // Arrange.
    const { store: fake, persistence: plane } = await persistence("invalid-fault");
    const faults: conversationv1.SessionFault[] = [];
    plane.onFault((fault) => faults.push(fault));
    fake.failWritesWith("invalid_request", "entry 0 sets no item arm");

    // Act.
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    // Assert.
    expect(faults.map((fault) => fault.kind.case)).toContain("storeUnreachable");
  });

  it("opens a degraded window while the store is unreachable", async () => {
    const { store: fake, persistence: plane } = await persistence("degraded-open");
    const windows: conversationv1.SessionDegradedWindow[] = [];
    plane.onDegradedWindow((window) => windows.push(window));
    fake.failWrites("the store is down");

    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    expect(windows[0]?.extent.case).toBe("open");
    expect(windows[0]?.component).toBe("store-writer");
  });

  it("announces the degraded window BEFORE the fault that reports it", async () => {
    // A fault restates the session's diagnostics, and a consumer reading the
    // first unhealthy diagnostics has to see the window that explains it.
    const { store: fake, persistence: plane } = await persistence("degraded-order");
    const order: string[] = [];
    plane.onDegradedWindow(() => order.push("window"));
    plane.onFault(() => order.push("fault"));
    fake.failWrites("the store is down");

    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    expect(order[0]).toBe("window");
  });

  it("reports the rows THIS flush lost, not the writer's lifetime total", async () => {
    // The stand-down's exit code answers "did the writes this flush waited for
    // land"; an outage the session already recovered from is not a dirty exit.
    const { store: fake, persistence: plane } = await persistence("flush-scoped");
    fake.failWrites("the store is down");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();
    fake.failWrites(null);

    plane.write([readEntry(BOOK, "unit-2", "/tmp/b")]);
    const outcome = await plane.flush();

    expect(outcome.lostRows).toBe(0);
  });

  it("reports the rows a flush watched being dropped", async () => {
    const { store: fake, persistence: plane } = await persistence("flush-lost");
    fake.failWrites("the store is down");

    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    const outcome = await plane.flush();

    expect(outcome.lostRows).toBe(1);
  });

  it("closes the degraded window with what was lost once a write succeeds again", async () => {
    const { store: fake, persistence: plane } = await persistence("degraded-close");
    const windows: conversationv1.SessionDegradedWindow[] = [];
    plane.onDegradedWindow((window) => windows.push(window));
    fake.failWrites("the store is down");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();
    fake.failWrites(null);

    plane.write([readEntry(BOOK, "unit-2", "/tmp/b")]);
    await plane.flush();

    const closed = windows.find((window) => window.extent.case === "closed");
    expect(closed).toBeDefined();
    const extent = closed?.extent.value as conversationv1.SessionDegradedClosed;
    expect(extent.droppedCount).toBe(1n);
  });

  it("evicts the oldest batch when the buffer is full, naming what was lost", async () => {
    const started = await startFakeStore(socketPathForTest("capacity"));
    store = started;
    started.failWrites("the store is down");
    const plane = createPersistence({
      client: createStoreClient(started.socketPath),
      producer: PRODUCER,
      nowMs: () => 1_000,
      sleep: async () => undefined,
      retry: { bufferCapacity: 1, backoffMs: [0], maxAttempts: 2 },
    });

    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    plane.write([readEntry(BOOK, "unit-2", "/tmp/b")]);
    plane.write([readEntry(BOOK, "unit-3", "/tmp/c")]);
    await plane.flush();

    // Nothing landed — the point is that the buffer stayed bounded rather than
    // growing to hold every batch a dead store never accepted.
    expect(started.book("book-1")).toHaveLength(0);
  });
});

describe("flush", () => {
  it("resolves at once when nothing is buffered", async () => {
    const { persistence: plane } = await persistence("flush-empty");

    await expect(plane.flush()).resolves.toEqual({ lostRows: 0 });
  });

  it("awaits every buffered write before resolving", async () => {
    const { store: fake, persistence: plane } = await persistence("flush-waits");

    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    plane.write([readEntry(BOOK, "unit-2", "/tmp/b")]);
    await plane.flush();

    expect(fake.book("book-1")).toHaveLength(2);
  });
});

describe("upsert by identity", () => {
  it("replaces a unit's row in place when a later frame of it is written", async () => {
    const { store: fake, persistence: plane } = await persistence("upsert");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    plane.write([
      {
        ...readEntry(BOOK, "unit-1", "/tmp/a"),
        source: { vendorUuid: "uuid-unit-1", discriminator: "activity.read.success" },
      },
    ]);
    await plane.flush();

    expect(fake.book("book-1")).toHaveLength(1);
  });

  it("keeps a keep-alive row out of every book", async () => {
    const { store: fake, persistence: plane } = await persistence("keepalive");

    plane.write([readEntry(BOOK, "unit-1", "/tmp/a", { keepalive: true })]);
    await plane.flush();

    expect(fake.book("book-1")).toHaveLength(0);
    expect(fake.unserved()).toHaveLength(1);
  });
});

describe("the session-update arm", () => {
  it("lands a session fact as a session row rather than a page line", async () => {
    const { store: fake, persistence: plane } = await persistence("session");

    plane.write([
      {
        agentId: BOOK,
        upsertKey: "session:compacting:uuid-1",
        source: { vendorUuid: "uuid-1", discriminator: "session_update.compacting" },
        keepalive: false,
        item: {
          kind: "session_update",
          update: create(conversationv1.SessionUpdateSchema, {
            update: {
              case: "compacting",
              value: create(conversationv1.SessionCompactingSchema, {}),
            },
          }),
        },
      },
    ]);
    await plane.flush();

    expect(fake.sessionUpdates()).toHaveLength(1);
    expect(fake.book("book-1")).toHaveLength(0);
  });
});

describe("the producer's name", () => {
  it("refuses a write before StartSession named the conversation", async () => {
    const started = await startFakeStore(socketPathForTest("unnamed"));
    store = started;
    const plane = createPersistence({
      client: createStoreClient(started.socketPath),
      nowMs: () => 1_000,
      sleep: async () => undefined,
    });

    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    // A row landed under a placeholder name would have write ids in a namespace
    // no later replay could absorb against, so it would double on the first
    // retry after the real name arrived. Nothing lands instead.
    expect(started.book("book-1")).toHaveLength(0);
  });

  it("writes once StartSession has named it", async () => {
    const started = await startFakeStore(socketPathForTest("named"));
    store = started;
    const plane = createPersistence({
      client: createStoreClient(started.socketPath),
      nowMs: () => 1_000,
      sleep: async () => undefined,
    });

    plane.setProducer("vendor-session-1");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    expect(started.writes()[0]?.producer).toBe("claude-shim:vendor-session-1");
  });

  it("accepts the same name twice, since a resume names the same original id", async () => {
    const { persistence: plane } = await persistence("named-twice");

    plane.setProducer("vendor-session-1");

    expect(() => plane.setProducer("vendor-session-1")).not.toThrow();
  });

  it("refuses a DIFFERENT name: a conversation has one original vendor session id", async () => {
    const { persistence: plane } = await persistence("re-keyed");
    plane.setProducer("vendor-session-1");

    expect(() => plane.setProducer("vendor-session-2")).toThrow(/cannot become/);
  });
});

describe("clearProducer", () => {
  it("is a no-op when nothing was ever named", async () => {
    const { persistence: plane } = await persistence("clear-unnamed");

    expect(() => plane.clearProducer()).not.toThrow();
  });

  it("un-names an attempt that named the producer but wrote nothing", async () => {
    const { persistence: plane } = await persistence("clear-before-write");
    plane.setProducer("vendor-session-1");

    plane.clearProducer();

    // Un-named again is legal: a caller may name it fresh, exactly as if this
    // attempt had never happened.
    expect(() => plane.setProducer("vendor-session-2")).not.toThrow();
  });

  it("refuses to un-name a producer that already wrote rows", async () => {
    const { persistence: plane } = await persistence("clear-after-write");
    plane.setProducer("vendor-session-1");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    expect(() => plane.clearProducer()).toThrow(/has already written rows/);
  });
});

describe("liveWork", () => {
  it("delegates to the reconciler's own answer", async () => {
    const { persistence: plane } = await persistence("writer-live-work");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    const live = await plane.liveWork();

    expect(live.liveAgents.map((id) => id.value)).toContain("book-1");
  });
});

describe("the default backoff sleep", () => {
  it("lets a write that fails then recovers still land, using the real timer", async () => {
    const started = await startFakeStore(socketPathForTest("default-sleep"));
    store = started;
    const plane = createPersistence({
      client: createStoreClient(started.socketPath),
      producer: PRODUCER,
      nowMs: () => 1_000,
      // No `sleep` override: exercises the module's own real, unref'd
      // setTimeout-based default rather than a test double.
      retry: { ...DEFAULT_RETRY_POLICY, backoffMs: [20, 20, 20, 20] },
    });
    started.failWrites("transient");

    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    // Recover the store the moment the first attempt has actually been
    // REFUSED, rather than after a wall-clock sleep. A sleep races the real
    // backoff schedule: under load the four retries can burn while this thread
    // is descheduled, and the recovery then lands after the batch is already
    // lost. The refusal is the fact worth waiting for, and it is observable.
    for (let i = 0; i < 1_000; i++) {
      if (started.writeBatches().some((batch) => !batch.accepted)) break;
      await new Promise((resolve) => setImmediate(resolve));
    }
    started.failWrites(null);

    await plane.flush();

    expect(started.book("book-1")).toHaveLength(1);
  });
});
