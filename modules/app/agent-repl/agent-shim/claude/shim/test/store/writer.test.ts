/**
 * The WRITE half: routing, the durable ack, replay absorption, and the loud drop.
 *
 * Every test drives the real writer against the in-process fake store, because
 * the behaviors that matter — upsert by key, durable-or-nothing, write-id
 * absorption — are the STORE's semantics, and a hand-rolled double would be
 * asserting our own beliefs about them rather than the contract.
 */
import { afterEach, describe, expect, it } from "vitest";
import { logRecordsSince, logSinkMark } from "../log-records.js";
import { create, toBinary } from "@bufbuild/protobuf";
import { conversationv1, storev1 } from "../../src/proto.js";
import { createStoreClient, type StoreClient } from "../../src/store/client.js";
import { producerId } from "../../src/store/keys.js";
import {
  DEFAULT_BATCH_POLICY,
  DEFAULT_RETRY_POLICY,
  type Persistence,
  type PersistenceBatchPolicy,
  type PersistEntry,
} from "../../src/store/persistence.js";
import { createPersistence, toStoreEntry, toWriteBatchRequest } from "../../src/store/writer.js";
import { startFakeStore, type FakeStore } from "../fakes/store-server.js";
import {
  agent,
  bashRunEntry,
  promptEntry,
  readEntry,
  socketPathForTest,
  spawnEntry,
  terminalEntry,
} from "./persistence-fixtures.js";

const PRODUCER = producerId("vendor-session-1");
const BOOK = agent("book-1");

let store: FakeStore | undefined;

afterEach(async () => {
  await store?.close();
  store = undefined;
});

/**
 * A backoff that is FREE through the retry schedule and then PARKS.
 *
 * THE WRITER NEVER GIVES UP ON A HELD BATCH: past the schedule it retries for
 * as long as the process lives. A free sleep would make that a spin against a
 * store the test keeps down, so the sleep after the `free`-th parks until the
 * test calls `release()` -- typically once it has brought the store back.
 */
function schedule(free = DEFAULT_RETRY_POLICY.maxAttempts - 1): {
  sleep: (ms: number) => Promise<void>;
  release: () => void;
} {
  let calls = 0;
  let parked: (() => void)[] = [];
  return {
    sleep: () => {
      calls += 1;
      if (calls <= free) return Promise.resolve();
      return new Promise<void>((resolve) => {
        parked.push(resolve);
      });
    },
    release: () => {
      calls = 0;
      const waiting = parked;
      parked = [];
      for (const resolve of waiting) resolve();
    },
  };
}

/** A persistence over a fresh fake store, with a fast, deterministic backoff. */
async function persistence(
  name: string,
  free?: number,
): Promise<{ store: FakeStore; persistence: Persistence; release: () => void }> {
  const started = await startFakeStore(socketPathForTest(name));
  store = started;
  const backoff = schedule(free);
  return {
    store: started,
    release: backoff.release,
    persistence: createPersistence({
      client: createStoreClient(started.socketPath),
      producer: PRODUCER,
      nowMs: () => 1_000,
      // The schedule's SHAPE is what is under test, never its wall-clock cost.
      sleep: backoff.sleep,
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

  it("stamps the envelope with the turn the row was produced within", () => {
    const entry = toStoreEntry(PRODUCER, promptEntry(BOOK, "turn-1", "hello"));

    expect(entry.turn?.value).toBe("turn-1");
  });

  it("leaves the envelope unstamped for a row produced outside any turn", () => {
    const entry = toStoreEntry(PRODUCER, readEntry(BOOK, "unit-1", "/tmp/a"));

    expect(entry.turn).toBeUndefined();
  });

  it("routes a peer message to a servable page line of the recipient's book", () => {
    const entry = toStoreEntry(PRODUCER, {
      agentId: BOOK,
      upsertKey: "peer:u1",
      source: { vendorUuid: "u1", discriminator: "peer_message" },
      keepalive: false,
      turn: undefined,
      item: {
        kind: "peer",
        peer: create(conversationv1.PeerMessageSchema, { agent: BOOK, sender: "Explore", body: "hi", id: "u1" }),
      },
    });

    const update = entry.entry.value as storev1.StoreAgentUpdate;
    expect(update.agentInfo.case).toBe("serveableFrame");
    const item = (update.agentInfo.value as storev1.StorePageLine).agentItem;
    expect(item?.item.case).toBe("peerMessage");
  });

  it("refuses to envelope a keep-alive turn's entry, which the door should have dropped", () => {
    expect(() => toStoreEntry(PRODUCER, readEntry(BOOK, "unit-1", "/tmp/a", { keepalive: true }))).toThrow(
      /keep-alive entry .* reached the envelope; nothing of a keep-alive is stored/,
    );
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

  it("states the interactive class: a live frame goes ahead of any queued bulk copy", () => {
    const request = toWriteBatchRequest(PRODUCER, [readEntry(BOOK, "unit-1", "/tmp/a")]);

    expect(request.writeClass?.writeClass.case).toBe("interactive");
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

  it("rejects with store_unavailable once the store is known to be down", async () => {
    const { store: fake, persistence: plane } = await persistence("durable-fail");
    fake.failWrites("the store is down");

    await expect(plane.writeDurable([promptEntry(BOOK, "turn-1", "hello")])).rejects.toMatchObject({
      kind: "store_unavailable",
    });
  });

  it("makes ONE attempt at an unreachable store, leaving the schedule to the retry buffer", async () => {
    // Arrange. The caller of a durable write holds an RPC open, and the retry
    // schedule is longer than the deadline that RPC is held under. The backoff
    // parks from the first retry, so the count is the caller's own attempt.
    const { store: fake, persistence: plane } = await persistence("durable-one-attempt", 0);
    fake.failWrites("the store is down");

    // Act.
    await expect(plane.writeDurable([promptEntry(BOOK, "turn-1", "hello")])).rejects.toMatchObject({
      kind: "store_unavailable",
    });

    // Assert.
    expect(fake.writes()).toHaveLength(1);
  });

  it("keeps a durable row it could not ack HELD, and lands it once the store answers", async () => {
    // Arrange. A rejection releases the caller; it never costs the record a row.
    const { store: fake, persistence: plane, release } = await persistence("durable-held");
    fake.failWrites("the store is down");
    await expect(plane.writeDurable([promptEntry(BOOK, "turn-1", "hello")])).rejects.toMatchObject({
      kind: "store_unavailable",
    });

    // Act.
    fake.failWrites(null);
    release();
    await plane.flush();

    // Assert.
    expect(fake.book("book-1")).toHaveLength(1);
  });

  it("refuses without attempting at all while a degraded window is already open", async () => {
    // Arrange. The outage is already known, so there is nothing to learn from
    // one more inline attempt and nothing to wait for behind the drain.
    const { store: fake, persistence: plane } = await persistence("durable-already-degraded");
    fake.failWrites("the store is down");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();
    const attempted = fake.writes().length;

    // Act.
    await expect(plane.writeDurable([promptEntry(BOOK, "turn-1", "hello")])).rejects.toMatchObject({
      kind: "store_unavailable",
    });

    // Assert.
    expect(fake.writes()).toHaveLength(attempted);
  });
});

/** The upsert keys of every WriteBatch the fake store received, in order. */
function writtenKeys(fake: FakeStore): string[] {
  return fake.writes().flatMap((request) => (request.batch?.entries ?? []).map((entry) => entry.upsertKey));
}

describe("writeDurable's place in the buffer", () => {
  it("lands behind every batch buffered before it", async () => {
    // Arrange
    const { store: fake, persistence: plane } = await persistence("durable-behind-buffered");
    const first = readEntry(BOOK, "unit-1", "/tmp/a");
    const second = readEntry(BOOK, "unit-2", "/tmp/b");
    const prompt = promptEntry(BOOK, "turn-1", "hello");
    plane.write([first]);
    plane.write([second]);

    // Act
    await plane.writeDurable([prompt]);

    // Assert
    expect(writtenKeys(fake)).toEqual([first.upsertKey, second.upsertKey, prompt.upsertKey]);
  });

  it("is not starved by batches enqueued after it while it waits", async () => {
    // Arrange: a producer that enqueues a fresh batch each time a store call
    // answers, for as long as the durable write is outstanding, so the buffer
    // never goes idle -- a keep-alive's rows, a detached shell's spool.
    const started = await startFakeStore(socketPathForTest("durable-busy-buffer"));
    store = started;
    const real = createStoreClient(started.socketPath);
    const cap = 200;
    let durable = false;
    let fed = 0;
    // The client is built before the writer it feeds, so the writer is reached
    // through this slot.
    const target: { plane?: Persistence } = {};
    const client: StoreClient = {
      ...real,
      writeBatch: async (request) => {
        const response = await real.writeBatch(request);
        if (!durable && fed < cap) {
          fed += 1;
          target.plane?.write([readEntry(BOOK, `fed-${fed}`, "/tmp/fed")]);
        }
        return response;
      },
    };
    const plane = createPersistence({
      client,
      producer: PRODUCER,
      nowMs: () => 1_000,
      sleep: async () => undefined,
      retry: { ...DEFAULT_RETRY_POLICY, backoffMs: [0, 0, 0, 0] },
    });
    target.plane = plane;
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);

    // Act
    await plane.writeDurable([promptEntry(BOOK, "turn-1", "hello")]);
    durable = true;

    // Assert: it waited out the one batch ahead of it, not the producer.
    expect(fed).toBeLessThan(cap);
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

  it("raises a store_unreachable fault when the store stays down", async () => {
    const { store: fake, persistence: plane } = await persistence("exhausted");
    const faults: conversationv1.SessionFault[] = [];
    plane.onFault((fault) => faults.push(fault));
    fake.failWrites("the store is down");

    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    expect(faults.map((fault) => fault.kind.case)).toContain("storeUnreachable");
  });

  it("drops NOTHING when the store stays down past the schedule: the held row lands once it answers", async () => {
    // Arrange. The whole schedule fails, so the failure is declared persistent.
    const { store: fake, persistence: plane, release } = await persistence("exhausted-held");
    fake.failWrites("the store is down");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    // Act.
    fake.failWrites(null);
    release();
    await plane.flush();

    // Assert.
    expect(fake.book("book-1")).toHaveLength(1);
  });

  it("states a persistent failure at ERROR, naming the held keys", async () => {
    // Arrange.
    const { store: fake, persistence: plane } = await persistence("exhausted-error");
    fake.failWrites("the store is down");
    const before = logSinkMark();

    // Act.
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    // Assert.
    const record = logRecordsSince(before).find(
      (entry) => entry.level === "error" && String(entry.message).includes("whole retry schedule"),
    );
    expect((record?.context)?.held_upsert_keys).toEqual([
      "activity:unit-1",
    ]);
  });

  it("states at INFO that the held batch landed once the store answers again", async () => {
    // Arrange.
    const { store: fake, persistence: plane, release } = await persistence("exhausted-recovered");
    fake.failWrites("the store is down");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();
    const before = logSinkMark();

    // Act.
    fake.failWrites(null);
    release();
    await plane.flush();

    // Assert.
    expect(
      logRecordsSince(before).some(
        (entry) => entry.level === "info" && String(entry.message).includes("nothing was dropped"),
      ),
    ).toBe(true);
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

  /**
   * A REFUSAL IS THE STORE ANSWERING, NOT THE STORE FAILING.
   *
   * This used to assert `storeUnreachable`, which was a false diagnosis: the
   * store read the batch and named a malformed row, so it was plainly
   * reachable. The refusal is still surfaced as a fault -- nothing is swallowed
   * -- on the arm that names its real cause.
   */
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
    expect(faults.map((fault) => fault.kind.case)).toContain("converterDefect");
  });

  it("opens no degraded window for a refusal: nothing about the store is degraded", async () => {
    // Arrange.
    const { store: fake, persistence: plane } = await persistence("invalid-no-window");
    const windows: conversationv1.SessionDegradedWindow[] = [];
    plane.onDegradedWindow((window) => windows.push(window));
    fake.failWritesWith("invalid_request", "entry 0 sets no item arm");

    // Act.
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    // Assert.
    expect(windows).toHaveLength(0);
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
    const { store: fake, persistence: plane, release } = await persistence("flush-scoped");
    fake.failWrites("the store is down");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();
    fake.failWrites(null);
    release();

    plane.write([readEntry(BOOK, "unit-2", "/tmp/b")]);
    const outcome = await plane.flush();

    expect(outcome.lostRows).toBe(0);
  });

  it("counts the rows a flush left HELD under a persistent failure", async () => {
    const { store: fake, persistence: plane } = await persistence("flush-lost");
    fake.failWrites("the store is down");

    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    const outcome = await plane.flush();

    expect(outcome.lostRows).toBe(1);
  });

  it("closes the degraded window with NOTHING dropped once the held row lands", async () => {
    const { store: fake, persistence: plane, release } = await persistence("degraded-close");
    const windows: conversationv1.SessionDegradedWindow[] = [];
    plane.onDegradedWindow((window) => windows.push(window));
    fake.failWrites("the store is down");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();
    fake.failWrites(null);
    release();

    plane.write([readEntry(BOOK, "unit-2", "/tmp/b")]);
    await plane.flush();

    const closed = windows.find((window) => window.extent.case === "closed");
    expect(closed).toBeDefined();
    const extent = closed?.extent.value as conversationv1.SessionDegradedClosed;
    expect(extent.droppedCount).toBe(0n);
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

});

describe("a keep-alive turn's entries", () => {
  it("are never sent to the store by write", async () => {
    const { store: fake, persistence: plane } = await persistence("keepalive-write");

    plane.write([readEntry(BOOK, "unit-1", "/tmp/a", { keepalive: true })]);
    await plane.flush();

    expect(fake.writes()).toHaveLength(0);
  });

  it("are never sent to the store by writeDurable", async () => {
    const { store: fake, persistence: plane } = await persistence("keepalive-durable");

    await plane.writeDurable([readEntry(BOOK, "unit-1", "/tmp/a", { keepalive: true })]);

    expect(fake.writes()).toHaveLength(0);
  });

  it("are dropped from a batch that also carries real entries, which still land", async () => {
    const { store: fake, persistence: plane } = await persistence("keepalive-mixed");

    plane.write([
      readEntry(BOOK, "unit-ka", "/tmp/ka", { keepalive: true }),
      readEntry(BOOK, "unit-real", "/tmp/real"),
    ]);
    await plane.flush();

    const keys = fake.writes().flatMap((request) => request.batch?.entries.map((entry) => entry.upsertKey) ?? []);
    expect(keys).toEqual([readEntry(BOOK, "unit-real", "/tmp/real").upsertKey]);
  });

  it("do not count as rows written under the producer", async () => {
    const { persistence: plane } = await persistence("keepalive-producer");

    plane.write([readEntry(BOOK, "unit-1", "/tmp/a", { keepalive: true })]);
    await plane.flush();

    expect(plane.producerHasWrittenRows()).toBe(false);
  });

  it("are each stated at debug, naming no upsert key", async () => {
    const { persistence: plane } = await persistence("keepalive-log");
    const before = logSinkMark();

    plane.write([readEntry(BOOK, "unit-1", "/tmp/a", { keepalive: true })]);
    await plane.flush();

    const drops = logRecordsSince(before).filter(
      (record) => record.message === "a keep-alive turn's entry is never stored; dropped before the batch",
    );
    expect(drops.map((record) => [record.level, record.context.upsert_key])).toEqual([
      ["debug", undefined],
    ]);
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
        turn: undefined,
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

  it("stays a no-op on a writer that was never named, even after rows were attempted", async () => {
    // The early return comes BEFORE the already-wrote check, so an unnamed
    // writer whose rows never made it past the name check is still free.
    // Arrange.
    const started = await startFakeStore(socketPathForTest("clear-never-named"));
    store = started;
    const plane = createPersistence({
      client: createStoreClient(started.socketPath),
      producer: undefined,
      nowMs: () => 1_000,
      sleep: async () => undefined,
      retry: { ...DEFAULT_RETRY_POLICY, backoffMs: [0, 0, 0, 0] },
    });
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    // Act, Assert. The rows never landed under any name, so nothing is claimed.
    expect(() => plane.clearProducer()).not.toThrow();
    expect(() => plane.setProducer("vendor-session-1")).not.toThrow();
  });

  it("refuses to un-name a producer that already wrote rows", async () => {
    const { persistence: plane } = await persistence("clear-after-write");
    plane.setProducer("vendor-session-1");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    expect(() => plane.clearProducer()).toThrow(/has already written rows/);
  });
});

describe("producerHasWrittenRows", () => {
  it("says no before anything is written, so an abandoned attempt may un-name", async () => {
    const { persistence: plane } = await persistence("wrote-none");
    plane.setProducer("vendor-session-1");

    expect(plane.producerHasWrittenRows()).toBe(false);
  });

  it("says yes once a row has been handed over, so the caller keeps the name", async () => {
    // THE QUESTION `clearProducer` ANSWERS BY THROWING, asked instead. A start
    // the vendor opened and then refused has already written through the
    // converter, and its caller must learn that from a question rather than
    // from an exception escaping a typed verb.
    const { persistence: plane } = await persistence("wrote-some");
    plane.setProducer("vendor-session-1");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    expect(plane.producerHasWrittenRows()).toBe(true);
  });

  it("re-announcing the SAME name after rows is the no-op a retry needs", async () => {
    const { persistence: plane } = await persistence("re-announce");
    plane.setProducer("vendor-session-1");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    expect(() => plane.setProducer("vendor-session-1")).not.toThrow();
  });
});

describe("liveWork", () => {
  it("delegates to the reconciler's own answer", async () => {
    const { persistence: plane } = await persistence("writer-live-work");
    plane.write([spawnEntry(agent("main-1"), "book-1"), readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    const live = await plane.liveWork(agent("main-1"));

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

// ---------------------------------------------------------------------------
// The residue arm, and the row with nothing servable in it
// ---------------------------------------------------------------------------

/** A row the shim recorded but no book serves: a vendor record it cannot model. */
function residueEntry(): PersistEntry {
  return {
    agentId: BOOK,
    upsertKey: "residue:1",
    source: { vendorUuid: "uuid-residue", discriminator: "residue.unknown" },
    keepalive: false,
    turn: undefined,
    item: {
      kind: "residue",
      residue: create(storev1.StoreUnservedItemSchema, {
        unservedItem: {
          case: "unknown",
          value: create(storev1.StoreUnknownSchema, {}),
        },
      }),
    },
  };
}

describe("the residue arm", () => {
  it("lands a residue row under unserved_item, so no book ever serves it", () => {
    // Arrange, Act.
    const entry = toStoreEntry(PRODUCER, residueEntry());

    // Assert.
    const update = entry.entry.value as storev1.StoreAgentUpdate;
    expect(update.agentInfo.case).toBe("unservedItem");
  });

  it("names no top-level book for a residue row, which belongs to no agent's page", () => {
    // Arrange, Act.
    const entry = toStoreEntry(PRODUCER, residueEntry());

    // Assert.
    const update = entry.entry.value as storev1.StoreAgentUpdate;
    expect(update.topLevel).toBeUndefined();
  });
});

describe("an entry whose kind has nothing servable in it", () => {
  it("refuses loudly rather than writing a row with an empty agent_info arm", () => {
    // A kind the router does not know cannot be turned into a store arm, and a
    // row with no arm at all would be a blank line in someone's feed.
    // Arrange.
    const broken = {
      ...readEntry(BOOK, "unit-1", "/tmp/a"),
      item: { kind: "not_a_kind" } as unknown as PersistEntry["item"],
    };

    // Act, Assert.
    expect(() => toStoreEntry(PRODUCER, broken)).toThrow(/no servable item/);
  });
});

// ---------------------------------------------------------------------------
// A store that answers a WriteBatch badly
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

/** A persistence over a hand-built client, with no wall-clock backoff at all. */
function planeOver(overrides: Partial<StoreClient>): Persistence {
  return createPersistence({
    client: stubClient(overrides),
    producer: PRODUCER,
    nowMs: () => 1_000,
    sleep: schedule().sleep,
    retry: { ...DEFAULT_RETRY_POLICY, backoffMs: [0, 0, 0, 0] },
  });
}

describe("a WriteBatch the store answers badly", () => {
  it("reports every entry the store skipped as a book conflict at error, naming the key and both books", async () => {
    // A durable batch that left a row out of the book it named is not a quiet
    // success: the daemon watches that book and will never see the row.
    // Arrange.
    const plane = planeOver({
      writeBatch: async () =>
        create(storev1.WriteBatchResponseSchema, {
          result: {
            case: "success",
            value: create(storev1.WriteBatchSuccessSchema, {
              skipped: [
                create(storev1.WriteBatchSkippedEntrySchema, {
                  upsertKey: "activity:msg_1:1",
                  fromBook: "rotated-book",
                  toBook: "book-1",
                }),
              ],
            }),
          },
        }),
    });
    const before = logSinkMark();

    // Act.
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    // Assert.
    const records = logRecordsSince(before).filter((record) => typeof record.message === "string" && record.message.startsWith("the store skipped a row"));
    expect(
      records.map((record) => {
        const context = record.context;
        return [record.level, context.upsert_key, context.from_book, context.to_book];
      }),
    ).toEqual([["error", "activity:msg_1:1", "rotated-book", "book-1"]]);
  });

  it("states nothing when the store skipped nothing", async () => {
    // Arrange.
    const plane = planeOver({
      writeBatch: async () =>
        create(storev1.WriteBatchResponseSchema, {
          result: { case: "success", value: create(storev1.WriteBatchSuccessSchema, {}) },
        }),
    });
    const before = logSinkMark();

    // Act.
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    // Assert.
    const reported = logRecordsSince(before).filter((record) => record.level === "warn" || record.level === "error");
    expect(reported).toEqual([]);
  });

  it("renders a thrown non-Error as the fault's detail", async () => {
    // A rejected transport can carry anything at all; the fault must still say
    // something a reader of the diagnostics can act on.
    // Arrange.
    const plane = planeOver({ writeBatch: () => Promise.reject("the socket said no") });
    const faults: string[] = [];
    plane.onFault((fault) => faults.push(fault.detail));

    // Act.
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    // Assert.
    expect(faults).toContain("the socket said no");
  });

  it("treats an answer with no result arm as a refusal, never as durable", async () => {
    // A response that says neither durable nor failed cannot be acted on
    // either way, and calling it durable would lose the row silently.
    // Arrange.
    const plane = planeOver({
      writeBatch: async () => create(storev1.WriteBatchResponseSchema, {}),
    });
    const faults: string[] = [];
    plane.onFault((fault) => faults.push(fault.detail));

    // Act.
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    const outcome = await plane.flush();

    // Assert.
    expect(outcome.lostRows).toBe(1);
    expect(faults).toContain("store answered a WriteBatch with no result arm set");
  });

  it("still replays on a retry policy that names no backoff at all", async () => {
    // Arrange.
    let calls = 0;
    const plane = createPersistence({
      client: stubClient({
        writeBatch: async () => {
          calls += 1;
          return calls === 1
            ? create(storev1.WriteBatchResponseSchema, {
                result: {
                  case: "failure",
                  value: create(storev1.WriteBatchFailureSchema, {
                    detail: "transient",
                    kind: {
                      case: "storageFailure",
                      value: create(storev1.WriteBatchStorageFailureSchema, {}),
                    },
                  }),
                },
              })
            : create(storev1.WriteBatchResponseSchema, {
                result: {
                  case: "success",
                  value: create(storev1.WriteBatchSuccessSchema, {}),
                },
              });
        },
      }),
      producer: PRODUCER,
      nowMs: () => 1_000,
      sleep: async () => undefined,
      retry: { ...DEFAULT_RETRY_POLICY, backoffMs: [] },
    });

    // Act.
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    const outcome = await plane.flush();

    // Assert. The empty schedule is a zero wait, not a lost row.
    expect(outcome.lostRows).toBe(0);
    expect(calls).toBe(2);
  });
});

describe("a durable write the store calls invalid_request", () => {
  it("drops the row rather than leaving it to be replayed as the same bad bytes", async () => {
    // Arrange.
    const { store: fake, persistence: plane } = await persistence("durable-invalid");
    fake.failWritesWith("invalid_request", "row 0 names no agent");

    // Act.
    await expect(plane.writeDurable([promptEntry(BOOK, "turn-1", "hello")])).rejects.toMatchObject({
      kind: "invalid_request",
    });
    fake.failWritesWith(null, "");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();

    // Assert. The refused row is GONE rather than queued: the same bytes
    // cannot become valid, so only the later row reaches the book.
    expect(fake.book("book-1")).toHaveLength(1);
  });
});

/**
 * A ROW THIS WRITER CANNOT ENVELOPE AT ALL.
 *
 * `toStoreEntry` refuses an entry with an empty upsert key (it would collide
 * with every other row) and one whose kind has no servable item. The throw
 * happens BEFORE anything reaches the wire, so the store is not involved: it is
 * the producer's defect, and reporting it as `store_unreachable` -- which is
 * what the transport `catch` did while the envelope was built inside it -- both
 * named the wrong component and replayed bytes that could never become valid.
 */
describe("a row the writer cannot envelope", () => {
  /** An entry `toStoreEntry` refuses: an empty upsert key. */
  const unenvelopable = () => ({ ...readEntry(BOOK, "unit-1", "/tmp/a"), upsertKey: "" });

  it("raises it as a converter defect, since the store never saw it", async () => {
    // Arrange.
    const { persistence: plane } = await persistence("envelope-defect-fault");
    const faults: conversationv1.SessionFault[] = [];
    plane.onFault((fault) => faults.push(fault));

    // Act.
    plane.write([unenvelopable()]);
    await plane.flush();

    // Assert.
    expect(faults.map((fault) => fault.kind.case)).toEqual(["converterDefect"]);
  });

  it("names the refusal in the fault's detail rather than a transport message", async () => {
    // Arrange.
    const { persistence: plane } = await persistence("envelope-defect-detail");
    const faults: conversationv1.SessionFault[] = [];
    plane.onFault((fault) => faults.push(fault));

    // Act.
    plane.write([unenvelopable()]);
    await plane.flush();

    // Assert.
    expect(faults[0]?.detail).toMatch(/empty upsert key/);
  });

  it("opens no degraded window, because the store is answering fine", async () => {
    // Arrange.
    const { persistence: plane } = await persistence("envelope-defect-window");
    const windows: conversationv1.SessionDegradedWindow[] = [];
    plane.onDegradedWindow((window) => windows.push(window));

    // Act.
    plane.write([unenvelopable()]);
    await plane.flush();

    // Assert.
    expect(windows).toHaveLength(0);
  });

  it("sends nothing to the store, rather than replaying bytes it could not build", async () => {
    // Arrange.
    const { store: fake, persistence: plane } = await persistence("envelope-defect-no-wire");

    // Act.
    plane.write([unenvelopable()]);
    await plane.flush();

    // Assert.
    expect(fake.writes()).toHaveLength(0);
  });

  it("keeps the queue draining, so a later well-formed batch still lands", async () => {
    // Arrange.
    const { store: fake, persistence: plane } = await persistence("envelope-defect-drains");
    plane.write([unenvelopable()]);
    await plane.flush();

    // Act.
    plane.write([readEntry(BOOK, "unit-2", "/tmp/b")]);
    await plane.flush();

    // Assert.
    expect(fake.book("book-1")).toHaveLength(1);
  });
});

describe("a write of nothing at all", () => {
  it("enqueues no batch for an empty buffered write", async () => {
    // Arrange.
    const { store: fake, persistence: plane } = await persistence("write-empty");

    // Act.
    plane.write([]);
    await plane.flush();

    // Assert.
    expect(fake.writes()).toHaveLength(0);
  });

  it("sends nothing for an empty durable write, rather than an empty batch", async () => {
    // Arrange.
    const { store: fake, persistence: plane } = await persistence("write-durable-empty");

    // Act.
    await plane.writeDurable([]);

    // Assert.
    expect(fake.writes()).toHaveLength(0);
  });
});

describe("which writes end a minted book's absence", () => {
  /**
   * One session-scoped row. It carries the MAIN agent in its envelope so it has
   * a book to be filed under, and lands as a session row that registers nothing.
   */
  function sessionUpdateEntry(book: conversationv1.AgentId): PersistEntry {
    return {
      agentId: book,
      upsertKey: "session-update-1",
      source: { vendorUuid: "uuid-session-1", discriminator: "session_update" },
      keepalive: false,
      turn: undefined,
      item: {
        kind: "session_update",
        update: create(conversationv1.SessionUpdateSchema, {
          update: {
            case: "identityRotated",
            value: create(conversationv1.SessionIdentityRotatedSchema, {
              previousVendorSessionId: "v-1",
              vendorSessionId: "v-2",
            }),
          },
        }),
      },
    };
  }

  it("a session update leaves the book absent, so the store is still not asked", async () => {
    // Arrange.
    const { store: fake, persistence: plane } = await persistence("minted-session-update");
    plane.noteAgentMinted("book-1");
    const session = await plane.openAgentPage(BOOK, 10, undefined, () => true);
    void session.tail[Symbol.asyncIterator]().next();

    // Act.
    await plane.writeDurable([sessionUpdateEntry(BOOK)]);

    // Assert. It registered no `agent` row, so the absence it would have ended
    // was never over.
    expect(fake.reads().filter((read) => read.rpc === "OpenAgentSession")).toHaveLength(0);
    session.close();
  });

  it("a prompt ends the absence, and the store is asked once", async () => {
    // Arrange.
    const { store: fake, persistence: plane } = await persistence("minted-prompt");
    plane.noteAgentMinted("book-1");
    const session = await plane.openAgentPage(BOOK, 10, undefined, () => true);
    const first = session.tail[Symbol.asyncIterator]().next();

    // Act.
    await plane.writeDurable([promptEntry(BOOK, "turn-1", "hello")]);
    await first;

    // Assert.
    expect(fake.reads().filter((read) => read.rpc === "OpenAgentSession")).toHaveLength(1);
    session.close();
  });
});

// ---------------------------------------------------------------------------
// Backpressure, bounded batches, and the order the store receives
// ---------------------------------------------------------------------------

const MAIN = agent("main-1");
const SUB = agent("sub-1");

/** The store's durable answer. */
function durable(): storev1.WriteBatchResponse {
  return create(storev1.WriteBatchResponseSchema, {
    result: { case: "success", value: create(storev1.WriteBatchSuccessSchema, {}) },
  });
}

/** The store's refusal of a malformed batch. */
function malformed(detail: string): storev1.WriteBatchResponse {
  return create(storev1.WriteBatchResponseSchema, {
    result: {
      case: "failure",
      value: create(storev1.WriteBatchFailureSchema, {
        detail,
        kind: { case: "invalidRequest", value: create(storev1.WriteBatchInvalidRequestSchema, {}) },
      }),
    },
  });
}

/** The upsert keys one request carried, in order. */
function keysOf(request: storev1.WriteBatchRequest | undefined): string[] {
  return (request?.batch?.entries ?? []).map((entry) => entry.upsertKey);
}

/**
 * A store that answers WriteBatch only when the test lets it, recording every
 * request in the order it arrived.
 *
 * `hold()` makes the NEXT answers wait; `open()` lets every waiting and later
 * answer through. The first request of a drain is issued synchronously inside
 * `write()`, so a held store has exactly one batch in flight the moment the
 * first write returns — the in-flight write every later row queues behind.
 */
function gatedStore(answer: (request: storev1.WriteBatchRequest) => storev1.WriteBatchResponse = durable): {
  client: StoreClient;
  requests: storev1.WriteBatchRequest[];
  hold: () => void;
  open: () => void;
} {
  const requests: storev1.WriteBatchRequest[] = [];
  let gate: Promise<void> | undefined;
  let release: () => void = () => undefined;
  return {
    requests,
    client: stubClient({
      writeBatch: async (request) => {
        requests.push(request);
        if (gate !== undefined) await gate;
        return answer(request);
      },
    }),
    hold: () => {
      gate = new Promise<void>((resolve) => {
        release = resolve;
      });
    },
    open: () => {
      gate = undefined;
      release();
    },
  };
}

/** A writer over `client`, named for the MAIN book, with the given bounds. */
function boundedPlane(
  client: StoreClient,
  batching: Partial<PersistenceBatchPolicy> = {},
  nowMs: () => number = () => 1_000,
): Persistence {
  const plane = createPersistence({
    client,
    nowMs,
    sleep: schedule().sleep,
    retry: { ...DEFAULT_RETRY_POLICY, backoffMs: [0, 0, 0, 0] },
    batching: { ...DEFAULT_BATCH_POLICY, ...batching },
  });
  plane.setProducer(MAIN.value);
  return plane;
}

/** Let every pending I/O and microtask settle, so a promise that could resolve has. */
const settle = (): Promise<void> => new Promise((resolve) => setImmediate(resolve));

describe("backpressure, never eviction", () => {
  const MARKS = { backlogHighWaterRows: 4, backlogLowWaterRows: 1 };

  it("resolves whenWritable at once while there is no backlog", async () => {
    // Arrange.
    const { client } = gatedStore();
    const plane = boundedPlane(client, MARKS);

    // Act, Assert.
    await expect(plane.whenWritable()).resolves.toBeUndefined();
  });

  it("holds the vendor stream while the backlog is past its high-water mark", async () => {
    // Arrange. A slow store: the first batch stays in flight.
    const gated = gatedStore();
    gated.hold();
    const plane = boundedPlane(gated.client, MARKS);
    for (let index = 0; index < 5; index += 1) plane.write([readEntry(MAIN, `unit-${index}`, "/tmp/a")]);

    // Act.
    let writable = false;
    void plane.whenWritable().then(() => {
      writable = true;
    });
    await settle();

    // Assert.
    expect(writable).toBe(false);
    gated.open();
    await plane.flush();
  });

  it("releases the vendor stream once the backlog drains to its low-water mark", async () => {
    // Arrange.
    const gated = gatedStore();
    gated.hold();
    const plane = boundedPlane(gated.client, MARKS);
    for (let index = 0; index < 5; index += 1) plane.write([readEntry(MAIN, `unit-${index}`, "/tmp/a")]);
    const waiting = plane.whenWritable();

    // Act.
    gated.open();

    // Assert.
    await expect(waiting).resolves.toBeUndefined();
  });

  it("opens the episode on bytes alone when the rows are few but large", async () => {
    // Arrange. One row past the byte mark is a backlog even at one row.
    const gated = gatedStore();
    gated.hold();
    const plane = boundedPlane(gated.client, { backlogHighWaterBytes: 1, backlogLowWaterBytes: 0 });
    plane.write([readEntry(MAIN, "unit-0", "/tmp/a")]);

    // Act.
    let writable = false;
    void plane.whenWritable().then(() => {
      writable = true;
    });
    await settle();

    // Assert.
    expect(writable).toBe(false);
    gated.open();
    await plane.flush();
  });

  it("lands every row a slow store fell behind on, far past the old 256-batch buffer", async () => {
    // Arrange. The old buffer evicted its oldest batch at 256; nothing may go.
    const gated = gatedStore();
    gated.hold();
    const plane = boundedPlane(gated.client);
    for (let index = 0; index < 300; index += 1) plane.write([readEntry(MAIN, `unit-${index}`, "/tmp/a")]);

    // Act.
    gated.open();
    await plane.flush();

    // Assert.
    expect(new Set(gated.requests.flatMap(keysOf)).size).toBe(300);
  });

  it("warns ONCE per backlog episode, however far past the mark it grows", async () => {
    // Arrange.
    const gated = gatedStore();
    gated.hold();
    const plane = boundedPlane(gated.client, MARKS);
    const before = logSinkMark();

    // Act.
    for (let index = 0; index < 12; index += 1) plane.write([readEntry(MAIN, `unit-${index}`, "/tmp/a")]);

    // Assert.
    const warnings = logRecordsSince(before).filter(
      (entry) => entry.level === "warn" && String(entry.message).includes("high-water mark"),
    );
    expect(warnings).toHaveLength(1);
    gated.open();
    await plane.flush();
  });

  it("clears the episode at INFO once the backlog drains", async () => {
    // Arrange.
    const gated = gatedStore();
    gated.hold();
    const plane = boundedPlane(gated.client, MARKS);
    for (let index = 0; index < 5; index += 1) plane.write([readEntry(MAIN, `unit-${index}`, "/tmp/a")]);
    const before = logSinkMark();

    // Act.
    gated.open();
    await plane.flush();

    // Assert.
    expect(
      logRecordsSince(before).some(
        (entry) => entry.level === "info" && String(entry.message).includes("low-water mark"),
      ),
    ).toBe(true);
  });

  it("logs each batch's flush timing at debug while the writer is behind", async () => {
    // Arrange.
    const gated = gatedStore();
    gated.hold();
    const plane = boundedPlane(gated.client, MARKS);
    for (let index = 0; index < 5; index += 1) plane.write([readEntry(MAIN, `unit-${index}`, "/tmp/a")]);
    const before = logSinkMark();

    // Act.
    gated.open();
    await plane.flush();

    // Assert.
    const timing = logRecordsSince(before).find((entry) =>
      String(entry.message).includes("landed while the store writer is behind"),
    );
    expect(timing?.context).toMatchObject({ rows: 1, duration_ms: 0 });
  });
});

describe("bounded batches", () => {
  it("merges a backlog of small writes into batches of at most maxBatchRows", async () => {
    // Arrange. One write in flight, nine queued behind it.
    const gated = gatedStore();
    gated.hold();
    const plane = boundedPlane(gated.client, { maxBatchRows: 4 });
    for (let index = 0; index < 10; index += 1) plane.write([readEntry(MAIN, `unit-${index}`, "/tmp/a")]);

    // Act.
    gated.open();
    await plane.flush();

    // Assert.
    expect(gated.requests.map((request) => keysOf(request).length)).toEqual([1, 4, 4, 1]);
  });

  it("splits one large write, the way an interrupt's 494 rows arrive, into bounded batches", async () => {
    // Arrange.
    const gated = gatedStore();
    const plane = boundedPlane(gated.client, { maxBatchRows: 4 });
    const rows = Array.from({ length: 10 }, (_, index) => readEntry(MAIN, `unit-${index}`, "/tmp/a"));

    // Act.
    plane.write(rows);
    await plane.flush();

    // Assert.
    expect(gated.requests.map((request) => keysOf(request).length)).toEqual([4, 4, 2]);
  });

  it("cuts a batch at maxBatchBytes", async () => {
    // Arrange. The bound holds exactly two of these rows' payloads.
    const gated = gatedStore();
    const row = readEntry(MAIN, "unit-0", "/tmp/a");
    const size = row.item.kind === "frame" ? toBinary(conversationv1.AgentFrameSchema, row.item.frame).length : 0;
    const plane = boundedPlane(gated.client, { maxBatchBytes: size * 2 });
    const rows = Array.from({ length: 5 }, (_, index) => readEntry(MAIN, `unit-${index}`, "/tmp/a"));

    // Act.
    plane.write(rows);
    await plane.flush();

    // Assert.
    expect(gated.requests.map((request) => keysOf(request).length)).toEqual([2, 2, 1]);
  });

  it("carries a row larger than the byte bound on its own rather than never", async () => {
    // Arrange.
    const gated = gatedStore();
    const plane = boundedPlane(gated.client, { maxBatchBytes: 1 });

    // Act.
    plane.write([readEntry(MAIN, "unit-0", "/tmp/a"), readEntry(MAIN, "unit-1", "/tmp/b")]);
    await plane.flush();

    // Assert.
    expect(gated.requests.map((request) => keysOf(request).length)).toEqual([1, 1]);
  });

  it("halves the next batch's row bound after a batch that overran its time budget", async () => {
    // Arrange. Every WriteBatch takes a second on this clock; the budget is 500ms.
    let now = 0;
    const plane = boundedPlane(
      stubClient({
        writeBatch: () => {
          now += 1_000;
          return Promise.resolve(durable());
        },
      }),
      { maxBatchRows: 4, batchTimeBudgetMs: 500 },
      () => now,
    );
    const before = logSinkMark();

    // Act.
    plane.write(Array.from({ length: 6 }, (_, index) => readEntry(MAIN, `unit-${index}`, "/tmp/a")));
    await plane.flush();

    // Assert.
    const halved = logRecordsSince(before).find((entry) => String(entry.message).includes("halving"));
    expect(halved?.context).toMatchObject({ rows: 4, duration_ms: 1_000, row_limit: 2 });
  });

  it("raises the row bound back after a batch well inside its time budget", async () => {
    // Arrange. The first WriteBatch is slow, every later one instant.
    let now = 0;
    let calls = 0;
    const requests: storev1.WriteBatchRequest[] = [];
    const plane = boundedPlane(
      stubClient({
        writeBatch: (request) => {
          requests.push(request);
          calls += 1;
          if (calls === 1) now += 1_000;
          return Promise.resolve(durable());
        },
      }),
      { maxBatchRows: 4, batchTimeBudgetMs: 500 },
      () => now,
    );

    // Act.
    plane.write(Array.from({ length: 12 }, (_, index) => readEntry(MAIN, `unit-${index}`, "/tmp/a")));
    await plane.flush();

    // Assert.
    expect(requests.map((request) => keysOf(request).length)).toEqual([4, 2, 4, 2]);
  });
});

describe("turn edges, and the order the store receives", () => {
  it("ends a batch at a turn terminal, so its ack never waits on a row produced after it", async () => {
    // Arrange. One write in flight; the terminal is queued mid-backlog.
    const gated = gatedStore();
    gated.hold();
    const plane = boundedPlane(gated.client);
    plane.write([readEntry(MAIN, "unit-m0", "/tmp/a")]);
    plane.write([readEntry(MAIN, "unit-m1", "/tmp/a"), terminalEntry(MAIN, "t1")]);
    plane.write([readEntry(MAIN, "unit-m2", "/tmp/a")]);

    // Act.
    gated.open();
    await plane.flush();

    // Assert.
    expect(keysOf(gated.requests[1])).toEqual(["activity:unit-m1", "terminal:t1"]);
  });

  it("ends a batch at a prompt row", async () => {
    // Arrange.
    const gated = gatedStore();
    gated.hold();
    const plane = boundedPlane(gated.client);
    plane.write([readEntry(MAIN, "unit-m0", "/tmp/a")]);
    plane.write([promptEntry(MAIN, "turn-2", "next"), readEntry(MAIN, "unit-m1", "/tmp/a")]);

    // Act.
    gated.open();
    await plane.flush();

    // Assert.
    expect(keysOf(gated.requests[1])).toEqual(["prompt:turn-2"]);
  });

  it("drops a keep-alive's terminal rather than ending a batch at it: it is never stored", async () => {
    // Arrange.
    const gated = gatedStore();
    gated.hold();
    const plane = boundedPlane(gated.client);
    plane.write([readEntry(MAIN, "unit-m0", "/tmp/a")]);
    plane.write([{ ...terminalEntry(MAIN, "keepalive-1"), keepalive: true }, readEntry(MAIN, "unit-m1", "/tmp/a")]);

    // Act.
    gated.open();
    await plane.flush();

    // Assert: keep-alive rows are stored by neither plane (owner ruling,
    // 2026-09-23), so the terminal never reaches a batch at all.
    expect(keysOf(gated.requests[1])).toEqual(["activity:unit-m1"]);
  });

  it("does not end a batch at a session fact", async () => {
    // Arrange.
    const gated = gatedStore();
    gated.hold();
    const plane = boundedPlane(gated.client);
    plane.write([readEntry(MAIN, "unit-m0", "/tmp/a")]);
    plane.write([
      {
        agentId: MAIN,
        upsertKey: "session:compacting:uuid-1",
        source: { vendorUuid: "uuid-1", discriminator: "session_update.compacting" },
        keepalive: false,
        turn: undefined,
        item: {
          kind: "session_update",
          update: create(conversationv1.SessionUpdateSchema, {
            update: { case: "compacting", value: create(conversationv1.SessionCompactingSchema, {}) },
          }),
        },
      },
      readEntry(MAIN, "unit-m1", "/tmp/a"),
    ]);

    // Act.
    gated.open();
    await plane.flush();

    // Assert.
    expect(keysOf(gated.requests[1])).toEqual(["session:compacting:uuid-1", "activity:unit-m1"]);
  });

  it("hands the store every row in exactly the order produced, across books", async () => {
    // Arrange. A backlog interleaving the main book, a subagent and the turn's end.
    const gated = gatedStore();
    gated.hold();
    const plane = boundedPlane(gated.client, { maxBatchRows: 2 });
    const produced = [
      readEntry(MAIN, "unit-m0", "/tmp/a"),
      spawnEntry(MAIN, "sub-1"),
      readEntry(SUB, "unit-s1", "/tmp/s"),
      readEntry(MAIN, "unit-m1", "/tmp/a"),
      readEntry(SUB, "unit-s2", "/tmp/s"),
      terminalEntry(SUB, "sub-end"),
      terminalEntry(MAIN, "t1"),
    ];
    for (const row of produced) plane.write([row]);

    // Act.
    gated.open();
    await plane.flush();

    // Assert.
    expect(gated.requests.flatMap(keysOf)).toEqual(produced.map((row) => row.upsertKey));
  });

  it("lands a turn's terminal only after every row the turn produced before it", async () => {
    // Arrange. The interrupt shape: the calls a stop cut, then the terminal.
    const gated = gatedStore();
    const plane = boundedPlane(gated.client, { maxBatchRows: 4 });
    const cut = Array.from({ length: 10 }, (_, index) => readEntry(SUB, `cut-${index}`, "/tmp/s"));

    // Act.
    plane.write([...cut, terminalEntry(MAIN, "t1")]);
    await plane.flush();

    // Assert. The terminal is the last word the store received.
    expect(gated.requests.flatMap(keysOf).at(-1)).toBe("terminal:t1");
  });

  it("acks a durable prompt only after every row produced before it", async () => {
    // Arrange.
    const gated = gatedStore();
    gated.hold();
    const plane = boundedPlane(gated.client, { maxBatchRows: 4 });
    plane.write([readEntry(MAIN, "unit-m0", "/tmp/a")]);
    for (let index = 0; index < 6; index += 1) plane.write([readEntry(SUB, `unit-s${index}`, "/tmp/s")]);
    const acked = plane.writeDurable([promptEntry(MAIN, "turn-2", "next")]);

    // Act.
    gated.open();
    await acked;

    // Assert.
    expect(gated.requests.flatMap(keysOf)).toEqual([
      "activity:unit-m0",
      ...Array.from({ length: 6 }, (_, index) => `activity:unit-s${index}`),
      "prompt:turn-2",
    ]);
  });
});

describe("a multi-row batch the store refuses as malformed", () => {
  /** A store that refuses any batch carrying the `bad` unit, and takes the rest. */
  function pickyStore(): ReturnType<typeof gatedStore> {
    return gatedStore((request) =>
      keysOf(request).includes("activity:bad") ? malformed("entries[0] is malformed") : durable(),
    );
  }

  it("lands every row the store would carry on its own", async () => {
    // Arrange.
    const picky = pickyStore();
    const plane = boundedPlane(picky.client);

    // Act.
    plane.write([readEntry(MAIN, "good-1", "/tmp/a"), readEntry(MAIN, "bad", "/tmp/a"), readEntry(MAIN, "good-2", "/tmp/a")]);
    await plane.flush();

    // Assert. The last three requests are the one-row resends, in order.
    expect(picky.requests.slice(1).map(keysOf)).toEqual([
      ["activity:good-1"],
      ["activity:bad"],
      ["activity:good-2"],
    ]);
  });

  it("names only the refused row at ERROR", async () => {
    // Arrange.
    const picky = pickyStore();
    const plane = boundedPlane(picky.client);
    const before = logSinkMark();

    // Act.
    plane.write([readEntry(MAIN, "good-1", "/tmp/a"), readEntry(MAIN, "bad", "/tmp/a")]);
    await plane.flush();

    // Assert.
    const refused = logRecordsSince(before).filter(
      (entry) => entry.level === "error" && String(entry.message).includes("refused a row as malformed"),
    );
    expect(refused.map((entry) => entry.context.lost_upsert_keys)).toEqual([
      ["activity:bad"],
    ]);
  });

  it("states at debug that it is isolating the refusal", async () => {
    // Arrange.
    const picky = pickyStore();
    const plane = boundedPlane(picky.client);
    const before = logSinkMark();

    // Act.
    plane.write([readEntry(MAIN, "good-1", "/tmp/a"), readEntry(MAIN, "bad", "/tmp/a")]);
    await plane.flush();

    // Assert.
    expect(
      logRecordsSince(before).some(
        (entry) => entry.level === "debug" && String(entry.message).includes("one at a time"),
      ),
    ).toBe(true);
  });

  it("counts the refused row as lost to the flush that watched it", async () => {
    // Arrange.
    const picky = pickyStore();
    const plane = boundedPlane(picky.client);

    // Act.
    plane.write([readEntry(MAIN, "good-1", "/tmp/a"), readEntry(MAIN, "bad", "/tmp/a")]);
    const outcome = await plane.flush();

    // Assert.
    expect(outcome.lostRows).toBe(1);
  });
});

describe("payload sizing for the byte bound", () => {
  /** A peer message row in the MAIN book. */
  function peerEntry(id: string): PersistEntry {
    return {
      agentId: MAIN,
      upsertKey: `peer:${id}`,
      source: { vendorUuid: id, discriminator: "peer_message" },
      keepalive: false,
      turn: undefined,
      item: {
        kind: "peer",
        peer: create(conversationv1.PeerMessageSchema, { agent: MAIN, sender: "Explore", body: "hi", id }),
      },
    };
  }

  it("sizes a peer message by its payload", async () => {
    // Arrange. A one-byte bound holds one sized row per batch.
    const gated = gatedStore();
    const plane = boundedPlane(gated.client, { maxBatchBytes: 1 });

    // Act.
    plane.write([peerEntry("u1"), peerEntry("u2")]);
    await plane.flush();

    // Assert.
    expect(gated.requests.map((request) => keysOf(request).length)).toEqual([1, 1]);
  });

  it("sizes a residue row by its payload", async () => {
    // Arrange.
    const gated = gatedStore();
    const plane = boundedPlane(gated.client, { maxBatchBytes: 1 });
    const residue = (key: string): PersistEntry => ({
      ...residueEntry(),
      upsertKey: key,
      item: {
        kind: "residue",
        residue: create(storev1.StoreUnservedItemSchema, {
          unservedItem: { case: "unknown", value: create(storev1.StoreUnknownSchema, { discriminator: "a_new_kind" }) },
        }),
      },
    });

    // Act.
    plane.write([residue("residue:1"), residue("residue:2")]);
    await plane.flush();

    // Assert.
    expect(gated.requests.map((request) => keysOf(request).length)).toEqual([1, 1]);
  });

  it("sizes a row of unknown kind as nothing, leaving its refusal to the envelope", async () => {
    // Arrange. The unknown row rides with the next one, and the envelope refuses it.
    const gated = gatedStore();
    const good = readEntry(MAIN, "unit-1", "/tmp/a");
    const size = good.item.kind === "frame" ? toBinary(conversationv1.AgentFrameSchema, good.item.frame).length : 0;
    const plane = boundedPlane(gated.client, { maxBatchBytes: size });
    const broken = { ...readEntry(MAIN, "unit-0", "/tmp/a"), item: { kind: "not_a_kind" } as unknown as PersistEntry["item"] };

    // Act.
    plane.write([broken, good]);
    await plane.flush();

    // Assert. Only the well-formed row ever reaches the wire.
    expect(gated.requests.map(keysOf)).toEqual([["activity:unit-1"]]);
  });
});

describe("a durable write split across batches", () => {
  it("acks only once its LAST row lands", async () => {
    // Arrange. Three rows, cut into two batches.
    const gated = gatedStore();
    const plane = boundedPlane(gated.client, { maxBatchRows: 2 });

    // Act.
    await plane.writeDurable([
      promptEntry(MAIN, "turn-1", "one"),
      readEntry(MAIN, "unit-1", "/tmp/a"),
      readEntry(MAIN, "unit-2", "/tmp/b"),
    ]);

    // Assert. The prompt, a turn edge, ends the first batch.
    expect(gated.requests.map((request) => keysOf(request).length)).toEqual([1, 2]);
  });
});

describe("a held batch past the retry schedule", () => {
  it("is retried every heldRetryMs once its failure is persistent", async () => {
    // Arrange. Record every backoff; the sixth one parks, and says so.
    const slept: number[] = [];
    let sixth: () => void = () => undefined;
    const reachedSixth = new Promise<void>((resolve) => {
      sixth = resolve;
    });
    const plane = createPersistence({
      client: stubClient({ writeBatch: () => Promise.reject(new Error("the store is down")) }),
      producer: PRODUCER,
      nowMs: () => 1_000,
      sleep: (ms) => {
        slept.push(ms);
        if (slept.length < 6) return Promise.resolve();
        sixth();
        return new Promise<void>(() => undefined);
      },
      retry: { backoffMs: [1, 2, 3, 4], maxAttempts: 5, heldRetryMs: 99 },
    });

    // Act.
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await reachedSixth;

    // Assert. The schedule, then the held cadence.
    expect(slept).toEqual([1, 2, 3, 4, 99, 99]);
  });

  it("keeps retrying it, stating each further failure at debug", async () => {
    // Arrange. The schedule is spent and the store is still down.
    const { store: fake, persistence: plane, release } = await persistence("held-retries");
    fake.failWrites("the store is down");
    plane.write([readEntry(BOOK, "unit-1", "/tmp/a")]);
    await plane.flush();
    const before = logSinkMark();

    // Act. One more attempt fails, and the flush answers on it.
    release();
    await plane.flush();

    // Assert.
    expect(
      logRecordsSince(before).some(
        (entry) => entry.level === "debug" && String(entry.message).includes("still failing"),
      ),
    ).toBe(true);
  });
});
