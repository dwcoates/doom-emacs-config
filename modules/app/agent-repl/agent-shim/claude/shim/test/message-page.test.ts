/**
 * The store→shim BOUNDED, BACKWARD-ANCHORED page read.
 *
 * The properties under test are the ones the old vocabulary could not state:
 * a head-anchored request names NO seq, a continuation copies `last_page_seq`
 * VERBATIM rather than computing one, a StoredMessage arrives WHOLE, the
 * retained-floor arm is distinguishable from more-remains, and a short page is
 * not read as the beginning.
 */
import { afterEach, describe, expect, it } from "vitest";
import fs from "node:fs";
import net from "node:net";
import { create } from "@bufbuild/protobuf";
import { StoreClient } from "../src/uds/store-client.js";
import {
  continueBelow,
  HEAD_ANCHOR,
  MESSAGE_PAGE_SLOTS,
  messagePageRequest,
  pageBoundary,
  pageMessages,
  reachedRetainedFloor,
} from "../src/uds/message-page.js";
import {
  EventSchema,
  HistoryAtRetainedFloorSchema,
  HistoryRemainsBelowSchema,
  MessagePage,
  MessagePageHeadSchema,
  MessagePageRequest,
  MessagePageRequestSchema,
  MessagePageSchema,
  StoredMessageSchema,
} from "../src/uds/proto.js";
import { FramedPeer, tmpSocketPath, tmpSpillDir, until } from "./uds-harness.js";

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

interface FakeStore {
  socketPath: string;
  conns: FramedPeer[];
  close: () => void;
}

function fakeStore(): Promise<FakeStore> {
  const socketPath = tmpSocketPath();
  try {
    fs.unlinkSync(socketPath);
  } catch {
    // No stale file: the normal case for a fresh path.
  }
  const conns: FramedPeer[] = [];
  return new Promise((resolve, reject) => {
    const server = net.createServer((socket) => conns.push(new FramedPeer(socket)));
    server.once("error", reject);
    server.listen(socketPath, () =>
      resolve({
        socketPath,
        conns,
        close: () => {
          conns.forEach((c) => c.destroy());
          server.close();
        },
      }),
    );
  });
}

const clients: StoreClient[] = [];
const stores: FakeStore[] = [];
afterEach(() => {
  clients.splice(0).forEach((c) => c.close());
  stores.splice(0).forEach((s) => s.close());
});

async function connectedClient(store: FakeStore): Promise<StoreClient> {
  const client = new StoreClient({
    spillDir: tmpSpillDir(),
    socketPath: store.socketPath,
    sessionId: "sess-1",
    producer: "claude-shim:sess-1",
    heartbeatIntervalMs: 0,
  });
  clients.push(client);
  await client.connect();
  await until(() => store.conns.length >= 1);
  return client;
}

/** Await the page connection's request frame, and return it with its peer. */
async function pageExchange(store: FakeStore): Promise<{ peer: FramedPeer; req: MessagePageRequest }> {
  await until(() => store.conns.length > 1);
  const peer = store.conns[1]!;
  const req = await peer.next(MessagePageRequestSchema);
  return { peer, req };
}

function storedMessage(id: string, recordSeqs: number[]) {
  return create(StoredMessageSchema, {
    messageId: id,
    records: recordSeqs.map((seq) => create(EventSchema, { sessionId: "sess-1", seq: BigInt(seq) })),
  });
}

// ---------------------------------------------------------------------------
// The request vocabulary
// ---------------------------------------------------------------------------

describe("messagePageRequest", () => {
  it("names no seq for a head anchor", () => {
    // Arrange / Act
    const req = messagePageRequest("req-1", HEAD_ANCHOR);

    // Assert: the head arm carries an EMPTY message, so there is no field on
    // this request that could hold a caller-authored position.
    expect(req.anchor).toEqual({ case: "head", value: create(MessagePageHeadSchema, {}) });
  });

  it("copies last_page_seq verbatim on a continuation", () => {
    // Arrange: a page the store minted, whose last_page_seq is nothing the
    // caller could have derived from the records.
    const page = create(MessagePageSchema, { requestId: "req-1", lastPageSeq: 90210n });

    // Act
    const req = messagePageRequest("req-2", continueBelow(page));

    // Assert: verbatim — not decremented, not computed.
    expect(req.anchor).toEqual({ case: "beforeSeq", value: 90210n });
  });
});

// ---------------------------------------------------------------------------
// The page shape
// ---------------------------------------------------------------------------

describe("pageMessages", () => {
  it("delivers a StoredMessage with many records whole", () => {
    // Arrange: one message owning far more records than a page has slots.
    const seqs = Array.from({ length: 250 }, (_, i) => i + 1);
    const page = create(MessagePageSchema, { message1: storedMessage("m-1", seqs) });

    // Act
    const messages = pageMessages(page);

    // Assert: one renderable unit, un-split and un-rechunked.
    expect(messages).toHaveLength(1);
    expect(messages[0]!.records.map((r) => Number(r.seq))).toEqual(seqs);
  });

  it("cannot yield more than the ten slots the type has", () => {
    // Arrange: every slot filled.
    const page = create(MessagePageSchema, {
      message1: storedMessage("m-1", [1]), message2: storedMessage("m-2", [2]),
      message3: storedMessage("m-3", [3]), message4: storedMessage("m-4", [4]),
      message5: storedMessage("m-5", [5]), message6: storedMessage("m-6", [6]),
      message7: storedMessage("m-7", [7]), message8: storedMessage("m-8", [8]),
      message9: storedMessage("m-9", [9]), message10: storedMessage("m-10", [10]),
    });

    // Act / Assert
    expect(pageMessages(page)).toHaveLength(MESSAGE_PAGE_SLOTS);
  });
});

// ---------------------------------------------------------------------------
// The boundary oneof
// ---------------------------------------------------------------------------

describe("pageBoundary", () => {
  it("reports more-remains distinctly from the retained floor", () => {
    // Arrange
    const page = create(MessagePageSchema, {
      boundary: { case: "more", value: create(HistoryRemainsBelowSchema, {}) },
    });

    // Act / Assert
    expect(pageBoundary(page)).toBe("more");
    expect(reachedRetainedFloor(page)).toBe(false);
  });

  it("reports the retained floor as the serving side's own fact", () => {
    // Arrange
    const page = create(MessagePageSchema, {
      boundary: { case: "floor", value: create(HistoryAtRetainedFloorSchema, {}) },
    });

    // Act / Assert: the floor is the oldest RETAINED record, never collapsed
    // into "more remains".
    expect(pageBoundary(page)).toBe("retained-floor");
    expect(reachedRetainedFloor(page)).toBe(true);
  });

  it("does not read a short page as the beginning", () => {
    // Arrange: two messages and an explicit more-remains arm — short, but the
    // conversation continues below.
    const page = create(MessagePageSchema, {
      message1: storedMessage("m-1", [9]),
      message2: storedMessage("m-2", [8]),
      boundary: { case: "more", value: create(HistoryRemainsBelowSchema, {}) },
    });

    // Act / Assert: shortness is not evidence; only the arm speaks.
    expect(pageMessages(page)).toHaveLength(2);
    expect(reachedRetainedFloor(page)).toBe(false);
  });

  it("surfaces an unset boundary rather than defaulting it into an arm", () => {
    // Arrange: a serving side that set no arm at all.
    const page = create(MessagePageSchema, {});

    // Act / Assert
    expect(pageBoundary(page)).toBe("unset");
    expect(reachedRetainedFloor(page)).toBe(false);
  });
});

// ---------------------------------------------------------------------------
// The wire hop
// ---------------------------------------------------------------------------

describe("StoreClient.fetchMessagePage", () => {
  it("asks the store for the newest page without naming a seq", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);

    // Act
    const pending = client.fetchMessagePage(HEAD_ANCHOR, 2000);
    const { peer, req } = await pageExchange(store);
    peer.send(MessagePageSchema, create(MessagePageSchema, { requestId: req.requestId }));
    await pending;

    // Assert
    expect(req.anchor.case).toBe("head");
  });

  it("sends the received page's last_page_seq as the continuation anchor", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    const first = client.fetchMessagePage(HEAD_ANCHOR, 2000);
    const firstExchange = await pageExchange(store);
    firstExchange.peer.send(MessagePageSchema, create(MessagePageSchema, {
      requestId: firstExchange.req.requestId,
      lastPageSeq: 4242n,
    }));
    const page: MessagePage = await first;

    // Act
    const second = client.fetchMessagePage(continueBelow(page), 2000);
    await until(() => store.conns.length > 2);
    const secondPeer = store.conns[2]!;
    const secondReq = await secondPeer.next(MessagePageRequestSchema);
    secondPeer.send(MessagePageSchema, create(MessagePageSchema, { requestId: secondReq.requestId }));
    await second;

    // Assert
    expect(secondReq.anchor).toEqual({ case: "beforeSeq", value: 4242n });
  });

  it("leaves the standing subscription untouched", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);

    // Act
    const pending = client.fetchMessagePage(HEAD_ANCHOR, 2000);
    const { peer, req } = await pageExchange(store);
    peer.send(MessagePageSchema, create(MessagePageSchema, { requestId: req.requestId }));
    await pending;

    // Assert: the page rode a THROWAWAY connection, so nothing the daemon's
    // live tail owns was reopened.
    expect(store.conns).toHaveLength(2);
  });

  it("rejects rather than returning an empty page when the store never answers", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);

    // Act
    const pending = client.fetchMessagePage(HEAD_ANCHOR, 50);

    // Assert: an empty page is a claim about the conversation; a timeout is a
    // claim about the link, and the two are never conflated.
    await expect(pending).rejects.toThrow(/no MessagePage within 50ms/);
  });

  it("discards a page carrying a request_id it is not awaiting", async () => {
    // Arrange
    const store = await fakeStore();
    stores.push(store);
    const client = await connectedClient(store);
    const pending = client.fetchMessagePage(HEAD_ANCHOR, 2000);
    const { peer, req } = await pageExchange(store);

    // Act: a stale page first, then the awaited one.
    peer.send(MessagePageSchema, create(MessagePageSchema, { requestId: "some-other-request", lastPageSeq: 7n }));
    peer.send(MessagePageSchema, create(MessagePageSchema, { requestId: req.requestId, lastPageSeq: 11n }));

    // Assert
    await expect(pending).resolves.toMatchObject({ lastPageSeq: 11n });
  });
});
