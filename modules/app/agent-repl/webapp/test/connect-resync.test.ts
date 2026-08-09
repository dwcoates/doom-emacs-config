import { describe, it, expect } from "vitest";
import { ConnectResync, type ConnectResyncLogLevel } from "../src/connect-resync.js";

interface Sent {
  workspace: string;
  fromSeq: number;
  fence: string;
}

interface Harness {
  trigger: ConnectResync;
  sent: Sent[];
  logs: Array<[ConnectResyncLogLevel, string]>;
}

/** A trigger whose resync always succeeds, recording what it was asked to send. */
function snapshot(workspace: string, fromSeq: number, fence = "f-current") {
  return { workspace, fromSeq, fence };
}

function harness(resync?: (sent: Sent) => Promise<void>): Harness {
  const sent: Sent[] = [];
  const logs: Array<[ConnectResyncLogLevel, string]> = [];
  const trigger = new ConnectResync({
    resync: (request) => {
      sent.push(request);
      return resync ? resync(request) : Promise.resolve();
    },
    log: (level, message) => logs.push([level, message]),
  });
  return { trigger, sent, logs };
}

describe("ConnectResync", () => {
  it("asks for history once the connect snapshot lands", () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 0));
    // Assert
    expect(h.sent).toEqual([snapshot("/ws", 0)]);
  });

  it("carries the store's through-seq watermark as from_seq", () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 7117));
    // Assert
    expect(h.sent[0]?.fromSeq).toBe(7117);
  });

  it("does not ask again on the snapshot its own resync provokes", () => {
    // Arrange — the daemon answers a ResyncCmd with a fresh StateSnapshot.
    const h = harness();
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 0));
    // Act
    h.trigger.observe(true, snapshot("/ws", 0));
    // Assert
    expect(h.sent).toHaveLength(1);
  });

  it("asks nothing before any snapshot has landed", () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    // Act
    h.trigger.observe(false, snapshot("/ws", 0));
    // Assert
    expect(h.sent).toEqual([]);
  });

  it("holds fire while the workspace is still unknown", () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("", 0));
    // Assert
    expect(h.sent).toEqual([]);
  });

  it("fires on the first frame that supplies the workspace after a bare snapshot", () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("", 0));
    // Act
    h.trigger.observe(false, snapshot("/ws", 12));
    // Assert
    expect(h.sent).toEqual([snapshot("/ws", 12)]);
  });

  it("asks nothing before any socket has connected", () => {
    // Arrange
    const h = harness();
    // Act
    h.trigger.observe(true, snapshot("/ws", 0));
    // Assert
    expect(h.sent).toEqual([]);
  });

  it("asks again on a reconnect", () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 0));
    h.trigger.onDisconnect();
    // Act
    h.trigger.onConnect();
    h.trigger.observe(true, snapshot("/ws", 42));
    // Assert
    expect(h.sent).toEqual([
      snapshot("/ws", 0),
      snapshot("/ws", 42),
    ]);
  });

  it("asks nothing on a snapshot that arrives after the socket dropped", () => {
    // Arrange
    const h = harness();
    h.trigger.onConnect();
    h.trigger.onDisconnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 0));
    // Assert
    expect(h.sent).toEqual([]);
  });

  it("reports a refused resync loudly", async () => {
    // Arrange
    const h = harness(() => Promise.reject(new Error("no live session")));
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 0));
    await Promise.resolve();
    // Assert
    expect(h.logs.filter(([level]) => level === "error")).toHaveLength(1);
  });

  it("keeps a delayed old client's exact fence", () => {
    // Arrange — the client snapshot carries the fence that was current when
    // this webview rendered. A replacement can publish a newer fence before the
    // old webview's request reaches the daemon.
    const h = harness();
    h.trigger.onConnect();
    // Act
    h.trigger.observe(true, snapshot("/ws", 13, "f-old"));
    h.trigger.observe(true, snapshot("/ws", 0, "f-new"));
    // Assert — the server can now classify the delayed request as superseded;
    // it is never silently rebound to the newer controller.
    expect(h.sent).toEqual([snapshot("/ws", 13, "f-old")]);
  });
});
