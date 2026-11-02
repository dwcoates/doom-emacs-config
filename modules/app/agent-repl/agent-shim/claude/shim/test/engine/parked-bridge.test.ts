/**
 * engine/parked-bridge.ts — the parked conversation's bridge state: the
 * watch's frames, the remote prompt latch, and the promotion that ends every
 * watch.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1, shimv1 } from "../../src/proto.js";
import { ParkedBridge, parkedBridgeRefused, type ParkedBridgeRefusal } from "../../src/engine/parked-bridge.js";
import { textSaid } from "../../src/engine/turn.js";
import { logRecordsSince, logSinkMark } from "../log-records.js";

function bridge(): ParkedBridge {
  return new ParkedBridge(
    "resume-1",
    create(conversationv1.SessionStartedSchema, { vendorSessionId: "resume-1" }),
    "bridge-1",
  );
}

function prompt(text: string, clientUuid: string): { said: conversationv1.UserSaid; clientUuid: string } {
  return { said: textSaid(text), clientUuid };
}

/** Every frame of a watch, as its event case and the fields that matter. */
async function framesOf(frames: AsyncIterable<shimv1.HoldParkedBridgeResponse>): Promise<unknown[]> {
  const seen: unknown[] = [];
  for await (const frame of frames) seen.push(summary(frame));
  return seen;
}

function summary(frame: shimv1.HoldParkedBridgeResponse): unknown {
  switch (frame.event.case) {
    case "opened":
      return { opened: frame.event.value.bridgeSessionId, alreadyOpen: frame.event.value.alreadyOpen };
    case "remotePrompt": {
      const block = frame.event.value.said?.content?.blocks[0]?.block;
      return { remotePrompt: frame.event.value.clientUuid, said: block?.case === "text" ? block.value.text : "" };
    }
    case "refused":
      return { refused: frame.event.value.cause.case, detail: frame.event.value.detail };
    default:
      return { unset: true };
  }
}

describe("ParkedBridge.watch", () => {
  it("states opened, then ends when the bridge is promoted", async () => {
    // Arrange
    const b = bridge();
    const watching = framesOf(b.watch(false, new AbortController().signal));

    // Act
    b.promote();

    // Assert
    expect(await watching).toEqual([{ opened: "bridge-1", alreadyOpen: false }]);
  });

  it("states a remote prompt that arrives while it watches, after opened", async () => {
    // Arrange
    const b = bridge();
    const watching = framesOf(b.watch(false, new AbortController().signal));

    // Act
    b.remotePromptArrived(prompt("fix the build", "uuid-1"));
    b.promote();

    // Assert
    expect(await watching).toEqual([
      { opened: "bridge-1", alreadyOpen: false },
      { remotePrompt: "uuid-1", said: "fix the build" },
    ]);
  });

  it("states a remote prompt that arrived before it opened, so a later watch never misses it", async () => {
    // Arrange
    const b = bridge();
    b.remotePromptArrived(prompt("fix the build", "uuid-1"));

    // Act
    const watching = framesOf(b.watch(true, new AbortController().signal));
    b.promote();

    // Assert
    expect(await watching).toEqual([
      { opened: "bridge-1", alreadyOpen: true },
      { remotePrompt: "uuid-1", said: "fix the build" },
    ]);
  });

  it("ends when its signal aborts, leaving the bridge unpromoted", async () => {
    // Arrange
    const b = bridge();
    const controller = new AbortController();
    const watching = framesOf(b.watch(false, controller.signal));

    // Act
    controller.abort();

    // Assert
    expect([await watching, b.isPromoted]).toEqual([[{ opened: "bridge-1", alreadyOpen: false }], false]);
  });
});

describe("ParkedBridge.remotePromptArrived", () => {
  it("latches only the first prompt", async () => {
    // Arrange
    const b = bridge();
    b.remotePromptArrived(prompt("first", "uuid-1"));

    // Act
    b.remotePromptArrived(prompt("second", "uuid-2"));

    // Assert
    expect(b.remotePrompt?.clientUuid).toBe("uuid-1");
  });

  it("records the arrival at INFO, naming the prompt and the conversation", () => {
    // Arrange
    const b = bridge();
    const mark = logSinkMark();

    // Act
    b.remotePromptArrived(prompt("fix the build", "uuid-1"));

    // Assert
    expect(
      logRecordsSince(mark).map((record) => [record.level, record.context?.client_uuid, record.context?.vendor_session_id]),
    ).toEqual([["info", "uuid-1", "resume-1"]]);
  });

  it("records a later prompt at INFO as one the gate is already answered for", () => {
    // Arrange
    const b = bridge();
    b.remotePromptArrived(prompt("first", "uuid-1"));
    const mark = logSinkMark();

    // Act
    b.remotePromptArrived(prompt("second", "uuid-2"));

    // Assert
    expect(
      logRecordsSince(mark).map((record) => [record.level, record.context?.client_uuid, record.context?.latched_uuid]),
    ).toEqual([["info", "uuid-2", "uuid-1"]]);
  });

  it("throws for a prompt noted on a promoted bridge", () => {
    // Arrange
    const b = bridge();
    b.promote();

    // Act + Assert
    expect(() => b.remotePromptArrived(prompt("late", "uuid-1"))).toThrow(/already promoted/);
  });
});

describe("ParkedBridge.promote", () => {
  it("throws when promoted twice", () => {
    // Arrange
    const b = bridge();
    b.promote();

    // Act + Assert
    expect(() => b.promote()).toThrow(/promoted twice/);
  });
});

describe("parkedBridgeRefused", () => {
  const cases: { name: string; refusal: ParkedBridgeRefusal; want: string }[] = [
    { name: "a resume that failed", refusal: { kind: "resumeFailed", detail: "d1" }, want: "resumeFailed" },
    { name: "a bridge the vendor refused", refusal: { kind: "vendorRefused", detail: "d2" }, want: "vendorRefused" },
    { name: "a shim already serving a session", refusal: { kind: "alreadyStarted", detail: "d3" }, want: "alreadyStarted" },
  ];
  for (const tc of cases) {
    it(`states ${tc.name} under its own arm, with its detail`, () => {
      // Act
      const frame = parkedBridgeRefused(tc.refusal);

      // Assert
      expect(summary(frame)).toEqual({ refused: tc.want, detail: tc.refusal.detail });
    });
  }
});
