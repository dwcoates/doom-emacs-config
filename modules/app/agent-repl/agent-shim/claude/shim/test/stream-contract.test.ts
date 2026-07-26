import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import { anyUnpack } from "@bufbuild/protobuf/wkt";

import { convert } from "../src/proto/convert.js";
import { StreamMessageTracker, toEphemeralEvent } from "../src/proto/delta.js";
import {
  ClaudeStreamMessageSchema,
  type ClaudeStreamMessage,
} from "../../../../proto/gen/ts/agentshim/data/v1/stream_pb.js";

/**
 * The streaming identity CONTRACT, asserted at THIS end of the chain.
 *
 * The shim is where a streamed message's identity is CHOSEN — it is the only
 * hop that ever sees the `message_start` naming it — so everything downstream
 * can only carry what is stamped here. This suite pins that stamp against the
 * shared fixture the daemon and webapp suites read, so a change to what the
 * shim emits fails in CI rather than silently arriving as a second, divergent
 * identity at the far end.
 *
 * See the fixture's `$comment` for why the message deliberately holds two
 * content blocks.
 */

interface Fixture {
  sessionId: string;
  messageId: string;
  sdkMessages: Array<Record<string, unknown>>;
  frontendFrames: Array<Record<string, any>>;
}

const fixture = JSON.parse(
  readFileSync(
    new URL("../../../../testdata/stream-contract/one-message.json", import.meta.url),
    "utf8",
  ),
) as Fixture;

/** The ephemeral ContentDeltas the fixture's SDK stream produces, in order. */
function contentDeltas(): Array<{ uuid: string; blockIndex: number; kind: string; text: string }> {
  const tracker = new StreamMessageTracker();
  const out: Array<{ uuid: string; blockIndex: number; kind: string; text: string }> = [];
  for (const msg of fixture.sdkMessages) {
    tracker.observe(msg);
    const evt = toEphemeralEvent(msg, { nowMs: 1000, messageId: tracker.current() });
    if (evt?.payload.case !== "contentDelta") continue;
    const cd = evt.payload.value;
    out.push({
      uuid: cd.uuid,
      blockIndex: cd.blockIndex,
      kind: cd.delta.case ?? "",
      text: String(cd.delta.value ?? ""),
    });
  }
  return out;
}

/** The persistent assistant records the fixture produces, in order. */
function assistantRecords(): Array<{ uuid: string; messageId: string }> {
  const out: Array<{ uuid: string; messageId: string }> = [];
  for (const msg of fixture.sdkMessages) {
    if (msg["type"] !== "assistant") continue;
    const any = convert(msg).vendor.payload.value;
    const csm = anyUnpack(any as Parameters<typeof anyUnpack>[0], ClaudeStreamMessageSchema) as
      | ClaudeStreamMessage
      | undefined;
    if (csm?.msg.case !== "assistant") throw new Error("expected an assistant arm");
    out.push({ uuid: csm.msg.value.uuid, messageId: csm.msg.value.message?.id ?? "" });
  }
  return out;
}

/** The typing frames the fixture declares the daemon will relay, in order. */
function expectedDeltas(): Array<{ uuid: string; blockIndex: number; kind: string; text: string }> {
  return fixture.frontendFrames
    .filter((f) => f["typingDelta"] !== undefined)
    .map((f) => {
      const d = f["typingDelta"].delta as Record<string, unknown>;
      const kind = ["text", "thinking", "inputJson", "signature"].find((k) => k in d)!;
      return {
        uuid: String(d["uuid"]),
        blockIndex: Number(d["blockIndex"] ?? 0),
        kind,
        text: String(d[kind]),
      };
    });
}

/** The conversation items the fixture declares, in order. */
function expectedRecords(): Array<{ uuid: string; messageId: string }> {
  return fixture.frontendFrames
    .filter((f) => f["conversationDelta"] !== undefined)
    .flatMap((f) => f["conversationDelta"].items as Array<Record<string, any>>)
    .map((item) => ({ uuid: String(item["uuid"]), messageId: String(item["assistantMessage"].id) }));
}

describe("the shim stamps the identity the whole chain carries", () => {
  it("emits exactly the content deltas the fixture's wire declares", () => {
    // Arrange / Act / Assert — the shim's output IS the fixture's typing half,
    // so the two cannot drift apart unnoticed.
    expect(contentDeltas()).toEqual(expectedDeltas());
  });

  it("stamps every delta of one message with that message's id", () => {
    // The SDK mints a fresh envelope uuid per emitted event, so keying on it
    // gave each chunk its own id and the frontend opened a bubble per chunk.
    // Arrange / Act
    const uuids = new Set(contentDeltas().map((d) => d.uuid));
    // Assert
    expect([...uuids]).toEqual([fixture.messageId]);
  });

  it("keeps the TRUE API block index on each delta", () => {
    // This ordinal is the only thing distinguishing the message's two blocks
    // while it streams; flattening it is what merged them downstream.
    // Arrange / Act / Assert
    expect(contentDeltas().map((d) => d.blockIndex)).toEqual([0, 0, 1, 1]);
  });

  it("emits exactly the assistant records the fixture's wire declares", () => {
    // Arrange / Act / Assert
    expect(assistantRecords()).toEqual(expectedRecords());
  });

  it("gives each record its own envelope uuid while repeating the message id", () => {
    // This is the asymmetry the whole contract rests on: the envelope tells
    // the two records apart, the message id ties them to one stream.
    // Arrange / Act
    const records = assistantRecords();
    // Assert
    expect(new Set(records.map((r) => r.uuid)).size).toBe(records.length);
    expect(new Set(records.map((r) => r.messageId))).toEqual(new Set([fixture.messageId]));
  });
});
