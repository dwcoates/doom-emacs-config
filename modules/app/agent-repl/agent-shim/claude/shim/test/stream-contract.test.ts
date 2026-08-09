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
function conversationItems(): Array<Record<string, any>> {
  return fixture.frontendFrames
    .filter((f) => f["conversationDelta"] !== undefined)
    .flatMap((f) => f["conversationDelta"].items as Array<Record<string, any>>);
}

/** Every arm `AgentEmission` names. An item outside this set is a contract change. */
const AGENT_EMISSION_ARMS = new Set([
  "response",
  "thinking",
  "toolCall",
  "toolResult",
  "toolOutcome",
  "skillBody",
  "turnResult",
]);

/**
 * The sole `AgentEmission` arm an item carries.
 *
 * A missing arm, several arms, or an arm the contract does not name is a wire
 * change this suite exists to catch, so each hard-errors rather than being
 * quietly skipped into a passing assertion.
 */
function agentArm(item: Record<string, any>): string {
  const agent = item["agent"];
  if (agent === undefined) {
    throw new Error(`item ${item["uuid"]}: expected an agent emission, got ${Object.keys(item)}`);
  }
  const arms = Object.keys(agent);
  if (arms.length !== 1) throw new Error(`item ${item["uuid"]}: expected one arm, got ${arms}`);
  const arm = arms[0];
  if (!AGENT_EMISSION_ARMS.has(arm)) {
    throw new Error(`item ${item["uuid"]}: unknown AgentEmission arm ${arm}`);
  }
  return arm;
}

/**
 * The ENVELOPE uuid each item is addressed under, in order.
 *
 * One assistant record is curated into several emissions — a thinking arm per
 * reasoning block, addressed `<envelope-uuid>#thinking:<i>`, then the response
 * arm under the envelope uuid itself. So an item's address is no longer EQUAL
 * to the uuid the shim stamped, but is still rooted in it; this recovers that
 * root, which is the half of the identity the shim owns.
 */
function itemEnvelopeUuids(items: Array<Record<string, any>> = conversationItems()): string[] {
  return items.map((item) => {
    agentArm(item);
    return String(item["uuid"]).split("#", 1)[0];
  });
}

/**
 * The message ids the items carry, in order, for the items that carry one.
 *
 * Only the response arm holds a durable `ApiAssistantMessage`; a thinking arm's
 * body is a bare `ThinkingBlock` and names no message, because reasoning is
 * stripped out of the response body into its own emission.
 */
function itemMessageIds(items: Array<Record<string, any>> = conversationItems()): string[] {
  const out: string[] = [];
  for (const item of items) {
    if (agentArm(item) !== "response") continue;
    const body = item["agent"]["response"]["body"];
    if (body === undefined) throw new Error(`item ${item["uuid"]}: response carries no body`);
    out.push(String(body["id"]));
  }
  return out;
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

  it("addresses every item the fixture's wire declares under a stamped envelope uuid", () => {
    // The wire used to copy an assistant record onto one item, so this was a
    // plain equality against `assistantMessage`. Curation now splits a record
    // into a thinking emission plus a response emission, so the surviving
    // claim is that every item's address is ROOTED in the shim's stamp.
    // Arrange / Act / Assert
    expect(itemEnvelopeUuids()).toEqual(assistantRecords().map((r) => r.uuid));
  });

  it("repeats the shim-stamped message id on every durable body the wire carries", () => {
    // The other half of the retired equality: the id survives the curation,
    // on the one arm that still carries a durable assistant message.
    // Arrange / Act
    const carried = itemMessageIds();
    // Assert
    expect(carried.length).toBeGreaterThan(0);
    expect(new Set(carried)).toEqual(new Set(assistantRecords().map((r) => r.messageId)));
  });

  it("hard-errors on an item carrying no agent emission", () => {
    // Arrange / Act / Assert — a non-agent item must not slip through as a
    // silently skipped, still-passing assertion.
    expect(() => itemEnvelopeUuids([{ uuid: "env-1", userMessage: {} }])).toThrow(
      /expected an agent emission/,
    );
  });

  it("hard-errors on an emission arm the contract does not name", () => {
    // Arrange / Act / Assert
    expect(() => itemEnvelopeUuids([{ uuid: "env-1", agent: { whistling: {} } }])).toThrow(
      /unknown AgentEmission arm whistling/,
    );
  });

  it("hard-errors on a response emission carrying no body", () => {
    // Arrange / Act / Assert — an empty response would otherwise read as an
    // item that simply declares no message id.
    expect(() => itemMessageIds([{ uuid: "env-1", agent: { response: {} } }])).toThrow(
      /response carries no body/,
    );
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
