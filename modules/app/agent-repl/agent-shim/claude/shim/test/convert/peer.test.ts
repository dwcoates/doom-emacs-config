/**
 * A MESSAGE FROM ANOTHER CLAUDE, on the live stream plane.
 *
 * These pin the discriminator (`origin.kind == "peer"` covers inter-session
 * peers and subagent hand-backs), the sender/body resolution, and — through
 * convertUserRecord — that a peer record becomes the ONE peer-message row while
 * a genuine (non-peer) user record still follows its existing R15 path.
 */
import { describe, expect, it } from "vitest";
import type { SdkMessage } from "../../src/sdk/types.js";
import { convertPeerMessage, peerFacts, userRecordText } from "../../src/convert/peer.js";
import { convertUserRecord } from "../../src/convert/tool-results.js";
import { createTaskKindRegistry } from "../../src/convert/detached.js";
import { createCallRegistry } from "../../src/convert/tool-calls.js";
import { TOOL_CONVERTERS } from "../../src/convert/tools/registry.js";
import { peerUpsertKey } from "../../src/store/keys.js";
import { foldContext, MAIN_AGENT, type ContextOverrides } from "./fold-harness.js";

type UserRecord = Extract<SdkMessage, { type: "user" }>;

/** One `type: "user"` record, as the vendor spells one. */
function userRecord(fields: Record<string, unknown>): UserRecord {
  return {
    type: "user",
    uuid: "uuid-peer",
    session_id: "session-1",
    parent_tool_use_id: null,
    ...fields,
  } as unknown as UserRecord;
}

/** A peer-message record: a user record whose `origin.kind` is "peer". */
function peerRecord(origin: Record<string, unknown>, fields: Record<string, unknown> = {}): UserRecord {
  return userRecord({
    isMeta: true,
    promptSource: "system",
    origin: { kind: "peer", ...origin },
    message: { role: "user", content: "Another Claude session sent a message:\n<agent-message>hi</agent-message>" },
    ...fields,
  });
}

/** What one user record converts to through the full user-record path. */
function convert(message: UserRecord, overrides: ContextOverrides = {}) {
  return convertUserRecord(
    message,
    foldContext(overrides),
    createCallRegistry(),
    TOOL_CONVERTERS,
    createTaskKindRegistry(),
  );
}

describe("peerFacts — the shared discriminator", () => {
  it("returns undefined when the record has no origin", () => {
    // Arrange
    const record = { message: { content: "hi" } };
    // Act & Assert
    expect(peerFacts(record, "hi")).toBeUndefined();
  });

  it("returns undefined when origin.kind is not peer", () => {
    // Arrange
    const record = { origin: { kind: "human" } };
    // Act & Assert
    expect(peerFacts(record, "hi")).toBeUndefined();
  });

  it("reads the sender from origin.from when present", () => {
    // Arrange
    const record = { origin: { kind: "peer", from: "Explore", senderTaskId: "task-9" } };
    // Act
    const facts = peerFacts(record, "fallback");
    // Assert
    expect(facts?.sender).toBe("Explore");
  });

  it("falls back to origin.senderTaskId when from is absent", () => {
    // Arrange
    const record = { origin: { kind: "peer", senderTaskId: "task-9" } };
    // Act
    const facts = peerFacts(record, "fallback");
    // Assert
    expect(facts?.sender).toBe("task-9");
  });

  it("prefers origin.body for the message body", () => {
    // Arrange
    const record = { origin: { kind: "peer", from: "X", body: "the real body" } };
    // Act
    const facts = peerFacts(record, "record-text");
    // Assert
    expect(facts?.body).toBe("the real body");
  });

  it("falls back to the record text when origin.body is absent", () => {
    // Arrange
    const record = { origin: { kind: "peer", from: "X" } };
    // Act
    const facts = peerFacts(record, "record-text");
    // Assert
    expect(facts?.body).toBe("record-text");
  });

  it("treats a handback (origin.handback) as a peer message too", () => {
    // Arrange
    const record = { origin: { kind: "peer", from: "child", handback: true, body: "done" } };
    // Act
    const facts = peerFacts(record, "fallback");
    // Assert
    expect(facts).toEqual({ sender: "child", body: "done" });
  });
});

describe("userRecordText", () => {
  it("returns a bare string content whole", () => {
    expect(userRecordText({ content: "just words" })).toBe("just words");
  });

  it("joins the text blocks of a block-list content", () => {
    expect(userRecordText({ content: [{ type: "text", text: "a" }, { type: "text", text: "b" }] })).toBe("a\nb");
  });
});

describe("convertPeerMessage", () => {
  it("emits a peer-message entry for a peer record", () => {
    // Arrange
    const message = peerRecord({ from: "Explore", body: "found it" });
    // Act
    const entry = convertPeerMessage(message, foldContext());
    // Assert
    expect(entry?.item.kind).toBe("peer");
  });

  it("keys the entry on the record uuid (the cross-plane contract)", () => {
    // Arrange
    const message = peerRecord({ from: "Explore" }, { uuid: "uuid-XYZ" });
    // Act
    const entry = convertPeerMessage(message, foldContext());
    // Assert
    expect(entry?.upsertKey).toBe(peerUpsertKey("uuid-XYZ"));
  });

  it("spells the record uuid into PeerMessage.id so both planes collapse", () => {
    // Arrange
    const message = peerRecord({ from: "Explore" }, { uuid: "uuid-XYZ" });
    // Act
    const entry = convertPeerMessage(message, foldContext());
    // Assert
    expect(entry?.item.kind === "peer" ? entry.item.peer.id : "").toBe("uuid-XYZ");
  });

  it("books the message to the main agent for a top-level peer message", () => {
    // Arrange
    const message = peerRecord({ from: "Explore" });
    // Act
    const entry = convertPeerMessage(message, foldContext());
    // Assert
    expect(entry?.item.kind === "peer" ? entry.item.peer.agent?.value : "").toBe(MAIN_AGENT.value);
  });

  it("marks a record with origin.handback as a subagent hand-back", () => {
    // Arrange
    const message = peerRecord({ from: "a1d968043b47deee9", handback: true });
    // Act
    const entry = convertPeerMessage(message, foldContext());
    // Assert
    expect(entry?.item.kind === "peer" ? entry.item.peer.kind.case : "").toBe("subagentHandback");
  });

  it("marks a record with no origin.handback as an inter-session message", () => {
    // Arrange
    const message = peerRecord({ from: "Explore" });
    // Act
    const entry = convertPeerMessage(message, foldContext());
    // Assert
    expect(entry?.item.kind === "peer" ? entry.item.peer.kind.case : "").toBe("interSession");
  });

  it("returns undefined for a non-peer user record", () => {
    // Arrange
    const message = userRecord({ message: { role: "user", content: "hello" } });
    // Act & Assert
    expect(convertPeerMessage(message, foldContext())).toBeUndefined();
  });

  it("returns undefined for a peer record with no uuid, leaving it to the ordinary path", () => {
    // Arrange
    const message = peerRecord({ from: "Explore" }, { uuid: "" });
    // Act & Assert
    expect(convertPeerMessage(message, foldContext())).toBeUndefined();
  });
});

describe("convertUserRecord routing", () => {
  it("routes a peer record to the peer-message row", () => {
    // Arrange
    const message = peerRecord({ from: "Explore", body: "found it" });
    // Act
    const entries = convert(message);
    // Assert
    expect(entries.map((e) => e.item.kind)).toEqual(["peer"]);
  });

  it("still drops a genuine (non-peer) prompt echo under R15 (regression)", () => {
    // Arrange
    const message = userRecord({ message: { role: "user", content: "hello" } });
    // Act & Assert
    expect(convert(message)).toEqual([]);
  });
});
