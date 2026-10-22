/**
 * An agent's prompt, sent as another session's message.
 *
 * WHAT THIS GUARDS: the ruled policy (2026-10-20) that an agent's prompt
 * declares its SENDER's own permission class, honestly, and that a message
 * the vendor holds refuses the StartTurn rather than stalling it; plus the
 * envelope and the two undeclared vendor frames the shim reads to know.
 */
import { create } from "@bufbuild/protobuf";
import { afterEach, describe, expect, it, vi } from "vitest";
import { logRecordsSince, logSinkMark } from "../log-records.js";
import {
  PeerAdmissions,
  PeerNotAdmittedError,
  commandLifecycleOf,
  peerAddressLabel,
  peerDeclarationOf,
  peerContent,
  peerEnvelope,
  peerHoldOf,
  permissionClassOf,
} from "../../src/engine/peer-send.js";
import { conversationv1, shimv1 } from "../../src/proto.js";
import type { SdkMessage } from "../../src/sdk/types.js";

const UUID = "00000000-0000-4000-8000-000000000001";

/** A permission mode on the arm named. */
function mode(arm: string): conversationv1.AgentPermissionMode {
  return create(conversationv1.AgentPermissionModeSchema, { mode: { case: arm, value: {} } as never });
}

const lifecycle = (state: string, uuid = UUID): SdkMessage =>
  ({ type: "command_lifecycle", command_uuid: uuid, state, uuid: "frame-uuid", session_id: "s" }) as unknown as SdkMessage;

const hold = (fields: Record<string, unknown>): SdkMessage =>
  ({
    type: "system",
    subtype: "peer_message_hold",
    state: "held",
    message_uuid: UUID,
    lane: "stdin",
    from: "agent-repl:parser-fix",
    cause: "mode-mismatch",
    uuid: "frame-uuid",
    session_id: "s",
    ...fields,
  }) as unknown as SdkMessage;

/** Whether PROMISE has settled after the microtasks already queued. */
async function settled(promise: Promise<unknown>): Promise<"resolved" | "rejected" | "pending"> {
  let state: "resolved" | "rejected" | "pending" = "pending";
  promise.then(
    () => {
      state = "resolved";
    },
    () => {
      state = "rejected";
    },
  );
  await new Promise((resolve) => setImmediate(resolve));
  return state;
}

/** The rejection PROMISE settles with. */
async function rejection(promise: Promise<unknown>): Promise<Error> {
  try {
    await promise;
  } catch (err) {
    return err as Error;
  }
  throw new Error("the promise resolved");
}

afterEach(() => {
  vi.useRealTimers();
});

describe("permissionClassOf", () => {
  it.each([
    ["bypass", "bypass"],
    ["default", "prompting"],
    ["acceptEdits", "prompting"],
    ["plan", "prompting"],
    ["dontAsk", "prompting"],
    ["auto", "prompting"],
  ])("declares %s as %s", (arm, want) => {
    // Arrange, Act, Assert
    expect(permissionClassOf(mode(arm))).toBe(want);
  });

  it("refuses a mode that names no mode", () => {
    // Arrange, Act, Assert
    expect(() => permissionClassOf(create(conversationv1.AgentPermissionModeSchema, {}))).toThrow(/names no mode/);
  });
});

describe("peerAddressLabel", () => {
  it.each([
    ["an address-shaped name whole", "parser-fix_2.v1/a:b", "parser-fix_2.v1/a:b"],
    ["a space as its percent-encoding", "parser fix", "parser%20fix"],
    ["a non-ASCII character as its UTF-8 bytes", "fixé", "fix%C3%A9"],
    ["a percent sign itself", "100%", "100%25"],
  ])("keeps %s", (_name, label, want) => {
    // Arrange, Act, Assert
    expect(peerAddressLabel(label)).toBe(want);
  });
});

describe("peerDeclarationOf", () => {
  it("declares a named sender by its name, with its own class", () => {
    // Arrange
    const sentBy = create(shimv1.StartTurnSentBySchema, { workspaceId: "ws-1", workspaceName: "parser-fix", permissionMode: mode("bypass") });
    // Act, Assert
    expect(peerDeclarationOf(sentBy)).toEqual({ from: "agent-repl:parser-fix", fromName: "parser-fix", fromMode: "bypass" });
  });

  it("declares a sender the daemon cannot name by its id, with no name", () => {
    // Arrange
    const sentBy = create(shimv1.StartTurnSentBySchema, { workspaceId: "ws-1", permissionMode: mode("auto") });
    // Act, Assert
    expect(peerDeclarationOf(sentBy)).toEqual({ from: "agent-repl:ws-1", fromMode: "prompting" });
  });

  it("declares no class for a sender whose mode the daemon does not hold", () => {
    // Arrange
    const sentBy = create(shimv1.StartTurnSentBySchema, { workspaceId: "ws-1", workspaceName: "parser-fix" });
    // Act, Assert
    expect(peerDeclarationOf(sentBy).fromMode).toBeUndefined();
  });
});

describe("peerEnvelope", () => {
  it("wraps the prompt in the vendor's cross-session envelope", () => {
    // Arrange, Act, Assert
    expect(peerEnvelope("review the parser", { from: "agent-repl:parser-fix", fromName: "parser-fix" })).toBe(
      '<cross-session-message from="agent-repl:parser-fix" from-name="parser-fix">\nreview the parser\n</cross-session-message>',
    );
  });

  it("states no from-name for an unnamed sender", () => {
    // Arrange, Act, Assert
    expect(peerEnvelope("hi", { from: "agent-repl:ws-1" })).toBe('<cross-session-message from="agent-repl:ws-1">\nhi\n</cross-session-message>');
  });

  it("escapes a name the attribute could not carry", () => {
    // Arrange, Act
    const envelope = peerEnvelope("hi", { from: "agent-repl:x", fromName: 'a "b" <c>\nd & e' });
    // Assert
    expect(envelope.split("\n")[0]).toBe('<cross-session-message from="agent-repl:x" from-name="a &quot;b&quot; &lt;c&gt; d &amp; e">');
  });
});

describe("peerContent", () => {
  const peer = { from: "agent-repl:ws-1" };

  it("is the envelope's string for words alone, exactly as peerEnvelope spells it", () => {
    expect(peerContent("hi", peer)).toBe(peerEnvelope("hi", peer));
  });

  it("puts the envelope's tags around a block list, every block kept in its place", () => {
    // Arrange
    const image = { type: "image" as const, source: { type: "url" as const, url: "https://example.invalid/a.png" } };

    // Act
    const content = peerContent([{ type: "text", text: "look" }, image], peer);

    // Assert
    expect(content).toEqual([
      { type: "text", text: '<cross-session-message from="agent-repl:ws-1">' },
      { type: "text", text: "look" },
      image,
      { type: "text", text: "</cross-session-message>" },
    ]);
  });
});

describe("commandLifecycleOf", () => {
  it("reads the command's uuid and state", () => {
    // Arrange, Act, Assert
    expect(commandLifecycleOf(lifecycle("started"))).toEqual({ commandUuid: UUID, state: "started" });
  });

  it("answers absence for any other message", () => {
    // Arrange, Act, Assert
    expect(commandLifecycleOf(hold({}))).toBeUndefined();
  });

  it("answers absence, at ERROR, for a frame naming no known state", () => {
    // Arrange
    const mark = logSinkMark();
    // Act
    const read = commandLifecycleOf(lifecycle("exploded"));
    // Assert
    expect([read, logRecordsSince(mark).some((record) => record.level === "error")]).toEqual([undefined, true]);
  });
});

describe("peerHoldOf", () => {
  it("reads a hold's uuid, cause and lane", () => {
    // Arrange, Act
    const read = peerHoldOf(hold({}));
    // Assert
    expect([read?.state, read?.messageUuid, read?.cause, read?.lane]).toEqual(["held", UUID, "mode-mismatch", "stdin"]);
  });

  it("answers absence for any other system frame", () => {
    // Arrange, Act, Assert
    expect(peerHoldOf({ type: "system", subtype: "status", uuid: "u", session_id: "s" } as unknown as SdkMessage)).toBeUndefined();
  });
});

describe("PeerAdmissions", () => {
  it.each([["queued"], ["started"]])("admits a send the vendor %s", async (state) => {
    // Arrange
    const admissions = new PeerAdmissions();
    const admitted = admissions.expect(UUID, "turn-1", "agent-repl:parser-fix");
    // Act
    admissions.observe(lifecycle(state));
    // Assert
    expect(await settled(admitted)).toBe("resolved");
  });

  it("refuses a send the vendor held, naming why", async () => {
    // Arrange
    const admissions = new PeerAdmissions();
    const admitted = admissions.expect(UUID, "turn-1", "agent-repl:parser-fix");
    // Act
    admissions.observe(hold({}));
    // Assert
    const err = await rejection(admitted);
    expect([err instanceof PeerNotAdmittedError, /HELD.*permission class is not this session's/.test(err.message)]).toEqual([true, true]);
  });

  it("refuses a send held by this session's settings, naming the setting", async () => {
    // Arrange
    const admissions = new PeerAdmissions();
    const admitted = admissions.expect(UUID, "turn-1", "agent-repl:parser-fix");
    // Act
    admissions.observe(hold({ cause: "managed-setting" }));
    // Assert
    expect((await rejection(admitted)).message).toMatch(/crossSessionInbound settings .*managed-setting/);
  });

  it.each([["cancelled"], ["discarded"], ["refused"]])("refuses a send the vendor %s", async (state) => {
    // Arrange
    const admissions = new PeerAdmissions();
    const admitted = admissions.expect(UUID, "turn-1", "agent-repl:parser-fix");
    // Act
    admissions.observe(lifecycle(state));
    // Assert
    expect((await rejection(admitted)).message).toContain(`the vendor ${state}`);
  });

  it("refuses a send the vendor said nothing of within the budget", async () => {
    // Arrange
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    const admissions = new PeerAdmissions(50);
    const admitted = admissions.expect(UUID, "turn-1", "agent-repl:parser-fix");
    const refused = rejection(admitted);
    // Act
    vi.advanceTimersByTime(51);
    // Assert
    expect((await refused).message).toMatch(/within 50ms/);
  });

  it("refuses every waiting send when the query ends", async () => {
    // Arrange
    const admissions = new PeerAdmissions();
    const admitted = admissions.expect(UUID, "turn-1", "agent-repl:parser-fix");
    // Act
    admissions.abandonAll("the stream ended");
    // Assert
    expect((await rejection(admitted)).message).toMatch(/query ended before it queued the message: the stream ended/);
  });

  it("is not settled by a lifecycle frame about another send", async () => {
    // Arrange
    const admissions = new PeerAdmissions();
    const admitted = admissions.expect(UUID, "turn-1", "agent-repl:parser-fix");
    // Act
    admissions.observe(lifecycle("queued", "00000000-0000-4000-8000-0000000000ff"));
    // Assert
    expect(await settled(admitted)).toBe("pending");
  });

  it("is not settled by a hold on the socket lane, whose uuid is another session's", async () => {
    // Arrange
    const admissions = new PeerAdmissions();
    const admitted = admissions.expect(UUID, "turn-1", "agent-repl:parser-fix");
    // Act
    admissions.observe(hold({ lane: "socket" }));
    // Assert
    expect(await settled(admitted)).toBe("pending");
  });

  it("refuses a second wait on one send", async () => {
    // Arrange
    const admissions = new PeerAdmissions();
    void admissions.expect(UUID, "turn-1", "agent-repl:parser-fix");
    // Act, Assert
    expect((await rejection(admissions.expect(UUID, "turn-1", "agent-repl:parser-fix"))).message).toMatch(/already awaiting/);
  });

  it("records at ERROR a hold about a send it already admitted", () => {
    // Arrange
    const admissions = new PeerAdmissions();
    void admissions.expect(UUID, "turn-1", "agent-repl:parser-fix");
    admissions.observe(lifecycle("queued"));
    const mark = logSinkMark();
    // Act
    admissions.observe(hold({}));
    // Assert
    expect(logRecordsSince(mark).filter((record) => record.level === "error").map((record) => record.message)).toEqual([
      "the vendor reported a hold for a peer send no StartTurn is waiting on",
    ]);
  });

  it("records the drop that ends a refused hold at INFO, not as a fault", () => {
    // Arrange
    const admissions = new PeerAdmissions();
    void admissions.expect(UUID, "turn-1", "agent-repl:parser-fix").catch(() => undefined);
    admissions.observe(hold({}));
    const mark = logSinkMark();
    // Act
    admissions.observe(hold({ state: "dropped", cause: undefined, outcome: "expired" }));
    // Assert
    expect(logRecordsSince(mark).map((record) => [record.level, record.message])).toEqual([
      ["info", "the vendor dropped a peer send it had held; its StartTurn was already refused"],
    ]);
  });

  it("leaves a withdrawn send unsettled and its timer cleared", async () => {
    // Arrange
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    const admissions = new PeerAdmissions(50);
    const admitted = admissions.expect(UUID, "turn-1", "agent-repl:parser-fix");
    // Act
    admissions.withdraw(UUID);
    vi.advanceTimersByTime(51);
    // Assert
    expect([await settled(admitted), vi.getTimerCount()]).toEqual(["pending", 0]);
  });
});
