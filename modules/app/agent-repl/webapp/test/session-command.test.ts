/**
 * The session-command item: the feed's record that the user ran a slash
 * command the CLI answered itself, in place of the prompt bubble that used to
 * claim they had said it to the agent.
 */
import { describe, expect, it } from "vitest";

import type { ConversationItem, SessionCommandItem } from "../src/store.js";
import { SESSION_COMMANDS, decodeFrontendFrame, sessionCommandOf } from "../src/frontend-proto.js";
import { SessionCommand as GeneratedSessionCommand } from "../../proto/gen/ts/agentshim/frontend/v1/slash-menu_pb";
import { StateAdapter, type AdapterEffect } from "../src/state-adapter.js";
import { renderItem, itemKey, sessionCommandLabel } from "../src/render.js";

/** One frame through a fresh adapter, as the effects it produces. */
function applyOne(obj: unknown): AdapterEffect[] {
  return new StateAdapter().apply(decodeFrontendFrame(JSON.stringify(obj)));
}

/** The store items one conversation-item frame decomposes into. */
function itemsFrom(item: Record<string, unknown>): ConversationItem[] {
  const effects = applyOne({
    conversationDelta: {
      fence: "s1",
      workspace: "ws",
      throughSeq: "9",
      items: [{ source: "CONVERSATION_SOURCE_USER", ...item }],
    },
  });
  const conv = effects.find((e) => e.kind === "conversation-items");
  if (conv?.kind !== "conversation-items") throw new Error("no conversation-items effect");
  return conv.items;
}

/** A session-command item for COMMAND. */
function commandItem(command: SessionCommandItem["command"]): SessionCommandItem {
  return { kind: "session-command", uuid: "sc1", command };
}

describe("sessionCommandOf", () => {
  it("reads a prefixed wire value", () => {
    // Arrange + Act + Assert
    expect(sessionCommandOf("SESSION_COMMAND_MODEL", "SessionCommandItem")).toBe("MODEL");
  });

  it("reads a bare value", () => {
    // Arrange + Act + Assert
    expect(sessionCommandOf("COMPACT", "SessionCommandItem")).toBe("COMPACT");
  });

  it("throws on UNSPECIFIED rather than guessing a command", () => {
    // Arrange + Act + Assert — the command IS the item's entire content, so an
    // item that cannot say which one it reports is empty.
    expect(() => sessionCommandOf("SESSION_COMMAND_UNSPECIFIED", "SessionCommandItem")).toThrow(
      /unrecognized value/,
    );
  });

  it("throws on a command this build does not know", () => {
    // Arrange + Act + Assert
    expect(() => sessionCommandOf("SESSION_COMMAND_TELEPORT", "SessionCommandItem")).toThrow(
      /unrecognized value/,
    );
  });
});

describe("the sessionCommand arm", () => {
  it("maps the command and the envelope uuid", () => {
    // Arrange + Act
    const items = itemsFrom({ uuid: "m1", sessionCommand: { command: "SESSION_COMMAND_MODEL" } });

    // Assert
    const expected: SessionCommandItem = { kind: "session-command", uuid: "m1", command: "MODEL" };
    expect(items).toEqual([expected]);
  });

  it("carries no text, because the wire message has none to carry", () => {
    // Arrange — THE invariant: `/model opus` and `/model` are indistinguishable
    // by the time they reach this end, so the argument the user typed cannot
    // reappear in the feed.
    const items = itemsFrom({ uuid: "m1", sessionCommand: { command: "SESSION_COMMAND_MODEL" } });

    // Act
    const keys = Object.keys(items[0]);

    // Assert
    expect(keys.sort()).toEqual(["command", "kind", "uuid"]);
  });

  it("rejects a frame whose command cannot be read", () => {
    // Arrange + Act + Assert
    expect(() => itemsFrom({ uuid: "m1", sessionCommand: {} })).toThrow(/unrecognized value/);
  });
});

describe("the command set is the schema's", () => {
  it("covers every generated enum arm except UNSPECIFIED", () => {
    // Arrange — this parity used to be maintained by review across three
    // hand-written tables. A command added to the wire simply went missing
    // from the webapp, and both sides' tests passed.
    const generated = Object.keys(GeneratedSessionCommand).filter(
      (key) => Number.isNaN(Number(key)) && key !== "UNSPECIFIED",
    );

    // Act + Assert.
    expect([...SESSION_COMMANDS].sort()).toEqual(generated.sort());
  });

  it("excludes UNSPECIFIED, which names no command", () => {
    // Arrange + Act + Assert — an item reporting it is malformed rather than
    // a command the user ran.
    expect(SESSION_COMMANDS).not.toContain("UNSPECIFIED");
  });
});

describe("sessionCommandLabel", () => {
  it("names every command in the closed set", () => {
    // Arrange + Act + Assert — a command with no label would render blank,
    // which reads as a feed glitch rather than as a command that ran.
    for (const command of SESSION_COMMANDS) {
      expect(sessionCommandLabel(command)).toMatch(/^\/[a-z-]+$/);
    }
  });

  it("writes the model command in its slash form", () => {
    // Arrange + Act + Assert
    expect(sessionCommandLabel("MODEL")).toBe("/model");
  });
});

describe("rendering a session command", () => {
  it("draws a chip naming the command", () => {
    // Arrange + Act
    const html = renderItem(commandItem("MODEL"));

    // Assert
    expect(html).toContain("session-command");
    expect(html).toContain("/model");
  });

  it("draws no user bubble", () => {
    // Arrange — the whole point: the agent never saw this, so nothing may
    // appear on the user's side of the feed claiming it did.
    const html = renderItem(commandItem("MODEL"));

    // Assert
    expect(html).not.toContain("user-turn");
  });

  it("keys the node on the uuid so a resync reuses it", () => {
    // Arrange + Act + Assert — the uuid is derived from the submit's request
    // id, so a replayed invocation lands on its own node.
    expect(itemKey(commandItem("MODEL"), 3)).toBe("session-command:sc1");
  });
});
