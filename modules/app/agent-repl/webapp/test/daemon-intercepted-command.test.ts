/**
 * The daemon-intercepted-command item: the feed's record that the user ran a slash
 * command the CLI answered itself, in place of the prompt bubble that used to
 * claim they had said it to the agent.
 */
import { describe, expect, it } from "vitest";

import { ConversationStore, type ConversationItem, type DaemonInterceptedCommandItem } from "../src/store.js";
import { SESSION_COMMANDS, decodeFrontendFrame, sessionCommandOf } from "../src/frontend-proto.js";
import { SessionCommand as GeneratedSessionCommand } from "../../proto/gen/ts/frontend/v1/shared_pb";
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
      messages: [{ source: "CONVERSATION_SOURCE_USER", ...item }],
    },
  });
  const conv = effects.find((e) => e.kind === "conversation-items");
  if (conv?.kind !== "conversation-items") throw new Error("no conversation-items effect");
  return conv.items;
}

/** A store holding the workspace fence a page is measured against. */
function armedStore(): ConversationStore {
  const store = new ConversationStore();
  store.ingest([
    {
      kind: "workspace-state",
      value: {
        workspace: "ws",
        sessionId: "s1",
        fence: "f1",
        state: "idle",
        turnActive: false,
        liveTaskCount: 0,
        causeKind: "turn_ended",
        causeSeq: 1,
        atMs: 1000,
        connectivity: "operational",
        sessionStatus: "ready",
        controllerGenerationId: "g1",
        activeFaults: [],
        mergeLeaseHeld: false,
        mergeStatus: null,
        mergeDequeueOffer: null,
      },
    },
  ]);
  return store;
}

/** One tail page of history carrying ITEMS. */
function pageEffect(requestId: string, items: ConversationItem[]): AdapterEffect {
  return {
    kind: "conversation-page",
    workspace: "ws",
    fence: "f1",
    requestId,
    items,
    continuation: { case: "start" },
    liveJoinSeq: 9,
  };
}

/** A daemon-intercepted-command item for COMMAND. */
function commandItem(command: DaemonInterceptedCommandItem["command"]): DaemonInterceptedCommandItem {
  return { kind: "daemon-intercepted-command", uuid: "sc1", command };
}

/** The store items one PAGE of history decomposes into — the durable route. */
function itemsFromPage(item: Record<string, unknown>): ConversationItem[] {
  const effects = applyOne({
    conversationPage: {
      workspace: "ws",
      requestId: "r-1",
      messages: [{ source: "CONVERSATION_SOURCE_USER", ...item }],
      start: {},
      liveJoinSeq: "9",
      fence: "s1",
    },
  });
  const page = effects.find((e) => e.kind === "conversation-page");
  if (page?.kind !== "conversation-page") throw new Error("no conversation-page effect");
  return page.items;
}

describe("sessionCommandOf", () => {
  it("reads a prefixed wire value", () => {
    // Arrange + Act + Assert
    expect(sessionCommandOf("SESSION_COMMAND_MODEL", "DaemonInterceptedCommandItem")).toBe("MODEL");
  });

  it("reads a bare value", () => {
    // Arrange + Act + Assert
    expect(sessionCommandOf("COMPACT", "DaemonInterceptedCommandItem")).toBe("COMPACT");
  });

  it("throws on UNSPECIFIED rather than guessing a command", () => {
    // Arrange + Act + Assert — the command IS the item's entire content, so an
    // item that cannot say which one it reports is empty.
    expect(() => sessionCommandOf("SESSION_COMMAND_UNSPECIFIED", "DaemonInterceptedCommandItem")).toThrow(
      /unrecognized value/,
    );
  });

  it("throws on a command this build does not know", () => {
    // Arrange + Act + Assert
    expect(() => sessionCommandOf("SESSION_COMMAND_TELEPORT", "DaemonInterceptedCommandItem")).toThrow(
      /unrecognized value/,
    );
  });
});

describe("the daemonInterceptedCommand arm", () => {
  it("maps the command and the envelope uuid", () => {
    // Arrange + Act
    const items = itemsFrom({ uuid: "m1", daemonInterceptedCommand: { command: "SESSION_COMMAND_MODEL" } });

    // Assert
    const expected: DaemonInterceptedCommandItem = { kind: "daemon-intercepted-command", uuid: "m1", command: "MODEL" };
    expect(items).toEqual([expected]);
  });

  it("carries no text, because the wire message has none to carry", () => {
    // Arrange — THE invariant: `/model opus` and `/model` are indistinguishable
    // by the time they reach this end, so the argument the user typed cannot
    // reappear in the feed.
    const items = itemsFrom({ uuid: "m1", daemonInterceptedCommand: { command: "SESSION_COMMAND_MODEL" } });

    // Act
    const keys = Object.keys(items[0]);

    // Assert
    expect(keys.sort()).toEqual(["command", "kind", "uuid"]);
  });

  it("rejects a frame whose command cannot be read", () => {
    // Arrange + Act + Assert
    expect(() => itemsFrom({ uuid: "m1", daemonInterceptedCommand: {} })).toThrow(/unrecognized value/);
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

describe("a command that arrived DURABLY, from the store", () => {
  it("decodes off a page into the same item a live delta produces", () => {
    // Arrange — a CLI-handled command is a durable record, so a reload brings
    // it back on a ConversationPage instead of on a delta.
    const live = itemsFrom({ uuid: "m1", daemonInterceptedCommand: { command: "SESSION_COMMAND_COMPACT" } });

    // Act
    const paged = itemsFromPage({ uuid: "m1", daemonInterceptedCommand: { command: "SESSION_COMMAND_COMPACT" } });

    // Assert
    expect(paged).toEqual(live);
  });

  it("renders identically whichever route delivered it", () => {
    // Arrange — the item carries no arrival route, so there is nothing a
    // renderer could branch on even if it wanted to.
    const paged = itemsFromPage({ uuid: "m1", daemonInterceptedCommand: { command: "SESSION_COMMAND_COMPACT" } });
    const live = itemsFrom({ uuid: "m1", daemonInterceptedCommand: { command: "SESSION_COMMAND_COMPACT" } });

    // Act + Assert
    expect(renderItem(paged[0])).toBe(renderItem(live[0]));
  });

  it("carries no text off a page either, because the wire message has none", () => {
    // Arrange — the durable route is the one a `/model opus` argument could
    // have been persisted on; it was not, and there is no field to read it from.
    const paged = itemsFromPage({ uuid: "m1", daemonInterceptedCommand: { command: "SESSION_COMMAND_COMPACT" } });

    // Act
    const keys = Object.keys(paged[0]);

    // Assert
    expect(keys.sort()).toEqual(["command", "kind", "uuid"]);
  });

  it("lands in the feed as an ordinary message", () => {
    // Arrange
    const store = armedStore();
    store.notePageRequested({ requestId: "r-1", anchor: "tail", fence: "f1" });

    // Act
    store.ingest([pageEffect("r-1", [{ kind: "daemon-intercepted-command", uuid: "m1", command: "COMPACT" }])]);

    // Assert
    expect(store.state.items).toEqual([
      expect.objectContaining({ kind: "daemon-intercepted-command", uuid: "m1", command: "COMPACT" }),
    ]);
  });

  it("reconciles onto the live row rather than drawing the command twice", () => {
    // Arrange — the tail page and the live replay overlap by construction, so
    // the same command arrives by both routes.
    const store = armedStore();
    const item: DaemonInterceptedCommandItem = { kind: "daemon-intercepted-command", uuid: "m1", command: "COMPACT" };
    store.notePageRequested({ requestId: "r-1", anchor: "tail", fence: "f1" });
    store.ingest([pageEffect("r-1", [item])]);

    // Act
    store.ingest([{ kind: "conversation-items", workspace: "ws", fence: "f1", throughSeq: 9, items: [item] }]);

    // Assert
    expect(store.state.items).toHaveLength(1);
  });
});

describe("rendering an intercepted command", () => {
  it("draws a chip naming the command", () => {
    // Arrange + Act
    const html = renderItem(commandItem("MODEL"));

    // Assert
    expect(html).toContain("daemon-intercepted-command");
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
    expect(itemKey(commandItem("MODEL"), 3)).toBe("daemon-intercepted-command:sc1");
  });
});
