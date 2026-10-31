/**
 * The vendor's echo of a user message, and whose it was.
 *
 * WHAT THIS GUARDS: that asking the vendor for echoes changes nothing about
 * the shim's own sends (each is recognized as its own), and that only a
 * person's words on the main thread are ever read as a prompt typed on
 * claude.ai, never a command's output, a notification or a peer's message.
 */
import { describe, expect, it } from "vitest";
import { classifyReplay, isReplay, OWN_SENDS_REMEMBERED, OwnSends } from "../../src/engine/replays.js";
import type { SdkMessage, SdkUserMessageReplay } from "../../src/sdk/types.js";

/** One echo, its content and fields varied. */
function replay(fields: Record<string, unknown> = {}): SdkUserMessageReplay {
  return {
    type: "user",
    message: { role: "user", content: "What changed since I left?" },
    parent_tool_use_id: null,
    uuid: "00000000-0000-4000-8000-0000000000aa",
    session_id: "s",
    isReplay: true,
    ...fields,
  };
}

describe("OwnSends", () => {
  it("knows a uuid it was told of", () => {
    // Arrange
    const own = new OwnSends();

    // Act
    own.note("u-1");

    // Assert
    expect(own.has("u-1")).toBe(true);
  });

  it("does not know a uuid it was never told of", () => {
    // Arrange, Act, Assert
    expect(new OwnSends().has("u-1")).toBe(false);
  });

  it("forgets the oldest once more than its bound were sent", () => {
    // Arrange
    const own = new OwnSends();

    // Act
    for (let index = 0; index <= OWN_SENDS_REMEMBERED; index++) own.note(`u-${index}`);

    // Assert
    expect([own.has("u-0"), own.has("u-1"), own.has(`u-${OWN_SENDS_REMEMBERED}`)]).toEqual([false, true, true]);
  });

  it("keeps a re-delivered send as the newest, so it is not the next forgotten", () => {
    // Arrange
    const own = new OwnSends();
    for (let index = 0; index < OWN_SENDS_REMEMBERED; index++) own.note(`u-${index}`);

    // Act
    own.note("u-0");
    own.note("u-new");

    // Assert
    expect([own.has("u-0"), own.has("u-1")]).toEqual([true, false]);
  });
});

describe("isReplay", () => {
  it("is true for a user message marked isReplay", () => {
    // Arrange, Act, Assert
    expect(isReplay(replay())).toBe(true);
  });

  it("is false for an ordinary user message (a tool result)", () => {
    // Arrange, Act, Assert
    expect(isReplay({ ...replay(), isReplay: undefined })).toBe(false);
  });

  it("is false for a message of another type", () => {
    // Arrange, Act, Assert
    expect(isReplay({ type: "result", subtype: "success" } as unknown as SdkMessage)).toBe(false);
  });
});

describe("classifyReplay", () => {
  it("names the echo of one of the shim's own sends as its own", () => {
    // Arrange
    const own = new OwnSends();
    own.note("00000000-0000-4000-8000-0000000000aa");

    // Act, Assert
    expect(classifyReplay(replay(), own).kind).toBe("own");
  });

  it("names a person's string prompt under a foreign uuid as remote", () => {
    // Arrange, Act
    const classified = classifyReplay(replay(), new OwnSends());

    // Assert
    expect(classified.kind === "remote" ? classified.text : classified.kind).toBe("What changed since I left?");
  });

  it("states a remote prompt's words as one text block", () => {
    // Arrange, Act
    const classified = classifyReplay(replay(), new OwnSends());

    // Assert
    const block = classified.kind === "remote" ? classified.said.content?.blocks[0]?.block : undefined;
    expect(block?.case === "text" ? block.value.text : undefined).toBe("What changed since I left?");
  });

  it("joins a block prompt's text blocks and leaves its other blocks out", () => {
    // Arrange
    const message = replay({
      message: {
        role: "user",
        content: [
          { type: "text", text: "look at this" },
          { type: "image", source: { type: "base64", media_type: "image/png", data: "" } },
          { type: "text", text: "and fix it" },
        ],
      },
    });

    // Act
    const classified = classifyReplay(message, new OwnSends());

    // Assert
    expect(classified.kind === "remote" ? classified.text : classified.kind).toBe("look at this\nand fix it");
  });

  it("reads a prompt of human origin as remote", () => {
    // Arrange, Act, Assert
    expect(classifyReplay(replay({ origin: { kind: "human" } }), new OwnSends()).kind).toBe("remote");
  });

  it.each([
    { name: "a subagent's message", fields: { parent_tool_use_id: "toolu_spawn" } },
    { name: "a synthetic message", fields: { isSynthetic: true } },
    { name: "a task notification", fields: { origin: { kind: "task-notification" } } },
    { name: "a peer's message", fields: { origin: { kind: "peer", from: "uds:/x" } } },
    { name: "a wordless message", fields: { message: { role: "user", content: "   " } } },
    {
      name: "a command's replayed output",
      fields: { message: { role: "user", content: "<local-command-stdout>ok</local-command-stdout>" } },
    },
    { name: "a shell line typed remotely", fields: { message: { role: "user", content: "<bash-input>ls</bash-input>" } } },
  ])("names $name under a foreign uuid as other", ({ fields }) => {
    // Arrange, Act, Assert
    expect(classifyReplay(replay(fields), new OwnSends()).kind).toBe("other");
  });

  it("reads a prompt that merely contains a tag as remote", () => {
    // Arrange
    const message = replay({ message: { role: "user", content: "why does <bash-input> show up?" } });

    // Act, Assert
    expect(classifyReplay(message, new OwnSends()).kind).toBe("remote");
  });
});
