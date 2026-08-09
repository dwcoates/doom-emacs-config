import { describe, expect, it } from "vitest";

import {
  applyStreamDelta,
  blockKey,
  insertBySeq,
  phaseOf,
  previewBlockId,
  recordBlockIdentity,
  settleStreamedBlock,
  type StreamDelta,
} from "../src/streaming.js";
import type { ConversationItem, TextItem, ThinkingItem, ToolItem } from "../src/store.js";

// The ONE authority on a streamed block's identity and lifecycle. The store
// exercises these transitions end-to-end through `ingest`; this suite pins the
// module's own contract — what an identity is, and what each transition does.

const TS = "2026-07-19T12:00:00.000Z";

type TextDelta = { kind: "text"; messageId: string; blockIndex: number; delta: string };
type ThinkingDelta = { kind: "thinking"; messageId: string; blockIndex: number; delta: string };
type InputDelta = { kind: "input_json"; messageId: string; blockIndex: number; delta: string; toolUseId: string };

function delta(over: Partial<Omit<TextDelta, "kind">> = {}): TextDelta {
  return { kind: "text", messageId: "msg_1", blockIndex: 0, delta: "hi", ...over };
}

function thinkingDelta(over: Partial<Omit<ThinkingDelta, "kind">> = {}): ThinkingDelta {
  return { kind: "thinking", messageId: "msg_1", blockIndex: 0, delta: "hi", ...over };
}

function inputDelta(over: Partial<Omit<InputDelta, "kind">> = {}): InputDelta {
  return { kind: "input_json", messageId: "msg_1", blockIndex: 0, delta: "hi", toolUseId: "tu1", ...over };
}

function preview(over: Partial<TextItem> = {}): TextItem {
  return { kind: "text", blockId: "msg_1:0", messageId: "msg_1", text: "hi", done: false, ts: TS, ...over };
}

function finished(over: Partial<TextItem> = {}): TextItem {
  return {
    kind: "text",
    blockId: "env1:0",
    uuid: "env1:0",
    messageId: "msg_1",
    text: "hi there",
    done: true,
    ts: TS,
    ...over,
  };
}

// --- identity ---------------------------------------------------------------

describe("previewBlockId", () => {
  it("names the block by the two facts the live stream states", () => {
    expect(previewBlockId("msg_1", 2)).toBe("msg_1:2");
  });
});

describe("recordBlockIdentity", () => {
  it("takes the identity from the envelope, not the message id", () => {
    // Every record of one message repeats the same message id, so deriving the
    // identity from it collapses a multi-block message onto one key.
    expect(recordBlockIdentity("env1", "msg_1", 0)).toEqual({ blockId: "env1:0", uuid: "env1:0" });
  });

  it("falls back to the message id when the envelope carries none", () => {
    expect(recordBlockIdentity("", "msg_1", 1)).toEqual({ blockId: "msg_1:1", uuid: "msg_1:1" });
  });

  it("leaves the identity absent when there is no id to build one from", () => {
    // Better keyed on its feed place than keyed onto a shared empty id with
    // every other id-less block in the session.
    expect(recordBlockIdentity("", "", 0)).toEqual({ blockId: ":0" });
  });
});

describe("phaseOf", () => {
  it("reads a block with no record identity as previewing", () => {
    expect(phaseOf(preview())).toBe("previewing");
  });

  it("reads a block carrying a record identity as final", () => {
    expect(phaseOf(finished())).toBe("final");
  });
});

describe("blockKey", () => {
  it("keys a preview on its feed place", () => {
    expect(blockKey(preview())).toBe("text:msg_1:0");
  });

  it("keys a settled block on its record identity, which a replay can reproduce", () => {
    expect(blockKey(finished({ blockId: "msg_1:0" }))).toBe("text:u:env1:0");
  });

  it("separates the two kinds so prose never collides with reasoning", () => {
    const thinking: ThinkingItem = { kind: "thinking", blockId: "msg_1:0", messageId: "msg_1", text: "", done: false };
    expect(blockKey(thinking)).not.toBe(blockKey(preview()));
  });
});

// --- feed order ---------------------------------------------------------------

describe("insertBySeq", () => {
  it("slots a lower-seq item above a higher-seq one already mounted", () => {
    // A replayed history item must pass the live item that beat it to the
    // socket — arrival order is interleave, not conversation order.
    const items: ConversationItem[] = [];
    insertBySeq(items, preview({ blockId: "live" }), 100);
    insertBySeq(items, preview({ blockId: "history" }), 5);
    expect(items.map((i) => (i as TextItem).blockId)).toEqual(["history", "live"]);
  });

  it("keeps arrival order for equal seqs", () => {
    // Items of one delta share its through-seq and must not reorder.
    const items: ConversationItem[] = [];
    insertBySeq(items, preview({ blockId: "first" }), 7);
    insertBySeq(items, preview({ blockId: "second" }), 7);
    expect(items.map((i) => (i as TextItem).blockId)).toEqual(["first", "second"]);
  });

  it("ranks an unranked standing item oldest", () => {
    // A fixture item minted before ranking existed never blocks a ranked
    // insert from landing after it.
    const items: ConversationItem[] = [preview({ blockId: "unranked" })];
    insertBySeq(items, preview({ blockId: "ranked" }), 1);
    expect(items.map((i) => (i as TextItem).blockId)).toEqual(["unranked", "ranked"]);
  });

  it("stamps the rank onto the item", () => {
    const items: ConversationItem[] = [];
    insertBySeq(items, preview(), 42);
    expect(items[0].seq).toBe(42);
  });
});

describe("settle keeps the standing rank", () => {
  it("a record claiming its preview keeps the preview's rank, not its own", () => {
    // A settle replaces content, never position: moving the block would
    // remount its DOM node and re-type prose already on screen.
    const items: ConversationItem[] = [];
    applyStreamDelta(items, delta(), TS, 3);
    settleStreamedBlock(items, finished(), 9);
    expect(items[0].seq).toBe(3);
  });
});

// --- open / grow ------------------------------------------------------------

describe("applyStreamDelta opens a block", () => {
  it("opens a text preview at the feed place its identity names", () => {
    // Arrange
    const items: ConversationItem[] = [];
    // Act
    applyStreamDelta(items, delta({ messageId: "msg_1", blockIndex: 3 }), TS, 0);
    // Assert
    expect((items[0] as TextItem).blockId).toBe("msg_1:3");
  });

  it("opens a thinking preview for a thinking delta", () => {
    // Arrange
    const items: ConversationItem[] = [];
    // Act
    applyStreamDelta(items, thinkingDelta(), TS, 0);
    // Assert
    expect(items[0]!.kind).toBe("thinking");
  });

  it("stamps the opening block with the clock it was handed", () => {
    // Arrange
    const items: ConversationItem[] = [];
    // Act
    applyStreamDelta(items, delta(), TS, 0);
    // Assert
    expect((items[0] as TextItem).ts).toBe(TS);
  });
});

describe("applyStreamDelta grows a block", () => {
  it("appends onto the block already standing at that feed place", () => {
    // Arrange
    const items: ConversationItem[] = [preview({ text: "Hel" })];
    // Act
    applyStreamDelta(items, delta({ delta: "lo" }), TS, 0);
    // Assert
    expect((items[0] as TextItem).text).toBe("Hello");
  });

  it("reopens a settled block a late chunk still arrives for", () => {
    // A chunk that lands after its record must not be dropped on the floor.
    // Arrange
    const items: ConversationItem[] = [finished({ blockId: "msg_1:0", text: "Hel" })];
    // Act
    applyStreamDelta(items, delta({ delta: "lo" }), TS, 0);
    // Assert
    expect((items[0] as TextItem).done).toBe(false);
  });

  it("opens a second block for a second index of one message", () => {
    // Arrange
    const items: ConversationItem[] = [preview()];
    // Act
    applyStreamDelta(items, delta({ blockIndex: 1 }), TS, 0);
    // Assert
    expect(items).toHaveLength(2);
  });
});

// --- tool input -------------------------------------------------------------

describe("applyStreamDelta grows tool input", () => {
  function toolItem(over: Partial<ToolItem> = {}): ToolItem {
    return {
      kind: "tool",
      toolUseId: "tu1",
      toolName: "Bash",
      messageId: "msg_1",
      ts: TS,
      inputJson: "",
      inputDone: false,
      ...over,
    };
  }

  it("grows the exact call whose input is still arriving", () => {
    // Arrange
    const items: ConversationItem[] = [toolItem()];
    // Act
    applyStreamDelta(items, inputDelta({ delta: '{"a":1}' }), TS, 0);
    // Assert
    expect((items[0] as ToolItem).inputJson).toBe('{"a":1}');
  });

  it("leaves unrelated open calls untouched", () => {
    // Arrange
    const items: ConversationItem[] = [
      toolItem({ toolUseId: "tu1", inputJson: "left" }),
      toolItem({ toolUseId: "tu2", inputJson: "right" }),
    ];
    // Act
    applyStreamDelta(items, inputDelta({ toolUseId: "tu1", delta: "-grown" }), TS, 0);
    // Assert
    expect((items[0] as ToolItem).inputJson).toBe("left-grown");
    expect((items[1] as ToolItem).inputJson).toBe("right");
  });

  it("creates an exact-id preview when the ephemeral chunk arrives first", () => {
    // Arrange
    const items: ConversationItem[] = [];
    // Act
    const outcome = applyStreamDelta(items, inputDelta({ toolUseId: "tu3", blockIndex: 4, delta: "{" }), TS, 0);
    // Assert
    expect(outcome).toEqual({ changed: true, toolInput: { toolUseId: "tu3", phase: "absent", branch: "preview-created" } });
    expect(items).toMatchObject([{ kind: "tool", toolUseId: "tu3", inputJson: "{", inputDone: false }]);
  });

  it("classifies a late ephemeral chunk as superseded by an authoritative final", () => {
    const items: ConversationItem[] = [toolItem({ toolUseId: "tu1", inputDone: true })];
    expect(applyStreamDelta(items, inputDelta({ delta: "ignored" }), TS, 0)).toEqual({
      changed: false,
      reason: "superseded-authoritative-tool",
      toolInput: { toolUseId: "tu1", phase: "final", branch: "superseded-authoritative-final" },
    });
    expect((items[0] as ToolItem).inputJson).toBe("");
  });
});

// --- settle -----------------------------------------------------------------

describe("settleStreamedBlock", () => {
  it("settles onto the open preview of the same message and kind", () => {
    // Arrange
    const items: ConversationItem[] = [preview()];
    // Act
    settleStreamedBlock(items, finished(), 0);    // Assert
    expect(items).toHaveLength(1);
  });

  it("keeps the preview's feed place so the reveal cursor survives", () => {
    // Arrange
    const items: ConversationItem[] = [preview({ blockId: "msg_1:5" })];
    // Act
    settleStreamedBlock(items, finished(), 0);    // Assert
    expect((items[0] as TextItem).blockId).toBe("msg_1:5");
  });

  it("adopts the record's text over the half-typed preview's", () => {
    // Arrange
    const items: ConversationItem[] = [preview({ text: "Hel" })];
    // Act
    settleStreamedBlock(items, finished({ text: "Hello" }), 0);    // Assert
    expect((items[0] as TextItem).text).toBe("Hello");
  });

  it("refuses a preview of a different kind", () => {
    // Arrange
    const items: ConversationItem[] = [
      { kind: "thinking", blockId: "msg_1:0", messageId: "msg_1", text: "hmm", done: false },
    ];
    // Act
    settleStreamedBlock(items, finished(), 0);    // Assert
    expect(items).toHaveLength(2);
  });

  it("refuses a preview belonging to a different message", () => {
    // Arrange
    const items: ConversationItem[] = [preview({ messageId: "msg_OTHER" })];
    // Act
    settleStreamedBlock(items, finished(), 0);    // Assert
    expect(items).toHaveLength(2);
  });

  it("refuses a preview owned by a different subagent", () => {
    // Arrange
    const items: ConversationItem[] = [preview({ parentToolUseId: "tu9" })];
    // Act
    settleStreamedBlock(items, finished(), 0);    // Assert
    expect(items).toHaveLength(2);
  });

  it("refuses a preview an earlier record already claimed", () => {
    // Claiming stamps the identity, which is what keeps same-kind blocks of one
    // message paired in block order instead of piling onto the first preview.
    // Arrange
    const items: ConversationItem[] = [finished({ blockId: "msg_1:0", done: false })];
    // Act
    settleStreamedBlock(items, finished({ uuid: "env2:0" }), 0);    // Assert
    expect(items).toHaveLength(2);
  });

  it("claims the EARLIEST unclaimed preview", () => {
    // Arrange
    const items: ConversationItem[] = [
      preview({ blockId: "msg_1:0", text: "a" }),
      preview({ blockId: "msg_1:1", text: "b" }),
    ];
    // Act
    settleStreamedBlock(items, finished({ text: "first" }), 0);    // Assert
    expect((items[0] as TextItem).text).toBe("first");
  });

  it("replaces the block a redelivered record already produced", () => {
    // Arrange
    const items: ConversationItem[] = [finished({ blockId: "msg_1:0" })];
    // Act
    settleStreamedBlock(items, finished(), 0);    // Assert
    expect(items).toHaveLength(1);
  });

  it("appends a record no preview ever opened", () => {
    // Arrange
    const items: ConversationItem[] = [];
    // Act
    settleStreamedBlock(items, finished(), 0);    // Assert
    expect(items).toHaveLength(1);
  });
});

// --- settle onto a NAMED preview ---------------------------------------------

// `AgentThinking` states the message its block was stripped from and the index
// it held there, so a settled reasoning block names its preview outright. These
// pin that the name is honoured and that it is honoured EXACTLY.

function thinkingPreview(over: Partial<ThinkingItem> = {}): ThinkingItem {
  return { kind: "thinking", blockId: "msg_1:0", messageId: "msg_1", text: "we", done: false, ...over };
}

function settledThinking(over: Partial<ThinkingItem> = {}): ThinkingItem {
  return {
    kind: "thinking",
    blockId: "env1#thinking:0:1",
    uuid: "env1#thinking:0:1",
    messageId: "msg_1",
    text: "weighing it",
    done: true,
    ...over,
  };
}

describe("settleStreamedBlock onto a preview the record names", () => {
  it("settles onto the exact preview the record named", () => {
    // Arrange
    const items: ConversationItem[] = [thinkingPreview({ blockId: "msg_1:1" })];
    // Act
    settleStreamedBlock(items, settledThinking({ previewBlockId: "msg_1:1" }), 0);
    // Assert
    expect(items).toHaveLength(1);
  });

  it("keeps the named preview's feed place", () => {
    // Arrange
    const items: ConversationItem[] = [thinkingPreview({ blockId: "msg_1:1" })];
    // Act
    settleStreamedBlock(items, settledThinking({ previewBlockId: "msg_1:1" }), 0);
    // Assert
    expect((items[0] as ThinkingItem).blockId).toBe("msg_1:1");
  });

  it("passes over an earlier preview the record did not name", () => {
    // The un-named path takes the EARLIEST same-message preview. A named record
    // must take the one it named, or two reasoning blocks of one message settle
    // onto the wrong halves of each other.
    // Arrange
    const items: ConversationItem[] = [
      thinkingPreview({ blockId: "msg_1:0", text: "first" }),
      thinkingPreview({ blockId: "msg_1:1", text: "second" }),
    ];
    // Act
    settleStreamedBlock(items, settledThinking({ previewBlockId: "msg_1:1" }), 0);
    // Assert
    expect((items[0] as ThinkingItem).text).toBe("first");
  });

  it("appends rather than guessing when the named preview is absent", () => {
    // Arrange
    const items: ConversationItem[] = [thinkingPreview({ blockId: "msg_1:0" })];
    // Act
    settleStreamedBlock(items, settledThinking({ previewBlockId: "msg_1:7" }), 0);
    // Assert
    expect(items).toHaveLength(2);
  });

  it("refuses a named preview an earlier record already claimed", () => {
    // Arrange
    const items: ConversationItem[] = [settledThinking({ blockId: "msg_1:1", done: false })];
    // Act
    settleStreamedBlock(items, settledThinking({ uuid: "env2:0", previewBlockId: "msg_1:1" }), 0);
    // Assert
    expect(items).toHaveLength(2);
  });
});
