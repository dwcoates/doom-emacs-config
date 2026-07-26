import { describe, expect, it } from "vitest";

import {
  applyStreamDelta,
  blockKey,
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

function delta(over: Partial<StreamDelta> = {}): StreamDelta {
  return { kind: "text", messageId: "msg_1", blockIndex: 0, delta: "hi", ...over };
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

// --- open / grow ------------------------------------------------------------

describe("applyStreamDelta opens a block", () => {
  it("opens a text preview at the feed place its identity names", () => {
    // Arrange
    const items: ConversationItem[] = [];
    // Act
    applyStreamDelta(items, delta({ messageId: "msg_1", blockIndex: 3 }), TS);
    // Assert
    expect((items[0] as TextItem).blockId).toBe("msg_1:3");
  });

  it("opens a thinking preview for a thinking delta", () => {
    // Arrange
    const items: ConversationItem[] = [];
    // Act
    applyStreamDelta(items, delta({ kind: "thinking" }), TS);
    // Assert
    expect(items[0]!.kind).toBe("thinking");
  });

  it("stamps the opening block with the clock it was handed", () => {
    // Arrange
    const items: ConversationItem[] = [];
    // Act
    applyStreamDelta(items, delta(), TS);
    // Assert
    expect((items[0] as TextItem).ts).toBe(TS);
  });
});

describe("applyStreamDelta grows a block", () => {
  it("appends onto the block already standing at that feed place", () => {
    // Arrange
    const items: ConversationItem[] = [preview({ text: "Hel" })];
    // Act
    applyStreamDelta(items, delta({ delta: "lo" }), TS);
    // Assert
    expect((items[0] as TextItem).text).toBe("Hello");
  });

  it("reopens a settled block a late chunk still arrives for", () => {
    // A chunk that lands after its record must not be dropped on the floor.
    // Arrange
    const items: ConversationItem[] = [finished({ blockId: "msg_1:0", text: "Hel" })];
    // Act
    applyStreamDelta(items, delta({ delta: "lo" }), TS);
    // Assert
    expect((items[0] as TextItem).done).toBe(false);
  });

  it("opens a second block for a second index of one message", () => {
    // Arrange
    const items: ConversationItem[] = [preview()];
    // Act
    applyStreamDelta(items, delta({ blockIndex: 1 }), TS);
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

  it("grows the call whose input is still arriving", () => {
    // Arrange
    const items: ConversationItem[] = [toolItem()];
    // Act
    applyStreamDelta(items, delta({ kind: "input_json", delta: '{"a":1}' }), TS);
    // Assert
    expect((items[0] as ToolItem).inputJson).toBe('{"a":1}');
  });

  it("grows the LATEST open call when an earlier one already closed", () => {
    // Arrange
    const items: ConversationItem[] = [
      toolItem({ toolUseId: "tu1", inputDone: true }),
      toolItem({ toolUseId: "tu2" }),
    ];
    // Act
    applyStreamDelta(items, delta({ kind: "input_json", delta: "x" }), TS);
    // Assert
    expect((items[1] as ToolItem).inputJson).toBe("x");
  });

  it("reports a tool-input delta with no open call rather than dropping it", () => {
    // The relay carries no tool_use_id, so an unattachable delta is a real gap
    // the caller must be able to surface — never a silent no-op.
    // Arrange
    const items: ConversationItem[] = [];
    // Act
    const outcome = applyStreamDelta(items, delta({ kind: "input_json", blockIndex: 4 }), TS);
    // Assert
    expect(outcome).toEqual({ changed: false, reason: "no-open-tool", blockId: "msg_1:4" });
  });
});

// --- settle -----------------------------------------------------------------

describe("settleStreamedBlock", () => {
  it("settles onto the open preview of the same message and kind", () => {
    // Arrange
    const items: ConversationItem[] = [preview()];
    // Act
    settleStreamedBlock(items, finished());
    // Assert
    expect(items).toHaveLength(1);
  });

  it("keeps the preview's feed place so the reveal cursor survives", () => {
    // Arrange
    const items: ConversationItem[] = [preview({ blockId: "msg_1:5" })];
    // Act
    settleStreamedBlock(items, finished());
    // Assert
    expect((items[0] as TextItem).blockId).toBe("msg_1:5");
  });

  it("adopts the record's text over the half-typed preview's", () => {
    // Arrange
    const items: ConversationItem[] = [preview({ text: "Hel" })];
    // Act
    settleStreamedBlock(items, finished({ text: "Hello" }));
    // Assert
    expect((items[0] as TextItem).text).toBe("Hello");
  });

  it("refuses a preview of a different kind", () => {
    // Arrange
    const items: ConversationItem[] = [
      { kind: "thinking", blockId: "msg_1:0", messageId: "msg_1", text: "hmm", done: false },
    ];
    // Act
    settleStreamedBlock(items, finished());
    // Assert
    expect(items).toHaveLength(2);
  });

  it("refuses a preview belonging to a different message", () => {
    // Arrange
    const items: ConversationItem[] = [preview({ messageId: "msg_OTHER" })];
    // Act
    settleStreamedBlock(items, finished());
    // Assert
    expect(items).toHaveLength(2);
  });

  it("refuses a preview owned by a different subagent", () => {
    // Arrange
    const items: ConversationItem[] = [preview({ parentToolUseId: "tu9" })];
    // Act
    settleStreamedBlock(items, finished());
    // Assert
    expect(items).toHaveLength(2);
  });

  it("refuses a preview an earlier record already claimed", () => {
    // Claiming stamps the identity, which is what keeps same-kind blocks of one
    // message paired in block order instead of piling onto the first preview.
    // Arrange
    const items: ConversationItem[] = [finished({ blockId: "msg_1:0", done: false })];
    // Act
    settleStreamedBlock(items, finished({ uuid: "env2:0" }));
    // Assert
    expect(items).toHaveLength(2);
  });

  it("claims the EARLIEST unclaimed preview", () => {
    // Arrange
    const items: ConversationItem[] = [
      preview({ blockId: "msg_1:0", text: "a" }),
      preview({ blockId: "msg_1:1", text: "b" }),
    ];
    // Act
    settleStreamedBlock(items, finished({ text: "first" }));
    // Assert
    expect((items[0] as TextItem).text).toBe("first");
  });

  it("replaces the block a redelivered record already produced", () => {
    // Arrange
    const items: ConversationItem[] = [finished({ blockId: "msg_1:0" })];
    // Act
    settleStreamedBlock(items, finished());
    // Assert
    expect(items).toHaveLength(1);
  });

  it("appends a record no preview ever opened", () => {
    // Arrange
    const items: ConversationItem[] = [];
    // Act
    settleStreamedBlock(items, finished());
    // Assert
    expect(items).toHaveLength(1);
  });
});
