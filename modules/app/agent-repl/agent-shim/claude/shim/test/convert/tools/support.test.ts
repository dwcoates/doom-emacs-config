/**
 * The shared readers every per-tool converter leans on. Each one has a narrow
 * refusal — a negative "unsigned", a block that is not text, a hunk with no
 * range, a patch that is not a list — and each refusal is the difference
 * between an honest absence and an invented figure, so each is asserted alone.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import { diffHunks, hunksOf, resultText, uint } from "../../../src/convert/tools/support.js";

/** A tool result carrying the given blocks, in order. */
function content(
  blocks: conversationv1.ToolResultContentBlock[],
): conversationv1.ToolResultContent {
  return create(conversationv1.ToolResultContentSchema, { blocks });
}

/** A text block. */
function textBlock(text: string): conversationv1.ToolResultContentBlock {
  return create(conversationv1.ToolResultContentBlockSchema, {
    block: { case: "text", value: create(conversationv1.TextBlockSchema, { text }) },
  });
}

/** A block whose kind this contract does not model. */
function unsupported(): conversationv1.ToolResultContentBlock {
  return create(conversationv1.ToolResultContentBlockSchema, {
    block: {
      case: "unsupported",
      value: create(conversationv1.UnsupportedBlockSchema, { kind: "video" }),
    },
  });
}

describe("uint", () => {
  it("refuses a NEGATIVE number rather than handing back a bogus unsigned", () => {
    // Arrange, Act, Assert.
    expect(uint({ n: -3 }, "n")).toBeUndefined();
  });

  it("truncates a fractional number toward zero", () => {
    // Arrange, Act, Assert.
    expect(uint({ n: 4.9 }, "n")).toBe(4);
  });
});

describe("resultText", () => {
  it("contributes an EMPTY line for a block that is not text, keeping the order", () => {
    // Arrange.
    const blocks = content([textBlock("before"), unsupported(), textBlock("after")]);

    // Act, Assert.
    expect(resultText(blocks)).toBe("before\n\nafter");
  });
});

describe("hunksOf", () => {
  it("keeps a hunk the vendor stated whole", () => {
    // Arrange.
    const patch = [{ oldStart: 3, oldLines: 1, newStart: 3, newLines: 2, lines: ["-a", "+b", "+c"] }];

    // Act.
    const hunks = hunksOf(patch);

    // Assert.
    expect(hunks[0]?.oldRange).toEqual(
      create(conversationv1.FilePatchHunkRangeSchema, { start: 3, lines: 1 }),
    );
  });

  it("drops an entry that is not an object at all", () => {
    // Arrange, Act, Assert.
    expect(hunksOf(["not a hunk"])).toEqual([]);
  });

  it("answers NO hunks when the patch is not a list", () => {
    // Arrange, Act, Assert.
    expect(hunksOf({ oldStart: 1 })).toEqual([]);
  });
});

describe("diffHunks", () => {
  it("answers NO hunks when the two versions are identical", () => {
    // Arrange, Act, Assert.
    expect(diffHunks("same\ntext", "same\ntext")).toEqual([]);
  });

  it("states a whole-file DELETION as removals with no additions", () => {
    // Arrange, Act.
    const hunks = diffHunks("gone\naway", "");

    // Assert.
    expect(hunks[0]?.lines).toEqual(["-gone", "-away"]);
  });

  it("states a creation from nothing as additions with no removals", () => {
    // Arrange, Act.
    const hunks = diffHunks("", "new\nfile");

    // Assert.
    expect(hunks[0]?.lines).toEqual(["+new", "+file"]);
  });
});
