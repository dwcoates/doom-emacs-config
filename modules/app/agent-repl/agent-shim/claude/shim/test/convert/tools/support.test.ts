/**
 * The shared readers every per-tool converter leans on. Each one has a narrow
 * refusal — a negative "unsigned", a block that is not text, a hunk with no
 * range, a patch that is not a list — and each refusal is the difference
 * between an honest absence and an invented figure, so each is asserted alone.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import {
  diffHunks,
  failureOf,
  hunksOf,
  resultText,
  returnedContent,
  settle,
  uint,
  untypedArguments,
} from "../../../src/convert/tools/support.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";

/** A call announced at a known instant. */
function call(startedAtMs: number): PendingCall {
  return {
    toolUseId: "toolu_s",
    toolName: "Read",
    input: {},
    startedAtMs,
    agentId: create(conversationv1.AgentIdSchema, { value: "main" }),
  };
}

/** A result settled at a known instant. */
function outcome(settledAtMs: number): ToolOutcome {
  return { content: undefined, isError: true, structured: undefined, settledAtMs };
}

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

  it("does not count a file's TERMINATING newline as a line", () => {
    // Arrange: every real file ends with one, and the vendor hands `content`
    // verbatim -- so a bare split used to draw a one-line creation as two
    // additions, the second blank, under a "+1,2" header.
    // Act.
    const hunks = diffHunks("", "export const fresh = true;\n");

    // Assert.
    expect(hunks[0]?.lines).toEqual(["+export const fresh = true;"]);
    expect(hunks[0]?.newRange?.lines).toBe(1);
  });

  it("KEEPS a final blank line that is real content, not the terminator", () => {
    // Arrange: "one\n\n" genuinely ends with a blank line, and only the
    // terminator after it is dropped.
    // Act.
    const hunks = diffHunks("", "one\n\n");

    // Assert.
    expect(hunks[0]?.lines).toEqual(["+one", "+"]);
  });
});

describe("settle", () => {
  it("restates the call's own start beside the settle instant", () => {
    // Arrange
    const announced = call(1_000);

    // Act
    const instant = settle(announced, outcome(4_000));

    // Assert
    expect(instant.startedAt?.atMs).toBe(1_000n);
  });
});

describe("failureOf", () => {
  it("restates the failed call's own start beside the settle instant", () => {
    // Arrange
    const announced = call(2_000);

    // Act
    const failure = failureOf(announced, outcome(5_000));

    // Assert
    expect(failure.settledAt?.startedAt?.atMs).toBe(2_000n);
  });
});

describe("untypedArguments", () => {
  it("carries a JSON-representable input as its JSON object", () => {
    // Arrange
    const announced = { ...call(0), input: { tabId: 7, nested: { a: [1, "b"] } } };

    // Act, Assert
    expect(untypedArguments(announced)).toEqual({ tabId: 7, nested: { a: [1, "b"] } });
  });

  it("answers undefined for an input JSON cannot represent", () => {
    // Arrange
    const cyclic: Record<string, unknown> = {};
    cyclic["self"] = cyclic;

    // Act, Assert
    expect(untypedArguments({ ...call(0), input: cyclic })).toBeUndefined();
  });
});

describe("returnedContent", () => {
  it("carries what the tool returned, and says it returned", () => {
    // Arrange
    const returned = content([]);

    // Act
    const got = returnedContent({ ...outcome(0), content: returned });

    // Assert
    expect(got).toEqual({ content: returned, returned: true });
  });

  it("answers an EMPTY content when the vendor returned nothing, and says so", () => {
    // Arrange, Act
    const got = returnedContent(outcome(0));

    // Assert
    expect(got).toEqual({ content: create(conversationv1.ToolResultContentSchema, {}), returned: false });
  });
});
