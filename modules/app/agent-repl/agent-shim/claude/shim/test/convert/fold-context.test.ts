/**
 * The RECORD PLANE'S accessors over the one fold context.
 *
 * Both lookups here are OPTIONAL on the engine's declaration — they were added
 * additively — so what is pinned is what an ABSENT one means. A `?.` at each
 * call site would let two sites disagree about that; these two functions are
 * the one place it is stated.
 */
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../../src/proto.js";
import { mcpServerNames, subagentBook } from "../../src/convert/fold-context.js";
import type { FoldContext } from "../../src/convert/fold-context.js";
import { foldContext, MAIN_AGENT } from "./fold-harness.js";

/** The context an ENGINE THAT PREDATES these fields hands the fold. */
function contextWithout(field: "mcpServerNames" | "subagentFor"): FoldContext {
  const context = { ...foldContext() } as Record<string, unknown>;
  delete context[field];
  return context as unknown as FoldContext;
}

describe("the MCP server names the session knows", () => {
  it("are the names the engine states", () => {
    expect(mcpServerNames(foldContext({ mcpServerNames: ["Slack", "Gmail"] }))).toEqual([
      "Slack",
      "Gmail",
    ]);
  });

  it("are NONE when the engine states none", () => {
    expect(mcpServerNames(foldContext())).toEqual([]);
  });

  it("are NONE when the engine declares no such lookup at all", () => {
    // The field is optional on the engine's declaration; an engine that never
    // learned it must read as "no servers", not as a crash.
    expect(mcpServerNames(contextWithout("mcpServerNames"))).toEqual([]);
  });
});

describe("a subagent's book", () => {
  it("is the id the engine learned, whenever it has one", () => {
    const real = create(conversationv1.AgentIdSchema, { value: "agent-real" });

    expect(
      subagentBook(foldContext({ subagentFor: () => real }), "toolu_spawn").value,
    ).toBe("agent-real");
  });

  it("falls back to the SPAWNING CALL's own id, the only identity the stream carries", () => {
    // The pinned SDK stream states no agent id anywhere: a subagent's messages
    // name `parent_tool_use_id` and nothing else.
    expect(subagentBook(foldContext(), "toolu_spawn").value).toContain("toolu_spawn");
  });

  it("falls back the same way when the engine declares no such lookup at all", () => {
    expect(subagentBook(contextWithout("subagentFor"), "toolu_spawn").value).toContain(
      "toolu_spawn",
    );
  });

  it("is NOT the main agent's book, which is a different book entirely", () => {
    expect(subagentBook(foldContext(), "toolu_spawn").value).not.toBe(MAIN_AGENT.value);
  });
});
