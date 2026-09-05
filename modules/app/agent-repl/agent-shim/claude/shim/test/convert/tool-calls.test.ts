/**
 * A TOOL CALL BECOMES A UNIT, and its RETURN settles it.
 *
 * The three name sets are the load-bearing part: an exempt tool is a DECISION
 * and is dropped silently, an engine-owned tool belongs to the gate, and
 * `AgentUnmodeled` is for a tool whose schema genuinely cannot be known. A
 * recognizable built-in in that last set is a producer defect, so the suite
 * forbids it by name.
 */
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../../src/proto.js";
import {
  CALL_REGISTRY_CAPACITY,
  convertProgressBeat,
  convertToolResult,
  convertToolUse,
  ENGINE_OWNED_TOOLS,
  EXEMPT_TOOLS,
  UNMODELED_KEY,
  createCallRegistry,
  dispositionOf,
  environmentOf,
  type PendingCall,
  type ToolConverter,
  type ToolOutcome,
} from "../../src/convert/tool-calls.js";
import { TOOL_CONVERTERS } from "../../src/convert/tools/registry.js";
import { foldContext, MAIN_AGENT } from "./fold-harness.js";

/** One call in flight. */
function call(toolUseId: string, toolName = "Read"): PendingCall {
  return {
    toolUseId,
    toolName,
    input: { file_path: "/tmp/a" },
    startedAtMs: 5,
    agentId: MAIN_AGENT,
  };
}

/** One settled outcome. */
function outcome(): ToolOutcome {
  return { content: undefined, isError: false, structured: undefined, settledAtMs: 9 };
}

describe("the registry of calls in flight", () => {
  it("remembers a call so its terminal can restate the call's own facts", () => {
    const registry = createCallRegistry();
    registry.remember(call("toolu_1"));

    expect(registry.peek("toolu_1")?.toolName).toBe("Read");
  });

  it("forgets a call when it settles, so nothing accumulates per turn", () => {
    const registry = createCallRegistry();
    registry.remember(call("toolu_1"));

    registry.take("toolu_1");

    expect(registry.peek("toolu_1")).toBeUndefined();
  });

  it("answers nothing for a call this shim never saw announced", () => {
    expect(createCallRegistry().take("toolu_nope")).toBeUndefined();
  });

  it("stays BOUNDED: a vendor that never returns a result cannot grow it forever", () => {
    const registry = createCallRegistry();
    for (let index = 0; index <= CALL_REGISTRY_CAPACITY; index += 1) {
      registry.remember(call(`toolu_${index}`));
    }

    // The oldest is forgotten rather than the table growing without bound.
    expect(registry.peek("toolu_0")).toBeUndefined();
    expect(registry.peek(`toolu_${CALL_REGISTRY_CAPACITY}`)).toBeDefined();
  });

  it("peeks without settling, which is what a progress beat needs", () => {
    const registry = createCallRegistry();
    registry.remember(call("toolu_1"));

    registry.peek("toolu_1");

    expect(registry.peek("toolu_1")).toBeDefined();
  });
});

describe("disposition", () => {
  it("is MODELLED for a tool a converter owns", () => {
    expect(dispositionOf(TOOL_CONVERTERS, "Read").case).toBe("modelled");
  });

  it("is EXEMPT for a built-in the contract deliberately does not carry", () => {
    expect(dispositionOf(TOOL_CONVERTERS, "TaskList").case).toBe("exempt");
  });

  it("is ENGINE-OWNED for the ask the permission gate holds", () => {
    expect(dispositionOf(TOOL_CONVERTERS, "AskUserQuestion").case).toBe("engine_owned");
  });

  it("is UNMODELED for a runtime-registered MCP tool", () => {
    expect(dispositionOf(TOOL_CONVERTERS, "mcp__Slack__send").case).toBe("unmodeled");
  });

  it("never routes a recognizable built-in to unmodeled — that would be a defect", () => {
    const builtIns = [
      "Read",
      "Write",
      "Edit",
      "Grep",
      "Glob",
      "Bash",
      "Agent",
      "Task",
      "Skill",
      "SendMessage",
      "TaskCreate",
      "TaskUpdate",
      "WebFetch",
      "WebSearch",
      "Monitor",
      "ScheduleWakeup",
      "Artifact",
      "EnterPlanMode",
      "ExitPlanMode",
      "ReportFindings",
      "EnterWorktree",
      "ExitWorktree",
      "CronCreate",
      "CronDelete",
      "CronList",
      "PushNotification",
      ...EXEMPT_TOOLS,
      ...ENGINE_OWNED_TOOLS,
    ];

    const defects = builtIns.filter(
      (name) => dispositionOf(TOOL_CONVERTERS, name).case === "unmodeled",
    );
    expect(defects).toEqual([]);
  });
});

describe("the registry table", () => {
  it("files the unmodeled converter under a key no vendor tool can be named", () => {
    // THE INVARIANT: a genuine vendor tool literally called "unmodeled" must
    // not silently take the fallback's place, so the key carries a character no
    // tool name can.
    expect(TOOL_CONVERTERS.get(UNMODELED_KEY)?.kind).toBe("unmodeled");
    expect(TOOL_CONVERTERS.has("unmodeled")).toBe(false);
    expect(dispositionOf(TOOL_CONVERTERS, "unmodeled").case).toBe("unmodeled");
  });

  it("spells one spawn two ways, because the vendor does", () => {
    expect(TOOL_CONVERTERS.get("Agent")).toBe(TOOL_CONVERTERS.get("Task"));
  });

  it("gives the plan-mode pair one converter, distinguished by the call's name", () => {
    expect(TOOL_CONVERTERS.get("EnterPlanMode")).toBe(TOOL_CONVERTERS.get("ExitPlanMode"));
  });

  it("declares a progress arm only where the proto does", () => {
    // A monitor is armed and then ends; it has no progress arm at all.
    expect(TOOL_CONVERTERS.get("Read")?.carriesProgress).toBe(true);
    expect(TOOL_CONVERTERS.get("Monitor")?.carriesProgress).toBe(false);
  });
});

describe("the tool environment", () => {
  it("carries the MCP server names the session knows, and nothing else", () => {
    expect(environmentOf(foldContext({ mcpServerNames: ["Slack"] }))).toEqual({
      mcpServerNames: ["Slack"],
    });
  });

  it("is empty when the engine states no servers", () => {
    expect(environmentOf(foldContext()).mcpServerNames).toEqual([]);
  });
});

describe("a converter's own contract", () => {
  it("answers a start arm for the kind it owns", () => {
    const item = TOOL_CONVERTERS.get("Read")?.start(call("toolu_1"));

    expect(item?.case).toBe("read");
  });

  it("answers UNDEFINED from settle when this result does not conclude the unit", () => {
    // A skill settles on its DOCUMENT, not on the acknowledgement.
    const item = TOOL_CONVERTERS.get("Skill")?.settle(call("toolu_s", "Skill"), {
      ...outcome(),
      structured: { success: true, commandName: "debug-logs" },
    });

    expect(item).toBeUndefined();
  });

  it("re-remembers a DECLINED call with what only that result carried", () => {
    // Arrange: a skill's acknowledgement declares the allowances and settles nothing.
    const registry = createCallRegistry();
    registry.remember(call("toolu_s", "Skill"));

    // Act.
    convertToolResult(
      TOOL_CONVERTERS,
      foldContext(),
      registry,
      "toolu_s",
      { ...outcome(), structured: { success: true, allowedTools: ["Bash(run.sh:*)"] } },
      { vendorUuid: "u_1" },
    );

    // Assert.
    expect(registry.peek("toolu_s")?.retainedAllowedTools).toEqual(["Bash(run.sh:*)"]);
  });

  it("answers a progress arm on a kind that declares one", () => {
    const beat = create(conversationv1.AgentToolCallProgressSchema, { lastProgressAtMs: 3n });

    expect(TOOL_CONVERTERS.get("Read")?.progress?.(beat)?.case).toBe("read");
  });
});

describe("a start with no announcement frame", () => {
  /** The call as `convertToolUse` is handed one. */
  function taskCreate(): PendingCall {
    return {
      toolUseId: "toolu_tc",
      toolName: "TaskCreate",
      input: { subject: "s", description: "d" },
      startedAtMs: 5,
      agentId: MAIN_AGENT,
    };
  }

  it("writes NO entry for a TaskCreate, whose identity does not exist until it returns", () => {
    // Arrange.
    const registry = createCallRegistry();

    // Act.
    const entries = convertToolUse(TOOL_CONVERTERS, foldContext(), registry, taskCreate(), {
      agentId: MAIN_AGENT,
      vendorUuid: "uuid-1",
    });

    // Assert. An entry here sets no item arm, which the store refuses.
    expect(entries).toEqual([]);
  });

  it("still REMEMBERS the call, so its terminal can restate what was asked", () => {
    // Arrange.
    const registry = createCallRegistry();

    // Act.
    convertToolUse(TOOL_CONVERTERS, foldContext(), registry, taskCreate(), {
      agentId: MAIN_AGENT,
      vendorUuid: "uuid-1",
    });

    // Assert.
    expect(registry.peek("toolu_tc")?.toolName).toBe("TaskCreate");
  });

  it("writes the start entry for a call that DOES announce one", () => {
    // Arrange.
    const registry = createCallRegistry();

    // Act.
    const entries = convertToolUse(TOOL_CONVERTERS, foldContext(), registry, call("toolu_r"), {
      agentId: MAIN_AGENT,
      vendorUuid: "uuid-2",
    });

    // Assert.
    expect(entries).toHaveLength(1);
  });
});

/**
 * THE UNMODELED FALLBACK IS NOT OPTIONAL. Every tool this contract does not
 * model routes through the one converter filed under {@link UNMODELED_KEY}, so
 * a registry missing it cannot silently drop a call — it FAILS, loudly, where
 * the wiring is wrong rather than where the row is missing.
 */
describe("a registry with no unmodeled converter", () => {
  it("refuses a call it cannot model rather than dropping it", () => {
    // Arrange
    const registry = createCallRegistry();
    const empty = new Map<string, ToolConverter>();

    // Act + Assert
    expect(() =>
      convertToolUse(empty, foldContext(), registry, call("toolu_x", "mcp__Slack__send"), {
        agentId: MAIN_AGENT,
        vendorUuid: "uuid-1",
      }),
    ).toThrow(/unmodeled converter is missing/);
  });

  it("refuses the RESULT of a call it cannot model too", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember(call("toolu_x", "mcp__Slack__send"));
    const empty = new Map<string, ToolConverter>();

    // Act + Assert
    expect(() =>
      convertToolResult(empty, foldContext(), registry, "toolu_x", outcome(), {
        vendorUuid: "uuid-1",
      }),
    ).toThrow(/unmodeled converter is missing/);
  });
});

describe("an unmodeled converter that announces nothing", () => {
  it("refuses the call, since an unmodeled unit ALWAYS has a start arm", () => {
    // Arrange: a stub that declines to announce, which the contract forbids here.
    const silent: ToolConverter = {
      kind: "unmodeled",
      carriesProgress: false,
      start: () => undefined,
      settle: () => undefined,
    };
    const registry = createCallRegistry();

    // Act + Assert
    expect(() =>
      convertToolUse(
        new Map([[UNMODELED_KEY, silent]]),
        foldContext(),
        registry,
        call("toolu_x", "mcp__Slack__send"),
        { agentId: MAIN_AGENT, vendorUuid: "uuid-1" },
      ),
    ).toThrow(/produced no start frame/);
  });
});

describe("a progress beat", () => {
  it("is consumed for a tool the fold does not own at all", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember(call("toolu_e", "TaskList"));

    // Act
    const entries = convertProgressBeat(TOOL_CONVERTERS, foldContext(), registry, "toolu_e", 11, {
      vendorUuid: "uuid-1",
    });

    // Assert
    expect(entries).toEqual([]);
  });

  it("is consumed for a unit kind that declares no progress arm", () => {
    // Arrange: a monitor is armed and then ends; the proto gives it no progress.
    const registry = createCallRegistry();
    registry.remember(call("toolu_m", "Monitor"));

    // Act
    const entries = convertProgressBeat(TOOL_CONVERTERS, foldContext(), registry, "toolu_m", 11, {
      vendorUuid: "uuid-1",
    });

    // Assert
    expect(entries).toEqual([]);
  });

  it("is consumed for a call this shim never saw announced", () => {
    // Arrange + Act
    const entries = convertProgressBeat(
      TOOL_CONVERTERS,
      foldContext(),
      createCallRegistry(),
      "toolu_never",
      11,
      { vendorUuid: "uuid-1" },
    );

    // Assert
    expect(entries).toEqual([]);
  });

  it("relays the beat on a kind that DOES declare the arm", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember(call("toolu_r"));

    // Act
    const entries = convertProgressBeat(TOOL_CONVERTERS, foldContext(), registry, "toolu_r", 11, {
      vendorUuid: "uuid-1",
    });

    // Assert
    expect(entries[0]?.source.discriminator).toBe("activity.read.progress");
  });
});
