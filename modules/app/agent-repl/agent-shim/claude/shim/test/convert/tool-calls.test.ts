/**
 * A TOOL CALL BECOMES A UNIT, and its RETURN settles it.
 *
 * The three name sets are the load-bearing part: an exempt tool is a DECISION
 * and is dropped silently, an engine-owned tool belongs to the gate, and
 * `AgentUnmodeled` is for a tool whose schema genuinely cannot be known. A
 * recognizable built-in in that last set is a producer defect, so the suite
 * forbids it by name.
 */
import { writeSync } from "node:fs";
import { describe, expect, it, vi } from "vitest";
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
  endQueryCalls,
  endTurnCalls,
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

/** One call on a subagent's stream: the spawning call its message named. */
function onStream(toolUseId: string, spawningCall: string): PendingCall {
  return { ...call(toolUseId), spawningCall };
}

interface LogRecord {
  readonly level: string;
  readonly message: string;
  readonly context: Record<string, unknown>;
}

/** Every log record `act` wrote, parsed back out of the mocked sink. */
function recordsDuring(act: () => unknown): LogRecord[] {
  const mockedWriteSync = vi.mocked(writeSync);
  const before = mockedWriteSync.mock.calls.length;
  act();
  const calls = mockedWriteSync.mock.calls.slice(before) as unknown as Array<[number, Buffer, number, number]>;
  return calls.map(([, bytes, offset, length]) =>
    JSON.parse(bytes.subarray(offset, offset + length).toString("utf8")) as LogRecord,
  );
}

const REGISTRY_FULL =
  "invariant violated: the in-flight call registry is full; the oldest call is forgotten and its unit can no longer settle on this plane";
const UNHELD_STREAM_RESULT =
  "a tool result on a stream this plane does not hold (a backgrounded agent's); the file plane settles its unit";
const UNANNOUNCED_RESULT =
  "a tool result arrived for a call this shim never saw announced; no terminal is produced";
const STREAM_MISMATCH =
  "invariant violated: a tool result's stream is not its call's; settling by the vendor's call id";
const TURN_RELEASED =
  "the turn ended with calls held that no record on this stream will settle; they are released";
const QUERY_RELEASED = "the query ended with calls held; they are released and nothing is written for them";

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
    // Arrange
    const registry = createCallRegistry();

    // Act
    for (let index = 0; index <= CALL_REGISTRY_CAPACITY; index += 1) {
      registry.remember(call(`toolu_${index}`));
    }

    // Assert: the oldest is forgotten rather than the table growing without bound.
    expect(registry.peek("toolu_0")).toBeUndefined();
    expect(registry.peek(`toolu_${CALL_REGISTRY_CAPACITY}`)).toBeDefined();
  });

  it("raises reaching the bound as an ERROR-level invariant violation", () => {
    // Arrange: the table drains at every turn's end, so reaching the bound
    // means a path held calls and never let them go — and the eviction loses
    // that card's settle, which is never a quiet warning.
    const registry = createCallRegistry();
    for (let index = 0; index < CALL_REGISTRY_CAPACITY; index += 1) {
      registry.remember(call(`toolu_${index}`));
    }

    // Act
    const records = recordsDuring(() => registry.remember(call("toolu_overflow")));

    // Assert
    expect(records.filter((record) => record.message === REGISTRY_FULL)).toMatchObject([
      {
        level: "error",
        context: {
          tool_use_id: "toolu_0",
          tool: "Read",
          capacity: CALL_REGISTRY_CAPACITY,
          held: CALL_REGISTRY_CAPACITY,
        },
      },
    ]);
  });

  it("writes no warning at all when the bound is reached", () => {
    // Arrange
    const registry = createCallRegistry();
    for (let index = 0; index < CALL_REGISTRY_CAPACITY; index += 1) {
      registry.remember(call(`toolu_${index}`));
    }

    // Act
    const records = recordsDuring(() => registry.remember(call("toolu_overflow")));

    // Assert
    expect(records.filter((record) => record.level === "warn")).toEqual([]);
  });

  it("does not evict when a call already held is held again", () => {
    // Arrange: a re-remembered call replaces itself; it does not grow the table.
    const registry = createCallRegistry();
    for (let index = 0; index < CALL_REGISTRY_CAPACITY; index += 1) {
      registry.remember(call(`toolu_${index}`));
    }

    // Act
    registry.remember(call("toolu_5"));

    // Assert
    expect(registry.peek("toolu_0")).toBeDefined();
  });

  it("peeks without settling, which is what a progress beat needs", () => {
    const registry = createCallRegistry();
    registry.remember(call("toolu_1"));

    registry.peek("toolu_1");

    expect(registry.peek("toolu_1")).toBeDefined();
  });

  it("answers every call still in flight, which is what a STOP needs to know", () => {
    // Arrange: the set matters only at the turn's end, and nothing else can ask
    // which calls a stop cut.
    const registry = createCallRegistry();
    registry.remember(call("toolu_1"));
    registry.remember(call("toolu_2"));

    // Act, Assert.
    expect(registry.open().map((pending) => pending.toolUseId)).toEqual(["toolu_1", "toolu_2"]);
  });
});

describe("the streams the registry holds calls for", () => {
  it("holds a call on the main agent's own stream", () => {
    // Arrange
    const registry = createCallRegistry();

    // Act
    const held = registry.remember(call("toolu_1"));

    // Assert
    expect(held).toBe(true);
  });

  it("holds a call on a subagent's stream while its spawning call is held", () => {
    // Arrange: an awaited spawn's own calls return on this stream.
    const registry = createCallRegistry();
    registry.remember(call("toolu_spawn", "Agent"));

    // Act
    const held = registry.remember(onStream("toolu_sub", "toolu_spawn"));

    // Assert
    expect(held).toBe(true);
  });

  it("REFUSES a call on a stream whose spawning call it never held", () => {
    // Arrange: a backgrounded agent's calls reach this stream with no result
    // behind them, so the file plane settles them and nothing here ever would.
    const registry = createCallRegistry();

    // Act
    const held = registry.remember(onStream("toolu_sub", "toolu_unknown_spawn"));

    // Assert
    expect(held).toBe(false);
    expect(registry.open()).toEqual([]);
  });

  it("releases a subagent's calls when its spawning call settles", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember(call("toolu_spawn", "Agent"));
    registry.remember(onStream("toolu_sub", "toolu_spawn"));

    // Act
    registry.take("toolu_spawn");

    // Assert
    expect(registry.peek("toolu_sub")).toBeUndefined();
  });

  it("releases a nested subagent's calls with the agent that spawned it", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember(call("toolu_spawn", "Agent"));
    registry.remember({ ...onStream("toolu_nested", "toolu_spawn"), toolName: "Agent" });
    registry.remember(onStream("toolu_deep", "toolu_nested"));

    // Act
    registry.take("toolu_spawn");

    // Assert
    expect(registry.open()).toEqual([]);
  });

  it("releases a spawn's calls at its handoff, and keeps the spawning call held", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember(call("toolu_spawn", "Agent"));
    registry.remember(onStream("toolu_sub", "toolu_spawn"));

    // Act
    registry.detach("toolu_spawn");

    // Assert
    expect(registry.open().map((pending) => pending.toolUseId)).toEqual(["toolu_spawn"]);
  });

  it("refuses a handed-off spawn's later calls", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember(call("toolu_spawn", "Agent"));
    registry.detach("toolu_spawn");

    // Act
    const held = registry.remember(onStream("toolu_sub", "toolu_spawn"));

    // Assert
    expect(held).toBe(false);
  });

  it("marks a handed-off call detached", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember(call("toolu_bash", "Bash"));

    // Act
    registry.detach("toolu_bash");

    // Assert
    expect(registry.isDetached("toolu_bash")).toBe(true);
  });

  it("ignores a handoff for a call it does not hold", () => {
    // Arrange
    const registry = createCallRegistry();

    // Act
    registry.detach("toolu_never");

    // Assert
    expect(registry.isDetached("toolu_never")).toBe(false);
  });

  it("empties itself when drained", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember(call("toolu_1"));
    registry.remember(call("toolu_2"));

    // Act
    const drained = registry.drain();

    // Assert
    expect(drained.map(({ call: pending }) => pending.toolUseId)).toEqual(["toolu_1", "toolu_2"]);
    expect(registry.open()).toEqual([]);
  });
});

describe("a result's settle", () => {
  it("takes the call out even when it produces no terminal", () => {
    // Arrange: a subagent's result carries no typed output on this stream, so
    // the read writes nothing — and nothing later here could settle it.
    const registry = createCallRegistry();
    registry.remember(call("toolu_r"));

    // Act
    const entries = convertToolResult(TOOL_CONVERTERS, foldContext(), registry, "toolu_r", outcome(), {
      vendorUuid: "u_1",
    });

    // Assert
    expect(entries).toEqual([]);
    expect(registry.peek("toolu_r")).toBeUndefined();
  });

  it("releases a backgrounded shell's call at its receipt", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember({ ...call("toolu_bash", "Bash"), input: { command: "sleep 600", run_in_background: true } });

    // Act
    convertToolResult(
      TOOL_CONVERTERS,
      foldContext(),
      registry,
      "toolu_bash",
      { ...outcome(), structured: { stdout: "", stderr: "", interrupted: false, backgroundTaskId: "b1" } },
      { vendorUuid: "u_1" },
    );

    // Assert
    expect(registry.peek("toolu_bash")).toBeUndefined();
  });

  it("releases an async spawn's call, and its agent's stream, at its launch receipt", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember({ ...call("toolu_spawn", "Agent"), input: { prompt: "sweep", run_in_background: true } });
    registry.remember(onStream("toolu_sub", "toolu_spawn"));

    // Act
    convertToolResult(
      TOOL_CONVERTERS,
      foldContext(),
      registry,
      "toolu_spawn",
      { ...outcome(), structured: { isAsync: true, status: "async_launched", agentId: "a1" } },
      { vendorUuid: "u_1" },
    );

    // Assert
    expect(registry.open()).toEqual([]);
  });

  it("releases a monitor's call at its arming receipt", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember({ ...call("toolu_mon", "Monitor"), input: { command: "tail -f log", description: "watch" } });

    // Act
    convertToolResult(TOOL_CONVERTERS, foldContext(), registry, "toolu_mon", outcome(), {
      vendorUuid: "u_1",
    });

    // Assert
    expect(registry.peek("toolu_mon")).toBeUndefined();
  });

  it("logs a result on a stream it does not hold at debug, never as a warning", () => {
    // Arrange: a backgrounded agent's result that did reach this stream.
    const registry = createCallRegistry();

    // Act
    const records = recordsDuring(() =>
      convertToolResult(
        TOOL_CONVERTERS,
        foldContext(),
        registry,
        "toolu_sub",
        outcome(),
        { vendorUuid: "u_1" },
        "toolu_unknown_spawn",
      ),
    );

    // Assert
    expect(records.filter((record) => record.message === UNHELD_STREAM_RESULT).map((r) => r.level)).toEqual([
      "debug",
    ]);
    expect(records.filter((record) => record.level === "warn")).toEqual([]);
  });

  it("still warns for an unannounced result on a stream it holds", () => {
    // Arrange
    const registry = createCallRegistry();

    // Act
    const records = recordsDuring(() =>
      convertToolResult(TOOL_CONVERTERS, foldContext(), registry, "toolu_never", outcome(), {
        vendorUuid: "u_1",
      }),
    );

    // Assert
    expect(records.filter((record) => record.message === UNANNOUNCED_RESULT).map((r) => r.level)).toEqual([
      "warn",
    ]);
  });

  it("raises a result on another stream than its call's as an ERROR", () => {
    // Arrange: registration and settlement share one identity; a result that
    // rides a different stream from its announcement breaks that.
    const registry = createCallRegistry();
    registry.remember(call("toolu_r"));

    // Act
    const records = recordsDuring(() =>
      convertToolResult(
        TOOL_CONVERTERS,
        foldContext(),
        registry,
        "toolu_r",
        outcome(),
        { vendorUuid: "u_1" },
        "toolu_some_spawn",
      ),
    );

    // Assert
    expect(records.filter((record) => record.message === STREAM_MISMATCH)).toMatchObject([
      {
        level: "error",
        context: { registered_on: "main", settled_on: "toolu_some_spawn" },
      },
    ]);
  });
});

describe("the turn's end", () => {
  // A stopped turn returns no `tool_result` for the call it landed inside, so
  // these terminals are owed here or nowhere, and a unit left on its running
  // arm draws a live tool inside a turn that has ended.
  const stop = { vendorUuid: "vendor-result-uuid" };

  it("settles a shell that was open when the stop landed", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember({ ...call("toolu_bash", "Bash"), input: { command: "sleep 600" } });

    // Act
    const entries = endTurnCalls(TOOL_CONVERTERS, foldContext(), registry, stop, true);

    // Assert
    expect(entries).toHaveLength(1);
    expect(entries[0]?.upsertKey).toBe("activity:toolu_bash");
  });

  it("takes the cut call OUT, so a late result cannot settle one unit twice", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember({ ...call("toolu_bash", "Bash"), input: { command: "sleep 600" } });

    // Act
    endTurnCalls(TOOL_CONVERTERS, foldContext(), registry, stop, true);

    // Assert
    expect(registry.peek("toolu_bash")).toBeUndefined();
  });

  it("writes nothing for a kind that states no cut", () => {
    // Arrange: a read has no vocabulary for being cut short, so no frame
    // could say how it ended.
    const registry = createCallRegistry();
    registry.remember(call("toolu_read", "Read"));

    // Act
    const entries = endTurnCalls(TOOL_CONVERTERS, foldContext(), registry, stop, true);

    // Assert
    expect(entries).toHaveLength(0);
  });

  it("releases a kind that states no cut, since nothing after the turn settles it", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember(call("toolu_read", "Read"));

    // Act
    endTurnCalls(TOOL_CONVERTERS, foldContext(), registry, stop, true);

    // Assert
    expect(registry.peek("toolu_read")).toBeUndefined();
  });

  it("never cuts a call whose work was handed off", () => {
    // Arrange: a backgrounded shell still running elsewhere is not open here.
    const registry = createCallRegistry();
    registry.remember({ ...call("toolu_bash", "Bash"), input: { command: "sleep 600" } });
    registry.detach("toolu_bash");

    // Act
    const entries = endTurnCalls(TOOL_CONVERTERS, foldContext(), registry, stop, true);

    // Assert
    expect(entries).toEqual([]);
  });

  it("cuts nothing when the turn ended on its own", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember({ ...call("toolu_bash", "Bash"), input: { command: "sleep 600" } });

    // Act
    const entries = endTurnCalls(TOOL_CONVERTERS, foldContext(), registry, stop, false);

    // Assert
    expect(entries).toEqual([]);
  });

  it("drains the registry at a turn that ended on its own", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember({ ...call("toolu_skill", "Skill"), input: { skill: "debug-logs" } });

    // Act
    endTurnCalls(TOOL_CONVERTERS, foldContext(), registry, stop, false);

    // Assert
    expect(registry.open()).toEqual([]);
  });

  it("logs the calls it released at info", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember(call("toolu_read", "Read"));

    // Act
    const records = recordsDuring(() => endTurnCalls(TOOL_CONVERTERS, foldContext(), registry, stop, false));

    // Assert
    expect(records.filter((record) => record.message === TURN_RELEASED)).toMatchObject([
      {
        level: "info",
        context: { released: 1, tool_use_ids: ["toolu_read"] },
      },
    ]);
  });

  it("gives each cut frame its own block ordinal, so two cannot share a write id", () => {
    // Arrange: the write id is minted from the source coordinates, and both
    // frames derive from ONE vendor record — so without the ordinal the store
    // would absorb the second as a duplicate of the first.
    const registry = createCallRegistry();
    registry.remember({ ...call("toolu_a", "Bash"), input: { command: "sleep 1" } });
    registry.remember({ ...call("toolu_b", "Bash"), input: { command: "sleep 2" } });

    // Act
    const entries = endTurnCalls(TOOL_CONVERTERS, foldContext(), registry, stop, true);

    // Assert
    expect(entries.map((entry) => entry.source.blockIndex)).toEqual([0, 1]);
  });
});

describe("the query's end", () => {
  it("releases every call still held", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember({ ...call("toolu_bash", "Bash"), input: { command: "sleep 600" } });

    // Act
    endQueryCalls(registry, "the vendor query died");

    // Assert
    expect(registry.open()).toEqual([]);
  });

  it("logs what it released at info", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember(call("toolu_read", "Read"));

    // Act
    const records = recordsDuring(() => endQueryCalls(registry, "the vendor query died"));

    // Assert
    expect(records.filter((record) => record.message === QUERY_RELEASED)).toMatchObject([
      {
        level: "info",
        context: { why: "the vendor query died", released: 1 },
      },
    ]);
  });

  it("logs nothing when nothing was held", () => {
    // Arrange
    const registry = createCallRegistry();

    // Act
    const records = recordsDuring(() => endQueryCalls(registry, "the vendor query died"));

    // Assert
    expect(records.filter((record) => record.message === QUERY_RELEASED)).toEqual([]);
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

  it("is MODELLED as an MCP tool call for a runtime-registered MCP tool", () => {
    const disposition = dispositionOf(TOOL_CONVERTERS, "mcp__Slack__send");
    expect(disposition.case === "modelled" ? disposition.converter.kind : disposition.case).toBe("mcp_tool_call");
  });

  it("is UNMODELED for a genuinely unknown tool", () => {
    expect(dispositionOf(TOOL_CONVERTERS, "StructuredOutput").case).toBe("unmodeled");
  });

  it("refuses an MCP tool when the registry holds no MCP converter", () => {
    expect(() => dispositionOf(new Map(), "mcp__Slack__send")).toThrow(/MCP tool converter is missing/);
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
      convertToolUse(empty, foldContext(), registry, call("toolu_x", "StructuredOutput"), {
        agentId: MAIN_AGENT,
        vendorUuid: "uuid-1",
      }),
    ).toThrow(/unmodeled converter is missing/);
  });

  it("refuses the RESULT of a call it cannot model too", () => {
    // Arrange
    const registry = createCallRegistry();
    registry.remember(call("toolu_x", "StructuredOutput"));
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
        call("toolu_x", "StructuredOutput"),
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
