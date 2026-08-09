import { describe, expect, it } from "vitest";

import {
  SUBAGENT_TOOLS,
  agentSubagents,
  agentsMenuHtml,
  agentsToggleHtml,
  sessionSubagents,
} from "../src/agents.js";
import { CounterEntry } from "../src/counter-menu.js";
import { ConversationItem, ToolItem } from "../src/store.js";

/** A subagent tool call in the feed, defaulted to a settled successful one. */
function agentTool(over: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
    ts: "2026-05-24T10:00:00.000Z",
    toolUseId: "t1",
    toolName: "Agent",
    messageId: "m1",
    inputJson: "",
    input: { description: "hunt the flake", subagent_type: "Explore" },
    inputDone: true,
    result: { isError: false, content: "found it" },
    ...over,
  };
}

/** A context clear, which bounds the context the roster counts. */
function cleared(): ConversationItem {
  return { kind: "context-cleared", uuid: "c1" };
}

/** A compaction, the other event the roster counts from. */
function compacted(): ConversationItem {
  return {
    kind: "context-compacted",
    uuid: "k1",
    trigger: "auto",
    preTokens: 100,
    postTokens: 10,
    durationMs: 500,
    summary: "what came before",
  };
}

/** A counter entry, for the render-delegation tests. */
function entry(over: Partial<CounterEntry> = {}): CounterEntry {
  return {
    id: "t1",
    summary: "hunt the flake",
    detail: "Explore",
    status: "running",
    nested: false,
    ...over,
  };
}

describe("SUBAGENT_TOOLS", () => {
  it("counts the current CLI's Agent tool", () => {
    // Arrange + Act + Assert
    expect(SUBAGENT_TOOLS.has("Agent")).toBe(true);
  });

  it("counts the legacy Task tool a replayed transcript still carries", () => {
    // Arrange + Act + Assert
    expect(SUBAGENT_TOOLS.has("Task")).toBe(true);
  });
});

describe("sessionSubagents", () => {
  it("derives one entry per subagent tool call", () => {
    // Arrange
    const items: ConversationItem[] = [agentTool(), agentTool({ toolUseId: "t2" })];
    // Act
    const agents = sessionSubagents(items);
    // Assert
    expect(agents.map((a) => a.id)).toEqual(["t1", "t2"]);
  });

  it("includes a legacy Task call", () => {
    // Arrange
    const items: ConversationItem[] = [agentTool({ toolName: "Task" })];
    // Act + Assert
    expect(sessionSubagents(items)).toHaveLength(1);
  });

  it("ignores tool calls that spawn no subagent", () => {
    // Arrange
    const items: ConversationItem[] = [agentTool({ toolName: "Bash" })];
    // Act + Assert
    expect(sessionSubagents(items)).toHaveLength(0);
  });

  it("ignores non-tool items", () => {
    // Arrange
    const items: ConversationItem[] = [
      { kind: "text", blockId: "b1", messageId: "m1", text: "hi", done: true, ts: "2026-05-24T09:05:00Z" },
    ];
    // Act + Assert
    expect(sessionSubagents(items)).toHaveLength(0);
  });

  it("carries the call's description as the summary", () => {
    // Arrange + Act
    const [agent] = sessionSubagents([agentTool()]);
    // Assert
    expect(agent.summary).toBe("hunt the flake");
  });

  it("carries the call's subagent type as the detail", () => {
    // Arrange + Act
    const [agent] = sessionSubagents([agentTool()]);
    // Assert
    expect(agent.detail).toBe("Explore");
  });

  it("leaves the detail empty when the call named no type", () => {
    // Arrange + Act
    const [agent] = sessionSubagents([agentTool({ input: { description: "d" } })]);
    // Assert
    expect(agent.detail).toBe("");
  });

  it("reads a call whose input is still streaming as starting", () => {
    // Arrange
    const item = agentTool({ inputDone: false, input: undefined, result: undefined });
    // Act
    const [agent] = sessionSubagents([item]);
    // Assert
    expect(agent.status).toBe("starting");
  });

  it("leaves the summary empty while the input still streams", () => {
    // Arrange
    const item = agentTool({ inputDone: false, input: undefined, result: undefined });
    // Act
    const [agent] = sessionSubagents([item]);
    // Assert
    expect(agent.summary).toBe("");
  });

  it("reads a call awaiting its result as running", () => {
    // Arrange + Act
    const [agent] = sessionSubagents([agentTool({ result: undefined })]);
    // Assert
    expect(agent.status).toBe("running");
  });

  it("reads a settled call as done", () => {
    // Arrange + Act
    const [agent] = sessionSubagents([agentTool()]);
    // Assert
    expect(agent.status).toBe("done");
  });

  it("reads a failed call as error", () => {
    // Arrange
    const item = agentTool({ result: { isError: true, content: "boom" } });
    // Act
    const [agent] = sessionSubagents([item]);
    // Assert
    expect(agent.status).toBe("error");
  });

  it("marks a subagent another subagent spawned as nested", () => {
    // Arrange + Act
    const [agent] = sessionSubagents([agentTool({ parentToolUseId: "t0" })]);
    // Assert
    expect(agent.nested).toBe(true);
  });

  it("marks a subagent the main loop spawned as not nested", () => {
    // Arrange + Act
    const [agent] = sessionSubagents([agentTool()]);
    // Assert
    expect(agent.nested).toBe(false);
  });

  it("forgets a subagent a clear discarded from the context", () => {
    // Arrange
    const items: ConversationItem[] = [agentTool(), cleared()];
    // Act + Assert
    expect(sessionSubagents(items)).toHaveLength(0);
  });

  it("forgets a subagent a compaction discarded from the context", () => {
    // Arrange
    const items: ConversationItem[] = [agentTool(), compacted()];
    // Act + Assert
    expect(sessionSubagents(items)).toHaveLength(0);
  });

  it("keeps a subagent spawned after the clear", () => {
    // Arrange
    const items: ConversationItem[] = [agentTool(), cleared(), agentTool({ toolUseId: "t2" })];
    // Act
    const agents = sessionSubagents(items);
    // Assert
    expect(agents.map((a) => a.id)).toEqual(["t2"]);
  });
});

describe("agentSubagents", () => {
  it("keeps only the subagents spawned directly by the given agent", () => {
    // Arrange — one child of a1, one main-chain sibling.
    const items = [
      agentTool({ toolUseId: "a1" }),
      agentTool({ toolUseId: "c1", parentToolUseId: "a1" }),
      agentTool({ toolUseId: "s1" }),
    ];
    // Act
    const entries = agentSubagents(items, "a1");
    // Assert
    expect(entries.map((e) => e.id)).toEqual(["c1"]);
  });

  it("leaves a grandchild to its own parent's roster", () => {
    // Arrange — c1 under a1, g1 under c1.
    const items = [
      agentTool({ toolUseId: "a1" }),
      agentTool({ toolUseId: "c1", parentToolUseId: "a1" }),
      agentTool({ toolUseId: "g1", parentToolUseId: "c1" }),
    ];
    // Act + Assert
    expect(agentSubagents(items, "a1").map((e) => e.id)).toEqual(["c1"]);
  });

  it("never marks a row nested, since every row is a direct spawn", () => {
    // Arrange
    const items = [agentTool({ toolUseId: "c1", parentToolUseId: "a1" })];
    // Act
    const [child] = agentSubagents(items, "a1");
    // Assert
    expect(child.nested).toBe(false);
  });

  it("ignores non-subagent children of the agent", () => {
    // Arrange — a Bash call the agent ran is not a subagent.
    const items = [agentTool({ toolUseId: "b1", toolName: "Bash", parentToolUseId: "a1" })];
    // Act + Assert
    expect(agentSubagents(items, "a1")).toEqual([]);
  });
});

describe("agentsMenuHtml", () => {
  it("renders nothing when the session spawned no subagents", () => {
    // Arrange + Act + Assert
    expect(agentsMenuHtml([], false)).toBe("");
  });

  it("labels the chip with the running-subagent count", () => {
    // Arrange + Act
    const html = agentsMenuHtml([entry(), entry({ id: "t2" })], false);
    // Assert
    expect(html).toContain("2 agents");
  });

  it("renders the subagent menu under the agents class", () => {
    // Arrange + Act
    const html = agentsMenuHtml([entry()], false);
    // Assert
    expect(html).toContain("agents-menu");
  });

  it("hides a settled subagent from the roster", () => {
    // Arrange — one done subagent, no longer running.
    const agents = [entry({ status: "done" })];
    // Act + Assert — the whole chip disappears once nothing is running.
    expect(agentsMenuHtml(agents, false)).toBe("");
  });

  it("counts only the still-running subagents", () => {
    // Arrange — one running, one done.
    const agents = [entry({ id: "t1", status: "running" }), entry({ id: "t2", status: "done" })];
    // Act + Assert
    expect(agentsMenuHtml(agents, false)).toContain("1 agent");
  });
});

describe("agentsToggleHtml", () => {
  it("renders nothing when the session spawned no subagents", () => {
    // Arrange + Act + Assert
    expect(agentsToggleHtml([], false)).toBe("");
  });

  it("labels the chip with the running-subagent count", () => {
    // Arrange + Act
    const html = agentsToggleHtml([entry(), entry({ id: "t2" })], false);
    // Assert
    expect(html).toContain("2 agents");
  });

  it("drops no overlay of its own while the section it toggles is open", () => {
    // Arrange + Act — the roster it would list is the expanded footer's rows.
    const html = agentsToggleHtml([entry()], true);
    // Assert
    expect(html).not.toContain("agents-overlay");
  });

  it("reports the expanded footer's state as the chip's own", () => {
    // Arrange + Act
    const html = agentsToggleHtml([entry()], true);
    // Assert
    expect(html).toContain('aria-expanded="true"');
  });
});
