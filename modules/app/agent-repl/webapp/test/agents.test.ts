import { describe, expect, it } from "vitest";

import {
  SUBAGENT_TOOLS,
  SubagentEntry,
  agentsLabel,
  agentsMenuHtml,
  agentsOverlayHtml,
  runningCount,
  sessionSubagents,
} from "../src/agents.js";
import { ConversationItem, ToolItem } from "../src/store.js";

/** A subagent tool call in the feed, defaulted to a settled successful one. */
function agentTool(over: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
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

/** A roster entry, defaulted to a settled successful one. */
function entry(over: Partial<SubagentEntry> = {}): SubagentEntry {
  return {
    toolUseId: "t1",
    description: "hunt the flake",
    agentType: "Explore",
    status: "done",
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
    expect(agents.map((a) => a.toolUseId)).toEqual(["t1", "t2"]);
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
      { kind: "text", blockId: "b1", messageId: "m1", text: "hi", done: true },
    ];
    // Act + Assert
    expect(sessionSubagents(items)).toHaveLength(0);
  });

  it("carries the call's description", () => {
    // Arrange + Act
    const [agent] = sessionSubagents([agentTool()]);
    // Assert
    expect(agent.description).toBe("hunt the flake");
  });

  it("carries the call's subagent type", () => {
    // Arrange + Act
    const [agent] = sessionSubagents([agentTool()]);
    // Assert
    expect(agent.agentType).toBe("Explore");
  });

  it("leaves the subagent type empty when the call named none", () => {
    // Arrange + Act
    const [agent] = sessionSubagents([agentTool({ input: { description: "d" } })]);
    // Assert
    expect(agent.agentType).toBe("");
  });

  it("reads a call whose input is still streaming as starting", () => {
    // Arrange
    const item = agentTool({ inputDone: false, input: undefined, result: undefined });
    // Act
    const [agent] = sessionSubagents([item]);
    // Assert
    expect(agent.status).toBe("starting");
  });

  it("leaves the description empty while the input still streams", () => {
    // Arrange
    const item = agentTool({ inputDone: false, input: undefined, result: undefined });
    // Act
    const [agent] = sessionSubagents([item]);
    // Assert
    expect(agent.description).toBe("");
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
});

describe("runningCount", () => {
  it("counts a subagent awaiting its result", () => {
    // Arrange + Act + Assert
    expect(runningCount([entry({ status: "running" })])).toBe(1);
  });

  it("counts a subagent whose input is still streaming", () => {
    // Arrange + Act + Assert
    expect(runningCount([entry({ status: "starting" })])).toBe(1);
  });

  it("ignores a settled subagent", () => {
    // Arrange + Act + Assert
    expect(runningCount([entry({ status: "done" })])).toBe(0);
  });

  it("ignores a failed subagent", () => {
    // Arrange + Act + Assert
    expect(runningCount([entry({ status: "error" })])).toBe(0);
  });
});

describe("agentsLabel", () => {
  it("singularizes a lone subagent", () => {
    // Arrange + Act + Assert
    expect(agentsLabel(1)).toBe("1 agent");
  });

  it("pluralizes several subagents", () => {
    // Arrange + Act + Assert
    expect(agentsLabel(3)).toBe("3 agents");
  });
});

describe("agentsMenuHtml", () => {
  it("renders nothing when the session spawned no subagents", () => {
    // Arrange + Act + Assert
    expect(agentsMenuHtml([], false)).toBe("");
  });

  it("labels the chip with the subagent count", () => {
    // Arrange + Act
    const html = agentsMenuHtml([entry(), entry({ toolUseId: "t2" })], false);
    // Assert
    expect(html).toContain("2 agents");
  });

  it("carries a caret so the chip reads as a dropdown", () => {
    // Arrange + Act
    const html = agentsMenuHtml([entry()], false);
    // Assert
    expect(html).toContain(`<span class="agents-caret" aria-hidden="true">▾</span>`);
  });

  it("flips the caret when the roster is open", () => {
    // Arrange + Act
    const html = agentsMenuHtml([entry()], true);
    // Assert
    expect(html).toContain(`<span class="agents-caret" aria-hidden="true">▴</span>`);
  });

  it("marks the toggle for the topbar's delegated click handler", () => {
    // Arrange + Act
    const html = agentsMenuHtml([entry()], false);
    // Assert
    expect(html).toContain("data-agents-toggle");
  });

  it("reports the closed roster to assistive tech", () => {
    // Arrange + Act
    const html = agentsMenuHtml([entry()], false);
    // Assert
    expect(html).toContain(`aria-expanded="false"`);
  });

  it("reports the open roster to assistive tech", () => {
    // Arrange + Act
    const html = agentsMenuHtml([entry()], true);
    // Assert
    expect(html).toContain(`aria-expanded="true"`);
  });

  it("withholds the roster while the chip is closed", () => {
    // Arrange + Act
    const html = agentsMenuHtml([entry()], false);
    // Assert
    expect(html).not.toContain("agents-overlay");
  });

  it("drops the roster as an overlay while the chip is open", () => {
    // Arrange + Act
    const html = agentsMenuHtml([entry()], true);
    // Assert
    expect(html).toContain("agents-overlay");
  });

  it("annotates the chip with how many subagents are still working", () => {
    // Arrange + Act
    const html = agentsMenuHtml([entry({ status: "running" }), entry({ toolUseId: "t2" })], false);
    // Assert
    expect(html).toContain(`<span class="agents-running">1 running</span>`);
  });

  it("omits the running annotation once every subagent has settled", () => {
    // Arrange + Act
    const html = agentsMenuHtml([entry()], false);
    // Assert
    expect(html).not.toContain("agents-running");
  });
});

describe("agentsOverlayHtml", () => {
  it("renders one row per subagent", () => {
    // Arrange + Act
    const html = agentsOverlayHtml([entry(), entry({ toolUseId: "t2" })]);
    // Assert
    expect(html.match(/class="agent-row/g)).toHaveLength(2);
  });

  it("names each subagent by its description", () => {
    // Arrange + Act
    const html = agentsOverlayHtml([entry({ description: "hunt the flake" })]);
    // Assert
    expect(html).toContain("hunt the flake");
  });

  it("stands a placeholder in for a description that has not streamed yet", () => {
    // Arrange + Act
    const html = agentsOverlayHtml([entry({ description: "", status: "starting" })]);
    // Assert
    expect(html).toContain("starting…");
  });

  it("chips the subagent type beside the description", () => {
    // Arrange + Act
    const html = agentsOverlayHtml([entry({ agentType: "Explore" })]);
    // Assert
    expect(html).toContain(`<span class="agent-type">Explore</span>`);
  });

  it("omits the type chip for a subagent that named no type", () => {
    // Arrange + Act
    const html = agentsOverlayHtml([entry({ agentType: "" })]);
    // Assert
    expect(html).not.toContain("agent-type");
  });

  it("colors the status dot by the subagent's status", () => {
    // Arrange + Act
    const html = agentsOverlayHtml([entry({ status: "error" })]);
    // Assert
    expect(html).toContain("agent-dot agent-error");
  });

  it("spells the status out beside the dot", () => {
    // Arrange + Act
    const html = agentsOverlayHtml([entry({ status: "running" })]);
    // Assert
    expect(html).toContain(`<span class="agent-status">running</span>`);
  });

  it("indents a nested subagent's row", () => {
    // Arrange + Act
    const html = agentsOverlayHtml([entry({ nested: true })]);
    // Assert
    expect(html).toContain(`class="agent-row nested"`);
  });

  it("escapes markup in a subagent's description", () => {
    // Arrange + Act
    const html = agentsOverlayHtml([entry({ description: "<img src=x>" })]);
    // Assert
    expect(html).not.toContain("<img");
  });

  it("escapes markup in a subagent's type", () => {
    // Arrange + Act
    const html = agentsOverlayHtml([entry({ agentType: "<img src=x>" })]);
    // Assert
    expect(html).not.toContain("<img");
  });
});
