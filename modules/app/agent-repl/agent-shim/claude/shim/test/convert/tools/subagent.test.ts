/**
 * The subagent spawn. A WRONG created-agent id here does not crash — it draws a
 * whole subagent's book under an agent that does not exist — so the id the
 * announcement carries is asserted to be exactly the one the subagent's frames
 * are routed by: the spawning call's `tool_use_id` (landing 3's binding minting
 * rule, and the same value `convert/fold-context.ts:subagentBook` uses).
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import {
  isLaunchReceipt,
  subagentConverter,
  subagentPrompt,
  subagentStartFrom,
} from "../../../src/convert/tools/subagent.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { toolInput, toolUseResult } from "./corpus.js";

function call(input: Record<string, unknown>, toolName = "Agent"): PendingCall {
  return {
    toolUseId: "toolu_spawn",
    toolName,
    input,
    startedAtMs: 1_000,
    agentId: create(conversationv1.AgentIdSchema, { value: "caller" }),
  };
}

function outcome(structured: unknown, isError = false): ToolOutcome {
  return { content: undefined, isError, structured, settledAtMs: 9_000 };
}

function successOf(item: conversationv1.AgentActivity["item"] | undefined): conversationv1.AgentSubagentSuccess {
  expect(item?.case).toBe("subagent");
  const result = (item?.value as conversationv1.AgentSubagent).result;
  expect(result.case).toBe("success");
  return result.value as conversationv1.AgentSubagentSuccess;
}

describe("subagentConverter.start", () => {
  it("names the created agent as the spawning call's own id", () => {
    // Arrange.
    const pending = call(toolInput("agent"));

    // Act.
    const item = subagentConverter.start(pending);

    // Assert.
    expect(item?.case).toBe("subagent");
    const result = (item?.value as conversationv1.AgentSubagent).result;
    expect(result.case).toBe("start");
    expect((result.value as conversationv1.AgentSubagentStart).createdAgentId?.value).toBe(
      "toolu_spawn",
    );
  });

  it("restates the commission on the start, from the call's own input", () => {
    // A start frame stands alone: a consumer draws the container AND what the
    // subagent was asked to do from this one frame.
    // Arrange.
    const pending = call(toolInput("agent"));

    // Act.
    const item = subagentConverter.start(pending);

    // Assert.
    const result = (item?.value as conversationv1.AgentSubagent).result;
    const start = result.value as conversationv1.AgentSubagentStart;
    expect(start.prompt).toEqual(subagentPrompt(pending));
    expect(start.startedAt).toBeDefined();
  });
});

describe("subagentStartFrom", () => {
  it("names the created agent, from an identity the caller supplies", () => {
    // Arrange.
    const created = create(conversationv1.AgentIdSchema, { value: "a36ef865012a4672a" });

    // Act.
    const item = subagentStartFrom(call(toolInput("agent")), created);

    // Assert.
    const start = (item?.value as conversationv1.AgentSubagent).result
      .value as conversationv1.AgentSubagentStart;
    expect(start.createdAgentId?.value).toBe("a36ef865012a4672a");
  });

  it("leaves spawn_depth unset, because the fold holds no parent chain to walk", () => {
    // Arrange, Act.
    const item = subagentStartFrom(
      call(toolInput("agent")),
      create(conversationv1.AgentIdSchema, { value: "a1" }),
    );

    // Assert.
    const start = (item?.value as conversationv1.AgentSubagent).result
      .value as conversationv1.AgentSubagentStart;
    expect(start.spawnDepth).toBeUndefined();
  });

  it("leaves working_dir unset, because no vendor field states one", () => {
    // Arrange, Act.
    const item = subagentStartFrom(
      call(toolInput("agent")),
      create(conversationv1.AgentIdSchema, { value: "a1" }),
    );

    // Assert.
    const start = (item?.value as conversationv1.AgentSubagent).result
      .value as conversationv1.AgentSubagentStart;
    expect(start.workingDir).toBeUndefined();
  });

  it("stamps the announcement instant", () => {
    // Arrange, Act.
    const item = subagentStartFrom(
      call(toolInput("agent")),
      create(conversationv1.AgentIdSchema, { value: "a1" }),
    );

    // Assert.
    const start = (item?.value as conversationv1.AgentSubagent).result
      .value as conversationv1.AgentSubagentStart;
    expect(start.startedAt?.atMs).toBe(1_000n);
  });
});

describe("subagentPrompt", () => {
  it("carries the corpus spawn's description and subagent type", () => {
    // Arrange.
    const input = toolInput("agent");

    // Act.
    const prompt = subagentPrompt(call(input));

    // Assert.
    expect([prompt.description, prompt.subagentType]).toEqual([
      "Explore tests and design docs",
      "Explore",
    ]);
  });

  it("carries the instruction text from the call's own input", () => {
    // Arrange.
    const input = toolInput("agent");

    // Act.
    const prompt = subagentPrompt(call(input));

    // Assert.
    expect(prompt.text).toBe(input["prompt"]);
  });

  it("reads a fork off the subagent TYPE, since the vendor spells it there", () => {
    // Arrange, Act.
    const prompt = subagentPrompt(call({ prompt: "p", subagent_type: "fork" }));

    // Assert.
    expect(prompt.forkedFromCaller).toBe(true);
  });

  it("is not a fork for an ordinary subagent type", () => {
    // Arrange, Act.
    const prompt = subagentPrompt(call({ prompt: "p", subagent_type: "Explore" }));

    // Assert.
    expect(prompt.forkedFromCaller).toBe(false);
  });

  it("carries a requested model override as an AgentModel", () => {
    // Arrange, Act.
    const prompt = subagentPrompt(call({ prompt: "p", model: "opus" }));

    // Assert.
    expect(prompt.requestedModel?.name).toBe("opus");
  });

  it("leaves the requested model unset when the caller asked for no override", () => {
    // Arrange, Act.
    const prompt = subagentPrompt(call({ prompt: "p" }));

    // Assert.
    expect(prompt.requestedModel).toBeUndefined();
  });

  it("carries the name that makes a running subagent addressable", () => {
    // Arrange, Act.
    const prompt = subagentPrompt(call({ prompt: "p", name: "vetter" }));

    // Assert.
    expect(prompt.requestedName).toBe("vetter");
  });

  it("is isolation `none` when the caller asked for none", () => {
    // Arrange, Act.
    const prompt = subagentPrompt(call({ prompt: "p" }));

    // Assert.
    expect(prompt.isolation.case).toBe("none");
  });

  it("is isolation `worktree` when the caller asked for a tree of its own", () => {
    // Arrange, Act.
    const prompt = subagentPrompt(call({ prompt: "p", isolation: "worktree" }));

    // Assert.
    expect(prompt.isolation.case).toBe("worktree");
  });

  it("is isolation `remote` when the caller asked for a cloud environment", () => {
    // Arrange, Act.
    const prompt = subagentPrompt(call({ prompt: "p", isolation: "remote" }));

    // Assert.
    expect(prompt.isolation.case).toBe("remote");
  });

  it("carries no remote handles at issue, because they arrive with the launch", () => {
    // Arrange, Act.
    const prompt = subagentPrompt(call({ prompt: "p", isolation: "remote" }));

    // Assert.
    const remote = prompt.isolation.value as conversationv1.AgentSubagentIsolationRemote;
    expect([remote.sessionUrl, remote.remoteTaskId]).toEqual([undefined, undefined]);
  });

  it("carries an empty instruction rather than losing the spawn when no prompt was stated", () => {
    // Arrange, Act.
    const prompt = subagentPrompt(call({ description: "d" }));

    // Assert.
    expect(prompt.text).toBe("");
  });
});

describe("subagentConverter.settle", () => {
  it("names the created agent on the conclusion, so a settled-only delivery can address it", () => {
    // A history replay hands a consumer this frame and no start; without the
    // id the bubble it draws would address nothing.
    // Arrange, Act.
    const success = successOf(
      subagentConverter.settle(call(toolInput("agent")), outcome(toolUseResult("agent"))),
    );

    // Assert: the minting rule's value, the same one the start states.
    expect(success.createdAgentId?.value).toBe("toolu_spawn");
  });

  it("settles the corpus's completed spawn with the subagent's own prose", () => {
    // Arrange.
    const structured = toolUseResult("agent");

    // Act.
    const success = successOf(subagentConverter.settle(call({ prompt: "p" }), outcome(structured)));

    // Assert.
    expect(success.report?.prose?.markdown).toBe(
      (structured["content"] as { text: string }[])[0].text,
    );
  });

  it("leaves the structured result unset, because no vendor field declares one", () => {
    // Arrange, Act.
    const success = successOf(
      subagentConverter.settle(call({ prompt: "p" }), outcome(toolUseResult("agent"))),
    );

    // Assert.
    expect(success.report?.structuredResult).toBeUndefined();
  });

  it("carries the corpus run's duration and tool-use count", () => {
    // Arrange, Act.
    const success = successOf(
      subagentConverter.settle(call({ prompt: "p" }), outcome(toolUseResult("agent"))),
    );

    // Assert.
    expect([success.totals?.durationMs, success.totals?.toolUseCount]).toEqual([228_159n, 18]);
  });

  it("maps the corpus usage counters into the canonical token shape", () => {
    // Arrange, Act.
    const success = successOf(
      subagentConverter.settle(call({ prompt: "p" }), outcome(toolUseResult("agent"))),
    );

    // Assert.
    expect(success.totals?.usage).toEqual({
      case: "full",
      value: create(conversationv1.TokenUsageSchema, {
        inputHits: create(conversationv1.TokenCacheHitsSchema, { read: 50_066n }),
        inputMisses: create(conversationv1.TokenCacheMissesSchema, { written: 1_707n, unwritten: 2n }),
        outputTokens: 3_200n,
        outputThinkingTokens: 0n,
      }),
    });
  });

  it("takes the SYNC usage arm, because an awaited completion is the only path it settles", () => {
    // Arrange, Act.
    const success = successOf(
      subagentConverter.settle(call({ prompt: "p" }), outcome(toolUseResult("agent"))),
    );

    // Assert.
    expect(success.totals?.usage.case).toBe("full");
  });

  it("leaves the usage arm unset when the vendor reported no usage at all", () => {
    // Arrange.
    const structured = { ...toolUseResult("agent") };
    delete structured["usage"];

    // Act.
    const success = successOf(subagentConverter.settle(call({ prompt: "p" }), outcome(structured)));

    // Assert.
    expect(success.totals?.usage.case).toBeUndefined();
  });

  it("carries the corpus tool-stat breakdown", () => {
    // Arrange, Act.
    const success = successOf(
      subagentConverter.settle(call({ prompt: "p" }), outcome(toolUseResult("agent"))),
    );

    // Assert.
    expect(success.totals?.toolStats?.bashCount).toBe(18);
  });

  it("leaves the tool stats unset when the vendor broke nothing out", () => {
    // Arrange.
    const structured = { ...toolUseResult("agent") };
    delete structured["toolStats"];

    // Act.
    const success = successOf(subagentConverter.settle(call({ prompt: "p" }), outcome(structured)));

    // Assert.
    expect(success.totals?.toolStats).toBeUndefined();
  });

  it("falls back to the resolved model for models_used when no list was given", () => {
    // Arrange, Act.
    const success = successOf(
      subagentConverter.settle(call({ prompt: "p" }), outcome(toolUseResult("agent"))),
    );

    // Assert.
    expect(success.modelsUsed.map((model) => model.name)).toEqual(["claude-fable-5"]);
  });

  it("carries the whole models_used list when the vendor stated a mid-run swap", () => {
    // Arrange.
    const structured = { ...toolUseResult("agent"), modelsUsed: ["claude-opus-5", "claude-fable-5"] };

    // Act.
    const success = successOf(subagentConverter.settle(call({ prompt: "p" }), outcome(structured)));

    // Assert.
    expect(success.modelsUsed.map((model) => model.name)).toEqual([
      "claude-opus-5",
      "claude-fable-5",
    ]);
  });

  it("reads resolved_subagent_type off agentType, which is what that field means", () => {
    // Arrange, Act.
    const success = successOf(
      subagentConverter.settle(call({ prompt: "p" }), outcome(toolUseResult("agent"))),
    );

    // Assert.
    expect(success.resolvedSubagentType).toBe("general-purpose");
  });

  it("carries the worktree when the vendor stated both a path and a branch", () => {
    // Arrange.
    const structured = {
      ...toolUseResult("agent"),
      worktreePath: "/tmp/wt",
      worktreeBranch: "agent/wt",
    };

    // Act.
    const success = successOf(subagentConverter.settle(call({ prompt: "p" }), outcome(structured)));

    // Assert.
    expect([success.worktree?.path, success.worktree?.branch]).toEqual(["/tmp/wt", "agent/wt"]);
  });

  it("carries NO worktree when the vendor stated only half of one", () => {
    // Arrange.
    const structured = { ...toolUseResult("agent"), worktreePath: "/tmp/wt" };

    // Act.
    const success = successOf(subagentConverter.settle(call({ prompt: "p" }), outcome(structured)));

    // Assert.
    expect(success.worktree).toBeUndefined();
  });

  it("carries no worktree for a subagent that shared its caller's tree", () => {
    // Arrange, Act.
    const success = successOf(
      subagentConverter.settle(call({ prompt: "p" }), outcome(toolUseResult("agent"))),
    );

    // Assert.
    expect(success.worktree).toBeUndefined();
  });

  it("stamps the settle instant", () => {
    // Arrange, Act.
    const success = successOf(
      subagentConverter.settle(call({ prompt: "p" }), outcome(toolUseResult("agent"))),
    );

    // Assert.
    expect(success.settledAt?.atMs).toBe(9_000n);
  });

  it("restates the prompt on the settled frame, so it describes itself", () => {
    // Arrange, Act.
    const success = successOf(
      subagentConverter.settle(call({ prompt: "instruction" }), outcome(toolUseResult("agent"))),
    );

    // Assert.
    expect(success.prompt?.text).toBe("instruction");
  });

  it("does NOT settle the corpus's async launch, which is a receipt and not a conclusion", () => {
    // Arrange, Act, Assert.
    expect(
      subagentConverter.settle(call({ prompt: "p" }), outcome(toolUseResult("agent_async_launch"))),
    ).toBeUndefined();
  });

  it("does NOT settle a remote launch, whose run moved off these streams", () => {
    // Arrange, Act, Assert.
    expect(
      subagentConverter.settle(
        call({ prompt: "p", isolation: "remote" }),
        outcome({ status: "remote_launched", taskId: "t1", sessionUrl: "https://x", prompt: "p" }),
      ),
    ).toBeUndefined();
  });

  it("settles a vendor-marked error as the failure arm", () => {
    // Arrange, Act.
    const item = subagentConverter.settle(call({ prompt: "p" }), outcome(undefined, true))!;

    // Assert.
    expect((item?.value as conversationv1.AgentSubagent).result.case).toBe("failure");
  });

  it("leaves the failure cause unset, because no vendor field states a user stop", () => {
    // Arrange, Act.
    const item = subagentConverter.settle(call({ prompt: "p" }), outcome(undefined, true))!;

    // Assert.
    const failure = (item?.value as conversationv1.AgentSubagent).result
      .value as conversationv1.AgentSubagentFailure;
    expect(failure.cause.case).toBeUndefined();
  });

  it("produces NO frame for a completion that stated no report content", () => {
    // Arrange.
    const structured = { ...toolUseResult("agent") };
    delete structured["content"];

    // Act, Assert.
    expect(subagentConverter.settle(call({ prompt: "p" }), outcome(structured))).toBeUndefined();
  });

  it("produces NO frame for a completion that stated no duration", () => {
    // Arrange.
    const structured = { ...toolUseResult("agent") };
    delete structured["totalDurationMs"];

    // Act, Assert.
    expect(subagentConverter.settle(call({ prompt: "p" }), outcome(structured))).toBeUndefined();
  });

  it("produces NO frame for a result that carried no typed output at all", () => {
    // Arrange, Act, Assert.
    expect(subagentConverter.settle(call({ prompt: "p" }), outcome("prose only"))).toBeUndefined();
  });

  it("declares no progress arm, because AgentSubagent has none", () => {
    // Arrange, Act, Assert.
    expect(subagentConverter.carriesProgress).toBe(false);
  });
});

describe("subagentPrompt isolation the contract has no arm for", () => {
  it("carries none for an isolation this contract does not model", () => {
    // Arrange, Act.
    const prompt = subagentPrompt(call({ prompt: "p", isolation: "moonbase" }));

    // Assert.
    expect(prompt.isolation.case).toBe("none");
  });
});

describe("subagentConverter.settle for facts the vendor left unstated", () => {
  /** The corpus completion, with the named keys replaced. */
  function completionWith(patch: Record<string, unknown>): Record<string, unknown> {
    return { ...toolUseResult("agent"), ...patch };
  }

  it("still carries the report when the completion named no agent id", () => {
    // Arrange.
    const structured = completionWith({});
    delete structured["agentId"];

    // Act.
    const success = successOf(subagentConverter.settle(call({ prompt: "p" }), outcome(structured)));

    // Assert.
    expect(success.report?.prose?.markdown).toBe(
      (structured["content"] as { text: string }[])[0].text,
    );
  });

  const USAGE_DEFAULTS: [string, (usage: conversationv1.TokenUsage) => bigint][] = [
    ["cache reads", (usage) => usage.inputHits!.read],
    ["cache writes", (usage) => usage.inputMisses!.written],
    ["uncached input tokens", (usage) => usage.inputMisses!.unwritten],
    ["output tokens", (usage) => usage.outputTokens],
    ["thinking tokens", (usage) => usage.outputThinkingTokens],
  ];

  for (const [name, read] of USAGE_DEFAULTS) {
    it(`counts zero ${name} when the reported usage stated none`, () => {
      // Arrange.
      const structured = completionWith({ usage: {} });

      // Act.
      const success = successOf(subagentConverter.settle(call({ prompt: "p" }), outcome(structured)));

      // Assert.
      expect(read(success.totals!.usage.value as conversationv1.TokenUsage)).toBe(0n);
    });
  }

  const TOOL_STAT_DEFAULTS: [string, (stats: conversationv1.AgentSubagentToolStats) => number][] = [
    ["reads", (stats) => stats.readCount],
    ["searches", (stats) => stats.searchCount],
    ["bash runs", (stats) => stats.bashCount],
    ["file edits", (stats) => stats.editFileCount],
    ["lines added", (stats) => stats.linesAdded],
    ["lines removed", (stats) => stats.linesRemoved],
    ["other tool calls", (stats) => stats.otherToolCount],
    ["frames", (stats) => stats.frameCount],
  ];

  for (const [name, read] of TOOL_STAT_DEFAULTS) {
    it(`counts zero ${name} when the reported tool stats stated none`, () => {
      // Arrange.
      const structured = completionWith({ toolStats: {} });

      // Act.
      const success = successOf(subagentConverter.settle(call({ prompt: "p" }), outcome(structured)));

      // Assert.
      expect(read(success.totals!.toolStats!)).toBe(0);
    });
  }
});

describe("whether a spawn's result is a launch receipt", () => {
  it.each(["async_launched", "remote_launched"])("is a receipt when the status is %s", (status) => {
    // Arrange / Act
    const receipt = isLaunchReceipt({ status, isAsync: true });

    // Assert
    expect(receipt).toBe(true);
  });

  it("is NOT a receipt when the run completed", () => {
    // Arrange / Act
    const receipt = isLaunchReceipt({ status: "completed" });

    // Assert
    expect(receipt).toBe(false);
  });

  it("is NOT a receipt when the result states no status", () => {
    // Arrange / Act
    const receipt = isLaunchReceipt({ content: [] });

    // Assert
    expect(receipt).toBe(false);
  });

  it("is NOT a receipt when there is no structured result at all", () => {
    // Arrange / Act
    const receipt = isLaunchReceipt(undefined);

    // Assert
    expect(receipt).toBe(false);
  });
});
