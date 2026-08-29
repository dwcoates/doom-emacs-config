/**
 * The subagent family. Two planes again, and the FILE plane carries facts the
 * stream cannot: the `.meta.json` is the only source of a subagent's type,
 * description, spawning call and spawn depth.
 */
import { describe, expect, it } from "vitest";

import { driveScenario, ofType, toolUseResults } from "../harness.js";

const agentIdOf = (driven: Awaited<ReturnType<typeof driveScenario>>): string => {
  const attributed = (driven.messages as unknown as Record<string, unknown>[]).find(
    (m) => typeof m.agent_id === "string",
  );
  const launched = toolUseResults(driven.transcript()).find(
    (r) => typeof (r as { agentId?: unknown }).agentId === "string",
  ) as { agentId?: string } | undefined;
  return String(attributed?.agent_id ?? launched?.agentId ?? "");
};

describe("a synchronous subagent", () => {
  it("writes the four-field meta sidecar the stream cannot carry", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent Explore the module"]);
    const agentId = agentIdOf(driven);

    // Assert
    expect(driven.subagentMeta(agentId)).toEqual({
      agentType: "general-purpose",
      description: "Explore the module",
      toolUseId: expect.stringMatching(/^toolu_fake_/),
      spawnDepth: 1,
    });
  });

  it("records the agent's commission as the first user line of its own transcript", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent"]);
    const lines = driven.subagent(agentIdOf(driven));

    // Assert. A workflow or subagent's prompt exists nowhere else.
    expect({ type: lines[0]?.type, sidechain: lines[0]?.isSidechain }).toEqual({
      type: "user",
      sidechain: true,
    });
  });

  it("attributes the subagent's stream messages by agent_id and parent_tool_use_id", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent"]);
    const attributed = (driven.messages as unknown as Record<string, unknown>[]).filter(
      (m) => m.agent_id !== undefined,
    );

    // Assert. Without forwardSubagentText none of these reach the shim at all.
    expect(attributed.every((m) => typeof m.parent_tool_use_id === "string")).toBe(true);
  });

  it("carries the subagent's type and task description on its messages", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent Explore the module"]);
    const first = (driven.messages as unknown as Record<string, unknown>[]).find(
      (m) => m.agent_id !== undefined,
    );

    // Assert
    expect({ type: first?.subagent_type, description: first?.task_description }).toEqual({
      type: "general-purpose",
      description: "Explore the module",
    });
  });

  it("keeps the subagent's nested tool round in ITS transcript, not the main one", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent"]);
    const nestedResults = driven.subagent(agentIdOf(driven)).filter((l) => l.toolUseResult !== undefined);

    // Assert
    expect(nestedResults).toHaveLength(1);
  });

  it("reports the subagent's model, which differs from the session's", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent"]);
    const first = (driven.messages as unknown as Record<string, unknown>[]).find(
      (m) => m.agent_id !== undefined && m.type === "assistant",
    );

    // Assert
    expect((first?.message as { model: string }).model).toBe("fake-sonnet-5");
  });

  it("answers the Agent call with a completed output carrying FULL usage", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent"]);
    const output = toolUseResults(driven.transcript()).find(
      (r) => (r as { status?: string }).status === "completed",
    ) as Record<string, unknown>;

    // Assert. A sync subagent reports per-field usage; only a DETACHED one is
    // reduced to three totals.
    expect({
      hasUsage: typeof output.usage === "object",
      hasToolStats: typeof output.toolStats === "object",
    }).toEqual({ hasUsage: true, hasToolStats: true });
  });
});

describe("a detached subagent", () => {
  it("answers the launch with async_launched and the output file, not a report", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-detached"]);
    const output = toolUseResults(driven.transcript())[0] as Record<string, unknown>;

    // Assert
    expect({ status: output.status, isAsync: output.isAsync, canRead: output.canReadOutputFile }).toEqual({
      status: "async_launched",
      isAsync: true,
      canRead: true,
    });
  });

  it("names the spool with an a-prefixed 17-character agent id", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-detached"]);

    // Assert
    expect(driven.spoolIds()).toEqual([expect.stringMatching(/^a[a-f0-9]{16}$/)]);
  });

  it("writes the spool as AGENT JSONL, not prose and not an EXIT-terminated file", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-detached"]);
    const spool = driven.spool(agentIdOf(driven)) ?? "";

    // Assert
    expect({
      parses: spool.trim().split("\n").every((l) => JSON.parse(l) !== null),
      terminated: spool.includes("EXIT="),
    }).toEqual({ parses: true, terminated: false });
  });

  it("uses the agent id as the task id, so all three addressings agree", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-detached"]);

    // Assert
    expect(String(ofType(driven, "system", "task_started")[0]?.task_id)).toBe(agentIdOf(driven));
  });

  it("reports only THREE totals as usage on the completion notification", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-detached"]);
    const notification = ofType(driven, "system", "task_notification")[0];

    // Assert. `AgentSubagentAsyncUsage` exists because this is all there is.
    expect(Object.keys(notification?.usage as object).sort()).toEqual(
      ["duration_ms", "tool_uses", "total_tokens"].sort(),
    );
  });

  it("delivers the agent's own work AFTER the turn ended", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-detached"]);
    const types = (driven.messages as unknown as Record<string, unknown>[]).map((m) => m.type);
    const resultIndex = types.indexOf("result");
    const lateAttribution = (driven.messages as unknown as Record<string, unknown>[]).findIndex(
      (m, i) => i > resultIndex && m.agent_id !== undefined,
    );

    // Assert. That the work outlives the turn is what "detached" means.
    expect(lateAttribution).toBeGreaterThan(resultIndex);
  });

  it("reports a failing agent through both task_updated and the notification", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-failed"]);

    // Assert
    expect({
      updated: (ofType(driven, "system", "task_updated")[0]?.patch as { status: string }).status,
      notified: ofType(driven, "system", "task_notification")[0]?.status,
    }).toEqual({ updated: "failed", notified: "failed" });
  });
});

describe("the fan-wide cancel setup", () => {
  it("leaves THREE live items — two agents and a shell", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!cancel-all"]);
    const kinds = ofType(driven, "system", "task_started").map((m) => m.task_type);

    // Assert
    expect(kinds).toEqual(["local_agent", "local_agent", "local_bash"]);
  });

  it("writes a meta sidecar for each agent", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!cancel-all"]);
    const agentIds = ofType(driven, "system", "task_started")
      .filter((m) => m.task_type === "local_agent")
      .map((m) => String(m.task_id));

    // Assert
    expect(agentIds.map((id) => driven.subagentMeta(id).agentType)).toEqual([
      "general-purpose",
      "general-purpose",
    ]);
  });

  it("writes NO agents_killed record while the items are still live", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!cancel-all"]);

    // Assert. The record states the SET emptied; nothing has emptied yet.
    expect(driven.transcript().some((l) => l.subtype === "agents_killed")).toBe(false);
  });

  it("writes agents_killed once the LAST live item is stopped", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!cancel-all"], {
      during: async (query, _prompts, messages) => {
        for (let i = 0; i < 8; i++) await new Promise((r) => setImmediate(r));
        for (const started of messages.filter((m) => m.subtype === "task_started")) {
          await query.stopTask(String(started.task_id));
        }
      },
    });

    // Assert
    expect(driven.transcript().filter((l) => l.subtype === "agents_killed")).toHaveLength(1);
  });

  it("stops every live item, leaving the announced set empty", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!cancel-all"], {
      during: async (query, _prompts, messages) => {
        for (let i = 0; i < 8; i++) await new Promise((r) => setImmediate(r));
        for (const started of messages.filter((m) => m.subtype === "task_started")) {
          await query.stopTask(String(started.task_id));
        }
      },
    });
    const announcements = ofType(driven, "system", "background_tasks_changed");

    // Assert
    expect((announcements.at(-1)?.tasks as unknown[])).toEqual([]);
  });
});
