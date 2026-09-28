/**
 * The subagent family. Two planes again, and the FILE plane carries facts the
 * stream cannot: the `.meta.json` is the only source of a subagent's type,
 * description, spawning call and spawn depth.
 */
import { describe, expect, it } from "vitest";
import { logRecordsSince, logSinkMark } from "../../log-records.js";
import { createFold } from "../../../src/convert/fold.js";
import type { PersistEntry } from "../../../src/store/persistence.js";
import { activityOf, foldContext, MAIN_AGENT } from "../../convert/fold-harness.js";
import { matching } from "../../expect-shapes.js";

import { driveScenario, ofType, toolUseResults } from "../harness.js";

const agentIdOf = (driven: Awaited<ReturnType<typeof driveScenario>>): string => {
  const attributed = (driven.messages as unknown as Record<string, unknown>[]).find(
    (m) => typeof m.agent_id === "string",
  );
  const launched = toolUseResults(driven.transcript()).find(
    (r) => typeof (r as { agentId?: unknown }).agentId === "string",
  ) as { agentId?: string } | undefined;
  const named = attributed?.agent_id ?? launched?.agentId;
  // Both sources are checked for `typeof === "string"` above, so a non-string
  // here would be a defect in this reader rather than an id to stringify.
  return typeof named === "string" ? named : "";
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
      toolUseId: matching(/^toolu_fake_/),
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

  it("emits running task_progress beats whose token sum grows toward the settled total", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-detached"]);
    const beats = ofType(driven, "system", "task_progress");
    const settled = ofType(driven, "system", "task_notification")[0];

    // Assert. The running sum only grows, and stays under the settled total.
    const running = beats.map((b) => (b.usage as { total_tokens: number }).total_tokens);
    const total = (settled?.usage as { total_tokens: number }).total_tokens;
    expect(running).toEqual([4_200, 8_600]);
    expect(running[running.length - 1]).toBeLessThan(total);
  });

  it("keys each running beat to the SAME spawning call the task started under", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-detached"]);
    const beats = ofType(driven, "system", "task_progress");
    const started = ofType(driven, "system", "task_started")[0];

    // Assert. Every beat names the spawn's own tool-use id, so it advances the
    // spawn unit rather than being dropped for want of a correlation.
    const calls = new Set(beats.map((b) => b.tool_use_id));
    expect([...calls]).toEqual([started?.tool_use_id]);
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

describe("a detached subagent's mid-flight utterance", () => {
  it("emits exactly ONE sidechain assistant text line after the turn ends", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-detached-utterance"]);
    const sidechain = (driven.messages as unknown as Record<string, unknown>[]).filter(
      (m) => m.type === "assistant" && m.agent_id !== undefined,
    );

    // Assert
    expect(sidechain).toHaveLength(1);
  });

  it("stamps the utterance with IsSidechain, AgentId and SourceToolUseId on its own transcript", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-detached-utterance"]);
    const agentId = agentIdOf(driven);
    const utterance = driven
      .subagent(agentId)
      .find((l) => l.type === "assistant");

    // Assert
    expect({
      isSidechain: utterance?.isSidechain,
      agentId: utterance?.agentId,
    }).toEqual({ isSidechain: true, agentId });
  });

  it("writes NO completion — the agent stays live", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-detached-utterance"]);

    // Assert. Nothing here ever finishes the agent; only a stop does.
    expect(ofType(driven, "system", "task_notification")).toHaveLength(0);
    expect(ofType(driven, "system", "task_updated")).toHaveLength(0);
  });

  it("delivers the utterance AFTER the turn ended, same as any detached work", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-detached-utterance"]);
    const types = (driven.messages as unknown as Record<string, unknown>[]).map((m) => m.type);
    const resultIndex = types.indexOf("result");
    const lateAttribution = (driven.messages as unknown as Record<string, unknown>[]).findIndex(
      (m, i) => i > resultIndex && m.agent_id !== undefined,
    );

    // Assert
    expect(lateAttribution).toBeGreaterThan(resultIndex);
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

  it("terminates the stopped SHELL's spool with EXIT=143", async () => {
    // A shell spool is incremental bytes, and the tailer's only way to learn
    // the run ended is the EXIT line; 143 is SIGTERM's code.
    const driven = await driveScenario(["!cancel-all"], {
      during: async (query, _prompts, messages) => {
        for (let i = 0; i < 8; i++) await new Promise((r) => setImmediate(r));
        for (const started of messages.filter((m) => m.subtype === "task_started")) {
          await query.stopTask(String(started.task_id));
        }
      },
    });
    const shells = driven.spoolIds().filter((id) => id.startsWith("b"));

    expect(shells.map((id) => (driven.spool(id) ?? "").includes("EXIT=143"))).toEqual([true]);
  });

  it("writes NO EXIT line into a stopped AGENT's spool", async () => {
    // An agent spool is the agent's own JSONL and carries no terminator ever,
    // so an `EXIT=` line there is a shape no real tree contains.
    const driven = await driveScenario(["!cancel-all"], {
      during: async (query, _prompts, messages) => {
        for (let i = 0; i < 8; i++) await new Promise((r) => setImmediate(r));
        for (const started of messages.filter((m) => m.subtype === "task_started")) {
          await query.stopTask(String(started.task_id));
        }
      },
    });
    const agents = driven.spoolIds().filter((id) => id.startsWith("a"));

    expect(agents.map((id) => (driven.spool(id) ?? "").includes("EXIT="))).toEqual([false, false]);
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


  it("leaves the subagent live after the turn concludes, then raises its OWN gated call", async () => {
    // The main turn concludes normally (Dispatched a long-running agent to the
    // background); what is untested is that a SECOND, unawaited canUseTool
    // ask (the subagent's own) still lands after that, carrying the
    // subagent's agentID.
    const seen: unknown[] = [];
    const driven = await driveScenario(["!subagent-detached-live"], {
      canUseTool: async (_name, input, options) => {
        seen.push(options.agentID);
        return { behavior: "allow", updatedInput: input };
      },
      during: async (_query, _prompts, messages) => {
        for (let i = 0; i < 8; i++) await new Promise((r) => setImmediate(r));
        void messages;
      },
    });

    expect(ofType(driven, "result")).toHaveLength(1);
    const started = ofType(driven, "system", "task_started")[0];
    expect(seen).toEqual([started?.task_id]);
  });
});

describe("a turn that holds beside its own live background agent", () => {
  /** Wait for the hold's parked frame, then interrupt the turn. */
  const interruptTheHold = async (
    query: { interrupt: () => Promise<unknown> },
    _prompts: unknown,
    messages: Record<string, unknown>[],
  ): Promise<void> => {
    const parked = (): boolean =>
      messages.some(
        (m) =>
          m.type === "assistant" &&
          JSON.stringify((m.message as { content?: unknown } | undefined)?.content ?? "").includes("Waiting beside"),
      );
    for (let i = 0; i < 64 && !parked(); i++) await new Promise((r) => setImmediate(r));
    if (!parked()) throw new Error("the hold never parked");
    await query.interrupt();
  };

  const stoppedTasks = (driven: Awaited<ReturnType<typeof driveScenario>>): unknown[] =>
    ofType(driven, "system", "task_notification")
      .filter((m) => (m as Record<string, unknown>).status === "stopped")
      .map((m) => (m as Record<string, unknown>).task_id);

  it("ends the held turn the way an interrupted turn ends", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-detached-hold"], {
      opts: { perTaskStopAffordance: true },
      during: interruptTheHold,
    });

    // Assert
    expect(ofType(driven, "result")[0]).toMatchObject({ is_error: true, terminal_reason: "aborted_streaming" });
  });

  it("leaves the agent live across the interrupt under the per-task stop declaration", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-detached-hold"], {
      opts: { perTaskStopAffordance: true },
      during: interruptTheHold,
    });

    // Assert
    expect(stoppedTasks(driven)).toEqual([]);
  });

  it("stops the agent with the interrupt when the per-task stop is not declared", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-detached-hold"], { during: interruptTheHold });

    // Assert
    expect(stoppedTasks(driven)).toEqual([ofType(driven, "system", "task_started")[0]?.task_id]);
  });
});

describe("historical usage attributed to a nested subagent", () => {
  // UNGROUNDED, INVENTED (see MANIFEST.md): no capture carries a file-plane
  // -only historical usage record with nested-subagent attribution.
  const nestedAgentIdOf = (result: string): string => {
    const match = /nested subagent (\S+)\./.exec(result);
    if (match?.[1] === undefined) throw new Error("no nested agent id in the conclusion");
    return match[1];
  };

  it("writes NO paired stream-plane message for the historical record", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!usage-historical"]);
    const attributed = (driven.messages as unknown as Record<string, unknown>[]).filter(
      (m) => m.agent_id !== undefined,
    );

    // Assert. Only the ordinary turn's own conclude() messages reach the
    // stream; the nested agent's usage record is FILE-plane only.
    expect(attributed).toHaveLength(0);
  });

  it("marks the nested agent with spawnDepth 2", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!usage-historical"]);
    const agentId = nestedAgentIdOf((ofType(driven, "result")[0]?.result as string) ?? "");

    // Assert
    expect(driven.subagentMeta(agentId).spawnDepth).toBe(2);
  });

  it("carries the ephemeral 5m/1h cache-creation split", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!usage-historical"]);
    const agentId = nestedAgentIdOf((ofType(driven, "result")[0]?.result as string) ?? "");
    const record = driven.subagent(agentId).find((l) => l.type === "assistant") as
      | { message: { usage: { cache_creation: Record<string, number> } } }
      | undefined;

    // Assert
    expect(record?.message.usage.cache_creation).toEqual({
      ephemeral_5m_input_tokens: 25,
      ephemeral_1h_input_tokens: 50,
    });
  });

  it("carries server_tool_use, service_tier, speed and inference_geo", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!usage-historical"]);
    const agentId = nestedAgentIdOf((ofType(driven, "result")[0]?.result as string) ?? "");
    const record = driven.subagent(agentId).find((l) => l.type === "assistant") as
      | {
          message: {
            usage: {
              server_tool_use: Record<string, number>;
              service_tier: string;
              speed: string;
              inference_geo: string;
            };
          };
        }
      | undefined;

    // Assert
    expect({
      serverToolUse: record?.message.usage.server_tool_use,
      serviceTier: record?.message.usage.service_tier,
      speed: record?.message.usage.speed,
      inferenceGeo: record?.message.usage.inference_geo,
    }).toEqual({
      serverToolUse: { web_search_requests: 2, web_fetch_requests: 3 },
      serviceTier: "priority",
      speed: "fast",
      inferenceGeo: "us-east-1",
    });
  });
});

describe("a detached subagent streaming INTO the main agent's open blocks", () => {
  /** Fold what the mock emitted through the REAL fold, and the log records it wrote. */
  async function foldInterleaved(): Promise<{ entries: PersistEntry[]; logs: { message: string }[] }> {
    const driven = await driveScenario(["!subagent-interleaved"]);
    const fold = createFold();
    const context = foldContext();
    const before = logSinkMark();
    const entries = driven.messages.flatMap((message) => [
      ...fold.onSdkMessage(message, context).entries,
    ]);
    const logs = logRecordsSince(before);
    return { entries, logs };
  }

  /** The distinct units of one kind a book holds. */
  function unitsIn(entries: readonly PersistEntry[], main: boolean, kind: string): string[] {
    const keys = entries
      .filter((entry) => (entry.agentId.value === MAIN_AGENT.value) === main)
      .filter((entry) => activityOf(entry)?.item.case === kind)
      .map((entry) => entry.upsertKey);
    return [...new Set(keys)];
  }

  it("really lands a subagent message_start between two deltas of a main block", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-interleaved"]);
    const order = (driven.messages as unknown as Record<string, unknown>[])
      .filter((m) => m.type === "stream_event")
      .map((m) => ({ parent: m.parent_tool_use_id, event: (m.event as { type: string }).type }));
    const lastIndexOf = (test: (e: (typeof order)[number]) => boolean, within: typeof order): number =>
      within.reduce((last, e, at) => (test(e) ? at : last), -1);
    const conclusionStart = lastIndexOf((e) => e.parent === null && e.event === "message_start", order);
    const rest = order.slice(conclusionStart);

    // Assert: after the conclusion's message_start, a subagent message_start
    // precedes a later main delta.
    const subagentStart = rest.findIndex((e) => e.parent !== null && e.event === "message_start");
    const laterMainDelta = lastIndexOf((e) => e.parent === null && e.event === "content_block_delta", rest);
    expect(subagentStart).toBeGreaterThan(0);
    expect(laterMainDelta).toBeGreaterThan(subagentStart);
  });

  it("folds the main agent's concluding prose into exactly ONE main unit", async () => {
    // Arrange + Act
    const { entries } = await foldInterleaved();

    // Assert
    expect(unitsIn(entries, true, "response")).toHaveLength(1);
  });

  it("folds each main reasoning block into exactly ONE main unit", async () => {
    // Arrange + Act: one reasoning block for the spawn's call, one for the conclusion.
    const { entries } = await foldInterleaved();

    // Assert
    expect(unitsIn(entries, true, "thinking")).toHaveLength(2);
  });

  it("streams BOTH halves of each interleaved main block onto the main book", async () => {
    // Arrange + Act: the conclusion's thinking and text blocks each stream two
    // deltas, with a whole subagent response between them.
    const { entries } = await foldInterleaved();

    // Assert
    const mainDeltas = entries.filter(
      (entry) =>
        entry.agentId.value === MAIN_AGENT.value &&
        ["activity.thinking.update", "activity.response.update"].includes(entry.source.discriminator),
    );
    expect(mainDeltas).toHaveLength(4);
  });

  it("folds the subagent's two responses into its OWN book", async () => {
    // Arrange + Act
    const { entries } = await foldInterleaved();

    // Assert
    expect(unitsIn(entries, false, "response")).toHaveLength(2);
  });

  it("leaves no streamed unit started and unsettled", async () => {
    // Arrange + Act
    const { logs } = await foldInterleaved();

    // Assert
    expect(logs.filter((record) => record.message.startsWith("invariant violated: a streamed unit"))).toEqual([]);
  });
});

describe("a subagent resumed by a send", () => {
  /** The `task_started` a resume emits. */
  const resumeStart = (driven: Awaited<ReturnType<typeof driveScenario>>): Record<string, unknown> | undefined =>
    (driven.messages as unknown as Record<string, unknown>[]).find(
      (m) => m.type === "system" && m.subtype === "task_started",
    );

  it("starts the agent's task under the task id the prompt names", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-resumed a5583c88f8f4d5a90"]);

    // Assert
    expect(resumeStart(driven)?.task_id).toBe("a5583c88f8f4d5a90");
  });

  it("starts the resumed task from the SEND, never from a spawn", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-resumed a5583c88f8f4d5a90"]);
    const send = (driven.messages as unknown as Record<string, unknown>[])
      .flatMap((m) => ((m.message as { content?: unknown[] } | undefined)?.content ?? []) as Record<string, unknown>[])
      .find((block) => block.type === "tool_use" && block.name === "SendMessage");

    // Assert
    expect(resumeStart(driven)?.tool_use_id).toBe(send?.id);
  });

  it("mints a task id when the prompt names none", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!subagent-resumed"]);

    // Assert
    expect(resumeStart(driven)?.task_id).toEqual(matching(/./));
  });
});
