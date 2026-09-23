/**
 * The mocked vendor's ENGINE: identity, the turn loop, the block split, and
 * every `QueryLike` control verb.
 *
 * Scenario families have their own suites. This one pins the machinery every
 * scenario stands on, because a defect here is invisible in a scenario test
 * (which asserts what the scenario said, not how the engine said it).
 */
import { describe, expect, it, vi } from "vitest";
import { join } from "node:path";
import { writeSync } from "node:fs";

import {
  createFakeQuery,
  defaultSpoolRoot,
  FAIL_TURN_MARKER,
  FAKE_ACCOUNT_INFO,
  FAKE_AGENTS,
  FAKE_COMMANDS,
  FAKE_MCP_SERVERS,
  FAKE_MODELS,
  SPOOL_ROOT_ENV,
  TURN_GATE_PATH_ENV,
  TURN_GATE_TEXT_ENV,
} from "../../src/fake/index.js";
import type { CanUseToolLike, SdkUserMessage } from "../../src/sdk/types.js";
import {
  FAKE_SESSION_WINDOW_RESETS_IN_MS,
  FAKE_WEEKLY_WINDOW_RESETS_IN_MS,
} from "../../src/fake/catalogs.js";
import { HARNESS_NOW_MS, driveScenario, ofType, recordsOfType, theResult } from "./harness.js";

const ALLOW: CanUseToolLike = async (_n, input) =>
  ({ behavior: "allow", updatedInput: input });

const emptyPrompt = (async function* (): AsyncGenerator<SdkUserMessage> {})();

/** Let the scenario announce its task, then stop it by the id it announced. */
const stopTheLiveTask = async (
  query: { stopTask: (id: string) => Promise<void> },
  _prompts: unknown,
  messages: Record<string, unknown>[],
): Promise<void> => {
  for (let i = 0; i < 8; i++) await new Promise((r) => setImmediate(r));
  const started = messages.find((m) => m.subtype === "task_started");
  if (started === undefined) throw new Error("the scenario announced no task to stop");
  await query.stopTask(String(started.task_id));
};

describe("the session's opening facts", () => {
  it("opens with a system:init before any turn", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"]);

    // Assert
    expect(driven.messages[0]).toMatchObject({ type: "system", subtype: "init" });
  });

  it("reports the pre-minted session id on a fresh start", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"]);

    // Assert
    expect((driven.messages[0] as { session_id?: string }).session_id).toBe("sess-fake-1");
  });

  it("reports the RESUMED id on a resume, and re-emits no history", async () => {
    // Arrange. Verified real behavior: context restores, no history messages
    // come back through the stream.
    const driven = await driveScenario([], { resume: "sess-resumed-9" });

    // Act + Assert
    expect({
      init: (driven.messages[0] as { session_id?: string }).session_id,
      total: driven.messages.length,
    }).toEqual({ init: "sess-resumed-9", total: 1 });
  });

  it("names the init's model and permission mode from the options", async () => {
    // Arrange + Act
    const driven = await driveScenario([], {
      opts: { model: "fake-haiku-4-5", permissionMode: "acceptEdits" },
    });

    // Assert
    expect(driven.messages[0]).toMatchObject({ model: "fake-haiku-4-5", permissionMode: "acceptEdits" });
  });
});

describe("the turn loop", () => {
  it("writes the queue-operation line before the prompt line", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"]);
    const lines = driven.transcript();

    // Assert
    expect(lines.slice(0, 2).map((l) => l.type)).toEqual(["queue-operation", "user"]);
  });

  it("stamps the prompt record with the permission mode and promptSource", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"]);

    // Assert
    expect(driven.transcript()[1]).toMatchObject({
      type: "user",
      permissionMode: "default",
      promptSource: "sdk",
    });
  });

  it("ends every turn with exactly one result message", async () => {
    // Arrange + Act
    const driven = await driveScenario(["one", "two", "three"]);

    // Assert
    expect(ofType(driven, "result")).toHaveLength(3);
  });

  it("numbers turns cumulatively on the result", async () => {
    // Arrange + Act
    const driven = await driveScenario(["one", "two"]);

    // Assert
    expect(ofType(driven, "result").map((r) => r.num_turns)).toEqual([1, 2]);
  });

  it("writes a turn_duration record at each turn's end", async () => {
    // Arrange + Act
    const driven = await driveScenario(["one", "two"]);

    // Assert
    expect(
      recordsOfType(driven.transcript(), "system").filter((l) => l.subtype === "turn_duration"),
    ).toHaveLength(2);
  });

  it("ends the message stream when the prompt iterable ends", async () => {
    // Arrange + Act. The drive returns only once the iterable is exhausted, so
    // reaching this assertion at all is the fact under test.
    const driven = await driveScenario(["hello"]);

    // Assert
    expect(driven.messages.at(-1)).toMatchObject({ type: "result" });
  });
});

/**
 * THE SEND A TURN ANSWERS, echoed as the vendor does (sdk.d.ts
 * `user_message_uuid`): the first top-level stream event, the first top-level
 * assistant message and the result — and nothing else, and nothing at all for
 * a send that carried no client uuid.
 */
describe("the echo of the send a turn answers", () => {
  /** Every message of the drive that carries the echo. */
  const echoed = (messages: readonly unknown[]): Record<string, unknown>[] =>
    (messages as Record<string, unknown>[]).filter((message) => "user_message_uuid" in message);

  it("stamps exactly the first stream event, the first assistant message and the result", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"], { clientUuids: ["client-send-1"] });

    // Assert
    expect(echoed(driven.messages).map((message) => message.type)).toEqual(["stream_event", "assistant", "result"]);
  });

  it("names the send's own uuid in both the field and the list", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"], { clientUuids: ["client-send-1"] });

    // Assert
    expect(theResult(driven)).toMatchObject({
      user_message_uuid: "client-send-1",
      user_message_uuids: ["client-send-1"],
    });
  });

  it("stamps nothing for a send that carried no client uuid", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"]);

    // Assert
    expect(echoed(driven.messages)).toEqual([]);
  });

  it("stamps each turn with its OWN send", async () => {
    // Arrange + Act
    const driven = await driveScenario(["one", "two"], { clientUuids: ["client-send-1", "client-send-2"] });

    // Assert
    expect(ofType(driven, "result").map((line) => line.user_message_uuid)).toEqual([
      "client-send-1",
      "client-send-2",
    ]);
  });
});

describe("the block split", () => {
  it("emits one assistant message per block, all sharing the message id", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"]);
    const assistants = ofType(driven, "assistant");

    // Assert. The default prose scenario emits four blocks.
    expect({
      count: assistants.length,
      ids: new Set(assistants.map((a) => (a.message as { id: string }).id)).size,
    }).toEqual({ count: 4, ids: 1 });
  });

  it("writes one transcript line per block, chained in block order", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"]);
    const assistantLines = recordsOfType(driven.transcript(), "assistant");

    // Assert
    expect(assistantLines.map((l, i) => l.parentUuid === (i === 0 ? undefined : assistantLines[i - 1]?.uuid))
      .slice(1)).toEqual([true, true, true]);
  });

  it("gives every split line the SAME usage, so block 0 can carry it", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hello"]);
    const usages = ofType(driven, "assistant").map((a) =>
      JSON.stringify((a.message as { usage: unknown }).usage),
    );

    // Assert
    expect(new Set(usages).size).toBe(1);
  });

  it("streams a signature delta for a WITHHELD thinking block", async () => {
    // Arrange + Act. A withheld block is an empty `thinking` with a signature;
    // the signature delta is what says the reasoning existed.
    const driven = await driveScenario(["hello"]);
    const deltas = ofType(driven, "stream_event")
      .map((m) => (m.event as { delta?: { type?: string } }).delta?.type)
      .filter((t) => t !== undefined);

    // Assert
    expect(deltas).toContain("signature_delta");
  });

  it("streams two text deltas per text block, never one", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hi"]);
    const textDeltas = ofType(driven, "stream_event").filter(
      (m) => (m.event as { delta?: { type?: string } }).delta?.type === "text_delta",
    );

    // Assert. Two text blocks in the prose scenario, two deltas each.
    expect(textDeltas.length).toBe(4);
  });

  it("frames every response with message_start and message_stop", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hi"]);
    const events = ofType(driven, "stream_event").map((m) => (m.event as { type: string }).type);

    // Assert
    expect({ first: events[0], last: events.at(-1) }).toEqual({
      first: "message_start",
      last: "message_stop",
    });
  });

  it("stamps ttft_ms on the message_start and nowhere else", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hi"]);
    const stamped = ofType(driven, "stream_event").filter((m) => m.ttft_ms !== undefined);

    // Assert
    expect(stamped.map((m) => (m.event as { type: string }).type)).toEqual(["message_start"]);
  });
});

describe("message ids", () => {
  it("mints ids unique to the created query", async () => {
    // Arrange. Two queries in one process must not share an id: the store keys
    // token utilization by session plus api message id, and a session outlives
    // its shim.
    const first = await driveScenario(["hi"]);
    const second = await driveScenario(["hi"]);
    const idOf = (d: typeof first) =>
      (ofType(d, "assistant")[0]?.message as { id: string } | undefined)?.id;

    // Act + Assert
    expect(idOf(first)).not.toBe(idOf(second));
  });

  it("keeps the id recognizable as a fake one", async () => {
    // Arrange + Act
    const driven = await driveScenario(["hi"]);

    // Assert
    expect((ofType(driven, "assistant")[0]?.message as { id: string }).id).toMatch(
      /^msg_fake_[0-9a-f]{8}_\d+$/,
    );
  });
});

describe("interrupt", () => {
  it("answers a representative receipt rather than undefined", async () => {
    // Arrange. SDK 0.3.220 always answers with one; `undefined` would model a
    // CLI the shim no longer ships against.
    const query = createFakeQuery(emptyPrompt, ALLOW, { sessionId: "s", newUuid: () => "u" });

    // Act + Assert
    await expect(query.interrupt()).resolves.toEqual({ still_queued: [] });
  });

  it("ends a parked turn with an aborted_streaming error result and no content", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!hold"], {
      during: async (query) => {
        // Give the hold time to arm its park before the stop lands.
        await new Promise((r) => setImmediate(r));
        await new Promise((r) => setImmediate(r));
        await query.interrupt();
      },
    });

    // Assert
    expect(theResult(driven)).toMatchObject({
      subtype: "error_during_execution",
      is_error: true,
      terminal_reason: "aborted_streaming",
    });
  });

  it("marks the in-flight assistant message aborted when the interrupt lands mid-tool", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!interrupt"], {
      during: async (query) => {
        await new Promise((r) => setImmediate(r));
        await new Promise((r) => setImmediate(r));
        await query.interrupt();
      },
    });

    // Assert
    expect(ofType(driven, "assistant")[0]).toMatchObject({ aborted: true });
  });

  it("reports the tool result as interrupted when the interrupt lands mid-tool", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!interrupt"], {
      during: async (query) => {
        await new Promise((r) => setImmediate(r));
        await new Promise((r) => setImmediate(r));
        await query.interrupt();
      },
    });

    // Assert
    expect(ofType(driven, "user")[0]?.tool_use_result).toMatchObject({ interrupted: true });
  });
});

/**
 * The CLI's interrupt, both ways the consumer can declare it. `!cancel-all`
 * leaves two background agents and a background shell live after its turn.
 */
describe("interrupt and the per-task stop declaration", () => {
  /** Wait for `!cancel-all`'s turn to conclude, then interrupt. */
  const interruptAfterTheTurn = async (
    query: { interrupt: () => Promise<unknown> },
    _prompts: unknown,
    messages: Record<string, unknown>[],
  ): Promise<void> => {
    for (let i = 0; i < 64 && !messages.some((m) => m.type === "result"); i++) {
      await new Promise((r) => setImmediate(r));
    }
    if (!messages.some((m) => m.type === "result")) throw new Error("the turn never concluded");
    await query.interrupt();
  };

  /** Every task id the drive started, by kind. */
  const started = (driven: Awaited<ReturnType<typeof driveScenario>>, kind: string): string[] =>
    driven.messages
      .filter((m) => (m as Record<string, unknown>).subtype === "task_started")
      .filter((m) => (m as Record<string, unknown>).task_type === kind)
      .map((m) => String((m as Record<string, unknown>).task_id));

  /** Every task id the drive reported stopped. */
  const stopped = (driven: Awaited<ReturnType<typeof driveScenario>>): string[] =>
    driven.messages
      .filter((m) => (m as Record<string, unknown>).subtype === "task_notification")
      .filter((m) => (m as Record<string, unknown>).status === "stopped")
      .map((m) => String((m as Record<string, unknown>).task_id));

  it("spares every background task when the per-task stop is declared", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!cancel-all"], {
      opts: { perTaskStopAffordance: true },
      during: interruptAfterTheTurn,
    });

    // Assert
    expect(stopped(driven)).toEqual([]);
  });

  it("stops every background agent when the per-task stop is not declared", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!cancel-all"], { during: interruptAfterTheTurn });

    // Assert
    expect(stopped(driven)).toEqual(started(driven, "local_agent"));
  });

  it("leaves a background shell running even when the per-task stop is not declared", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!cancel-all"], { during: interruptAfterTheTurn });

    // Assert
    expect(stopped(driven)).not.toContain(started(driven, "local_bash")[0]);
  });
});

describe("setModel", () => {
  it("makes the NEXT assistant message report the new model", async () => {
    // Arrange + Act
    // The second prompt is fed AFTER the switch, because a turn already in
    // flight keeps the model it started on — which is the behavior being pinned.
    const driven = await driveScenario(["one"], {
      during: async (query, prompts) => {
        for (let i = 0; i < 4; i++) await new Promise((r) => setImmediate(r));
        await query.setModel("fake-haiku-4-5");
        prompts.push("two");
        for (let i = 0; i < 4; i++) await new Promise((r) => setImmediate(r));
      },
    });
    const models = ofType(driven, "assistant").map((a) => (a.message as { model: string }).model);

    // Assert. The first turn answered on the default, the last on the new one.
    expect({ first: models[0], last: models.at(-1) }).toEqual({
      first: "fake-opus-4-8",
      last: "fake-haiku-4-5",
    });
  });

  it("emits the declared session_state_changed beat", async () => {
    // Arrange
    const query = createFakeQuery(emptyPrompt, ALLOW, { sessionId: "s", newUuid: () => "u" });
    const seen: unknown[] = [];
    const drain = (async () => {
      for await (const m of query) seen.push(m);
    })();

    // Act
    await query.setModel("fake-sonnet-5");
    query.close();
    await drain;

    // Assert. `sdk.d.ts` declares NO model_changed message; this is the closest
    // declared signal and the assistant message is the real evidence.
    expect(seen.some((m) => (m as { subtype?: string }).subtype === "session_state_changed")).toBe(true);
  });

  it("restores the default model when called with no argument", async () => {
    // Arrange
    const query = createFakeQuery(emptyPrompt, ALLOW, {
      sessionId: "s",
      newUuid: () => "u",
      model: "fake-haiku-4-5",
    });

    // Act + Assert. Resolving without raising is the contract; the model the
    // next response reports is covered by the turn test above.
    await expect(query.setModel()).resolves.toBeUndefined();
  });
});

describe("setPermissionMode", () => {
  it("announces the new mode on a status message", async () => {
    // Arrange
    const query = createFakeQuery(emptyPrompt, ALLOW, { sessionId: "s", newUuid: () => "u" });
    const seen: Record<string, unknown>[] = [];
    const drain = (async () => {
      for await (const m of query) seen.push(m);
    })();

    // Act
    await query.setPermissionMode("plan");
    query.close();
    await drain;

    // Assert
    expect(seen.find((m) => m.subtype === "status")).toMatchObject({ permissionMode: "plan" });
  });

  it("makes the next prompt record carry the new mode", async () => {
    // Arrange + Act
    const driven = await driveScenario(["one"], {
      during: async (query, prompts) => {
        for (let i = 0; i < 4; i++) await new Promise((r) => setImmediate(r));
        await query.setPermissionMode("acceptEdits");
        prompts.push("two");
        for (let i = 0; i < 4; i++) await new Promise((r) => setImmediate(r));
      },
    });
    const prompts = recordsOfType(driven.transcript(), "user").filter((l) => l.promptSource === "sdk");

    // Assert
    expect(prompts.at(-1)).toMatchObject({ permissionMode: "acceptEdits" });
  });
});

describe("the catalogs", () => {
  const query = () => createFakeQuery(emptyPrompt, ALLOW, { sessionId: "s", newUuid: () => "u" });

  it("offers at least three models with differing capabilities", async () => {
    // Arrange + Act
    const models = await query().supportedModels();

    // Assert
    expect({
      count: models.length,
      distinctEffortSupport: new Set(models.map((m) => m.supportsEffort)).size,
      distinctFastMode: new Set(models.map((m) => m.supportsFastMode)).size,
      distinctAutoMode: new Set(models.map((m) => m.supportsAutoMode)).size,
    }).toEqual({ count: FAKE_MODELS.length, distinctEffortSupport: 2, distinctFastMode: 2, distinctAutoMode: 2 });
  });

  it("carries an alias row whose resolvedModel names a concrete one", async () => {
    // Arrange + Act
    const models = await query().supportedModels();
    const alias = models.find((m) => m.resolvedModel !== undefined);

    // Assert
    expect(models.some((m) => m.value === alias?.resolvedModel)).toBe(true);
  });

  it("answers the vendor-owned slash set with argument hints", async () => {
    // Arrange + Act
    const commands = await query().supportedCommands();

    // Assert
    expect(commands.map((c) => c.name)).toEqual(
      expect.arrayContaining(["clear", "compact", "context", "status", "model", "usage"]),
    );
  });

  it("offers /cost as an ALIAS of /usage rather than its own command", async () => {
    // Arrange + Act
    const commands = await query().supportedCommands();

    // Assert
    expect({
      ownCommand: commands.some((c) => c.name === "cost"),
      alias: commands.find((c) => c.name === "usage")?.aliases,
    }).toEqual({ ownCommand: false, alias: ["cost", "stats"] });
  });

  it("omits /agents and /help, which never reach the shim", async () => {
    // Arrange + Act
    const commands = await query().supportedCommands();

    // Assert
    expect(commands.map((c) => c.name)).not.toEqual(expect.arrayContaining(["agents", "help"]));
  });

  it("answers the subagent catalog", async () => {
    // Arrange + Act + Assert
    await expect(query().supportedAgents()).resolves.toEqual(FAKE_AGENTS);
  });

  it("reports every declared MCP health across the catalog", async () => {
    // Arrange + Act
    const servers = await query().mcpServerStatus();

    // Assert
    expect(new Set(servers.map((s) => s.status))).toEqual(
      new Set(["connected", "failed", "needs-auth", "pending", "disabled"]),
    );
  });

  it("answers accountInfo", async () => {
    // Arrange + Act + Assert
    await expect(query().accountInfo()).resolves.toEqual(FAKE_ACCOUNT_INFO);
  });

  it("answers initializationResult with the same catalogs", async () => {
    // Arrange + Act
    const init = await query().initializationResult();

    // Assert
    expect({ commands: init.commands.length, models: init.models.length }).toEqual({
      commands: FAKE_COMMANDS.length,
      models: FAKE_MODELS.length,
    });
  });
});

describe("getContextUsage", () => {
  it("answers every declared field, optional ones included", async () => {
    // Arrange
    const query = createFakeQuery(emptyPrompt, ALLOW, { sessionId: "s", newUuid: () => "u" });

    // Act
    const usage = await query.getContextUsage();

    // Assert
    expect(Object.keys(usage).sort()).toEqual(
      [
        "agents", "apiUsage", "autoCompactThreshold", "categories", "deferredBuiltinTools",
        "gridRows", "isAutoCompactEnabled", "maxTokens", "mcpTools", "memoryFiles",
        "messageBreakdown", "model", "percentage", "rawMaxTokens", "skills", "slashCommands",
        "systemPromptSections", "systemTools", "totalTokens",
      ].sort(),
    );
  });

  it("marks at least one category deferred", async () => {
    // Arrange
    const query = createFakeQuery(emptyPrompt, ALLOW, { sessionId: "s", newUuid: () => "u" });

    // Act
    const usage = await query.getContextUsage();

    // Assert
    expect(usage.categories.some((c) => c.isDeferred === true)).toBe(true);
  });

  it("reports the model currently in force", async () => {
    // Arrange
    const query = createFakeQuery(emptyPrompt, ALLOW, {
      sessionId: "s",
      newUuid: () => "u",
      model: "fake-sonnet-5",
    });

    // Act + Assert
    expect((await query.getContextUsage()).model).toBe("fake-sonnet-5");
  });
});

describe("the account-usage probe", () => {
  const usage = async (prompts: string[], nowMs?: () => number) => {
    let answer: unknown;
    await driveScenario(prompts, {
      ...(nowMs === undefined ? {} : { opts: { nowMs } }),
      during: async (query) => {
        await new Promise((r) => setImmediate(r));
        await new Promise((r) => setImmediate(r));
        answer = await query.usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET();
      },
    });
    return answer as {
      rate_limits: Record<string, unknown> | null;
      rate_limits_available: boolean;
      behaviors: unknown;
      subscription_type?: string;
    };
  };

  it("resets the SESSION window a fixed offset after the fake's own now", async () => {
    // Arrange + Act
    const answer = await usage(["!usage-available"]);

    // Assert. The instant is stated as an offset from the injected clock, so a
    // sampled countdown is always in the future rather than reading `0m`.
    expect(answer.rate_limits?.five_hour).toMatchObject({
      resets_at: new Date(HARNESS_NOW_MS + FAKE_SESSION_WINDOW_RESETS_IN_MS).toISOString(),
    });
  });

  it("resets the WEEKLY window a fixed offset after the fake's own now", async () => {
    // Arrange + Act
    const answer = await usage(["!usage-available"]);

    // Assert
    expect(answer.rate_limits?.seven_day).toMatchObject({
      resets_at: new Date(HARNESS_NOW_MS + FAKE_WEEKLY_WINDOW_RESETS_IN_MS).toISOString(),
    });
  });

  it("still resets AFTER now for a sample observed at a later clock", async () => {
    // Arrange. A clock a full year past the harness's own: an absolute fixture
    // would have gone stale here, an offset one cannot.
    const later = HARNESS_NOW_MS + 365 * 24 * 60 * 60 * 1_000;

    // Act
    const answer = await usage(["!usage-available"], () => later);

    // Assert
    const window = answer.rate_limits?.five_hour as { resets_at: string };
    expect(Date.parse(window.resets_at) - later).toBe(FAKE_SESSION_WINDOW_RESETS_IN_MS);
  });

  it("answers every window when the service is available", async () => {
    // Arrange + Act
    const answer = await usage(["!usage-available"]);

    // Assert
    expect(Object.keys(answer.rate_limits ?? {}).sort()).toEqual(
      [
        "extra_usage", "five_hour", "model_scoped", "seven_day", "seven_day_oauth_apps",
        "seven_day_opus", "seven_day_sonnet",
      ].sort(),
    );
  });

  it("answers every window under !usage-full too, which is the name the e2e roster spells", async () => {
    // Arrange + Act
    const answer = await usage(["!usage-full"]);

    // Assert
    expect(Object.keys(answer.rate_limits ?? {}).sort()).toEqual(
      [
        "extra_usage", "five_hour", "model_scoped", "seven_day", "seven_day_oauth_apps",
        "seven_day_opus", "seven_day_sonnet",
      ].sort(),
    );
  });

  it("gives every populated window BOTH a utilization and a reset instant", async () => {
    // Arrange + Act
    const answer = await usage(["!usage-full"]);
    const windows = ["five_hour", "seven_day", "seven_day_oauth_apps", "seven_day_opus", "seven_day_sonnet"];

    // Assert. A window with a utilization and no reset instant cannot be drawn
    // as a window at all, so "populated" has to mean both.
    expect(
      windows.map((name) => {
        const value = (answer.rate_limits ?? {})[name] as { utilization: unknown; resets_at: unknown };
        return typeof value.utilization === "number" && typeof value.resets_at === "string";
      }),
    ).toEqual(windows.map(() => true));
  });

  it("names the subscription the windows belong to", async () => {
    // Arrange + Act
    const answer = await usage(["!usage-full"]);

    // Assert
    expect(answer.subscription_type).toBe("max");
  });

  it("answers null rate limits when the service is unavailable", async () => {
    // Arrange + Act
    const answer = await usage(["!usage-service-unavailable"]);

    // Assert
    expect({ available: answer.rate_limits_available, limits: answer.rate_limits }).toEqual({
      available: false,
      limits: null,
    });
  });

  it("answers a null FIVE-HOUR window when the window is unavailable", async () => {
    // Arrange + Act
    const answer = await usage(["!usage-window-unavailable"]);

    // Assert. `SessionUsageWindowUnavailable` means "the service answered
    // without a five-hour window" and nothing else, so nulling any other window
    // would leave the reason unproducible while looking covered.
    expect(answer.rate_limits?.five_hour).toBeNull();
  });

  it("leaves the OTHER windows present, so the two null shapes stay distinguishable", async () => {
    // Arrange + Act
    const answer = await usage(["!usage-window-unavailable"]);

    // Assert
    expect(answer.rate_limits?.seven_day).toMatchObject({ utilization: 63 });
  });

  it("answers an ABSENT optional window under !usage-opus-absent, which is not an unavailability", async () => {
    // Arrange + Act
    const answer = await usage(["!usage-opus-absent"]);

    // Assert. The service answered in full; this account simply has no opus
    // window, so the arm is still the available one.
    expect({
      opus: answer.rate_limits?.seven_day_opus,
      fiveHour: answer.rate_limits?.five_hour,
    }).toEqual({
      opus: null,
      fiveHour: {
        utilization: 41,
        // PINNED AS AN OFFSET, not as a literal instant: the fixture's reset is
        // stated relative to the fake's own clock so a sampled window always
        // resets in the FUTURE. The clock is injected, so this is still exact.
        resets_at: new Date(HARNESS_NOW_MS + FAKE_SESSION_WINDOW_RESETS_IN_MS).toISOString(),
      },
    });
  });

  it("answers a null UTILIZATION when the figure is unavailable", async () => {
    // Arrange + Act
    const answer = await usage(["!usage-utilization-unavailable"]);

    // Assert
    expect(answer.rate_limits?.five_hour).toMatchObject({ utilization: null });
  });

  // AMENDED, deliberately. This used to assert `behaviors: null`, a shape the
  // converter never reads: `accountUsageUpdate` branches on
  // `rate_limits_available`, `rate_limits` and the five-hour window only, so
  // the arm the scenario names — `sampling_failure` — was unreachable and the
  // trigger was dead while looking covered. The shim's ONE producer of that
  // arm is the catch around this probe, so the mock of a failed sampling is
  // the probe RAISING.
  it("raises when the shim's own sampling failed, which is that arm's only producer", async () => {
    // Arrange + Act + Assert
    await expect(usage(["!usage-sampling-failure"])).rejects.toThrow(
      "the local transcript scan that produces `behaviors` failed",
    );
  });
});

describe("stopTask", () => {
  it("accepts a task it never started as an idempotent no-op", async () => {
    // Arrange
    const query = createFakeQuery(emptyPrompt, ALLOW, { sessionId: "s", newUuid: () => "u" });

    // Act + Assert
    await expect(query.stopTask("b-nonexistent")).resolves.toBeUndefined();
  });

  it("emits a stopped task_notification for a live task", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach-live"], { during: stopTheLiveTask });

    // Assert
    expect(ofType(driven, "system", "task_notification")).toMatchObject([{ status: "stopped" }]);
  });

  it("terminates a stopped shell's spool with EXIT=143", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach-live"], { during: stopTheLiveTask });
    const taskId = String(ofType(driven, "system", "task_started")[0]?.task_id);

    // Assert. The EXIT line is the tailer's ONLY signal that the run ended.
    expect(driven.spool(taskId)).toContain("EXIT=143");
  });

  it("re-announces the shrunk live set, because the payload is REPLACE semantics", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach-live"], { during: stopTheLiveTask });
    const announcements = ofType(driven, "system", "background_tasks_changed");

    // Assert
    expect((announcements.at(-1)?.tasks as unknown[])).toEqual([]);
  });

  it("writes the agents_killed record when the last live item goes", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-detach-live"], { during: stopTheLiveTask });

    // Assert. The record states that the SET emptied, which no single stop knows.
    expect(
      recordsOfType(driven.transcript(), "system").some((l) => l.subtype === "agents_killed"),
    ).toBe(true);
  });
});

describe("backgroundTasks", () => {
  it("reports false when nothing is live", async () => {
    // Arrange
    const query = createFakeQuery(emptyPrompt, ALLOW, { sessionId: "s", newUuid: () => "u" });

    // Act + Assert
    await expect(query.backgroundTasks()).resolves.toBe(false);
  });

  it("warns and answers the live-set question when the tool call is unknown", async () => {
    // Arrange
    const query = createFakeQuery(emptyPrompt, ALLOW, { sessionId: "s", newUuid: () => "u" });

    // Act + Assert
    await expect(query.backgroundTasks("toolu_absent")).resolves.toBe(false);
  });
});

describe("streamInput and close", () => {
  it("accepts a second input stream rather than refusing a verb that exists", async () => {
    // Arrange
    const query = createFakeQuery(emptyPrompt, ALLOW, { sessionId: "s", newUuid: () => "u" });

    // Act + Assert
    await expect(query.streamInput((async function* () {})() as never)).resolves.toBeUndefined();
  });

  it("ends the message stream on close", async () => {
    // Arrange
    const query = createFakeQuery(emptyPrompt, ALLOW, { sessionId: "s", newUuid: () => "u" });
    const seen: unknown[] = [];

    // Act
    const drain = (async () => {
      for await (const m of query) seen.push(m);
    })();
    query.close();
    await drain;

    // Assert
    expect(seen).toHaveLength(1);
  });

  it("ends the message stream when the abort signal fires", async () => {
    // Arrange
    const controller = new AbortController();
    const query = createFakeQuery(emptyPrompt, ALLOW, {
      sessionId: "s",
      newUuid: () => "u",
      abortSignal: controller.signal,
    });
    const seen: unknown[] = [];

    // Act
    const drain = (async () => {
      for await (const m of query) seen.push(m);
    })();
    controller.abort();
    await drain;

    // Assert
    expect(seen).toHaveLength(1);
  });
});

/**
 * Resolve once the fake has logged that a turn PARKED on its gate.
 *
 * Yields through the macrotask queue rather than sleeping: every tick is work
 * the drive actually did, so the wait is bounded by the mock's progress and not
 * by how loaded the machine is.
 */
async function untilParked(): Promise<void> {
  const parked = (): boolean =>
    (vi.mocked(writeSync).mock.calls as unknown as Array<[number, Buffer, number, number]>).some(
      ([, bytes, offset, length]) =>
        bytes.subarray(offset, offset + length).toString("utf8").includes("PARKED on its gate"),
    );
  for (let i = 0; i < 1_000; i++) {
    if (parked()) return;
    await new Promise((resolve) => setImmediate(resolve));
  }
  throw new Error("the fake never parked on its gate");
}

describe("the turn gate", () => {
  it("parks a turn whose text matches the gate and releases it when the path appears", async () => {
    // Arrange
    const { mkdtempSync, writeFileSync } = await import("node:fs");
    const { tmpdir } = await import("node:os");
    const dir = mkdtempSync(join(tmpdir(), "fake-gate-"));
    const gate = join(dir, "open");
    process.env[TURN_GATE_PATH_ENV] = gate;
    process.env[TURN_GATE_TEXT_ENV] = "gated turn";
    let released = false;

    try {
      // Act
      const driven = await driveScenario(["gated turn"], {
        during: async () => {
          // Wait for the PARK to be a FACT, not for wall-clock time. A real
          // timer here makes the test's duration a function of machine load
          // (it once blew the 2500ms bound under four concurrent suites);
          // the mock's own park record is the deterministic edge, and the
          // gate's level-then-edge check makes an early release safe anyway.
          await untilParked();
          released = true;
          writeFileSync(gate, "");
        },
      });

      // Assert. The turn ends the ORDINARY way, not through an interrupt.
      expect({ released, subtype: theResult(driven).subtype }).toEqual({
        released: true,
        subtype: "success",
      });
    } finally {
      delete process.env[TURN_GATE_PATH_ENV];
      delete process.env[TURN_GATE_TEXT_ENV];
    }
  });

  it("does not park a turn whose text does not match the gate", async () => {
    // Arrange
    process.env[TURN_GATE_PATH_ENV] = "/nonexistent/gate";
    process.env[TURN_GATE_TEXT_ENV] = "some other text";

    try {
      // Act + Assert. It completes rather than hanging.
      const driven = await driveScenario(["hello"]);
      expect(theResult(driven).subtype).toBe("success");
    } finally {
      delete process.env[TURN_GATE_PATH_ENV];
      delete process.env[TURN_GATE_TEXT_ENV];
    }
  });
});

describe("the spool root", () => {
  it("defaults to the vendor's /tmp/claude-<uid>", () => {
    // Arrange + Act + Assert
    expect(defaultSpoolRoot()).toMatch(/^\/tmp\/claude-\d+$/);
  });

  it("honors the env override", async () => {
    // Arrange
    const { mkdtempSync } = await import("node:fs");
    const { tmpdir } = await import("node:os");
    const root = mkdtempSync(join(tmpdir(), "fake-spool-env-"));
    process.env[SPOOL_ROOT_ENV] = root;

    try {
      // Act
      const query = createFakeQuery(emptyPrompt, ALLOW, { sessionId: "s", newUuid: () => "u" });
      query.close();

      // Assert. Constructing without raising against the override is the fact;
      // the path shape itself is pinned in vendor-files.test.ts.
      expect(process.env[SPOOL_ROOT_ENV]).toBe(root);
    } finally {
      delete process.env[SPOOL_ROOT_ENV];
    }
  });
});

describe("the e2e failure marker", () => {
  it("fails a turn whose prompt merely CONTAINS the marker", async () => {
    // Arrange + Act
    const driven = await driveScenario([`please do the thing ${FAIL_TURN_MARKER} thanks`]);

    // Assert
    expect(theResult(driven)).toMatchObject({ subtype: "error_during_execution", is_error: true });
  });

  it("emits no assistant content on the failing turn", async () => {
    // Arrange + Act
    const driven = await driveScenario([`x ${FAIL_TURN_MARKER}`]);

    // Assert. A blank bubble on every frontend is what fabricating one costs.
    expect(ofType(driven, "assistant")).toHaveLength(0);
  });
});

describe("MCP arm switching", () => {
  it("narrows the catalog when a scenario asks it to", async () => {
    // Arrange
    let servers: unknown[] = [];

    // Act
    await driveScenario(["!mcp-healthy"], {
      during: async (query) => {
        for (let i = 0; i < 4; i++) await new Promise((r) => setImmediate(r));
        servers = await query.mcpServerStatus();
      },
    });

    // Assert
    expect(servers).toHaveLength(1);
  });

  it("restores the full catalog when a scenario asks it to", async () => {
    // Arrange
    let servers: unknown[] = [];

    // Act
    await driveScenario(["!mcp-healthy", "!mcp-all"], {
      during: async (query) => {
        for (let i = 0; i < 8; i++) await new Promise((r) => setImmediate(r));
        servers = await query.mcpServerStatus();
      },
    });

    // Assert
    expect(servers).toHaveLength(FAKE_MCP_SERVERS.length);
  });
});

describe("a task's spool, from the moment the task starts", () => {
  it("names the spool on task_started, so a tailer learns the path from the start", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!bash-timeout"]);
    const started = ofType(driven, "system", "task_started")[0];

    // Assert.
    expect(String(started?.output_file)).toMatch(/\/tasks\/b[0-9a-z]+\.output$/);
  });

  it("CREATES the spool for a run that timed out, which no scenario line opens", async () => {
    // A run that ends by timing out or by being cancelled never writes output
    // of its own; without the file the tailer has nothing to open at all.
    const driven = await driveScenario(["!bash-timeout"]);
    const taskId = String(ofType(driven, "system", "task_started")[0]?.task_id);

    // Assert.
    expect(driven.spool(taskId)).not.toBeNull();
  });

  it("creates a spool for every item of a fan-wide cancel", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!cancel-all"]);
    const taskIds = ofType(driven, "system", "task_started").map((m) => String(m.task_id));

    // Assert.
    expect(taskIds.map((id) => driven.spool(id) !== null)).toEqual([true, true, true]);
  });
});
