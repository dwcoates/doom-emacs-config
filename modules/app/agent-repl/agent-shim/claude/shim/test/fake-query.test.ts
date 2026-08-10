import { describe, expect, it, vi } from "vitest";
import { writeSync } from "node:fs";
import { FAIL_TURN_MARKER, FAKE_MODELS, MARKDOWN_SHOWCASE, createFakeQuery } from "../src/fake-query.js";
import { AsyncQueue } from "../src/input-queue.js";
import {
  CanUseToolLike,
  PermissionResultLike,
  SdkMessageLike,
  SdkUserMessageLike,
} from "../src/session.js";

function persistedLogs(): Array<Record<string, unknown>> {
  const calls = vi.mocked(writeSync).mock.calls as unknown as Array<[number, Buffer, number, number]>;
  return calls.map(([, bytes, offset, length]) =>
    JSON.parse(bytes.subarray(offset, offset + length).toString("utf8")) as Record<string, unknown>,
  );
}

function userMsg(text: string): SdkUserMessageLike {
  return {
    type: "user",
    message: { role: "user", content: [{ type: "text", text }] },
    parent_tool_use_id: null,
    session_id: "s",
  };
}

interface FakeHarness {
  input: AsyncQueue<SdkUserMessageLike>;
  collect: (n?: number) => Promise<SdkMessageLike[]>;
  query: ReturnType<typeof createFakeQuery>;
  permissionRequests: Array<{ toolName: string; input: Record<string, unknown> }>;
}

function makeFake(decision: PermissionResultLike = { behavior: "allow", updatedInput: {} }): FakeHarness {
  const input = new AsyncQueue<SdkUserMessageLike>();
  const permissionRequests: FakeHarness["permissionRequests"] = [];
  const canUseTool: CanUseToolLike = async (toolName, toolInput) => {
    permissionRequests.push({ toolName, input: toolInput });
    return decision;
  };
  let uuidCounter = 0;
  const query = createFakeQuery(input, canUseTool, {
    sessionId: "s",
    newUuid: () => `u${++uuidCounter}`,
  });
  const collect = async (): Promise<SdkMessageLike[]> => {
    const out: SdkMessageLike[] = [];
    for await (const m of query) out.push(m);
    return out;
  };
  return { input, collect, query, permissionRequests };
}

/** The api message id carried by a collected turn's assistant message. */
function assistantMessageID(msgs: SdkMessageLike[]): string {
  const assistant = msgs.find((m) => m.type === "assistant") as { message?: { id?: string } } | undefined;
  const id = assistant?.message?.id;
  if (id === undefined) throw new Error("the collected turn carried no assistant message id");
  return id;
}

describe("createFakeQuery", () => {
  it("emits a system init message first", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.end();
    const msgs = await h.collect();
    // Assert
    expect(msgs[0]).toMatchObject({ type: "system", subtype: "init", model: "fake-model" });
  });

  it("reports the resumed session uuid in init when resuming", async () => {
    // Arrange — resume continuation mirrors verified real SDK behavior.
    const input = new AsyncQueue<SdkUserMessageLike>();
    const query = createFakeQuery(input, async () => ({ behavior: "allow", updatedInput: {} }), {
      sessionId: "s-new",
      newUuid: () => "u1",
      resume: "cli-uuid-resumed",
    });
    // Act
    input.end();
    const msgs: SdkMessageLike[] = [];
    for await (const m of query) msgs.push(m);
    // Assert — init carries the RESUMED uuid, not the shim-assigned id.
    expect(msgs[0]).toMatchObject({ type: "system", subtype: "init", session_id: "cli-uuid-resumed" });
  });

  it("streams an echoed text turn ending in a success result", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("hello"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const types = msgs.map((m) => m.type);
    expect(types).toContain("stream_event");
    expect(types).toContain("assistant");
    expect(msgs.find((m) => m.type === "stream_event" && (m["event"] as { type?: string } | undefined)?.type === "message_start"))
      .toMatchObject({ ttft_ms: 1 });
    const result = msgs.find((m) => m.type === "result")!;
    expect(result).toMatchObject({ subtype: "success", result: expect.stringContaining("hello") });
  });

  // THE API MESSAGE ID IS UNIQUE PER SPAWN, not merely per turn.
  //
  // The daemon keys token utilization by session PLUS api_message_id, and a
  // session outlives its shim: a revival respawns the fake query, whose first
  // turn used to re-mint `msg_fake_1` under the same session. The store
  // correctly refused the divergent duplicate, the shim link was declared lost,
  // and the controller was torn down — so this is a collision that costs the
  // whole session, not a cosmetic id clash.
  it("mints a different api message id for the same turn of a respawned query", async () => {
    // Arrange — two queries in one process, standing in for a respawn.
    const first = makeFake();
    const second = makeFake();
    // Act
    first.input.push(userMsg("hello"));
    first.input.end();
    second.input.push(userMsg("hello"));
    second.input.end();
    const firstID = assistantMessageID(await first.collect());
    const secondID = assistantMessageID(await second.collect());
    // Assert
    expect(secondID).not.toBe(firstID);
  });

  // The SHAPE stays recognizable: readers that knew a fake id by sight still
  // can, by its prefix and its turn suffix, rather than by an exact string that
  // was only ever unique by accident.
  it("keeps the fake api message id recognizable by prefix and turn", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("hello"));
    h.input.end();
    const id = assistantMessageID(await h.collect());
    // Assert
    expect(id).toMatch(/^msg_fake_[0-9a-f]+_1$/);
  });

  it("provides deterministic full cache usage on ordinary fake turns", async () => {
    const h = makeFake();
    h.input.push(userMsg("hello"));
    h.input.end();
    const messages = await h.collect();
    const assistant = messages.find((message) => message.type === "assistant") as Record<string, unknown>;
    expect(assistant).toMatchObject({ message: { usage: {
      input_tokens: 7,
      output_tokens: 11,
      cache_read_input_tokens: 80,
      cache_creation_input_tokens: 4,
      service_tier: "standard",
      cache_creation: { ephemeral_5m_input_tokens: 4 },
      server_tool_use: { web_search_requests: 0 },
      fallback_credit: { status: "not_used" },
      output_tokens_details: { reasoning_tokens: 3 },
      unmodeled_usage: { fake_extension_tokens: 2 },
      cache_diagnostic: { status: "hit", reason: "stable_prefix" },
    } } });
  });

  it("emits main and subagent usage records plus whole-turn model usage", async () => {
    const h = makeFake();
    h.input.push(userMsg("!usage-subagent"));
    h.input.end();
    const messages = await h.collect();
    const assistants = messages.filter((message) => message.type === "assistant") as Array<Record<string, unknown>>;
    expect(assistants).toHaveLength(2);
    expect(assistants[0]).toMatchObject({ message: { usage: {
      input_tokens: 11,
      cache_creation: { ephemeral_5m_input_tokens: 41, ephemeral_1h_input_tokens: 42 },
      server_tool_use: { web_search_requests: 51, web_fetch_requests: 52 },
      iterations: [{ type: "message", model: "main-agent-reasoning-model", input_tokens: 61, cache_creation: { ephemeral_1h_input_tokens: 66 } }],
      output_tokens_details: { reasoning_tokens: 71, reasoning_model: "main-agent-reasoning-model" },
      cache_diagnostic: { reason: "model_changed", cache_missed_input_tokens: 72 },
      fallback_credit: { status: { type: "redeemed", actor: "main-agent" }, credits: 73 },
      unmodeled_usage: { future_counter: 74, nested: { actor: "main-agent" } },
    } } });
    expect(assistants[1]).toMatchObject({ subagent_type: "general-purpose", message: { model: "fake-subagent-model", usage: {
      input_tokens: 111,
      cache_creation: { ephemeral_5m_input_tokens: 141, ephemeral_1h_input_tokens: 142 },
      server_tool_use: { web_search_requests: 151, web_fetch_requests: 152 },
      iterations: [{ type: "message", model: "subagent-reasoning-model", input_tokens: 161, cache_creation: { ephemeral_1h_input_tokens: 166 } }],
      output_tokens_details: { reasoning_tokens: 171, reasoning_model: "subagent-reasoning-model" },
      cache_diagnostic: { reason: "model_changed", cache_missed_input_tokens: 172 },
      fallback_credit: { status: { type: "redeemed", actor: "subagent" }, credits: 173 },
      unmodeled_usage: { future_counter: 174, nested: { actor: "subagent" } },
    } } });
    expect(messages.find((message) => message.type === "result")).toMatchObject({
      usage: { input_tokens: 11, iterations: [{ model: "main-agent-reasoning-model" }] },
      model_usage: expect.objectContaining({
        "fake-model": expect.objectContaining({ input_tokens: 11, web_search_requests: 51, cost_usd: 0.001, context_window: 200000, max_output_tokens: 32000, canonical_model: "fake-model", provider: "anthropic" }),
        "fake-subagent-model": expect.objectContaining({ input_tokens: 111, web_search_requests: 151, cost_usd: 0.002, context_window: 200000, max_output_tokens: 16000, canonical_model: "fake-subagent-model", provider: "anthropic" }),
      }),
    });
  });

  it("emits complete raw subagent lineage in the usage-accounting fixture", async () => {
    const h = makeFake();
    h.input.push(userMsg("!usage-subagent"));
    h.input.end();

    const messages = await h.collect();
    const subagent = messages.filter((message) => message.type === "assistant")[1];
    expect(subagent).toMatchObject({
      agent_id: "fake-subagent-agent-id",
      parent_tool_use_id: "toolu_fake_subagent",
      parent_agent_id: "fake-parent-agent-id",
      subagent_type: "general-purpose",
      task_description: "deterministic usage-accounting subagent",
    });
  });

  it("ends the iterator without a terminal result for the query EOF fixture", async () => {
    const h = makeFake();
    h.input.push(userMsg("!query-eof"));
    const messages = await h.collect();
    expect(messages.some((message) => message.type === "result")).toBe(false);
  });

  // A TURN FAILURE IS OTHERWISE UNPROVOKABLE OFFLINE, and the daemon's merge
  // pipeline classifies a failed before-action and a failed after-action in
  // opposite directions. Neither branch is reachable in --fake mode without
  // this.
  it("ends a turn in error when the prompt carries the fail marker", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg(`${FAIL_TURN_MARKER}: the action the acceptance gate makes fail`));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const result = msgs.find((m) => m.type === "result")!;
    expect(result).toMatchObject({ subtype: "error_during_execution", is_error: true });
  });

  // No assistant message accompanies it: the turn produced none, and an empty
  // one would render as a blank bubble in every frontend.
  it("emits no assistant message for a failed turn", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg(`${FAIL_TURN_MARKER}: nothing was produced`));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    expect(msgs.some((m) => m.type === "assistant")).toBe(false);
  });

  it("asks canUseTool before running a !tool turn", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("!tool ls -la"));
    h.input.end();
    await h.collect();
    // Assert
    expect(h.permissionRequests).toEqual([
      { toolName: "Bash", input: { command: "ls -la" } },
    ]);
  });

  it("emits a non-error tool_result user message when the tool is allowed", async () => {
    // Arrange
    const h = makeFake({ behavior: "allow", updatedInput: {} });
    // Act
    h.input.push(userMsg("!tool ls"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const toolResultMsg = msgs.find((m) => m.type === "user")!;
    const blocks = (toolResultMsg.message as { content: Array<Record<string, unknown>> }).content;
    expect(blocks[0]).toMatchObject({ type: "tool_result", is_error: false });
  });

  it("emits an error tool_result user message when the tool is denied", async () => {
    // Arrange
    const h = makeFake({ behavior: "deny", message: "not today" });
    // Act
    h.input.push(userMsg("!tool rm -rf /"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const toolResultMsg = msgs.find((m) => m.type === "user")!;
    const blocks = (toolResultMsg.message as { content: Array<Record<string, unknown>> }).content;
    expect(blocks[0]).toMatchObject({
      type: "tool_result",
      is_error: true,
      content: expect.stringContaining("not today"),
    });
  });

  it("propagates canUseTool rejection as one logged iterator failure without a clean result", async () => {
    vi.mocked(writeSync).mockClear();
    const input = new AsyncQueue<SdkUserMessageLike>();
    const failure = new Error("permission transport failed");
    const query = createFakeQuery(
      input,
      async () => {
        throw failure;
      },
      { sessionId: "s-failing", newUuid: () => "u-failing" },
    );
    input.push(userMsg("!tool ls"));
    input.end();

    const messages: SdkMessageLike[] = [];
    const iterator = query[Symbol.asyncIterator]();
    let caught: unknown;
    for (;;) {
      try {
        const next = await iterator.next();
        if (next.done) break;
        messages.push(next.value);
      } catch (err) {
        caught = err;
        break;
      }
    }

    expect(caught).toBe(failure);
    expect(messages.some((message) => message.type === "result")).toBe(false);
    const errors = persistedLogs().filter((record) =>
      record["level"] === "error" &&
      record["operation"] === "shim.fake-query.lifecycle",
    );
    expect(errors).toHaveLength(1);
    expect(errors[0]).toMatchObject({
      agent_repl_session_id: "test-agent-session",
      message: "fake SDK producer failed: permission transport failed",
      context: expect.objectContaining({
        cause: expect.objectContaining({ message: "permission transport failed" }),
      }),
    });
  });

  it("replies with the markdown showcase on !md", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("!md"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const result = msgs.find((m) => m.type === "result")!;
    expect(result.result).toBe(MARKDOWN_SHOWCASE);
  });

  it("reflects setPermissionMode in subsequent echoes", async () => {
    // Arrange
    const h = makeFake();
    // Act
    await h.query.setPermissionMode("plan");
    h.input.push(userMsg("check mode"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const result = msgs.find((m) => m.type === "result")!;
    expect(result.result).toContain("mode=plan");
  });

  it("ends the output stream when the input ends", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.end();
    const msgs = await h.collect();
    // Assert — collect() returning at all proves termination
    expect(msgs.length).toBeGreaterThan(0);
  });

  it("offers a supported-model menu", async () => {
    // Arrange
    const h = makeFake();
    // Act
    const models = await h.query.supportedModels();
    // Assert
    expect(models).toEqual(FAKE_MODELS);
  });

  it("reports the model set by setModel on subsequent assistant messages", async () => {
    // Arrange — this is the whole drift path in miniature: the model the
    // agent ANSWERS with is what the topbar mirrors.
    const h = makeFake();
    // Act
    await h.query.setModel("fake-model-fast");
    h.input.push(userMsg("hello"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const assistant = msgs.find((m) => m.type === "assistant")!;
    expect((assistant.message as { model: string }).model).toBe("fake-model-fast");
  });

  it("leaves the model at the default until setModel moves it", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("hello"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const assistant = msgs.find((m) => m.type === "assistant")!;
    expect((assistant.message as { model: string }).model).toBe("fake-model");
  });
});

describe("!agent detached-agent turns", () => {
  it("announces the launch as system:task_started", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("!agent hunt bugs"));
    h.input.end();
    const msgs = await h.collect();
    // Assert: real task LIFECYCLE, which is the whole point of this branch —
    // `!bg` produces only a task-notification text block and leaves nothing
    // live.
    const started = msgs.find((m) => m.type === "system" && m.subtype === "task_started");
    expect(started).toMatchObject({ task_type: "local_agent", description: "hunt bugs" });
  });

  it("leaves the agent RUNNING when the turn ends", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("!agent hunt bugs"));
    h.input.end();
    const msgs = await h.collect();
    // Assert: detached work that outlives its turn is the condition the cancel
    // exists for, so the fake must not end the agent for you.
    const ended = msgs.find((m) => m.type === "system" && m.subtype === "task_notification");
    expect(ended).toBeUndefined();
    expect(msgs.some((m) => m.type === "result")).toBe(true);
  });

  it("stopTask ends the live agent with a stopped task_notification", async () => {
    // Arrange: pull the stream by hand so the stop lands after the launch is
    // OBSERVED rather than after a hopeful delay.
    const h = makeFake();
    const it = h.query[Symbol.asyncIterator]();
    h.input.push(userMsg("!agent hunt bugs"));
    let taskId = "";
    for (;;) {
      const next = await it.next();
      if (next.done === true) throw new Error("the launch never arrived");
      const m = next.value;
      if (m.type === "system" && m.subtype === "task_started") {
        taskId = String(m.task_id);
        break;
      }
    }

    // Act
    await h.query.stopTask(taskId);
    h.input.end();

    // Assert: the ordinary terminal fact the whole stack settles on.
    let note: SdkMessageLike | undefined;
    for (;;) {
      const next = await it.next();
      if (next.done === true) break;
      if (next.value.type === "system" && next.value.subtype === "task_notification") {
        note = next.value;
        break;
      }
    }
    expect(note).toMatchObject({ task_id: taskId, status: "stopped" });
  });

  it("stopTask accepts a task it never started as a no-op", async () => {
    // Arrange
    const h = makeFake();
    const it = h.query[Symbol.asyncIterator]();
    h.input.push(userMsg("!agent hunt bugs"));
    for (;;) {
      const next = await it.next();
      if (next.done === true) throw new Error("the launch never arrived");
      if (next.value.type === "system" && next.value.subtype === "task_started") break;
    }

    // Act: the stop is idempotent, so a task that already ended is not an error.
    await h.query.stopTask("fakeagent-nope");
    h.input.end();

    // Assert: nothing was ended by a stop that named nothing live.
    const rest: SdkMessageLike[] = [];
    for (;;) {
      const next = await it.next();
      if (next.done === true) break;
      rest.push(next.value);
    }
    expect(rest.some((m) => m.type === "system" && m.subtype === "task_notification")).toBe(false);
  });
});

describe("!bg background turns", () => {
  it("emits the tool_progress heartbeat for a backgrounded command", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("!bg sleep 5"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const progress = msgs.find((m) => m.type === "tool_progress");
    expect(progress).toMatchObject({ tool_name: "Bash", elapsed_time_seconds: 1 });
  });

  it("announces the spawn with a task id and spool output path", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("!bg sleep 5"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const resultMsg = msgs.find((m) => {
      if (m.type !== "user") return false;
      const blocks = (m.message as { content: Array<Record<string, unknown>> }).content;
      return blocks[0]?.type === "tool_result";
    })!;
    const blocks = (resultMsg.message as { content: Array<Record<string, unknown>> }).content;
    expect(String(blocks[0].content)).toMatch(/with ID: fakebg\d+\. Output is being written to: \/tmp\/claude-0\//);
  });

  it("completes the task with a harness-shaped notification text block", async () => {
    // Arrange
    const h = makeFake();
    // Act
    h.input.push(userMsg("!bg sleep 5"));
    h.input.end();
    const msgs = await h.collect();
    // Assert
    const noteMsg = msgs.find((m) => {
      if (m.type !== "user") return false;
      const blocks = (m.message as { content: Array<Record<string, unknown>> }).content;
      return blocks[0]?.type === "text";
    })!;
    const blocks = (noteMsg.message as { content: Array<Record<string, unknown>> }).content;
    expect(String(blocks[0].text)).toContain("<task-notification>");
    expect(String(blocks[0].text)).toContain("<status>completed</status>");
  });
});

describe("!hold turns", () => {
  // The permission-free way to hold a turn open. It exists because the OTHER
  // hold — `!tool`, which parks a canUseTool — is answerable by typing: a
  // prompt submitted over a parked question declines it and stops the turn
  // (daemon sessioncontroller/permdecline.go), so a caller that needs a live
  // turn to submit prompts behind cannot hold one open with a question.
  //
  // The rendezvous is the hold's OWN first frame, never a timer: the resolver
  // is in place before that frame can be observed, so a stop sent on seeing it
  // always has something to release.
  async function holdParked(h: FakeHarness): Promise<AsyncIterator<SdkMessageLike>> {
    const iterator = h.query[Symbol.asyncIterator]() as AsyncIterator<SdkMessageLike>;
    h.input.push(userMsg("!hold"));
    for (let step = await iterator.next(); !step.done; step = await iterator.next()) {
      if (step.value.type === "stream_event") return iterator;
    }
    throw new Error("the hold turn never started");
  }

  /** Everything the query emits after the hold parked. */
  async function drain(iterator: AsyncIterator<SdkMessageLike>): Promise<SdkMessageLike[]> {
    const out: SdkMessageLike[] = [];
    for (let step = await iterator.next(); !step.done; step = await iterator.next()) {
      out.push(step.value);
    }
    return out;
  }

  it("emits no terminal result while it holds", async () => {
    // Arrange / Act — parked, with nothing asked of the user.
    const h = makeFake();
    const iterator = await holdParked(h);
    // Assert — the turn is live: no result has been produced.
    h.input.end();
    await h.query.interrupt();
    const after = await drain(iterator);
    expect(after.filter((m) => m.type === "result")).toHaveLength(1);
  });

  it("ends the held turn the way an interrupted turn ends", async () => {
    // Arrange
    const h = makeFake();
    const iterator = await holdParked(h);
    // Act
    await h.query.interrupt();
    h.input.end();
    // Assert
    const after = await drain(iterator);
    const result = after.find((m) => m.type === "result")!;
    expect(result).toMatchObject({ subtype: "error_during_execution", is_error: true });
  });

  it("asks the user nothing while it holds", async () => {
    // Arrange
    const h = makeFake();
    const iterator = await holdParked(h);
    // Act
    await h.query.interrupt();
    h.input.end();
    await drain(iterator);
    // Assert
    expect(h.permissionRequests).toEqual([]);
  });
});

describe("fake query interrupt receipt", () => {
  it("resolves the receipt shape the real SDK returns, not undefined", async () => {
    // The shim depends on SDK 0.3.220, whose interrupt() always answers with
    // a receipt (a real probe returns {"still_queued":[]}). Resolving
    // undefined here would leave every offline run exercising a shape the
    // real SDK never produces.
    const h = makeFake();
    // Act
    const receipt = await h.query.interrupt();
    // Assert
    expect(receipt).toEqual({ still_queued: [] });
  });
});

describe("createFakeQuery vendor session rotation (!rotate)", () => {
  // The vendor retires its transcript identity mid-stream on a `/clear` and
  // announces the new one with a fresh `system:init`; every message after that
  // point, the turn's own `result` included, belongs to the new identity. This
  // is the offline stand-in for that, and it is simulated vendor behavior, not
  // a shortcut around any code under test.

  /** Run one turn's worth of messages under a uuid-minting fake. */
  async function rotateRun(prompt: string): Promise<SdkMessageLike[]> {
    const input = new AsyncQueue<SdkUserMessageLike>();
    let n = 0;
    const query = createFakeQuery(input, async () => ({ behavior: "allow", updatedInput: {} }), {
      sessionId: "s-shim",
      newUuid: () => `u${++n}`,
    });
    input.push(userMsg(prompt));
    input.end();
    const msgs: SdkMessageLike[] = [];
    for await (const m of query) msgs.push(m);
    return msgs;
  }

  it("announces the new identity with a second system init", async () => {
    // Arrange / Act
    const msgs = await rotateRun("!rotate");

    // Assert — two inits: the opening one and the rotation's own.
    const inits = msgs.filter((m) => m.type === "system" && (m as { subtype?: string }).subtype === "init");
    expect(inits).toHaveLength(2);
  });

  it("files everything after the rotation under the NEW uuid", async () => {
    // Arrange / Act
    const msgs = await rotateRun("!rotate");

    // Assert — the result closing this turn belongs to the new identity, which
    // is exactly the split that orphaned the real turn's end.
    const result = msgs.find((m) => m.type === "result")!;
    expect(result.session_id).not.toBe("s-shim");
    const inits = msgs.filter((m) => m.type === "system" && (m as { subtype?: string }).subtype === "init");
    expect(result.session_id).toBe(inits[1]!.session_id);
  });

  it("leaves an ordinary turn's identity alone", async () => {
    // Arrange / Act
    const msgs = await rotateRun("just a prompt");

    // Assert
    const result = msgs.find((m) => m.type === "result")!;
    expect(result.session_id).toBe("s-shim");
  });
});
