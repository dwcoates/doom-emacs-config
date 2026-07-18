import { describe, expect, it } from "vitest";
import {
  ConversationStore,
  PermissionItem,
  ResultItem,
  TextItem,
  ThinkingItem,
  ToolItem,
  UserTurnItem,
  liveContextDelta,
  topLevelUsage,
} from "../src/store.js";

let autoSeq = 0;

function frame(type: string, fields: Record<string, unknown> = {}, seq?: number): string {
  if (seq === undefined) {
    autoSeq++;
    seq = autoSeq;
  } else {
    autoSeq = seq;
  }
  return JSON.stringify({ type, seq, ts: "T", session_id: "s1", ...fields });
}

function hello(fields: Record<string, unknown> = {}): string {
  return JSON.stringify({
    type: "hello",
    seq: 0,
    ts: "T",
    session_id: "s1",
    daemon_version: "0.1.0",
    resume_from_seq: 0,
    permission_mode: "default",
    model: "m",
    cwd: "/w",
    ...fields,
  });
}

function newStore(): ConversationStore {
  autoSeq = 0;
  const store = new ConversationStore();
  store.applyRaw(hello());
  return store;
}

/** A successful result frame, closing a turn. */
function resultFrame(fields: Record<string, unknown> = {}): string {
  return frame("result", {
    subtype: "success",
    duration_ms: 5,
    duration_api_ms: 3,
    num_turns: 1,
    total_cost_usd: 0.01,
    usage: { input_tokens: 1, output_tokens: 2 },
    is_error: false,
    result_text: "ok",
    ...fields,
  });
}

/** A usage frame declaring a request that carried TOTAL tokens of context. */
function usageFrame(total: number): string {
  return frame("usage", {
    message_id: "m1",
    usage: { input_tokens: total, output_tokens: 2 },
  });
}

/** The user's `/clear` turn, as the daemon replays it into the feed. */
function clearTurn(): string {
  return frame("user-turn", {
    request_id: "rc",
    content: [{ type: "text", text: "/clear" }],
  });
}

/** A one-model per-model usage map, for the token-tally reset tests. */
function sampleModelUsage(): Record<string, unknown> {
  return {
    "claude-opus-4-8": {
      input_tokens: 30,
      output_tokens: 40,
      cache_creation_input_tokens: 60,
      cache_read_input_tokens: 50,
      web_search_requests: 2,
      cost_usd: 0.12,
      context_window: 1_000_000,
    },
  };
}

/** The store's result items, in arrival order. */
function resultItems(store: ConversationStore): ResultItem[] {
  return store.state.items.filter((i): i is ResultItem => i.kind === "result");
}

describe("ConversationStore hello handling", () => {
  it("adopts session info from hello", () => {
    // Arrange + Act
    const store = newStore();
    // Assert
    expect(store.state).toMatchObject({
      sessionId: "s1",
      daemonVersion: "0.1.0",
      model: "m",
      cwd: "/w",
      permissionMode: "default",
    });
  });

  it("requests retained history on a fresh join mid-conversation", () => {
    // Arrange
    autoSeq = 0;
    const store = new ConversationStore();
    // Act — daemon retains 5..9.
    const result = store.applyRaw(hello({ seq: 9, resume_from_seq: 5 }));
    // Assert
    expect(result.send).toEqual({ type: "replay-request", from_seq: 5 });
    expect(store.state.lastSeq).toBe(4);
  });

  it("requests only the missed tail on reconnect within retention", () => {
    // Arrange — store already has 1..2 applied.
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }, 1));
    store.applyRaw(frame("system", { subtype: "init", data: {} }, 2));
    // Act — reconnect; daemon is at 6, retains from 1.
    const result = store.applyRaw(hello({ seq: 6, resume_from_seq: 1 }));
    // Assert — items kept, replay from 3.
    expect(result.send).toEqual({ type: "replay-request", from_seq: 3 });
    expect(store.state.items).toHaveLength(2);
  });

  it("rebuilds from scratch when the gap was evicted (fresh hello)", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }, 1));
    // Act — §2.10 eviction: daemon now only retains from 50.
    const result = store.applyRaw(hello({ seq: 60, resume_from_seq: 50 }));
    // Assert — local state discarded, replay from the earliest retained.
    expect(store.state.items).toHaveLength(0);
    expect(store.state.lastSeq).toBe(49);
    expect(result.send).toEqual({ type: "replay-request", from_seq: 50 });
  });

  it("requests nothing on a brand-new session hello", () => {
    // Arrange
    autoSeq = 0;
    const store = new ConversationStore();
    // Act
    const result = store.applyRaw(hello({ seq: 0, resume_from_seq: 0 }));
    // Assert
    expect(result.send).toBeUndefined();
  });
});

describe("ConversationStore seq handling", () => {
  it("skips duplicate frames from replay overlap", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }, 1));
    // Act — the same frame arrives again.
    const result = store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }, 1));
    // Assert
    expect(result.changed).toBe(false);
    expect(store.state.items).toHaveLength(1);
  });

  it("detects a gap, skips the frame and requests replay", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }, 1));
    // Act — seq 2 was dropped.
    const result = store.applyRaw(frame("system", { subtype: "init", data: {} }, 3));
    // Assert
    expect(result.send).toEqual({ type: "replay-request", from_seq: 2 });
    expect(store.state.lastSeq).toBe(1);
    expect(store.state.items).toHaveLength(1);
  });

  it("advances the cursor over unknown frame types", () => {
    // Arrange
    const store = newStore();
    // Act
    const result = store.applyRaw(frame("hologram", {}, 1));
    const next = store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }, 2));
    // Assert — no gap detected despite the unknown frame.
    expect(result.changed).toBe(false);
    expect(next.send).toBeUndefined();
    expect(store.state.lastSeq).toBe(2);
  });
});

describe("ConversationStore text blocks", () => {
  it("streams a text block through start/delta/end", () => {
    // Arrange
    const store = newStore();
    // Act
    store.applyRaw(frame("text-start", { block_id: "b1", message_id: "m1" }));
    store.applyRaw(frame("text-delta", { block_id: "b1", text: "hel" }));
    store.applyRaw(frame("text-delta", { block_id: "b1", text: "lo" }));
    store.applyRaw(frame("text-end", { block_id: "b1", final_text: "hello" }));
    // Assert
    const item = store.state.items[0] as TextItem;
    expect(item).toMatchObject({ kind: "text", text: "hello", done: true });
  });

  it("flags a text block as an error when its message's assistant-error frame lands", () => {
    // Arrange — the error frame arrives AFTER the block closes, as it does on
    // the wire (the SDK's verdict comes with the completed message).
    const store = newStore();
    store.applyRaw(frame("text-start", { block_id: "b1", message_id: "m1" }));
    store.applyRaw(frame("text-end", { block_id: "b1", final_text: "You've hit your session limit" }));
    // Act
    store.applyRaw(frame("assistant-error", { message_id: "m1", error: "rate_limit" }));
    // Assert
    expect((store.state.items[0] as TextItem).error).toBe("rate_limit");
  });

  it("leaves a different message's text block unflagged by an assistant-error frame", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("text-start", { block_id: "b1", message_id: "m1" }));
    store.applyRaw(frame("text-end", { block_id: "b1", final_text: "ok" }));
    // Act — the error names a different message.
    store.applyRaw(frame("assistant-error", { message_id: "m2", error: "rate_limit" }));
    // Assert
    expect((store.state.items[0] as TextItem).error).toBeUndefined();
  });

  it("keeps a subagent text block's owning call from its start frame", () => {
    // Arrange
    const store = newStore();
    // Act
    store.applyRaw(
      frame("text-start", { block_id: "b1", message_id: "m1", parent_tool_use_id: "task1" }),
    );
    // Assert
    expect((store.state.items[0] as TextItem).parentToolUseId).toBe("task1");
  });

  it("keeps a subagent thinking block's owning call from its start frame", () => {
    // Arrange
    const store = newStore();
    // Act
    store.applyRaw(
      frame("thinking-start", { block_id: "b1", message_id: "m1", parent_tool_use_id: "task1" }),
    );
    // Assert
    expect((store.state.items[0] as ThinkingItem).parentToolUseId).toBe("task1");
  });

  it("stamps a text block with the time its start frame carried", () => {
    // Arrange
    const store = newStore();
    // Act — the stamp is the block's OPENING time, so the end frame cannot move it.
    store.applyRaw(
      frame("text-start", {
        block_id: "b1",
        message_id: "m1",
        ts: "2026-05-24T14:33:00Z",
      }),
    );
    store.applyRaw(
      frame("text-end", { block_id: "b1", final_text: "hello", ts: "2026-05-24T14:35:00Z" }),
    );
    // Assert
    expect((store.state.items[0] as TextItem).ts).toBe("2026-05-24T14:33:00Z");
  });

  it("replaces accumulated text with the canonical final_text", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("text-start", { block_id: "b1", message_id: "m1" }));
    store.applyRaw(frame("text-delta", { block_id: "b1", text: "partial" }));
    // Act — canonical text differs from the accumulation.
    store.applyRaw(frame("text-end", { block_id: "b1", final_text: "canonical" }));
    // Assert
    expect((store.state.items[0] as TextItem).text).toBe("canonical");
  });
});

describe("ConversationStore tool cards", () => {
  function runToolStart(store: ConversationStore): void {
    store.applyRaw(frame("tool-use-start", {
      tool_use_id: "t1",
      tool_name: "Bash",
      message_id: "m1",
    }));
  }

  it("builds a tool item through the §2.6 lifecycle", () => {
    // Arrange
    const store = newStore();
    // Act
    runToolStart(store);
    store.applyRaw(frame("tool-use-input-delta", { tool_use_id: "t1", partial_json: '{"command":' }));
    store.applyRaw(frame("tool-use-input-end", { tool_use_id: "t1", input: { command: "ls" } }));
    store.applyRaw(frame("tool-use-result", {
      tool_use_id: "t1",
      is_error: false,
      content: "file-a",
      render: { kind: "bash", stdout: "file-a", stderr: "" },
    }));
    // Assert
    const item = store.state.items[0] as ToolItem;
    expect(item).toMatchObject({
      kind: "tool",
      toolName: "Bash",
      inputDone: true,
      input: { command: "ls" },
      result: { isError: false },
    });
    expect(item.result?.render).toMatchObject({ kind: "bash" });
  });

  it("stamps the tool item with the tool-use-start envelope ts", () => {
    // Arrange
    const store = newStore();
    // Act — frame() stamps every envelope with ts "T".
    runToolStart(store);
    // Assert — the agent-scoped elapsed clock counts from this stamp.
    expect((store.state.items[0] as ToolItem).ts).toBe("T");
  });

  it("banks an attributed usage frame on the spawning agent's tool item", () => {
    // Arrange — a subagent call the usage frame will name.
    const store = newStore();
    store.applyRaw(frame("tool-use-start", {
      tool_use_id: "a1",
      tool_name: "Agent",
      message_id: "m1",
    }));
    // Act — the agent's own request declares its context size.
    store.applyRaw(frame("usage", {
      message_id: "m2",
      parent_tool_use_id: "a1",
      usage: { input_tokens: 10, output_tokens: 2, cache_read_input_tokens: 90 },
    }));
    // Assert — input plus cache read, on the item.
    expect((store.state.items[0] as ToolItem).contextTokens).toBe(100);
  });

  it("keeps an attributed usage frame away from the session standing", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("tool-use-start", {
      tool_use_id: "a1",
      tool_name: "Agent",
      message_id: "m1",
    }));
    // Act — a sidechain figure that must not flip the topbar count.
    store.applyRaw(frame("usage", {
      message_id: "m2",
      parent_tool_use_id: "a1",
      usage: { input_tokens: 50_000, output_tokens: 2 },
    }));
    // Assert — the session's figure stays unknown, not the subagent's 50k.
    expect(store.state.contextTokens).toBeNull();
  });

  it("drops an attributed usage frame naming a call the feed does not hold", () => {
    // Arrange
    const store = newStore();
    // Act — attribution to an evicted (or interrupt-dropped) call.
    store.applyRaw(frame("usage", {
      message_id: "m2",
      parent_tool_use_id: "gone",
      usage: { input_tokens: 50_000, output_tokens: 2 },
    }));
    // Assert — nothing to bank on, and the session figure stays untouched.
    expect(store.state.contextTokens).toBeNull();
  });

  it("records tool progress text", () => {
    // Arrange
    const store = newStore();
    runToolStart(store);
    // Act
    store.applyRaw(frame("tool-use-progress", { tool_use_id: "t1", text: "Bash running (3s)" }));
    // Assert
    expect((store.state.items[0] as ToolItem).progress).toBe("Bash running (3s)");
  });

  it("keeps the heartbeat's raw elapsed clock beside the progress text", () => {
    // Arrange
    const store = newStore();
    runToolStart(store);
    // Act
    store.applyRaw(
      frame("tool-use-progress", { tool_use_id: "t1", text: "Bash running (3s)", elapsed_seconds: 3.5 }),
    );
    // Assert
    expect((store.state.items[0] as ToolItem).progressElapsedS).toBe(3.5);
  });

  it("lands a task notification on the spawning tool card", () => {
    // Arrange
    const store = newStore();
    runToolStart(store);
    // Act
    store.applyRaw(
      frame("task-notification", {
        tool_use_id: "t1",
        task_id: "bg1",
        status: "completed",
        summary: "done (exit code 0)",
        output_file: "/tmp/bg1.output",
        text: "<task-notification>…</task-notification>",
      }),
    );
    // Assert
    expect((store.state.items[0] as ToolItem).notification).toMatchObject({
      taskId: "bg1",
      status: "completed",
      summary: "done (exit code 0)",
      outputFile: "/tmp/bg1.output",
    });
  });

  it("surfaces an unattributable task notification as a system note", () => {
    // Arrange — no tool item carries the named id.
    const store = newStore();
    // Act
    store.applyRaw(
      frame("task-notification", { tool_use_id: "t-unknown", text: "<task-notification/>" }),
    );
    // Assert — the completion is never swallowed.
    expect(store.state.items[0]).toMatchObject({ kind: "system", subtype: "task-notification" });
  });

  it("stamps the deactivation turn when the result arrives", () => {
    // Arrange — one counted user turn precedes the tool.
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [{ type: "text", text: "go" }] }));
    runToolStart(store);
    // Act
    store.applyRaw(frame("tool-use-result", { tool_use_id: "t1", is_error: false, content: "done" }));
    // Assert — the tool item is items[1] (after the user turn).
    expect((store.state.items[1] as ToolItem).deactivatedAtTurn).toBe(1);
  });

  it("leaves the deactivation turn unstamped until the result arrives", () => {
    // Arrange + Act — tool started but not yet settled.
    const store = newStore();
    runToolStart(store);
    // Assert
    expect((store.state.items[0] as ToolItem).deactivatedAtTurn).toBeUndefined();
  });
});

describe("ConversationStore permissions", () => {
  it("mounts and resolves a permission prompt", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("permission-request", {
      request_id: "p1",
      tool_use_id: "t1",
      tool_name: "Bash",
      input: { command: "ls" },
      preview: { kind: "bash", command: "ls" },
    }));
    // Act
    store.applyRaw(frame("permission-resolved", { request_id: "p1", decision: "allow" }));
    // Assert
    const item = store.state.items[0] as PermissionItem;
    expect(item.resolution).toEqual({ decision: "allow", message: undefined });
  });

  it("dismisses a stale prompt on a cancel resolution", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("permission-request", {
      request_id: "p1",
      tool_use_id: "t1",
      tool_name: "Bash",
      input: {},
    }));
    // Act
    store.applyRaw(frame("permission-resolved", {
      request_id: "p1",
      decision: "cancel",
      message: "interrupted",
    }));
    // Assert
    const item = store.state.items[0] as PermissionItem;
    expect(item.resolution?.decision).toBe("cancel");
  });
});

describe("ConversationStore turn lifecycle", () => {
  it("records the envelope ts on the user turn", () => {
    // Arrange
    const store = newStore();
    // Act
    store.applyRaw(
      JSON.stringify({
        type: "user-turn",
        seq: 1,
        ts: "2026-05-24T12:34:56.789Z",
        session_id: "s1",
        request_id: "r1",
        content: [{ type: "text", text: "hi" }],
      }),
    );
    // Assert
    const item = store.state.items[0] as UserTurnItem;
    expect(item.ts).toBe("2026-05-24T12:34:56.789Z");
  });

  it("turns the spinner on at user-turn and off at result", () => {
    // Arrange
    const store = newStore();
    // Act + Assert
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [{ type: "text", text: "hi" }] }));
    expect(store.state.turnInFlight).toBe(true);
    store.applyRaw(frame("result", {
      subtype: "success",
      duration_ms: 5,
      duration_api_ms: 3,
      num_turns: 1,
      total_cost_usd: 0.01,
      usage: { input_tokens: 1, output_tokens: 2 },
      is_error: false,
      result_text: "ok",
    }));
    expect(store.state.turnInFlight).toBe(false);
    expect(store.state.costUsd).toBe(0.01);
  });

  it("does not move the standing context tokens from a result frame", () => {
    // Arrange — a result's usage is the turn's CUMULATIVE spend across every
    // API request it made, so routing it into the context figure runs the
    // topbar past the window without bound.
    const store = newStore();
    // Act — a result arrives before any per-request usage frame declared a size.
    store.applyRaw(frame("result", {
      subtype: "success",
      duration_ms: 5,
      duration_api_ms: 3,
      num_turns: 1,
      total_cost_usd: 0.01,
      usage: { input_tokens: 2_500_000, output_tokens: 2 },
      is_error: false,
      result_text: "ok",
    }));
    // Assert — the context size stays unknown, not the 2.5M the result carried.
    expect(store.state.contextTokens).toBeNull();
  });

  it("holds the context tokens at the last usage frame across a result", () => {
    // Arrange — a per-request usage frame sets the standing size.
    const store = newStore();
    store.applyRaw(usageFrame(200_000));
    // Act — a result carrying a far larger cumulative figure closes the turn.
    store.applyRaw(resultFrame({ usage: { input_tokens: 2_500_000, output_tokens: 2 } }));
    // Assert — the header figure is the request's 200k, not the result's 2.5M.
    expect(store.state.contextTokens).toBe(200_000);
  });

  it("updates the context tokens from mid-turn usage frames", () => {
    // Arrange
    const store = newStore();
    // Act
    store.applyRaw(frame("usage", {
      message_id: "m1",
      usage: {
        input_tokens: 10,
        output_tokens: 20,
        cache_read_input_tokens: 190,
      },
      cost_usd: 0.2,
    }));
    // Assert — input plus cache read IS the request's context size.
    expect(store.state.contextTokens).toBe(200);
    expect(store.state.costUsd).toBe(0.2);
  });

  it("stamps a result with the session's standing input tokens", () => {
    // Arrange — a request's input side IS the context it carried.
    const store = newStore();
    store.applyRaw(frame("usage", {
      message_id: "m1",
      usage: {
        input_tokens: 1000,
        output_tokens: 20,
        cache_read_input_tokens: 250_000,
        cache_creation_input_tokens: 49_000,
      },
    }));
    // Act
    store.applyRaw(resultFrame());
    // Assert
    expect(resultItems(store)[0].context).toEqual({ total: 300_000, delta: 300_000 });
  });

  it("measures a result's increase against the previous result's standing", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(usageFrame(200_000));
    store.applyRaw(resultFrame());
    // Act
    store.applyRaw(usageFrame(300_000));
    store.applyRaw(resultFrame());
    // Assert
    expect(resultItems(store)[1].context).toEqual({ total: 300_000, delta: 100_000 });
  });

  it("signs a shrunken context as a decrease", () => {
    // Arrange — the turn after a compaction stands below the one before it.
    const store = newStore();
    store.applyRaw(usageFrame(200_000));
    store.applyRaw(resultFrame());
    // Act
    store.applyRaw(usageFrame(60_000));
    store.applyRaw(resultFrame());
    // Assert
    expect(resultItems(store)[1].context).toEqual({ total: 60_000, delta: -140_000 });
  });

  it("unknows the standing input tokens when a compaction rewrites the context", () => {
    // Arrange — the SDK reports no post-compaction size, so the old one is stale.
    const store = newStore();
    store.applyRaw(usageFrame(200_000));
    // Act
    store.applyRaw(frame("compact-boundary", { trigger: "manual", pre_tokens: 200_000, post_tokens: 0 }));
    store.applyRaw(resultFrame());
    // Assert
    expect(resultItems(store)[0].context).toBeNull();
  });

  it("re-anchors the standing input tokens on post_tokens when a compaction reports it", () => {
    // Arrange — the SDK reports the post-compaction size, so the topbar shows
    // it immediately instead of a dash.
    const store = newStore();
    store.applyRaw(usageFrame(200_000));
    // Act
    store.applyRaw(frame("compact-boundary", { trigger: "manual", pre_tokens: 200_000, post_tokens: 18_000 }));
    // Assert
    expect(store.state.contextTokens).toBe(18_000);
  });

  it("unknows the standing input tokens when a /clear re-inits the session", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(usageFrame(200_000));
    // Act
    store.applyRaw(frame("system", { subtype: "init", data: {} }));
    store.applyRaw(resultFrame());
    // Assert
    expect(resultItems(store)[0].context).toBeNull();
  });

  it("measures the first result after a /clear from zero", () => {
    // Arrange — a cleared session really does start its context over.
    const store = newStore();
    store.applyRaw(usageFrame(200_000));
    store.applyRaw(resultFrame());
    store.applyRaw(frame("system", { subtype: "init", data: {} }));
    store.applyRaw(resultFrame());
    // Act
    store.applyRaw(usageFrame(30_000));
    store.applyRaw(resultFrame());
    // Assert
    expect(resultItems(store)[2].context).toEqual({ total: 30_000, delta: 30_000 });
  });

  it("withholds the standing input tokens from a result the session never reported one for", () => {
    // Arrange — a turn that errored before any request went out.
    const store = newStore();
    // Act
    store.applyRaw(resultFrame({ subtype: "error_during_execution", is_error: true }));
    // Assert
    expect(resultItems(store)[0].context).toBeNull();
  });

  it("leaves the standing input tokens untouched by the result frame's own usage", () => {
    // Arrange — a result's usage sums the turn's requests, so it is no context size.
    const store = newStore();
    store.applyRaw(usageFrame(200_000));
    // Act
    store.applyRaw(resultFrame({ usage: { input_tokens: 999_999, output_tokens: 2 } }));
    // Assert
    expect(store.state.contextTokens).toBe(200_000);
  });

  it("tracks permission-mode-changed frames", () => {
    // Arrange
    const store = newStore();
    // Act
    store.applyRaw(frame("permission-mode-changed", { mode: "plan", origin: "user" }));
    // Assert
    expect(store.state.permissionMode).toBe("plan");
  });

  it("moves the model on a user-origin model-changed frame", () => {
    // Arrange
    const store = newStore();
    // Act
    store.applyRaw(frame("model-changed", { model: "haiku", origin: "user" }));
    // Assert
    expect(store.state.model).toBe("haiku");
  });

  it("moves the model when the AGENT switched it, not the user", () => {
    // Arrange — the /model-switch staleness this whole path exists to fix.
    const store = newStore();
    // Act
    store.applyRaw(frame("model-changed", { model: "haiku", origin: "agent" }));
    // Assert
    expect(store.state.model).toBe("haiku");
  });

  it("moves the model when the daemon's periodic reconcile caught drift", () => {
    // Arrange
    const store = newStore();
    // Act
    store.applyRaw(frame("model-changed", { model: "haiku", origin: "reconcile" }));
    // Assert
    expect(store.state.model).toBe("haiku");
  });

  it("stores the selectable-model menu from a models frame", () => {
    // Arrange
    const store = newStore();
    const models = [{ value: "opus", displayName: "Opus", description: "d" }];
    // Act
    store.applyRaw(frame("models", { models }));
    // Assert
    expect(store.state.models).toEqual(models);
  });

  it("takes the model menu from the hello", () => {
    // Arrange
    const store = newStore();
    const models = [{ value: "opus", displayName: "Opus", description: "d" }];
    // Act
    store.applyRaw(hello({ models }));
    // Assert
    expect(store.state.models).toEqual(models);
  });

  it("keeps a populated menu across a hello that carries none", () => {
    // Arrange — a reconnect before the shim has re-reported its models must
    // not empty a picker that already works.
    const store = newStore();
    store.applyRaw(frame("models", {
      models: [{ value: "opus", displayName: "Opus", description: "d" }],
    }));
    // Act
    store.applyRaw(hello({ seq: 0 }));
    // Assert
    expect(store.state.models).toHaveLength(1);
  });

  it("adds a compact-boundary divider item", () => {
    // Arrange
    const store = newStore();
    // Act
    store.applyRaw(frame("compact-boundary", { trigger: "auto", pre_tokens: 5000, post_tokens: 0 }));
    // Assert
    expect(store.state.items[0]).toMatchObject({ kind: "compact-boundary", trigger: "auto" });
  });

  it("clears the spinner on an unrecoverable error", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }));
    // Act
    store.applyRaw(frame("error", { code: "shim_died", message: "gone", recoverable: false }));
    // Assert
    expect(store.state.turnInFlight).toBe(false);
    expect(store.state.items.at(-1)).toMatchObject({ kind: "error", code: "shim_died" });
  });
});

describe("ConversationStore compaction progress", () => {
  it("starts not compacting", () => {
    // Arrange + Act
    const store = newStore();
    // Assert
    expect(store.state.compacting).toBe(false);
  });

  it("enters the compacting state on a compact-status frame", () => {
    // Arrange
    const store = newStore();
    // Act
    store.applyRaw(frame("compact-status", { active: true }));
    // Assert
    expect(store.state.compacting).toBe(true);
  });

  it("leaves the compacting state when the compact-boundary lands", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("compact-status", { active: true }));
    // Act
    store.applyRaw(frame("compact-boundary", { trigger: "manual", pre_tokens: 9000, post_tokens: 800 }));
    // Assert
    expect(store.state.compacting).toBe(false);
  });

  it("leaves the compacting state when the turn's result lands without a boundary", () => {
    // Arrange — a compaction that failed emits no boundary; the result still
    // ends the turn, so the indicator must not stick.
    const store = newStore();
    store.applyRaw(frame("compact-status", { active: true }));
    // Act
    store.applyRaw(resultFrame());
    // Assert
    expect(store.state.compacting).toBe(false);
  });

  it("leaves the compacting state on an unrecoverable error", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("compact-status", { active: true }));
    // Act
    store.applyRaw(frame("error", { code: "shim_died", message: "gone", recoverable: false }));
    // Assert
    expect(store.state.compacting).toBe(false);
  });

  it("keeps compacting through a recoverable error", () => {
    // Arrange — a recoverable error is followed by a retry; the compaction the
    // turn was running is still in flight.
    const store = newStore();
    store.applyRaw(frame("compact-status", { active: true }));
    // Act
    store.applyRaw(frame("error", { code: "sdk_error", message: "hiccup", recoverable: true }));
    // Assert
    expect(store.state.compacting).toBe(true);
  });
});

describe("ConversationStore fresh-join replay phases", () => {
  it("enters replaying mode when a fresh join requests full history", () => {
    // Arrange
    autoSeq = 0;
    const store = new ConversationStore();
    // Act — daemon retains 1..3.
    store.applyRaw(hello({ seq: 3, resume_from_seq: 1 }));
    // Assert
    expect(store.replaying).toBe(true);
  });

  it("stays quiet-changed while replay frames stream in", () => {
    // Arrange
    autoSeq = 0;
    const store = new ConversationStore();
    store.applyRaw(hello({ seq: 3, resume_from_seq: 1 }));
    // Act — first two of three replayed frames.
    const r1 = store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }, 1));
    const r2 = store.applyRaw(frame("system", { subtype: "init", data: {} }, 2));
    // Assert — applied but not yet restored.
    expect(r1).toMatchObject({ changed: true });
    expect(r1.restored).toBeUndefined();
    expect(r2.restored).toBeUndefined();
    expect(store.replaying).toBe(true);
  });

  it("reports restored exactly on the frame reaching the hello watermark", () => {
    // Arrange
    autoSeq = 0;
    const store = new ConversationStore();
    store.applyRaw(hello({ seq: 2, resume_from_seq: 1 }));
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }, 1));
    // Act
    const result = store.applyRaw(frame("system", { subtype: "init", data: {} }, 2));
    // Assert
    expect(result.restored).toBe(true);
    expect(store.replaying).toBe(false);
  });

  it("reports restored on an unknown frame type at the watermark", () => {
    // Arrange — the replay backlog must render even when the last
    // retained frame is a type this client version cannot parse.
    autoSeq = 0;
    const store = new ConversationStore();
    store.applyRaw(hello({ seq: 1, resume_from_seq: 1 }));
    // Act
    const result = store.applyRaw(frame("frame-from-the-future", {}, 1));
    // Assert
    expect(result).toMatchObject({ changed: true, restored: true });
  });

  it("does not enter replaying mode on an empty fresh session", () => {
    // Arrange
    autoSeq = 0;
    const store = new ConversationStore();
    // Act
    store.applyRaw(hello({ seq: 0, resume_from_seq: 0 }));
    // Assert
    expect(store.replaying).toBe(false);
  });
});

describe("ConversationStore gap-fill replay phases", () => {
  /** A store with seqs 1..2 applied, reconnecting to a daemon at seq 4. */
  function reconnected(): ConversationStore {
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }, 1));
    store.applyRaw(frame("system", { subtype: "init", data: {} }, 2));
    store.applyRaw(hello({ seq: 4, resume_from_seq: 1 }));
    return store;
  }

  it("enters replaying mode on a reconnect that missed frames", () => {
    // Arrange + Act — the backlog otherwise renders once per frame,
    // which is the switched-back-to feed's catch-up jitter.
    const store = reconnected();
    // Assert
    expect(store.replaying).toBe(true);
  });

  it("stays quiet-changed while the gap backlog streams in", () => {
    // Arrange
    const store = reconnected();
    // Act — first of the two missed frames.
    const result = store.applyRaw(frame("system", { subtype: "s3", data: {} }, 3));
    // Assert — applied, deferred, not yet caught up.
    expect(result).toMatchObject({ changed: true });
    expect(result.restored).toBeFalsy();
    expect(store.replaying).toBe(true);
  });

  it("completes at the hello watermark without the restored flag", () => {
    // Arrange
    const store = reconnected();
    store.applyRaw(frame("system", { subtype: "s3", data: {} }, 3));
    // Act — the frame reaching the watermark.
    const result = store.applyRaw(frame("system", { subtype: "s4", data: {} }, 4));
    // Assert — one ordinary reconcile render, NOT the restored rebuild:
    // the feed is already built, and a rebuild would drop expanded
    // sections and yank a scrolled-up reader to the tail.
    expect(result.changed).toBe(true);
    expect(result.restored).toBeFalsy();
    expect(store.replaying).toBe(false);
  });

  it("does not enter replaying mode on a reconnect that missed nothing", () => {
    // Arrange — store already has 1 applied.
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }, 1));
    // Act — reconnect; the daemon is exactly where we left off.
    store.applyRaw(hello({ seq: 1, resume_from_seq: 1 }));
    // Assert — nothing to defer.
    expect(store.replaying).toBe(false);
  });
});

describe("ConversationStore rebind support", () => {
  it("adopts claude_session_id from hello", () => {
    // Arrange
    autoSeq = 0;
    const store = new ConversationStore();
    // Act
    store.applyRaw(hello({ claude_session_id: "uuid-7" }));
    // Assert
    expect(store.state.claudeSessionId).toBe("uuid-7");
  });

  it("leaves claudeSessionId empty on a pre-init hello", () => {
    // Arrange + Act
    const store = newStore();
    // Assert
    expect(store.state.claudeSessionId).toBe("");
  });

  it("reset discards all state so a successor hello is a fresh join", () => {
    // Arrange — history applied, cursor advanced.
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }, 1));
    // Act
    store.reset();
    // Assert — a stale lastSeq would make the successor's replay look
    // like a gap-fill and splice two conversations together.
    expect(store.state.lastSeq).toBe(0);
    expect(store.state.items).toHaveLength(0);
    expect(store.state.sessionId).toBe("");
    expect(store.replaying).toBe(false);
  });
});

describe("ConversationStore task-timer start", () => {
  /** The daemon's stamp on the turn that started it. */
  const START = "2026-05-24T12:34:56.789Z";

  /** A user-turn frame stamped at TS. */
  function userTurn(ts: string, seq?: number): string {
    return JSON.stringify({
      type: "user-turn",
      seq: seq ?? ++autoSeq,
      ts,
      session_id: "s1",
      request_id: "r1",
      content: [{ type: "text", text: "hi" }],
    });
  }

  /** A result frame closing the turn. */
  function result(): string {
    return frame("result", {
      subtype: "success",
      duration_ms: 5,
      duration_api_ms: 3,
      num_turns: 1,
      total_cost_usd: 0.01,
      usage: { input_tokens: 1, output_tokens: 2 },
      is_error: false,
    });
  }

  it("carries no turn start before any turn runs", () => {
    // Arrange + Act + Assert — the header reads idle.
    expect(newStore().state.turnStartedAt).toBeNull();
  });

  it("takes the turn start from the user-turn frame's daemon stamp", () => {
    // Arrange
    const store = newStore();
    // Act
    store.applyRaw(userTurn(START));
    // Assert — the daemon's clock, not this tab's.
    expect(store.state.turnStartedAt).toBe(START);
  });

  it("clears the turn start at the result", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(userTurn(START));
    // Act
    store.applyRaw(result());
    // Assert
    expect(store.state.turnStartedAt).toBeNull();
  });

  it("clears the turn start on an unrecoverable error", () => {
    // Arrange — a dead shim sends no result, so the error is the turn's end.
    const store = newStore();
    store.applyRaw(userTurn(START));
    // Act
    store.applyRaw(frame("error", { code: "shim_died", message: "gone", recoverable: false }));
    // Assert
    expect(store.state.turnStartedAt).toBeNull();
  });

  it("keeps the turn start through a recoverable error", () => {
    // Arrange — the turn is still running, so the clock is still running.
    const store = newStore();
    store.applyRaw(userTurn(START));
    // Act
    store.applyRaw(frame("error", { code: "sdk_error", message: "hiccup", recoverable: true }));
    // Assert
    expect(store.state.turnStartedAt).toBe(START);
  });

  it("moves the turn start to a second turn's own stamp", () => {
    // Arrange
    const later = "2026-05-24T12:40:00.000Z";
    const store = newStore();
    store.applyRaw(userTurn(START));
    store.applyRaw(result());
    // Act
    store.applyRaw(userTurn(later));
    // Assert
    expect(store.state.turnStartedAt).toBe(later);
  });

  it("clears the turn start when a fresh join discards local state", () => {
    // Arrange
    autoSeq = 0;
    const store = new ConversationStore();
    store.applyRaw(userTurn(START, 1));
    // Act — an eviction rebuild (the retained window has moved past our
    // history) throws the old conversation away.
    store.applyRaw(hello({ seq: 9, resume_from_seq: 5 }));
    // Assert
    expect(store.state.turnStartedAt).toBeNull();
  });

  it("keeps the turn start across a reconnect that keeps local history", () => {
    // Arrange — the socket dropped mid-turn and the daemon still retains it.
    autoSeq = 0;
    const store = new ConversationStore();
    store.applyRaw(userTurn(START, 1));
    // Act
    store.applyRaw(hello({ seq: 1, resume_from_seq: 1 }));
    // Assert — the turn never stopped running, so the clock never stopped.
    expect(store.state.turnStartedAt).toBe(START);
  });

  it("resumes a mid-turn reconnect at the turn's original stamp", () => {
    // Arrange — a live session's second tab: a fresh join whose replay
    // carries a turn that never resulted, and whose hello reports the turn
    // genuinely still running.
    autoSeq = 0;
    const store = new ConversationStore();
    store.applyRaw(hello({ seq: 1, resume_from_seq: 1, turn_active: true }));
    // Act — the retained user-turn keeps the stamp the daemon gave it.
    store.applyRaw(userTurn(START, 1));
    // Assert — the count picks up where the turn really is, not at zero.
    expect(store.state.turnStartedAt).toBe(START);
  });

  it("drops a phantom turn a transcript-seeded fresh join replays", () => {
    // Arrange — a cold/rehydrated session (§2.11): the replay's trailing
    // user-turn never resulted (transcripts carry no `result`), and the
    // daemon's hello reports no turn actually running.
    autoSeq = 0;
    const store = new ConversationStore();
    store.applyRaw(hello({ seq: 1, resume_from_seq: 1, turn_active: false }));
    // Act — the replay reaches its watermark on the dangling user-turn.
    store.applyRaw(userTurn(START, 1));
    // Assert — no phantom clock counting from a days-old prompt.
    expect(store.state.turnStartedAt).toBeNull();
    expect(store.state.turnInFlight).toBe(false);
  });

  it("keeps a phantom turn's clear across a longer transcript replay", () => {
    // Arrange — the dangling user-turn is not the LAST replayed frame (a
    // trailing usage frame follows it, as BuildReplayFrames appends), so the
    // reconcile must survive frames applied after the turn.
    autoSeq = 0;
    const store = new ConversationStore();
    store.applyRaw(hello({ seq: 2, resume_from_seq: 1, turn_active: false }));
    store.applyRaw(userTurn(START, 1));
    // Act — the usage frame reaches the watermark.
    store.applyRaw(
      frame("usage", { message_id: "m1", usage: { input_tokens: 5, output_tokens: 2 } }, 2),
    );
    // Assert
    expect(store.state.turnStartedAt).toBeNull();
    expect(store.state.turnInFlight).toBe(false);
  });

  it("does not reconcile a gap-fill reconnect against turn_active", () => {
    // Arrange — a mid-turn client whose socket dropped and whose history the
    // daemon still retains: a gap-fill, NOT a fresh join. Its missed frames
    // carry the real end of the turn, so the phantom-turn reconcile is scoped
    // to fresh joins and must not fire here.
    autoSeq = 0;
    const store = new ConversationStore();
    store.applyRaw(userTurn(START, 1));
    // Act — reconnect to a daemon two frames ahead; the gap frames are the
    // turn's ongoing stream, not a result.
    store.applyRaw(hello({ seq: 3, resume_from_seq: 1, turn_active: false }));
    store.applyRaw(frame("text-start", { block_id: "b1", message_id: "m1" }, 2));
    store.applyRaw(frame("text-end", { block_id: "b1", message_id: "m1", final_text: "hi" }, 3));
    // Assert — the clock kept running; only a fresh join reconciles.
    expect(store.state.turnStartedAt).toBe(START);
    expect(store.state.turnInFlight).toBe(true);
  });

  it("clears the turn start when the view is reset onto a successor session", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(userTurn(START));
    // Act
    store.reset();
    // Assert
    expect(store.state.turnStartedAt).toBeNull();
  });
});

describe("ConversationStore final-response chip elapsed", () => {
  /** A user-turn frame carrying the daemon's stamp TS. */
  function userTurnAt(ts: string): string {
    return JSON.stringify({
      type: "user-turn",
      seq: ++autoSeq,
      ts,
      session_id: "s1",
      request_id: "r" + autoSeq,
      content: [{ type: "text", text: "hi" }],
    });
  }

  /** A result frame stamped at TS, closing a turn with the given subtype. */
  function resultAt(ts: string, subtype = "success"): string {
    return JSON.stringify({
      type: "result",
      seq: ++autoSeq,
      ts,
      session_id: "s1",
      subtype,
      // Distinct from every elapsed delta below, so a chip that read the
      // SDK figure instead of the timestamp delta would stand out.
      duration_ms: 999,
      duration_api_ms: 3,
      num_turns: 1,
      total_cost_usd: 0.01,
      usage: { input_tokens: 1, output_tokens: 2 },
      is_error: subtype !== "success",
    });
  }

  it("measures the first final response from the turn's own start", () => {
    // Arrange
    const store = newStore();
    // Act — 30s from the prompt to the answer.
    store.applyRaw(userTurnAt("2026-07-13T12:00:00.000Z"));
    store.applyRaw(resultAt("2026-07-13T12:00:30.000Z"));
    // Assert — the first response has no prior anchor, so it reads the whole
    // task time so far, not the SDK's duration_ms.
    expect(resultItems(store)[0].sincePrevFinalMs).toBe(30_000);
  });

  it("measures a second final response from the first, not the task start", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(userTurnAt("2026-07-13T12:00:00.000Z"));
    store.applyRaw(resultAt("2026-07-13T12:00:30.000Z"));
    // Act — a follow-up turn, answered 40s after the first answer.
    store.applyRaw(userTurnAt("2026-07-13T12:00:35.000Z"));
    store.applyRaw(resultAt("2026-07-13T12:01:10.000Z"));
    // Assert — 12:01:10 − 12:00:30, the delta since the first final response.
    expect(resultItems(store)[1].sincePrevFinalMs).toBe(40_000);
  });

  it("keeps the anchor on the previous final response, not the intervening prompt", () => {
    // Arrange — a first answer, then a queued follow-up picked up much later.
    const store = newStore();
    store.applyRaw(userTurnAt("2026-07-13T12:00:00.000Z"));
    store.applyRaw(resultAt("2026-07-13T12:00:30.000Z"));
    // Act — the follow-up prompt lands at 12:00:50, answered at 12:01:10.
    store.applyRaw(userTurnAt("2026-07-13T12:00:50.000Z"));
    store.applyRaw(resultAt("2026-07-13T12:01:10.000Z"));
    // Assert — 40s from the first result, NOT 20s from the follow-up prompt:
    // a queued or follow-up prompt never advances the anchor.
    expect(resultItems(store)[1].sincePrevFinalMs).toBe(40_000);
  });

  it("resets the anchor to an interrupt so the next answer's clock starts fresh", () => {
    // Arrange — a first answer, then an interrupted turn, then a real answer.
    const store = newStore();
    store.applyRaw(userTurnAt("2026-07-13T12:00:00.000Z"));
    store.applyRaw(resultAt("2026-07-13T12:00:30.000Z"));
    store.applyRaw(userTurnAt("2026-07-13T12:00:40.000Z"));
    store.applyRaw(resultAt("2026-07-13T12:00:50.000Z", "aborted"));
    // Act
    store.applyRaw(userTurnAt("2026-07-13T12:01:00.000Z"));
    store.applyRaw(resultAt("2026-07-13T12:01:30.000Z"));
    // Assert — 40s from the interrupt (12:00:50), NOT 60s across it: the
    // interrupt is a boundary the next answer's clock resets from.
    expect(resultItems(store)[2].sincePrevFinalMs).toBe(40_000);
  });

  it("measures the interrupt's own chip from the previous answer, not itself", () => {
    // Arrange — a first answer, then an interrupted turn.
    const store = newStore();
    store.applyRaw(userTurnAt("2026-07-13T12:00:00.000Z"));
    store.applyRaw(resultAt("2026-07-13T12:00:30.000Z"));
    // Act — the interrupted turn ends 20s after the first answer.
    store.applyRaw(userTurnAt("2026-07-13T12:00:40.000Z"));
    store.applyRaw(resultAt("2026-07-13T12:00:50.000Z", "aborted"));
    // Assert — the abort advances the anchor only AFTER its own chip is
    // measured, so it reads 20s from the prior answer, not 0 from itself.
    expect(resultItems(store)[1].sincePrevFinalMs).toBe(20_000);
  });

  it("does not let a retracted turn advance the anchor", () => {
    // Arrange — a first answer sets the anchor.
    const store = newStore();
    store.applyRaw(userTurnAt("2026-07-13T12:00:00.000Z"));
    store.applyRaw(resultAt("2026-07-13T12:00:30.000Z"));
    // A follow-up prompt is withdrawn: sent, retracted, then its aborted result.
    store.applyRaw(
      frame("user-turn", {
        request_id: "rr",
        ts: "2026-07-13T12:00:40.000Z",
        content: [{ type: "text", text: "oops" }],
      }),
    );
    store.applyRaw(
      frame("user-turn-retracted", { request_id: "rr", ts: "2026-07-13T12:00:45.000Z" }),
    );
    store.applyRaw(resultAt("2026-07-13T12:00:50.000Z", "aborted"));
    // Act — a real answer follows.
    store.applyRaw(userTurnAt("2026-07-13T12:01:00.000Z"));
    store.applyRaw(resultAt("2026-07-13T12:01:30.000Z"));
    // Assert — the retracted turn never happened, so it left no clock mark:
    // 60s from the first real answer (12:00:30), across the retraction.
    expect(resultItems(store)[1].sincePrevFinalMs).toBe(60_000);
  });

  it("computes the same elapsed for a tab that joins mid-session as one present all along", () => {
    // Arrange — the daemon's stamps for a two-answer session.
    const ut1 = "2026-07-13T12:00:00.000Z";
    const r1 = "2026-07-13T12:00:30.000Z";
    const ut2 = "2026-07-13T12:00:50.000Z";
    const r2 = "2026-07-13T12:01:10.000Z";
    const live = newStore();
    live.applyRaw(userTurnAt(ut1));
    live.applyRaw(resultAt(r1));
    live.applyRaw(userTurnAt(ut2));
    live.applyRaw(resultAt(r2));
    // A tab that joins mid-session: a fresh join, then the same frames replayed.
    autoSeq = 0;
    const joined = new ConversationStore();
    joined.applyRaw(hello({ seq: 4, resume_from_seq: 1 }));
    joined.applyRaw(userTurnAt(ut1));
    joined.applyRaw(resultAt(r1));
    joined.applyRaw(userTurnAt(ut2));
    joined.applyRaw(resultAt(r2));
    // Act + Assert — derived from the daemon stamps, so the two agree.
    expect(resultItems(joined)[1].sincePrevFinalMs).toBe(
      resultItems(live)[1].sincePrevFinalMs,
    );
  });

  it("falls back to the SDK's whole-task figure when the turn's start is unknown", () => {
    // Arrange — a result with no preceding user-turn (e.g. a replay that
    // began after the turn's start): no timestamp anchor to measure from.
    const store = newStore();
    // Act
    store.applyRaw(resultAt("2026-07-13T12:00:30.000Z"));
    // Assert — the SDK's duration_ms stands in.
    expect(resultItems(store)[0].sincePrevFinalMs).toBe(999);
  });
});

describe("ConversationStore in-flight queue", () => {
  /** A queue-added frame parking a message under the given ids. */
  function queueAdded(fields: Record<string, unknown> = {}): string {
    return frame("queue-added", {
      queue_id: "q1",
      request_id: "r2",
      content: [{ type: "text", text: "do this later" }],
      status: "classifying",
      ...fields,
    });
  }

  it("appends a classifying item on queue-added", () => {
    // Arrange
    const store = newStore();
    // Act
    store.applyRaw(queueAdded());
    // Assert — a queued message is NOT a conversation item.
    expect(store.state.items).toHaveLength(0);
    expect(store.state.queued).toEqual([
      {
        queue_id: "q1",
        request_id: "r2",
        content: [{ type: "text", text: "do this later" }],
        status: "classifying",
      },
    ]);
  });

  it("dedupes queue-added by queue_id so a replay re-send never doubles it", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(queueAdded());
    // Act — the same item re-broadcast under a fresh seq (the seeded-then-
    // replayed path), which the seq gate would let through.
    store.applyRaw(frame("queue-added", {
      queue_id: "q1",
      request_id: "r2",
      content: [{ type: "text", text: "do this later" }],
      status: "classifying",
    }));
    // Assert
    expect(store.state.queued).toHaveLength(1);
  });

  it("marks a wait classification as waiting and records the verdict", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(queueAdded());
    // Act
    store.applyRaw(frame("queue-classified", {
      queue_id: "q1",
      verdict: "wait",
      reason: "unrelated to the running task",
      source: "classifier",
    }));
    // Assert
    expect(store.state.queued[0]).toMatchObject({
      status: "waiting",
      verdict: "wait",
      reason: "unrelated to the running task",
    });
  });

  it("marks an interrupt classification as interrupt", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(queueAdded());
    // Act
    store.applyRaw(frame("queue-classified", {
      queue_id: "q1",
      verdict: "interrupt",
      reason: "edits the same file",
      source: "classifier",
    }));
    // Assert
    expect(store.state.queued[0]).toMatchObject({ status: "interrupt", verdict: "interrupt" });
  });

  it("discards a verdict whose item already left the queue", () => {
    // Arrange — no queued item under this id (drained/cancelled).
    const store = newStore();
    // Act
    store.applyRaw(frame("queue-classified", {
      queue_id: "gone",
      verdict: "wait",
      reason: "x",
      source: "classifier",
    }));
    // Assert
    expect(store.state.queued).toHaveLength(0);
  });

  it("removes a queued item by queue_id on queue-removed", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(queueAdded());
    // Act
    store.applyRaw(frame("queue-removed", { queue_id: "q1", reason: "cancelled" }));
    // Assert
    expect(store.state.queued).toHaveLength(0);
  });

  it("drops a parked item on queue-removed(interrupted)", () => {
    // Arrange — a user interrupt clears the daemon queue (§2.13).
    const store = newStore();
    store.applyRaw(queueAdded());
    // Act
    store.applyRaw(frame("queue-removed", { queue_id: "q1", reason: "interrupted" }));
    // Assert
    expect(store.state.queued).toHaveLength(0);
  });

  it("seeds the queue from the hello snapshot on a fresh join", () => {
    // Arrange
    autoSeq = 0;
    const store = new ConversationStore();
    const item = {
      queue_id: "q1",
      request_id: "r2",
      content: [{ type: "text", text: "later" }],
      status: "waiting",
      verdict: "wait",
      reason: "busy",
    };
    // Act
    store.applyRaw(hello({ queue: [item] }));
    // Assert
    expect(store.state.queued).toEqual([item]);
  });

  it("clears a seeded queue on an eviction rebuild that carries none", () => {
    // Arrange — a queue built up, then a §2.10 eviction rebuild hello.
    const store = newStore();
    store.applyRaw(queueAdded());
    // Act — resume target past our history and no queue in the hello.
    store.applyRaw(hello({ seq: 60, resume_from_seq: 50 }));
    // Assert
    expect(store.state.queued).toHaveLength(0);
  });

  it("never sets turnInFlight from a queued frame", () => {
    // Arrange — a store with no turn running.
    const store = newStore();
    // Act — the whole queued lifecycle.
    store.applyRaw(queueAdded());
    store.applyRaw(frame("queue-classified", {
      queue_id: "q1",
      verdict: "interrupt",
      reason: "x",
      source: "user",
    }));
    store.applyRaw(frame("queue-removed", { queue_id: "q1", reason: "cancelled" }));
    // Assert — only real user-turn/result frames move it.
    expect(store.state.turnInFlight).toBe(false);
  });

  it("converts a drained queued item into a normal user turn", () => {
    // Arrange — a turn in flight, a message queued behind it.
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [{ type: "text", text: "first" }] }));
    store.applyRaw(queueAdded({ content: [{ type: "text", text: "second" }] }));
    store.applyRaw(frame("queue-classified", {
      queue_id: "q1",
      verdict: "wait",
      reason: "unrelated",
      source: "classifier",
    }));
    // Act — the turn ends; the daemon drains: it removes the parked item
    // (reason "drained", carrying the request_id) and broadcasts the
    // user-turn it became.
    store.applyRaw(resultFrame());
    store.applyRaw(frame("queue-removed", { queue_id: "q1", reason: "drained", request_id: "r2" }));
    store.applyRaw(frame("user-turn", { request_id: "r2", content: [{ type: "text", text: "second" }] }));
    // Assert — the queue is empty and the message is now a real user turn.
    expect(store.state.queued).toHaveLength(0);
    const turns = store.state.items.filter((i): i is UserTurnItem => i.kind === "user-turn");
    expect(turns.map((t) => t.requestId)).toEqual(["r1", "r2"]);
    expect(store.state.turnInFlight).toBe(true);
  });
});

describe("ConversationStore result timestamps", () => {
  it("stamps the tool item with its result frame's envelope time", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(
      frame("tool-use-start", { tool_use_id: "t1", tool_name: "Bash", message_id: "m1" }),
    );
    // Act
    store.applyRaw(
      frame("tool-use-result", { tool_use_id: "t1", is_error: false, content: "ok" }),
    );
    // Assert — frame() stamps every envelope with ts "T".
    expect((store.state.items[0] as ToolItem).resultTs).toBe("T");
  });
});

describe("ConversationStore task output", () => {
  it("appends streamed task output onto the spawning card", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(
      frame("tool-use-start", { tool_use_id: "t1", tool_name: "Bash", message_id: "m1" }),
    );
    // Act
    store.applyRaw(frame("task-output-delta", { task_id: "bg1", tool_use_id: "t1", text: "a" }));
    store.applyRaw(frame("task-output-delta", { task_id: "bg1", tool_use_id: "t1", text: "b" }));
    // Assert
    expect((store.state.items[0] as ToolItem).taskOutput).toBe("ab");
  });

  it("drops a delta naming no card the feed holds", () => {
    // Arrange
    const store = newStore();
    // Act + Assert — no crash, no item invented.
    store.applyRaw(frame("task-output-delta", { task_id: "bg1", tool_use_id: "t9", text: "x" }));
    expect(store.state.items).toHaveLength(0);
  });
});

describe("ConversationStore async-source", () => {
  /** Open the spawning card an async-source attaches to. */
  function withAgentCard(store: ConversationStore): void {
    store.applyRaw(
      frame("tool-use-start", { tool_use_id: "t1", tool_name: "Agent", message_id: "m1" }),
    );
  }

  const agentSource = {
    source_id: "a9",
    kind: "agent",
    label: "find the thing",
    status: "running",
    stream: { transport: "poll", format: "jsonl-transcript" },
  };

  it("lands the descriptor on the spawning card", () => {
    // Arrange
    const store = newStore();
    withAgentCard(store);
    // Act
    store.applyRaw(frame("async-source", { tool_use_id: "t1", source: agentSource }));
    // Assert
    expect((store.state.items[0] as ToolItem).asyncSource).toMatchObject({
      source_id: "a9",
      kind: "agent",
    });
  });

  it("keeps the stream ref, which is what selects the fold's renderer", () => {
    // Arrange
    const store = newStore();
    withAgentCard(store);
    // Act
    store.applyRaw(frame("async-source", { tool_use_id: "t1", source: agentSource }));
    // Assert
    expect((store.state.items[0] as ToolItem).asyncSource?.stream).toEqual({
      transport: "poll",
      format: "jsonl-transcript",
    });
  });

  it("leaves a card that spawned nothing without a source", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(
      frame("tool-use-start", { tool_use_id: "t1", tool_name: "Read", message_id: "m1" }),
    );
    // Act — nothing arrives.
    // Assert
    expect((store.state.items[0] as ToolItem).asyncSource).toBeUndefined();
  });

  it("drops a source naming no card the feed holds", () => {
    // Arrange
    const store = newStore();
    // Act + Assert — no crash, no item invented.
    store.applyRaw(frame("async-source", { tool_use_id: "t9", source: agentSource }));
    expect(store.state.items).toHaveLength(0);
  });

  it("reads a status outside the closed enum as running", () => {
    // Arrange — a newer daemon paired with this client.
    const store = newStore();
    withAgentCard(store);
    // Act
    store.applyRaw(
      frame("async-source", {
        tool_use_id: "t1",
        source: { ...agentSource, status: "reticulating_splines" },
      }),
    );
    // Assert — a wrong "done" hides live output; a wrong "running" only spins.
    expect((store.state.items[0] as ToolItem).asyncSource?.status).toBe("running");
  });

  it("keeps a terminal status the enum does know", () => {
    // Arrange
    const store = newStore();
    withAgentCard(store);
    // Act
    store.applyRaw(
      frame("async-source", { tool_use_id: "t1", source: { ...agentSource, status: "killed" } }),
    );
    // Assert
    expect((store.state.items[0] as ToolItem).asyncSource?.status).toBe("killed");
  });
});

describe("ConversationStore interrupt gating", () => {
  /** Start a turn and a running Bash, then interrupt: the common arrange. */
  function interruptedWithBash(store: ConversationStore): void {
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }));
    store.applyRaw(frame("tool-use-start", { tool_use_id: "t1", tool_name: "Bash", message_id: "m1" }));
    store.applyRaw(frame("interrupt"));
  }

  function toolItems(store: ConversationStore): ToolItem[] {
    return store.state.items.filter((i): i is ToolItem => i.kind === "tool");
  }

  it("enters the interrupting state when a turn is in flight", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }));
    // Act
    store.applyRaw(frame("interrupt"));
    // Assert
    expect(store.state.interrupting).toBe(true);
  });

  it("ignores an interrupt frame when no turn is in flight", () => {
    // Arrange — idle session: nothing to interrupt.
    const store = newStore();
    // Act
    store.applyRaw(frame("interrupt"));
    // Assert
    expect(store.state.interrupting).toBe(false);
  });

  it("clears the interrupting state at the terminating result", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }));
    store.applyRaw(frame("interrupt"));
    // Act — the aborted result the interrupt was waiting on.
    store.applyRaw(resultFrame({ subtype: "aborted", result_text: "" }));
    // Assert
    expect(store.state.interrupting).toBe(false);
  });

  it("clears the interrupting state when the next turn starts", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }));
    store.applyRaw(frame("interrupt"));
    // Act — a queue-run-now preemption drains its promoted message here.
    store.applyRaw(frame("user-turn", { request_id: "r2", content: [] }));
    // Assert
    expect(store.state.interrupting).toBe(false);
  });

  it("clears the interrupting state on a non-recoverable error", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }));
    store.applyRaw(frame("interrupt"));
    // Act
    store.applyRaw(frame("error", { code: "shim_death", message: "gone", recoverable: false }));
    // Assert
    expect(store.state.interrupting).toBe(false);
  });

  it("drops a new main-chain text bubble while interrupting", () => {
    // Arrange
    const store = newStore();
    interruptedWithBash(store);
    // Act — the aborting turn tries to open a fresh assistant text bubble.
    store.applyRaw(frame("text-start", { block_id: "b1", message_id: "m2" }));
    // Assert — no text item entered the feed.
    expect(store.state.items.some((i) => i.kind === "text")).toBe(false);
  });

  it("drops a new main-chain thinking bubble while interrupting", () => {
    // Arrange
    const store = newStore();
    interruptedWithBash(store);
    // Act
    store.applyRaw(frame("thinking-start", { block_id: "b1", message_id: "m2" }));
    // Assert
    expect(store.state.items.some((i) => i.kind === "thinking")).toBe(false);
  });

  it("drops a new-shape tool bubble while interrupting", () => {
    // Arrange — current run is Bash; a Read would open a new bubble.
    const store = newStore();
    interruptedWithBash(store);
    // Act
    store.applyRaw(frame("tool-use-start", { tool_use_id: "t2", tool_name: "Read", message_id: "m2" }));
    // Assert — only the pre-interrupt Bash card exists.
    expect(toolItems(store)).toHaveLength(1);
    expect(toolItems(store)[0].toolName).toBe("Bash");
  });

  it("keeps a tool that continues the current run while interrupting", () => {
    // Arrange — a second consecutive Bash nests into the same tabbed bubble.
    const store = newStore();
    interruptedWithBash(store);
    // Act
    store.applyRaw(frame("tool-use-start", { tool_use_id: "t2", tool_name: "Bash", message_id: "m2" }));
    // Assert — both Bash cards are present (same-name run continues).
    expect(toolItems(store)).toHaveLength(2);
  });

  it("keeps a tool nested under an existing card while interrupting", () => {
    // Arrange — a subagent card exists; its nested tool streams within it.
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }));
    store.applyRaw(frame("tool-use-start", { tool_use_id: "a1", tool_name: "Agent", message_id: "m1" }));
    store.applyRaw(frame("interrupt"));
    // Act
    store.applyRaw(
      frame("tool-use-start", {
        tool_use_id: "n1",
        tool_name: "Read",
        message_id: "m2",
        parent_tool_use_id: "a1",
      }),
    );
    // Assert — the nested tool is kept (within the existing bubble).
    expect(toolItems(store).some((t) => t.toolUseId === "n1")).toBe(true);
  });

  it("still streams a result into a pre-interrupt tool while interrupting", () => {
    // Arrange
    const store = newStore();
    interruptedWithBash(store);
    // Act — the in-flight Bash settles after the interrupt registered.
    store.applyRaw(frame("tool-use-result", { tool_use_id: "t1", is_error: false, content: "done" }));
    // Assert — the update landed within the existing bubble.
    expect(toolItems(store)[0].result).toMatchObject({ isError: false });
  });
});

describe("liveContextDelta", () => {
  /** A user-turn frame beginning a fresh turn (no new request yet). */
  function userTurnFrame(): string {
    return frame("user-turn", {
      request_id: "r1",
      content: [{ type: "text", text: "hi" }],
    });
  }

  it("reads the growth from zero before any result has landed", () => {
    // Arrange — the session's first request, no prior result to measure from.
    const store = newStore();
    // Act
    store.applyRaw(usageFrame(50_000));
    // Assert
    expect(liveContextDelta(store.state)).toBe(50_000);
  });

  it("reads zero at a fresh turn's start, before its first request lands", () => {
    // Arrange — a completed turn left the context standing at 200k.
    const store = newStore();
    store.applyRaw(usageFrame(200_000));
    store.applyRaw(resultFrame());
    // Act — the next turn opens; no new request has grown the context yet.
    store.applyRaw(userTurnFrame());
    // Assert — the running tally starts each task at zero (a pure delta).
    expect(liveContextDelta(store.state)).toBe(0);
  });

  it("grows as the running turn's requests enlarge the context", () => {
    // Arrange — a turn settled with the context at 200k.
    const store = newStore();
    store.applyRaw(usageFrame(200_000));
    store.applyRaw(resultFrame());
    store.applyRaw(userTurnFrame());
    // Act — a request in the new turn pushes the context to 250k.
    store.applyRaw(usageFrame(250_000));
    // Assert — the tally is the growth over the last result, not the total.
    expect(liveContextDelta(store.state)).toBe(50_000);
  });

  it("measures from zero when the last result cleared its standing", () => {
    // Arrange — a /clear zeroed the last result's context, so a growth
    // measures from zero (the same baseline resultContext uses).
    const store = newStore();
    store.applyRaw(usageFrame(200_000));
    store.applyRaw(frame("system", { subtype: "init", data: {} }));
    store.applyRaw(resultFrame());
    // Act — the next request declares a fresh 30k context.
    store.applyRaw(usageFrame(30_000));
    // Assert
    expect(liveContextDelta(store.state)).toBe(30_000);
  });

  it("is unknown while the context size is unknown", () => {
    // Arrange — a /clear invalidates the standing until the next request.
    const store = newStore();
    store.applyRaw(usageFrame(200_000));
    // Act
    store.applyRaw(frame("system", { subtype: "init", data: {} }));
    // Assert — a dash, never a lying zero.
    expect(liveContextDelta(store.state)).toBeNull();
  });
});

describe("ConversationStore session usage tallies", () => {
  it("starts with no top-level tally, which renders as a dash not a zero", () => {
    // Arrange + Act
    const store = newStore();
    // Assert
    expect(topLevelUsage(store.state)).toBeNull();
  });

  it("adopts a real result's usage as the tally baseline", () => {
    // Arrange
    const store = newStore();
    // Act — results carry session-cumulative snapshots.
    store.applyRaw(resultFrame({ usage: { input_tokens: 10, output_tokens: 20 } }));
    // Assert
    expect(topLevelUsage(store.state)).toEqual({ input_tokens: 10, output_tokens: 20 });
  });

  it("supersedes the baseline with the next result instead of adding", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(resultFrame({ usage: { input_tokens: 10, output_tokens: 20 } }));
    // Act — the later snapshot already contains the earlier spend.
    store.applyRaw(resultFrame({ usage: { input_tokens: 15, output_tokens: 30 } }));
    // Assert
    expect(topLevelUsage(store.state)).toEqual({ input_tokens: 15, output_tokens: 30 });
  });

  it("moves the tally mid-turn as usage frames stream in", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(resultFrame({ usage: { input_tokens: 10, output_tokens: 20 } }));
    // Act — two REQUESTS (distinct messages) of the running turn.
    store.applyRaw(frame("usage", { message_id: "m1", usage: { input_tokens: 3, output_tokens: 1 } }));
    store.applyRaw(frame("usage", { message_id: "m2", usage: { input_tokens: 4, output_tokens: 2 } }));
    // Assert — distinct messages add onto the baseline.
    expect(topLevelUsage(store.state)).toMatchObject({ input_tokens: 17, output_tokens: 23 });
  });

  it("field-wise maxes repeated usage frames of one message rather than adding", () => {
    // Arrange
    const store = newStore();
    // Act — message_start then message_delta of the SAME message: fields
    // are cumulative within a message, so adding would double-count.
    store.applyRaw(frame("usage", { message_id: "m1", usage: { input_tokens: 5, output_tokens: 1 } }));
    store.applyRaw(frame("usage", { message_id: "m1", usage: { input_tokens: 5, output_tokens: 9 } }));
    // Assert
    expect(topLevelUsage(store.state)).toMatchObject({ input_tokens: 5, output_tokens: 9 });
  });

  it("drops the live increments once a result folds them into its snapshot", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("usage", { message_id: "m1", usage: { input_tokens: 5, output_tokens: 1 } }));
    // Act — the closing result's cumulative figure includes m1's spend.
    store.applyRaw(resultFrame({ usage: { input_tokens: 12, output_tokens: 4 } }));
    // Assert — counting m1 again on top would double it.
    expect(topLevelUsage(store.state)).toEqual({ input_tokens: 12, output_tokens: 4 });
  });

  it("ignores a zero-usage synthetic replay result instead of wiping the tally", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(resultFrame({ usage: { input_tokens: 10, output_tokens: 20 } }));
    // Act — a transcript-seeded replay closes turns with usage-less results.
    store.applyRaw(resultFrame({ usage: { input_tokens: 0, output_tokens: 0 } }));
    // Assert
    expect(topLevelUsage(store.state)).toEqual({ input_tokens: 10, output_tokens: 20 });
  });

  it("adopts a result's per-model usage map", () => {
    // Arrange
    const store = newStore();
    const modelUsage = {
      "claude-opus-4-8": {
        input_tokens: 30,
        output_tokens: 40,
        cache_creation_input_tokens: 60,
        cache_read_input_tokens: 50,
        web_search_requests: 2,
        cost_usd: 0.12,
        context_window: 1000000,
      },
    };
    // Act
    store.applyRaw(resultFrame({ model_usage: modelUsage }));
    // Assert
    expect(store.state.modelUsage).toEqual(modelUsage);
  });

  it("keeps the prior per-model map when a result carries none", () => {
    // Arrange — a real result with a map, then one without (old shim,
    // synthetic replay result).
    const store = newStore();
    const modelUsage = {
      m: {
        input_tokens: 1,
        output_tokens: 1,
        cache_creation_input_tokens: 0,
        cache_read_input_tokens: 0,
        web_search_requests: 0,
        cost_usd: 0.01,
        context_window: 1,
      },
    };
    store.applyRaw(resultFrame({ model_usage: modelUsage }));
    // Act
    store.applyRaw(resultFrame({}));
    // Assert
    expect(store.state.modelUsage).toEqual(modelUsage);
  });

  it("discards every tally on a fresh-join reset", () => {
    // Arrange — a populated session, then a rebind onto a fresh hello.
    const store = newStore();
    store.applyRaw(frame("usage", { message_id: "m1", usage: { input_tokens: 5, output_tokens: 1 } }));
    store.applyRaw(resultFrame({ usage: { input_tokens: 12, output_tokens: 4 } }));
    store.reset();
    // Act
    store.applyRaw(hello());
    // Assert
    expect(topLevelUsage(store.state)).toBeNull();
    expect(store.state.modelUsage).toBeNull();
  });

  it("zeroes the top-level tally when a /clear re-inits the session", () => {
    // Arrange — a spent session, then the user's /clear turn.
    const store = newStore();
    store.applyRaw(resultFrame({ usage: { input_tokens: 200_000, output_tokens: 5 } }));
    store.applyRaw(clearTurn());
    // Act — the /clear's re-init.
    store.applyRaw(frame("system", { subtype: "init", data: {} }));
    // Assert — the chip dashes (null) rather than keeping the pre-clear figure.
    expect(topLevelUsage(store.state)).toBeNull();
  });

  it("zeroes the per-model map when a /clear re-inits the session", () => {
    // Arrange — a session with a model map, then the user's /clear turn.
    const store = newStore();
    store.applyRaw(resultFrame({ model_usage: sampleModelUsage() }));
    store.applyRaw(clearTurn());
    // Act
    store.applyRaw(frame("system", { subtype: "init", data: {} }));
    // Assert
    expect(store.state.modelUsage).toBeNull();
  });

  it("keeps the top-level tally through a resume init no /clear preceded", () => {
    // Arrange — a resumed session: a normal turn whose usage replay restored.
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [{ type: "text", text: "carry on" }] }));
    store.applyRaw(usageFrame(50_000));
    // Act — the resume's own re-init must not wipe the restored tally.
    store.applyRaw(frame("system", { subtype: "init", data: {} }));
    // Assert
    expect(topLevelUsage(store.state)).toMatchObject({ input_tokens: 50_000 });
  });

  it("keeps the per-model map through a resume init no /clear preceded", () => {
    // Arrange — a resumed session whose last turn was a normal prompt.
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [{ type: "text", text: "carry on" }] }));
    store.applyRaw(resultFrame({ model_usage: sampleModelUsage() }));
    // Act
    store.applyRaw(frame("system", { subtype: "init", data: {} }));
    // Assert
    expect(store.state.modelUsage).toEqual(sampleModelUsage());
  });

  it("leaves the tally alone on an init with no preceding user-turn", () => {
    // Arrange — a cold-start init: a result banked, nothing typed yet.
    const store = newStore();
    store.applyRaw(resultFrame({ usage: { input_tokens: 10, output_tokens: 2 } }));
    // Act — an init arrives with no user-turn in the feed to key a /clear off.
    store.applyRaw(frame("system", { subtype: "init", data: {} }));
    // Assert — no /clear, so the tally stands.
    expect(topLevelUsage(store.state)).toMatchObject({ input_tokens: 10 });
  });
});

describe("ConversationStore turn retraction", () => {
  /** The store's user-turn items, in arrival order. */
  function userTurns(store: ConversationStore): UserTurnItem[] {
    return store.state.items.filter((i): i is UserTurnItem => i.kind === "user-turn");
  }

  /** A prompt sent, then interrupted and retracted before any answer. */
  function retractedTurn(store: ConversationStore): void {
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }));
    store.applyRaw(frame("interrupt"));
    store.applyRaw(frame("user-turn-retracted", { request_id: "r1" }));
  }

  /** The `aborted` result the retracted turn still ends in. */
  function abortedResult(): string {
    return frame("result", {
      subtype: "aborted",
      duration_ms: 10,
      duration_api_ms: 5,
      num_turns: 1,
      total_cost_usd: 0,
      usage: { input_tokens: 1, output_tokens: 0 },
      is_error: false,
    });
  }

  it("drops the retracted turn's bubble", () => {
    // Arrange
    const store = newStore();
    // Act
    retractedTurn(store);
    // Assert — the feed reads as though the prompt was never sent.
    expect(userTurns(store)).toHaveLength(0);
  });

  it("leaves other turns' bubbles standing", () => {
    // Arrange — an earlier turn the agent answered and closed.
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r0", content: [] }));
    store.applyRaw(frame("text-start", { block_id: "b1", message_id: "m1" }));
    // Act — a later prompt is retracted.
    retractedTurn(store);
    // Assert — retraction names one turn and takes only that one.
    expect(userTurns(store).map((t) => t.requestId)).toEqual(["r0"]);
  });

  it("ignores a retraction naming an unknown turn", () => {
    // Arrange
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }));
    // Act — a request id the feed never saw (an evicted or foreign turn).
    store.applyRaw(frame("user-turn-retracted", { request_id: "ghost" }));
    // Assert — no bubble is taken by mistake.
    expect(userTurns(store).map((t) => t.requestId)).toEqual(["r1"]);
  });

  it("keeps suppressing the aborting turn's tail after the retraction", () => {
    // Arrange
    const store = newStore();
    retractedTurn(store);
    // Act — a stray block the SDK had already dispatched.
    store.applyRaw(frame("text-start", { block_id: "b1", message_id: "m1" }));
    // Assert — the prompt is gone, so its tail must not sprout a bubble.
    expect(store.state.items).toHaveLength(0);
  });

  it("renders no result bubble for the retracted turn", () => {
    // Arrange
    const store = newStore();
    retractedTurn(store);
    // Act — the turn still ends in its own aborted result.
    store.applyRaw(abortedResult());
    // Assert — a retracted turn never happened, so it ends in nothing.
    expect(resultItems(store)).toHaveLength(0);
    expect(store.state.items).toHaveLength(0);
  });

  it("still ends the turn on the retracted turn's result", () => {
    // Arrange
    const store = newStore();
    retractedTurn(store);
    // Act
    store.applyRaw(abortedResult());
    // Assert — the result does its bookkeeping even though it renders nothing.
    expect(store.state.turnInFlight).toBe(false);
    expect(store.state.interrupting).toBe(false);
  });

  it("renders the next turn's result normally after a retraction", () => {
    // Arrange — a retracted turn, fully settled.
    const store = newStore();
    retractedTurn(store);
    store.applyRaw(abortedResult());
    // Act — the user revises the prompt and sends it again.
    store.applyRaw(frame("user-turn", { request_id: "r2", content: [] }));
    store.applyRaw(abortedResult());
    // Assert — the retraction is spent, so this turn reports itself.
    expect(resultItems(store)).toHaveLength(1);
  });

  it("renders a result for a turn that opened after the retraction", () => {
    // Arrange — a retraction whose turn has NOT settled yet.
    const store = newStore();
    retractedTurn(store);
    // Act — a fresh prompt supersedes it (a queue drain racing the abort).
    store.applyRaw(frame("user-turn", { request_id: "r2", content: [] }));
    store.applyRaw(abortedResult());
    // Assert — the new turn is its own, so its result is rendered.
    expect(resultItems(store)).toHaveLength(1);
  });

  it("replays to the same feed the live client saw", () => {
    // Arrange — a fresh client rebuilding history from the retained ring.
    const store = newStore();
    // Act — the ring's frames, in seq order.
    retractedTurn(store);
    store.applyRaw(abortedResult());
    // Assert — the retraction is a frame, so replay lands on the same feed.
    expect(store.state.items).toHaveLength(0);
    expect(store.state.turnInFlight).toBe(false);
  });
});
