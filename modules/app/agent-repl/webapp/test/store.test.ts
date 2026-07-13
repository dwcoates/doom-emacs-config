import { describe, expect, it } from "vitest";
import {
  ConversationStore,
  PermissionItem,
  ResultItem,
  TextItem,
  ToolItem,
  UserTurnItem,
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

  it("records tool progress text", () => {
    // Arrange
    const store = newStore();
    runToolStart(store);
    // Act
    store.applyRaw(frame("tool-use-progress", { tool_use_id: "t1", text: "Bash running (3s)" }));
    // Assert
    expect((store.state.items[0] as ToolItem).progress).toBe("Bash running (3s)");
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

  it("does not enter replaying mode on a reconnect within retention", () => {
    // Arrange — store already has 1..2 applied.
    const store = newStore();
    store.applyRaw(frame("user-turn", { request_id: "r1", content: [] }, 1));
    store.applyRaw(frame("system", { subtype: "init", data: {} }, 2));
    // Act — reconnect; daemon is at 6, retains from 1.
    store.applyRaw(hello({ seq: 6, resume_from_seq: 1 }));
    // Assert — tail fill is incremental, not a restore.
    expect(store.replaying).toBe(false);
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
    // Arrange — a fresh join whose replay carries a turn that never resulted.
    autoSeq = 0;
    const store = new ConversationStore();
    store.applyRaw(hello({ seq: 1, resume_from_seq: 1 }));
    // Act — the retained user-turn keeps the stamp the daemon gave it.
    store.applyRaw(userTurn(START, 1));
    // Assert — the count picks up where the turn really is, not at zero.
    expect(store.state.turnStartedAt).toBe(START);
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

  it("does not let an aborted turn between two answers advance the anchor", () => {
    // Arrange — a first answer, then an interrupted turn, then a real answer.
    const store = newStore();
    store.applyRaw(userTurnAt("2026-07-13T12:00:00.000Z"));
    store.applyRaw(resultAt("2026-07-13T12:00:30.000Z"));
    store.applyRaw(userTurnAt("2026-07-13T12:00:40.000Z"));
    store.applyRaw(resultAt("2026-07-13T12:00:50.000Z", "aborted"));
    // Act
    store.applyRaw(userTurnAt("2026-07-13T12:01:00.000Z"));
    store.applyRaw(resultAt("2026-07-13T12:01:30.000Z"));
    // Assert — measured from the last real answer (12:00:30), across the abort.
    expect(resultItems(store)[2].sincePrevFinalMs).toBe(60_000);
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
