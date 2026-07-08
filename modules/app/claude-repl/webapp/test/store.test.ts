import { describe, expect, it } from "vitest";
import { ConversationStore, PermissionItem, TextItem, ToolItem } from "../src/store.js";

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
    expect(store.state.usage).toEqual({ input_tokens: 1, output_tokens: 2 });
    expect(store.state.costUsd).toBe(0.01);
  });

  it("updates the usage chip from mid-turn usage frames", () => {
    // Arrange
    const store = newStore();
    // Act
    store.applyRaw(frame("usage", {
      message_id: "m1",
      usage: { input_tokens: 10, output_tokens: 20 },
      cost_usd: 0.2,
    }));
    // Assert
    expect(store.state.usage?.output_tokens).toBe(20);
    expect(store.state.costUsd).toBe(0.2);
  });

  it("tracks permission-mode-changed frames", () => {
    // Arrange
    const store = newStore();
    // Act
    store.applyRaw(frame("permission-mode-changed", { mode: "plan", origin: "user" }));
    // Assert
    expect(store.state.permissionMode).toBe("plan");
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
