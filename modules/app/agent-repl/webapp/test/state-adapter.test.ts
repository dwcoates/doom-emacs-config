/**
 * state-adapter — decompose decoded frontend.v1 frames (S9 typed
 * ConversationItem arms) onto the webapp's store/render vocabulary, plus the
 * explicit-ignore path for unsupported shapes. One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import { decodeFrontendFrame } from "../src/frontend-proto.js";
import {
  StateAdapter,
  type AdapterEffect,
  type AdapterLogLevel,
} from "../src/state-adapter.js";
import type {
  CompactBoundaryItem,
  ConversationItem,
  ErrorItem,
  PermissionItem,
  ResultItem,
  RetryItem,
  TextItem,
  ThinkingItem,
  ToolItem,
  UserTurnItem,
} from "../src/store.js";

function frame(obj: unknown): ReturnType<typeof decodeFrontendFrame> {
  return decodeFrontendFrame(JSON.stringify(obj));
}

function applyOne(obj: unknown): AdapterEffect[] {
  return new StateAdapter().apply(frame(obj));
}

/** The store items a single conversation item decomposes into. */
function itemsFrom(item: Record<string, unknown>): ConversationItem[] {
  const effects = applyOne({ conversationDelta: { sessionId: "s1", workspace: "ws", throughSeq: "9", items: [item] } });
  const conv = effects.find((e) => e.kind === "conversation-items");
  if (conv?.kind !== "conversation-items") throw new Error("no conversation-items effect");
  return conv.items;
}

// --- WorkspaceState → status/tail-row input --------------------------------

describe("WorkspaceState mapping", () => {
  it("maps the render state to its keyword and converts inputs", () => {
    const effects = applyOne({
      workspaceState: {
        workspace: "ws-a",
        sessionId: "s1",
        state: "RENDER_STATE_THINKING",
        turnActive: true,
        liveTaskCount: "2",
        mergePhase: "cherry-pick",
      },
    });
    expect(effects).toEqual([
      {
        kind: "workspace-state",
        value: {
          workspace: "ws-a",
          sessionId: "s1",
          state: "thinking",
          turnActive: true,
          liveTaskCount: 2,
          mergePhase: "cherry-pick",
        },
      },
    ]);
  });

  const keywordCases: Array<[string, string]> = [
    ["RENDER_STATE_INIT", "init"],
    ["RENDER_STATE_IDLE", "idle"],
    ["RENDER_STATE_IDLE_ASYNC", "idle_async"],
    ["RENDER_STATE_PERMISSION", "permission"],
    ["RENDER_STATE_MERGE_CONFLICT", "merge_conflict"],
    ["RENDER_STATE_MERGED", "merged"],
    ["RENDER_STATE_DEAD", "dead"],
    ["RENDER_STATE_DEGRADED", "degraded"],
  ];
  for (const [proto, keyword] of keywordCases) {
    it(`maps ${proto} to '${keyword}'`, () => {
      const effects = applyOne({ workspaceState: { workspace: "w", sessionId: "s", state: proto } });
      expect(effects[0]).toMatchObject({ kind: "workspace-state", value: { state: keyword } });
    });
  }
});

// --- SessionView → topbar input --------------------------------------------

describe("SessionView mapping", () => {
  it("converts int64 fields to numbers and carries configDir", () => {
    const effects = applyOne({
      sessionView: {
        workspace: "ws",
        sessionId: "s1",
        model: "claude-opus",
        slug: "slug",
        title: "objective",
        totalTokens: "12000",
        totalCostUsd: 1.25,
        contextWindow: "200000",
        permissionMode: "default",
        shimAttached: true,
        claudeSessionId: "cli-uuid-1",
        cwd: "/work/ws",
        configDir: "/home/u/.claude",
      },
    });
    expect(effects).toEqual([
      {
        kind: "session-view",
        value: {
          workspace: "ws",
          sessionId: "s1",
          model: "claude-opus",
          slug: "slug",
          title: "objective",
          totalTokens: 12000,
          totalCostUsd: 1.25,
          contextWindow: 200000,
          permissionMode: "default",
          shimAttached: true,
          claudeSessionId: "cli-uuid-1",
          cwd: "/work/ws",
          configDir: "/home/u/.claude",
        },
      },
    ]);
  });
});

// --- TypingDelta → smooth.ts reveal feed -----------------------------------

describe("TypingDelta mapping", () => {
  it("derives a stable block id from the content delta's uuid + block index", () => {
    const effects = applyOne({
      typingDelta: { workspace: "ws", sessionId: "s1", delta: { uuid: "msg-7", blockIndex: 2, thinking: "..." } },
    });
    expect(effects).toEqual([
      {
        kind: "typing",
        value: {
          workspace: "ws",
          sessionId: "s1",
          uuid: "msg-7",
          blockIndex: 2,
          kind: "thinking",
          delta: "...",
          blockId: "msg-7:2",
        },
      },
    ]);
  });

  it("ignores a signature content delta (no live preview)", () => {
    const effects = applyOne({ typingDelta: { sessionId: "s1", delta: { uuid: "u1", signature: "sig" } } });
    expect(effects).toEqual([{ kind: "ignored", shape: "content-delta:signature" }]);
  });
});

// --- TaskCatalog → async/task roster (CounterEntry[]) ----------------------

describe("TaskCatalog mapping", () => {
  it("maps a task entry onto the counter vocabulary", () => {
    const effects = applyOne({
      taskCatalog: {
        workspace: "ws",
        sessionId: "s1",
        tasks: [{ taskId: "t1", kind: "agent", description: "explore", status: "running", startedAtMs: "1000" }],
      },
    });
    expect(effects).toEqual([
      {
        kind: "task-catalog",
        value: {
          workspace: "ws",
          sessionId: "s1",
          entries: [{ id: "t1", summary: "explore", detail: "agent", status: "running", nested: false }],
        },
      },
    ]);
  });

  const statusCases: Array<[string, string]> = [
    ["running", "running"],
    ["done", "done"],
    ["stopped", "done"],
    ["error", "error"],
    ["killed", "error"],
    ["lost", "error"],
  ];
  for (const [proto, counter] of statusCases) {
    it(`maps task status '${proto}' to counter status '${counter}'`, () => {
      const effects = applyOne({
        taskCatalog: { sessionId: "s1", tasks: [{ taskId: "t1", kind: "shell", status: proto }] },
      });
      expect(effects[0]).toMatchObject({ kind: "task-catalog", value: { entries: [{ status: counter }] } });
    });
  }
});

// --- SessionInitView → /status snapshot source -----------------------------

describe("SessionInit mapping", () => {
  it("produces a session-init effect carrying the SystemInit payload", () => {
    const effects = applyOne({ sessionInit: { workspace: "ws", sessionId: "s1", init: { model: "claude", cwd: "/w" } } });
    expect(effects).toEqual([
      { kind: "session-init", value: { workspace: "ws", sessionId: "s1", init: { model: "claude", cwd: "/w" } } },
    ]);
  });
});

// --- DegradedNotice → banner -----------------------------------------------

describe("DegradedNotice mapping", () => {
  it("produces a banner input", () => {
    const effects = applyOne({
      degradedNotice: { component: "shim-store", reason: "socket closed", recovered: false, atMs: "1700000000000" },
    });
    expect(effects).toEqual([
      {
        kind: "degraded",
        value: { component: "shim-store", reason: "socket closed", recovered: false, atMs: 1700000000000 },
      },
    ]);
  });
});

// --- StateSnapshot decomposition -------------------------------------------

describe("StateSnapshot mapping", () => {
  it("fans out into per-workspace / per-session / per-catalog / per-init effects", () => {
    const effects = applyOne({
      snapshot: {
        workspaces: [{ workspace: "w", sessionId: "s", state: "RENDER_STATE_IDLE" }],
        sessions: [{ workspace: "w", sessionId: "s", model: "m" }],
        catalogs: [{ workspace: "w", sessionId: "s", tasks: [] }],
        inits: [{ workspace: "w", sessionId: "s", init: { model: "m" } }],
      },
    });
    expect(effects.map((e) => e.kind)).toEqual(["workspace-state", "session-view", "task-catalog", "session-init"]);
  });
});

// --- ConversationItem arms → the store's bubble/card vocabulary -------------

describe("ConversationDelta envelope", () => {
  it("carries workspace / session / throughSeq through", () => {
    const effects = applyOne({
      conversationDelta: { sessionId: "s1", workspace: "ws", throughSeq: "42", items: [] },
    });
    expect(effects[0]).toMatchObject({ kind: "conversation-items", workspace: "ws", sessionId: "s1", throughSeq: 42 });
  });
});

describe("assistantMessage arm", () => {
  it("decomposes content blocks into text/thinking items keyed by uuid:blockIndex", () => {
    const items = itemsFrom({
      uuid: "m1",
      assistantMessage: {
        content: [{ text: { text: "hello" } }, { thinking: { thinking: "hmm", signature: "sig" } }],
      },
    });
    const text: TextItem = { kind: "text", blockId: "m1:0", messageId: "m1", text: "hello", done: true, ts: "" };
    const thinking: ThinkingItem = {
      kind: "thinking",
      blockId: "m1:1",
      messageId: "m1",
      text: "hmm",
      done: true,
      signature: "sig",
    };
    expect(items).toEqual([text, thinking]);
  });

  it("derives the block ts from the envelope tsMs", () => {
    const items = itemsFrom({ uuid: "m1", tsMs: "1700000000000", assistantMessage: { content: [{ text: { text: "x" } }] } });
    expect((items[0] as TextItem).ts).toBe(new Date(1700000000000).toISOString());
  });

  it("decomposes an inline tool_use block into a tool item", () => {
    const items = itemsFrom({ uuid: "m1", assistantMessage: { content: [{ toolUse: { id: "tu1", name: "Bash", input: { command: "ls" } } }] } });
    const tool: ToolItem = {
      kind: "tool",
      toolUseId: "tu1",
      toolName: "Bash",
      messageId: "m1",
      ts: "",
      inputJson: "",
      inputDone: true,
      input: { command: "ls" },
    };
    expect(items).toEqual([tool]);
  });
});

describe("userMessage arm", () => {
  it("maps a string user message to a user-turn item", () => {
    const items = itemsFrom({ uuid: "m1", requestId: "r1", userMessage: { contentString: "hi there" } });
    const expected: UserTurnItem = { kind: "user-turn", requestId: "r1", content: [{ type: "text", text: "hi there" }], ts: "" };
    expect(items).toEqual([expected]);
  });

  it("splits tool_result blocks into tool items and text into one user-turn", () => {
    const items = itemsFrom({
      uuid: "m1",
      requestId: "r1",
      userMessage: {
        contentBlocks: { blocks: [{ toolResult: { toolUseId: "tu1", contentString: "done" } }, { text: { text: "and more" } }] },
      },
    });
    const tool: ToolItem = {
      kind: "tool",
      toolUseId: "tu1",
      toolName: "",
      messageId: "m1",
      ts: "",
      inputJson: "",
      inputDone: true,
      resultTs: "",
      result: { isError: false, content: "done" },
    };
    const turn: UserTurnItem = { kind: "user-turn", requestId: "r1", content: [{ type: "text", text: "and more" }], ts: "" };
    expect(items).toEqual([tool, turn]);
  });

  it("emits no user-turn for a pure tool-result feedback message", () => {
    const items = itemsFrom({
      uuid: "m1",
      requestId: "r1",
      userMessage: { contentBlocks: { blocks: [{ toolResult: { toolUseId: "tu1", contentString: "done" } }] } },
    });
    expect(items).toHaveLength(1);
    expect(items[0].kind).toBe("tool");
  });
});

describe("toolUse / toolResult arms", () => {
  it("builds the tool CALL item from a standalone toolUse arm", () => {
    const items = itemsFrom({ uuid: "m1", toolUse: { id: "tu1", name: "Read", input: { file_path: "/x" } } });
    const expected: ToolItem = {
      kind: "tool",
      toolUseId: "tu1",
      toolName: "Read",
      messageId: "m1",
      ts: "",
      inputJson: "",
      inputDone: true,
      input: { file_path: "/x" },
    };
    expect(items).toEqual([expected]);
  });

  it("builds the result-only tool item (empty toolName) from a toolResult arm", () => {
    const items = itemsFrom({ uuid: "m1", toolResult: { toolUseId: "tu1", contentString: "boom", isError: true } });
    const expected: ToolItem = {
      kind: "tool",
      toolUseId: "tu1",
      toolName: "",
      messageId: "m1",
      ts: "",
      inputJson: "",
      inputDone: true,
      resultTs: "",
      result: { isError: true, content: "boom" },
    };
    expect(items).toEqual([expected]);
  });

  it("flattens a block-list tool result to text-block content", () => {
    const items = itemsFrom({
      uuid: "m1",
      toolResult: { toolUseId: "tu1", contentBlocks: { blocks: [{ text: { text: "a" } }, { text: { text: "b" } }] } },
    });
    expect((items[0] as ToolItem).result?.content).toEqual([
      { type: "text", text: "a" },
      { type: "text", text: "b" },
    ]);
  });
});

describe("result arm", () => {
  it("maps a ResultMessage onto a ResultItem with snake-cased usage", () => {
    const items = itemsFrom({
      uuid: "m1",
      result: {
        subtype: "RESULT_SUBTYPE_SUCCESS",
        durationMs: "100",
        numTurns: 1,
        totalCostUsd: 0.1,
        usage: { inputTokens: "10", outputTokens: "5" },
        isError: false,
      },
    });
    const expected: ResultItem = {
      kind: "result",
      subtype: "success",
      durationMs: 100,
      sincePrevFinalMs: 0,
      numTurns: 1,
      totalCostUsd: 0.1,
      usage: { input_tokens: 10, output_tokens: 5, cache_creation_input_tokens: 0, cache_read_input_tokens: 0 },
      isError: false,
      context: null,
    };
    expect(items).toEqual([expected]);
  });

  it("maps the error-max-turns subtype", () => {
    const items = itemsFrom({ uuid: "m1", result: { subtype: "RESULT_SUBTYPE_ERROR_MAX_TURNS" } });
    expect((items[0] as ResultItem).subtype).toBe("error_max_turns");
  });
});

describe("compact-boundary arms", () => {
  it("maps a stream compactBoundary (postTokens unknown → 0)", () => {
    const items = itemsFrom({ uuid: "m1", compactBoundary: { trigger: "COMPACT_TRIGGER_AUTO", preTokens: "100" } });
    const expected: CompactBoundaryItem = { kind: "compact-boundary", trigger: "auto", preTokens: 100, postTokens: 0 };
    expect(items).toEqual([expected]);
  });

  it("maps a disk compactBoundaryLine's pre + post tokens", () => {
    const items = itemsFrom({ uuid: "m1", compactBoundaryLine: { compactMetadata: { trigger: "manual", preTokens: "200", postTokens: "50" } } });
    const expected: CompactBoundaryItem = { kind: "compact-boundary", trigger: "manual", preTokens: 200, postTokens: 50 };
    expect(items).toEqual([expected]);
  });
});

describe("apiError arm", () => {
  it("maps a retryable api error to a retry item", () => {
    const items = itemsFrom({ uuid: "m1", apiError: { error: { message: "overloaded" }, retryAttempt: 2, maxRetries: 5 } });
    const expected: RetryItem = { kind: "retry", attempt: 2, reason: "overloaded", fatal: false };
    expect(items).toEqual([expected]);
  });

  it("marks a retry fatal once it reaches maxRetries", () => {
    const items = itemsFrom({ uuid: "m1", apiError: { error: { message: "overloaded" }, retryAttempt: 5, maxRetries: 5 } });
    expect((items[0] as RetryItem).fatal).toBe(true);
  });

  it("maps a terminal api error to an error item", () => {
    const items = itemsFrom({ uuid: "m1", apiError: { error: { message: "boom" } } });
    const expected: ErrorItem = { kind: "error", code: "api_error", message: "boom", recoverable: false };
    expect(items).toEqual([expected]);
  });
});

describe("permission arm", () => {
  it("builds a pending permission item (no resolution) from the request", () => {
    const items = itemsFrom({ uuid: "pr1", permission: { request: { requestId: "pr1", toolName: "Bash", input: { command: "ls" } } } });
    const expected: PermissionItem = {
      kind: "permission",
      requestId: "pr1",
      toolUseId: "",
      toolName: "Bash",
      input: { command: "ls" },
    };
    expect(items).toEqual([expected]);
  });

  it("maps RESOLUTION_ALLOWED to an allow decision", () => {
    const items = itemsFrom({ uuid: "pr1", permission: { request: { requestId: "pr1", toolName: "Bash", input: {} }, resolution: "RESOLUTION_ALLOWED" } });
    expect((items[0] as PermissionItem).resolution).toEqual({ decision: "allow" });
  });

  it("maps RESOLUTION_DENIED to a deny decision carrying the deny message", () => {
    const items = itemsFrom({
      uuid: "pr1",
      permission: { request: { requestId: "pr1", toolName: "Bash", input: {} }, resolution: "RESOLUTION_DENIED", denyMessage: "no" },
    });
    expect((items[0] as PermissionItem).resolution).toEqual({ decision: "deny", message: "no" });
  });

  it("maps RESOLUTION_ABANDONED to a cancel decision", () => {
    const items = itemsFrom({ uuid: "pr1", permission: { request: { requestId: "pr1", toolName: "Bash", input: {} }, resolution: "RESOLUTION_ABANDONED" } });
    expect((items[0] as PermissionItem).resolution).toEqual({ decision: "cancel" });
  });
});

// --- explicit-ignore path (unsupported shapes) -----------------------------

describe("explicit-ignore path", () => {
  it("ignores a commandAck frame, counting and logging once per name", () => {
    const logs: Array<[AdapterLogLevel, string]> = [];
    const adapter = new StateAdapter((lvl, msg) => logs.push([lvl, msg]));
    const ack = frame({ commandAck: { requestId: "r1", ok: true } });

    const first = adapter.apply(ack);
    const second = adapter.apply(ack);

    expect(first).toEqual([{ kind: "ignored", shape: "commandAck" }]);
    expect(second).toEqual([{ kind: "ignored", shape: "commandAck" }]);
    expect(adapter.ignoredCounts().get("commandAck")).toBe(2);
    expect(logs.filter(([, m]) => m.includes("commandAck"))).toHaveLength(1);
  });

  it("ignores a daemonView frame (S7 unsupported shape)", () => {
    const adapter = new StateAdapter();
    const dv = frame({
      daemonView: { bootId: "b_x", protocolVersion: "1", daemonBinaryMtimeMs: "1", daemonVersion: "v" },
    });
    expect(adapter.apply(dv)).toEqual([{ kind: "ignored", shape: "daemonView" }]);
  });

  it("ignores a toolUseResult item (no correlation key), emitting an empty batch beside it", () => {
    const adapter = new StateAdapter();
    const effects = adapter.apply(
      frame({ conversationDelta: { sessionId: "s1", items: [{ uuid: "m1", toolUseResult: { rawString: "x" } }] } }),
    );
    expect(effects).toEqual([
      { kind: "conversation-items", workspace: "", sessionId: "s1", throughSeq: 0, items: [] },
      { kind: "ignored", shape: "conversation-item:toolUseResult" },
    ]);
    expect(adapter.ignoredCounts().get("conversation-item:toolUseResult")).toBe(1);
  });

  it("ignores an unrenderable content block, once per distinct name, keeping the rest", () => {
    const logs: Array<[AdapterLogLevel, string]> = [];
    const adapter = new StateAdapter((lvl, msg) => logs.push([lvl, msg]));
    const mk = () =>
      frame({
        conversationDelta: {
          sessionId: "s1",
          items: [{ uuid: "m1", assistantMessage: { content: [{ text: { text: "keep" } }, { image: { source: {} } }] } }],
        },
      });

    const effects = adapter.apply(mk());
    adapter.apply(mk());

    expect(effects[0]).toMatchObject({ kind: "conversation-items", items: [{ kind: "text", text: "keep" }] });
    expect(effects[1]).toEqual({ kind: "ignored", shape: "content-block:image" });
    expect(adapter.ignoredCounts().get("content-block:image")).toBe(2);
    expect(logs.filter(([, m]) => m.includes("content-block:image"))).toHaveLength(1);
  });
});

describe("heartbeat -> tool-progress (E4)", () => {
  it("maps a heartbeat frame to a tool-progress effect", () => {
    // Arrange / Act
    const effects = applyOne({
      heartbeat: {
        workspace: "ws",
        sessionId: "s1",
        progress: { toolUseId: "tu1", toolName: "Bash", elapsedSeconds: 12.5 },
      },
    });

    // Assert
    expect(effects).toEqual([
      {
        kind: "tool-progress",
        value: {
          workspace: "ws",
          sessionId: "s1",
          toolUseId: "tu1",
          toolName: "Bash",
          parentToolUseId: "",
          elapsedSeconds: 12.5,
        },
      },
    ]);
  });

  it("does not route the heartbeat down the explicit-ignore path", () => {
    // Arrange
    const adapter = new StateAdapter();

    // Act
    adapter.apply(
      frame({
        heartbeat: { sessionId: "s1", progress: { toolUseId: "tu1", elapsedSeconds: 1 } },
      }),
    );

    // Assert — a mapped visual must not be counted as an ignored shape.
    expect(adapter.ignoredCounts().get("heartbeat")).toBeUndefined();
  });
});
