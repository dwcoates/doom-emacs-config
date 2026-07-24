/**
 * state-adapter — mapping decoded frontend.v1 frames onto the webapp's
 * existing store/render input shapes, and the explicit-ignore path for
 * unsupported shapes. One edge per test (AAA).
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
  SystemItem,
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
  it("converts int64 fields to numbers", () => {
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
        },
      },
    ]);
  });

  it("carries the resume keys (claudeSessionId + cwd) onto the effect", () => {
    const effects = applyOne({
      sessionView: { sessionId: "s1", claudeSessionId: "cli-uuid-2", cwd: "/w" },
    });
    const value = effects[0]!.kind === "session-view" ? effects[0]!.value : undefined;
    expect(value?.claudeSessionId).toBe("cli-uuid-2");
    expect(value?.cwd).toBe("/w");
  });
});

// --- TypingDelta → smooth.ts reveal feed -----------------------------------

describe("TypingDelta mapping", () => {
  it("derives a stable block id from uuid + block index", () => {
    const effects = applyOne({
      typingDelta: { workspace: "ws", sessionId: "s1", uuid: "msg-7", blockIndex: 2, kind: "thinking", delta: "..." },
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
  it("fans out into per-workspace / per-session / per-catalog effects", () => {
    const effects = applyOne({
      snapshot: {
        workspaces: [{ workspace: "w", sessionId: "s", state: "RENDER_STATE_IDLE" }],
        sessions: [{ workspace: "w", sessionId: "s", model: "m" }],
        catalogs: [{ workspace: "w", sessionId: "s", tasks: [] }],
      },
    });
    expect(effects.map((e) => e.kind)).toEqual(["workspace-state", "session-view", "task-catalog"]);
  });
});

// --- ConversationDelta items → the store's bubble/card vocabulary ----------

describe("ConversationDelta item mapping", () => {
  function itemsFrom(item: Record<string, unknown>): ConversationItem[] {
    const effects = applyOne({ conversationDelta: { sessionId: "s1", workspace: "ws", throughSeq: "9", items: [item] } });
    const conv = effects.find((e) => e.kind === "conversation-items");
    if (conv?.kind !== "conversation-items") throw new Error("no conversation-items effect");
    return conv.items;
  }

  it("carries workspace / session / throughSeq through", () => {
    const effects = applyOne({
      conversationDelta: { sessionId: "s1", workspace: "ws", throughSeq: "42", items: [] },
    });
    expect(effects[0]).toMatchObject({ kind: "conversation-items", workspace: "ws", sessionId: "s1", throughSeq: 42 });
  });

  it("builds a user-turn item", () => {
    const expected: UserTurnItem = {
      kind: "user-turn",
      requestId: "r1",
      content: [{ type: "text", text: "hi" }],
      ts: "2026-01-01T00:00:00Z",
    };
    expect(itemsFrom({ ...expected })).toEqual([expected]);
  });

  it("builds a text item", () => {
    const expected: TextItem = {
      kind: "text",
      blockId: "b1",
      messageId: "m1",
      text: "hello",
      done: true,
      ts: "2026-01-01T00:00:00Z",
    };
    expect(itemsFrom({ ...expected })).toEqual([expected]);
  });

  it("builds a thinking item", () => {
    const expected: ThinkingItem = {
      kind: "thinking",
      blockId: "b2",
      messageId: "m1",
      text: "hmm",
      done: false,
    };
    expect(itemsFrom({ ...expected })).toEqual([expected]);
  });

  it("builds a tool item with defaulted input fields", () => {
    const expected: ToolItem = {
      kind: "tool",
      toolUseId: "tu1",
      toolName: "Bash",
      messageId: "m1",
      ts: "2026-01-01T00:00:00Z",
      inputJson: "",
      inputDone: false,
    };
    expect(itemsFrom({ kind: "tool", toolUseId: "tu1", toolName: "Bash", messageId: "m1", ts: expected.ts })).toEqual([
      expected,
    ]);
  });

  it("builds a permission item", () => {
    const expected: PermissionItem = {
      kind: "permission",
      requestId: "pr1",
      toolUseId: "tu1",
      toolName: "Bash",
      input: { command: "ls" },
    };
    expect(itemsFrom({ ...expected })).toEqual([expected]);
  });

  it("builds a result item with a null context", () => {
    const expected: ResultItem = {
      kind: "result",
      subtype: "success",
      durationMs: 100,
      sincePrevFinalMs: 50,
      numTurns: 1,
      totalCostUsd: 0.1,
      usage: { input_tokens: 10, output_tokens: 5 },
      isError: false,
      context: null,
    };
    expect(itemsFrom({ ...expected })).toEqual([expected]);
  });

  it("builds a compact-boundary item", () => {
    const expected: CompactBoundaryItem = { kind: "compact-boundary", trigger: "auto", preTokens: 100, postTokens: 50 };
    expect(itemsFrom({ ...expected })).toEqual([expected]);
  });

  it("builds an error item", () => {
    const expected: ErrorItem = { kind: "error", code: "sdk_error", message: "boom", recoverable: false };
    expect(itemsFrom({ ...expected })).toEqual([expected]);
  });

  it("builds a retry item", () => {
    const expected: RetryItem = { kind: "retry", attempt: 1, reason: "429", fatal: false };
    expect(itemsFrom({ ...expected })).toEqual([expected]);
  });

  it("builds a system item", () => {
    const expected: SystemItem = { kind: "system", subtype: "init" };
    expect(itemsFrom({ ...expected })).toEqual([expected]);
  });

  it("throws loudly on a known item kind missing a required field", () => {
    expect(() => itemsFrom({ kind: "text", messageId: "m1", text: "x", done: true, ts: "t" })).toThrow(
      /text item field `blockId`/,
    );
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

  it("ignores an unknown conversation-item kind, once per distinct name", () => {
    const logs: Array<[AdapterLogLevel, string]> = [];
    const adapter = new StateAdapter((lvl, msg) => logs.push([lvl, msg]));
    const mk = () =>
      frame({ conversationDelta: { sessionId: "s1", items: [{ kind: "mystery-card", blob: 1 }] } });

    const effects = adapter.apply(mk());
    adapter.apply(mk());

    // The supported (empty) item batch plus the explicit ignore of the unknown kind.
    expect(effects).toEqual([
      { kind: "conversation-items", workspace: "", sessionId: "s1", throughSeq: 0, items: [] },
      { kind: "ignored", shape: "conversation-item:mystery-card" },
    ]);
    expect(adapter.ignoredCounts().get("conversation-item:mystery-card")).toBe(2);
    expect(logs.filter(([, m]) => m.includes("mystery-card"))).toHaveLength(1);
  });

  it("still emits supported items alongside an ignored unknown item", () => {
    const effects = applyOne({
      conversationDelta: {
        sessionId: "s1",
        items: [{ kind: "system", subtype: "init" }, { kind: "mystery-card" }],
      },
    });
    expect(effects[0]).toMatchObject({ kind: "conversation-items", items: [{ kind: "system", subtype: "init" }] });
    expect(effects[1]).toEqual({ kind: "ignored", shape: "conversation-item:mystery-card" });
  });
});
