/**
 * state-adapter — decompose decoded frontend.v1 frames (S9 typed
 * ConversationItem arms) onto the webapp's store/render vocabulary, plus the
 * explicit-ignore path for unsupported shapes. One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import { decodeFrontendFrame } from "../src/frontend-proto.js";
import {
  StateAdapter,
  userTurnReceipt,
  type AdapterEffect,
  type AdapterLogLevel,
} from "../src/state-adapter.js";
import type {
  ContextClearedItem,
  ContextCompactedItem,
  ConversationItem,
  PermissionItem,
  ResultItem,
  SystemFailureCard,
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

function workspaceState(over: Record<string, unknown>): Record<string, unknown> {
  return {
    workspace: "ws-a",
    sessionId: "s1",
    state: "RENDER_STATE_READY",
    connectivity: "SESSION_CONNECTIVITY_OPERATIONAL",
    status: "SESSION_STATUS_READY",
    controllerGenerationId: "g1",
    activeFaults: [],
    ...over,
  };
}

/**
 * An ordinary user-driven item envelope. The daemon stamps a source on every
 * item it builds, so a fixture without one is not a wire the daemon can emit —
 * see the provenance-gate suite for the malformed and merge-driven cases.
 */
function userItem(item: Record<string, unknown>): Record<string, unknown> {
  return { source: "CONVERSATION_SOURCE_USER", ...item };
}

/** The store items a single conversation item decomposes into. */
function itemsFrom(item: Record<string, unknown>): ConversationItem[] {
  const effects = applyOne({
    conversationDelta: { sessionId: "s1", workspace: "ws", throughSeq: "9", items: [userItem(item)] },
  });
  const conv = effects.find((e) => e.kind === "conversation-items");
  if (conv?.kind !== "conversation-items") throw new Error("no conversation-items effect");
  return conv.items;
}

// --- WorkspaceState → status/tail-row input --------------------------------

describe("WorkspaceState mapping", () => {
  it("maps the render state to its keyword and converts inputs", () => {
    const effects = applyOne({
      workspaceState: workspaceState({
        state: "RENDER_STATE_THINKING",
        status: "SESSION_STATUS_THINKING",
        turnActive: true,
        liveTaskCount: "2",
        causeKind: "prompt_accepted",
        causeSeq: "17",
        atMs: "1234",
      }),
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
          causeKind: "prompt_accepted",
          causeSeq: 17,
          atMs: 1234,
          connectivity: "operational",
          sessionStatus: "thinking",
          controllerGenerationId: "g1",
          activeFaults: [],
          mergeLeaseHeld: false,
          mergeStatus: null,
        },
      },
    ]);
  });

  it("logs the raw and mapped state with its SSM transition identity", () => {
    const lines: string[] = [];
    const adapter = new StateAdapter((_level, message) => lines.push(message));

    adapter.apply(
      frame({
        workspaceState: workspaceState({
          state: "RENDER_STATE_READY",
          causeKind: "session_started",
          causeSeq: "9",
          atMs: "1234",
        }),
      }),
    );

    expect(lines).toEqual([
      expect.stringContaining(
        "connectivity=operational status=ready proto=READY keyword=ready turn_active=false live_tasks=0 " +
          "merge_lease_held=false merge_status=none " +
          "faults=none cause_kind=session_started cause_seq=9 at_ms=1234",
      ),
    ]);
  });

  it("carries the structured merge status through to the store input", () => {
    // Arrange / Act
    const effects = applyOne({
      workspaceState: workspaceState({
        state: "RENDER_STATE_MERGING",
        mergeStatus: {
          runId: "run-1",
          phaseStartedAtMs: "900",
          updatedAtMs: "1000",
          cherryPicking: {
            commitsTotal: 4,
            commitsLanded: 1,
            currentSha: "abc1234",
            currentSubject: "fix the thing",
          },
        },
      }),
    });
    // Assert
    expect(effects[0]).toMatchObject({
      kind: "workspace-state",
      value: {
        mergeStatus: {
          runId: "run-1",
          phase: { case: "cherryPicking", value: { commitsLanded: 1 } },
        },
      },
    });
  });

  it("maps an absent merge status to null, which is 'no merge run'", () => {
    // Arrange / Act
    const effects = applyOne({ workspaceState: workspaceState({}) });
    // Assert
    expect(effects[0]).toMatchObject({ kind: "workspace-state", value: { mergeStatus: null } });
  });

  it("logs the merge status as its phase, run, and refresh stamp", () => {
    // Arrange
    const lines: string[] = [];
    const adapter = new StateAdapter((_level, message) => lines.push(message));
    // Act
    adapter.apply(
      frame({
        workspaceState: workspaceState({
          state: "RENDER_STATE_MERGE_CONFLICT",
          mergeStatus: {
            runId: "run-7",
            updatedAtMs: "1000",
            conflict: { conflictedSha: "bad1234", commitsTotal: 4, commitsLanded: 2 },
          },
        }),
      }),
    );
    // Assert
    expect(lines).toEqual([expect.stringContaining("merge_status=conflict/run-7@1000")]);
  });

  it("decodes RENDER_STATE_CLEARING into its own keyword", () => {
    // Arrange / Act — the two context cuts share thinking's color, so only the
    // keyword can carry the distinction through to the footer's phase word.
    const effects = applyOne({
      workspaceState: workspaceState({ state: "RENDER_STATE_CLEARING" }),
    });
    // Assert
    expect(effects[0]).toMatchObject({ kind: "workspace-state", value: { state: "clearing" } });
  });

  it("decodes RENDER_STATE_COMPACTING into its own keyword", () => {
    // Arrange / Act
    const effects = applyOne({
      workspaceState: workspaceState({ state: "RENDER_STATE_COMPACTING" }),
    });
    // Assert
    expect(effects[0]).toMatchObject({ kind: "workspace-state", value: { state: "compacting" } });
  });

  const keywordCases: Array<[string, string]> = [
    ["RENDER_STATE_INIT", "init"],
    ["RENDER_STATE_IDLE", "idle"],
    ["RENDER_STATE_IDLE_ASYNC", "idle_async"],
    ["RENDER_STATE_PERMISSION", "permission"],
    ["RENDER_STATE_MERGE_ENQUEUING", "merge_enqueuing"],
    ["RENDER_STATE_MERGE_CONFLICT", "merge_conflict"],
    ["RENDER_STATE_MERGED", "merged"],
    ["RENDER_STATE_DEAD", "dead"],
    ["RENDER_STATE_DEGRADED", "degraded"],
    ["RENDER_STATE_SEVERED", "severed"],
    ["RENDER_STATE_HIBERNATED", "hibernated"],
  ];
  for (const [proto, keyword] of keywordCases) {
    it(`maps ${proto} to '${keyword}'`, () => {
      const effects = applyOne({ workspaceState: workspaceState({ workspace: "w", sessionId: "s", state: proto }) });
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
        modelOptions: [{ value: "claude-opus", displayName: "Opus", description: "highest capability" }],
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
          models: [{ value: "claude-opus", displayName: "Opus", description: "highest capability" }],
        },
      },
    ]);
  });
});

// --- TypingDelta → smooth.ts reveal feed -----------------------------------

describe("TypingDelta mapping", () => {
  it("carries the delta's identity through without deriving a block id of its own", () => {
    // The adapter is a CARRIER of identity, not a second source of it: the
    // message id was chosen at the shim and the block id is derived in exactly
    // one place (streaming.ts). Re-deriving one here is what let the preview's
    // key and the finished record's key drift apart.
    const effects = applyOne({
      typingDelta: { workspace: "ws", sessionId: "s1", delta: { uuid: "msg-7", blockIndex: 2, thinking: "..." } },
    });
    expect(effects).toEqual([
      {
        kind: "typing",
        value: {
          workspace: "ws",
          sessionId: "s1",
          messageId: "msg-7",
          blockIndex: 2,
          kind: "thinking",
          delta: "...",
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

// --- DegradedNotice: RETIRED (step 11) --------------------------------------

describe("DegradedNotice mapping", () => {
  it("rejects the retired degradedNotice frame arm", () => {
    expect(() =>
      applyOne({
        degradedNotice: { component: "shim-store", reason: "socket closed", recovered: false, atMs: "1700000000000" },
      }),
    ).toThrow(/unrecognized field/);
  });
});

// --- StateSnapshot decomposition -------------------------------------------

describe("StateSnapshot mapping", () => {
  it("fans out into per-workspace / per-session / per-catalog / per-init effects", () => {
    const effects = applyOne({
      snapshot: {
        workspaces: [workspaceState({ workspace: "w", sessionId: "s", state: "RENDER_STATE_IDLE" })],
        sessions: [{ workspace: "w", sessionId: "s", model: "m", modelOptions: [] }],
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
  it("decomposes content blocks into text/thinking items", () => {
    const items = itemsFrom({
      uuid: "m1",
      assistantMessage: {
        content: [{ text: { text: "hello" } }, { thinking: { thinking: "hmm", signature: "sig" } }],
      },
    });
    const text: TextItem = {
      kind: "text",
      blockId: "m1:0",
      uuid: "m1:0",
      messageId: "m1",
      text: "hello",
      done: true,
      ts: "",
    };
    const thinking: ThinkingItem = {
      kind: "thinking",
      blockId: "m1:1",
      uuid: "m1:1",
      messageId: "m1",
      text: "hmm",
      done: true,
      signature: "sig",
    };
    expect(items).toEqual([text, thinking]);
  });

  it("derives the block's feed place from the ENVELOPE, so two records never collide", () => {
    // The feed place must be unique per RECORD. Deriving it from the Anthropic
    // message id gave every record of one message the same place, because each
    // record's own content array holds exactly one block (index 0).
    const items = itemsFrom({
      uuid: "envelope-uuid",
      assistantMessage: { id: "msg_01ABC", content: [{ text: { text: "hello" } }] },
    });
    expect((items[0] as TextItem).blockId).toBe("envelope-uuid:0");
  });

  it("falls back to the envelope uuid when the payload carries no id", () => {
    // A payload with no id must still get a stable key rather than colliding
    // with every other id-less message on "".
    const items = itemsFrom({
      uuid: "envelope-uuid",
      assistantMessage: { content: [{ text: { text: "hello" } }] },
    });
    expect((items[0] as TextItem).blockId).toBe("envelope-uuid:0");
  });

  it("stamps the record identity from the ENVELOPE, not the message id", () => {
    // `uuid` is what the store dedups a finished block on. Deriving it from the
    // Anthropic message id collapses every block of a message onto one key,
    // because the SDK emits one record per block and each record's own content
    // array holds exactly one entry (so its index is always 0).
    const items = itemsFrom({
      uuid: "envelope-uuid",
      assistantMessage: { id: "msg_01ABC", content: [{ text: { text: "hello" } }] },
    });
    expect((items[0] as TextItem).uuid).toBe("envelope-uuid:0");
  });

  it("keeps the message id as the block's messageId, the stream's shared identity", () => {
    // The message id is what pairs the finished block with its live preview,
    // so it must survive as `messageId` even though it no longer keys the item.
    const items = itemsFrom({
      uuid: "envelope-uuid",
      assistantMessage: { id: "msg_01ABC", content: [{ text: { text: "hello" } }] },
    });
    expect((items[0] as TextItem).messageId).toBe("msg_01ABC");
  });

  it("gives two records of ONE message two distinct identities", () => {
    // The data-loss case: two thinking blocks of one API message arrive as two
    // records sharing `msg_01ABC`, each with content index 0. Keyed on that
    // index the second silently replaced the first.
    const first = itemsFrom({
      uuid: "env1",
      assistantMessage: { id: "msg_01ABC", content: [{ thinking: { thinking: "one" } }] },
    });
    const second = itemsFrom({
      uuid: "env2",
      assistantMessage: { id: "msg_01ABC", content: [{ thinking: { thinking: "two" } }] },
    });
    expect((first[0] as ThinkingItem).uuid).not.toBe((second[0] as ThinkingItem).uuid);
  });

  it("stamps the record identity on thinking blocks too", () => {
    const items = itemsFrom({
      uuid: "env1",
      assistantMessage: { id: "msg_01ABC", content: [{ thinking: { thinking: "hmm" } }] },
    });
    expect((items[0] as ThinkingItem).uuid).toBe("env1:0");
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
    const expected: UserTurnItem = { kind: "user-turn", requestId: "r1", uuid: "m1", content: [{ type: "text", text: "hi there" }], ts: "" };
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
    const turn: UserTurnItem = { kind: "user-turn", requestId: "r1", uuid: "m1", content: [{ type: "text", text: "and more" }], ts: "" };
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

describe("result arm: the per-model usage map", () => {
  /** A result frame carrying one model's whole-tree slice. */
  function withModelUsage(models: Record<string, unknown>): ResultItem {
    return itemsFrom({
      uuid: "m1",
      result: { subtype: "RESULT_SUBTYPE_SUCCESS", modelUsage: models },
    })[0] as ResultItem;
  }

  it("snake-cases one model's slice off the wire", () => {
    // Arrange / Act — the map has always ridden inside the typed ResultMessage.
    const item = withModelUsage({
      "claude-opus-4": {
        inputTokens: "100",
        outputTokens: "200",
        cacheReadInputTokens: "300",
        cacheCreationInputTokens: "40",
        webSearchRequests: "2",
        costUsd: 0.51,
        contextWindow: "200000",
      },
    });
    // Assert
    expect(item.modelUsage).toEqual({
      "claude-opus-4": {
        input_tokens: 100,
        output_tokens: 200,
        cache_read_input_tokens: 300,
        cache_creation_input_tokens: 40,
        web_search_requests: 2,
        cost_usd: 0.51,
        context_window: 200000,
      },
    });
  });

  it("carries every model the map names", () => {
    // Arrange / Act
    const item = withModelUsage({ a: { inputTokens: "1" }, b: { inputTokens: "2" } });
    // Assert
    expect(Object.keys(item.modelUsage ?? {})).toEqual(["a", "b"]);
  });

  it("leaves the map ABSENT when the result carried none", () => {
    // Arrange / Act — absent means "not itemized", which must not clobber the
    // standing map in the store.
    const item = itemsFrom({
      uuid: "m1",
      result: { subtype: "RESULT_SUBTYPE_SUCCESS" },
    })[0] as ResultItem;
    // Assert
    expect(item.modelUsage).toBeUndefined();
  });

  it("keeps an EMPTY map distinct from an absent one", () => {
    // Arrange / Act — a result that genuinely itemized nothing.
    const item = withModelUsage({});
    // Assert
    expect(item.modelUsage).toEqual({});
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

describe("contextCleared / contextCompacted arms", () => {
  it("maps the EMPTY contextCleared message to an item carrying its envelope uuid", () => {
    // Arrange + Act
    const items = itemsFrom({ uuid: "m1", contextCleared: {} });
    // Assert — existence and position are the whole fact.
    const expected: ContextClearedItem = { kind: "context-cleared", uuid: "m1" };
    expect(items).toEqual([expected]);
  });

  it("maps a contextCompacted's coalesced fields", () => {
    // Arrange + Act
    const items = itemsFrom({
      uuid: "m1",
      contextCompacted: {
        trigger: "CONTEXT_COMPACT_TRIGGER_AUTO",
        preTokens: "287028",
        postTokens: "9001",
        durationMs: "4200",
        summary: "the story so far",
      },
    });
    // Assert
    const expected: ContextCompactedItem = {
      kind: "context-compacted",
      uuid: "m1",
      trigger: "auto",
      preTokens: 287028,
      postTokens: 9001,
      durationMs: 4200,
      summary: "the story so far",
    };
    expect(items).toEqual([expected]);
  });

  it("maps the manual trigger", () => {
    // Arrange + Act
    const items = itemsFrom({
      uuid: "m1",
      contextCompacted: { trigger: "CONTEXT_COMPACT_TRIGGER_MANUAL" },
    });
    // Assert
    expect((items[0] as ContextCompactedItem).trigger).toBe("manual");
  });

  it("reads an ABSENT trigger as unspecified, the proto3 default protojson omits", () => {
    // Arrange + Act
    const items = itemsFrom({ uuid: "m1", contextCompacted: { summary: "s" } });
    // Assert
    expect((items[0] as ContextCompactedItem).trigger).toBe("unspecified");
  });

  it("throws on an unrecognized trigger rather than defaulting it", () => {
    // Arrange + Act + Assert — inventing a value the daemon never sent would
    // be the adapter making a fact up.
    expect(() =>
      itemsFrom({ uuid: "m1", contextCompacted: { trigger: "CONTEXT_COMPACT_TRIGGER_TELEPATHY" } }),
    ).toThrow("unknown ContextCompactTrigger");
  });

  it("carries an EMPTY summary as empty rather than inventing one", () => {
    // Arrange + Act
    const items = itemsFrom({ uuid: "m1", contextCompacted: { preTokens: "10" } });
    // Assert
    expect((items[0] as ContextCompactedItem).summary).toBe("");
  });

  it("rejects the retired compactBoundary arm", () => {
    // Arrange + Act + Assert
    expect(() => itemsFrom({ uuid: "m1", compactBoundary: { preTokens: "1" } })).toThrow(
      "unrecognized field(s): compactBoundary",
    );
  });

  it("rejects the retired compactBoundaryLine arm", () => {
    // Arrange + Act + Assert
    expect(() => itemsFrom({ uuid: "m1", compactBoundaryLine: {} })).toThrow(
      "unrecognized field(s): compactBoundaryLine",
    );
  });
});

describe("apiError arm: RETIRED (step 11)", () => {
  it("rejects the retired apiError conversation-item arm", () => {
    // Arrange / Act / Assert — the daemon curates a terminal line to
    // systemFailure only now; a mid-backoff line curates to no item at all.
    expect(() =>
      itemsFrom({
        uuid: "m1",
        apiError: { error: { message: "overloaded" }, retryAttempt: 2, maxRetries: 5 },
      }),
    ).toThrow(/unrecognized field/);
  });
});

describe("systemFailure arm", () => {
  it("adopts the daemon's classified failure as a card", () => {
    // Arrange / Act
    const items = itemsFrom({
      uuid: "failure:e9",
      systemFailure: {
        errorClass: "ERROR_CLASS_API",
        errorType: "api.overloaded",
        message: "the API is overloaded",
        sourceDetail: "status=529",
      },
    });
    // Assert
    const expected: SystemFailureCard = {
      kind: "failure",
      errorClass: "API",
      errorType: "api.overloaded",
      message: "the API is overloaded",
      sourceDetail: "status=529",
      resolvedAtMs: 0,
      uuid: "failure:e9",
    };
    expect(items).toEqual([expected]);
  });

  it("adopts an INTERNAL class unchanged", () => {
    // Arrange / Act
    const items = itemsFrom({
      uuid: "degraded:s1:shim-connection",
      systemFailure: {
        errorClass: "ERROR_CLASS_INTERNAL",
        errorType: "shim.degraded",
        message: "no traffic",
      },
    });
    // Assert
    expect((items[0] as SystemFailureCard).errorClass).toBe("INTERNAL");
  });

  it("carries the resolution stamp that settles a window", () => {
    // Arrange / Act — the closing edge of a degraded window.
    const items = itemsFrom({
      uuid: "degraded:s1:shim-connection",
      systemFailure: {
        errorClass: "ERROR_CLASS_INTERNAL",
        errorType: "shim.degraded",
        message: "no traffic",
        resolvedAtMs: "1700000000000",
      },
    });
    // Assert
    expect((items[0] as SystemFailureCard).resolvedAtMs).toBe(1700000000000);
  });

  it("carries the item uuid so the footer's error row can address it", () => {
    // Arrange / Act
    const items = itemsFrom({
      uuid: "failure:e9",
      systemFailure: {
        errorClass: "ERROR_CLASS_API",
        errorType: "api.overloaded",
        message: "boom",
      },
    });
    // Assert
    expect((items[0] as SystemFailureCard).uuid).toBe("failure:e9");
  });

  it("throws on an unrecognized class rather than guessing a color", () => {
    // Arrange / Act / Assert — the class decides the card's color, so a
    // default would paint a failure the wrong color, quietly.
    expect(() =>
      itemsFrom({
        uuid: "f1",
        systemFailure: {
          errorClass: "ERROR_CLASS_SOMETHING_NEW",
          errorType: "x",
          message: "y",
        },
      }),
    ).toThrow(/unrecognized error_class/);
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

// --- the provenance gate ----------------------------------------------------

/** One conversation delta carrying ITEM verbatim (no source is injected). */
function deltaOf(
  item: Record<string, unknown>,
  log?: (level: AdapterLogLevel, message: string) => void,
): AdapterEffect[] {
  const adapter = new StateAdapter(log);
  return adapter.apply(
    frame({
      conversationDelta: { sessionId: "s1", workspace: "/ws", throughSeq: "9", items: [item] },
    }),
  );
}

/** The store items one raw (un-stamped) conversation item produced. */
function gatedItems(item: Record<string, unknown>): ConversationItem[] {
  const conv = deltaOf(item).find((e) => e.kind === "conversation-items");
  if (conv?.kind !== "conversation-items") throw new Error("no conversation-items effect");
  return conv.items;
}

const PROSE = { uuid: "m1", assistantMessage: { id: "msg_1", content: [{ text: { text: "hi" } }] } };

describe("conversation provenance gate", () => {
  it("renders a USER-driven item, which is the ordinary turn", () => {
    // Arrange / Act
    const items = gatedItems({ ...PROSE, source: "CONVERSATION_SOURCE_USER" });
    // Assert
    expect(items).toHaveLength(1);
  });

  it("keeps the USER item's own content, not just its count", () => {
    // Arrange / Act
    const items = gatedItems({ ...PROSE, source: "CONVERSATION_SOURCE_USER" });
    // Assert
    expect((items[0] as TextItem).text).toBe("hi");
  });

  it("hides a MERGE-driven item from the feed", () => {
    // Arrange / Act — the merge drives the session under its lease, and its
    // turns are noise in the feed of a user who is only waiting for it.
    const items = gatedItems({ ...PROSE, source: "CONVERSATION_SOURCE_MERGE" });
    // Assert
    expect(items).toEqual([]);
  });

  it("counts a hidden MERGE item on the explicit-ignore path", () => {
    // Arrange / Act — a deliberate non-render stays visible in diagnostics
    // rather than looking like data loss.
    const effects = deltaOf({ ...PROSE, source: "CONVERSATION_SOURCE_MERGE" });
    // Assert
    expect(effects).toContainEqual({
      kind: "ignored",
      shape: "conversation-item-source:merge",
    });
  });

  it("does NOT log an error for a MERGE item, which is a normal frame", () => {
    // Arrange
    const logs: Array<[AdapterLogLevel, string]> = [];
    // Act
    deltaOf({ ...PROSE, source: "CONVERSATION_SOURCE_MERGE" }, (lvl, msg) => logs.push([lvl, msg]));
    // Assert
    expect(logs.filter(([lvl]) => lvl === "error")).toEqual([]);
  });

  it("logs an ERROR for an UNSPECIFIED source, which the daemon never emits", () => {
    // Arrange
    const logs: Array<[AdapterLogLevel, string]> = [];
    // Act — protojson omits the proto3 zero, so an absent `source` IS
    // UNSPECIFIED on the wire.
    deltaOf(PROSE, (lvl, msg) => logs.push([lvl, msg]));
    // Assert
    expect(logs.filter(([lvl]) => lvl === "error")).toHaveLength(1);
  });

  it("names the malformed item's uuid in that error, so it can be found", () => {
    // Arrange
    const logs: string[] = [];
    // Act
    deltaOf(PROSE, (_lvl, msg) => logs.push(msg));
    // Assert
    expect(logs.find((m) => m.includes("UNSPECIFIED source"))).toContain("uuid=m1");
  });

  it("names the session in that error, so the record is correlatable", () => {
    // Arrange
    const logs: string[] = [];
    // Act
    deltaOf(PROSE, (_lvl, msg) => logs.push(msg));
    // Assert
    expect(logs.find((m) => m.includes("UNSPECIFIED source"))).toContain("session=s1");
  });

  it("DROPS the UNSPECIFIED item rather than defaulting it to a user turn", () => {
    // Arrange / Act — the reserved zero exists precisely so a receiver never
    // draws a turn nothing vouches for.
    const items = gatedItems(PROSE);
    // Assert
    expect(items).toEqual([]);
  });

  it("counts the dropped UNSPECIFIED item under its own ignore shape", () => {
    // Arrange / Act
    const effects = deltaOf(PROSE);
    // Assert
    expect(effects).toContainEqual({
      kind: "ignored",
      shape: "conversation-item-source:unspecified",
    });
  });

  it("keeps a USER item in a batch whose sibling is a MERGE item", () => {
    // Arrange — the gate is per item, not per delta.
    const effects = new StateAdapter().apply(
      frame({
        conversationDelta: {
          sessionId: "s1",
          workspace: "/ws",
          throughSeq: "9",
          items: [
            { ...PROSE, source: "CONVERSATION_SOURCE_MERGE" },
            {
              uuid: "m2",
              source: "CONVERSATION_SOURCE_USER",
              assistantMessage: { id: "msg_2", content: [{ text: { text: "mine" } }] },
            },
          ],
        },
      }),
    );
    // Act
    const conv = effects.find((e) => e.kind === "conversation-items");
    // Assert
    expect(conv?.kind === "conversation-items" && conv.items).toHaveLength(1);
  });
});

describe("the workspace roster frame", () => {
  /** A minimal-but-valid roster frame in the repository grouping. */
  function rosterFrame(over: Record<string, unknown> = {}): Record<string, unknown> {
    return {
      workspaceRoster: {
        revision: "4",
        bootId: "boot-a",
        repository: {
          sections: [
            { repoKey: "doom", folded: false, rows: [{ dir: "/w/a", name: "a", ready: {}, current: true }] },
          ],
        },
        currentDir: "/w/a",
        ...over,
      },
    };
  }

  it("maps the roster frame to a workspace-roster effect", () => {
    // Arrange + Act
    const effects = applyOne(rosterFrame());

    // Assert
    expect(effects.map((e) => e.kind)).toEqual(["workspace-roster"]);
  });

  it("forwards the decoded roster whole, revision included", () => {
    // Arrange + Act
    const [effect] = applyOne(rosterFrame());

    // Assert — the sidebar owns the gate, so the adapter must not strip the
    // revision it ranks by.
    if (effect.kind !== "workspace-roster") throw new Error(`wrong effect ${effect.kind}`);
    expect(effect.value.revision).toBe(4);
  });

  it("logs the roster's arrival at debug", () => {
    // Arrange
    const logs: Array<[AdapterLogLevel, string]> = [];
    const adapter = new StateAdapter((lvl, msg) => logs.push([lvl, msg]));

    // Act
    adapter.apply(frame(rosterFrame()));

    // Assert
    expect(logs.some(([lvl, m]) => lvl === "debug" && m.includes("workspace roster revision=4"))).toBe(
      true,
    );
  });

  it("no longer counts the roster as an ignored shape", () => {
    // Arrange
    const adapter = new StateAdapter();

    // Act
    adapter.apply(frame(rosterFrame()));

    // Assert
    expect(adapter.ignoredCounts().get("workspaceRoster")).toBeUndefined();
  });
});

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
      frame({
        conversationDelta: {
          sessionId: "s1",
          items: [userItem({ uuid: "m1", toolUseResult: { rawString: "x" } })],
        },
      }),
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
          items: [
            userItem({
              uuid: "m1",
              assistantMessage: { content: [{ text: { text: "keep" } }, { image: { source: {} } }] },
            }),
          ],
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

describe("queue -> queue effect (E4)", () => {
  it("maps a queue frame to a queue effect carrying its entries", () => {
    // Arrange / Act
    const effects = applyOne({
      queue: {
        workspace: "ws",
        sessionId: "s1",
        entries: [{ id: "q1", text: "later", classification: "QUEUE_CLASSIFICATION_HOLD" }],
      },
    });

    // Assert
    expect(effects).toEqual([
      {
        kind: "queue",
        value: {
          workspace: "ws",
          sessionId: "s1",
          entries: [
            {
              id: "q1",
              text: "later",
              queuedAtMs: 0,
              classification: "hold",
              rationale: "",
              accepted: false,
            },
          ],
        },
      },
    ]);
  });

  it("passes an empty queue through as an empty entries list", () => {
    // Arrange / Act — the store needs this to CLEAR its chips.
    const effects = applyOne({ queue: { workspace: "ws", sessionId: "s1" } });
    // Assert
    expect(effects[0]).toEqual({
      kind: "queue",
      value: { workspace: "ws", sessionId: "s1", entries: [] },
    });
  });

  it("fans a snapshot's queues out into queue effects", () => {
    // Arrange / Act — a reconnect must restore the chips.
    const effects = applyOne({
      snapshot: {
        workspaces: [],
        sessions: [],
        catalogs: [],
        inits: [],
        queues: [
          {
            workspace: "ws",
            sessionId: "s1",
            entries: [{ id: "q1", text: "later", classification: "QUEUE_CLASSIFICATION_HOLD" }],
          },
        ],
      },
    });
    // Assert
    expect(effects.filter((e) => e.kind === "queue")).toHaveLength(1);
  });

  it("does not route the queue down the explicit-ignore path", () => {
    // Arrange
    const adapter = new StateAdapter();
    // Act
    adapter.apply(frame({ queue: { sessionId: "s1", entries: [] } }));
    // Assert
    expect(adapter.ignoredCounts().get("queue")).toBeUndefined();
  });
});

describe("progress (F1): the consolidated footer's whole input", () => {
  /** The minimum a ProgressView frame must carry to decode. */
  function progressFrame(over: Record<string, unknown> = {}): Record<string, unknown> {
    return { progress: { workspace: "ws", sessionId: "s1", state: "RENDER_STATE_INIT", ...over } };
  }

  /** The single progress effect a frame produced. */
  function progressOf(obj: unknown) {
    const eff = applyOne(obj).find((e) => e.kind === "progress");
    if (eff?.kind !== "progress") throw new Error("no progress effect");
    return eff.value;
  }

  it("carries no phase, even when a deprecated mirror is on the wire", () => {
    // Arrange / Act — an older daemon still sends the deprecated `state`, so
    // the decoder tolerates it; the adapter maps it to nothing, because the
    // phase is the WorkspaceState's and a second copy is what went stale.
    const got = progressOf(progressFrame({ state: "RENDER_STATE_THINKING" }));
    // Assert
    expect(got).not.toHaveProperty("state");
  });

  it("carries the turn's input tokens through", () => {
    // Arrange / Act — int64 arrives as a protojson numeric string.
    const got = progressOf(progressFrame({ inputTokens: "41200" }));
    // Assert
    expect(got.inputTokens).toBe(41200);
  });

  it("flattens an OPEN window to its detail", () => {
    // Arrange / Act
    const got = progressOf(
      progressFrame({ retrying: { active: true, sinceMs: "17", detail: "attempt 3/10" } }),
    );
    // Assert
    expect(got.retrying).toEqual({ sinceMs: 17, detail: "attempt 3/10" });
  });

  it("flattens an INACTIVE window to null", () => {
    // Arrange / Act — absent and inactive mean the same thing: nothing to say.
    const got = progressOf(progressFrame({ retrying: { active: false, detail: "stale" } }));
    // Assert
    expect(got.retrying).toBeNull();
  });

  it("flattens an absent window to null", () => {
    // Arrange / Act
    const got = progressOf(progressFrame());
    // Assert
    expect(got.compacting).toBeNull();
  });

  it("carries the blocked window, which is a window and NOT a phase", () => {
    // Arrange / Act — the phase is the WorkspaceState's; this is a separate
    // signal about being parked on the user.
    const got = progressOf(
      progressFrame({
        blocked: { active: true, sinceMs: "5", detail: "waiting on you" },
      }),
    );
    // Assert
    expect(got.blocked).toEqual({ sinceMs: 5, detail: "waiting on you" });
    expect(got).not.toHaveProperty("state");
  });

  it("carries the session allowance's structured detail", () => {
    // Arrange / Act
    const got = progressOf(
      progressFrame({
        rateLimited: {
          active: true,
          resetsAt: "1700000900",
          utilization: 0.91,
          status: "allowed_warning",
        },
      }),
    );
    // Assert
    expect(got.rateLimited).toEqual({
      active: true,
      resetsAt: 1700000900,
      utilization: 0.91,
      status: "allowed_warning",
    });
  });

  it("carries the WEEKLY allowance in its own slot, never folded into the session's", () => {
    // Arrange / Act — the two are separate facts; conflating them is what put
    // a weekly figure on screen under the session's name.
    const got = progressOf(
      progressFrame({
        rateLimitedWeekly: {
          active: true,
          resetsAt: "1700500000",
          utilization: 0.91,
          status: "allowed_warning",
        },
      }),
    );
    // Assert
    expect(got.rateLimitedWeekly).toEqual({
      active: true,
      resetsAt: 1700500000,
      utilization: 0.91,
      status: "allowed_warning",
    });
    expect(got.rateLimited).toBeNull();
  });

  it("keeps a QUIET allowance's figures rather than flattening it away", () => {
    // Arrange / Act — deliberately NOT the other windows' discipline: absent
    // means never reported, inactive means reported and unremarkable, and the
    // footer needs the second one to name both allowances side by side.
    const got = progressOf(
      progressFrame({
        rateLimited: { resetsAt: "1700000900", utilization: 0.12, status: "allowed" },
      }),
    );
    // Assert
    expect(got.rateLimited).toEqual({
      active: false,
      resetsAt: 1700000900,
      utilization: 0.12,
      status: "allowed",
    });
  });

  it("leaves an allowance the vendor never reported as null", () => {
    // Arrange / Act
    const got = progressOf(progressFrame({}));
    // Assert
    expect(got.rateLimitedWeekly).toBeNull();
  });

  it("carries an OPEN interrupt window's age and outcome into the footer's input", () => {
    // Arrange / Act — the daemon opened the window; the webapp keeps no
    // bookkeeping of its own and simply carries what it was sent.
    const got = progressOf(
      progressFrame({
        interrupt: { active: true, sinceMs: "42", outcome: "INTERRUPT_OUTCOME_INTERRUPTED" },
      }),
    );
    // Assert
    expect(got.interrupt).toEqual({ sinceMs: 42, outcome: "interrupted" });
  });

  it("logs an open interrupt window with the verdict and turn clock", () => {
    const lines: string[] = [];
    const adapter = new StateAdapter((_level, message) => lines.push(message));

    adapter.apply(
      frame(
        progressFrame({
          turnStartedAtMs: "23",
          interrupt: {
            active: true,
            sinceMs: "42",
            outcome: "INTERRUPT_OUTCOME_ALREADY_COMPLETE",
          },
        }),
      ),
    );

    expect(lines).toEqual([
      expect.stringContaining(
        "outcome=already_complete since_ms=42 turn_started_at_ms=23",
      ),
    ]);
  });

  it("flattens an INACTIVE interrupt window to null, leaving no residue", () => {
    // Arrange / Act — the daemon CLEARS the window when the next turn starts,
    // and a frame carrying it closed is the whole of the clearing.
    const got = progressOf(progressFrame({ interrupt: { active: false, sinceMs: "42" } }));
    // Assert
    expect(got.interrupt).toBeNull();
  });

  it("flattens an absent interrupt window to null", () => {
    // Arrange / Act
    const got = progressOf(progressFrame());
    // Assert
    expect(got.interrupt).toBeNull();
  });

  it("fans a snapshot's progress views out into per-workspace effects", () => {
    // Arrange / Act
    const effects = applyOne({
      snapshot: {
        workspaces: [],
        sessions: [],
        catalogs: [],
        inits: [],
        queues: [],
        progress: [{ workspace: "ws", sessionId: "s1", state: "RENDER_STATE_INIT" }],
      },
    });
    // Assert
    expect(effects.filter((e) => e.kind === "progress")).toHaveLength(1);
  });

  it("does not route progress down the explicit-ignore path", () => {
    // Arrange
    const adapter = new StateAdapter();
    // Act
    adapter.apply(frame(progressFrame()));
    // Assert
    expect(adapter.ignoredCounts().get("progress")).toBeUndefined();
  });
});

describe("userTurnReceipt (ingest-time arrival receipt)", () => {
  /** A conversation-items effect carrying ITEMS at THROUGHSEQ. */
  function delta(throughSeq: number, items: ConversationItem[]): AdapterEffect {
    return { kind: "conversation-items", workspace: "ws", sessionId: "s1", throughSeq, items };
  }

  function turn(requestId: string, text: string): UserTurnItem {
    return { kind: "user-turn", requestId, content: [{ type: "text", text }], ts: "" };
  }

  it("reports a live user turn with its request id, seq and text length", () => {
    // Arrange — the delta advances past what the store already holds.
    const effects = [delta(7, [turn("req-1", "hello there")])];
    // Act
    const receipt = userTurnReceipt(effects, 6);
    // Assert
    expect(receipt).toEqual({ requestId: "req-1", seq: 7, len: 11, live: true });
  });

  it("marks a replayed turn not live, so the caller can withhold the forward", () => {
    // Arrange — a connect resync replays history at or below lastSeq.
    const effects = [delta(3, [turn("req-1", "hello")])];
    // Act
    const receipt = userTurnReceipt(effects, 9);
    // Assert
    expect(receipt?.live).toBe(false);
  });

  it("returns null for a user turn whose text blocks are empty", () => {
    // Arrange — nothing to time when the prompt says nothing.
    const effects = [delta(7, [turn("req-1", "")])];
    // Act / Assert
    expect(userTurnReceipt(effects, 6)).toBeNull();
  });

  it("returns null for a batch with no user turn at all", () => {
    // Arrange — an assistant-only delta (the tool-feedback case yields no turn).
    const effects = [
      delta(7, [
        { kind: "text", blockId: "b1", messageId: "m1", text: "hi", done: true, ts: "" } as TextItem,
      ]),
    ];
    // Act / Assert
    expect(userTurnReceipt(effects, 6)).toBeNull();
  });

  it("sums the text blocks of a multi-block turn", () => {
    // Arrange — a turn whose prose arrived as two blocks.
    const item: UserTurnItem = {
      kind: "user-turn",
      requestId: "req-1",
      content: [
        { type: "text", text: "abc" },
        { type: "text", text: "de" },
      ],
      ts: "",
    };
    // Act
    const receipt = userTurnReceipt([delta(7, [item])], 6);
    // Assert
    expect(receipt?.len).toBe(5);
  });

  it("ignores non-text blocks when measuring the turn", () => {
    // Arrange — an image block carries no prompt text to time.
    const item: UserTurnItem = {
      kind: "user-turn",
      requestId: "req-1",
      content: [{ type: "image", source: {} }, { type: "text", text: "abcd" }],
      ts: "",
    };
    // Act
    const receipt = userTurnReceipt([delta(7, [item])], 6);
    // Assert
    expect(receipt?.len).toBe(4);
  });

  it("skips effects that are not conversation items", () => {
    // Arrange — an ignore effect sits ahead of the delta in the batch.
    const effects: AdapterEffect[] = [
      { kind: "ignored", shape: "commandAck" },
      delta(7, [turn("req-1", "hey")]),
    ];
    // Act
    const receipt = userTurnReceipt(effects, 6);
    // Assert
    expect(receipt?.requestId).toBe("req-1");
  });
});

describe("shutdown schedule", () => {
  const DRAINING = {
    scheduleId: "sched-1",
    scheduledAtMs: "1700000000000",
    cause: "manual restart",
    stopShims: false,
    holds: [{ workspace: "/w/app", sessionId: "s1", turn: { turnId: "t-1" } }],
  };

  it("maps a draining broadcast to a lease effect", () => {
    // Arrange / Act
    const effects = applyOne({ shutdownSchedule: { draining: DRAINING } });
    // Assert
    expect(effects).toEqual([
      { kind: "shutdown-schedule", value: { state: { case: "draining", value: expect.anything() } } },
    ]);
  });

  it("maps an idle broadcast to a lease effect, since idle clears the banner", () => {
    // Arrange / Act
    const effects = applyOne({ shutdownSchedule: { idle: {} } });
    // Assert
    expect(effects).toEqual([
      { kind: "shutdown-schedule", value: { state: { case: "idle", value: {} } } },
    ]);
  });

  it("seeds the lease from a connect snapshot", () => {
    // Arrange — a client joining mid-drain must see it without an edge.
    const effects = applyOne({
      snapshot: { workspaces: [], shutdownSchedule: { draining: DRAINING } },
    });
    // Act
    const lease = effects.filter((e) => e.kind === "shutdown-schedule");
    // Assert
    expect(lease).toHaveLength(1);
  });

  it("emits NO lease effect for a snapshot that does not carry the lease", () => {
    // Arrange — absence is the absence of information; synthesizing an idle
    // effect here would let a pre-feature snapshot clear a live banner.
    const effects = applyOne({ snapshot: { workspaces: [] } });
    // Act
    const lease = effects.filter((e) => e.kind === "shutdown-schedule");
    // Assert
    expect(lease).toEqual([]);
  });

  it("logs the adopted lease with its schedule and hold count", () => {
    // Arrange
    const lines: string[] = [];
    const adapter = new StateAdapter((_level: AdapterLogLevel, message: string) => {
      lines.push(message);
    });
    // Act
    adapter.apply(frame({ shutdownSchedule: { draining: DRAINING } }));
    // Assert
    expect(lines.join("\n")).toContain("shutdown schedule state=draining schedule=sched-1 holds=1");
  });
});
