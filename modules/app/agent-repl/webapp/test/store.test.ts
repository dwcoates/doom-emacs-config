import { describe, expect, it } from "vitest";

import {
  ConversationStore,
  contextTokens,
  liveContextDelta,
  stringField,
  topLevelUsage,
  type ConversationItem,
  type ResultItem,
  type StoreState,
  type TextItem,
  type ToolItem,
} from "../src/store.js";
import type {
  AdapterEffect,
  QueueInput,
  SessionViewInput,
  ToolProgressInput,
  TypingReveal,
  WorkspaceStatusInput,
} from "../src/state-adapter.js";
import type { CounterEntry } from "../src/counter-menu.js";
import type { Usage } from "../src/protocol.js";

// The store's ONLY ingestion path after the agent-shim cutover: it folds
// typed adapter effects (decoded `agentshim.frontend.v1` frames) onto its
// render state. These tests build the effects directly — the frame→effect
// mapping is state-adapter.ts's contract, covered in its own suite.

// --- effect builders --------------------------------------------------------

function workspaceEffect(over: Partial<WorkspaceStatusInput> = {}): AdapterEffect {
  return {
    kind: "workspace-state",
    value: {
      workspace: "ws",
      sessionId: "s1",
      state: "thinking",
      turnActive: true,
      liveTaskCount: 0,
      mergePhase: "",
      ...over,
    },
  };
}

function sessionEffect(over: Partial<SessionViewInput> = {}): AdapterEffect {
  return {
    kind: "session-view",
    value: {
      workspace: "ws",
      sessionId: "s1",
      model: "",
      slug: "",
      title: "",
      totalTokens: 0,
      totalCostUsd: 0,
      contextWindow: 0,
      permissionMode: "",
      shimAttached: true,
      claudeSessionId: "",
      cwd: "",
      configDir: "",
      ...over,
    },
  };
}

function typingEffect(over: Partial<TypingReveal> = {}): AdapterEffect {
  const uuid = over.uuid ?? "u1";
  const blockIndex = over.blockIndex ?? 0;
  return {
    kind: "typing",
    value: {
      workspace: "ws",
      sessionId: "s1",
      uuid,
      blockIndex,
      kind: "text",
      delta: "hi",
      blockId: `${uuid}:${blockIndex}`,
      ...over,
    },
  };
}

function itemsEffect(items: ConversationItem[], throughSeq = 0): AdapterEffect {
  return { kind: "conversation-items", workspace: "ws", sessionId: "s1", throughSeq, items };
}

function catalogEffect(entries: CounterEntry[]): AdapterEffect {
  return { kind: "task-catalog", value: { workspace: "ws", sessionId: "s1", entries } };
}

// --- item builders ----------------------------------------------------------

const TS = "2026-07-19T12:00:00.000Z";

function textItem(over: Partial<TextItem> = {}): TextItem {
  return { kind: "text", blockId: "b1", messageId: "m1", text: "hello", done: true, ts: TS, ...over };
}

function toolItem(over: Partial<ToolItem> = {}): ToolItem {
  return {
    kind: "tool",
    toolUseId: "tu1",
    toolName: "Bash",
    messageId: "m1",
    ts: TS,
    inputJson: "",
    inputDone: false,
    ...over,
  };
}

function resultItem(over: Partial<ResultItem> = {}): ResultItem {
  return {
    kind: "result",
    subtype: "success",
    durationMs: 10,
    sincePrevFinalMs: 10,
    numTurns: 1,
    totalCostUsd: 0.1,
    usage: { input_tokens: 1, output_tokens: 1 },
    isError: false,
    context: null,
    ...over,
  };
}

// --- workspace-state --------------------------------------------------------

describe("ingest workspace-state", () => {
  it("sets turnInFlight from turnActive", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([workspaceEffect({ turnActive: true })]);
    // Assert
    expect(store.state.turnInFlight).toBe(true);
  });

  it("clears turnInFlight when turnActive is false", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ turnActive: true })]);
    // Act
    store.ingest([workspaceEffect({ turnActive: false })]);
    // Assert
    expect(store.state.turnInFlight).toBe(false);
  });

  it("stamps turnStartedAt on the idle→active transition", () => {
    // Arrange — a fixed clock so the ISO stamp is deterministic.
    const store = new ConversationStore(() => {}, () => Date.parse(TS));
    // Act
    store.ingest([workspaceEffect({ turnActive: true })]);
    // Assert
    expect(store.state.turnStartedAt).toBe(TS);
  });

  it("holds turnStartedAt steady across consecutive active pushes", () => {
    // Arrange — clock advances between pushes; the stamp must not chase it.
    let t = Date.parse(TS);
    const store = new ConversationStore(() => {}, () => t);
    store.ingest([workspaceEffect({ turnActive: true })]);
    t += 5000;
    // Act
    store.ingest([workspaceEffect({ turnActive: true })]);
    // Assert
    expect(store.state.turnStartedAt).toBe(TS);
  });

  it("clears turnStartedAt when the turn ends", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ turnActive: true })]);
    // Act
    store.ingest([workspaceEffect({ turnActive: false })]);
    // Assert
    expect(store.state.turnStartedAt).toBeNull();
  });

  it("adopts a non-empty sessionId", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([workspaceEffect({ sessionId: "sX" })]);
    // Assert
    expect(store.state.sessionId).toBe("sX");
  });

  it("reports a change so the caller repaints", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    const result = store.ingest([workspaceEffect()]);
    // Assert
    expect(result.changed).toBe(true);
  });
});

// --- session-view -----------------------------------------------------------

describe("ingest session-view", () => {
  it("adopts a non-empty model", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([sessionEffect({ model: "opus" })]);
    // Assert
    expect(store.state.model).toBe("opus");
  });

  it("keeps the live model when the view carries none", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([sessionEffect({ model: "opus" })]);
    // Act
    store.ingest([sessionEffect({ model: "" })]);
    // Assert
    expect(store.state.model).toBe("opus");
  });

  it("adopts the permission mode", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([sessionEffect({ permissionMode: "plan" })]);
    // Assert
    expect(store.state.permissionMode).toBe("plan");
  });

  it("adopts the total cost", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([sessionEffect({ totalCostUsd: 0.42 })]);
    // Assert
    expect(store.state.costUsd).toBe(0.42);
  });

  it("maps a positive total-tokens count onto the context figure", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([sessionEffect({ totalTokens: 12_345 })]);
    // Assert
    expect(store.state.contextTokens).toBe(12_345);
  });

  it("reads a zero total-tokens count as unknown context", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([sessionEffect({ totalTokens: 0 })]);
    // Assert
    expect(store.state.contextTokens).toBeNull();
  });

  it("adopts the title as the objective label", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([sessionEffect({ title: "wire the counter" })]);
    // Assert
    expect(store.state.taskSummary).toBe("wire the counter");
  });

  it("keeps the last objective when the view carries no title", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([sessionEffect({ title: "first" })]);
    // Act
    store.ingest([sessionEffect({ title: "" })]);
    // Assert
    expect(store.state.taskSummary).toBe("first");
  });

  it("adopts the resume keys (claudeSessionId + cwd) for rebind/auto-continue", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([sessionEffect({ claudeSessionId: "cli-uuid", cwd: "/work/ws" })]);
    // Assert
    expect(store.state.claudeSessionId).toBe("cli-uuid");
    expect(store.state.cwd).toBe("/work/ws");
  });

  it("keeps the last resume keys when the view carries none", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([sessionEffect({ claudeSessionId: "cli-uuid", cwd: "/work/ws" })]);
    // Act
    store.ingest([sessionEffect({ claudeSessionId: "", cwd: "" })]);
    // Assert
    expect(store.state.claudeSessionId).toBe("cli-uuid");
    expect(store.state.cwd).toBe("/work/ws");
  });
});

// --- conversation-items -----------------------------------------------------

describe("ingest conversation-items", () => {
  it("appends a fresh item to the feed", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([itemsEffect([textItem()])]);
    // Assert
    expect(store.state.items).toHaveLength(1);
  });

  it("reconciles a tool card by tool-use id when its result arrives", () => {
    // Arrange — the card lands first, then a settled copy with the result.
    const store = new ConversationStore();
    store.ingest([itemsEffect([toolItem({ inputDone: true })])]);
    // Act
    store.ingest([
      itemsEffect([toolItem({ inputDone: true, result: { isError: false, content: "ok" } })]),
    ]);
    // Assert — one card, now carrying the result (not two).
    expect(store.state.items).toHaveLength(1);
    expect((store.state.items[0] as ToolItem).result?.content).toBe("ok");
  });

  it("preserves the call's name and input when the empty-named result item merges", () => {
    // Arrange — the tool_use card (name + input), then the daemon's result
    // item, which carries an empty toolName and no input by contract.
    const store = new ConversationStore();
    store.ingest([itemsEffect([toolItem({ toolName: "Bash", input: { command: "ls" }, inputDone: true })])]);
    // Act
    store.ingest([
      itemsEffect([toolItem({ toolName: "", inputDone: true, result: { isError: false, content: "ok" } })]),
    ]);
    // Assert — one card that kept its name + input and gained the result.
    expect(store.state.items).toHaveLength(1);
    const merged = store.state.items[0] as ToolItem;
    expect(merged.toolName).toBe("Bash");
    expect(merged.input).toEqual({ command: "ls" });
    expect(merged.result?.content).toBe("ok");
  });

  it("field-merges a tool pair regardless of arrival order (result before the call)", () => {
    // Arrange — the result item lands first (cross-plane reordering).
    const store = new ConversationStore();
    store.ingest([itemsEffect([toolItem({ toolName: "", result: { isError: true, content: "boom" } })])]);
    // Act — the call item arrives second, naming the tool + input.
    store.ingest([itemsEffect([toolItem({ toolName: "Read", input: { file_path: "/x" }, inputDone: true })])]);
    // Assert — one card carrying both halves.
    expect(store.state.items).toHaveLength(1);
    const merged = store.state.items[0] as ToolItem;
    expect(merged.toolName).toBe("Read");
    expect(merged.input).toEqual({ file_path: "/x" });
    expect(merged.result?.content).toBe("boom");
  });

  it("reconciles a text block by block id", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([itemsEffect([textItem({ text: "partial", done: false })])]);
    // Act
    store.ingest([itemsEffect([textItem({ text: "canonical", done: true })])]);
    // Assert
    expect(store.state.items).toHaveLength(1);
    expect((store.state.items[0] as TextItem).text).toBe("canonical");
  });

  it("appends a terminal result item every time (no id to reconcile on)", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([itemsEffect([resultItem()])]);
    store.ingest([itemsEffect([resultItem()])]);
    // Assert
    expect(store.state.items.filter((i) => i.kind === "result")).toHaveLength(2);
  });

  it("advances lastSeq to the delta's throughSeq", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([itemsEffect([textItem()], 9)]);
    // Assert
    expect(store.state.lastSeq).toBe(9);
  });

  it("never lowers lastSeq for a stale delta", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([itemsEffect([textItem()], 9)]);
    // Act
    store.ingest([itemsEffect([resultItem()], 3)]);
    // Assert
    expect(store.state.lastSeq).toBe(9);
  });

  it("reports no change for an empty item batch", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    const result = store.ingest([itemsEffect([], 5)]);
    // Assert
    expect(result.changed).toBe(false);
  });
});

// --- typing -----------------------------------------------------------------

describe("ingest typing", () => {
  it("grows an existing text block by its block id", () => {
    // Arrange — a live text block, then a delta for it.
    const store = new ConversationStore();
    store.ingest([itemsEffect([textItem({ blockId: "u1:0", text: "he", done: false })])]);
    // Act
    store.ingest([typingEffect({ uuid: "u1", blockIndex: 0, delta: "llo" })]);
    // Assert
    expect((store.state.items[0] as TextItem).text).toBe("hello");
  });

  it("creates a text block when the block id is not yet present", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([typingEffect({ uuid: "u9", blockIndex: 2, delta: "start" })]);
    // Assert
    const item = store.state.items[0] as TextItem;
    expect(item.kind).toBe("text");
    expect(item.blockId).toBe("u9:2");
    expect(item.text).toBe("start");
  });

  it("holds a grown block open (done false) so the reveal keeps typing it", () => {
    // Arrange — a block that had settled.
    const store = new ConversationStore();
    store.ingest([itemsEffect([textItem({ blockId: "u1:0", text: "he", done: true })])]);
    // Act
    store.ingest([typingEffect({ uuid: "u1", blockIndex: 0, delta: "llo" })]);
    // Assert
    expect((store.state.items[0] as TextItem).done).toBe(false);
  });

  it("creates a thinking block for a thinking delta", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([typingEffect({ kind: "thinking", uuid: "u2", blockIndex: 0, delta: "weigh" })]);
    // Assert
    expect(store.state.items[0].kind).toBe("thinking");
  });

  it("grows the most recent open tool's input for an input_json delta", () => {
    // Arrange — an open tool call awaiting its input.
    const store = new ConversationStore();
    store.ingest([
      itemsEffect([toolItem({ toolUseId: "tu1", inputJson: '{"cmd":', inputDone: false })]),
    ]);
    // Act
    store.ingest([typingEffect({ kind: "input_json", delta: '"ls"}' })]);
    // Assert
    expect((store.state.items[0] as ToolItem).inputJson).toBe('{"cmd":"ls"}');
  });

  it("loud-logs (never silently drops) an input_json delta with no open tool", () => {
    // Arrange
    const logged: Array<[string, string]> = [];
    const store = new ConversationStore((level, message) => logged.push([level, message]));
    // Act
    const result = store.ingest([typingEffect({ kind: "input_json", delta: "x" })]);
    // Assert — no change, and a warning names the orphaned delta.
    expect(result.changed).toBe(false);
    expect(logged).toHaveLength(1);
    expect(logged[0][0]).toBe("warn");
  });
});

// --- task-catalog -----------------------------------------------------------

describe("ingest task-catalog", () => {
  it("adopts the roster verbatim", () => {
    // Arrange
    const store = new ConversationStore();
    const entries: CounterEntry[] = [
      { id: "1", summary: "migrate", detail: "agent", status: "running", nested: false },
    ];
    // Act
    store.ingest([catalogEffect(entries)]);
    // Assert
    expect(store.taskRoster).toEqual(entries);
  });

  it("replaces the roster on the next catalog", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([
      catalogEffect([{ id: "1", summary: "a", detail: "", status: "running", nested: false }]),
    ]);
    // Act
    store.ingest([catalogEffect([])]);
    // Assert
    expect(store.taskRoster).toHaveLength(0);
  });
});

// --- session-init -----------------------------------------------------------

describe("ingest session-init", () => {
  it("adopts the pushed SystemInit as the status snapshot source", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([
      { kind: "session-init", value: { workspace: "ws", sessionId: "s1", init: { model: "claude", cwd: "/w" } } },
    ]);
    // Assert
    expect(store.state.systemInit).toEqual({ model: "claude", cwd: "/w" });
  });

  it("replaces the retained init wholesale on the next push", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([{ kind: "session-init", value: { workspace: "ws", sessionId: "s1", init: { fastModeState: "off" } } }]);
    // Act
    store.ingest([{ kind: "session-init", value: { workspace: "ws", sessionId: "s1", init: { fastModeState: "on" } } }]);
    // Assert
    expect(store.state.systemInit).toEqual({ fastModeState: "on" });
  });
});

// --- degraded / ignored / batching -----------------------------------------

describe("ingest degraded and ignored", () => {
  it("treats a degraded effect as no store change (the banner is the caller's)", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    const result = store.ingest([
      { kind: "degraded", value: { component: "store", reason: "down", recovered: false, atMs: 0 } },
    ]);
    // Assert
    expect(result.changed).toBe(false);
  });

  it("treats an ignored effect as no store change", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    const result = store.ingest([{ kind: "ignored", shape: "commandAck" }]);
    // Assert
    expect(result.changed).toBe(false);
  });

  it("folds a whole batch of effects in one call", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([sessionEffect({ model: "opus" }), itemsEffect([textItem()], 4)]);
    // Assert
    expect(store.state.model).toBe("opus");
    expect(store.state.items).toHaveLength(1);
    expect(store.state.lastSeq).toBe(4);
  });
});

// --- reset ------------------------------------------------------------------

describe("reset", () => {
  it("discards items, derived state and the task roster", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([sessionEffect({ model: "opus" }), itemsEffect([textItem()])]);
    store.ingest([
      catalogEffect([{ id: "1", summary: "a", detail: "", status: "running", nested: false }]),
    ]);
    // Act
    store.reset();
    // Assert
    expect(store.state.items).toHaveLength(0);
    expect(store.state.model).toBe("");
    expect(store.taskRoster).toHaveLength(0);
  });
});

// --- pure helpers -----------------------------------------------------------

describe("contextTokens", () => {
  it("sums every token dimension a request occupies", () => {
    // Arrange
    const usage: Usage = {
      input_tokens: 100,
      output_tokens: 30,
      cache_read_input_tokens: 200,
      cache_creation_input_tokens: 10,
    };
    // Act + Assert
    expect(contextTokens(usage)).toBe(340);
  });

  it("reads a null usage as zero", () => {
    // Arrange + Act + Assert
    expect(contextTokens(null)).toBe(0);
  });
});

describe("topLevelUsage", () => {
  it("returns null before any usage is known", () => {
    // Arrange
    const store = new ConversationStore();
    // Act + Assert
    expect(topLevelUsage(store.state)).toBeNull();
  });

  it("adds the running turn's per-message spend onto the result baseline", () => {
    // Arrange — a settled baseline plus one in-flight message.
    const state: StoreState = new ConversationStore().state;
    state.resultUsage = { input_tokens: 10, output_tokens: 5 };
    state.turnUsage = new Map([["m1", { input_tokens: 3, output_tokens: 2 }]]);
    // Act
    const total = topLevelUsage(state);
    // Assert
    expect(total).toMatchObject({ input_tokens: 13, output_tokens: 7 });
  });
});

describe("liveContextDelta", () => {
  it("is null when the standing context size is unknown", () => {
    // Arrange
    const state = new ConversationStore().state;
    state.contextTokens = null;
    // Act + Assert
    expect(liveContextDelta(state)).toBeNull();
  });

  it("measures growth past the last result's standing", () => {
    // Arrange — a result closed at 1000; the size has since grown to 1200.
    const state = new ConversationStore().state;
    state.items = [resultItem({ context: { total: 1000, delta: 0 } })];
    state.contextTokens = 1200;
    // Act + Assert
    expect(liveContextDelta(state)).toBe(200);
  });

  it("measures from zero before the first result", () => {
    // Arrange
    const state = new ConversationStore().state;
    state.contextTokens = 500;
    // Act + Assert
    expect(liveContextDelta(state)).toBe(500);
  });
});

describe("stringField", () => {
  it("returns a string input field", () => {
    // Arrange
    const item = toolItem({ input: { command: "ls" }, inputDone: true });
    // Act + Assert
    expect(stringField(item, "command")).toBe("ls");
  });

  it("returns empty string for an absent field", () => {
    // Arrange
    const item = toolItem({ input: { count: 3 }, inputDone: true });
    // Act + Assert
    expect(stringField(item, "missing")).toBe("");
  });

  it("returns empty string for a non-string field", () => {
    // Arrange
    const item = toolItem({ input: { count: 3 }, inputDone: true });
    // Act + Assert
    expect(stringField(item, "count")).toBe("");
  });
});

// --- tool-progress (E4 heartbeat relay) -------------------------------------

function progressEffect(over: Partial<ToolProgressInput> = {}): AdapterEffect {
  return {
    kind: "tool-progress",
    value: {
      workspace: "ws",
      sessionId: "s1",
      toolUseId: "tu1",
      toolName: "Bash",
      parentToolUseId: "",
      elapsedSeconds: 12.5,
      ...over,
    },
  };
}

describe("ingest tool-progress", () => {
  it("ticks the open call's elapsed clock", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([itemsEffect([toolItem({ inputDone: true })])]);
    // Act
    store.ingest([progressEffect()]);
    // Assert
    expect((store.state.items[0] as ToolItem).progressElapsedS).toBe(12.5);
  });

  it("reports a change so the caller re-renders the chip", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([itemsEffect([toolItem({ inputDone: true })])]);
    // Act
    const res = store.ingest([progressEffect()]);
    // Assert
    expect(res.changed).toBe(true);
  });

  it("reports no change when the elapsed value is unmoved", () => {
    // Arrange — a repeated heartbeat at the same clock must not churn a render.
    const store = new ConversationStore();
    store.ingest([itemsEffect([toolItem({ inputDone: true })])]);
    store.ingest([progressEffect()]);
    // Act
    const res = store.ingest([progressEffect()]);
    // Assert
    expect(res.changed).toBe(false);
  });

  it("ignores a heartbeat for a call that already settled", () => {
    // Arrange — a late heartbeat must not revive a frozen elapsed clock.
    const store = new ConversationStore();
    store.ingest([itemsEffect([toolItem({ inputDone: true, resultTs: TS })])]);
    // Act
    store.ingest([progressEffect({ elapsedSeconds: 99 })]);
    // Assert
    expect((store.state.items[0] as ToolItem).progressElapsedS).toBeUndefined();
  });

  it("invents no item for a heartbeat naming an unknown call", () => {
    // Arrange — legitimate after a reconnect: heartbeats are never replayed.
    const store = new ConversationStore();
    // Act
    store.ingest([progressEffect({ toolUseId: "nope" })]);
    // Assert
    expect(store.state.items).toHaveLength(0);
  });

  it("reports an unattributable heartbeat on the diagnostics channel", () => {
    // Arrange
    const logs: string[] = [];
    const store = new ConversationStore((_lvl, msg) => logs.push(msg));
    // Act
    store.ingest([progressEffect({ toolUseId: "nope" })]);
    // Assert — surfaced, not silently swallowed.
    expect(logs.some((m) => m.includes("heartbeat for unknown tool call nope"))).toBe(true);
  });

  it("attributes the heartbeat to the named call, not the newest one", () => {
    // Arrange — two open calls; only the named one may tick.
    const store = new ConversationStore();
    store.ingest([
      itemsEffect([
        toolItem({ toolUseId: "tu1", inputDone: true }),
        toolItem({ toolUseId: "tu2", inputDone: true }),
      ]),
    ]);
    // Act
    store.ingest([progressEffect({ toolUseId: "tu1" })]);
    // Assert
    expect((store.state.items[1] as ToolItem).progressElapsedS).toBeUndefined();
  });
});

// --- queue (E4 held prompts) -------------------------------------------------

function queueEffect(entries: QueueInput["entries"]): AdapterEffect {
  return { kind: "queue", value: { workspace: "ws", sessionId: "s1", entries } };
}

function queueEntry(over: Partial<QueueInput["entries"][number]> = {}) {
  return {
    id: "q1",
    text: "later",
    queuedAtMs: 1000,
    classification: "pending" as const,
    rationale: "",
    accepted: false,
    ...over,
  };
}

describe("ingest queue", () => {
  it("adopts the pushed entries", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([queueEffect([queueEntry()])]);
    // Assert
    expect(store.state.queued).toHaveLength(1);
    expect(store.state.queued[0].text).toBe("later");
  });

  it("carries the classification and rationale through", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([queueEffect([queueEntry({ classification: "hold", rationale: "unrelated" })])]);
    // Assert
    expect(store.state.queued[0].classification).toBe("hold");
    expect(store.state.queued[0].rationale).toBe("unrelated");
  });

  it("REPLACES the queue rather than merging into it", () => {
    // Arrange — the daemon owns the queue; merging here would make the webapp
    // a second, divergent source of truth for it.
    const store = new ConversationStore();
    store.ingest([queueEffect([queueEntry({ id: "q1" }), queueEntry({ id: "q2" })])]);
    // Act
    store.ingest([queueEffect([queueEntry({ id: "q2" })])]);
    // Assert
    expect(store.state.queued.map((q) => q.id)).toEqual(["q2"]);
  });

  it("empties the queue on an empty push", () => {
    // Arrange — this is how a drained queue and a dead session clear chips.
    const store = new ConversationStore();
    store.ingest([queueEffect([queueEntry()])]);
    // Act
    store.ingest([queueEffect([])]);
    // Assert
    expect(store.state.queued).toEqual([]);
  });

  it("preserves the daemon's entry order", () => {
    // Arrange — delivery is FIFO, so the display order is load-bearing.
    const store = new ConversationStore();
    // Act
    store.ingest([
      queueEffect([queueEntry({ id: "a" }), queueEntry({ id: "b" }), queueEntry({ id: "c" })]),
    ]);
    // Assert
    expect(store.state.queued.map((q) => q.id)).toEqual(["a", "b", "c"]);
  });

  it("reports a change so the caller re-renders the chips", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    const res = store.ingest([queueEffect([queueEntry()])]);
    // Assert
    expect(res.changed).toBe(true);
  });

  it("keeps a held prompt out of the conversation items", () => {
    // Arrange — a held prompt has NOT reached the agent; rendering it as a
    // conversation item would claim it had.
    const store = new ConversationStore();
    // Act
    store.ingest([queueEffect([queueEntry()])]);
    // Assert
    expect(store.state.items).toEqual([]);
  });
});
