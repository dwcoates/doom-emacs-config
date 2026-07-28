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
  type ThinkingItem,
  type ToolItem,
} from "../src/store.js";
import type {
  AdapterEffect,
  ProgressInput,
  QueueInput,
  SessionViewInput,
  ToolProgressInput,
  TypingReveal,
  WorkspaceStatusInput,
} from "../src/state-adapter.js";
import type { CounterEntry } from "../src/counter-menu.js";
import type { ModelUsage, Usage } from "../src/protocol.js";

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
      generation: 1,
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
  return {
    kind: "typing",
    value: {
      workspace: "ws",
      sessionId: "s1",
      messageId: "u1",
      blockIndex: 0,
      kind: "text",
      delta: "hi",
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

  it("retains the resolved render state as the footer's phase source (F5)", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([workspaceEffect({ state: "ready" })]);
    // Assert
    expect(store.state.renderState).toBe("ready");
  });

  it("retains the delivery generation the paint ack names", () => {
    // Arrange — Emacs is not sent the state until this end answers for this
    // generation, so losing it would strand the tab bar.
    const store = new ConversationStore();
    // Act
    store.ingest([workspaceEffect({ generation: 12 })]);
    // Assert
    expect(store.state.renderGeneration).toBe(12);
  });

  it("has no render state before the first workspace state lands", () => {
    // Arrange / Act — null, not a fabricated phase.
    const store = new ConversationStore();
    // Assert
    expect(store.state.renderState).toBeNull();
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

  it("adopts the resume keys (claudeSessionId + cwd) for rebind", () => {
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
});

// --- feed order (seq-ranked insertion) ---------------------------------------
//
// The connect resync replays history over the same socket that carries live
// pushes, so arrival order interleaves low-seq history with high-seq live
// frames. The feed is ordered by each delta's through-seq, not by arrival —
// the bug this pins: a prompt echo landing mid-replay stranded wherever the
// replay happened to be, permanently.

describe("feed order under replay/live interleave", () => {
  function userTurnItem(requestId: string): ConversationItem {
    return { kind: "user-turn", requestId, content: [{ type: "text", text: "hi" }], ts: TS };
  }

  it("slots a replayed item above a live item that arrived first", () => {
    // Arrange — the live prompt echo (seq 100) beats the replay to the socket.
    const store = new ConversationStore();
    store.ingest([itemsEffect([userTurnItem("live")], 100)]);
    // Act — a history item (seq 5) arrives late.
    store.ingest([itemsEffect([textItem({ blockId: "h1", uuid: "h1:0" })], 5)]);
    // Assert — history above, prompt below.
    expect(store.state.items.map((i) => i.kind)).toEqual(["text", "user-turn"]);
  });

  it("leaves a prompt echo last once a mid-replay burst completes", () => {
    // Arrange / Act — replay chunk, then the echo, then the rest of the replay.
    const store = new ConversationStore();
    store.ingest([itemsEffect([textItem({ blockId: "h1", uuid: "h1:0", messageId: "mh1" })], 5)]);
    store.ingest([itemsEffect([userTurnItem("echo")], 100)]);
    store.ingest([itemsEffect([textItem({ blockId: "h2", uuid: "h2:0", messageId: "mh2" })], 6)]);
    store.ingest([itemsEffect([textItem({ blockId: "h3", uuid: "h3:0", messageId: "mh3" })], 7)]);
    // Assert — the prompt is the LAST bubble, not third-to-last.
    expect(store.state.items.at(-1)?.kind).toBe("user-turn");
    expect(store.state.items.map((i) => (i.kind === "text" ? i.blockId : "prompt")))
      .toEqual(["h1", "h2", "h3", "prompt"]);
  });

  it("keeps intra-delta order for items sharing one through-seq", () => {
    // Arrange / Act — one delta delivering two blocks.
    const store = new ConversationStore();
    store.ingest([
      itemsEffect(
        [
          textItem({ blockId: "a", uuid: "a:0", messageId: "ma" }),
          textItem({ blockId: "b", uuid: "b:0", messageId: "mb" }),
        ],
        5,
      ),
    ]);
    // Assert
    expect(store.state.items.map((i) => (i as TextItem).blockId)).toEqual(["a", "b"]);
  });

  it("a redelivered item keeps its original rank", () => {
    // Arrange — prompt then its turn's result.
    const store = new ConversationStore();
    store.ingest([itemsEffect([userTurnItem("r1")], 10)]);
    store.ingest([itemsEffect([resultItem()], 11)]);
    // Act — a resync redelivers the prompt under a later through-seq.
    store.ingest([itemsEffect([userTurnItem("r1")], 12)]);
    // Assert — reconciled in place, not moved past the result.
    expect(store.state.items.map((i) => i.kind)).toEqual(["user-turn", "result"]);
  });

  it("ranks a live typing preview at the high-water mark, above late replay", () => {
    // Arrange — the store has seen up to seq 50; a preview opens live.
    const store = new ConversationStore();
    store.ingest([itemsEffect([textItem({ blockId: "h1", uuid: "h1:0", messageId: "mh1" })], 50)]);
    store.ingest([typingEffect({ messageId: "mlive" })]);
    // Act — older history (seq 20) arrives after the preview opened.
    store.ingest([itemsEffect([textItem({ blockId: "h0", uuid: "h0:0", messageId: "mh0" })], 20)]);
    // Assert — the preview stays below both history blocks.
    expect(store.state.items.map((i) => (i as TextItem).blockId)).toEqual([
      "h0",
      "h1",
      "mlive:0",
    ]);
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
    store.ingest([typingEffect({ messageId: "u1", blockIndex: 0, delta: "llo" })]);
    // Assert
    expect((store.state.items[0] as TextItem).text).toBe("hello");
  });

  it("creates a text block when the block id is not yet present", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([typingEffect({ messageId: "u9", blockIndex: 2, delta: "start" })]);
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
    store.ingest([typingEffect({ messageId: "u1", blockIndex: 0, delta: "llo" })]);
    // Assert
    expect((store.state.items[0] as TextItem).done).toBe(false);
  });

  it("creates a thinking block for a thinking delta", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([typingEffect({ kind: "thinking", messageId: "u2", blockIndex: 0, delta: "weigh" })]);
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

// --- ignored / batching ------------------------------------------------------

describe("ingest ignored", () => {
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

// ---------------------------------------------------------------------------
// Streaming chunks grow ONE block.
//
// Deltas are keyed by the Anthropic message id, which every chunk of one
// message shares. Keying them on the SDK envelope uuid — fresh per emitted
// event — gave each chunk its own blockId, so the store pushed a new item per
// chunk and the UI rendered a bubble per chunk instead of one growing bubble.
// ---------------------------------------------------------------------------

describe("streaming chunks reconcile into one block", () => {
  it("grows a single item across consecutive chunks of one message", () => {
    // Arrange
    const store = new ConversationStore();

    // Act: three chunks of the SAME message.
    store.ingest([typingEffect({ messageId: "msg_01ABC", delta: "Hel" })]);
    store.ingest([typingEffect({ messageId: "msg_01ABC", delta: "lo " })]);
    store.ingest([typingEffect({ messageId: "msg_01ABC", delta: "there" })]);

    // Assert: one bubble, fully assembled — not three.
    const texts = store.state.items.filter((i) => i.kind === "text");
    expect(texts).toHaveLength(1);
    expect(texts[0]!.kind === "text" && texts[0]!.text).toBe("Hello there");
  });

  it("opens a separate block for a different message", () => {
    // Arrange: two assistant messages in one turn are genuinely two bubbles.
    const store = new ConversationStore();

    // Act
    store.ingest([typingEffect({ messageId: "msg_FIRST", delta: "one" })]);
    store.ingest([typingEffect({ messageId: "msg_SECOND", delta: "two" })]);

    // Assert
    expect(store.state.items.filter((i) => i.kind === "text")).toHaveLength(2);
  });

  it("keeps separate blocks for separate block indexes of one message", () => {
    // Arrange: one message can hold several content blocks.
    const store = new ConversationStore();

    // Act
    store.ingest([typingEffect({ messageId: "msg_01ABC", blockIndex: 0, delta: "a" })]);
    store.ingest([typingEffect({ messageId: "msg_01ABC", blockIndex: 1, delta: "b" })]);

    // Assert
    expect(store.state.items.filter((i) => i.kind === "text")).toHaveLength(2);
  });

  it("replaces the streamed preview with the finished message", () => {
    // Arrange: the preview the chunks grew.
    const store = new ConversationStore();
    store.ingest([typingEffect({ messageId: "msg_01ABC", blockIndex: 0, delta: "Hel" })]);
    store.ingest([typingEffect({ messageId: "msg_01ABC", blockIndex: 0, delta: "lo" })]);

    // Act: the persistent item arrives, keyed on the SAME Anthropic id.
    store.ingest([
      itemsEffect([
        { kind: "text", blockId: "msg_01ABC:0", messageId: "msg_01ABC", text: "Hello", done: true, ts: "t" },
      ]),
    ]);

    // Assert: one item, now final — the preview was superseded, not duplicated.
    const texts = store.state.items.filter((i) => i.kind === "text");
    expect(texts).toHaveLength(1);
    expect(texts[0]!.kind === "text" && texts[0]!.done).toBe(true);
    expect(texts[0]!.kind === "text" && texts[0]!.text).toBe("Hello");
  });
});

// ---------------------------------------------------------------------------
// A finished block SETTLES onto the preview its deltas grew.
//
// The two sides share no key and cannot: a preview is keyed by the API block
// index (the only ordinal the live stream states), while a finished record is
// keyed by an envelope that did not exist while the message was streaming.
//
// The SDK emits one assistant record PER CONTENT BLOCK — every record of one
// API message carrying the same `message.id` and a `content` array of length
// ONE — so the index within `content` is always 0 and says nothing about which
// API block the record holds. Keying finished blocks on it therefore both
// FAILED to meet the preview (a `[thinking, text]` message previewed its text
// at index 1 and finalized it at index 0, so the half-typed card stayed on
// screen beside the final one) and COLLIDED with itself (two thinking blocks
// of one message both keyed `:0`, so the second silently replaced the first).
// ---------------------------------------------------------------------------

/** A finished text block as the adapter now emits it: envelope-keyed. */
function finishedText(over: Partial<TextItem> = {}): TextItem {
  return {
    kind: "text",
    blockId: "env1:0",
    uuid: "env1:0",
    messageId: "msg_01ABC",
    text: "Hello",
    done: true,
    ts: TS,
    ...over,
  };
}

/** A finished thinking block as the adapter now emits it: envelope-keyed. */
function finishedThinking(over: Partial<ThinkingItem> = {}): ThinkingItem {
  return {
    kind: "thinking",
    blockId: "env1:0",
    uuid: "env1:0",
    messageId: "msg_01ABC",
    text: "hmm",
    done: true,
    ...over,
  };
}

describe("a finished block settles onto its streamed preview", () => {
  it("lands a text block previewed at API index 1 on that preview, not beside it", () => {
    // Arrange: the reported bug's exact shape — a [thinking, text] message,
    // so the text streams at API block index 1.
    const store = new ConversationStore();
    store.ingest([typingEffect({ kind: "thinking", messageId: "msg_01ABC", blockIndex: 0, delta: "hmm" })]);
    store.ingest([typingEffect({ kind: "text", messageId: "msg_01ABC", blockIndex: 1, delta: "Hel" })]);

    // Act: the finished text record, whose own content index is 0.
    store.ingest([itemsEffect([finishedText({ blockId: "env2:0", uuid: "env2:0", text: "Hello" })])]);

    // Assert: ONE text card, complete — not a stalled preview plus a final.
    const texts = store.state.items.filter((i) => i.kind === "text");
    expect(texts).toHaveLength(1);
    expect(texts[0]!.kind === "text" && texts[0]!.text).toBe("Hello");
  });

  it("keeps two finished thinking blocks of one message as two items", () => {
    // Arrange: both records carry content index 0, so an index-keyed identity
    // made the second silently overwrite the first.
    const store = new ConversationStore();

    // Act
    store.ingest([itemsEffect([finishedThinking({ blockId: "env1:0", uuid: "env1:0", text: "first" })])]);
    store.ingest([itemsEffect([finishedThinking({ blockId: "env2:0", uuid: "env2:0", text: "second" })])]);

    // Assert: no data loss — both blocks survive.
    const thinking = store.state.items.filter((i) => i.kind === "thinking");
    expect(thinking.map((i) => i.kind === "thinking" && i.text)).toEqual(["first", "second"]);
  });

  it("pins the settled block to the preview's blockId so the reveal never restarts", () => {
    // Arrange: smooth.ts tracks reveal progress BY blockId, so moving the id
    // under a settling block would re-type prose already on screen.
    const store = new ConversationStore();
    store.ingest([typingEffect({ messageId: "msg_01ABC", blockIndex: 3, delta: "Hel" })]);

    // Act
    store.ingest([itemsEffect([finishedText({ blockId: "env9:0", uuid: "env9:0", text: "Hello" })])]);

    // Assert: the settled block kept the id the reveal has been animating.
    const settled = store.state.items.filter((i) => i.kind === "text" && i.done);
    expect(settled.map((i) => i.kind === "text" && i.blockId)).toEqual(["msg_01ABC:3"]);
  });

  it("replaces rather than duplicates when a resync re-delivers a settled block", () => {
    // Arrange: the settled item is keyed by its envelope, so the replay of the
    // very same record must find it even though it holds the preview's blockId.
    const store = new ConversationStore();
    store.ingest([typingEffect({ messageId: "msg_01ABC", blockIndex: 1, delta: "Hel" })]);
    store.ingest([itemsEffect([finishedText({ blockId: "env2:0", uuid: "env2:0", text: "Hello" })])]);

    // Act: the same record again, as a reconnect gap-fill delivers it.
    store.ingest([itemsEffect([finishedText({ blockId: "env2:0", uuid: "env2:0", text: "Hello" })])]);

    // Assert
    expect(store.state.items.filter((i) => i.kind === "text")).toHaveLength(1);
  });

  it("appends a finished block that no preview ever opened", () => {
    // Arrange: a replayed or gap-filled message whose deltas this webapp never
    // saw must still render, rather than being swallowed for want of a preview.
    const store = new ConversationStore();

    // Act
    store.ingest([itemsEffect([finishedText({ text: "backfilled" })])]);

    // Assert
    const texts = store.state.items.filter((i) => i.kind === "text");
    expect(texts).toHaveLength(1);
    expect(texts[0]!.kind === "text" && texts[0]!.text).toBe("backfilled");
  });

  it("does not let a text block settle onto a thinking preview of the same message", () => {
    // Arrange: kind is part of the match, so the two streams cannot cross.
    const store = new ConversationStore();
    store.ingest([typingEffect({ kind: "thinking", messageId: "msg_01ABC", blockIndex: 0, delta: "hmm" })]);

    // Act
    store.ingest([itemsEffect([finishedText({ blockId: "env2:0", uuid: "env2:0", text: "Hello" })])]);

    // Assert: the thinking preview is untouched and the text landed on its own.
    expect(store.state.items.filter((i) => i.kind === "thinking")).toHaveLength(1);
    expect(store.state.items.filter((i) => i.kind === "text")).toHaveLength(1);
  });

  it("settles each of two same-kind blocks onto its own preview, earliest first", () => {
    // Arrange: two thinking blocks streaming, then both finishing. Claiming the
    // EARLIEST unclaimed preview is what keeps the pairing in block order.
    const store = new ConversationStore();
    store.ingest([typingEffect({ kind: "thinking", messageId: "msg_01ABC", blockIndex: 0, delta: "first" })]);
    store.ingest([typingEffect({ kind: "thinking", messageId: "msg_01ABC", blockIndex: 1, delta: "second" })]);

    // Act
    store.ingest([itemsEffect([finishedThinking({ blockId: "env1:0", uuid: "env1:0", text: "first" })])]);
    store.ingest([itemsEffect([finishedThinking({ blockId: "env2:0", uuid: "env2:0", text: "second" })])]);

    // Assert: two items, each pinned to the preview it grew from.
    const thinking = store.state.items.filter((i) => i.kind === "thinking");
    expect(thinking.map((i) => i.kind === "thinking" && i.blockId)).toEqual([
      "msg_01ABC:0",
      "msg_01ABC:1",
    ]);
  });
});

describe("the progress footer's input (F1)", () => {
  /** A resolved progress view, defaulted to a quiet idle session. */
  function progressValue(over: Partial<ProgressInput> = {}): ProgressInput {
    return {
      workspace: "/w",
      sessionId: "s1",
      turnStartedAtMs: 0,
      thinkingTokens: 0,
      inputTokens: 0,
      ttftMs: 0,
      compacting: null,
      retrying: null,
      authenticating: null,
      hook: null,
      blocked: null,
      interrupt: null,
      rateLimited: null,
      failure: null,
      pendingPermissions: 0,
      queueDepth: 0,
      liveTaskCount: 0,
      ...over,
    };
  }

  function progressEffect(over: Partial<ProgressInput> = {}): AdapterEffect {
    return { kind: "progress", value: progressValue(over) };
  }

  it("is null before the daemon has resolved anything", () => {
    // Arrange / Act
    const store = new ConversationStore();
    // Assert
    expect(store.progress).toBeNull();
  });

  it("adopts the resolved view wholesale", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([progressEffect({ inputTokens: 41_200 })]);
    // Assert
    expect(store.progress?.inputTokens).toBe(41_200);
  });

  it("retains the compaction window on the progress view the footer reads", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([progressEffect({ compacting: { sinceMs: 1, detail: "compacting" } })]);
    // Assert
    expect(store.progress?.compacting).toEqual({ sinceMs: 1, detail: "compacting" });
  });

  it("takes the turn's REAL start stamp, so a mid-turn join does not restart the clock", () => {
    // Arrange
    const store = new ConversationStore();
    const startedAt = Date.parse("2024-05-01T12:00:00.000Z");
    // Act
    store.ingest([progressEffect({ turnStartedAtMs: startedAt })]);
    // Assert
    expect(store.state.turnStartedAt).toBe(new Date(startedAt).toISOString());
  });

  it("leaves the turn stamp alone off-turn", () => {
    // Arrange
    const store = new ConversationStore();
    // Act — a 0 stamp is "no turn in flight", not a 1970 start.
    store.ingest([progressEffect()]);
    // Assert
    expect(store.state.turnStartedAt).toBeNull();
  });

  it("is discarded on a rebind onto a different session", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([progressEffect({ inputTokens: 9 })]);
    // Act
    store.reset();
    // Assert
    expect(store.progress).toBeNull();
  });
});

describe("the tokens overlay's cumulative usage sources", () => {
  /** One model's whole-tree slice. */
  function slice(over: Partial<ModelUsage> = {}): ModelUsage {
    return {
      input_tokens: 100,
      output_tokens: 200,
      cache_creation_input_tokens: 0,
      cache_read_input_tokens: 0,
      web_search_requests: 0,
      cost_usd: 0.5,
      context_window: 200_000,
      ...over,
    };
  }

  it("has no top-level baseline before the first result", () => {
    // Arrange / Act — dashes in the overlay rather than a lying zero.
    const store = new ConversationStore();
    // Assert
    expect(store.state.resultUsage).toBeNull();
  });

  it("has no per-model map before the first result", () => {
    // Arrange / Act
    const store = new ConversationStore();
    // Assert
    expect(store.state.modelUsage).toBeNull();
  });

  it("adopts a landed result's top-level usage as the baseline", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([itemsEffect([resultItem({ usage: { input_tokens: 7, output_tokens: 3 } })])]);
    // Assert
    expect(store.state.resultUsage).toEqual({ input_tokens: 7, output_tokens: 3 });
  });

  it("adopts a landed result's per-model map", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([itemsEffect([resultItem({ modelUsage: { opus: slice() } })])]);
    // Assert
    expect(store.state.modelUsage).toEqual({ opus: slice() });
  });

  it("REPLACES the baseline on the next result, since each is cumulative", () => {
    // Arrange — the SDK recomputes the session total per result, never a delta.
    const store = new ConversationStore();
    store.ingest([itemsEffect([resultItem({ usage: { input_tokens: 7, output_tokens: 3 } })])]);
    // Act
    store.ingest([itemsEffect([resultItem({ usage: { input_tokens: 20, output_tokens: 9 } })])]);
    // Assert — 20, not 27.
    expect(store.state.resultUsage).toEqual({ input_tokens: 20, output_tokens: 9 });
  });

  it("REPLACES the per-model map on the next result", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([itemsEffect([resultItem({ modelUsage: { opus: slice({ input_tokens: 1 }) } })])]);
    // Act
    store.ingest([itemsEffect([resultItem({ modelUsage: { opus: slice({ input_tokens: 5 }) } })])]);
    // Assert
    expect(store.state.modelUsage?.opus.input_tokens).toBe(5);
  });

  it("keeps the standing map when a result carries none", () => {
    // Arrange — an absent map is the SDK declining to itemize, not a claim
    // that the session has spent nothing.
    const store = new ConversationStore();
    store.ingest([itemsEffect([resultItem({ modelUsage: { opus: slice() } })])]);
    // Act
    store.ingest([itemsEffect([resultItem()])]);
    // Assert
    expect(store.state.modelUsage).toEqual({ opus: slice() });
  });

  it("clears the per-message tally when a fresh baseline lands", () => {
    // Arrange — a request already folded into the new baseline must not also
    // be summed on top of it by topLevelUsage.
    const store = new ConversationStore();
    store.state.turnUsage.set("m1", { input_tokens: 5, output_tokens: 5 });
    // Act
    store.ingest([itemsEffect([resultItem({ usage: { input_tokens: 7, output_tokens: 3 } })])]);
    // Assert
    expect(topLevelUsage(store.state)).toEqual({ input_tokens: 7, output_tokens: 3 });
  });

  it("discards both sources on a rebind onto a different session", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([itemsEffect([resultItem({ modelUsage: { opus: slice() } })])]);
    // Act
    store.reset();
    // Assert
    expect(store.state.modelUsage).toBeNull();
    expect(store.state.resultUsage).toBeNull();
  });
});
