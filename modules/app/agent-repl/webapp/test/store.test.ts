import { describe, expect, it } from "vitest";

import {
  CONNECTIVITY_WINDOW_FAILURE_TYPES,
  ConversationStore,
  contextTokens,
  liveContextDelta,
  stringField,
  topLevelUsage,
  type ConversationItem,
  type ResultItem,
  type StoreState,
  type SystemFailureCard,
  type TextItem,
  type ThinkingItem,
  type ToolItem,
} from "../src/store.js";
import type { AsyncBubble, AsyncBubbleDelta, AsyncBubbleUpdate } from "../src/async-bubble.js";
import type {
  AdapterEffect,
  MergeStatus,
  MergeStatusCherryPicking,
  ProgressInput,
  QueueInput,
  SessionViewInput,
  ToolProgressInput,
  TypingReveal,
  UnidentifiedToolInputReveal,
  WorkspaceStatusInput,
} from "../src/state-adapter.js";
import type {
  ShutdownScheduleDraining,
  ShutdownScheduleView,
} from "../src/frontend-proto.js";
import type { CounterEntry } from "../src/counter-menu.js";
import type { ModelUsage, Usage } from "../src/protocol.js";
import { generatedSessionUtilization, generatedUngroupedResponse, ungroupedResponse } from "./token-utilization-fixture.js";

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
      causeKind: "turn_started",
      causeSeq: 1,
      atMs: 1000,
      connectivity: "operational",
      sessionStatus: "thinking",
      controllerGenerationId: "g1",
      activeFaults: [],
      mergeLeaseHeld: false,
      mergeStatus: null,
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
      models: [],
      hibernation: null,
      ...over,
    },
  };
}

type TextReveal = { workspace: string; sessionId: string; messageId: string; blockIndex: number; kind: "text"; delta: string };
type ThinkingReveal = { workspace: string; sessionId: string; messageId: string; blockIndex: number; kind: "thinking"; delta: string };
type InputReveal = { workspace: string; sessionId: string; messageId: string; blockIndex: number; kind: "input_json"; toolUseId: string; delta: string };

function typingEffect(over: Partial<TextReveal> = {}): AdapterEffect {
  return {
    kind: "typing",
    value: {
      workspace: "ws",
      fence: "s1",
      messageId: "u1",
      blockIndex: 0,
      kind: "text",
      delta: "hi",
      ...over,
    },
  };
}

function inputTypingEffect(over: Partial<InputReveal> = {}): AdapterEffect {
  return {
    kind: "typing",
    value: {
      workspace: "ws",
      fence: "s1",
      messageId: "u1",
      blockIndex: 0,
      kind: "input_json",
      toolUseId: "tu1",
      delta: "{",
      ...over,
    },
  };
}

function thinkingTypingEffect(over: Partial<ThinkingReveal> = {}): AdapterEffect {
  return {
    kind: "typing",
    value: { workspace: "ws", fence: "s1", messageId: "u1", blockIndex: 0, kind: "thinking", delta: "hmm", ...over },
  };
}

function unidentifiedInputTypingEffect(over: Partial<UnidentifiedToolInputReveal> = {}): AdapterEffect {
  return {
    kind: "typing",
    value: { workspace: "ws", fence: "s1", messageId: "u1", blockIndex: 0, kind: "input_json", delta: "{", ...over },
  };
}

function itemsEffect(items: ConversationItem[], throughSeq = 0): AdapterEffect {
  return { kind: "conversation-items", workspace: "ws", fence: "s1", throughSeq, items };
}

function catalogEffect(entries: CounterEntry[]): AdapterEffect {
  return { kind: "task-catalog", value: { workspace: "ws", fence: "s1", entries } };
}

// --- item builders ----------------------------------------------------------

const TS = "2026-07-19T12:00:00.000Z";

function textItem(over: Partial<TextItem> = {}): TextItem {
  return { kind: "text", blockId: "b1", messageId: "m1", text: "hello", done: true, ts: TS, ...over };
}

function failureCard(over: Partial<SystemFailureCard> = {}): SystemFailureCard {
  return {
    kind: "failure",
    errorClass: "INTERNAL",
    errorType: "client.daemon_unreachable",
    message: "lost the connection to the daemon; reconnecting",
    sourceDetail: "close=1005",
    resolvedAtMs: 0,
    uuid: "local:client.daemon_unreachable",
    detail: { kind: "none" },
    ...over,
  };
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

  it("adopts the merge lease, which the composer gates on", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([workspaceEffect({ state: "merge_conflict", mergeLeaseHeld: true })]);
    // Assert
    expect(store.state.mergeLeaseHeld).toBe(true);
  });

  it("adopts the merge-queue place beside the phase that names it", () => {
    // REWRITTEN off the retired flat merge_queue_position / merge_queue_depth
    // pair onto MergeStatus's `enqueued` arm, which is the only surface those
    // figures arrive on now. The guarantee is unchanged: the queue place is
    // adopted from the same revisioned message that named the phase.
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([
      workspaceEffect({ state: "merge_queued", mergeStatus: enqueuedStatus(2, 3) }),
    ]);
    // Assert
    expect(store.state.mergeStatus?.phase).toEqual({
      case: "enqueued",
      value: { position: 2, depth: 3 },
    });
  });

  it("releases the merge lease when a later revision clears it", () => {
    // Arrange — the daemon clears the flag on the same revisioned message that
    // set it, so the composer un-gates with no local state to unwind.
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ state: "merging", mergeLeaseHeld: true })]);
    // Act
    store.ingest([workspaceEffect({ state: "merged", mergeLeaseHeld: false, atMs: 1001 })]);
    // Assert
    expect(store.state.mergeLeaseHeld).toBe(false);
  });

  it("drops the merge lease when frontend state is invalidated", () => {
    // Arrange — a lease that outlived its freshness lease would leave the
    // composer gated against a state nothing current claims.
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ state: "merging", mergeLeaseHeld: true })]);
    // Act
    store.invalidateFrontendState("websocket_disconnected");
    // Assert
    expect(store.state.mergeLeaseHeld).toBe(false);
  });

  it("drops the merge-queue place when frontend state is invalidated", () => {
    // REWRITTEN off the retired flat pair onto the `enqueued` arm that carries
    // the figures now. The guarantee is unchanged: a queue place that outlived
    // its freshness lease must not stay on screen.
    // Arrange
    const store = new ConversationStore();
    store.ingest([
      workspaceEffect({ state: "merge_queued", mergeStatus: enqueuedStatus(2, 3) }),
    ]);
    // Act
    store.invalidateFrontendState("websocket_disconnected");
    // Assert
    expect(store.state.mergeStatus).toBeNull();
  });

  /** An enqueued merge status at a given place on its repository's queue. */
  function enqueuedStatus(position: number, depth: number): MergeStatus {
    return {
      runId: "run-enqueued",
      phaseStartedAtMs: 900,
      updatedAtMs: 1000,
      phase: { case: "enqueued", value: { position, depth } },
    };
  }

  // --- the structured merge status -----------------------------------------

  /** A cherry-picking merge status, defaulted to the first of four commits. */
  function pickingStatus(over: Partial<MergeStatusCherryPicking> = {}): MergeStatus {
    return {
      runId: "run-1",
      phaseStartedAtMs: 900,
      updatedAtMs: 1000,
      phase: {
        case: "cherryPicking",
        value: {
          commitsTotal: 4,
          commitsLanded: 1,
          currentSha: "abc1234",
          currentSubject: "fix the thing",
          ...over,
        },
      },
    };
  }

  it("adopts the structured merge status beside the phase word", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([workspaceEffect({ state: "merging", mergeStatus: pickingStatus() })]);
    // Assert
    expect(store.state.mergeStatus?.phase).toEqual({
      case: "cherryPicking",
      value: { commitsTotal: 4, commitsLanded: 1, currentSha: "abc1234", currentSubject: "fix the thing" },
    });
  });

  it("ingests a per-commit tick at a new revision without a conflict violation", () => {
    // Arrange — the run's first commit has landed at revision 1000.
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ state: "merging", mergeStatus: pickingStatus() })]);
    // Act — the second lands, which is a NEW revision of the same run whose
    // only moved fields live inside the merge status.
    store.ingest([
      workspaceEffect({
        state: "merging",
        atMs: 1001,
        mergeStatus: {
          ...pickingStatus({ commitsLanded: 2, currentSha: "def5678", currentSubject: "next one" }),
          updatedAtMs: 1001,
        },
      }),
    ]);
    // Assert
    expect(store.state.mergeStatus?.phase.value).toMatchObject({ commitsLanded: 2 });
  });

  it("rejects two different merge statuses claiming the SAME revision", () => {
    // Arrange — the fingerprint must include the merge status, or a genuinely
    // conflicting duplicate would fingerprint identically to the state held.
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ state: "merging", mergeStatus: pickingStatus() })]);
    // Act / Assert — same atMs, different landed count.
    expect(() =>
      store.ingest([
        workspaceEffect({ state: "merging", mergeStatus: pickingStatus({ commitsLanded: 3 }) }),
      ]),
    ).toThrow(/revision conflicted/);
  });

  it("drops the merge status when frontend state is invalidated", () => {
    // Arrange — a status that outlived its freshness lease would keep a merge
    // on screen that nothing current claims.
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ state: "merging", mergeStatus: pickingStatus() })]);
    // Act
    store.invalidateFrontendState("websocket_disconnected");
    // Assert
    expect(store.state.mergeStatus).toBeNull();
  });

  it("reports a change when invalidation clears a held merge status", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ state: "merging", mergeStatus: pickingStatus() })]);
    // Act
    const changed = store.invalidateFrontendState("lease_expired");
    // Assert
    expect(changed).toBe(true);
  });

  it("clears turnInFlight when turnActive is false", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ turnActive: true })]);
    // Act
    store.ingest([workspaceEffect({ turnActive: false, state: "done", atMs: 1001 })]);
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
    store.ingest([workspaceEffect({ turnActive: false, state: "done", atMs: 1001 })]);
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

  it("logs state adoption with transition identity and old/new values", () => {
    const logs: string[] = [];
    const store = new ConversationStore((_level, message) => logs.push(message));

    store.ingest([
      workspaceEffect({
        state: "ready",
        turnActive: false,
        causeKind: "session_started",
        causeSeq: 7,
        atMs: 1234,
      }),
    ]);

    expect(logs).toEqual([
      expect.stringContaining(
        "state=none->ready connectivity=none->operational status=none->thinking generation=g1 " +
          "turn_active=false->false live_tasks=0 faults=none " +
          "merge_lease_held=false merge_status=none cause_kind=session_started cause_seq=7 at_ms=1234",
      ),
    ]);
  });

  it("rejects a regressing WorkspaceState revision without partial mutation", () => {
    const logs: string[] = [];
    const store = new ConversationStore((_level, message) => logs.push(message));
    store.ingest([workspaceEffect({ state: "done", turnActive: false, atMs: 2000 })]);

    expect(() => store.ingest([workspaceEffect({ state: "thinking", turnActive: true, atMs: 1999 })]))
      .toThrow("WorkspaceState revision regressed");
    expect(store.state.renderState).toBe("done");
    expect(store.state.turnInFlight).toBe(false);
    expect(logs.at(-1)).toContain("INVARIANT VIOLATION");
  });

  it("rejects a non-positive WorkspaceState revision", () => {
    const store = new ConversationStore();
    expect(() => store.ingest([workspaceEffect({ atMs: 0 })]))
      .toThrow("WorkspaceState has non-positive revision");
    expect(store.state.renderState).toBeNull();
  });

  it("rejects conflicting payloads at one WorkspaceState revision", () => {
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ state: "done", turnActive: false, atMs: 2000 })]);
    expect(() => store.ingest([workspaceEffect({ state: "ready", turnActive: false, atMs: 2000 })]))
      .toThrow("WorkspaceState revision conflicted");
  });

  it("invalidates every socket-owned active projection while retaining conversation metadata", () => {
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ state: "thinking", turnActive: true, atMs: 2000 })]);
    store.state.cwd = "/w";

    const changed = store.invalidateFrontendState("websocket_disconnected");

    expect(changed).toBe(true);
    expect(store.state.renderState).toBeNull();
    expect(store.state.turnInFlight).toBe(false);
    expect(store.state.cwd).toBe("/w");
    expect(store.state.workspaceStateAtMs).toBe(2000);
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

  it("clears a prior model when a new session's authoritative view carries none", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([sessionEffect({ sessionId: "s1", model: "opus" })]);
    // Act
    store.ingest([sessionEffect({ sessionId: "s2", model: "" })]);
    // Assert
    expect(store.state.sessionId).toBe("s2");
    expect(store.state.model).toBe("");
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

  it("retains each ungrouped subagent response without aggregating it", () => {
    const responses = [
      generatedUngroupedResponse({ apiMessageId: "message-one", usage: { ...ungroupedResponse().usage, inputTokens: 11 } }),
      generatedUngroupedResponse({ apiMessageId: "message-two", usage: { ...ungroupedResponse().usage, inputTokens: 22 } }),
    ];
    const tokenUtilization = generatedSessionUtilization(responses);
    const store = new ConversationStore();

    store.ingest([sessionEffect({ tokenUtilization })]);

    expect(store.state.tokenUtilization).toBe(tokenUtilization);
    expect(store.state.tokenUtilization?.ungroupedSubagentResponses).toEqual(responses);
    expect(store.state.tokenUtilization?.ungroupedSubagentResponses.map((response) => [
      response.apiMessageId,
      response.usage?.inputTokens,
    ])).toEqual([["message-one", 11n], ["message-two", 22n]]);
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

// --- session identity authority ---------------------------------------------
//
// WorkspaceState is the SOLE writer of the workspace's live session identity.
// A SessionView is a per-session catalog entry that legitimately describes
// retired sessions, so it may seed an identity nobody has ruled on, never
// rebind one WorkspaceState has.

describe("session identity authority", () => {
  it("seeds the identity from a session view before any workspace state rules", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([sessionEffect({ sessionId: "s_live" })]);
    // Assert
    expect(store.state.sessionId).toBe("s_live");
  });

  it("adopts the identity from a workspace state", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([workspaceEffect({ sessionId: "s_live" })]);
    // Assert
    expect(store.state.sessionId).toBe("s_live");
  });

  it("keeps the workspace-state identity when a retired session view follows", () => {
    // Arrange — the mount ordering that deadlocked the view: live workspace
    // ruling, then the superseded session's catalog entry.
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ sessionId: "s_live" })]);
    // Act
    store.ingest([sessionEffect({ sessionId: "s_retired" })]);
    // Assert
    expect(store.state.sessionId).toBe("s_live");
  });

  it("reports the rejected session view identity", () => {
    // Arrange
    const logged: string[] = [];
    const store = new ConversationStore((_level, message) => logged.push(message));
    store.ingest([workspaceEffect({ sessionId: "s_live" })]);
    // Act
    store.ingest([sessionEffect({ sessionId: "s_retired" })]);
    // Assert
    expect(logged.some((line) => line.includes("session view identity rejected"))).toBe(true);
  });

  it("rebinds the identity when the workspace state rotates its owning session", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ sessionId: "s_old" })]);
    // Act
    store.ingest([workspaceEffect({ sessionId: "s_new" })]);
    // Assert
    expect(store.state.sessionId).toBe("s_new");
  });

  it("keeps the ruled identity when a corroborating session view repeats it", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ sessionId: "s_live" })]);
    // Act
    store.ingest([sessionEffect({ sessionId: "s_live" })]);
    // Assert
    expect(store.state.sessionId).toBe("s_live");
  });

  it("leaves hibernation untouched when a non-owning session view reports it", () => {
    // Arrange — a retired session's catalog entry must not flip the composer
    // and revival gates the live session owns.
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ sessionId: "s_live" })]);
    // Act
    store.ingest([
      sessionEffect({
        sessionId: "s_retired",
        hibernation: { sinceMs: 1, cause: { case: "forced", value: {} } },
      }),
    ]);
    // Assert
    expect(store.state.hibernation).toBeNull();
  });

  it("leaves the cost and model fields untouched when a non-owning session view carries them", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ sessionId: "s_live" })]);
    store.ingest([sessionEffect({ sessionId: "s_live", model: "opus", totalCostUsd: 1.5 })]);
    // Act
    store.ingest([sessionEffect({ sessionId: "s_retired", model: "haiku", totalCostUsd: 99 })]);
    // Assert
    expect([store.state.model, store.state.costUsd]).toEqual(["opus", 1.5]);
  });

  it("applies the whole view when the owning session reports it", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ sessionId: "s_live" })]);
    // Act
    store.ingest([sessionEffect({ sessionId: "s_live", model: "opus", totalCostUsd: 2.5 })]);
    // Assert
    expect([store.state.model, store.state.costUsd]).toEqual(["opus", 2.5]);
  });

  it("still seeds the whole view before any workspace state rules", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([sessionEffect({ sessionId: "s_seed", model: "opus", totalCostUsd: 3.5 })]);
    // Assert
    expect([store.state.sessionId, store.state.model, store.state.costUsd]).toEqual([
      "s_seed",
      "opus",
      3.5,
    ]);
  });

  it("lets a session view seed again after a reset drops the workspace ruling", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ sessionId: "s_live" })]);
    store.reset();
    // Act
    store.ingest([sessionEffect({ sessionId: "s_next" })]);
    // Assert
    expect(store.state.sessionId).toBe("s_next");
  });

  it("leaves the ruled identity untouched when a progress view names another session", () => {
    // Arrange — the interrupt-adoption path correlates a session, never binds
    // one.
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ sessionId: "s_live" })]);
    const progress: ProgressInput = {
      workspace: "ws",
      fence: "s_retired",
      turnStartedAtMs: 0,
      thinkingTokens: 0,
      inputTokens: 0,
      ttftMs: 0,
      compacting: null,
      retrying: null,
      authenticating: null,
      hook: null,
      blocked: null,
      interrupt: { outcome: "interrupted", sinceMs: 0 },
      rateLimited: null,
      rateLimitedWeekly: null,
      failure: null,
      expensiveTurn: null,
      pendingPermissions: 0,
      queueDepth: 0,
      liveTaskCount: 0,
    };
    // Act
    store.ingest([{ kind: "progress", value: progress }]);
    // Assert
    expect(store.state.sessionId).toBe("s_live");
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

  /**
   * A prompt as the REAL pipeline delivers it: a transcript user line, whose
   * request id is empty and whose only identity is the record uuid.
   */
  function transcriptTurn(uuid: string, text: string): ConversationItem {
    return { kind: "user-turn", requestId: "", uuid, content: [{ type: "text", text }], ts: TS };
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

  it("ranks a seq-less daemon push at the feed tail", () => {
    // Arrange — history up to seq 10.
    const store = new ConversationStore();
    store.ingest([itemsEffect([textItem({ blockId: "h1", uuid: "h1:0" })], 10)]);
    // Act — a daemon-composed delta (through-seq 0: a prompt receipt, a
    // permission card, a failure card — none of them store facts).
    store.ingest([itemsEffect([userTurnItem("r1")], 0)]);
    // Assert — it describes what is happening NOW, so it belongs at the tail;
    // rank 0 would file it above every item the session ever produced.
    expect(store.state.items.map((i) => i.kind)).toEqual(["text", "user-turn"]);
  });

  it("leaves a seq-less item where it landed when its durable twin arrives", () => {
    // Arrange — history, then the daemon's seq-less prompt receipt.
    const store = new ConversationStore();
    store.ingest([itemsEffect([textItem({ blockId: "h1", uuid: "h1:0" })], 10)]);
    store.ingest([itemsEffect([userTurnItem("r1")], 0)]);
    // Act — the durable transcript line, stamped with the same request id.
    store.ingest([itemsEffect([userTurnItem("r1")], 11)]);
    // Assert — one bubble, still at the tail: a redelivery replaces content,
    // never position.
    expect(store.state.items.map((i) => i.kind)).toEqual(["text", "user-turn"]);
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

  it("keeps two request-id-less prompts apart on their uuids", () => {
    // Arrange — the LIVE shape: transcript-borne prompts, empty request ids.
    const store = new ConversationStore();
    store.ingest([itemsEffect([transcriptTurn("u1", "first prompt")], 10)]);
    // Act — a second prompt lands under the same (empty) request id.
    store.ingest([itemsEffect([transcriptTurn("u2", "second prompt")], 11)]);
    // Assert — two bubbles, not one overwritten by the other.
    expect(store.state.items.map((i) => i.kind)).toEqual(["user-turn", "user-turn"]);
  });

  it("reconciles a redelivered request-id-less prompt on its uuid", () => {
    // Arrange — a resync replays the very same transcript line.
    const store = new ConversationStore();
    store.ingest([itemsEffect([transcriptTurn("u1", "only prompt")], 10)]);
    // Act
    store.ingest([itemsEffect([transcriptTurn("u1", "only prompt")], 12)]);
    // Assert — one bubble, not a duplicate.
    expect(store.state.items).toHaveLength(1);
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
    store.ingest([thinkingTypingEffect({ messageId: "u2", blockIndex: 0, delta: "weigh" })]);
    // Assert
    expect(store.state.items[0].kind).toBe("thinking");
  });

  it("grows the exact tool's input for an input_json delta", () => {
    const store = new ConversationStore();
    store.ingest([
      itemsEffect([toolItem({ toolUseId: "tu1", inputJson: '{"cmd":', inputDone: false })]),
    ]);
    // Act
    store.ingest([inputTypingEffect({ delta: '"ls"}' })]);
    // Assert
    expect((store.state.items[0] as ToolItem).inputJson).toBe('{"cmd":"ls"}');
  });

  it("emits one verbose exact-ID reconciliation record with the chunk evidence", () => {
    const logged: Array<[string, string, Record<string, unknown> | undefined, boolean | undefined]> = [];
    const store = new ConversationStore((level, message, context, verbose) => logged.push([level, message, context, verbose]));
    store.ingest([sessionEffect({ claudeSessionId: "claude-s1" })]);
    store.ingest([inputTypingEffect({ toolUseId: "tu-evidence", messageId: "api-msg", blockIndex: 4, delta: "xyz" })]);
    expect(logged).toEqual([[
      "info",
      "tool input delta reconciled branch=preview_created",
      expect.objectContaining({
        operation: "conversation-store.tool-input",
        workspace: "ws",
        agent_repl_session_id: "s1",
        claude_session_id: "claude-s1",
        api_message_id: "api-msg",
        block_index: 4,
        tool_use_id: "tu-evidence",
        chunk_length: 3,
        item_phase: "absent",
        branch: "preview_created",
      }),
      true,
    ]]);
  });

  it("rejects a missing tool identity once before any batch mutation", () => {
    const logged: Array<[string, string, Record<string, unknown> | undefined]> = [];
    const store = new ConversationStore((level, message, context) => logged.push([level, message, context]));
    store.ingest([sessionEffect({ claudeSessionId: "claude-s1" })]);
    const before = structuredClone(store.state);
    expect(() => store.ingest([unidentifiedInputTypingEffect(), typingEffect({ delta: "must-not-land" })])).toThrow(/missing stable tool identity/);
    expect(store.state).toEqual(before);
    expect(logged).toHaveLength(1);
    expect(logged[0][0]).toBe("error");
    expect(logged[0][2]).toMatchObject({
      operation: "conversation-store.tool-input",
      workspace: "ws",
      agent_repl_session_id: "s1",
      claude_session_id: "claude-s1",
      api_message_id: "u1",
      block_index: 0,
      tool_use_id: "",
      chunk_length: 1,
      item_phase: "unresolved",
      branch: "missing_identity",
    });
  });

  it("replaces an ephemeral preview with authoritative durable input", () => {
    const store = new ConversationStore();
    store.ingest([inputTypingEffect({ toolUseId: "tu-preview", delta: '{"partial":' })]);
    store.ingest([itemsEffect([toolItem({ toolUseId: "tu-preview", input: { command: "ls" }, inputDone: true })])]);
    expect(store.state.items).toMatchObject([{ kind: "tool", toolUseId: "tu-preview", input: { command: "ls" }, inputJson: "", inputDone: true }]);
  });

  it("reconciles multiple tool blocks of one API message by their individual identities", () => {
    const store = new ConversationStore();
    store.ingest([
      inputTypingEffect({ messageId: "api-one", blockIndex: 0, toolUseId: "tu-left", delta: "left" }),
      inputTypingEffect({ messageId: "api-one", blockIndex: 1, toolUseId: "tu-right", delta: "right" }),
    ]);
    expect(store.state.items).toMatchObject([
      { kind: "tool", toolUseId: "tu-left", inputJson: "left" },
      { kind: "tool", toolUseId: "tu-right", inputJson: "right" },
    ]);
  });

  it("keeps a durable final authoritative when reconnect delivers a late chunk", () => {
    const store = new ConversationStore();
    store.ingest([itemsEffect([toolItem({ toolUseId: "tu-final", input: { command: "pwd" }, inputDone: true })])]);
    expect(store.ingest([inputTypingEffect({ toolUseId: "tu-final", delta: "ignored" })]).changed).toBe(false);
    expect((store.state.items[0] as ToolItem)).toMatchObject({ input: { command: "pwd" }, inputJson: "", inputDone: true });
  });

  it("keeps exact durable redelivery idempotent after an input preview", () => {
    const store = new ConversationStore();
    const final = toolItem({ toolUseId: "tu-redeliver", input: { path: "/a" }, inputDone: true });
    store.ingest([inputTypingEffect({ toolUseId: "tu-redeliver", delta: '{"path":' })]);
    store.ingest([itemsEffect([final]), itemsEffect([final])]);
    expect(store.state.items).toHaveLength(1);
    expect((store.state.items[0] as ToolItem).input).toEqual({ path: "/a" });
  });

  it("preserves exact final input across deterministic cross-plane permutations", () => {
    const permutations = <T,>(values: readonly T[]): T[][] => values.length === 0
      ? [[]]
      : values.flatMap((value, index) => permutations([...values.slice(0, index), ...values.slice(index + 1)]).map((rest) => [value, ...rest]));
    const events = ["first", "second", "final"] as const;
    for (const order of permutations(events)) {
      const store = new ConversationStore();
      for (const event of order) {
        if (event === "final") {
          store.ingest([itemsEffect([toolItem({ toolUseId: "tu-permute", input: { command: "echo final" }, inputDone: true })])]);
        } else {
          store.ingest([inputTypingEffect({ toolUseId: "tu-permute", delta: event })]);
        }
      }
      expect(store.state.items).toHaveLength(1);
      expect(store.state.items[0]).toMatchObject({ kind: "tool", toolUseId: "tu-permute", input: { command: "echo final" }, inputJson: "", inputDone: true });
    }
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
      { kind: "session-init", value: { workspace: "ws", fence: "s1", init: { model: "claude", cwd: "/w" } } },
    ]);
    // Assert
    expect(store.state.systemInit).toEqual({ model: "claude", cwd: "/w" });
  });

  it("replaces the retained init wholesale on the next push", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([{ kind: "session-init", value: { workspace: "ws", fence: "s1", init: { fastModeState: "off" } } }]);
    // Act
    store.ingest([{ kind: "session-init", value: { workspace: "ws", fence: "s1", init: { fastModeState: "on" } } }]);
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

// --- rebaseSeqSpace ---------------------------------------------------------

describe("rebaseSeqSpace", () => {
  it("drops the retired conversation and its seq mark", () => {
    // Arrange — the vendor rotated its session uuid, so these items and this
    // mark count in a store seq space that no longer exists.
    const store = new ConversationStore();
    store.ingest([itemsEffect([textItem()], 1060)]);
    // Act
    store.rebaseSeqSpace();
    // Assert
    expect([store.state.items.length, store.state.lastSeq]).toEqual([0, 0]);
  });

  it("keeps the session the rotation did not change", () => {
    // Arrange — the daemon session, its cwd and its model are facts about the
    // SESSION; only the conversation rotated.
    const store = new ConversationStore();
    store.ingest([sessionEffect({ model: "opus", cwd: "/ws" }), itemsEffect([textItem()], 1060)]);
    // Act
    store.rebaseSeqSpace();
    // Assert
    expect([store.state.model, store.state.cwd]).toEqual(["opus", "/ws"]);
  });

  it("ranks the new space's first item at its own low seq", () => {
    // Arrange — the whole point of dropping the mark: the new space starts at 1,
    // and a stale watermark would rank every arriving item beneath history that
    // no longer exists.
    const store = new ConversationStore();
    store.ingest([itemsEffect([textItem()], 1060)]);
    store.rebaseSeqSpace();
    // Act
    store.ingest([itemsEffect([textItem({ blockId: "b-new", messageId: "m-new" })], 1)]);
    // Assert
    expect([store.state.items.length, store.state.lastSeq]).toEqual([1, 1]);
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
      fence: "s1",
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
  return { kind: "queue", value: { workspace: "ws", fence: "s1", entries } };
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
    store.ingest([thinkingTypingEffect({ messageId: "msg_01ABC", blockIndex: 0, delta: "hmm" })]);
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
    store.ingest([thinkingTypingEffect({ messageId: "msg_01ABC", blockIndex: 0, delta: "hmm" })]);

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
    store.ingest([thinkingTypingEffect({ messageId: "msg_01ABC", blockIndex: 0, delta: "first" })]);
    store.ingest([thinkingTypingEffect({ messageId: "msg_01ABC", blockIndex: 1, delta: "second" })]);

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
      fence: "s1",
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
      rateLimitedWeekly: null,
      failure: null,
      expensiveTurn: null,
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

  it("logs interrupt-window adoption only when the outcome changes", () => {
    const logs: string[] = [];
    const store = new ConversationStore((_level, message) => logs.push(message));

    store.ingest([
      progressEffect({
        interrupt: { sinceMs: 42, outcome: "already_complete" },
      }),
    ]);
    store.ingest([
      progressEffect({
        interrupt: { sinceMs: 42, outcome: "already_complete" },
      }),
    ]);

    expect(logs).toEqual([
      expect.stringContaining(
        "outcome=none->already_complete since_ms=42 turn_started_at_ms=0",
      ),
    ]);
  });

  it("rejects already-complete progress beside an active WorkspaceState", () => {
    const logs: string[] = [];
    const store = new ConversationStore((_level, message) => logs.push(message));
    store.ingest([workspaceEffect({ state: "thinking", turnActive: true, atMs: 2000 })]);

    expect(() => store.ingest([
      progressEffect({ interrupt: { sinceMs: 42, outcome: "already_complete" } }),
    ])).toThrow("ALREADY_COMPLETE contradicts active WorkspaceState");
    expect(store.progress).toBeNull();
    expect(logs.at(-1)).toContain("INVARIANT VIOLATION");
  });

  it("rejects already-complete progress beside the pre-submit active phase", () => {
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ state: "submitting", turnActive: true, atMs: 2000 })]);
    expect(() => store.ingest([
      progressEffect({ interrupt: { sinceMs: 42, outcome: "already_complete" } }),
    ])).toThrow("ALREADY_COMPLETE contradicts active WorkspaceState");
  });

  it("accepts an atomic snapshot that settles state before already-complete progress", () => {
    const store = new ConversationStore();
    store.ingest([workspaceEffect({ state: "thinking", turnActive: true, atMs: 2000 })]);
    store.ingest([
      workspaceEffect({ state: "done", turnActive: false, atMs: 2001 }),
      progressEffect({ interrupt: { sinceMs: 42, outcome: "already_complete" } }),
    ]);
    expect(store.state.renderState).toBe("done");
    expect(store.progress?.interrupt?.outcome).toBe("already_complete");
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

// A connectivity window's closing edge RETRACTS its opening card: once the
// link is back there is no condition left to report, and a settled
// "reconnecting" card reads as a standing fault to anyone who misses the small
// resolved stamp beneath it.
describe("resolved connectivity failures", () => {
  it("removes the open card when the local daemon-unreachable window closes", () => {
    // Arrange
    const store = new ConversationStore();
    store.addFailure(failureCard());
    // Act
    store.addFailure(failureCard({ message: "reconnected to the daemon", sourceDetail: "", resolvedAtMs: 1700000000000 }));
    // Assert
    expect(store.state.items).toEqual([]);
  });

  it("removes the open card when the daemon's shim-degraded window closes", () => {
    // Arrange
    const store = new ConversationStore();
    const open = failureCard({ errorType: "shim.degraded", uuid: "degraded:s1:connection" });
    store.ingest([itemsEffect([open], 4)]);
    // Act
    store.ingest([itemsEffect([{ ...open, resolvedAtMs: 1700000000000 }], 5)]);
    // Assert
    expect(store.state.items).toEqual([]);
  });

  it("files no card when the closing edge finds nothing open", () => {
    // Arrange — a view that loaded after the drop never held the opening card.
    const store = new ConversationStore();
    // Act
    store.addFailure(failureCard({ resolvedAtMs: 1700000000000 }));
    // Assert
    expect(store.state.items).toEqual([]);
  });

  it("logs the retraction of a card it did remove", () => {
    // Arrange
    const lines: string[] = [];
    const store = new ConversationStore((_level, message) => lines.push(message));
    store.addFailure(failureCard());
    // Act
    store.addFailure(failureCard({ resolvedAtMs: 1700000000000 }));
    // Assert
    expect(lines).toContainEqual(
      "retracted the resolved connectivity card failure:local:client.daemon_unreachable (client.daemon_unreachable)",
    );
  });

  it("logs the closing edge that found no open card", () => {
    // Arrange
    const lines: string[] = [];
    const store = new ConversationStore((_level, message) => lines.push(message));
    // Act
    store.addFailure(failureCard({ resolvedAtMs: 1700000000000 }));
    // Assert
    expect(lines).toContainEqual(
      "connectivity window client.daemon_unreachable closed with no open card to retract (failure:local:client.daemon_unreachable)",
    );
  });

  it("leaves an OPEN connectivity card standing", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.addFailure(failureCard());
    // Assert
    expect(store.state.items).toEqual([expect.objectContaining({ uuid: "local:client.daemon_unreachable" })]);
  });

  it("settles a resolved store-outage card in place rather than retracting it", () => {
    // Arrange — dropped conversation is permanently gone, so its record stays.
    const store = new ConversationStore();
    const open = failureCard({ errorType: "shim.store_write_rejected", uuid: "degraded:s1:store" });
    store.ingest([itemsEffect([open], 4)]);
    // Act
    store.ingest([itemsEffect([{ ...open, resolvedAtMs: 1700000000000 }], 5)]);
    // Assert
    expect(store.state.items).toEqual([expect.objectContaining({ resolvedAtMs: 1700000000000 })]);
  });

  it("retracts only the named window, leaving neighbouring items alone", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([itemsEffect([textItem()], 3)]);
    store.addFailure(failureCard());
    // Act
    store.addFailure(failureCard({ resolvedAtMs: 1700000000000 }));
    // Assert
    expect(store.state.items).toEqual([expect.objectContaining({ kind: "text" })]);
  });

  it("names both transport windows and nothing else", () => {
    // Arrange / Act / Assert — the closed set the retraction rule keys on.
    expect(CONNECTIVITY_WINDOW_FAILURE_TYPES).toEqual([
      "client.daemon_unreachable",
      "shim.degraded",
    ]);
  });
});

// --- the scheduled-shutdown drain lease --------------------------------------

function drainingLease(over: Partial<ShutdownScheduleDraining> = {}): ShutdownScheduleDraining {
  return {
    scheduleId: "sched-1",
    scheduledAtMs: 1_700_000_000_000,
    cause: "manual restart",
    stopShims: false,
    holds: [{ workspace: "/w/app", sessionId: "s1", turn: { turnId: "t-1" } }],
    ...over,
  };
}

function leaseEffect(view: ShutdownScheduleView): AdapterEffect {
  return { kind: "shutdown-schedule", value: view };
}

const IDLE_LEASE: ShutdownScheduleView = { state: { case: "idle", value: {} } };

describe("ingest shutdown schedule", () => {
  it("holds no lease before any frame carries one", () => {
    // Arrange / Act / Assert — null is "no drain", not "unknown drain".
    expect(new ConversationStore().state.shutdownSchedule).toBeNull();
  });

  it("adopts a draining lease from a broadcast edge", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([leaseEffect({ state: { case: "draining", value: drainingLease() } })]);
    // Assert
    expect(store.state.shutdownSchedule?.scheduleId).toBe("sched-1");
  });

  it("re-adopts a shrinking holds list under the same schedule", () => {
    // Arrange — the holds list IS the drain's progress.
    const store = new ConversationStore();
    store.ingest([
      leaseEffect({
        state: {
          case: "draining",
          value: drainingLease({
            holds: [
              { workspace: "/w/a", sessionId: "s1", turn: { turnId: "t-1" } },
              { workspace: "/w/b", sessionId: "s2", tasks: { count: 2 } },
            ],
          }),
        },
      }),
    ]);
    // Act
    store.ingest([leaseEffect({ state: { case: "draining", value: drainingLease() } })]);
    // Assert
    expect(store.state.shutdownSchedule?.holds).toHaveLength(1);
  });

  it("CLEARS the lease on an idle broadcast", () => {
    // Arrange — a cancel or a completed drain has to take the banner down.
    const store = new ConversationStore();
    store.ingest([leaseEffect({ state: { case: "draining", value: drainingLease() } })]);
    // Act
    store.ingest([leaseEffect(IDLE_LEASE)]);
    // Assert
    expect(store.state.shutdownSchedule).toBeNull();
  });

  it("reports no visible change when idle follows idle", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    const result = store.ingest([leaseEffect(IDLE_LEASE)]);
    // Assert
    expect(result.changed).toBe(false);
  });

  it("reports a visible change when a drain starts", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    const result = store.ingest([
      leaseEffect({ state: { case: "draining", value: drainingLease() } }),
    ]);
    // Assert
    expect(result.changed).toBe(true);
  });

  it("drops the lease when frontend state is invalidated", () => {
    // Arrange — the lease belongs to a daemon this client can no longer hear
    // from, so the banner must not outlive the connection that claimed it.
    const store = new ConversationStore();
    store.ingest([leaseEffect({ state: { case: "draining", value: drainingLease() } })]);
    // Act
    store.invalidateFrontendState("transport dropped");
    // Assert
    expect(store.state.shutdownSchedule).toBeNull();
  });

  it("counts a standing lease as state worth invalidating", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([leaseEffect({ state: { case: "draining", value: drainingLease() } })]);
    // Act
    const changed = store.invalidateFrontendState("transport dropped");
    // Assert
    expect(changed).toBe(true);
  });

  it("carries a queue entry's lease hold through to the render state", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([queueEffect([queueEntry({ shutdownHold: { scheduleId: "sched-1" } })])]);
    // Assert
    expect(store.state.queued[0].shutdownHold?.scheduleId).toBe("sched-1");
  });

  it("leaves an ordinary entry's lease hold absent", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([queueEffect([queueEntry()])]);
    // Assert
    expect(store.state.queued[0].shutdownHold).toBeUndefined();
  });
});

describe("hibernation adoption (the revival gate's source of truth)", () => {
  const ASLEEP = { sinceMs: 1700000000000, cause: { case: "forced" as const, value: {} } };

  it("adopts the pushed hibernation detail so the gate can name the cause", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([sessionEffect({ hibernation: ASLEEP })]);
    // Assert
    expect(store.state.hibernation).toEqual(ASLEEP);
  });

  it("clears the detail when a later view reports the session awake", () => {
    // Arrange — the revive landed, and only the daemon can say so.
    const store = new ConversationStore();
    store.ingest([sessionEffect({ hibernation: ASLEEP })]);
    // Act
    store.ingest([sessionEffect({ hibernation: null })]);
    // Assert
    expect(store.state.hibernation).toBeNull();
  });

  it("starts null, so a session with no view yet is never gated as asleep", () => {
    // Arrange / Act
    const store = new ConversationStore();
    // Assert
    expect(store.state.hibernation).toBeNull();
  });
});

describe("keep-alive hold adoption (the queue bubble's source of truth)", () => {
  it("carries a queue entry's keep-alive hold through to the rendered item", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([queueEffect([queueEntry({ keepAliveHold: { turnId: "turn-9" } })])]);
    // Assert
    expect(store.state.queued[0].keepAliveHold).toEqual({ turnId: "turn-9" });
  });

  it("leaves the hold absent on an ordinary classifier-held entry", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([queueEffect([queueEntry()])]);
    // Assert
    expect(store.state.queued[0].keepAliveHold).toBeUndefined();
  });
});

describe("revival hold adoption (the queue bubble's source of truth)", () => {
  it("carries a queue entry's revival hold through to the rendered item", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([queueEffect([queueEntry({ revivalHold: {} })])]);
    // Assert — the hold is a bare marker after the figma-idl reshape: its
    // PRESENCE is the whole claim, which is what selects the revival bubble.
    expect(store.state.queued[0].revivalHold).toEqual({});
  });

  it("leaves the hold absent on an ordinary classifier-held entry", () => {
    // Arrange
    const store = new ConversationStore();
    // Act
    store.ingest([queueEffect([queueEntry()])]);
    // Assert
    expect(store.state.queued[0].revivalHold).toBeUndefined();
  });
});

// --- the async-bubble ingest seam -------------------------------------------

describe("async bubble ingestion", () => {
  const LIVE = { case: "live", value: { lastActivityMs: 0 } } as const;

  function agentBubble(id: string, over: Partial<AsyncBubble> = {}): AsyncBubble {
    return {
      id,
      originToolUseId: "",
      parentBubbleId: "",
      label: "",
      startedAtMs: 0,
      liveness: LIVE,
      kind: { case: "agent", value: { emissions: [], fold: { droppedBefore: 0, tailCap: 0 } } },
      ...over,
    };
  }

  function delta(opened: AsyncBubble[], updates: AsyncBubbleUpdate[] = []): AsyncBubbleDelta {
    return { workspace: "/w", opened, updates, throughSeq: 1, fence: "f1" };
  }

  it("opens a pushed bubble into the store's registry", () => {
    // Arrange
    const store = new ConversationStore();

    // Act
    store.ingest([{ kind: "async-bubble-delta", value: delta([agentBubble("b1")]) }]);

    // Assert
    expect(store.asyncBubbles.get("b1")?.id).toBe("b1");
  });

  it("opens a FEED-ANCHORED bubble through the very same seam", () => {
    // Arrange — the anchored arm carries a push whose `opened` list is the
    // bubbles the conversation delta anchored.
    const store = new ConversationStore();

    // Act
    store.ingest([{ kind: "async-bubble-anchored", value: delta([agentBubble("b1")]) }]);

    // Assert
    expect(store.asyncBubbles.get("b1")?.id).toBe("b1");
  });

  it("reports a gap when a push names a bubble that is not open", () => {
    // Arrange
    const store = new ConversationStore();

    // Act
    const result = store.ingest([
      { kind: "async-bubble-delta", value: delta([], [{ bubbleId: "ghost", update: { case: "liveness", value: LIVE } }]) },
    ]);

    // Assert
    expect(result.asyncGap?.kind).toBe("unknown-bubble");
  });

  it("reports no gap for a push that lands cleanly", () => {
    // Arrange
    const store = new ConversationStore();

    // Act
    const result = store.ingest([{ kind: "async-bubble-delta", value: delta([agentBubble("b1")]) }]);

    // Assert
    expect("asyncGap" in result).toBe(false);
  });

  it("REPLACES the registry from a snapshot rather than merging into it", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([{ kind: "async-bubble-delta", value: delta([agentBubble("b1")]) }]);

    // Act
    store.ingest([{ kind: "async-bubbles-snapshot", bubbles: [agentBubble("b2")] }]);

    // Assert
    expect(store.asyncBubbles.all().map((b) => b.id)).toEqual(["b2"]);
  });

  it("retires every bubble on an empty snapshot, which is a real daemon statement", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([{ kind: "async-bubble-delta", value: delta([agentBubble("b1")]) }]);

    // Act
    store.ingest([{ kind: "async-bubbles-snapshot", bubbles: [] }]);

    // Assert
    expect(store.asyncBubbles.size).toBe(0);
  });

  it("empties the registry on reset", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([{ kind: "async-bubble-delta", value: delta([agentBubble("b1")]) }]);

    // Act
    store.reset();

    // Assert
    expect(store.asyncBubbles.size).toBe(0);
  });

  it("KEEPS bubbles across a seq-space rebase, which retires a conversation, not a process", () => {
    // Arrange
    const store = new ConversationStore();
    store.ingest([{ kind: "async-bubble-delta", value: delta([agentBubble("b1")]) }]);

    // Act
    store.rebaseSeqSpace();

    // Assert
    expect(store.asyncBubbles.get("b1")?.id).toBe("b1");
  });
});
