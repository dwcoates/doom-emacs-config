/**
 * The turn verbs.
 *
 * WHAT THIS GUARDS: the two invariants a consumer cannot see being broken. One
 * turn is in flight STRUCTURALLY — a second StartTurn is refused, never queued,
 * because a queue nobody can see makes the daemon's model of what is pending
 * silently untrue. And the prompt row is DURABLE before the prompt is submitted
 * (R15) — a crash between the two would leave a turn's activity hanging under a
 * prompt that was never recorded.
 */
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import { describe, expect, it } from "vitest";
import { conversationv1, shimv1 } from "../../src/proto.js";
import { PersistenceError } from "../../src/store/persistence.js";
import { ForegroundUnitTable } from "../../src/engine/foreground.js";
import { PermissionGate } from "../../src/engine/permission-gate.js";
import { LiveWorkTable } from "../../src/engine/detached.js";
import { SessionIdentity, createAgentIdentityStore } from "../../src/engine/identity.js";
import {
  buildPrompt,
  promptEntry,
  saidText,
  textSaid,
  TurnEngine,
  type OpenTurn,
  type SessionContext,
} from "../../src/engine/turn.js";
import { RecordingPersistence, ScriptedQuery } from "./fakes.js";
import { mkdtempSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import type { SdkTaskStartedMessage } from "../../src/sdk/types.js";

const TURN = create(conversationv1.TurnIdSchema, { value: "turn-1" });

interface Harness {
  readonly turns: TurnEngine;
  readonly persistence: RecordingPersistence;
  readonly query: ScriptedQuery;
  readonly live: LiveWorkTable;
  readonly gate: PermissionGate;
  readonly foreground: ForegroundUnitTable;
  readonly submitted: { said: conversationv1.UserSaid; keepalive: boolean }[];
  open: OpenTurn | undefined;
  identity: SessionIdentity | undefined;
  submitRejects: Error | undefined;
  /** Every store_unreachable the turn verbs reported to the session. */
  readonly storeFaults: string[];
  /** What this session's `knowsAgent` answers. */
  knows: boolean;
  /** When set, `SessionContext.query()` answers absence, as a dead query does. */
  queryDead: boolean;
}

async function harness(persistence: RecordingPersistence = new RecordingPersistence()): Promise<Harness> {
  const foreground = new ForegroundUnitTable();
  const query = new ScriptedQuery();
  const live = new LiveWorkTable();
  const identity = await SessionIdentity.fresh(
    createAgentIdentityStore(mkdtempSync(path.join(os.tmpdir(), "shim-turn-")), "ws000000"),
    () => "agent-1",
  );
  const gate = new PermissionGate({
    mainAgentId: () => identity.agentId,
    agentFor: () => undefined,
    persist: (entries) => persistence.write(entries),
    keepalive: () => false,
    nowMs: () => 1,
    onPermissionModeSet: () => undefined,
  });
  const state: Harness = {
    turns: undefined as unknown as TurnEngine,
    persistence,
    query,
    live,
    gate,
    foreground,
    submitted: [],
    open: undefined,
    identity,
    submitRejects: undefined,
    storeFaults: [] as string[],
    knows: true,
    queryDead: false,
  };
  const context: SessionContext = {
    persistence,
    foreground,
    gate,
    live,
    identity: () => state.identity,
    query: () => (state.queryDead ? undefined : query),
    nowMs: () => 1,
    openTurn: () => state.open,
    watcherOpened: () => () => undefined,
    bashWatcherOpened: () => () => undefined,
    concludeStoppedRuns: () => undefined,
    knowsAgent: () => state.knows,
    reportStoreUnreachable: (detail: string) => state.storeFaults.push(detail),
    submit: (said, keepalive) => {
      if (state.submitRejects !== undefined) return Promise.reject(state.submitRejects);
      state.submitted.push({ said, keepalive });
      return Promise.resolve();
    },
    setOpenTurn: (turn) => {
      state.open = turn;
    },
  };
  return Object.assign(state, { turns: new TurnEngine(context) });
}

function startTurn(): shimv1.StartTurnRequest {
  return create(shimv1.StartTurnRequestSchema, {
    turn: TURN,
    said: textSaid("do the thing"),
    origin: conversationv1.PromptOrigin.USER_SENT,
    pageSize: 20,
  });
}

function failureKind(response: { result: { case?: string; value?: unknown } }): string | undefined {
  const failure = response.result.value as { kind?: { case?: string }; cause?: { case?: string } };
  return failure?.kind?.case ?? failure?.cause?.case;
}

describe("what the user said", () => {
  it("is the text of the text blocks", () => {
    expect(saidText(textSaid("hello"))).toBe("hello");
  });

  it("carries an image by its path rather than re-encoding its bytes", () => {
    const said = create(conversationv1.UserSaidSchema, {
      content: create(conversationv1.UserContentSchema, {
        blocks: [
          create(conversationv1.UserContentBlockSchema, {
            block: {
              case: "image",
              value: create(conversationv1.ImageBlockSchema, {
                mediaType: "image/png",
                location: { case: "path", value: create(conversationv1.ImageBlockPathSchema, { path: "/tmp/a.png" }) },
              }),
            },
          }),
        ],
      }),
    });

    expect(saidText(said)).toBe("/tmp/a.png");
  });

  it("RAISES on an UnsupportedBlock rather than silently dropping what the user sent", () => {
    const said = create(conversationv1.UserSaidSchema, {
      content: create(conversationv1.UserContentSchema, {
        blocks: [
          create(conversationv1.UserContentBlockSchema, {
            block: {
              case: "unsupported",
              value: create(conversationv1.UnsupportedBlockSchema, { kind: "video" }),
            },
          }),
        ],
      }),
    });

    expect(() => saidText(said)).toThrow(/UnsupportedBlock is not a fallback/);
  });
});

describe("the prompt row", () => {
  it("is keyed by the turn", () => {
    const agent = create(conversationv1.AgentIdSchema, { value: "agent-1" });
    const prompt = buildPrompt(TURN, agent, textSaid("x"), conversationv1.PromptOrigin.USER_SENT);

    expect(promptEntry(prompt, agent, false).upsertKey).toBe("prompt:turn-1");
  });

  it("is flagged keep-alive for the shim's own turns", () => {
    const agent = create(conversationv1.AgentIdSchema, { value: "agent-1" });
    const prompt = buildPrompt(TURN, agent, textSaid("x"), conversationv1.PromptOrigin.UNSPECIFIED);

    expect(promptEntry(prompt, agent, true).keepalive).toBe(true);
  });
});

describe("StartTurn", () => {
  it("accepts the prompt and returns it as delivered", async () => {
    const h = await harness();

    const response = await h.turns.startTurn(startTurn());

    expect(response.result.case).toBe("success");
  });

  it("adopts the daemon's TurnId rather than minting one", async () => {
    const h = await harness();

    const response = await h.turns.startTurn(startTurn());

    expect(
      response.result.case === "success" ? response.result.value.prompt?.id?.value : undefined,
    ).toBe("turn-1");
  });

  it("addresses the prompt to the main agent — the WatchAgent address", async () => {
    const h = await harness();

    const response = await h.turns.startTurn(startTurn());

    expect(
      response.result.case === "success" ? response.result.value.prompt?.agent?.value : undefined,
    ).toBe("agent-1");
  });

  it("writes the prompt row DURABLY (R15)", async () => {
    const h = await harness();

    await h.turns.startTurn(startTurn());

    expect(h.persistence.durable.map((entry) => entry.upsertKey)).toEqual(["prompt:turn-1"]);
  });

  it("has that ack BEFORE it submits", async () => {
    // The order is the whole of R15: a crash between the two must never leave a
    // turn's activity hanging under a prompt nothing recorded.
    const h = await harness();
    const order: string[] = [];
    const durable = h.persistence.writeDurable.bind(h.persistence);
    h.persistence.writeDurable = async (entries) => {
      await durable(entries);
      order.push("durable");
    };
    const submitted = h.submitted;
    Object.defineProperty(submitted, "push", {
      value: (...items: { said: conversationv1.UserSaid; keepalive: boolean }[]) => {
        order.push("submit");
        return Array.prototype.push.apply(submitted, items);
      },
    });

    await h.turns.startTurn(startTurn());

    expect(order).toEqual(["durable", "submit"]);
  });

  it("re-queues the prompt row behind the retry buffer when the store cannot take it", async () => {
    // A STORE OUTAGE IS NOT A REFUSED TURN: the vendor can still do the work,
    // and letting the PersistenceError escape answered Code.Internal, which
    // the daemon cannot tell from a shim defect.
    const h = await harness();
    h.persistence.writeDurable = () =>
      Promise.reject(new PersistenceError("store_unavailable", "the store is down"));

    const response = await h.turns.startTurn(startTurn());

    expect(response.result.case).toBe("success");
    expect(h.persistence.buffered.map((entry) => entry.upsertKey)).toContain("prompt:turn-1");
  });

  it("does NOT swallow a non-persistence failure of the prompt write", async () => {
    const h = await harness();
    h.persistence.writeDurable = () => Promise.reject(new Error("a defect, not an outage"));

    await expect(h.turns.startTurn(startTurn())).rejects.toThrow("a defect, not an outage");
  });

  it("REFUSES a second StartTurn while one is open", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());

    const second = await h.turns.startTurn(startTurn());

    expect(failureKind(second)).toBe("turnAlreadyOpen");
  });

  it("does not queue the refused prompt", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());

    await h.turns.startTurn(startTurn());

    expect(h.submitted).toHaveLength(1);
  });

  it("refuses when no session has been started", async () => {
    const h = await harness();
    h.identity = undefined;

    expect(failureKind(await h.turns.startTurn(startTurn()))).toBe("noSession");
  });

  it("refuses with the vendor's wording when the submission fails", async () => {
    const h = await harness();
    h.submitRejects = new Error("the binary said no");

    expect(failureKind(await h.turns.startTurn(startTurn()))).toBe("vendorRefused");
  });

  it("does not leave the turn open after a refused submission", async () => {
    const h = await harness();
    h.submitRejects = new Error("no");

    await h.turns.startTurn(startTurn());

    expect(h.open).toBeUndefined();
  });
});

describe("UpdateAgent.stop", () => {
  const stop = (target?: conversationv1.AgentId): shimv1.UpdateAgentRequest =>
    create(shimv1.UpdateAgentRequestSchema, {
      ...(target === undefined ? {} : { target }),
      input: create(conversationv1.AgentInputSchema, {
        input: { case: "stop", value: create(conversationv1.AgentStopSchema, {}) },
      }),
    });

  it("interrupts the main agent", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());

    await h.turns.updateAgent(stop());

    expect(h.query.calls).toContain("interrupt");
  });

  it("resolves pending callbacks BEFORE the interrupt", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());
    const pending = h.gate.canUseTool("Bash", {}, {
      signal: new AbortController().signal,
      toolUseID: "toolu_1",
      requestId: "r",
    });
    await Promise.resolve();

    await h.turns.updateAgent(stop());

    expect(await pending).toEqual({ behavior: "deny", message: "the main agent was stopped" });
  });

  it("refuses when the main agent has no turn in flight", async () => {
    const h = await harness();

    expect(failureKind(await h.turns.updateAgent(stop()))).toBe("nothingRunning");
  });

  it("stops a subagent by its task id", async () => {
    const h = await harness();
    h.live.onTaskStarted({
      type: "system",
      subtype: "task_started",
      task_id: "a01",
      tool_use_id: "toolu_1",
      description: "",
      uuid: "00000000-0000-4000-8000-000000000000",
      session_id: "s",
    });

    await h.turns.updateAgent(stop(create(conversationv1.AgentIdSchema, { value: "a01" })));

    expect(h.query.stoppedTasks).toEqual(["a01"]);
  });

  it("refuses an unknown agent", async () => {
    const h = await harness();

    expect(
      failureKind(await h.turns.updateAgent(stop(create(conversationv1.AgentIdSchema, { value: "nope" })))),
    ).toBe("unknownAgent");
  });
});

/**
 * The gate holds ONE book for the whole session, so an ask raised under a
 * subagent survives that subagent being stopped. Answering it would settle a
 * promise for an agent that is gone and report `delivered` for a delivery that
 * reached nobody, so the addressee is resolved BEFORE the gate is asked.
 *
 * Each case leaves the gate EMPTY on purpose: an empty gate answers
 * `noOpenAsk`, so `unknownAgent` can only come from the addressee check, and
 * `noOpenAsk` proves the check let the answer through to the gate.
 */
describe("UpdateAgent.answer addressee", () => {
  const answer = (target?: conversationv1.AgentId): shimv1.UpdateAgentRequest =>
    create(shimv1.UpdateAgentRequestSchema, {
      ...(target === undefined ? {} : { target }),
      input: create(conversationv1.AgentInputSchema, {
        input: {
          case: "answer",
          value: create(conversationv1.AgentAnswerSchema, {
            answer: {
              case: "permissionDecision",
              value: create(conversationv1.AgentPermissionDecisionSchema, {
                ask: create(conversationv1.AgentPermissionIdSchema, { value: "ask-1" }),
                decision: {
                  case: "denied",
                  value: create(conversationv1.AgentPermissionDeniedByUserSchema, {}),
                },
              }),
            },
          }),
        },
      }),
    });

  const started = (taskId: string, toolUseId: string): SdkTaskStartedMessage =>
    ({
      type: "system",
      subtype: "task_started",
      task_id: taskId,
      tool_use_id: toolUseId,
      description: "",
      uuid: "00000000-0000-4000-8000-000000000000",
      session_id: "s",
    });

  const cases: {
    readonly name: string;
    readonly live: readonly SdkTaskStartedMessage[];
    readonly target: string | undefined;
    readonly want: string | undefined;
  }[] = [
    {
      name: "reaches the gate for the main agent, which names no subagent at all",
      live: [],
      target: undefined,
      want: "noOpenAsk",
    },
    {
      name: "reaches the gate for a subagent addressed by its task id",
      live: [started("a01", "toolu_1")],
      target: "a01",
      want: "noOpenAsk",
    },
    {
      name: "reaches the gate for a subagent addressed by its spawning tool_use_id",
      live: [started("a01", "toolu_1")],
      target: "toolu_1",
      want: "noOpenAsk",
    },
    {
      name: "REFUSES unknown_agent for an ask whose subagent has been stopped",
      live: [],
      target: "a01",
      want: "unknownAgent",
    },
  ];

  for (const testCase of cases) {
    it(testCase.name, async () => {
      // Arrange.
      const h = await harness();
      for (const message of testCase.live) h.live.onTaskStarted(message);

      // Act.
      const response = await h.turns.updateAgent(
        answer(
          testCase.target === undefined
            ? undefined
            : create(conversationv1.AgentIdSchema, { value: testCase.target }),
        ),
      );

      // Assert.
      expect(failureKind(response)).toBe(testCase.want);
    });
  }
});

describe("UpdateAgent.prompt", () => {
  const prompt = (target: conversationv1.AgentId): shimv1.UpdateAgentRequest =>
    create(shimv1.UpdateAgentRequestSchema, {
      target,
      input: create(conversationv1.AgentInputSchema, {
        input: { case: "prompt", value: textSaid("more") },
      }),
    });

  it("refuses to prompt the session's own turn — StartTurn is that verb", async () => {
    const h = await harness();

    expect(
      failureKind(await h.turns.updateAgent(prompt(create(conversationv1.AgentIdSchema, { value: "agent-1" })))),
    ).toBe("unknownAgent");
  });

  it("REFUSES to prompt a subagent, stating that no declared route delivers one", async () => {
    // Guessing an address would inject the user's words into the main thread.
    const response = await (await harness()).turns.updateAgent(
      prompt(create(conversationv1.AgentIdSchema, { value: "a01" })),
    );

    expect(
      response.result.case === "failure" ? response.result.value.detail : undefined,
    ).toMatch(/declares no route/);
  });
});

describe("KillTurn", () => {
  const kill = (force: boolean, turn = TURN): shimv1.KillTurnRequest =>
    create(shimv1.KillTurnRequestSchema, { turn, force });

  it("refuses when no turn is open", async () => {
    const h = await harness();

    expect(failureKind(await h.turns.killTurn(kill(false)))).toBe("noTurnOpen");
  });

  it("refuses `live` when the turn has CLOSED but its work is still running", async () => {
    // Detached work outlives the turn that spawned it by design, so answering
    // "no turn is open" would leave the daemon with a running shell it has no
    // verb to stop under the turn it belongs to.
    const h = await harness();
    h.live.onTaskStarted(
      { type: "system", subtype: "task_started", task_id: "b01", tool_use_id: "t", description: "", uuid: "00000000-0000-4000-8000-000000000000", session_id: "s" },
      "turn-1",
    );

    expect(failureKind(await h.turns.killTurn(kill(false)))).toBe("live");
  });

  it("forced, ends the work a CLOSED turn left running", async () => {
    const h = await harness();
    h.live.onTaskStarted(
      { type: "system", subtype: "task_started", task_id: "b01", tool_use_id: "t", description: "", uuid: "00000000-0000-4000-8000-000000000000", session_id: "s" },
      "turn-1",
    );

    const response = await h.turns.killTurn(kill(true));

    expect(response.result.case === "success" ? response.result.value.killed?.how.case : undefined).toBe(
      "forced",
    );
  });

  it("still refuses noTurnOpen when a closed turn left nothing running", async () => {
    const h = await harness();

    expect(failureKind(await h.turns.killTurn(kill(false)))).toBe("noTurnOpen");
  });

  it("refuses a turn that is not the open one", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());

    const response = await h.turns.killTurn(
      kill(false, create(conversationv1.TurnIdSchema, { value: "turn-2" })),
    );

    expect(failureKind(response)).toBe("notTheOpenTurn");
  });

  it("kills an idle turn as agent_only", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());

    const response = await h.turns.killTurn(kill(false));

    expect(response.result.case === "success" ? response.result.value.killed?.how.case : undefined).toBe(
      "agentOnly",
    );
  });

  it("REFUSES while the turn has live work and force was not set", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());
    h.live.onTaskStarted(
      { type: "system", subtype: "task_started", task_id: "b01", tool_use_id: "t", description: "", uuid: "00000000-0000-4000-8000-000000000000", session_id: "s" },
      "turn-1",
    );

    expect(failureKind(await h.turns.killTurn(kill(false)))).toBe("live");
  });

  it("NAMES the live work in the refusal, by its spawning call", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());
    h.live.onTaskStarted(
      { type: "system", subtype: "task_started", task_id: "b01", tool_use_id: "t", description: "", uuid: "00000000-0000-4000-8000-000000000000", session_id: "s" },
      "turn-1",
    );

    const response = await h.turns.killTurn(kill(false));
    const failure = response.result.case === "failure" ? response.result.value : undefined;
    expect(
      failure?.cause.case === "live" ? failure.cause.value.liveWork.map((id) => id.value) : undefined,
    ).toEqual(["t"]);
  });

  it("stops the whole transitive set when forced", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());
    h.live.onTaskStarted(
      { type: "system", subtype: "task_started", task_id: "b01", tool_use_id: "t", description: "", uuid: "00000000-0000-4000-8000-000000000000", session_id: "s" },
      "turn-1",
    );

    await h.turns.killTurn(kill(true));

    expect(h.query.stoppedTasks).toEqual(["b01"]);
  });

  it("does not reach work another turn spawned", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());
    h.live.onTaskStarted(
      { type: "system", subtype: "task_started", task_id: "b99", tool_use_id: "t", description: "", uuid: "00000000-0000-4000-8000-000000000000", session_id: "s" },
      "another-turn",
    );

    await h.turns.killTurn(kill(true));

    expect(h.query.stoppedTasks).toEqual([]);
  });
});

describe("DetachForeground", () => {
  const detach = (unit: string): shimv1.DetachForegroundRequest =>
    create(shimv1.DetachForegroundRequestSchema, {
      unit: create(conversationv1.AgentActivityIdSchema, { value: unit }),
    });

  it("refuses a unit nothing knows about", async () => {
    const h = await harness();

    expect(failureKind(await h.turns.detachForeground(detach("toolu_x")))).toBe("unknownUnit");
  });

  it("refuses a known unit with no live background work", async () => {
    const h = await harness();
    h.live.onTaskStarted({
      type: "system",
      subtype: "task_started",
      task_id: "b01",
      tool_use_id: "toolu_1",
      description: "",
      uuid: "00000000-0000-4000-8000-000000000000",
      session_id: "s",
    });

    expect(failureKind(await h.turns.detachForeground(detach("toolu_1")))).toBe("alreadyConcluded");
  });

  it("reports a unit the vendor is already running in the background as detached", async () => {
    const h = await harness();
    h.query.backgroundTaskAnswer = true;

    expect((await h.turns.detachForeground(detach("toolu_1"))).result.case).toBe("success");
  });
});

describe("ReadHistory", () => {
  const first = (): shimv1.ReadHistoryRequest =>
    create(shimv1.ReadHistoryRequestSchema, {
      pageSize: 10,
      position: { case: "first", value: create(shimv1.ReadHistoryFirstSchema, {}) },
    });

  it("serves the first page from the store", async () => {
    const h = await harness();

    expect((await h.turns.readHistory(first())).result.case).toBe("success");
  });

  it("closes the reading session it opened, because ReadHistory has no tail", async () => {
    const h = await harness();

    await h.turns.readHistory(first());

    expect(h.persistence.closedPages).toBe(1);
  });

  it("maps an unknown agent onto the ReadHistory arm for it", async () => {
    const h = await harness();
    h.persistence.openError = new PersistenceError("unknown_agent", "no such book");

    expect(failureKind(await h.turns.readHistory(first()))).toBe("unknownAgent");
  });

  it("maps a stale pointer onto its own arm", async () => {
    const h = await harness();
    h.persistence.readError = new PersistenceError("stale_pointer", "that pointer is gone");

    const response = await h.turns.readHistory(
      create(shimv1.ReadHistoryRequestSchema, {
        pageSize: 10,
        position: {
          case: "after",
          value: create(conversationv1.HistoryPointerSchema, { value: "p-1" }),
        },
      }),
    );

    expect(failureKind(response)).toBe("stalePointer");
  });

  it("maps an unreachable store onto its own arm", async () => {
    const h = await harness();
    h.persistence.openError = new PersistenceError("store_unavailable", "the store is down");

    expect(failureKind(await h.turns.readHistory(first()))).toBe("storeUnavailable");
  });

  it("REPORTS an unreachable store as a session fault, not only to its caller", async () => {
    // A refusal tells the caller its own call failed; every other consumer --
    // the daemon deciding whether to trust what it is painting -- learns
    // nothing from that.
    const h = await harness();
    h.persistence.openError = new PersistenceError("store_unavailable", "the store is down");

    await h.turns.readHistory(first());

    expect(h.storeFaults).toHaveLength(1);
  });

  it("does NOT report a session fault for an unknown agent", async () => {
    // A caller error is not the record plane being unreachable.
    const h = await harness();
    h.persistence.openError = new PersistenceError("unknown_agent", "no such agent");

    await h.turns.readHistory(first());

    expect(h.storeFaults).toEqual([]);
  });
});

describe("WatchAgent", () => {
  it("opens with the page", async () => {
    const h = await harness();
    const frames: string[] = [];

    for await (const response of h.turns.watchAgent(
      create(shimv1.WatchAgentRequestSchema, { pageSize: 10 }),
    )) {
      frames.push(response.frame.case ?? "");
    }

    expect(frames[0]).toBe("page");
  });

  it("then tails one entry per store write", async () => {
    const h = await harness();
    h.persistence.tail = [
      create(conversationv1.HistoryEntryAtSchema, {
        at: create(conversationv1.HistoryPointerSchema, { value: "p-1" }),
      }),
    ];
    const frames: string[] = [];

    for await (const response of h.turns.watchAgent(
      create(shimv1.WatchAgentRequestSchema, { pageSize: 10 }),
    )) {
      frames.push(response.frame.case ?? "");
    }

    expect(frames).toEqual(["page", "entry"]);
  });

  it("closes the stream at the transport when the target names no agent", async () => {
    const h = await harness();
    h.persistence.openError = new PersistenceError("unknown_agent", "no such book");

    await expect(
      (async () => {
        for await (const _ of h.turns.watchAgent(
          create(shimv1.WatchAgentRequestSchema, { pageSize: 10 }),
        )) {
          // the open is refused before anything is yielded
        }
      })(),
    ).rejects.toThrow(/no such book/);
  });

  it("hands the record plane the producer's own verdict on the target", async () => {
    // The store refuses a book it holds no row for, and the main agent's row is
    // created by its FIRST WRITE — so the record plane needs this verdict to
    // tell a fresh agent apart from an id nobody ever minted.
    // Arrange.
    const h = await harness();
    h.knows = false;

    // Act. The empty book is refused after the open, which is beside the point
    // here: what is asserted is the verdict the open was given.
    await expect(
      (async () => {
        for await (const _ of h.turns.watchAgent(
          create(shimv1.WatchAgentRequestSchema, { pageSize: 10 }),
        )) {
          break;
        }
      })(),
    ).rejects.toThrow();

    // Assert.
    expect(h.persistence.lastKnownAgent?.()).toBe(false);
  });

  it("closes the store's own unknown_agent refusal with Code.NotFound", async () => {
    // Landing 7: the store refuses a well-formed id naming no book; the shim
    // relays that as NotFound rather than as an internal error.
    const h = await harness();
    h.persistence.openError = new PersistenceError("unknown_agent", "no book under that id");

    const error = await (async () => {
      try {
        for await (const _ of h.turns.watchAgent(
          create(shimv1.WatchAgentRequestSchema, { pageSize: 10 }),
        )) {
          // the open is refused before anything is yielded
        }
        return undefined;
      } catch (caught) {
        return caught;
      }
    })();

    expect(error instanceof ConnectError ? error.code : undefined).toBe(Code.NotFound);
  });
});

describe("StopBash", () => {
  const stop = (work: string): shimv1.StopBashRequest =>
    create(shimv1.StopBashRequestSchema, {
      work: create(conversationv1.DetachedWorkIdSchema, { value: work }),
    });

  it("resolves the HANDLE to the vendor's task id, which is what stopTask wants", async () => {
    const h = await harness();
    h.live.onTaskStarted({
      type: "system",
      subtype: "task_started",
      task_id: "b01",
      tool_use_id: "t",
      description: "",
      uuid: "00000000-0000-4000-8000-000000000000",
      session_id: "s",
    });

    // The handle names the SPAWNING CALL (ruling, landing 3); the task id stays
    // shim-side as the internal address.
    await h.turns.stopBash(stop("t"));

    expect(h.query.stoppedTasks).toEqual(["b01"]);
  });

  it("refuses work nothing is running", async () => {
    const h = await harness();

    expect(failureKind(await h.turns.stopBash(stop("b99")))).toBe("unknownWork");
  });
});

describe("WatchBash", () => {
  it("relays the run's frames", async () => {
    const h = await harness();
    h.persistence.bashFrames = [create(conversationv1.AgentBashSchema, {})];
    const frames: conversationv1.AgentBash[] = [];

    for await (const response of h.turns.watchBash(
      create(shimv1.WatchBashRequestSchema, {
        work: create(conversationv1.DetachedWorkIdSchema, { value: "b01" }),
      }),
    )) {
      if (response.bash !== undefined) frames.push(response.bash);
    }

    expect(frames).toHaveLength(1);
  });
});

// ---------------------------------------------------------------------------
// Landing 3: the opening page, and the two arms that name the SDK's gaps
// ---------------------------------------------------------------------------

describe("the opening page StartTurn paints", () => {
  it("rides the answer, so one call submits AND paints", async () => {
    const h = await harness();
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries: [
        create(conversationv1.HistoryEntryAtSchema, {
          at: create(conversationv1.HistoryPointerSchema, { value: "1" }),
          entry: create(conversationv1.HistoryEntrySchema, {
            entry: { case: "userPrompt", value: create(conversationv1.AgentPromptSchema, {}) },
          }),
        }),
      ],
      boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
    });

    const response = await h.turns.startTurn(startTurn());

    const success = response.result.value as shimv1.StartTurnSuccess;
    expect(success.page?.entries).toHaveLength(1);
  });

  it("is read AFTER the prompt is durable, so the first paint holds the new turn", async () => {
    const h = await harness();

    await h.turns.startTurn(startTurn());

    // The durable prompt row lands before the page is opened; a page read first
    // would paint a feed missing the very turn the caller just opened.
    expect(h.persistence.durable).toHaveLength(1);
    expect(h.persistence.closedPages).toBe(1);
  });

  it("closes the page session at once: StartTurn paints once and never tails", async () => {
    const h = await harness();

    await h.turns.startTurn(startTurn());

    expect(h.persistence.closedPages).toBe(1);
  });

  it("answers an EMPTY page with a floor when the store cannot be read, never a failure", async () => {
    const h = await harness();
    h.persistence.openError = new PersistenceError("store_unavailable", "the store is down");

    const response = await h.turns.startTurn(startTurn());

    // The prompt is already durable and already delivered, so a failure here
    // would tell the daemon a turn did not start that is running.
    expect(response.result.case).toBe("success");
    const success = response.result.value as shimv1.StartTurnSuccess;
    expect(success.page?.entries).toHaveLength(0);
    expect(success.page?.boundary.case).toBe("floor");
  });

  it("never answers an ABSENT page, which a consumer cannot tell from an empty one", async () => {
    const h = await harness();
    h.persistence.openError = new PersistenceError("store_unavailable", "the store is down");

    const response = await h.turns.startTurn(startTurn());

    const success = response.result.value as shimv1.StartTurnSuccess;
    expect(success.page).toBeDefined();
  });
});

describe("UpdateAgent.prompt to a subagent", () => {
  it("refuses not_deliverable: the gap is the SDK's, not the agent's state", async () => {
    const h = await harness();

    const response = await h.turns.updateAgent(
      create(shimv1.UpdateAgentRequestSchema, {
        target: create(conversationv1.AgentIdSchema, { value: "agent-7" }),
        input: create(conversationv1.AgentInputSchema, {
          input: { case: "prompt", value: textSaid("carry on") },
        }),
      }),
    );

    expect(failureKind(response)).toBe("notDeliverable");
  });

  it("never says nothing_running, which would send a caller hunting a live agent", async () => {
    const h = await harness();

    const response = await h.turns.updateAgent(
      create(shimv1.UpdateAgentRequestSchema, {
        target: create(conversationv1.AgentIdSchema, { value: "agent-7" }),
        input: create(conversationv1.AgentInputSchema, {
          input: { case: "prompt", value: textSaid("carry on") },
        }),
      }),
    );

    expect(failureKind(response)).not.toBe("nothingRunning");
  });

  /** A running task under `taskId`, of the named kind. */
  function running(h: Harness, taskId: string, taskType?: string): void {
    h.live.onTaskStarted(
      {
        type: "system",
        subtype: "task_started",
        task_id: taskId,
        tool_use_id: `toolu_${taskId}`,
        description: "a subagent",
        uuid: "00000000-0000-4000-8000-000000000000",
        session_id: "s",
        ...(taskType === undefined ? {} : { task_type: taskType }),
      },
      "turn-1",
    );
  }

  const promptTo = (target: string): shimv1.UpdateAgentRequest =>
    create(shimv1.UpdateAgentRequestSchema, {
      target: create(conversationv1.AgentIdSchema, { value: target }),
      input: create(conversationv1.AgentInputSchema, {
        input: { case: "prompt", value: textSaid("carry on") },
      }),
    });

  it("refuses agent_busy when the addressed subagent's own turn is running", async () => {
    // Landing 7: the daemon relays this as bubble_refused{agent_busy}.
    const h = await harness();
    running(h, "agent-7", "local_agent");

    const response = await h.turns.updateAgent(promptTo("agent-7"));

    expect(failureKind(response)).toBe("agentBusy");
  });

  it("refuses agent_busy when the target is the SPAWNING CALL's handle", async () => {
    // A subagent is addressed by its tool_use_id on the wire, never by the
    // vendor's task id.
    const h = await harness();
    running(h, "agent-7", "local_agent");

    const response = await h.turns.updateAgent(promptTo("toolu_agent-7"));

    expect(failureKind(response)).toBe("agentBusy");
  });

  it("refuses agent_busy for a running task whose kind the vendor left unstated", async () => {
    // An unset `task_type` is the agent kind, as everywhere else in the shim.
    const h = await harness();
    running(h, "agent-7");

    const response = await h.turns.updateAgent(promptTo("agent-7"));

    expect(failureKind(response)).toBe("agentBusy");
  });

  it("keeps not_deliverable for a running SHELL under the same handle", async () => {
    // A `local_bash` run is not an agent, so its liveness says nothing about a
    // subagent's turn; the SDK route gap is still the refusal.
    const h = await harness();
    running(h, "agent-7", "local_bash");

    const response = await h.turns.updateAgent(promptTo("agent-7"));

    expect(failureKind(response)).toBe("notDeliverable");
  });
});

describe("DetachForeground on a live foreground unit", () => {
  /** A live task the vendor has NOT backgrounded. */
  function liveForeground(h: Harness): void {
    h.live.onTaskStarted(
      {
        type: "system",
        subtype: "task_started",
        task_id: "b01",
        tool_use_id: "toolu_f",
        description: "sleep 100",
        uuid: "00000000-0000-4000-8000-000000000000",
        session_id: "s",
      },
      "turn-1",
    );
  }

  it("refuses unsupported: the pinned SDK offers no verb to INITIATE a detachment", async () => {
    // The unit is detachable in kind and STILL IN THE FOREGROUND -- the vendor
    // holds no background work for it, so there is nothing to confirm and no
    // verb to start one with.
    const h = await harness();
    await h.turns.startTurn(startTurn());
    h.foreground.note("toolu_f", "bash", false);
    h.query.backgroundTaskAnswer = false;

    const response = await h.turns.detachForeground(
      create(shimv1.DetachForegroundRequestSchema, {
        unit: create(conversationv1.AgentActivityIdSchema, { value: "toolu_f" }),
      }),
    );

    expect(failureKind(response)).toBe("unsupported");
  });

  it("never says not_detachable, which would deny a kind that backgrounds routinely", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());
    h.foreground.note("toolu_f", "bash", false);
    h.query.backgroundTaskAnswer = false;

    const response = await h.turns.detachForeground(
      create(shimv1.DetachForegroundRequestSchema, {
        unit: create(conversationv1.AgentActivityIdSchema, { value: "toolu_f" }),
      }),
    );

    expect(failureKind(response)).not.toBe("notDetachable");
  });

  it("CONFIRMS on backgroundTasks alone, without waiting for the live table's flag", async () => {
    // `backgroundTasks(unit) === true` IS the observation of the detachment;
    // the table's own `backgrounded` flag is a laggier restatement that arrives
    // on a later background_tasks_changed, and requiring it too refused
    // detachments the vendor had already made.
    const h = await harness();
    await h.turns.startTurn(startTurn());
    liveForeground(h);
    h.query.backgroundTaskAnswer = true;

    const response = await h.turns.detachForeground(
      create(shimv1.DetachForegroundRequestSchema, {
        unit: create(conversationv1.AgentActivityIdSchema, { value: "toolu_f" }),
      }),
    );

    expect(response.result.case).toBe("success");
  });

  it("CONFIRMS a detachment the vendor made on its own", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());
    liveForeground(h);
    h.live.onTaskUpdated({
      type: "system",
      subtype: "task_updated",
      task_id: "b01",
      patch: { is_backgrounded: true },
      uuid: "00000000-0000-4000-8000-000000000001",
      session_id: "s",
    } as never);
    h.query.backgroundTaskAnswer = true;

    const response = await h.turns.detachForeground(
      create(shimv1.DetachForegroundRequestSchema, {
        unit: create(conversationv1.AgentActivityIdSchema, { value: "toolu_f" }),
      }),
    );

    expect(response.result.case).toBe("success");
  });
});

describe("DetachForeground on a live foreground subagent", () => {
  // A subagent spawned by Task/Agent is addressed on the wire by its
  // tool_use id, exactly like a bash call -- `AgentActivity.item.case` is
  // "subagent" rather than "bash", and both are DETACHABLE_KINDS.
  const detach = (unit: string): shimv1.DetachForegroundRequest =>
    create(shimv1.DetachForegroundRequestSchema, {
      unit: create(conversationv1.AgentActivityIdSchema, { value: unit }),
    });

  it("refuses unsupported: a live foreground subagent is detachable in kind, but the pinned SDK offers no verb to INITIATE a detachment", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());
    h.foreground.note("toolu_agent", "subagent", false);
    h.query.backgroundTaskAnswer = false;

    const response = await h.turns.detachForeground(detach("toolu_agent"));

    expect(failureKind(response)).toBe("unsupported");
  });

  it("CONFIRMS a live foreground subagent the vendor already holds live background work for", async () => {
    // `backgroundTasks(unit) === true` is the observation of a detachment the
    // vendor made on its own; it applies to a subagent's tool_use id exactly
    // as it does to a bash's.
    const h = await harness();
    await h.turns.startTurn(startTurn());
    h.foreground.note("toolu_agent", "subagent", false);
    h.query.backgroundTaskAnswer = true;

    const response = await h.turns.detachForeground(detach("toolu_agent"));

    expect(response.result.case).toBe("success");
  });

  it("refuses unknown_unit for a stale subagent tool_use id the foreground table never saw", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());
    h.query.backgroundTaskAnswer = false;

    const response = await h.turns.detachForeground(detach("toolu_agent_stale"));

    expect(failureKind(response)).toBe("unknownUnit");
  });
});

/**
 * watchBash's own standing predicate -- the shim's answer to "where does this
 * run stand", used to turn a store refusal into a race worth waiting out
 * (store/reader.ts's awaitFirstRow). RecordingPersistence records the predicate
 * it was handed so it can be invoked directly.
 */
describe("WatchBash's announcement predicate", () => {
  it("says live while the live table still holds the work", async () => {
    const h = await harness();
    h.live.onTaskStarted({
      type: "system",
      subtype: "task_started",
      task_id: "b01",
      tool_use_id: "t",
      description: "",
      uuid: "00000000-0000-4000-8000-000000000000",
      session_id: "s",
    });
    h.persistence.bashFrames = [create(conversationv1.AgentBashSchema, {})];

    for await (const response of h.turns.watchBash(
      create(shimv1.WatchBashRequestSchema, {
        work: create(conversationv1.DetachedWorkIdSchema, { value: "t" }),
      }),
    )) {
      void response;
      break;
    }

    expect(h.persistence.lastAnnouncement?.()).toBe("live");
  });

  it("says concluded for a handle the live table retired", async () => {
    const h = await harness();
    h.live.onTaskStarted({
      type: "system",
      subtype: "task_started",
      task_id: "b02",
      tool_use_id: "t",
      description: "",
      uuid: "00000000-0000-4000-8000-000000000000",
      session_id: "s",
    });
    h.live.onTaskNotification({
      type: "system",
      subtype: "task_notification",
      task_id: "b02",
      status: "completed",
      output_file: "",
      summary: "",
      uuid: "00000000-0000-4000-8000-000000000001",
      session_id: "s",
    });
    h.persistence.bashFrames = [create(conversationv1.AgentBashSchema, {})];

    for await (const response of h.turns.watchBash(
      create(shimv1.WatchBashRequestSchema, {
        work: create(conversationv1.DetachedWorkIdSchema, { value: "t" }),
      }),
    )) {
      void response;
      break;
    }

    expect(h.persistence.lastAnnouncement?.()).toBe("concluded");
  });

  it("says unknown for a handle the live table never held", async () => {
    const h = await harness();
    h.persistence.bashFrames = [create(conversationv1.AgentBashSchema, {})];

    for await (const response of h.turns.watchBash(
      create(shimv1.WatchBashRequestSchema, {
        work: create(conversationv1.DetachedWorkIdSchema, { value: "nope" }),
      }),
    )) {
      void response;
      break;
    }

    expect(h.persistence.lastAnnouncement?.()).toBe("unknown");
  });
});

// ---------------------------------------------------------------------------
// The refusals and guards a healthy session never reaches: a dead query, a
// malformed request the wire validator would have caught, and the record
// plane failing in a way that is not a PersistenceError.
// ---------------------------------------------------------------------------

/** A record plane whose `openAgentPage` rejects with something that is not an Error. */
class NonErrorPageStore extends RecordingPersistence {
  override openAgentPage(): Promise<never> {
    return Promise.reject("the store threw a string");
  }
}

/** A record plane whose `openBashRun` rejects with a failure the suite chooses. */
class FailingBashStore extends RecordingPersistence {
  constructor(private readonly failure: unknown) {
    super();
  }
  override openBashRun(): Promise<never> {
    return Promise.reject(this.failure);
  }
}

const AGENT = create(conversationv1.AgentIdSchema, { value: "agent-1" });

function unsupportedSaid(): conversationv1.UserSaid {
  return create(conversationv1.UserSaidSchema, {
    content: create(conversationv1.UserContentSchema, {
      blocks: [create(conversationv1.UserContentBlockSchema, {})],
    }),
  });
}

describe("what the user said, at the edges of the block union", () => {
  const imageSaid = (
    location: conversationv1.ImageBlock["location"],
  ): conversationv1.UserSaid =>
    create(conversationv1.UserSaidSchema, {
      content: create(conversationv1.UserContentSchema, {
        blocks: [
          create(conversationv1.UserContentBlockSchema, {
            block: {
              case: "image",
              value: create(conversationv1.ImageBlockSchema, { mediaType: "image/png", location }),
            },
          }),
        ],
      }),
    });

  it("carries an image hosted at a url by that url", () => {
    const said = imageSaid({
      case: "url",
      value: create(conversationv1.ImageBlockUrlSchema, { url: "https://example.invalid/a.png" }),
    });

    expect(saidText(said)).toBe("https://example.invalid/a.png");
  });

  it("contributes an empty line for an image that states no location at all", () => {
    // Neither arm set: the block says an image is here and does not say where.
    expect(saidText(imageSaid({ case: undefined }))).toBe("");
  });

  it("RAISES on a content block carrying no arm, rather than delivering a prompt short one block", () => {
    expect(() => saidText(unsupportedSaid())).toThrow(/carries no arm/);
  });
});

describe("the prompt row's own guard", () => {
  it("REFUSES to record a prompt that carries no turn id", () => {
    const prompt = create(conversationv1.AgentPromptSchema, { agent: AGENT, said: textSaid("x") });

    expect(() => promptEntry(prompt, AGENT, false)).toThrow(/no turn id cannot be recorded/);
  });
});

describe("StartTurn against a dead query", () => {
  it("refuses query_dead rather than writing a prompt row nothing can deliver", async () => {
    const h = await harness();
    h.queryDead = true;

    expect(failureKind(await h.turns.startTurn(startTurn()))).toBe("queryDead");
  });

  it("writes no prompt row at all when the query is dead", async () => {
    const h = await harness();
    h.queryDead = true;

    await h.turns.startTurn(startTurn());

    expect(h.persistence.durable).toEqual([]);
  });

  it("RAISES when a StartTurn reaches the engine with no turn id", async () => {
    const h = await harness();
    const request = create(shimv1.StartTurnRequestSchema, { said: textSaid("x"), pageSize: 10 });

    await expect(h.turns.startTurn(request)).rejects.toThrow(/without a turn id or a prompt/);
  });

  it("RAISES when a StartTurn reaches the engine with no prompt", async () => {
    const h = await harness();
    const request = create(shimv1.StartTurnRequestSchema, { turn: TURN, pageSize: 10 });

    await expect(h.turns.startTurn(request)).rejects.toThrow(/without a turn id or a prompt/);
  });

  it("still refuses turn_already_open when the second StartTurn names no turn of its own", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());

    const response = await h.turns.startTurn(
      create(shimv1.StartTurnRequestSchema, { said: textSaid("again"), pageSize: 10 }),
    );

    expect(failureKind(response)).toBe("turnAlreadyOpen");
  });

  it("reports a non-Error submission failure by its string value", async () => {
    const h = await harness();
    h.submitRejects = "the vendor threw a string" as unknown as Error;

    const response = await h.turns.startTurn(startTurn());

    expect(response.result.case === "failure" ? response.result.value.detail : undefined).toBe(
      "the vendor threw a string",
    );
  });

  it("answers an empty page when the store rejects the opening read with a non-Error", async () => {
    const h = await harness(new NonErrorPageStore());

    const response = await h.turns.startTurn(startTurn());

    expect(
      response.result.case === "success" ? response.result.value.page?.entries.length : undefined,
    ).toBe(0);
  });
});

describe("UpdateAgent's own guards", () => {
  it("refuses no_session before it looks at the input", async () => {
    const h = await harness();
    h.identity = undefined;

    const response = await h.turns.updateAgent(
      create(shimv1.UpdateAgentRequestSchema, {
        input: create(conversationv1.AgentInputSchema, {
          input: { case: "stop", value: create(conversationv1.AgentStopSchema, {}) },
        }),
      }),
    );

    expect(failureKind(response)).toBe("noSession");
  });

  it("RAISES when UpdateAgent reaches the engine with no input", async () => {
    const h = await harness();

    await expect(h.turns.updateAgent(create(shimv1.UpdateAgentRequestSchema, {}))).rejects.toThrow(
      /with no input/,
    );
  });

  it("RAISES when the input carries no arm", async () => {
    const h = await harness();
    const request = create(shimv1.UpdateAgentRequestSchema, {
      input: create(conversationv1.AgentInputSchema, {}),
    });

    await expect(h.turns.updateAgent(request)).rejects.toThrow(/with no input arm/);
  });

  it("refuses a stop with nothing_running when the vendor query is dead", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());
    h.queryDead = true;

    const response = await h.turns.updateAgent(
      create(shimv1.UpdateAgentRequestSchema, {
        input: create(conversationv1.AgentInputSchema, {
          input: { case: "stop", value: create(conversationv1.AgentStopSchema, {}) },
        }),
      }),
    );

    expect(failureKind(response)).toBe("nothingRunning");
  });
});

describe("UpdateAgent.answer's own guards", () => {
  const answerRequest = (
    answer: conversationv1.AgentAnswer,
  ): shimv1.UpdateAgentRequest =>
    create(shimv1.UpdateAgentRequestSchema, {
      input: create(conversationv1.AgentInputSchema, { input: { case: "answer", value: answer } }),
    });

  it("RAISES when a question answer carries no ask", async () => {
    const h = await harness();
    const answer = create(conversationv1.AgentAnswerSchema, {
      answer: {
        case: "questionAnswer",
        value: create(conversationv1.AgentQuestionAnswerSchema, {
          answers: create(conversationv1.AgentQuestionAnswersSchema, {}),
        }),
      },
    });

    await expect(h.turns.updateAgent(answerRequest(answer))).rejects.toThrow(
      /carries no ask or no answers/,
    );
  });

  it("RAISES when an answer carries no arm", async () => {
    const h = await harness();

    await expect(
      h.turns.updateAgent(answerRequest(create(conversationv1.AgentAnswerSchema, {}))),
    ).rejects.toThrow(/an answer carries no arm/);
  });

  it("routes a question answer to the gate, which refuses an ask it is not holding", async () => {
    const h = await harness();
    const answer = create(conversationv1.AgentAnswerSchema, {
      answer: {
        case: "questionAnswer",
        value: create(conversationv1.AgentQuestionAnswerSchema, {
          ask: create(conversationv1.AgentQuestionIdSchema, { value: "q-1" }),
          answers: create(conversationv1.AgentQuestionAnswersSchema, {}),
        }),
      },
    });

    expect(failureKind(await h.turns.updateAgent(answerRequest(answer)))).toBe("noOpenAsk");
  });

  it("refuses answer_mismatch when a permission IS open and the answer names another", async () => {
    const h = await harness();
    void h.gate.canUseTool("Bash", {}, {
      signal: new AbortController().signal,
      toolUseID: "toolu_open",
      requestId: "r",
    } as Parameters<PermissionGate["canUseTool"]>[2]);
    await Promise.resolve();
    const answer = create(conversationv1.AgentAnswerSchema, {
      answer: {
        case: "permissionDecision",
        value: create(conversationv1.AgentPermissionDecisionSchema, {
          ask: create(conversationv1.AgentPermissionIdSchema, { value: "toolu_other" }),
          decision: {
            case: "denied",
            value: create(conversationv1.AgentPermissionDeniedByUserSchema, {}),
          },
        }),
      },
    });

    expect(failureKind(await h.turns.updateAgent(answerRequest(answer)))).toBe("answerMismatch");
  });

  it("refuses no_open_ask for a permission decision that names no ask at all", async () => {
    const h = await harness();
    const answer = create(conversationv1.AgentAnswerSchema, {
      answer: {
        case: "permissionDecision",
        value: create(conversationv1.AgentPermissionDecisionSchema, {
          decision: {
            case: "denied",
            value: create(conversationv1.AgentPermissionDeniedByUserSchema, {}),
          },
        }),
      },
    });

    expect(failureKind(await h.turns.updateAgent(answerRequest(answer)))).toBe("noOpenAsk");
  });
});

describe("KillTurn and DetachForeground without a session", () => {
  it("KillTurn refuses no_session", async () => {
    const h = await harness();
    h.identity = undefined;

    expect(
      failureKind(await h.turns.killTurn(create(shimv1.KillTurnRequestSchema, { turn: TURN }))),
    ).toBe("noSession");
  });

  it("KillTurn refuses no_turn_open for a request that names no turn at all", async () => {
    const h = await harness();

    expect(failureKind(await h.turns.killTurn(create(shimv1.KillTurnRequestSchema, {})))).toBe(
      "noTurnOpen",
    );
  });

  it("DetachForeground refuses no_session", async () => {
    const h = await harness();
    h.identity = undefined;

    expect(
      failureKind(
        await h.turns.detachForeground(create(shimv1.DetachForegroundRequestSchema, {})),
      ),
    ).toBe("noSession");
  });

  it("DetachForeground refuses already_concluded when the vendor query is dead", async () => {
    const h = await harness();
    h.queryDead = true;

    expect(
      failureKind(
        await h.turns.detachForeground(
          create(shimv1.DetachForegroundRequestSchema, {
            unit: create(conversationv1.AgentActivityIdSchema, { value: "toolu_1" }),
          }),
        ),
      ),
    ).toBe("alreadyConcluded");
  });
});

describe("WatchAgent's non-persistence failures", () => {
  it("lets a failure that is not a store refusal out as itself, not as NotFound", async () => {
    const h = await harness(new NonErrorPageStore());

    await expect(
      (async () => {
        for await (const _ of h.turns.watchAgent(
          create(shimv1.WatchAgentRequestSchema, { pageSize: 10 }),
        )) {
          break;
        }
      })(),
    ).rejects.toBe("the store threw a string");
  });
});

describe("ReadHistory's remaining arms", () => {
  it("refuses unknown_agent when no session has been started", async () => {
    const h = await harness();
    h.identity = undefined;

    const response = await h.turns.readHistory(
      create(shimv1.ReadHistoryRequestSchema, {
        pageSize: 10,
        position: { case: "first", value: create(shimv1.ReadHistoryFirstSchema, {}) },
      }),
    );

    expect(failureKind(response)).toBe("unknownAgent");
  });

  it("serves the page a cursored read answers with", async () => {
    const h = await harness();
    h.persistence.page = create(conversationv1.HistoryPageSchema, {
      entries: [create(conversationv1.HistoryEntryAtSchema, {})],
    });

    const response = await h.turns.readHistory(
      create(shimv1.ReadHistoryRequestSchema, {
        pageSize: 10,
        position: {
          case: "after",
          value: create(conversationv1.HistoryPointerSchema, { value: "p-1" }),
        },
      }),
    );

    expect(
      response.result.case === "success" ? response.result.value.page?.entries.length : undefined,
    ).toBe(1);
  });

  it("lets a failure that is not a store refusal out rather than mapping it to an arm", async () => {
    const h = await harness(new NonErrorPageStore());

    await expect(
      h.turns.readHistory(
        create(shimv1.ReadHistoryRequestSchema, {
          pageSize: 10,
          position: { case: "first", value: create(shimv1.ReadHistoryFirstSchema, {}) },
        }),
      ),
    ).rejects.toBe("the store threw a string");
  });
});

describe("WatchBash's failure arms", () => {
  const request = create(shimv1.WatchBashRequestSchema, {
    work: create(conversationv1.DetachedWorkIdSchema, { value: "b01" }),
  });

  const drain = async (stream: AsyncIterable<shimv1.WatchBashResponse>): Promise<void> => {
    for await (const _ of stream) {
      // The failure surfaces from the iteration itself.
    }
  };

  it("RAISES when WatchBash reaches the engine with no work id", async () => {
    const h = await harness();

    await expect(drain(h.turns.watchBash(create(shimv1.WatchBashRequestSchema, {})))).rejects.toThrow(
      /no work id/,
    );
  });

  it("closes the stream with NotFound when the store refuses the run", async () => {
    const h = await harness(
      new FailingBashStore(new PersistenceError("unknown_agent", "no such run")),
    );

    await expect(drain(h.turns.watchBash(request))).rejects.toBeInstanceOf(ConnectError);
  });

  it("names the run in the NotFound it closes with", async () => {
    const h = await harness(
      new FailingBashStore(new PersistenceError("unknown_agent", "no such run")),
    );

    await expect(drain(h.turns.watchBash(request))).rejects.toMatchObject({
      code: Code.NotFound,
    });
  });

  it("lets a failure that is not a store refusal out as itself", async () => {
    const boom = new Error("the reader blew up");
    const h = await harness(new FailingBashStore(boom));

    await expect(drain(h.turns.watchBash(request))).rejects.toBe(boom);
  });
});

describe("StopBash's remaining arms", () => {
  const stopRequest = (work: string): shimv1.StopBashRequest =>
    create(shimv1.StopBashRequestSchema, {
      work: create(conversationv1.DetachedWorkIdSchema, { value: work }),
    });

  const started = (taskId: string, toolUseId: string): SdkTaskStartedMessage =>
    ({
      type: "system",
      subtype: "task_started",
      task_id: taskId,
      tool_use_id: toolUseId,
      description: "",
      uuid: "00000000-0000-4000-8000-000000000000",
      session_id: "s",
    }) as SdkTaskStartedMessage;

  it("RAISES when StopBash reaches the engine with no work id", async () => {
    const h = await harness();

    await expect(h.turns.stopBash(create(shimv1.StopBashRequestSchema, {}))).rejects.toThrow(
      /no work id/,
    );
  });

  it("refuses already_ended for a handle the live table watched RETIRE", async () => {
    const h = await harness();
    h.live.onTaskStarted(started("b01", "t"));
    h.live.onTaskNotification({
      type: "system",
      subtype: "task_notification",
      task_id: "b01",
      status: "completed",
      output_file: "",
      summary: "",
      uuid: "00000000-0000-4000-8000-000000000001",
      session_id: "s",
    } as SdkTaskNotificationMessage);

    expect(failureKind(await h.turns.stopBash(stopRequest("t")))).toBe("alreadyEnded");
  });

  it("refuses already_ended when the vendor query is dead under a live handle", async () => {
    const h = await harness();
    h.live.onTaskStarted(started("b01", "t"));
    h.queryDead = true;

    expect(failureKind(await h.turns.stopBash(stopRequest("t")))).toBe("alreadyEnded");
  });
});

describe("DetachForeground's kind-before-state answer", () => {
  it("refuses not_detachable for a live unit whose KIND can never be backgrounded", async () => {
    const h = await harness();
    h.foreground.note("toolu_read", "read", false);

    const response = await h.turns.detachForeground(
      create(shimv1.DetachForegroundRequestSchema, {
        unit: create(conversationv1.AgentActivityIdSchema, { value: "toolu_read" }),
      }),
    );

    expect(failureKind(response)).toBe("notDetachable");
  });

  it("refuses unknown_unit for a request that addresses no unit at all", async () => {
    const h = await harness();

    expect(
      failureKind(await h.turns.detachForeground(create(shimv1.DetachForegroundRequestSchema, {}))),
    ).toBe("unknownUnit");
  });
});

describe("what the user said, with no content at all", () => {
  it("is the empty string for a prompt carrying no content message", () => {
    expect(saidText(create(conversationv1.UserSaidSchema, {}))).toBe("");
  });
});
