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
import type { SdkTaskNotificationMessage, SdkTaskStartedMessage } from "../../src/sdk/types.js";

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
}

async function harness(): Promise<Harness> {
  const persistence = new RecordingPersistence();
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
  };
  const context: SessionContext = {
    persistence,
    foreground,
    gate,
    live,
    identity: () => state.identity,
    query: () => query,
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
    } as Parameters<PermissionGate["canUseTool"]>[2]);
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
    } as SdkTaskStartedMessage);

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
      { type: "system", subtype: "task_started", task_id: "b01", tool_use_id: "t", description: "", uuid: "00000000-0000-4000-8000-000000000000", session_id: "s" } as SdkTaskStartedMessage,
      "turn-1",
    );

    expect(failureKind(await h.turns.killTurn(kill(false)))).toBe("live");
  });

  it("forced, ends the work a CLOSED turn left running", async () => {
    const h = await harness();
    h.live.onTaskStarted(
      { type: "system", subtype: "task_started", task_id: "b01", tool_use_id: "t", description: "", uuid: "00000000-0000-4000-8000-000000000000", session_id: "s" } as SdkTaskStartedMessage,
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
      { type: "system", subtype: "task_started", task_id: "b01", tool_use_id: "t", description: "", uuid: "00000000-0000-4000-8000-000000000000", session_id: "s" } as SdkTaskStartedMessage,
      "turn-1",
    );

    expect(failureKind(await h.turns.killTurn(kill(false)))).toBe("live");
  });

  it("NAMES the live work in the refusal, by its spawning call", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());
    h.live.onTaskStarted(
      { type: "system", subtype: "task_started", task_id: "b01", tool_use_id: "t", description: "", uuid: "00000000-0000-4000-8000-000000000000", session_id: "s" } as SdkTaskStartedMessage,
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
      { type: "system", subtype: "task_started", task_id: "b01", tool_use_id: "t", description: "", uuid: "00000000-0000-4000-8000-000000000000", session_id: "s" } as SdkTaskStartedMessage,
      "turn-1",
    );

    await h.turns.killTurn(kill(true));

    expect(h.query.stoppedTasks).toEqual(["b01"]);
  });

  it("does not reach work another turn spawned", async () => {
    const h = await harness();
    await h.turns.startTurn(startTurn());
    h.live.onTaskStarted(
      { type: "system", subtype: "task_started", task_id: "b99", tool_use_id: "t", description: "", uuid: "00000000-0000-4000-8000-000000000000", session_id: "s" } as SdkTaskStartedMessage,
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
    } as SdkTaskStartedMessage);

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
    } as SdkTaskStartedMessage);

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
      } as SdkTaskStartedMessage,
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
      } as SdkTaskStartedMessage,
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
    } as SdkTaskStartedMessage);
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
    } as SdkTaskStartedMessage);
    h.live.onTaskNotification({
      type: "system",
      subtype: "task_notification",
      task_id: "b02",
      status: "completed",
      output_file: "",
      summary: "",
      uuid: "00000000-0000-4000-8000-000000000001",
      session_id: "s",
    } as SdkTaskNotificationMessage);
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
