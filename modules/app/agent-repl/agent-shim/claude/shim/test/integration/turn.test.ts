/**
 * test/integration/turn.test.ts — StartTurn, WatchAgent, UpdateAgent, KillTurn,
 * ReadHistory.
 *
 * The agent surface is ONE API whether the agent is the main thread or a
 * subagent, and its two invariants run through everything here: ONE TURN IN
 * FLIGHT (structurally, never queued — the daemon is the only queue), and
 * HISTORY IS SERVED FROM THE STORE (never from memory), which is what makes a
 * restarted daemon able to reattach and miss nothing.
 */
import { create } from "@bufbuild/protobuf";
import { afterEach, describe, expect, test } from "vitest";
import { conversationv1, shimv1 } from "../../src/proto.js";
import { cleanupShims, spawnShim } from "../integration-support/harness.js";
import {
  agentId,
  freshSession,
  openStream,
  pointer,
  promptAgent,
  readHistoryAfter,
  readHistoryFirst,
  startTurnRequest,
  stopAgent,
  turnId,
  watchAgentRequest,
} from "../integration-support/client.js";
import {
  entryFrame,
  entryPrompt,
  historyPage,
  killTurnCause,
  killTurnLive,
  readHistoryKind,
  sessionStarted,
  sessionUpdate,
  startTurnKind,
  turnKilled,
  turnStarted,
  updateAccepted,
  updateAgentKind,
  watchAgentEntry,
  watchAgentPage,
} from "../integration-support/expect.js";
import { writtenKeys } from "../integration-support/store.js";
import { KEEPALIVE_PROMPT_MARKER as KEEPALIVE_MARKER } from "../../src/engine/keepalive.js";
import { promptText, readTranscript, userPrompts } from "../integration-support/vendor.js";

afterEach(cleanupShims);

/** Pull the WatchAgent stream until the turn's terminal frame arrives. */
async function untilTerminal(
  stream: ReturnType<typeof openStream<shimv1.WatchAgentResponse>>,
): Promise<conversationv1.HistoryEntryAt> {
  const frame = await stream.until((f) => {
    if (f.frame.case !== "entry") return false;
    const inner = watchAgentEntry(f).entry?.entry;
    if (inner?.case !== "agentFrame") return false;
    return inner.value.result.case === "success" || inner.value.result.case === "failure";
  });
  return watchAgentEntry(frame);
}

describe("StartTurn", () => {
  test("the answer echoes the turn id, the said and the origin", async () => {
    // ECHO TOKENS: every one of these is the daemon's own value coming back, so
    // a mismatch means the shim adopted something it minted itself.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({
        turn: "turn-echo",
        text: "!md",
        origin: conversationv1.PromptOrigin.WEBAPP_USER_SENT,
      }),
    );

    const prompt = turnStarted(response);
    expect(prompt.id?.value).toBe("turn-echo");
    expect(prompt.origin).toBe(conversationv1.PromptOrigin.WEBAPP_USER_SENT);
    expect(prompt.said?.content?.blocks[0]?.block.case).toBe("text");
    if (prompt.said?.content?.blocks[0]?.block.case === "text") {
      expect(prompt.said.content.blocks[0].block.value.text).toBe("!md");
    }
  });

  test("prompt.agent is the MAIN AgentId — the WatchAgent address", async () => {
    // The main AgentId is the conversation's ORIGINAL vendor session id, and it
    // is what a consumer addresses the turn's stream by.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));

    const prompt = turnStarted(
      await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" })),
    );

    expect(prompt.agent?.value).toBe(started.vendorSessionId);
  });

  test("a fresh session's opening page is EMPTY with a floor boundary", async () => {
    // An empty page is a page: `floor` says there is no older history, which is
    // a different statement from "no page was served".
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!md" }),
    );

    if (response.result.case !== "success") throw new Error("StartTurn refused");
    const page = response.result.value.page;
    expect(page?.entries).toEqual([]);
    expect(page?.boundary.case).toBe("floor");
  });

  test("a later turn's page carries the previous turn's settled entries, newest first", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const first = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await first.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await untilTerminal(first);
    first.close();

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t2", text: "!md" }),
    );

    if (response.result.case !== "success") throw new Error("StartTurn refused");
    const entries = response.result.value.page?.entries ?? [];
    expect(entries.length).toBeGreaterThan(0);
    // Newest first: the pointers descend.
    const pointers = entries.map((entry) => Number(entry.at?.value ?? 0));
    expect([...pointers].sort((a, b) => b - a)).toEqual(pointers);
  });

  test("the page carries terminal frames AS ENTRIES", async () => {
    // The feed's stop notice has no other source: a terminal that were not an
    // entry would vanish on repaint.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await untilTerminal(watch);
    watch.close();

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t2", text: "!md" }),
    );

    if (response.result.case !== "success") throw new Error("StartTurn refused");
    const entries = response.result.value.page?.entries ?? [];
    const terminals = entries.filter((entry) => {
      const frame = entryFrame(entry);
      return frame?.result.case === "success" || frame?.result.case === "failure";
    });
    expect(terminals.length).toBeGreaterThan(0);
  });

  test("the page replays NO start frames", async () => {
    // A settled frame carries the start's facts by the upsert rule, so a
    // replayed start would draw a second, permanently-running copy of the unit.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!read" }));
    await untilTerminal(watch);
    watch.close();

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t2", text: "!md" }),
    );

    if (response.result.case !== "success") throw new Error("StartTurn refused");
    const startArms = (response.result.value.page?.entries ?? []).filter((entry) => {
      const frame = entryFrame(entry);
      if (frame?.result.case !== "update") return false;
      const update = frame.result.value.update;
      if (update.case !== "activity") return false;
      const item = update.value.item;
      return item.case !== undefined && "result" in item.value
        ? (item.value as { result: { case?: string } }).result.case === "start"
        : false;
    });
    expect(startArms).toEqual([]);
  });

  test("the page carries prompts as user_prompt entries", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await untilTerminal(watch);
    watch.close();

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t2", text: "!md" }),
    );

    if (response.result.case !== "success") throw new Error("StartTurn refused");
    const prompts = (response.result.value.page?.entries ?? [])
      .map(entryPrompt)
      .filter((p): p is conversationv1.AgentPrompt => p !== null);
    expect(prompts.map((p) => p.id?.value)).toContain("t1");
  });

  test("R15: the AgentPrompt row is written BEFORE any activity row", async () => {
    // The shim's AgentPrompt row is the ONE served prompt, and it is durably
    // acked before the turn's first activity frame — otherwise a feed can paint
    // an answer above the question it answers.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await untilTerminal(watch);

    const keys = writtenKeys(shim.store?.writes() ?? []);
    const promptAt = keys.indexOf("prompt:t1");
    const firstActivityAt = keys.findIndex((key) => key.startsWith("activity:"));
    expect(promptAt).toBeGreaterThanOrEqual(0);
    expect(firstActivityAt).toBeGreaterThan(promptAt);
    watch.close();
  });

  test("a second StartTurn while one is open is refused turn_already_open", async () => {
    // ONE TURN IN FLIGHT, STRUCTURALLY: a second is a daemon fault, refused,
    // never queued. The vendor's queue is never ours.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!hold" }));

    const second = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t2", text: "!md" }),
    );

    expect(startTurnKind(second)).toBe("turnAlreadyOpen");
  });

  test("StartTurn with no session is refused no_session", async () => {
    const shim = await spawnShim();

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!md" }),
    );

    expect(startTurnKind(response)).toBe("noSession");
  });
});

describe("WatchAgent", () => {
  test("it opens with a page and then tails one POINTERED entry per write", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    const page = watchAgentPage(await watch.next());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    const first = watchAgentEntry(await watch.next());

    expect(page.boundary.case).toBe("floor");
    expect(first.at?.value).not.toBe("");
    expect(first.entry).toBeDefined();
    watch.close();
  });

  test("a streamed response's update frames are DELTAS, never cumulative", async () => {
    // `AgentResponseUpdate.new_markdown` is a delta and the accumulator is the
    // daemon's; a cumulative update would have every consumer double the text.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await untilTerminal(watch);

    const deltas: string[] = [];
    for (const frame of watch.frames()) {
      if (frame.frame.case !== "entry") continue;
      const agentFrame = entryFrame(frame.frame.value);
      if (agentFrame?.result.case !== "update") continue;
      const update = agentFrame.result.value.update;
      if (update.case !== "activity") continue;
      const item = update.value.item;
      if (item.case !== "response" || item.value.result.case !== "update") continue;
      deltas.push(item.value.result.value.newMarkdown);
    }
    expect(deltas.length).toBeGreaterThan(0);
    // No delta contains the one before it: cumulative text would nest.
    for (let index = 1; index < deltas.length; index++) {
      const previous = deltas[index - 1] ?? "";
      const current = deltas[index] ?? "";
      if (previous.length > 0) expect(current.startsWith(previous)).toBe(false);
    }
    watch.close();
  });

  test("the response's terminal carries the WHOLE prose", async () => {
    // Terminals carry wholes and self-correct: a consumer that dropped a delta
    // still ends up with the right text.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await untilTerminal(watch);

    const settled = watch.frames().flatMap((frame) => {
      if (frame.frame.case !== "entry") return [];
      const agentFrame = entryFrame(frame.frame.value);
      if (agentFrame?.result.case !== "update") return [];
      const update = agentFrame.result.value.update;
      if (update.case !== "activity") return [];
      const item = update.value.item;
      if (item.case !== "response" || item.value.result.case !== "success") return [];
      return [item.value.result.value];
    });
    expect(settled.length).toBeGreaterThan(0);
    expect(settled[0]?.prose?.markdown).not.toBe("");
    expect(settled[0]?.authorship.case).toBe("fromModel");
    watch.close();
  });

  test("usage rides EXACTLY ONE unit per API response", async () => {
    // Usage rides the AgentActivity ENVELOPE on the first-block unit; absence
    // means "not the carrying unit", never "free".
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "hello there" }));
    await untilTerminal(watch);

    const withUsage = watch.frames().filter((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(frame.frame.value);
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return update.case === "activity" && update.value.usage !== undefined;
    });
    // One API response in this scenario, so exactly one carrying unit.
    const carryingIds = new Set(
      withUsage.map((frame) => {
        const agentFrame =
          frame.frame.case === "entry" ? entryFrame(frame.frame.value) : null;
        if (agentFrame?.result.case !== "update") return "";
        const update = agentFrame.result.value.update;
        return update.case === "activity" ? (update.value.activityId?.value ?? "") : "";
      }),
    );
    expect(carryingIds.size).toBe(1);
    watch.close();
  });

  test("known_through serves ONLY newer entries", async () => {
    // The caller's own high-water mark: the store remembers nothing about what
    // it served, so catch-up is the caller's statement, not the store's memory.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const first = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await first.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    const terminal = await untilTerminal(first);
    const highWater = terminal.at?.value ?? "";
    first.close();

    const second = openStream((options) =>
      shim.clients.h1.watchAgent(
        watchAgentRequest({ knownThrough: pointer(highWater) }),
        options,
      ),
    );
    const page = watchAgentPage(await second.next());

    expect(
      page.entries.every((entry) => Number(entry.at?.value ?? 0) > Number(highWater)),
    ).toBe(true);
    second.close();
  });

  test("a reattach with known_through misses nothing and doubles nothing", async () => {
    // The daemon restart case: the connection dies mid-turn and the replacement
    // reattaches with its own mark.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const before = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await before.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    const seen = watchAgentEntry(await before.next());
    const mark = seen.at?.value ?? "";
    // The connection dies mid-turn.
    before.close();
    const during = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest({ knownThrough: pointer(mark) }), options),
    );
    await untilTerminal(during);
    during.close();

    const after = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest({ knownThrough: pointer(mark) }), options),
    );
    const page = watchAgentPage(await after.next());

    // Nothing doubled: the entry already seen is not replayed.
    expect(page.entries.map((entry) => entry.at?.value)).not.toContain(mark);
    // Nothing missed: the turn's terminal is in the catch-up page.
    expect(
      page.entries.some((entry) => {
        const frame = entryFrame(entry);
        return frame?.result.case === "success" || frame?.result.case === "failure";
      }),
    ).toBe(true);
    after.close();
  });

  test("an unknown watch target closes the stream at the transport", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const stream = openStream((options) =>
      shim.clients.h1.watchAgent(
        watchAgentRequest({ target: agentId("no-such-agent") }),
        options,
      ),
    );

    await expect(stream.next()).rejects.toThrow();
  });
});

describe("ReadHistory", () => {
  test("first serves the NEWEST page", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    const terminal = await untilTerminal(watch);
    watch.close();

    const page = historyPage(await shim.clients.h1.readHistory(readHistoryFirst()));

    expect(page.entries[0]?.at?.value).toBe(terminal.at?.value);
  });

  test("after(last_entry) walks older until the floor", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!read" }));
    await untilTerminal(watch);
    watch.close();
    const firstPage = historyPage(
      await shim.clients.h1.readHistory(readHistoryFirst({ pageSize: 2 })),
    );
    if (firstPage.boundary.case !== "more") {
      throw new Error("the fixture produced too few entries to page over");
    }

    const older = historyPage(
      await shim.clients.h1.readHistory(
        readHistoryAfter(firstPage.boundary.value.lastEntry ?? pointer("0"), { pageSize: 50 }),
      ),
    );

    expect(older.boundary.case).toBe("floor");
    const newest = Number(firstPage.entries[firstPage.entries.length - 1]?.at?.value ?? 0);
    expect(older.entries.every((entry) => Number(entry.at?.value ?? 0) < newest)).toBe(true);
  });

  test("an unknown agent is refused unknown_agent", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.readHistory(
      readHistoryFirst({ target: agentId("no-such-agent") }),
    );

    expect(readHistoryKind(response)).toBe("unknownAgent");
  });

  test("a stale pointer is refused stale_pointer", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.readHistory(
      readHistoryAfter(pointer("pointer-from-a-previous-store")),
    );

    expect(readHistoryKind(response)).toBe("stalePointer");
  });

  test("an unreachable store is refused store_unavailable", async () => {
    // The shim serves history FROM THE STORE, so a store that is down is a
    // refusal with a cause and never an empty page — an empty page would read
    // as "this conversation has no history".
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await shim.store?.close();

    const response = await shim.clients.h1.readHistory(readHistoryFirst());

    expect(readHistoryKind(response)).toBe("storeUnavailable");
  });

  test("an unreachable store also raises a store_unreachable fault", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    await watch.next();

    await shim.store?.close();
    await shim.clients.h1.readHistory(readHistoryFirst());
    const faulted = await watch.until((frame) => {
      const update = sessionUpdate(frame);
      if (update.update.case !== "diagnostics") return false;
      const health = update.update.value.health;
      return (
        health.case === "unhealthy" &&
        health.value.faults.some((fault) => fault.kind.case === "storeUnreachable")
      );
    });

    expect(sessionUpdate(faulted).update.case).toBe("diagnostics");
    watch.close();
  });
});

describe("UpdateAgent", () => {
  test("stop on the main agent during a held turn interrupts it", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!hold" }));

    const response = await shim.clients.h1.updateAgent(stopAgent());
    const terminal = await untilTerminal(watch);

    updateAccepted(response);
    const frame = entryFrame(terminal);
    expect(frame?.result.case).toBe("success");
    if (frame?.result.case === "success") {
      expect(frame.result.value.outcome.case).toBe("interrupted");
      if (frame.result.value.outcome.case === "interrupted") {
        expect(frame.result.value.outcome.value.cause.case).toBe("byUser");
      }
    }
    watch.close();
  });

  test("stop with nothing running is refused nothing_running", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.updateAgent(stopAgent());

    expect(updateAgentKind(response)).toBe("nothingRunning");
  });

  test("a prompt to an existing subagent is refused not_deliverable", async () => {
    // THE PINNED SDK HAS NO ROUTE to prompt an existing subagent, and the
    // nearest landed arm (nothing_running) would have lied about why.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent" }));
    const spawned = await watch.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      if (update.case !== "activity") return false;
      const item = update.value.item;
      return item.case === "subagent" && item.value.result.case === "start";
    });
    const agentFrame = entryFrame(watchAgentEntry(spawned));
    let created = "";
    if (agentFrame?.result.case === "update") {
      const update = agentFrame.result.value.update;
      if (update.case === "activity" && update.value.item.case === "subagent") {
        const subagent = update.value.item.value;
        if (subagent.result.case === "start") {
          created = subagent.result.value.createdAgentId?.value ?? "";
        }
      }
    }

    const response = await shim.clients.h1.updateAgent(
      promptAgent("keep going", agentId(created)),
    );

    expect(updateAgentKind(response)).toBe("notDeliverable");
    watch.close();
  });

  test("an answer with no open ask is refused no_open_ask", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.updateAgent(
      create(shimv1.UpdateAgentRequestSchema, {
        input: create(conversationv1.AgentInputSchema, {
          input: {
            case: "answer",
            value: create(conversationv1.AgentAnswerSchema, {
              answer: {
                case: "permissionDecision",
                value: create(conversationv1.AgentPermissionDecisionSchema, {
                  ask: create(conversationv1.AgentPermissionIdSchema, { value: "nobody" }),
                  decision: {
                    case: "allowed",
                    value: create(conversationv1.AgentPermissionAllowedSchema, {
                      scope: {
                        case: "once",
                        value: create(conversationv1.AgentPermissionAllowedOnceSchema, {}),
                      },
                    }),
                  },
                }),
              },
            }),
          },
        }),
      }),
    );

    expect(updateAgentKind(response)).toBe("noOpenAsk");
  });

  test("an unknown target agent is refused unknown_agent", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.updateAgent(stopAgent(agentId("no-such-agent")));

    expect(updateAgentKind(response)).toBe("unknownAgent");
  });
});

describe("KillTurn", () => {
  test("with nothing spawned it ends agent_only and the stream concludes interrupted", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!hold" }));

    const response = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: false }),
    );
    const terminal = await untilTerminal(watch);

    expect(turnKilled(response).how.case).toBe("agentOnly");
    const frame = entryFrame(terminal);
    if (frame?.result.case === "success" && frame.result.value.outcome.case === "interrupted") {
      expect(frame.result.value.outcome.value.cause.case).toBe("byUser");
    } else {
      throw new Error("the turn did not conclude AgentSuccess.interrupted.by_user");
    }
    watch.close();
  });

  test("live detached work refuses, NAMING it", async () => {
    // The refusal set is transitive via the spawn-provenance map — the one
    // bounded piece of state the shim keeps, and only so this refusal can name
    // what forcing would destroy.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    await watch.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      return entryFrame(watchAgentEntry(frame))?.result.case === "detachedWork";
    });

    const response = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: false }),
    );

    expect(killTurnCause(response)).toBe("live");
    expect(killTurnLive(response).liveWork.length).toBeGreaterThan(0);
    watch.close();
  });

  test("force ends it, naming the stopped work", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: false }),
    );

    const response = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: true }),
    );

    const killed = turnKilled(response);
    expect(killed.how.case).toBe("forced");
    if (killed.how.case === "forced") {
      expect(killed.how.value.stoppedWork.length).toBeGreaterThan(0);
    }
  });

  test("a TurnId that is not the open turn is refused not_the_open_turn", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!hold" }));

    const response = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("some-other-turn"), force: false }),
    );

    expect(killTurnCause(response)).toBe("notTheOpenTurn");
  });

  test("with no turn open it is refused no_turn_open", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: false }),
    );

    expect(killTurnCause(response)).toBe("noTurnOpen");
  });
});

describe("keep-alives", () => {
  // KEEP-ALIVES ARE ENTIRELY SHIM-INTERNAL: nothing keep-alive-shaped exists on
  // the wire (no PromptOrigin value, no rpc, no control-plane signal), so the
  // only producer is the shim's OWN cadence. Its interval is a module constant
  // (four minutes against the vendor's five-minute cache tier), and waiting
  // four real minutes is the sleep this suite refuses to write — so `main.ts`
  // honors `AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS` under `--fake` and ONLY
  // under `--fake`, which is what makes both obligations below observable.
  // Every wait here is on the shim's own records, never on a clock.

  /** A shim whose keep-alive cadence beats fast enough to observe. */
  const spawnBeating = async (): Promise<Awaited<ReturnType<typeof spawnShim>>> =>
    spawnShim({ env: { AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS: "200" } });

  /** Resolves on the shim's record that it submitted a keep-alive. */
  const keepaliveSubmitted = async (
    shim: Awaited<ReturnType<typeof spawnShim>>,
  ): Promise<void> => {
    await shim.log.record((record) => record.context.outcome === "keepalive_submitted");
  };

  test("a keep-alive's rows land UNSERVED rather than in the book", async () => {
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());

    await keepaliveSubmitted(shim);

    const unserved = shim.store?.unserved() ?? [];
    expect(unserved.some((item) => item.unservedItem.case === "keepalive")).toBe(true);
  });

  test("no keep-alive prompt appears in any page", async () => {
    // The keep-alive made a real API call, so it is RECORDED; it has no book,
    // so serving it would put a turn nobody asked for in the feed.
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());
    await keepaliveSubmitted(shim);

    const page = historyPage(await shim.clients.h1.readHistory(readHistoryFirst()));

    const said = page.entries
      .map(entryPrompt)
      .flatMap((prompt) => prompt?.said?.content?.blocks ?? [])
      .map((block) => (block.block.case === "text" ? block.block.value.text : ""));
    expect(said.some((text) => text.startsWith(KEEPALIVE_MARKER))).toBe(false);
  });

  test("a real prompt after keep-alives is delivered with the context rolled back", async () => {
    // A real prompt must never build on keep-alive context, so the query is
    // replaced by one that resumes only THROUGH the last real record.
    const shim = await spawnBeating();
    await shim.clients.h1.startSession(freshSession());
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "hello" }));
    await keepaliveSubmitted(shim);

    const rewound = shim.log.record((record) => record.context.resume_session_at !== undefined);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "and again" }));
    const record = await rewound;

    expect(record.context.discarded_keepalive_turns).toBeDefined();
  });

  test("a real prompt's transcript record carries NO keep-alive marker", async () => {
    // The half of the yield obligation that IS observable: the shim's own
    // prompts are marked, and a daemon-submitted prompt must never be.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!keepalive" }));
    await untilTerminal(watch);

    const prompts = userPrompts(readTranscript(shim.dirs, started.vendorSessionId));
    expect(prompts.length).toBeGreaterThan(0);
    expect(prompts.every((record) => !promptText(record).startsWith("<!--agent-repl:keepalive-->"))).toBe(
      true,
    );
    watch.close();
  });
});
