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
import { Code, ConnectError } from "@connectrpc/connect";
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

  test("a fresh session's opening page carries EXACTLY the prompt just delivered", async () => {
    // R15 (RULED): the AgentPrompt row is DURABLE before the page is read, and
    // ONE CALL SUBMITS AND PAINTS -- so the page a fresh session's first
    // StartTurn answers with already contains the prompt that opened the turn,
    // and nothing else. An empty page here would mean the consumer's first
    // paint was missing the turn it had just started. `floor` still says there
    // is no older history, which is a different statement from "no page".
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));

    const response = await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!md" }),
    );

    if (response.result.case !== "success") throw new Error("StartTurn refused");
    const page = response.result.value.page;
    expect(page?.entries.length).toBe(1);
    expect(page?.boundary.case).toBe("floor");
    // THE ENTRY IS THAT PROMPT, not merely a prompt: its TurnId and its text
    // are the ones this call carried. "A user_prompt is on the page" would pass
    // on a shim that served some other turn's question.
    const served = entryPrompt(page?.entries[0] ?? create(conversationv1.HistoryEntryAtSchema, {}));
    expect(served?.id?.value).toBe("t1");
    const blocks = served?.said?.content?.blocks ?? [];
    expect(blocks.map((block) => (block.block.case === "text" ? block.block.value.text : ""))).toEqual([
      "!md",
    ]);
    // AND THE FILE PLANE AGREES. Both values above are the shim's own; the
    // vendor's transcript is the independent witness that the prompt it served
    // is the prompt it actually delivered.
    expect(userPrompts(readTranscript(shim.dirs, started.vendorSessionId)).map(promptText)).toEqual([
      "!md",
    ]);
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
    // NEWEST FIRST, ASSERTED WITHOUT READING THE POINTER. Pointers are OPAQUE:
    // the store mints them and only it may interpret them, so parsing one as an
    // integer here would build the suite against a store that happens to mint
    // numbers. What "newest first" means on the wire is the SERVED SEQUENCE —
    // the page's own order — against a ground truth this test already knows:
    // the order the two turns were started in. t1's prompt must come AFTER t2's
    // in the served list.
    const promptOrder = entries
      .map(entryPrompt)
      .filter((prompt): prompt is conversationv1.AgentPrompt => prompt !== null)
      .map((prompt) => prompt.id?.value ?? "");
    expect(promptOrder).toEqual(["t2", "t1"]);
    // Every pointer is distinct, which is the only other property a consumer
    // may rely on.
    const served = entries.map((entry) => entry.at?.value ?? "");
    expect(new Set(served).size).toBe(served.length);
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
  test("opened before any turn, it opens with an EMPTY page and a floor boundary", async () => {
    // The counterpart to R15's StartTurn page: nothing has been written yet, so
    // this is the one open that legitimately paints nothing. An empty page is
    // still a page -- `floor` says there is no older history, which is a
    // different statement from "no page was served".
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    const page = watchAgentPage(await watch.next());

    expect(page.entries).toEqual([]);
    expect(page.boundary.case).toBe("floor");
    watch.close();
  });

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
    // THE TERMINAL SELF-CORRECTS: a consumer that dropped every delta still
    // ends up with the right text, which is only true if the whole IS the
    // concatenation. "Not empty" would pass on a terminal carrying one word.
    const deltas = watch.frames().flatMap((frame) => {
      if (frame.frame.case !== "entry") return [];
      const agentFrame = entryFrame(frame.frame.value);
      if (agentFrame?.result.case !== "update") return [];
      const update = agentFrame.result.value.update;
      if (update.case !== "activity") return [];
      const item = update.value.item;
      if (item.case !== "response" || item.value.result.case !== "update") return [];
      return [item.value.result.value.newMarkdown];
    });
    expect(settled.length).toBe(1);
    expect(deltas.length).toBeGreaterThan(1);
    expect(settled[0]?.prose?.markdown).toBe(deltas.join(""));
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
    // THE CARRIER IS THE MESSAGE'S FIRST BLOCK. Unit keys are
    // `activity:<message>:<n>` 0-based, so the carrier's id ends in `:0` — and
    // it is the EARLIEST-POINTERED unit of that message, which is the property
    // a consumer relies on when it draws the cost beside the answer's opening.
    const carrier = [...carryingIds][0] ?? "";
    expect(carrier.endsWith(":0")).toBe(true);
    const message = carrier.slice(0, carrier.lastIndexOf(":"));
    // Served order IS pointer order on one tail, so "earliest-pointered" is the
    // first frame of that message the stream served — no pointer is parsed.
    const ofMessage = watch
      .frames()
      .filter((frame) => frame.frame.case === "entry")
      .map((frame) => {
        const agentFrame = entryFrame(watchAgentEntry(frame));
        if (agentFrame?.result.case !== "update") return "";
        const update = agentFrame.result.value.update;
        return update.case === "activity" ? (update.value.activityId?.value ?? "") : "";
      })
      .filter((id) => id.startsWith(`${message}:`));
    expect(ofMessage[0]).toBe(carrier);
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

    // POINTERS ARE OPAQUE: only the store may interpret one, so "newer" is
    // asserted as the two facts a consumer actually has — the mark itself is
    // not replayed, and every entry the FIRST watch already served (the whole
    // turn, through its terminal) is absent from the catch-up page.
    const alreadySeen = new Set(
      first
        .frames()
        .filter((frame) => frame.frame.case === "entry")
        .map((frame) => watchAgentEntry(frame).at?.value ?? ""),
    );
    expect(alreadySeen.has(highWater)).toBe(true);
    expect(
      page.entries.map((entry) => entry.at?.value ?? "").filter((at) => alreadySeen.has(at)),
    ).toEqual([]);
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

  test("an unknown watch target is refused NOT_FOUND, naming the agent", async () => {
    // INTERIM RULING (ledger, rebuild merge): `WatchAgent` has no refusal arm
    // of its own, so an unknown target is a TRANSPORT refusal — and it is
    // pinned rather than left as "some throw", because a store outage, a
    // cancelled call and an id nobody minted all reach a caller as an
    // exception and only the code tells them apart.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const stream = openStream((options) =>
      shim.clients.h1.watchAgent(
        watchAgentRequest({ target: agentId("no-such-agent") }),
        options,
      ),
    );
    const failure = await stream.nextOrEnd().then(
      () => undefined,
      (err: unknown) => ConnectError.from(err),
    );

    if (failure === undefined) throw new Error("the unknown target was not refused");
    expect(failure.code).toBe(Code.NotFound);
    expect(failure.message).toContain("no-such-agent");
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
    // "OLDER" WITHOUT READING A POINTER: the walk is disjoint from the page it
    // continued, and it stopped at the floor. Parsing the marks as integers
    // would assert against a store that happens to mint numbers.
    const firstMarks = new Set(firstPage.entries.map((entry) => entry.at?.value ?? ""));
    expect(older.entries.map((entry) => entry.at?.value ?? "").filter((at) => firstMarks.has(at))).toEqual(
      [],
    );
    expect(older.entries.length).toBeGreaterThan(0);
  });

  test("after(last_entry) with a page_size larger than the remainder ends at the FLOOR", async () => {
    // A budget bigger than what is left is not an error and not a `more`: the
    // page is however many entries remain, and the boundary says there are no
    // older ones. A shim that reported `more` here would have a consumer paging
    // forever against an empty tail.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!read" }));
    await untilTerminal(watch);
    watch.close();
    const whole = historyPage(
      await shim.clients.h1.readHistory(readHistoryFirst({ pageSize: 200 })),
    );
    if (whole.boundary.case !== "floor") {
      throw new Error("the fixture did not fit in one page, so there is no remainder to floor");
    }
    const firstPage = historyPage(
      await shim.clients.h1.readHistory(readHistoryFirst({ pageSize: 2 })),
    );
    if (firstPage.boundary.case !== "more") {
      throw new Error("the fixture produced too few entries to page over");
    }

    const rest = historyPage(
      await shim.clients.h1.readHistory(
        readHistoryAfter(firstPage.boundary.value.lastEntry ?? pointer("0"), { pageSize: 200 }),
      ),
    );

    expect(rest.boundary.case).toBe("floor");
    // EXACTLY the remainder: the whole book, less the page already served.
    expect(rest.entries.length).toBe(whole.entries.length - firstPage.entries.length);
  });

  test("an unknown agent is refused unknown_agent", async () => {
    // THE STORE DECIDES, AND IT SAYS SO IN A TYPED ARM. The shim does not know
    // which books exist — it asks — so this refusal only happens if the store
    // states it. The fake used to serve an empty page for a book nobody had
    // written, which made the shim look wrong when it was the fake that never
    // refused; it is now armed with the arm a real store would send.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    shim.store?.failReads("OpenAgentSession", "invalid_request", "no book named that");

    const response = await shim.clients.h1.readHistory(
      readHistoryFirst({ target: agentId("no-such-agent") }),
    );

    expect(readHistoryKind(response)).toBe("unknownAgent");
  });

  test("a stale pointer is refused stale_pointer", async () => {
    // The pointer is the STORE's to recognize: it minted it, and only it can
    // say the mark names no line of this book.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    // A pointer-bearing read WALKS OLDER through ReadAgentPage; only the
    // pointerless first read goes through OpenAgentSession, so arming the
    // opening verb here would arm one the request never reaches.
    shim.store?.failReads("ReadAgentPage", "stale_pointer", "that mark is not in this book");

    const response = await shim.clients.h1.readHistory(
      readHistoryAfter(pointer("pointer-from-a-previous-store")),
    );

    expect(readHistoryKind(response)).toBe("stalePointer");
  });

  test("the refusal follows the ARM, not the store's prose", async () => {
    // The negative that gives the two above their meaning: a detail saying the
    // opposite of the arm must not change the answer. An earlier reader
    // classified by substring, so a storage failure whose driver text happened
    // to mention a pointer was reported as a stale pointer and the engine
    // re-read a book that was actually unreachable.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    shim.store?.failReads("OpenAgentSession", "storage_failure", "that pointer names no such agent");

    const response = await shim.clients.h1.readHistory(readHistoryFirst());

    expect(readHistoryKind(response)).toBe("storeUnavailable");
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
    const announced = await watch.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      return entryFrame(watchAgentEntry(frame))?.result.case === "detachedWork";
    });
    const detached = entryFrame(watchAgentEntry(announced));
    if (detached?.result.case !== "detachedWork") {
      throw new Error("expected the run's announcement");
    }
    const run = detached.result.value.work?.value ?? "";

    const response = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: false }),
    );

    expect(killTurnCause(response)).toBe("live");
    // NAMING IT is the whole point of the refusal: a count says only that
    // SOMETHING is live, and the refusal exists so a consumer can tell the user
    // exactly what forcing would destroy.
    expect(killTurnLive(response).liveWork.map((work) => work.value)).toEqual([run]);
    watch.close();
  });

  test("force ends it, naming the stopped work", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const watch = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    await watch.next();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    const announced = await watch.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      return entryFrame(watchAgentEntry(frame))?.result.case === "detachedWork";
    });
    const detached = entryFrame(watchAgentEntry(announced));
    if (detached?.result.case !== "detachedWork") {
      throw new Error("expected the run's announcement");
    }
    const run = detached.result.value.work?.value ?? "";
    const refused = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: false }),
    );

    const response = await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: true }),
    );

    const killed = turnKilled(response);
    expect(killed.how.case).toBe("forced");
    if (killed.how.case !== "forced") throw new Error("the forced kill did not report forced");
    // THE SAME ITEM THE REFUSAL NAMED, now named as stopped: the two lists are
    // the consumer's before-and-after of one act, and a count would not say
    // they are about the same work.
    expect(killed.how.value.stoppedWork.map((work) => work.value)).toEqual([run]);
    expect(killTurnLive(refused).liveWork.map((work) => work.value)).toEqual([run]);
    watch.close();
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
    // THE SUBMISSION AND THE ROW ARE TWO INSTANTS. The shim's own record says
    // it submitted; the row reaches the store on the writer's next batch. Wait
    // for the row itself, never for the log line that precedes it.
    const arrived = await (shim.store?.unservedArrived("keepalive") ??
      Promise.reject(new Error("this shim has no store")));

    expect(arrived.unservedItem.case).toBe("keepalive");
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
    // What the VENDOR received, not merely what the shim intended: the mock
    // records the rewind target it was handed, and the shim's own record is no
    // evidence the value ever reached the query.
    const atVendor = shim.log.record(
      (record) => record.context.vendor_resume_session_at !== undefined,
    );
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "and again" }));
    const record = await rewound;

    expect(record.context.discarded_keepalive_turns).toBeDefined();
    expect((await atVendor).context.vendor_resume_session_at).toBe(
      record.context.resume_session_at,
    );
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
    expect(prompts.every((record) => !promptText(record).startsWith(KEEPALIVE_MARKER))).toBe(true);
    watch.close();
  });
});
