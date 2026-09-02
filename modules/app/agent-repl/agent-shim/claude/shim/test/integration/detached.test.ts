/**
 * test/integration/detached.test.ts — detached work: announcements, WatchBash,
 * StopBash, DetachForeground, subagents, and reconciliation at resume.
 *
 * # The two rules that shape every test here
 *
 * LIVENESS IS STRUCTURAL: the set of open detached-item streams IS the live set,
 * and the daemon opens them eagerly on announcement. So the announcement is a
 * consumer OBLIGATION, not a notification, and it has to carry everything the
 * consumer needs to open the right stream.
 *
 * ONE HANDLE, NO VENDOR IDS: `DetachedWorkId.value` is the SPAWNING CALL's
 * tool_use_id — for a subagent it is also that agent's `AgentId` — so the
 * announcement handle, the unit's activity id and `created_agent_id` are one
 * value. The vendor's own task id never appears on the wire, which is why the
 * spool assertions here SCAN the spool directory instead of deriving a filename
 * from a wire value.
 */
import { create } from "@bufbuild/protobuf";
import { afterEach, describe, expect, test } from "vitest";
import { conversationv1, shimv1 } from "../../src/proto.js";
import { cleanupShims, spawnShim } from "../integration-support/harness.js";
import {
  activityId,
  agentId,
  freshSession,
  openStream,
  resumeSession,
  startTurnRequest,
  startTurnRequest as turnFor,
  watchAgentRequest,
  workId,
} from "../integration-support/client.js";
import {
  bashFrame,
  detachForegroundAccepted,
  detachForegroundKind,
  entryFrame,
  sessionStarted,
  stopBashAccepted,
  stopBashKind,
  watchAgentEntry,
  watchAgentPage,
} from "../integration-support/expect.js";
import {
  createStoreClient,
  seedBashLifecycle,
  seedDetachedAnnouncement,
  sidecarProducer,
  writtenKeys,
} from "../integration-support/store.js";
import { awaitSpoolExit, findSubagentMetaByToolUseId } from "../integration-support/vendor.js";

afterEach(cleanupShims);

type AgentStream = ReturnType<typeof openStream<shimv1.WatchAgentResponse>>;

/** Open the main agent's stream past its opening page. */
async function openAgentStream(
  shim: Awaited<ReturnType<typeof spawnShim>>,
  target?: conversationv1.AgentId,
): Promise<AgentStream> {
  const stream = openStream((options) =>
    shim.clients.h1.watchAgent(
      watchAgentRequest(target === undefined ? {} : { target }),
      options,
    ),
  );
  await stream.next();
  return stream;
}

/**
 * Wait for a detached-work announcement on a stream and return it.
 *
 * `withOutput` waits for the announcement that STATES an output. A detached
 * shell is announced TWICE onto one row: `task_started` says the work left the
 * turn, and the vendor names no output file there (corpus:
 * stream/task_started.jsonl), so the path arrives with the tool result's upsert
 * of the same row. A test about the path must wait for that one — waiting for
 * the first frame asserts against a message the vendor cannot fill.
 */
async function awaitAnnouncement(
  stream: AgentStream,
  withOutput = false,
): Promise<conversationv1.AgentDetachedWork> {
  const frame = await stream.until((f) => {
    if (f.frame.case !== "entry") return false;
    const result = entryFrame(watchAgentEntry(f))?.result;
    if (result?.case !== "detachedWork") return false;
    return !withOutput || result.value.output !== undefined;
  });
  const agentFrame = entryFrame(watchAgentEntry(frame));
  if (agentFrame?.result.case !== "detachedWork") {
    throw new Error("expected a detached_work announcement");
  }
  return agentFrame.result.value;
}

describe("a detached shell's announcement", () => {
  test("!bash-detach announces detached_work with a readable output path", async () => {
    // The announcement is the consumer's open-a-stream obligation, and the
    // output path is how a surface offers the file when the stream is gone.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach" }));
    const announced = await awaitAnnouncement(stream, true);

    expect(announced.work?.value).not.toBe("");
    expect(announced.output?.path).not.toBe("");
    expect(announced.output?.readability.case).toBe("readable");
    stream.close();
  });

  test("the announcement's origin is detached, naming the call it left", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach" }));
    const announced = await awaitAnnouncement(stream);

    expect(announced.origin.case).toBe("detached");
    if (announced.origin.case === "detached") {
      expect(announced.origin.value.detachedFromId?.value).not.toBe("");
      expect(announced.origin.value.cause.case).toBe("requested");
    }
    stream.close();
  });

  test("the DetachedWorkId IS the spawning call's activity id", async () => {
    // ONE HANDLE: the wire never carries the vendor's task id, so the work id
    // and the unit's own id are the same value and a consumer needs no mapping.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach" }));
    const announced = await awaitAnnouncement(stream);

    if (announced.origin.case !== "detached") throw new Error("expected the detached origin");
    expect(announced.work?.value).toBe(announced.origin.value.detachedFromId?.value);
    stream.close();
  });

  test("the announcement is a page line keyed detached:<work id>", async () => {
    // Landing 3: the announcement is its OWN row, so it cannot overwrite the
    // spawning call, and it replays on a repaint like any other page line.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach" }));
    const announced = await awaitAnnouncement(stream);

    expect(writtenKeys(shim.store?.writes() ?? [])).toContain(
      `detached:${announced.work?.value ?? ""}`,
    );
    stream.close();
  });

  test("the announcement replays in a later WatchAgent page", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach" }));
    const announced = await awaitAnnouncement(stream);
    stream.close();

    const repaint = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest(), options),
    );
    const page = watchAgentPage(await repaint.next());

    const announcements = page.entries.filter(
      (entry) => entryFrame(entry)?.result.case === "detachedWork",
    );
    expect(
      announcements.some((entry) => {
        const frame = entryFrame(entry);
        return (
          frame?.result.case === "detachedWork" &&
          frame.result.value.work?.value === announced.work?.value
        );
      }),
    ).toBe(true);
    repaint.close();
  });
});

describe("WatchBash serves the SIDECAR's rows", () => {
  test("it opens with start carrying the ORIGINAL instant, then updates, then the terminal", async () => {
    // Every byte of detached shell output comes from the sidecar tailing the
    // vendor's spool — foreground output is observable nowhere while running —
    // so the rows are seeded here and the shim must serve them back in order.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach" }));
    const announced = await awaitAnnouncement(stream);
    const run = announced.work?.value ?? "";
    const originalInstant = 1_700_000_000_000;
    await seedBashLifecycle(
      createStoreClient(shim.dirs.storeSocket),
      sidecarProducer(started.vendorSessionId),
      {
        run,
        work: run,
        command: "sleep 1 && echo done",
        startedAtMs: originalInstant,
        chunks: ["first\n", "second\n"],
        exitCode: 0,
        topLevel: started.vendorSessionId,
      },
    );

    const bash = openStream((options) =>
      shim.clients.h1.watchBash(
        create(shimv1.WatchBashRequestSchema, { work: workId(run) }),
        options,
      ),
    );
    const frames = await bash.drain();

    const arms = frames.map((frame) => bashFrame(frame).result.case);
    expect(arms[0]).toBe("start");
    expect(arms[arms.length - 1]).toBe("success");
    expect(arms.filter((arm) => arm === "update").length).toBe(2);
    const first = bashFrame(frames[0] as shimv1.WatchBashResponse);
    if (first.result.case === "start") {
      // A RE-ANNOUNCEMENT REPEATS THE ORIGINAL INSTANT: drawn clocks must not
      // reset when work moves streams.
      expect(first.result.value.startedAt?.atMs).toBe(BigInt(originalInstant));
    }
    stream.close();
  });

  test("the update frames carry offsets and deltas, never a settled whole", async () => {
    // Bash keeps offset+delta because its spool has no settled whole; prose does
    // not, because its terminal carries the text and self-corrects.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach" }));
    const announced = await awaitAnnouncement(stream);
    const run = announced.work?.value ?? "";
    await seedBashLifecycle(
      createStoreClient(shim.dirs.storeSocket),
      sidecarProducer(started.vendorSessionId),
      {
        run,
        work: run,
        command: "echo",
        startedAtMs: 1_700_000_000_000,
        chunks: ["abc", "de"],
        exitCode: 0,
        topLevel: started.vendorSessionId,
      },
    );

    const bash = openStream((options) =>
      shim.clients.h1.watchBash(
        create(shimv1.WatchBashRequestSchema, { work: workId(run) }),
        options,
      ),
    );
    const frames = await bash.drain();

    const updates = frames
      .map(bashFrame)
      .filter((frame) => frame.result.case === "update")
      .map((frame) => (frame.result.case === "update" ? frame.result.value : null));
    expect(updates.map((update) => Number(update?.fromOffset ?? -1))).toEqual([0, 3]);
    expect(updates.map((update) => update?.newOutput)).toEqual(["abc", "de"]);
    stream.close();
  });

  test("an unknown work id closes the stream at the transport", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const bash = openStream((options) =>
      shim.clients.h1.watchBash(
        create(shimv1.WatchBashRequestSchema, { work: workId("nobody") }),
        options,
      ),
    );

    await expect(bash.next()).rejects.toThrow();
  });
});

describe("StopBash", () => {
  test("a live run is stopped, the vendor's stopTask fires, and the stream concludes interrupted", async () => {
    // A process's only input is stop, and the spool's `EXIT=143` is the vendor's
    // own evidence that the stop reached the shell rather than only the record.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    const announced = await awaitAnnouncement(stream);
    const run = announced.work?.value ?? "";

    const response = await shim.clients.h1.stopBash(
      create(shimv1.StopBashRequestSchema, { work: workId(run) }),
    );
    await awaitSpoolExit(shim.dirs, started.vendorSessionId, 143);

    stopBashAccepted(response);
    stream.close();
  });

  test("the stopped run's own stream concludes with an interrupted arm", async () => {
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    const announced = await awaitAnnouncement(stream);
    const run = announced.work?.value ?? "";
    const bash = openStream((options) =>
      shim.clients.h1.watchBash(
        create(shimv1.WatchBashRequestSchema, { work: workId(run) }),
        options,
      ),
    );
    await bash.next();

    await shim.clients.h1.stopBash(create(shimv1.StopBashRequestSchema, { work: workId(run) }));
    const frames = await bash.drain();

    const terminal = frames.map(bashFrame).at(-1);
    expect(terminal?.result.case).toBe("success");
    if (terminal?.result.case === "success") {
      expect(terminal.result.value.outcome.case).toBe("interrupted");
      if (terminal.result.value.outcome.case === "interrupted") {
        expect(terminal.result.value.outcome.value.cause.case).toBe("byUser");
      }
    }
    expect(started.vendorSessionId).not.toBe("");
    stream.close();
  });

  test("an unknown work id is refused unknown_work", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.stopBash(
      create(shimv1.StopBashRequestSchema, { work: workId("nobody") }),
    );

    expect(stopBashKind(response)).toBe("unknownWork");
  });

  test("a run that already ended is refused already_ended", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach" }));
    const announced = await awaitAnnouncement(stream);
    const run = announced.work?.value ?? "";
    // Drive the run to its own terminal before asking to stop it.
    const bash = openStream((options) =>
      shim.clients.h1.watchBash(
        create(shimv1.WatchBashRequestSchema, { work: workId(run) }),
        options,
      ),
    );
    await bash.drain();

    const response = await shim.clients.h1.stopBash(
      create(shimv1.StopBashRequestSchema, { work: workId(run) }),
    );

    expect(stopBashKind(response)).toBe("alreadyEnded");
    stream.close();
  });
});

describe("subagents", () => {
  test("a synchronous subagent's frames arrive FLAT, carrying its own AgentId", async () => {
    // FRAMES ARE FLAT: `AgentFrame.agent_id` is the whole of attribution and a
    // subagent's work is never nested in its spawn.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent" }));
    const spawned = await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return (
        update.case === "activity" &&
        update.value.item.case === "subagent" &&
        update.value.item.value.result.case === "start"
      );
    });
    const spawnFrame = entryFrame(watchAgentEntry(spawned));
    let created = "";
    if (spawnFrame?.result.case === "update") {
      const update = spawnFrame.result.value.update;
      if (update.case === "activity" && update.value.item.case === "subagent") {
        const subagent = update.value.item.value;
        if (subagent.result.case === "start") {
          created = subagent.result.value.createdAgentId?.value ?? "";
        }
      }
    }
    const own = await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      return entryFrame(watchAgentEntry(frame))?.agentId?.value === created;
    });

    expect(created).not.toBe("");
    expect(created).not.toBe(started.vendorSessionId);
    expect(entryFrame(watchAgentEntry(own))?.agentId?.value).toBe(created);
    stream.close();
  });

  test("the subagent's AgentId IS the spawning call's activity id", async () => {
    // The one id both the stream and `meta.json` carry, which is what lets the
    // two planes agree without a mapping table.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent" }));
    const spawned = await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return (
        update.case === "activity" &&
        update.value.item.case === "subagent" &&
        update.value.item.value.result.case === "start"
      );
    });
    const spawnFrame = entryFrame(watchAgentEntry(spawned));
    if (spawnFrame?.result.case !== "update") throw new Error("expected the spawn frame");
    const update = spawnFrame.result.value.update;
    if (update.case !== "activity" || update.value.item.case !== "subagent") {
      throw new Error("expected a subagent activity");
    }
    const subagent = update.value.item.value;
    if (subagent.result.case !== "start") throw new Error("expected the subagent's start");

    expect(subagent.result.value.createdAgentId?.value).toBe(update.value.activityId?.value);
    // And the FILE plane agrees — BY THE JOIN, not by the name. The vendor names
    // its subagent files by its own 17-hex agentId and `meta.toolUseId` is the
    // only link back to the spawning call, so the reader joins on that rather
    // than guessing a file name from the wire identity.
    const meta = findSubagentMetaByToolUseId(
      shim.dirs,
      started.vendorSessionId,
      subagent.result.value.createdAgentId?.value ?? "",
    );
    expect(Object.keys(meta).sort()).toEqual(
      ["agentType", "description", "spawnDepth", "toolUseId"].sort(),
    );
    stream.close();
  });

  test("a synchronous subagent's unit settles with its report", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent" }));
    const settled = await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return (
        update.case === "activity" &&
        update.value.item.case === "subagent" &&
        update.value.item.value.result.case === "success"
      );
    });

    const agentFrame = entryFrame(watchAgentEntry(settled));
    if (agentFrame?.result.case !== "update") throw new Error("expected the settled frame");
    const update = agentFrame.result.value.update;
    if (update.case !== "activity" || update.value.item.case !== "subagent") {
      throw new Error("expected a subagent activity");
    }
    const subagent = update.value.item.value;
    if (subagent.result.case !== "success") throw new Error("expected the success arm");
    expect(subagent.result.value.report).toBeDefined();
    expect(subagent.result.value.totals).toBeDefined();
    stream.close();
  });

  test("!subagent-detached announces detached work for the subagent", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!subagent-detached" }),
    );
    const announced = await awaitAnnouncement(stream);

    expect(announced.work?.value).not.toBe("");
    expect(announced.origin.case).toBe("detached");
    stream.close();
  });

  test("WatchAgent(created_agent_id) opens with that agent's own page and tails its frames", async () => {
    // ONE API whether the agent is the main thread or a subagent: the created
    // agent id is the key a consumer draws a container under.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(
      startTurnRequest({ turn: "t1", text: "!subagent-detached" }),
    );
    const announced = await awaitAnnouncement(stream);
    const created = announced.work?.value ?? "";

    const child = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest({ target: agentId(created) }), options),
    );
    const page = watchAgentPage(await child.next());
    const tailed = await child.until((frame) => frame.frame.case === "entry");

    expect(page.boundary.case).not.toBeUndefined();
    expect(entryFrame(watchAgentEntry(tailed))?.agentId?.value).toBe(created);
    stream.close();
    child.close();
  });
});

describe("DetachForeground", () => {
  test("a vendor-backgrounded unit is CONFIRMED and announced", async () => {
    // Ctrl-B cannot be INITIATED on the pinned SDK: `!ctrl-b` is the VENDOR
    // performing the detachment (`backgroundTasks(toolUseId)` marks the call,
    // and the foreground result reports `backgroundedByUser`), and
    // DetachForeground confirms it so the consumer's obligation is stated by the
    // same verb either way.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!ctrl-b" }));
    const running = await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return (
        update.case === "activity" &&
        update.value.item.case === "bash" &&
        update.value.item.value.result.case === "start"
      );
    });
    const runningFrame = entryFrame(watchAgentEntry(running));
    let unit = "";
    if (runningFrame?.result.case === "update") {
      const update = runningFrame.result.value.update;
      if (update.case === "activity") unit = update.value.activityId?.value ?? "";
    }

    const response = await shim.clients.h1.detachForeground(
      create(shimv1.DetachForegroundRequestSchema, { unit: activityId(unit) }),
    );
    const announced = await awaitAnnouncement(stream);

    detachForegroundAccepted(response);
    expect(announced.work?.value).toBe(unit);
    if (announced.origin.case === "detached") {
      expect(announced.origin.value.cause.case).toBe("byUser");
    }
    stream.close();
  });

  test("a live unit the SDK cannot detach is refused unsupported", async () => {
    // `unsupported` and NOT `not_detachable`: the unit is perfectly
    // detachable-in-kind, and the pinned SDK simply offers no verb to initiate
    // it — the wrong arm would have lied about the reason.
    //
    // `!bash-hold` RATHER THAN `!bash`: the refusal under test is only reachable
    // while the unit is genuinely live, and `!bash` settles in the same tick it
    // starts, so the call raced the settle and the table answered
    // `already_concluded` instead. `!bash-hold` parks the foreground call until
    // an interrupt, with no background work for it anywhere.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-hold" }));
    const running = await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return (
        update.case === "activity" &&
        update.value.item.case === "bash" &&
        update.value.item.value.result.case === "start"
      );
    });
    const runningFrame = entryFrame(watchAgentEntry(running));
    let unit = "";
    if (runningFrame?.result.case === "update") {
      const update = runningFrame.result.value.update;
      if (update.case === "activity") unit = update.value.activityId?.value ?? "";
    }

    const response = await shim.clients.h1.detachForeground(
      create(shimv1.DetachForegroundRequestSchema, { unit: activityId(unit) }),
    );

    expect(detachForegroundKind(response)).toBe("unsupported");
    stream.close();
  });

  test("a settled unit is refused already_concluded", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!read" }));
    const settled = await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return (
        update.case === "activity" &&
        update.value.item.case === "read" &&
        update.value.item.value.result.case === "success"
      );
    });
    const settledFrame = entryFrame(watchAgentEntry(settled));
    let unit = "";
    if (settledFrame?.result.case === "update") {
      const update = settledFrame.result.value.update;
      if (update.case === "activity") unit = update.value.activityId?.value ?? "";
    }

    const response = await shim.clients.h1.detachForeground(
      create(shimv1.DetachForegroundRequestSchema, { unit: activityId(unit) }),
    );

    expect(detachForegroundKind(response)).toBe("alreadyConcluded");
    stream.close();
  });

  test("an unknown unit is refused unknown_unit", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const response = await shim.clients.h1.detachForeground(
      create(shimv1.DetachForegroundRequestSchema, { unit: activityId("nobody") }),
    );

    expect(detachForegroundKind(response)).toBe("unknownUnit");
  });

  test("a unit of a non-detachable kind is refused not_detachable", async () => {
    // A Read is not detachable IN KIND — the four detachable kinds are subagent,
    // bash, workflow and monitor — which is the distinction `unsupported` does
    // not make.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!hold" }));
    // The held turn's own response unit is live and of a kind nothing can
    // detach.
    const live = await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return (
        update.case === "activity" &&
        (update.value.item.case === "response" || update.value.item.case === "thinking")
      );
    });
    const liveFrame = entryFrame(watchAgentEntry(live));
    let unit = "";
    if (liveFrame?.result.case === "update") {
      const update = liveFrame.result.value.update;
      if (update.case === "activity") unit = update.value.activityId?.value ?? "";
    }

    const response = await shim.clients.h1.detachForeground(
      create(shimv1.DetachForegroundRequestSchema, { unit: activityId(unit) }),
    );

    expect(detachForegroundKind(response)).toBe("notDetachable");
    stream.close();
  });
});

describe("reconciliation at session start", () => {
  test("a store-only unterminated item is re-adopted with the CREATED origin", async () => {
    // `SessionStarted.live_work` announces what the revived vendor process
    // actually has, and it announces it as `created` — the item's start plus its
    // description — because a resume is not a detachment: nothing left a turn,
    // the work simply already exists.
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(first);
    await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    const announced = await awaitAnnouncement(stream);
    const run = announced.work?.value ?? "";
    stream.close();
    await first.clients.h1.killSession(
      create(shimv1.KillSessionRequestSchema, { force: true }),
    );
    await first.exited;

    const second = await spawnShim({ reuse: first.dirs });
    const revived = sessionStarted(
      await second.clients.h1.startSession(resumeSession(started.vendorSessionId)),
    );

    const adopted = revived.liveWork.find((item) => item.work?.value === run);
    if (adopted === undefined) {
      throw new Error(
        `the revived session did not announce ${run}; it announced ${JSON.stringify(
          revived.liveWork.map((item) => item.work?.value),
        )}`,
      );
    }
    expect(adopted.origin.case).toBe("created");
    if (adopted.origin.case === "created") {
      expect(adopted.origin.value.workCreated?.work.case).toBe("bash");
    }
  });

  test("an item the vendor did NOT keep is closed with a lost.swept_up terminal", async () => {
    // EVERY STARTED THING EVENTUALLY GETS A TERMINAL ROW, by observation or by
    // reconciliation. The dual write closes the record AND puts the stop notice
    // in the feed, so a surface never shows work that no longer exists.
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await first.clients.h1.killSession(
      create(shimv1.KillSessionRequestSchema, { force: true }),
    );
    await first.exited;
    // A run the RECORD believes is live and the vendor has never heard of.
    const store = createStoreClient(first.dirs.storeSocket);
    await seedDetachedAnnouncement(store, sidecarProducer(started.vendorSessionId), {
      work: "orphan-run",
      agent: started.vendorSessionId,
      detachedFromId: "orphan-run",
      outputPath: "/nonexistent/orphan.output",
    });

    const second = await spawnShim({ reuse: first.dirs });
    const revived = sessionStarted(
      await second.clients.h1.startSession(resumeSession(started.vendorSessionId)),
    );

    expect(revived.liveWork.map((item) => item.work?.value)).not.toContain("orphan-run");
    const bash = openStream((options) =>
      second.clients.h1.watchBash(
        create(shimv1.WatchBashRequestSchema, { work: workId("orphan-run") }),
        options,
      ),
    );
    const frames = await bash.drain();
    const terminal = frames.map(bashFrame).at(-1);
    if (terminal?.result.case === "success" && terminal.result.value.outcome.case === "interrupted") {
      const cause = terminal.result.value.outcome.value.cause;
      expect(cause.case).toBe("lost");
      if (cause.case === "lost") expect(cause.value.how.case).toBe("sweptUp");
    } else {
      throw new Error("the orphaned run was not closed with an interrupted terminal");
    }
  });

  test("GetLiveWork is called exactly ONCE, at session start", async () => {
    // It answers "what did the record see START and never see END", which is
    // timeless and cannot go stale — so asking again would be the shim
    // treating the record as a live view of the world.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    await shim.clients.h1.startTurn(turnFor({ turn: "t1", text: "!bash-detach-live" }));

    const liveWorkReads = (shim.store?.reads() ?? []).filter((read) => read.rpc === "GetLiveWork");
    expect(liveWorkReads.length).toBe(1);
  });
});

describe("a fan-wide cancel", () => {
  test("!cancel-all concludes every live item", async () => {
    // Emptying the vendor's live set is the signal, and the shim consumes that
    // LEVEL rather than diffing it or pairing edges into a retained set.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(turnFor({ turn: "t1", text: "!cancel-all" }));
    const announcements: string[] = [];
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case === "detachedWork") {
        announcements.push(agentFrame.result.value.work?.value ?? "");
      }
      return announcements.length >= 3;
    });
    const concluded = new Set<string>();
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case === "update") {
        const update = agentFrame.result.value.update;
        if (update.case === "activity") {
          const item = update.value.item;
          const settled =
            item.case !== undefined &&
            typeof item.value === "object" &&
            "result" in item.value &&
            ["success", "failure"].includes(
              (item.value as { result: { case?: string } }).result.case ?? "",
            );
          if (settled && announcements.includes(update.value.activityId?.value ?? "")) {
            concluded.add(update.value.activityId?.value ?? "");
          }
        }
      }
      return concluded.size >= announcements.length;
    });

    expect([...concluded].sort()).toEqual([...announcements].sort());
    stream.close();
  });
});
