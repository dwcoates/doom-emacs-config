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
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import { afterEach, describe, expect, test } from "vitest";
import { conversationv1, shimv1, storev1 } from "../../src/proto.js";
import { cleanupShims, spawnShim } from "../integration-support/harness.js";
import {
  activityId,
  agentId,
  freshSession,
  openSessionUpdates,
  openStream,
  resumeSession,
  startTurnRequest,
  startTurnRequest as turnFor,
  readHistoryFirst,
  turnId,
  watchAgentRequest,
  workId,
} from "../integration-support/client.js";
import {
  bashFrame,
  detachForegroundAccepted,
  detachForegroundKind,
  entryFrame,
  historyPage,
  sessionStarted,
  sessionStartedFrame,
  stopBashAccepted,
  stopBashKind,
  watchAgentEntry,
  watchAgentPage,
} from "../integration-support/expect.js";
import {
  bashCompleted,
  bashRowEntry,
  bashStart,
  bashTail,
  createStoreClient,
  seedBashLifecycle,
  seedDetachedAnnouncement,
  seedSubagentSpawn,
  sidecarProducer,
  writeEntries,
  writtenKeys,
  entriesKeyed,
} from "../integration-support/store.js";
import {
  awaitSpoolExit,
  findSubagentLocatorByToolUseId,
  findSubagentMetaByToolUseId,
  readSpools,
  readTranscript,
  spoolFilePath,
} from "../integration-support/vendor.js";

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

/** The `AgentBash` a tailed activity entry carries. */
function bashOf(frame: shimv1.WatchAgentResponse): conversationv1.AgentBash {
  const agentFrame = entryFrame(watchAgentEntry(frame));
  if (agentFrame?.result.case !== "update") throw new Error("expected an update frame");
  const update = agentFrame.result.value.update;
  if (update.case !== "activity" || update.value.item.case !== "bash") {
    throw new Error("expected a bash activity");
  }
  return update.value.item.value;
}

describe("a detached shell's announcement", () => {
  test("!bash-detach announces detached_work with a readable output path", async () => {
    // The announcement is the consumer's open-a-stream obligation, and the
    // output path is how a surface offers the file when the stream is gone.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach" }));
    const announced = await awaitAnnouncement(stream, true);

    expect(announced.work?.value).not.toBe("");
    expect(announced.output?.readability.case).toBe("readable");
    // THE PATH IS THE FILE THE VENDOR ACTUALLY WROTE, not merely a non-empty
    // string: this is the offer a surface makes when the stream is gone, so a
    // path that named nothing would be a broken offer nobody noticed. The
    // spool is found by SCANNING (the vendor's task id never crosses the wire),
    // and the announced path must be that file.
    const spools = readSpools(shim.dirs, started.vendorSessionId);
    expect(spools.length).toBeGreaterThan(0);
    expect(spools.map((spool) => spoolFilePath(shim.dirs, started.vendorSessionId, spool.taskId))).toContain(
      announced.output?.path,
    );
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
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach" }));
    const announced = await awaitAnnouncement(stream);

    if (announced.origin.case !== "detached") throw new Error("expected the detached origin");
    expect(announced.work?.value).toBe(announced.origin.value.detachedFromId?.value);
    // AND THE FILE PLANE AGREES. Both values above are the shim's own, so on
    // their own they only say the shim was consistent with itself. The vendor's
    // transcript carries the spawning call's `tool_use` block id, and THAT is
    // the value the handle must be.
    const toolUseIds = readTranscript(shim.dirs, started.vendorSessionId)
      .filter((record) => record.type === "assistant")
      .flatMap((record) => {
        const message = record.message as { content?: unknown } | undefined;
        const content = Array.isArray(message?.content) ? message.content : [];
        return content
          .filter((block): block is { type: string; id: string } => {
            const typed = block as { type?: unknown; id?: unknown };
            return typed.type === "tool_use" && typeof typed.id === "string";
          })
          .map((block) => block.id);
      });
    expect(toolUseIds).toContain(announced.work?.value);
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
  test("it opens with start carrying the ORIGINAL instant, then the tail, then the terminal", async () => {
    // Every byte of detached shell output comes from the sidecar tailing the
    // vendor's spool — foreground output is observable nowhere while running —
    // so the rows are seeded here and the shim must serve them back in order.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
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
    // ONE tail row, superseded by the second write: the replay draws the
    // newest window, which holds both chunks.
    expect(arms.filter((arm) => arm === "tail").length).toBe(1);
    const first = bashFrame(frames[0]);
    if (first.result.case === "start") {
      // A RE-ANNOUNCEMENT REPEATS THE ORIGINAL INSTANT: drawn clocks must not
      // reset when work moves streams.
      expect(first.result.value.startedAt?.atMs).toBe(BigInt(originalInstant));
    }
    stream.close();
  });

  test("the tail frames carry the rendered window, never the whole spool", async () => {
    // Output beyond what is rendered is not stored (owner ruling 2026-09-23):
    // a run's output is one snapshot superseded whole, never a delta sequence.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
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

    // The store holds ONE tail row, superseded by each write, so a replay
    // opened after both writes serves the newest window and nothing before it.
    const tails = frames
      .map(bashFrame)
      .filter((frame) => frame.result.case === "tail")
      .map((frame) => (frame.result.case === "tail" ? frame.result.value : null));
    expect(tails.map((tail) => tail?.text)).toEqual(["abcde"]);
    expect(tails.map((tail) => Number(tail?.bytesOmitted ?? -1))).toEqual([0]);
    stream.close();
  });

  test("an unknown work id is refused NOT_FOUND, naming the work", async () => {
    // INTERIM RULING (ledger, rebuild merge): `WatchBash` has no refusal arm of
    // its own, so an unknown handle is a TRANSPORT refusal — pinned rather than
    // left as "some throw", because a store outage, a cancelled call and a
    // handle nobody minted all reach a caller as an exception and only the code
    // tells them apart.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    const bash = openStream((options) =>
      shim.clients.h1.watchBash(
        create(shimv1.WatchBashRequestSchema, { work: workId("nobody") }),
        options,
      ),
    );
    const failure = await bash.nextOrEnd().then(
      () => undefined,
      (err: unknown) => ConnectError.from(err),
    );

    if (failure === undefined) throw new Error("the unknown work id was not refused");
    expect(failure.code).toBe(Code.NotFound);
    expect(failure.message).toContain("nobody");
  });
});

describe("WatchBash and the rows it relays", () => {
  test("an ANNOUNCED run opens with `start` at once, before the sidecar has written a row", async () => {
    // THE ANNOUNCEMENT IS A CONSUMER OBLIGATION: the daemon opens the stream
    // the moment it sees one, which is necessarily before the sidecar has
    // written a single row. The shim wrote the run's start ahead of the
    // announcement, so the first frame is owed immediately — never a silent
    // wait for a producer this process cannot see.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    const announced = await awaitAnnouncement(stream);
    const run = announced.work?.value ?? "";

    // Opened with NO sidecar row for this run, and none is ever seeded.
    const bash = openStream((options) =>
      shim.clients.h1.watchBash(
        create(shimv1.WatchBashRequestSchema, { work: workId(run) }),
        options,
      ),
    );
    const first = bashFrame(await bash.next());

    expect(first.result.case).toBe("start");
    bash.close();
    stream.close();
  });

  test("the sidecar's rows written after the shim's start follow it on the same stream", async () => {
    // THE PLANES SHARE ONE ROW PER FACT: the sidecar's tail and terminal land
    // on the run the shim's start opened, and are served after it.
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
    expect(bashFrame(await bash.next()).result.case).toBe("start");

    await writeEntries(createStoreClient(shim.dirs.storeSocket), sidecarProducer(started.vendorSessionId), [
      bashRowEntry({
        run,
        frame: bashTail("late\n"),
        writeId: `${run}-tail`,
        upsertKey: `bash:${run}:tail`,
        topLevel: started.vendorSessionId,
      }),
      bashRowEntry({
        run,
        frame: bashCompleted("sleep 1 && echo done", 0, "late\n"),
        writeId: `${run}-terminal`,
        upsertKey: `bash:${run}:terminal`,
        topLevel: started.vendorSessionId,
      }),
    ]);
    const arms = (await bash.drain()).map((frame) => bashFrame(frame).result.case);

    expect(arms).toEqual(["start", "tail", "success"]);
    stream.close();
  });

  test("it FOLLOWS: tails and the terminal seeded after the open still arrive, in order", async () => {
    // The same obligation stated as an ordering: the stream is opened first and
    // the rows are written afterwards, so nothing here can be served from a
    // snapshot taken at the open.
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
    await seedBashLifecycle(
      createStoreClient(shim.dirs.storeSocket),
      sidecarProducer(started.vendorSessionId),
      {
        run,
        work: run,
        command: "echo",
        startedAtMs: 1_700_000_000_000,
        chunks: ["one\n", "two\n"],
        exitCode: 0,
        topLevel: started.vendorSessionId,
      },
    );
    const frames = await bash.drain();

    // A tail SUPERSEDES the one before it, so whether the watcher met the
    // first window live or only the newest on its replay depends on when its
    // open was answered; what is owed either way is the order, and the newest
    // window holding every chunk.
    const arms = frames.map((frame) => bashFrame(frame).result.case);
    expect(arms.filter((arm, index) => arm !== "tail" || arms[index - 1] !== "tail")).toEqual([
      "start",
      "tail",
      "success",
    ]);
    const tails = frames
      .map(bashFrame)
      .filter((frame) => frame.result.case === "tail")
      .map((frame) => (frame.result.case === "tail" ? frame.result.value.text : ""));
    expect(tails.at(-1)).toBe("one\ntwo\n");
    stream.close();
  });

  test("the terminal is served ONCE and ENDS the run's stream; a later tail is not served", async () => {
    // A RUN'S STREAM ENDS AT ITS TERMINAL. The terminal is served exactly once
    // and is the last thing on the stream — a second would have a consumer draw
    // the run finishing twice, and anything after it would arrive on a stream
    // the consumer has already closed.
    //
    // THE OBLIGATION THIS PUTS ON THE SIDECAR: a last chunk must be written
    // BEFORE the terminal row, never after it. A tail written afterwards is
    // durable in the record and reaches no watcher, which is asserted here so
    // the ordering is a stated rule rather than a surprise.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach" }));
    const announced = await awaitAnnouncement(stream);
    const run = announced.work?.value ?? "";
    const store = createStoreClient(shim.dirs.storeSocket);
    const producer = sidecarProducer(started.vendorSessionId);
    // The terminal FIRST...
    await writeEntries(store, producer, [
      bashRowEntry({
        run,
        frame: bashStart("echo", 1_700_000_000_000),
        writeId: `${run}-start`,
        topLevel: started.vendorSessionId,
      }),
      bashRowEntry({
        run,
        frame: bashCompleted("echo", 0, "early\n"),
        writeId: `${run}-terminal`,
        upsertKey: `bash:${run}:terminal`,
        topLevel: started.vendorSessionId,
      }),
    ]);
    // ...and a tail after it.
    await writeEntries(store, producer, [
      bashRowEntry({
        run,
        frame: bashTail("early\ntrailing\n"),
        writeId: `${run}-late`,
        upsertKey: `bash:${run}:tail`,
        topLevel: started.vendorSessionId,
      }),
    ]);

    const bash = openStream((options) =>
      shim.clients.h1.watchBash(
        create(shimv1.WatchBashRequestSchema, { work: workId(run) }),
        options,
      ),
    );
    const frames = await bash.drain();

    const arms = frames.map((frame) => bashFrame(frame).result.case);
    expect(arms.filter((arm) => arm === "success").length).toBe(1);
    expect(arms.at(-1)).toBe("success");
    expect(arms).not.toContain("tail");
    expect(bash.isEnded()).toBe(true);
    stream.close();
  });
});

describe("what the PRODUCER states about a shell run", () => {
  test("!bash-fail: a NON-ZERO exit is a completed run carrying the code", async () => {
    // A command's own verdict on itself is not a failure of the CALL. Drawing
    // `exit 3` as a failed tool would tell the user the shell broke, when what
    // happened is that a grep found nothing.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-fail" }));
    const settled = await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return (
        update.case === "activity" &&
        update.value.item.case === "bash" &&
        update.value.item.value.result.case !== "start"
      );
    });

    const bash = bashOf(settled);
    if (bash.result.case !== "success") {
      throw new Error("a non-zero exit was drawn as something other than a completed run");
    }
    const outcome = bash.result.value.outcome;
    if (outcome.case !== "completed") throw new Error("the run did not settle completed");
    expect(outcome.value.termination?.how.case).toBe("exited");
    if (outcome.value.termination?.how.case === "exited") {
      expect(outcome.value.termination.how.value.code).toBe(3);
    }
    stream.close();
  });

  test("!bash-timeout: the run MOVES rather than ends, and the receipt settles nothing", async () => {
    // The vendor AUTO-BACKGROUNDS a timed-out command instead of killing it, so
    // the foreground receipt is not a terminal: the run is still going, and its
    // detached-work frame is what settles it. A receipt drawn as a terminal
    // here would show the user a finished command that is still running.
    //
    // `AgentBashInterruptedByTimeout.timeout_ms` — the CONFIGURED limit, not
    // the runtime — is asserted where its producer lives, on the converter, in
    // test/convert/tools/bash.test.ts: reaching it end to end needs the
    // sidecar's rows, which no integration harness runs.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-timeout" }));
    const announced = await awaitAnnouncement(stream);
    // Drive to the turn's end so every frame it will produce has been served.
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      return agentFrame?.result.case === "success" || agentFrame?.result.case === "failure";
    });

    expect(announced.origin.case).toBe("detached");
    const settledReceipts = stream.frames().filter((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return (
        update.case === "activity" &&
        update.value.item.case === "bash" &&
        update.value.item.value.result.case !== "start"
      );
    });
    expect(settledReceipts).toEqual([]);
    stream.close();
  });

  test("termination is SET for a detached shell and UNSET for a foreground one", async () => {
    // THE FACT EXISTS FOR EXACTLY ONE OF THE TWO PATHS. A detached shell's
    // spool is terminated by the shell itself, so the sidecar reads an `EXIT=`
    // line and states how it ended; a foreground call's result carries the
    // output and no shell status at all. Stating it as absent for the
    // foreground path is the honest shape — a synthesized `exited(0)` would be
    // the shim inventing a status nothing reported.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash" }));
    const foreground = await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return (
        update.case === "activity" &&
        update.value.item.case === "bash" &&
        update.value.item.value.result.case === "success" &&
        update.value.item.value.result.value.outcome.case === "completed"
      );
    });
    const foregroundBash = bashOf(foreground);
    if (
      foregroundBash.result.case !== "success" ||
      foregroundBash.result.value.outcome.case !== "completed"
    ) {
      throw new Error("the foreground run did not settle completed");
    }

    expect(foregroundBash.result.value.outcome.value.termination).toBeUndefined();

    // The detached path, whose rows are the SIDECAR's and are seeded here as
    // everywhere else in this file.
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "!bash-detach-live" }));
    const announced = await awaitAnnouncement(stream);
    const run = announced.work?.value ?? "";
    await seedBashLifecycle(
      createStoreClient(shim.dirs.storeSocket),
      sidecarProducer(started.vendorSessionId),
      {
        run,
        work: run,
        command: "echo done",
        startedAtMs: 1_700_000_000_000,
        chunks: ["done\n"],
        exitCode: 0,
        topLevel: started.vendorSessionId,
      },
    );
    const watch = openStream((options) =>
      shim.clients.h1.watchBash(
        create(shimv1.WatchBashRequestSchema, { work: workId(run) }),
        options,
      ),
    );
    const terminal = (await watch.drain()).map(bashFrame).at(-1);

    if (terminal?.result.case !== "success" || terminal.result.value.outcome.case !== "completed") {
      throw new Error("the detached run did not settle completed");
    }
    expect(terminal.result.value.outcome.value.termination?.how.case).toBe("exited");
    stream.close();
  });

});

// ---------------------------------------------------------------------------
// A DETACHED SHELL'S LIFECYCLE (owner ruling, 2026-09-29): the shim writes the
// run's START (a shell the sidecar never tailed, task bfa5s1wjd, still opens)
// and retires it from its live set at the vendor's notification; the run's
// TERMINAL is the sidecar's alone, from the spool. No sidecar runs in this
// harness, so the sidecar's terminal, where a test needs one, is seeded.
// ---------------------------------------------------------------------------

describe("a detached shell's lifecycle: the shim writes its start, the sidecar its end", () => {
  /** A detach gate: a path the mock vendor's detached work parks on until it exists. */
  function detachGate(): { readonly path: string; release(): void; dispose(): void } {
    const dir = mkdtempSync(join(tmpdir(), "shim-detach-gate-"));
    const path = join(dir, "release");
    return {
      path,
      release: () => writeFileSync(path, ""),
      dispose: () => rmSync(dir, { recursive: true, force: true }),
    };
  }

  /** The live membership a NEW WatchSession is re-announced, by handle. */
  async function reannouncedLiveWork(shim: Awaited<ReturnType<typeof spawnShim>>): Promise<string[]> {
    const watch = openStream((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    await watch.next();
    const started = sessionStartedFrame(await watch.next());
    watch.close();
    return started.liveWork.map((item) => item.work?.value ?? "");
  }

  /**
   * Resolves once the shim has processed RUN's task notification: the
   * notification restates the run's `detached:` row under a write identity of
   * its own, after the announcement's.
   */
  async function concluded(shim: Awaited<ReturnType<typeof spawnShim>>, run: string): Promise<void> {
    const key = `detached:${run}`;
    const announced = await shim.store?.entryLanded((entry) => entry.upsertKey === key);
    await shim.store?.entryLanded((entry) => entry.upsertKey === key && entry.writeId !== announced?.writeId);
  }

  /** Announce a `!bash-detach` run and wait for the shim to conclude it. */
  async function concludedRun(shim: Awaited<ReturnType<typeof spawnShim>>): Promise<string> {
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach" }));
    const run = (await awaitAnnouncement(stream)).work?.value ?? "";
    stream.close();
    await concluded(shim, run);
    return run;
  }

  /** Every terminal row the SHIM wrote for RUN. */
  function shimTerminals(shim: Awaited<ReturnType<typeof spawnShim>>, run: string): storev1.StoreEntry[] {
    return entriesKeyed(shim.store?.writes() ?? [], `bash:${run}:terminal`);
  }

  /** The sidecar's terminal for RUN, read off the spool's `EXIT=0`, seeded. */
  async function sidecarEnds(
    shim: Awaited<ReturnType<typeof spawnShim>>,
    vendorSessionId: string,
    run: string,
  ): Promise<void> {
    await writeEntries(createStoreClient(shim.dirs.storeSocket), sidecarProducer(vendorSessionId), [
      bashRowEntry({
        run,
        frame: bashCompleted("echo done", 0, "done\n"),
        writeId: `${run}-sidecar-terminal`,
        upsertKey: `bash:${run}:terminal`,
        topLevel: vendorSessionId,
      }),
    ]);
  }

  test("a concluded run the sidecar never tailed still has its start", async () => {
    // THE INCIDENT: nothing but the shim ever wrote a row for this run.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const run = await concludedRun(shim);

    // Act.
    const bash = openStream((options) =>
      shim.clients.h1.watchBash(create(shimv1.WatchBashRequestSchema, { work: workId(run) }), options),
    );
    const first = bashFrame(await bash.next());
    bash.close();

    // Assert.
    expect(first.result.case).toBe("start");
  });

  test("the shim writes NO terminal for a concluded run: its end is the sidecar's", async () => {
    // Arrange.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());

    // Act.
    const run = await concludedRun(shim);

    // Assert.
    expect(shimTerminals(shim, run)).toEqual([]);
  });

  test("the sidecar's terminal ENDS a WatchBash opened while the run was live", async () => {
    // Arrange: the run parks on its gate, so the watch is opened on a LIVE run.
    const gate = detachGate();
    try {
      const shim = await spawnShim({ env: { AGENT_REPL_FAKE_DETACH_GATE: gate.path } });
      const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
      const stream = await openAgentStream(shim);
      await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach" }));
      const run = (await awaitAnnouncement(stream)).work?.value ?? "";
      const bash = openStream((options) =>
        shim.clients.h1.watchBash(create(shimv1.WatchBashRequestSchema, { work: workId(run) }), options),
      );
      await bash.next();
      gate.release();
      await concluded(shim, run);

      // Act.
      await sidecarEnds(shim, started.vendorSessionId, run);
      const frames = await bash.drain();

      // Assert.
      expect(frames.map((frame) => bashFrame(frame).result.case).at(-1)).toBe("success");
      stream.close();
    } finally {
      gate.dispose();
    }
  });

  test("a concluded run stays live work until the sidecar writes its end", async () => {
    // Arrange: the vendor notified, and no sidecar has read the spool yet.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const run = await concludedRun(shim);

    // Act.
    const live = await reannouncedLiveWork(shim);

    // Assert.
    expect(live).toContain(run);
  });

  test("a run the sidecar ended is no longer re-announced as live work", async () => {
    // Arrange.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const run = await concludedRun(shim);
    await sidecarEnds(shim, started.vendorSessionId, run);

    // Act.
    const live = await reannouncedLiveWork(shim);

    // Assert.
    expect(live).not.toContain(run);
  });

  test("a sidecar row landing AFTER the run's terminal does not reopen the run", async () => {
    // Arrange: the run is ended, then the sidecar's tail lands late.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const run = await concludedRun(shim);
    await sidecarEnds(shim, started.vendorSessionId, run);
    await writeEntries(createStoreClient(shim.dirs.storeSocket), sidecarProducer(started.vendorSessionId), [
      bashRowEntry({
        run,
        frame: bashTail("late\n"),
        writeId: `${run}-late-tail`,
        upsertKey: `bash:${run}:tail`,
        topLevel: started.vendorSessionId,
      }),
    ]);

    // Act.
    const live = await reannouncedLiveWork(shim);

    // Assert.
    expect(live).not.toContain(run);
  });

  test("the run's stream is its start, then the sidecar's ONE terminal", async () => {
    // Arrange.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const run = await concludedRun(shim);
    await sidecarEnds(shim, started.vendorSessionId, run);

    // Act.
    const bash = openStream((options) =>
      shim.clients.h1.watchBash(create(shimv1.WatchBashRequestSchema, { work: workId(run) }), options),
    );
    const arms = (await bash.drain()).map((frame) => bashFrame(frame).result.case);

    // Assert.
    expect(arms).toEqual(["start", "success"]);
  });

  test("the sidecar's terminal carries the spool's evidence", async () => {
    // Arrange.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const run = await concludedRun(shim);
    await sidecarEnds(shim, started.vendorSessionId, run);

    // Act.
    const bash = openStream((options) =>
      shim.clients.h1.watchBash(create(shimv1.WatchBashRequestSchema, { work: workId(run) }), options),
    );
    const terminal = (await bash.drain()).map(bashFrame).at(-1);

    // Assert.
    const outcome = terminal?.result.case === "success" ? terminal.result.value.outcome : undefined;
    expect(outcome?.case === "completed" ? outcome.value.termination?.how.case : undefined).toBe("exited");
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
    // EVERY BYTE OF A DETACHED SHELL COMES FROM THE SIDECAR, which no
    // integration harness runs — so the run's rows are seeded here exactly as
    // the WatchBash tests above seed theirs. `exitCode: null` leaves the run
    // UNTERMINATED, which is what gives the stop something live to conclude.
    await seedBashLifecycle(
      createStoreClient(shim.dirs.storeSocket),
      sidecarProducer(started.vendorSessionId),
      {
        run,
        work: run,
        command: "sleep 600",
        startedAtMs: 1_700_000_000_000,
        chunks: ["running\n"],
        exitCode: null,
        topLevel: started.vendorSessionId,
      },
    );
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
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach" }));
    const announced = await awaitAnnouncement(stream);
    const run = announced.work?.value ?? "";
    // The sidecar's rows, seeded as everywhere else in this file — and this
    // seed CARRIES A TERMINAL, so the drain below reaches the run's own end.
    await seedBashLifecycle(
      createStoreClient(shim.dirs.storeSocket),
      sidecarProducer(started.vendorSessionId),
      {
        run,
        work: run,
        command: "echo done",
        startedAtMs: 1_700_000_000_000,
        chunks: ["done\n"],
        exitCode: 0,
        topLevel: started.vendorSessionId,
      },
    );
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
    expect(created).not.toBe("");
    expect(created).not.toBe(started.vendorSessionId);

    // FLAT MEANS FLAT. The subagent's own frames are never nested in the
    // spawn's stream -- `AgentFrame.agent_id` is the whole of attribution, and
    // `created_agent_id` is the key a consumer opens a SECOND WatchAgent on.
    // The daemon owns that fan-out: one WatchAgent per created_agent_id it
    // learns from a spawn, never a child fan-out inside the parent's stream.
    const child = openStream((options) =>
      shim.clients.h1.watchAgent(watchAgentRequest({ target: agentId(created) }), options),
    );
    // THE CHILD'S FRAMES ARE ON ITS PAGE OR ITS TAIL, whichever the writer's
    // pace put them on: the store writer lands a backlog in merged batches, so
    // a synchronous subagent's rows can all be durable before this watch opens.
    const opening = watchAgentPage(await child.next());
    const own =
      opening.entries.length > 0
        ? opening.entries[0]
        : watchAgentEntry(await child.until((frame) => frame.frame.case === "entry"));

    expect(entryFrame(own)?.agentId?.value).toBe(created);
    // AND NOTHING OF THE CHILD'S IS ON THE PARENT'S STREAM: every frame the
    // spawning agent's book served names the spawning agent.
    for (const frame of stream.frames()) {
      if (frame.frame.case !== "entry") continue;
      expect(entryFrame(watchAgentEntry(frame))?.agentId?.value).not.toBe(created);
    }
    // ONE WATCHAGENT SERVES ONE BOOK — the PAGE as well as the tail. A repaint
    // that folded the child's rows into the parent's page would put a
    // subagent's work in the main conversation on every reload, which the tail
    // assertion above cannot catch.
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      return agentFrame?.result.case === "success" || agentFrame?.result.case === "failure";
    });
    stream.close();
    child.close();
    const parentPage = historyPage(await shim.clients.h1.readHistory(readHistoryFirst()));
    expect(
      parentPage.entries.map((entry) => entryFrame(entry)?.agentId?.value ?? ""),
    ).not.toContain(created);
    // And the child's OWN book is where they are.
    const childPage = historyPage(
      await shim.clients.h1.readHistory(readHistoryFirst({ target: agentId(created) })),
    );
    const childAgents = new Set(
      childPage.entries.map((entry) => entryFrame(entry)?.agentId?.value ?? ""),
    );
    expect([...childAgents]).toEqual([created]);
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

  test("WatchAgent(created_agent_id) opens with that agent's own page, serving its own frames", async () => {
    // ONE API whether the agent is the main thread or a subagent: the created
    // agent id is the key a consumer draws a container under.
    //
    // ITS FRAMES ARE ON THE PAGE OR THE TAIL, whichever the writer's pace put
    // them on. This used to wait for a TAIL entry, which only held because the
    // writer once spent a store round trip per vendor message; it lands a
    // backlog in merged batches now, so the agent's rows can all be durable
    // before this watch opens. The tail itself is proven by the main-book and
    // reader suites with rows written after the open.
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
    const own =
      page.entries.length > 0
        ? page.entries[0]
        : watchAgentEntry(await child.until((frame) => frame.frame.case === "entry"));

    expect(page.boundary.case).not.toBeUndefined();
    expect(entryFrame(own)?.agentId?.value).toBe(created);
    stream.close();
    child.close();
  });
});

describe("a subagent resumed by SendMessage after the shim restarted", () => {
  /** The agent a subagent announcement names. */
  const announcedAgent = (work: conversationv1.AgentDetachedWork): string =>
    work.kind?.kind.case === "subagent" ? (work.kind.kind.value.agentId?.value ?? "") : "";

  /**
   * Spawn a background subagent in one shim, stop that shim, and hand the
   * store the pairing the SIDECAR reads from the vendor's own files — the
   * locator from the meta file's name, the agent from its spawning call.
   */
  async function spawnedThenRestarted(): Promise<{
    first: Awaited<ReturnType<typeof spawnShim>>;
    second: Awaited<ReturnType<typeof spawnShim>>;
    spawned: string;
    locator: string;
  }> {
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(first);
    await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-detached" }));
    const spawned = announcedAgent(await awaitAnnouncement(stream));
    stream.close();
    await first.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await first.exited;
    const locator = findSubagentLocatorByToolUseId(first.dirs, started.vendorSessionId, spawned);
    const store = createStoreClient(first.dirs.storeSocket);
    const written = await store.writeBatch(
      create(storev1.WriteBatchRequestSchema, {
        producer: sidecarProducer(started.vendorSessionId),
        writeClass: create(storev1.WriteClassSchema, {
          writeClass: { case: "bulk", value: create(storev1.WriteClassBulkSchema, {}) },
        }),
        batch: create(storev1.EntryBatchSchema, {
          agentLocators: [
            create(storev1.AgentLocatorSchema, {
              vendorTaskId: locator,
              agent: create(conversationv1.AgentIdSchema, { value: spawned }),
            }),
          ],
        }),
      }),
    );
    if (written.result.case !== "success") throw new Error("the store refused the sidecar's pairing");
    const second = await spawnShim({ reuse: first.dirs });
    sessionStarted(await second.clients.h1.startSession(resumeSession(started.vendorSessionId)));
    return { first, second, spawned, locator };
  }

  test("the resume is announced with the agent its spawn created, named by the store", async () => {
    // Arrange.
    const { second, spawned, locator } = await spawnedThenRestarted();
    const stream = await openAgentStream(second);

    // Act.
    await second.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: `!subagent-resumed ${locator}` }));
    const frame = await stream.until((f) => {
      if (f.frame.case !== "entry") return false;
      const result = entryFrame(watchAgentEntry(f))?.result;
      return result?.case === "detachedWork" && result.value.work?.value !== spawned;
    });
    stream.close();

    // Assert.
    const result = entryFrame(watchAgentEntry(frame))?.result;
    expect(result?.case === "detachedWork" ? announcedAgent(result.value) : "").toBe(spawned);
  });

  test("the restarted shim asks the store for the locator the resume names", async () => {
    // Arrange: the store outlives the shim, as the launchd service does.
    const { first, second, locator } = await spawnedThenRestarted();
    const stream = await openAgentStream(second);

    // Act.
    await second.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: `!subagent-resumed ${locator}` }));
    await awaitAnnouncement(stream);
    stream.close();

    // Assert.
    const asked = (first.store?.reads() ?? [])
      .filter((read) => read.rpc === "GetAgentByVendorTask")
      .map((read) => (read.request as storev1.GetAgentByVendorTaskRequest).vendorTaskId);
    expect(asked).toEqual([locator]);
  });
});

/**
 * A SUBAGENT A NETWORK OUTAGE CUT OFF (owner ruling 2026-09-27;
 * engine/network-resume.ts), end to end through the built shim and the mocked
 * vendor. The outage is the reachability GATE: absent, the API is down;
 * written, it is back. The probe beat and the window are the `--fake`-only
 * overrides, so nothing here rides the production five seconds or thirty
 * minutes, and nothing touches a network.
 */
describe("a subagent a network outage cut off", () => {
  const NETWORK_RESUME_OP = "shim.engine.network_resume";
  /** Every gate directory a test made, removed after it. */
  const gateDirs: string[] = [];
  afterEach(() => {
    for (const dir of gateDirs.splice(0)) rmSync(dir, { recursive: true, force: true });
  });

  /** A shim whose API is down until the returned gate is written. */
  async function outageShim(windowMs = 60_000): Promise<{ shim: Awaited<ReturnType<typeof spawnShim>>; gate: string }> {
    const dir = mkdtempSync(join(tmpdir(), "shim-api-gate-"));
    gateDirs.push(dir);
    const gate = join(dir, "reachable");
    const shim = await spawnShim({
      env: {
        AGENT_REPL_FAKE_API_REACHABLE_GATE: gate,
        AGENT_REPL_FAKE_NETWORK_RESUME_INTERVAL_MS: "20",
        AGENT_REPL_FAKE_NETWORK_RESUME_WINDOW_MS: String(windowMs),
      },
    });
    await shim.clients.h1.startSession(freshSession());
    return { shim, gate };
  }

  /** The network-resume record whose outcome is `outcome`. */
  function outcome(value: string): (record: { operation?: unknown; context: Record<string, unknown> }) => boolean {
    return (record) => record.operation === NETWORK_RESUME_OP && record.context.outcome === value;
  }

  test("waits while the API is unreachable", async () => {
    // Arrange
    const { shim } = await outageShim();

    // Act
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-network-failed" }));

    // Assert
    const waiting = await shim.log.record(outcome("waiting"));
    expect(waiting.level).toBe("info");
  });

  test("resumes the SAME agent once the API is reachable again", async () => {
    // Arrange
    const { shim, gate } = await outageShim();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-network-failed" }));
    const waiting = await shim.log.record(outcome("waiting"));

    // Act
    writeFileSync(gate, "");

    // Assert
    const resumed = await shim.log.record(outcome("resumed"));
    expect(resumed.context.task_id).toBe(waiting.context.task_id);
  });

  test("the resumed agent runs to completion", async () => {
    // Arrange
    const { shim, gate } = await outageShim();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-network-failed" }));
    const waiting = await shim.log.record(outcome("waiting"));

    // Act
    writeFileSync(gate, "");

    // Assert
    const ended = await shim.log.record(
      (record) =>
        record.operation === NETWORK_RESUME_OP &&
        record.message === "an agent resumed after a network outage ended without failing again",
    );
    expect(ended.context).toMatchObject({ task_id: waiting.context.task_id, status: "completed" });
  });

  test("the resume path records no warning and no error", async () => {
    // Arrange
    const { shim, gate } = await outageShim();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-network-failed" }));
    await shim.log.record(outcome("waiting"));

    // Act
    writeFileSync(gate, "");
    await shim.log.record(
      (record) =>
        record.operation === NETWORK_RESUME_OP &&
        record.message === "an agent resumed after a network outage ended without failing again",
    );

    // Assert
    const loud = shim.log
      .records()
      .filter((record) => record.level === "warn" || record.level === "error")
      .map((record) => `${String(record.operation)}: ${record.message}`);
    expect(loud).toEqual([]);
  });

  test("gives up at ERROR when the API stays unreachable for the whole window", async () => {
    // Arrange
    const { shim } = await outageShim(200);

    // Act
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-network-failed" }));

    // Assert
    const gaveUp = await shim.log.record(outcome("gave_up"));
    expect(gaveUp.level).toBe("error");
  });

  test("an agent that failed for another reason is not resumed", async () => {
    // Arrange
    const { shim } = await outageShim();

    // Act
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-failed" }));

    // Assert
    const notResumed = await shim.log.record(outcome("not_resumed"));
    expect(notResumed.level).toBe("info");
  });

  /** A WatchSession on `shim`, re-announcement dropped. */
  function watchSession(shim: Awaited<ReturnType<typeof spawnShim>>): ReturnType<typeof openSessionUpdates> {
    return openSessionUpdates((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
  }

  /** The session update a frame carries, if it carries one. */
  function updateOf(frame: shimv1.WatchSessionResponse): conversationv1.SessionUpdate["update"] | undefined {
    return frame.frame.case === "update" ? frame.frame.value.update : undefined;
  }

  /** The next waiting set the stream states. */
  async function nextWaits(
    watch: ReturnType<typeof openSessionUpdates>,
  ): Promise<conversationv1.SessionNetworkResumeWait[]> {
    const frame = await watch.until((candidate) => updateOf(candidate)?.case === "networkResumeWaits");
    const update = updateOf(frame);
    return update?.case === "networkResumeWaits" ? update.value.waits : [];
  }

  /** The next outcome the stream states. */
  async function nextOutcome(
    watch: ReturnType<typeof openSessionUpdates>,
  ): Promise<conversationv1.SessionNetworkResumeOutcome | undefined> {
    const frame = await watch.until((candidate) => updateOf(candidate)?.case === "networkResumeOutcome");
    const update = updateOf(frame);
    return update?.case === "networkResumeOutcome" ? update.value : undefined;
  }

  test("the session stream states the wait, its window and no resumes yet", async () => {
    // Arrange
    const { shim } = await outageShim();
    const watch = watchSession(shim);

    // Act
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-network-failed" }));

    // Assert
    const [wait, ...rest] = await nextWaits(watch);
    watch.close();
    expect([
      rest.length,
      (wait?.work?.value ?? "") !== "",
      Number((wait?.givesUpAtMs ?? 0n) - (wait?.failedAtMs ?? 0n)),
      wait?.resumesDelivered,
    ]).toEqual([0, true, 60_000, 0]);
  });

  test("a consumer that opens mid-wait is told the standing wait", async () => {
    // Arrange
    const { shim } = await outageShim();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-network-failed" }));
    await shim.log.record(outcome("waiting"));

    // Act
    const watch = watchSession(shim);

    // Assert
    const waits = await nextWaits(watch);
    watch.close();
    expect(waits).toHaveLength(1);
  });

  test("the session stream states the resume for the work that waited", async () => {
    // Arrange
    const { shim, gate } = await outageShim();
    const watch = watchSession(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-network-failed" }));
    const [wait] = await nextWaits(watch);

    // Act
    writeFileSync(gate, "");

    // Assert
    const ended = await nextOutcome(watch);
    watch.close();
    expect([ended?.work?.value, ended?.outcome.case]).toEqual([wait?.work?.value, "resumed"]);
  });

  test("the session stream states the empty set once the resume is delivered", async () => {
    // Arrange
    const { shim, gate } = await outageShim();
    const watch = watchSession(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-network-failed" }));
    await nextWaits(watch);

    // Act
    writeFileSync(gate, "");

    // Assert
    const after = await nextWaits(watch);
    watch.close();
    expect(after).toEqual([]);
  });

  test("the session stream states a give-up", async () => {
    // Arrange
    const { shim } = await outageShim(200);
    const watch = watchSession(shim);

    // Act
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-network-failed" }));

    // Assert
    const ended = await nextOutcome(watch);
    watch.close();
    expect(ended?.outcome.case).toBe("gaveUp");
  });

  test("the session stream states the wait abandoned at stand-down, then the empty set", async () => {
    // Arrange
    const { shim } = await outageShim();
    const watch = watchSession(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-network-failed" }));
    await nextWaits(watch);

    // Act
    await shim.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, {}));

    // Assert
    const stated = (await watch.drain())
      .map(updateOf)
      .flatMap((update) =>
        update?.case === "networkResumeOutcome"
          ? [update.value.outcome.case]
          : update?.case === "networkResumeWaits"
            ? [`waits:${String(update.value.waits.length)}`]
            : [],
      );
    expect(stated.slice(-2)).toEqual(["abandoned", "waits:0"]);
  });

  test("stand-down abandons the wait", async () => {
    // Arrange
    const { shim } = await outageShim();
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-network-failed" }));
    await shim.log.record(outcome("waiting"));

    // Act
    await shim.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, {}));

    // Assert
    const abandoned = await shim.log.record(outcome("abandoned"));
    expect(abandoned.level).toBe("info");
  });
});

describe("DetachForeground", () => {
  test("a vendor-backgrounded unit is CONFIRMED and announced", async () => {
    // A vendor-side detach cannot be INITIATED on the pinned SDK: `!vendor-
    // backgrounded` is the VENDOR performing the detachment
    // (`backgroundTasks(toolUseId)` marks the call, and the foreground result
    // reports `backgroundedByUser`), and DetachForeground confirms it so the
    // consumer's obligation is stated by the same verb either way.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!vendor-backgrounded" }));
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
    // UNCONDITIONAL: a guarded assertion passes when the origin is some OTHER
    // arm, which is exactly the regression worth catching — the whole claim is
    // that a confirmed vendor-side detach is announced as detached BY THE USER.
    if (announced.origin.case !== "detached") {
      throw new Error("the confirmed detachment was not announced with a detached origin");
    }
    // THE SHIM ASKED FOR THIS MOVE (DetachForeground noted the unit before
    // `backgroundTasks`), so the patch's announcement is already the user's
    // (ruled 2026-09-30); the call's own result states `backgroundedByUser`
    // and restates the row with the same cause.
    expect(announced.origin.value.cause.case).toBe("byUser");
    const restated = await stream.until((f) => {
      if (f.frame.case !== "entry") return false;
      const result = entryFrame(watchAgentEntry(f))?.result;
      return result?.case === "detachedWork" && result.value.origin.case === "detached";
    });
    const restatedFrame = entryFrame(watchAgentEntry(restated));
    if (restatedFrame?.result.case !== "detachedWork" || restatedFrame.result.value.origin.case !== "detached") {
      throw new Error("the restated detachment was not announced with a detached origin");
    }
    expect(restatedFrame.result.value.origin.value.cause.case).toBe("byUser");
    // AND THE TURN ITSELF COMPLETES. `AgentSuccess.backgrounded` is the
    // WHOLE-TURN arm — the vendor's `background_requested` terminal reason,
    // where the agent's own run moved to the background — and this is not
    // that: one CALL left the turn and the agent kept working, which is why the
    // detachment is stated on the announcement above and the terminal is an
    // ordinary completion. Asserting `backgrounded` here would demand the mock
    // claim the whole turn had been backgrounded by a per-call detach.
    const terminal = await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      return agentFrame?.result.case === "success" || agentFrame?.result.case === "failure";
    });
    const frame = entryFrame(watchAgentEntry(terminal));
    if (frame?.result.case !== "success") throw new Error("the turn did not end AgentSuccess");
    expect(frame.result.value.outcome.case).toBe("completed");
    stream.close();
  });

  test("a vendor-moved shell's WatchBash opens with `start` at once", async () => {
    // THE INCIDENT'S PATH: a shell the vendor moved to the background is
    // announced from its patch, and the sidecar has written nothing for it. The
    // shim wrote its start ahead of that announcement, so the first frame is
    // owed immediately.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!vendor-backgrounded" }));
    const running = await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      return update.case === "activity" && update.value.item.case === "bash";
    });
    const runningFrame = entryFrame(watchAgentEntry(running));
    const update = runningFrame?.result.case === "update" ? runningFrame.result.value.update : undefined;
    const unit = update?.case === "activity" ? (update.value.activityId?.value ?? "") : "";
    detachForegroundAccepted(
      await shim.clients.h1.detachForeground(
        create(shimv1.DetachForegroundRequestSchema, { unit: activityId(unit) }),
      ),
    );
    await awaitAnnouncement(stream);

    const bash = openStream((options) =>
      shim.clients.h1.watchBash(create(shimv1.WatchBashRequestSchema, { work: workId(unit) }), options),
    );
    const first = bashFrame(await bash.next());

    expect(first.result.case).toBe("start");
    bash.close();
    stream.close();
  });

  test("a live unit the vendor matched to no foreground task is refused not_in_foreground", async () => {
    // `not_in_foreground` and NOT `not_detachable`: the unit is perfectly
    // detachable-in-kind, but the vendor tracks no foreground task for it, so
    // `backgroundTasks` moved nothing — the wrong arm would have lied about
    // the reason.
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

    expect(detachForegroundKind(response)).toBe("notInForeground");
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

  test("DetachForeground before StartSession is refused no_session", async () => {
    const shim = await spawnShim();

    const response = await shim.clients.h1.detachForeground(
      create(shimv1.DetachForegroundRequestSchema, { unit: activityId("anything") }),
    );

    expect(detachForegroundKind(response)).toBe("noSession");
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
    // bash, workflow and monitor — which is the distinction `not_in_foreground` does
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
    // A SHELL IS ITS OWN OS PROCESS: the replaced CLI process did not end it,
    // so a revival re-adopts it, announced as `created` — the item's start plus
    // its description — because a resume is not a detachment: nothing left a
    // turn, the work simply already exists.
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(first);
    await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    const announced = await awaitAnnouncement(stream);
    const run = announced.work?.value ?? "";
    stream.close();
    // THE SHIM DIES WITHOUT STANDING DOWN, which is the whole premise: an
    // ORDERLY kill stops every live item and writes its terminal, leaving
    // nothing to re-adopt. A crash stops nothing — the backgrounded shell keeps
    // running, its spool keeps no `EXIT=` line, and the record keeps an open
    // obligation for the revived session to reconcile.
    first.child.kill("SIGKILL");
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

  test("a subagent the replaced CLI process ran is closed at revival, never re-adopted", async () => {
    // A SUBAGENT RUNS INSIDE THE CLI PROCESS, so a revival (a new CLI process)
    // provably ended it: the revived shim closes it and does not announce it.
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(first);
    await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!subagent-detached-live" }));
    const announced = await awaitAnnouncement(stream);
    const run = announced.work?.value ?? "";
    stream.close();
    first.child.kill("SIGKILL");
    await first.exited;

    const second = await spawnShim({ reuse: first.dirs });
    const revived = sessionStarted(
      await second.clients.h1.startSession(resumeSession(started.vendorSessionId)),
    );
    // An ORDERLY stand-down flushes every write the shim enqueued, so the
    // reconciliation's closings have landed once it returns.
    await second.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await second.exited;
    const store = createStoreClient(first.dirs.storeSocket);
    const live = await store.getLiveWork(
      create(storev1.GetLiveWorkRequestSchema, {
        session: create(conversationv1.AgentIdSchema, { value: started.vendorSessionId }),
      }),
    );

    expect([
      revived.liveWork.map((item) => item.work?.value).includes(run),
      live.result.case === "success" ? live.result.value.liveDetached.map((id) => id.value).includes(run) : undefined,
    ]).toEqual([false, false]);
  });

  test("a spool-backed shell is left live at revival, its terminal the sidecar's", async () => {
    // A SHELL IS ITS OWN OS PROCESS WRITING ITS OWN SPOOL: the replaced CLI
    // process did not end it, so the revived shim writes no terminal for it.
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(first);
    await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    const announced = await awaitAnnouncement(stream);
    const run = announced.work?.value ?? "";
    stream.close();
    first.child.kill("SIGKILL");
    await first.exited;

    const second = await spawnShim({ reuse: first.dirs });
    await second.clients.h1.startSession(resumeSession(started.vendorSessionId));
    // THE WRITER IS ONE ORDERED BUFFER: a turn's terminal landing means every
    // row the reconciliation enqueued ahead of it has landed too.
    const agent = openStream((options) => second.clients.h1.watchAgent(watchAgentRequest(), options));
    await second.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "!md" }));
    await agent.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const result = entryFrame(watchAgentEntry(frame))?.result;
      return result?.case === "success" || result?.case === "failure";
    });
    agent.close();
    const store = createStoreClient(first.dirs.storeSocket);
    const live = await store.getLiveWork(
      create(storev1.GetLiveWorkRequestSchema, {
        session: create(conversationv1.AgentIdSchema, { value: started.vendorSessionId }),
      }),
    );

    expect(live.result.case === "success" ? live.result.value.liveDetached.map((id) => id.value) : []).toContain(run);
  });

  test("a session start never closes ANOTHER session's live work (2026-09-23)", async () => {
    // THE DEFECT: one store serves every session on the host. Opening a second
    // workspace reconciled the WHOLE record against its own vendor and wrote
    // lost.swept_up terminals for five subagents another session was still
    // running. Session A's work here is five live subagents and a shell run;
    // session B starts with an in-process subagent of its own, whose closing is
    // the proof its reconciliation actually ran.
    const first = await spawnShim();
    const b = sessionStarted(await first.clients.h1.startSession(freshSession()));
    await first.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await first.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await first.exited;
    const store = createStoreClient(first.dirs.storeSocket);
    const sessionA = "session-a-main";
    const aSubagents = ["a-sub-1", "a-sub-2", "a-sub-3", "a-sub-4", "a-sub-5"];
    for (const created of aSubagents) {
      await seedSubagentSpawn(store, sidecarProducer(sessionA), { spawner: sessionA, created });
    }
    await seedDetachedAnnouncement(store, sidecarProducer(sessionA), {
      work: "a-run",
      agent: sessionA,
      detachedFromId: "a-run",
      outputPath: "/nonexistent/a-run.output",
    });
    await seedSubagentSpawn(store, sidecarProducer(b.vendorSessionId), {
      spawner: b.vendorSessionId,
      created: "b-orphan-sub",
    });

    // Session B starts while A's work is live.
    const second = await spawnShim({ reuse: first.dirs });
    sessionStarted(await second.clients.h1.startSession(resumeSession(b.vendorSessionId)));
    // An ORDERLY stand-down flushes every write the shim ever enqueued, so
    // after it nothing B's reconciliation wrote can still be in flight.
    await second.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await second.exited;

    // Assert: nothing B's shim wrote names A's work...
    const shimKeys = writtenKeys(
      (first.store?.writeBatches() ?? [])
        .filter((batch) => batch.accepted && !batch.request.producer.startsWith("claude-sidecar:"))
        .map((batch) => batch.request),
    );
    expect(shimKeys.filter((key) => key.includes("a-sub-") || key.includes("a-run"))).toEqual([]);
    // ...and A's work is still open in A's own lineage.
    const live = await store.getLiveWork(
      create(storev1.GetLiveWorkRequestSchema, {
        session: create(conversationv1.AgentIdSchema, { value: sessionA }),
      }),
    );
    const open = live.result.case === "success" ? live.result.value : undefined;
    expect(open?.liveAgents.map((id) => id.value)).toEqual(aSubagents);
    expect(open?.liveDetached.map((id) => id.value)).toEqual(["a-run"]);
    // ...while B's own in-process subagent, proof its reconciliation ran, is closed.
    const bLive = await store.getLiveWork(
      create(storev1.GetLiveWorkRequestSchema, {
        session: create(conversationv1.AgentIdSchema, { value: b.vendorSessionId }),
      }),
    );
    expect(bLive.result.case === "success" ? bLive.result.value.liveAgents.map((id) => id.value) : undefined).toEqual([]);
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
    // THE CANCEL IS OURS, NOT THE MOCK'S. `!cancel-all` only ESTABLISHES the
    // fan — two agents and a shell, left live and unterminated — because the
    // vendor has no fan-wide verb: the cancel is a `stopTask` per item, which
    // is what `KillTurn{force}` issues over the turn's whole spawn set. The
    // mock then empties its live set and writes the `agents_killed` record,
    // which states a fact about the SET that no single stop could know.
    //
    // EACH ITEM CONCLUDES ON ITS OWN STREAM, which is the flatness rule again:
    // an agent's terminal is a page line on the spawning agent's book, and a
    // shell run's is a LIFECYCLE row served by `WatchBash` — never a page line
    // — so looking for all three in one place would be looking in the wrong
    // one for the shell.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(turnFor({ turn: "t1", text: "!cancel-all" }));
    // THE FAN IS EXACTLY THREE — two agents and a shell — and it is named as
    // three because that is what `!cancel-all` establishes. A `>= 3` would pass
    // on a scenario that announced four, which is a different fan than the one
    // the assertions below are about.
    const announcements: string[] = [];
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case === "detachedWork") {
        announcements.push(agentFrame.result.value.work?.value ?? "");
      }
      return announcements.length === 3;
    });
    expect(new Set(announcements).size).toBe(3);

    await shim.clients.h1.killTurn(
      create(shimv1.KillTurnRequestSchema, { turn: turnId("t1"), force: true }),
    );

    // THE TWO AGENTS: `stopped_by_user`, on the spawning agent's own book.
    const stoppedAgents = new Set<string>();
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      const update = agentFrame.result.value.update;
      if (update.case !== "activity") return false;
      const item = update.value.item;
      if (item.case !== "subagent" || item.value.result.case !== "failure") return false;
      if (item.value.result.value.cause.case !== "stoppedByUser") return false;
      stoppedAgents.add(update.value.activityId?.value ?? "");
      return stoppedAgents.size >= 2;
    });
    for (const agent of stoppedAgents) expect(announcements).toContain(agent);

    // THE SHELL: whatever the two agents were not, concluding `interrupted` on
    // the stream a shell run has — its own.
    const shells = announcements.filter((work) => !stoppedAgents.has(work));
    expect(shells).toHaveLength(1);
    const bash = openStream((options) =>
      shim.clients.h1.watchBash(
        create(shimv1.WatchBashRequestSchema, { work: workId(shells[0] ?? "") }),
        options,
      ),
    );
    const terminal = (await bash.drain()).map(bashFrame).at(-1);
    expect(terminal?.result.case).toBe("success");
    if (terminal?.result.case === "success") {
      expect(terminal.result.value.outcome.case).toBe("interrupted");
      if (terminal.result.value.outcome.case === "interrupted") {
        expect(terminal.result.value.outcome.value.cause.case).toBe("byUser");
      }
    }

    // AND THE SET EMPTIED. `agents_killed` is the vendor's record of that fact.
    const records = readTranscript(shim.dirs, started.vendorSessionId);
    expect(records.some((record) => record.subtype === "agents_killed")).toBe(true);
    stream.close();
  });
});
