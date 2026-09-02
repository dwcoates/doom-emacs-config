/**
 * test/integration/record.test.ts — the RECORD PLANE, as the store sees it.
 *
 * Everything here is asserted on what the shim actually SENT: the entries the
 * fake store received, the keys they upsert on, the write ids that make a resend
 * absorbable, and the vendor files the mocked binary left on disk. Nothing here
 * reads a shim internal — the store's inbox IS the observable.
 *
 * # The retry story, and why there is no spill
 *
 * A failed WriteBatch commits NOTHING, so a whole-batch retry is correct rather
 * than duplicating. Failures hold in a bounded IN-MEMORY buffer: transient blips
 * absorb silently, and exhausted retries are a LOUD LOGGED DROP — never a shim
 * crash and never a durable spill nobody drains. Graceful stand-down waits for
 * every ack, because exiting with unacknowledged writes is the loud failure.
 */
import { create } from "@bufbuild/protobuf";
import { existsSync, readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { afterEach, describe, expect, test } from "vitest";
import { shimv1 } from "../../src/proto.js";
import { cleanupShims, spawnShim } from "../integration-support/harness.js";
import {
  freshSession,
  openStream,
  startTurnRequest,
  watchAgentRequest,
} from "../integration-support/client.js";
import {
  entryFrame,
  sessionStarted,
  sessionUpdate,
  watchAgentEntry,
} from "../integration-support/expect.js";
import {
  entriesKeyed,
  pageLineOf,
  producers,
  writtenEntries,
  writtenKeys,
} from "../integration-support/store.js";
import {
  awaitFile,
  readSpools,
  projectDir,
  readSubagentMeta,
  readTranscript,
  sessionTranscriptPath,
  subagentTranscriptPathFor,
} from "../integration-support/vendor.js";

afterEach(cleanupShims);

type AgentStream = ReturnType<typeof openStream<shimv1.WatchAgentResponse>>;

/** Open the main agent's stream past its opening page. */
async function openAgentStream(
  shim: Awaited<ReturnType<typeof spawnShim>>,
): Promise<AgentStream> {
  const stream = openStream((options) =>
    shim.clients.h1.watchAgent(watchAgentRequest(), options),
  );
  await stream.next();
  return stream;
}

/** Drive one turn to its terminal on the given stream. */
async function runTurn(
  shim: Awaited<ReturnType<typeof spawnShim>>,
  stream: AgentStream,
  turn: string,
  text: string,
): Promise<void> {
  await shim.clients.h1.startTurn(startTurnRequest({ turn, text }));
  await stream.until((frame) => {
    if (frame.frame.case !== "entry") return false;
    const agentFrame = entryFrame(watchAgentEntry(frame));
    return agentFrame?.result.case === "success" || agentFrame?.result.case === "failure";
  });
}

/**
 * The vendor's own agent id for the subagent a given call spawned.
 *
 * TWO IDENTITIES, ONE SUBAGENT, AND THEY ARE NOT THE SAME BYTES. The wire
 * AgentId of a subagent is its SPAWNING CALL's `tool_use_id` (ruling, landing
 * 3), while the vendor names its files by an id of its own — a 17-hex agent id
 * it mints internally and never puts on the stream. Deriving one file path from
 * the other is what produced the ENOENT: there is no derivation, only a JOIN,
 * and the vendor states it in the sidecar's own `toolUseId` field.
 *
 * So the sidecars are read and the one naming this call is the answer. This
 * lives here rather than in integration-support/vendor.ts only to stay out of a
 * concurrently-owned file; it belongs beside the other vendor-file readers.
 */
function vendorAgentIdForCall(
  dirs: Awaited<ReturnType<typeof spawnShim>>["dirs"],
  vendorSessionId: string,
  toolUseId: string,
): string {
  const dir = join(projectDir(dirs), vendorSessionId, "subagents");
  const metas = existsSync(dir)
    ? readdirSync(dir).filter((name) => name.endsWith(".meta.json"))
    : [];
  for (const name of metas) {
    const meta = JSON.parse(readFileSync(join(dir, name), "utf8")) as { toolUseId?: string };
    if (meta.toolUseId === toolUseId) {
      return name.replace(/^agent-/, "").replace(/\.meta\.json$/, "");
    }
  }
  throw new Error(
    `no subagent sidecar under ${dir} names the spawning call ${toolUseId} (saw ${metas.join(", ")})`,
  );
}

/** The key prefixes the kickoff ruling allows, and nothing else. */
const ALLOWED_KEY_PREFIXES = [
  "activity:",
  "prompt:",
  "question:",
  "permission:",
  "terminal:",
  "bash:",
  "session:",
  "detached:",
  "residue:",
];

describe("every entry's envelope", () => {
  test("the plane is STREAM on everything the shim writes", async () => {
    // The shim is the stream-plane producer; the sidecar is the file plane. A
    // stream row marked `file` would collide with the sidecar's own row for the
    // same unit under a different provenance.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!read");

    const planes = new Set(
      writtenEntries(shim.store?.writes() ?? []).map((entry) => entry.plane?.plane.case),
    );
    expect([...planes]).toEqual(["stream"]);
    stream.close();
  });

  test("the producer is claude-shim:<original vendor session id>", async () => {
    // Keyed by the ORIGINAL id, not the current one: a `/clear` rotates the
    // vendor session id, and a producer name that rotated with it would make one
    // writer look like two and split its deterministic write ids across two
    // namespaces.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!md");

    expect(producers(shim.store?.writes() ?? [])).toEqual([
      `claude-shim:${started.vendorSessionId}`,
    ]);
    stream.close();
  });

  test("every upsert_key uses a ruled prefix", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!read");

    const unruled = writtenKeys(shim.store?.writes() ?? []).filter(
      (key) => !ALLOWED_KEY_PREFIXES.some((prefix) => key.startsWith(prefix)),
    );
    expect(unruled).toEqual([]);
    stream.close();
  });

  test("the prompt row is keyed prompt:<TurnId>", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "turn-keyed", "!md");

    expect(writtenKeys(shim.store?.writes() ?? [])).toContain("prompt:turn-keyed");
    stream.close();
  });

  test("a tool unit's rows are keyed activity:<its tool_use_id>", async () => {
    // Every frame of one unit shares the key, which is what makes a unit that
    // starts and settles appear ONCE in a book instead of twice.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!read");

    const activityKeys = entriesKeyed(shim.store?.writes() ?? [], "activity:").map(
      (entry) => entry.upsertKey,
    );
    expect(activityKeys.length).toBeGreaterThan(0);
    // At least one unit wrote more than one frame under ONE key.
    const counts = new Map<string, number>();
    for (const key of activityKeys) counts.set(key, (counts.get(key) ?? 0) + 1);
    expect([...counts.values()].some((count) => count > 1)).toBe(true);
    stream.close();
  });

  test("a question's rows are keyed question:<the ask's tool_use_id>", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!ask-unanswered" }));
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      return agentFrame.result.value.update.case === "question";
    });

    expect(
      writtenKeys(shim.store?.writes() ?? []).some((key) => key.startsWith("question:")),
    ).toBe(true);
    stream.close();
  });

  test("a permission's rows are keyed permission:<the GATED call's id>", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!perm-deny-policy" }));
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      return agentFrame.result.value.update.case === "permission";
    });

    const permissionKeys = writtenKeys(shim.store?.writes() ?? []).filter((key) =>
      key.startsWith("permission:"),
    );
    const activityKeys = writtenKeys(shim.store?.writes() ?? []).filter((key) =>
      key.startsWith("activity:"),
    );
    expect(permissionKeys.length).toBeGreaterThan(0);
    // The permission's id IS the gated call's, so the suffixes coincide.
    expect(
      permissionKeys.some((key) =>
        activityKeys.includes(`activity:${key.slice("permission:".length)}`),
      ),
    ).toBe(true);
    stream.close();
  });

  test("an agent's terminal is keyed terminal:<AgentId>:<vendor record uuid>", async () => {
    // Keyed by agent AND record, because one agent terminates once per turn and
    // a key of `terminal:<agent>` alone would leave a conversation with exactly
    // one visible terminal.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!md");
    await runTurn(shim, stream, "t2", "!md");

    const terminals = writtenKeys(shim.store?.writes() ?? []).filter((key) =>
      key.startsWith(`terminal:${started.vendorSessionId}:`),
    );
    expect(new Set(terminals).size).toBe(2);
    stream.close();
  });

  test("a page line names its book and its top level", async () => {
    // Pageability is the PRODUCER's arm: a page line NAMES its book, and
    // unserveable material has no book at all.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!md");

    const lines = writtenEntries(shim.store?.writes() ?? [])
      .map((entry) => ({ entry, line: pageLineOf(entry) }))
      .filter((pair): pair is { entry: (typeof pair)["entry"]; line: NonNullable<(typeof pair)["line"]> } =>
        pair.line !== null,
      );
    expect(lines.length).toBeGreaterThan(0);
    for (const { entry, line } of lines) {
      expect(line.pageAgentId?.value).not.toBe("");
      if (entry.entry.case === "agentUpdate") {
        expect(entry.entry.value.topLevel?.value).toBe(started.vendorSessionId);
      }
    }
    stream.close();
  });

  test("a frame's page_agent_id is the FRAME's agent, not always the main one", async () => {
    // A subagent's frames belong to the subagent's book. Filing them under the
    // main agent would make one conversation's page carry another agent's work.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!subagent");

    const books = new Set(
      writtenEntries(shim.store?.writes() ?? [])
        .map((entry) => pageLineOf(entry)?.pageAgentId?.value)
        .filter((book): book is string => book !== undefined && book !== ""),
    );
    expect(books.has(started.vendorSessionId)).toBe(true);
    expect(books.size).toBeGreaterThan(1);
    stream.close();
  });

  test("shim-synthesized session facts are NEVER written", async () => {
    // Diagnostics and context usage are the shim's own report about ITSELF, not
    // vendor conversation, so they ride WatchSession and never the record.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const session = openStream((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    // Consume enough to know both were pushed.
    await session.until((frame) => sessionUpdate(frame).update.case === "diagnostics");
    await session.until((frame) => sessionUpdate(frame).update.case === "contextUsage");
    const stream = await openAgentStream(shim);
    await runTurn(shim, stream, "t1", "!md");

    const armsWritten = (shim.store?.sessionUpdates() ?? []).map((update) => update.update.case);

    expect(armsWritten).not.toContain("diagnostics");
    expect(armsWritten).not.toContain("contextUsage");
    expect(
      writtenKeys(shim.store?.writes() ?? []).filter(
        (key) => key.startsWith("session:diagnostics") || key.startsWith("session:context_usage"),
      ),
    ).toEqual([]);
    session.close();
    stream.close();
  });
});

describe("write ids and absorption", () => {
  test("a re-sent frame mints the SAME write_id and the store absorbs it", async () => {
    // Deterministic identity from provenance is the whole retry story: the
    // writer can resend freely because the id comes from the content's source,
    // not from when it was sent.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);
    // Fail writes, run a turn (so the batches are retried), then accept again.
    shim.store?.failWrites("the store is down for this test");
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    // The refused batches are already in the store's inbox; let it accept.
    shim.store?.failWrites(null);
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      return agentFrame?.result.case === "success" || agentFrame?.result.case === "failure";
    });

    const entries = writtenEntries(shim.store?.writes() ?? []);
    const byKey = new Map<string, Set<string>>();
    for (const entry of entries) {
      const ids = byKey.get(entry.upsertKey) ?? new Set<string>();
      ids.add(entry.writeId);
      byKey.set(entry.upsertKey, ids);
    }
    // A resent frame carries the id it carried the first time.
    const resent = entries.filter(
      (entry) =>
        entries.filter((other) => other.writeId === entry.writeId).length > 1,
    );
    expect(resent.length).toBeGreaterThan(0);
    // And the book holds ONE row per key despite the resend.
    const book = shim.store?.book(started.vendorSessionId) ?? [];
    expect(book.length).toBeLessThanOrEqual(byKey.size);
    stream.close();
  });

  test("a transient outage loses nothing and preserves order", async () => {
    // The bounded in-memory buffer replays in order: a row that jumped the
    // queue would put an answer above the question in the feed.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    shim.store?.failWrites("transient");
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!read" }));
    shim.store?.failWrites(null);
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      return agentFrame?.result.case === "success" || agentFrame?.result.case === "failure";
    });

    const keys = writtenKeys(shim.store?.writes() ?? []);
    // The prompt row still precedes the first activity row after the replay.
    const promptAt = keys.indexOf("prompt:t1");
    const firstActivityAt = keys.findIndex((key) => key.startsWith("activity:"));
    expect(promptAt).toBeGreaterThanOrEqual(0);
    expect(firstActivityAt).toBeGreaterThan(promptAt);
    stream.close();
  });

  test("exhausted retries drop LOUDLY, naming the lost keys", async () => {
    // There is NO durable spill: exhausted retries log what was lost and the
    // shim keeps running. A crash here would lose the live session too.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    shim.store?.failWrites("down for good");
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    const dropped = await shim.log.record(
      (record) => record.level === "error" && record.context.lost_upsert_keys !== undefined,
    );

    expect(Array.isArray(dropped.context.lost_upsert_keys)).toBe(true);
    expect((dropped.context.lost_upsert_keys as string[]).length).toBeGreaterThan(0);
    // Still serving: the drop is a report, not a death.
    expect(shim.child.exitCode).toBeNull();
    stream.close();
  });

  test("an outage opens a degraded window and reports store_unreachable", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const session = openStream((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    await session.next();

    shim.store?.failWrites("down for good");
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    const degraded = await session.until((frame) => {
      const update = sessionUpdate(frame);
      if (update.update.case !== "diagnostics") return false;
      const health = update.update.value.health;
      return (
        health.case === "unhealthy" &&
        health.value.faults.some((fault) => fault.kind.case === "storeUnreachable")
      );
    });

    const update = sessionUpdate(degraded);
    if (update.update.case === "diagnostics") {
      expect(update.update.value.degradedWindows.length).toBeGreaterThan(0);
    }
    session.close();
  });

  test("a recovered outage closes the window with the dropped count", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const session = openStream((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    await session.next();
    shim.store?.failWrites("down for a while");
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await shim.log.record(
      (record) => record.level === "error" && record.context.lost_upsert_keys !== undefined,
    );

    shim.store?.failWrites(null);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t2", text: "!md" }));
    const closed = await session.until((frame) => {
      const update = sessionUpdate(frame);
      if (update.update.case !== "diagnostics") return false;
      return update.update.value.degradedWindows.some(
        (window) =>
          window.extent.case === "closed" && window.extent.value.droppedCount > 0n,
      );
    });

    expect(sessionUpdate(closed).update.case).toBe("diagnostics");
    session.close();
  });
});

describe("graceful stand-down", () => {
  test("SIGTERM exits only AFTER the buffered writes are acked", async () => {
    // Exiting with unacknowledged writes is the loud failure: the record is the
    // one thing the shim cannot reconstruct after it is gone.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    shim.store?.failWrites("briefly down");
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));

    // The store accepts again in the same tick the signal is raised, so the
    // stand-down must wait for the buffer to drain rather than racing it.
    shim.store?.failWrites(null);
    const exit = await shim.standDown();

    expect(exit.code).toBe(0);
    const keys = writtenKeys(shim.store?.writes() ?? []);
    expect(keys).toContain("prompt:t1");
    // The last batch the store saw was accepted, not refused.
    const accepted = (shim.store?.writes() ?? []).length;
    expect(accepted).toBeGreaterThan(0);
    stream.close();
  });
});

describe("the mocked vendor's files, at the ruled paths", () => {
  test("the session transcript exists where the sidecar will look for it", async () => {
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!md");
    await awaitFile(sessionTranscriptPath(shim.dirs, started.vendorSessionId));

    const records = readTranscript(shim.dirs, started.vendorSessionId);
    // A chained record carries a uuid; an unchained metadata record does not,
    // and writing one through the chained path would orphan the next record.
    expect(records.some((record) => record.uuid !== undefined)).toBe(true);
    stream.close();
  });

  test("a subagent gets its own transcript beside its meta sidecar", async () => {
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!subagent");

    const created = writtenEntries(shim.store?.writes() ?? [])
      .map((entry) => pageLineOf(entry)?.pageAgentId?.value)
      .find((book) => book !== undefined && book !== "" && book !== started.vendorSessionId);
    if (created === undefined) throw new Error("no subagent book was written");
    // `created` is the WIRE identity (the spawning call's id); the file is
    // named by the vendor's own agent id, joined through the sidecar.
    const vendorAgent = vendorAgentIdForCall(shim.dirs, started.vendorSessionId, created);
    expect(
      existsSync(subagentTranscriptPathFor(shim.dirs, started.vendorSessionId, vendorAgent)),
    ).toBe(true);
    stream.close();
  });

  test("the meta sidecar carries EXACTLY the four camelCase fields", async () => {
    // The corpus has four (`agentType`, `description`, `toolUseId`,
    // `spawnDepth`) and no `model`; a fifth would be invented, and snake_case
    // would be a different file than the one the vendor writes.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!subagent");

    const created = writtenEntries(shim.store?.writes() ?? [])
      .map((entry) => pageLineOf(entry)?.pageAgentId?.value)
      .find((book) => book !== undefined && book !== "" && book !== started.vendorSessionId);
    if (created === undefined) throw new Error("no subagent book was written");
    const vendorAgent = vendorAgentIdForCall(shim.dirs, started.vendorSessionId, created);
    const meta = readSubagentMeta(shim.dirs, started.vendorSessionId, vendorAgent);
    expect(Object.keys(meta).sort()).toEqual(
      ["agentType", "description", "spawnDepth", "toolUseId"].sort(),
    );
    stream.close();
  });

  test("a detached shell's spool is terminated by its EXIT= line", async () => {
    // The `EXIT=<code>` line is what terminates the sidecar's tail; a run that
    // never ends leaves no line at all, which is a different state and not an
    // error.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!bash-detach");

    const spools = readSpools(shim.dirs, started.vendorSessionId);
    expect(spools.length).toBeGreaterThan(0);
    expect(spools.some((spool) => /EXIT=\d+\s*$/.test(spool.text.trimEnd()))).toBe(true);
    stream.close();
  });

  test("a run that never ends leaves an UNTERMINATED spool", async () => {
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      return entryFrame(watchAgentEntry(frame))?.result.case === "detachedWork";
    });

    const spools = readSpools(shim.dirs, started.vendorSessionId);
    expect(spools.length).toBeGreaterThan(0);
    expect(spools.every((spool) => !spool.text.includes("EXIT="))).toBe(true);
    stream.close();
  });
});

describe("residue", () => {
  test("!residue writes the two attachment records as vendor_specific, on no page", async () => {
    // A tool-availability delta and an agent-type listing are vendor
    // BOOKKEEPING about what the model may call — not `context_injected`, which
    // carries instructions the model reads. Both planes drop them from every
    // page, and the kind spelling is shared with the sidecar so the two agree.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!residue");

    const kinds = (shim.store?.unserved() ?? [])
      .map((item) =>
        item.unservedItem.case === "vendorSpecific" ? item.unservedItem.value.kind : "",
      )
      .filter((kind) => kind !== "");
    expect(kinds).toContain("attachment/deferred_tools_delta");
    expect(kinds).toContain("attachment/agent_listing_delta");
    stream.close();
  });

  test("a residue row is keyed residue:<the vendor record's own uuid>", async () => {
    // THE RULED SPELLING (landing 5), not merely the `residue:` prefix. The key
    // exists so the sidecar's row and the shim's row for the SAME transcript
    // line collide on one key and the store absorbs the second; a different
    // spelling on either plane would make one record appear twice.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!residue");

    const residueKeys = writtenKeys(shim.store?.writes() ?? []).filter((key) =>
      key.startsWith("residue:"),
    );
    const attachmentUuids = readTranscript(shim.dirs, started.vendorSessionId)
      .filter((record) => record.type === "attachment")
      .map((record) => String(record.uuid));

    expect(attachmentUuids.length).toBeGreaterThan(0);
    for (const uuid of attachmentUuids) expect(residueKeys).toContain(`residue:${uuid}`);
    stream.close();
  });

  test("!away-summary lands as system/away_summary residue", async () => {
    // A system record's residue kind is `system/<subtype>`; no conversation.v1
    // arm models the vendor's recap, and inventing one would put prose nobody
    // wrote into the feed.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!away-summary");

    const kinds = (shim.store?.unserved() ?? []).map((item) =>
      item.unservedItem.case === "vendorSpecific" ? item.unservedItem.value.kind : "",
    );
    expect(kinds).toContain("system/away_summary");
    stream.close();
  });
});
