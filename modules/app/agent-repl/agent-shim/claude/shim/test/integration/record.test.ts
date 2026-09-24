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
 * than duplicating. Failures hold in an IN-MEMORY buffer bounded by pausing the
 * vendor stream: transient blips absorb silently, and a store that stays down
 * past the schedule is a LOUD ERROR naming the held keys — never a dropped row,
 * never a shim crash and never a durable spill nobody drains. Graceful
 * stand-down waits for every ack, because exiting with unacknowledged writes is
 * the loud failure.
 */
import { create } from "@bufbuild/protobuf";
import { existsSync, readdirSync, readFileSync, rmSync } from "node:fs";
import { join } from "node:path";
import { afterEach, describe, expect, test } from "vitest";
import { shimv1 } from "../../src/proto.js";
import { cleanupShims, spawnShim } from "../integration-support/harness.js";
import { workspaceLockKey } from "../../src/locks.js";
import { BACKUP_KEEP, backupDir } from "../../src/engine/backup.js";
import { agentIdPath, vendorLinkPath } from "../../src/engine/identity.js";
import {
  freshSession,
  openStream, openSessionUpdates,
  remediationPay,
  resumeSession,
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
  workspaceRealPath,
} from "../integration-support/vendor.js";
import type { TranscriptRecord } from "../../src/fake/vendor-files.js";

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

/**
 * Every content block of a transcript record's message, whatever its shape.
 *
 * THE FILE PLANE IS THE ORACLE. A key's suffix is only proved by the value the
 * VENDOR wrote — comparing two shim-minted strings proves the shim agrees with
 * itself and nothing more.
 */
function messageBlocks(record: TranscriptRecord): Array<Record<string, unknown>> {
  const message = record.message as { content?: unknown } | undefined;
  const content = message?.content;
  return Array.isArray(content) ? (content as Array<Record<string, unknown>>) : [];
}

/** Every `tool_use` block id the vendor wrote into a transcript, in order. */
function toolUseIds(records: readonly TranscriptRecord[]): string[] {
  return records.flatMap((record) =>
    messageBlocks(record)
      .filter((block) => block.type === "tool_use" && typeof block.id === "string")
      .map((block) => block.id as string),
  );
}

/** Every chained record's own uuid, in file order. */
function recordUuids(records: readonly TranscriptRecord[]): string[] {
  return records
    .map((record) => record.uuid)
    .filter((uuid): uuid is string => typeof uuid === "string");
}

/** This spawn's workspace key — the directory both state trees are rooted at. */
function keyOf(shim: Awaited<ReturnType<typeof spawnShim>>): string {
  return workspaceLockKey(workspaceRealPath(shim.dirs));
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

  test("the activity key's SUFFIX is the vendor's own tool_use_id from the transcript", async () => {
    // The key is only right if it names the id the VENDOR minted. Asserting the
    // `activity:` prefix alone would pass on a key whose suffix the shim
    // invented, and the sidecar keying the same unit from the file would then
    // write a second row for one call.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!read");
    await awaitFile(sessionTranscriptPath(shim.dirs, started.vendorSessionId));

    const vendorIds = toolUseIds(readTranscript(shim.dirs, started.vendorSessionId));
    const suffixes = new Set(
      entriesKeyed(shim.store?.writes() ?? [], "activity:").map((entry) =>
        entry.upsertKey.slice("activity:".length),
      ),
    );
    expect(vendorIds.length).toBeGreaterThan(0);
    // EVERY TOOL CALL THE VENDOR WROTE HAS ITS ROW UNDER THE VENDOR'S OWN ID.
    for (const id of vendorIds) expect([...suffixes]).toContain(id);
    // And the only OTHER activity keys are the prose/thinking units, whose
    // suffix is the vendor's message id plus the block index (`<msg>:<n>`) —
    // a block has no id of its own, so the message's is the only vendor value
    // there is to key by.
    const unaccounted = [...suffixes].filter(
      (suffix) => !vendorIds.includes(suffix) && !/^msg_[^:]+:\d+$/.test(suffix),
    );
    expect(unaccounted).toEqual([]);
    stream.close();
  });

  test("a compaction's cut is keyed session:context_cut:<the boundary's own uuid>", async () => {
    // ONE COMPACTION IS ONE ROW. The vendor states a `compact_boundary` on the
    // stream AND writes the identical record — same uuid — to the transcript,
    // so this plane and the sidecar both convert it. They collapse onto one
    // store row only if the key BYTES match, and the sidecar mints
    // `session:context_cut:<uuid>`. This plane spelled it `cut:<uuid>`, so the
    // store held two entries at two positions and the feed drew the divider
    // twice, the second copy from whichever plane's frame was less complete.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!compact");
    await awaitFile(sessionTranscriptPath(shim.dirs, started.vendorSessionId));

    const boundaries = readTranscript(shim.dirs, started.vendorSessionId).filter(
      (record) => record.type === "system" && record.subtype === "compact_boundary",
    );
    expect(boundaries).toHaveLength(1);
    const uuid = boundaries[0]?.uuid as string;
    expect(writtenKeys(shim.store?.writes() ?? [])).toContain(`session:context_cut:${uuid}`);
    stream.close();
  });

  test("a compaction writes exactly one context-cut row, however many planes saw it", async () => {
    // The duplicate the key fixes is COUNTED here rather than spelled: two keys
    // for one boundary is two rows however either of them is spelled.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!compact");

    const cutKeys = new Set(
      writtenKeys(shim.store?.writes() ?? []).filter((key) => key.startsWith("session:context_cut:")),
    );
    expect([...cutKeys]).toHaveLength(1);
    stream.close();
  });

  test("a clear's cut is keyed session:context_cut:<the session it rotated to>", async () => {
    // ONE CLEAR IS ONE ROW, and the two planes see DIFFERENT records for it.
    // This plane's evidence is the SDK `conversation_reset`; the sidecar's is
    // the expanded `/clear` envelope the vendor writes into the NEW transcript,
    // and it keys on that file's session uuid. Their record uuids are
    // unrelated — `identity-rotation-clear` has `cc07c2a0-…` on the stream and
    // `04f97c00-…` on disk — so the ONLY identity both can mint is the session
    // the clear rotated to, which is why this row waits for the init that
    // states it.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!rotate");

    const links = readdirSync(join(shim.dirs.stateDir, "shim", keyOf(shim), "vendor-id"));
    expect(links).toHaveLength(1);
    const rotatedTo = links[0].replace(/\.json$/, "");
    expect(writtenKeys(shim.store?.writes() ?? [])).toContain(
      `session:context_cut:${rotatedTo}`,
    );
    stream.close();
  });

  test("a clear writes exactly one context-cut row, however many planes saw it", async () => {
    // The duplicate the key fixes is COUNTED here rather than spelled: two keys
    // for one clear is two store rows, two positions, and two "context cleared"
    // dividers in the feed.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!rotate");

    const cutKeys = new Set(
      writtenKeys(shim.store?.writes() ?? []).filter((key) => key.startsWith("session:context_cut:")),
    );
    expect([...cutKeys]).toHaveLength(1);
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

  test("the question key's SUFFIX is the ASK's own tool_use_id from the transcript", async () => {
    // An ask IS a tool call, so its id is the vendor's, and a question row
    // keyed by anything else could never collide with the sidecar's row for the
    // same line.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!ask-unanswered" }));
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      return agentFrame.result.value.update.case === "question";
    });
    await awaitFile(sessionTranscriptPath(shim.dirs, started.vendorSessionId));

    const vendorIds = toolUseIds(readTranscript(shim.dirs, started.vendorSessionId));
    const questionSuffixes = writtenKeys(shim.store?.writes() ?? [])
      .filter((key) => key.startsWith("question:"))
      .map((key) => key.slice("question:".length));
    expect(questionSuffixes.length).toBeGreaterThan(0);
    for (const suffix of questionSuffixes) expect(vendorIds).toContain(suffix);
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
    // AND THE SUFFIX IS THE VENDOR'S RECORD UUID, not a counter: two distinct
    // suffixes could be minted by a shim that numbered its own terminals, and
    // the sidecar reading the same two lines would key them differently.
    const uuids = recordUuids(readTranscript(shim.dirs, started.vendorSessionId));
    expect(uuids.length).toBeGreaterThan(0);
    for (const key of new Set(terminals)) {
      expect(uuids).toContain(key.slice(`terminal:${started.vendorSessionId}:`.length));
    }
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
    const session = openSessionUpdates((options) =>
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

    const batches = shim.store?.writeBatches() ?? [];
    const entries = writtenEntries(shim.store?.writes() ?? []);
    const byKey = new Map<string, Set<string>>();
    for (const entry of entries) {
      const ids = byKey.get(entry.upsertKey) ?? new Set<string>();
      ids.add(entry.writeId);
      byKey.set(entry.upsertKey, ids);
    }
    // EVERY id is a 64-hex digest of its provenance. A shorter or non-hex id
    // would mean the writer minted something other than the ruled hash, and a
    // resend would then be a new row rather than an absorbed one.
    for (const entry of entries) expect(entry.writeId).toMatch(/^[0-9a-f]{64}$/);
    // A resent frame carries the id it carried the first time, and it repeats
    // for exactly ONE reason: it rode a REFUSED batch and was sent again.
    const counts = new Map<string, number>();
    for (const entry of entries) counts.set(entry.writeId, (counts.get(entry.writeId) ?? 0) + 1);
    const duplicated = [...counts].filter(([, count]) => count > 1).map(([id]) => id);
    expect(duplicated.length).toBeGreaterThan(0);
    const refusedIds = new Set(
      batches
        .filter((batch) => !batch.accepted)
        .flatMap((batch) => batch.request.batch?.entries ?? [])
        .map((entry) => entry.writeId),
    );
    expect(duplicated.filter((id) => !refusedIds.has(id))).toEqual([]);
    // And the book holds EXACTLY one row per key: ABSORPTION, not merely "no
    // more rows than keys", which a store that silently dropped writes would
    // also satisfy. Counted over the rows that were actually ACCEPTED and that
    // name this book, since those are the only ones the book can hold.
    const book = shim.store?.book(started.vendorSessionId) ?? [];
    const bookKeys = new Set(
      batches
        .filter((batch) => batch.accepted)
        .flatMap((batch) => batch.request.batch?.entries ?? [])
        .filter((entry) => pageLineOf(entry)?.pageAgentId?.value === started.vendorSessionId)
        .map((entry) => entry.upsertKey),
    );
    expect(book.length).toBe(bookKeys.size);
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

  test("an exhausted schedule is a LOUD ERROR naming the held keys, and nothing is dropped", async () => {
    // There is NO durable spill and NO drop: the held rows are named at ERROR
    // and the shim keeps running. A crash here would lose the live session too.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    shim.store?.failWrites("down for good");
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    const held = await shim.log.record(
      (record) => record.level === "error" && record.context.held_upsert_keys !== undefined,
    );

    expect((held.context.held_upsert_keys as string[]).length).toBeGreaterThan(0);
    // Still serving: the persistent failure is a report, not a death.
    expect(shim.child.exitCode).toBeNull();
    stream.close();
  });

  test("an outage opens a degraded window and reports store_unreachable", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const session = openSessionUpdates((options) =>
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

  test("a recovered outage closes the window with nothing dropped", async () => {
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const session = openSessionUpdates((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    await session.next();
    shim.store?.failWrites("down for a while");
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await shim.log.record(
      (record) => record.level === "error" && record.context.held_upsert_keys !== undefined,
    );

    shim.store?.failWrites(null);
    const closed = await session.until((frame) => {
      const update = sessionUpdate(frame);
      if (update.update.case !== "diagnostics") return false;
      return update.update.value.degradedWindows.some((window) => window.extent.case === "closed");
    });

    const update = sessionUpdate(closed);
    const window =
      update.update.case === "diagnostics"
        ? update.update.value.degradedWindows.find((candidate) => candidate.extent.case === "closed")
        : undefined;
    expect(window?.extent.case === "closed" ? window.extent.value.droppedCount : undefined).toBe(0n);
    session.close();
  });

  test("the rows a persistent outage held all land once the store answers", async () => {
    // The owner's rule: dropping a store write is data loss. The turn's prompt
    // and its terminal are both still written after the outage.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    shim.store?.failWrites("down for a while");
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    await shim.log.record(
      (record) => record.level === "error" && record.context.held_upsert_keys !== undefined,
    );

    shim.store?.failWrites(null);
    await shim.store?.entryLanded((entry) => {
      const line = pageLineOf(entry);
      const frame = line?.agentItem?.item.case === "agentFrame" ? line.agentItem.item.value : undefined;
      return frame?.result.case === "success" || frame?.result.case === "failure";
    });

    const accepted = (shim.store?.writeBatches() ?? []).filter((batch) => batch.accepted).map((batch) => batch.request);
    expect(writtenKeys(accepted)).toContain("prompt:t1");
  });
});

describe("graceful stand-down", () => {
  test("SIGTERM exits only AFTER the buffered writes are acked", async () => {
    // Exiting with unacknowledged writes is the loud failure: the record is the
    // one thing the shim cannot reconstruct after it is gone.
    //
    // THE REFUSAL IS HELD ACROSS THE SIGNAL. An earlier version let the store
    // accept again BEFORE raising SIGTERM, so a shim that exited without
    // waiting would still have found its buffer empty and passed. Here the
    // stand-down begins with the store still refusing, the process is observed
    // ALIVE at that moment, and only then does the store start accepting — so
    // the exit cannot precede the acks.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    shim.store?.failWrites("briefly down");
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));

    shim.signal("SIGTERM");
    // The stand-down's own record, awaited rather than scanned.
    await shim.log.record((record) => record.context.outcome === "graceful_stand_down_started");
    // STILL RUNNING, with the buffer still unacked.
    expect(shim.child.exitCode).toBeNull();

    shim.store?.failWrites(null);
    const exit = await shim.exited;

    expect(exit.code).toBe(0);
    // The rows the buffer was holding did land, and they landed on an ACCEPTED
    // batch — the last verdict the store gave is the proof the exit waited.
    const batches = shim.store?.writeBatches() ?? [];
    expect(writtenKeys(shim.store?.writes() ?? [])).toContain("prompt:t1");
    expect(batches.at(-1)?.accepted).toBe(true);
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

describe("R9: the identity the files carry", () => {
  test("agent-id.json exists BEFORE the first record, with the three ruled fields", async () => {
    // R9 CASE 1. The file is written before the query is created, so a crash
    // between the mint and the first record still leaves the identity
    // recoverable — which is only true if it is on disk while the store's
    // inbox is still empty.
    const shim = await spawnShim();

    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));

    const file = agentIdPath(shim.dirs.stateDir, keyOf(shim));
    expect(existsSync(file)).toBe(true);
    const record = JSON.parse(readFileSync(file, "utf8")) as Record<string, unknown>;
    expect(Object.keys(record).sort()).toEqual(
      ["minted_at_ms", "original_vendor_session_id", "workspace_key"].sort(),
    );
    expect(record.original_vendor_session_id).toBe(started.vendorSessionId);
    expect(record.workspace_key).toBe(keyOf(shim));
    expect(typeof record.minted_at_ms).toBe("number");
    // NOTHING THE VENDOR WOULD WRITE EXISTS YET: the identity is on disk while
    // the transcript — the vendor's first record — has not been created.
    expect(existsSync(sessionTranscriptPath(shim.dirs, started.vendorSessionId))).toBe(false);
  });

  test("a resume with the file ABSENT adopts the resume id and logs the derivation", async () => {
    // R9 CASE 2. Every real transcript's `sessionId` equals its filename, so
    // the resume id IS the original id and adopting it is a DERIVATION. It is
    // logged as one, because a silent adoption and a guess look identical
    // afterwards.
    const first = await spawnShim();
    const started = sessionStarted(await first.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(first);
    await runTurn(first, stream, "t1", "!md");
    stream.close();
    await first.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await first.exited;
    // The state a file-only reader would face: the transcript, and no identity.
    rmSync(agentIdPath(first.dirs.stateDir, keyOf(first)));

    const second = await spawnShim({ reuse: first.dirs });
    const resumed = sessionStarted(
      await second.clients.h1.startSession(
        resumeSession(started.vendorSessionId, remediationPay()),
      ),
    );

    expect(resumed.vendorSessionId).toBe(started.vendorSessionId);
    const derived = await second.log.record(
      (record) => record.context.rule === "absent_file_adopts_resume_id",
    );
    expect(derived.level).toBe("info");
    expect(derived.context.vendor_session_id).toBe(started.vendorSessionId);
    // And the derivation was PERSISTED, so the next reader does not derive again.
    const record = JSON.parse(
      readFileSync(agentIdPath(second.dirs.stateDir, keyOf(second)), "utf8"),
    ) as Record<string, unknown>;
    expect(record.original_vendor_session_id).toBe(started.vendorSessionId);
  });
});

describe("rotation, on the FILE plane", () => {
  test("a new transcript appears under the POST-CLEAR init id and the old file just stops", async () => {
    // ROTATION AS OBSERVED (capture identity-rotation-clear): the id the
    // session rotates to is the second `system:init`'s session_id, a new file
    // appears under it, and the old file ends with NO closing record of any
    // kind — the "closing system record" was declared and never observed.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);
    await runTurn(shim, stream, "t1", "!md");
    const before = readTranscript(shim.dirs, started.vendorSessionId);

    await runTurn(shim, stream, "t2", "!rotate");

    const links = readdirSync(join(shim.dirs.stateDir, "shim", keyOf(shim), "vendor-id"));
    expect(links).toHaveLength(1);
    const newId = links[0].replace(/\.json$/, "");
    expect(newId).not.toBe(started.vendorSessionId);
    // The NEW file exists under the post-clear init id...
    expect(existsSync(sessionTranscriptPath(shim.dirs, newId))).toBe(true);
    expect(readTranscript(shim.dirs, newId).length).toBeGreaterThan(0);
    // ...and the OLD one simply STOPPED. Everything it gained is the part of
    // the rotating turn that happened BEFORE the reset (its prompt and the
    // assistant lines that preceded the reset); nothing CLOSES it. The mock's
    // declared "closing system record" was dropped when the capture showed the
    // real file ends mid-conversation, so a trailing system record here would
    // be a shape no real transcript has.
    const after = readTranscript(shim.dirs, started.vendorSessionId);
    expect(after.length).toBeGreaterThan(before.length);
    expect(after.at(-1)?.type).not.toBe("system");
    stream.close();
  });

  test("vendor-id/<new>.json holds the three ruled link fields", async () => {
    // The link is the smallest thing that closes the gap the files leave: the
    // reset message is the only place the old and new ids ever appear together,
    // so nobody can recover the link afterwards unless it was written down.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!rotate");

    const links = readdirSync(join(shim.dirs.stateDir, "shim", keyOf(shim), "vendor-id"));
    expect(links).toHaveLength(1);
    const newId = links[0].replace(/\.json$/, "");
    const link = JSON.parse(
      readFileSync(vendorLinkPath(shim.dirs.stateDir, keyOf(shim), newId), "utf8"),
    ) as Record<string, unknown>;
    expect(Object.keys(link).sort()).toEqual(
      ["linked_at_ms", "original_vendor_session_id", "vendor_session_id"].sort(),
    );
    expect(link.vendor_session_id).toBe(newId);
    expect(link.original_vendor_session_id).toBe(started.vendorSessionId);
    expect(typeof link.linked_at_ms).toBe("number");
    stream.close();
  });

  test("the rotation's SessionUpdate row is keyed session:identity_rotated:<vendor uuid>", async () => {
    // The ruled spelling, and the suffix is the VENDOR's record uuid so the
    // sidecar's row for the same reset line collides with this one.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!rotate");

    const rotatedKeys = writtenKeys(shim.store?.writes() ?? []).filter((key) =>
      key.startsWith("session:identity_rotated:"),
    );
    expect(rotatedKeys.length).toBeGreaterThan(0);
    // THE SUFFIX CANNOT BE JOINED TO THE FILE PLANE, AND THAT IS THE FINDING.
    // `conversation_reset` exists ONLY on the stream — the observed capture
    // shows the old transcript simply stopping with no record of the reset at
    // all — so this arm's uuid is a stream uuid with no transcript line behind
    // it. What IS assertable is that it is the vendor's uuid rather than a
    // shim-minted counter, and that it names no line either file holds.
    const links = readdirSync(join(shim.dirs.stateDir, "shim", keyOf(shim), "vendor-id"));
    const newId = links[0].replace(/\.json$/, "");
    const fileUuids = new Set([
      ...recordUuids(readTranscript(shim.dirs, started.vendorSessionId)),
      ...recordUuids(readTranscript(shim.dirs, newId)),
    ]);
    for (const key of rotatedKeys) {
      const suffix = key.slice("session:identity_rotated:".length);
      expect(suffix).toMatch(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/);
      expect(fileUuids.has(suffix)).toBe(false);
    }
    stream.close();
  });
});

describe("the transcript backup", () => {
  test("a turn leaves a BYTE-EQUAL copy under the state dir", async () => {
    // The transcript is the one artifact nobody can regenerate, so the copy is
    // asserted byte for byte: a truncated or re-serialized copy would restore
    // a conversation that is not the one that was lost.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, "t1", "!md");

    const directory = backupDir(shim.dirs.stateDir, keyOf(shim));
    const copies = readdirSync(directory).filter((name) => name.endsWith(".jsonl"));
    expect(copies.length).toBeGreaterThan(0);
    const original = readFileSync(sessionTranscriptPath(shim.dirs, started.vendorSessionId));
    expect(
      copies.some((name) => readFileSync(join(directory, name)).equals(original)),
    ).toBe(true);
    stream.close();
  });

  test("a rotation takes a SECOND copy", async () => {
    // A copy is taken at every turn end AND at every rotation, because the
    // rotation is exactly the moment the old transcript stops growing and
    // becomes the thing there is no other copy of.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    await runTurn(shim, stream, "t1", "!md");
    const directory = backupDir(shim.dirs.stateDir, keyOf(shim));
    const afterTurn = readdirSync(directory).length;

    await runTurn(shim, stream, "t2", "!rotate");

    expect(readdirSync(directory).length).toBeGreaterThan(afterTurn);
    stream.close();
  });

  test("the copies are BOUNDED by the keep constant", async () => {
    // An unbounded backup of a growing file eventually costs more than it
    // protects. The bound is read from the producer's own constant rather than
    // spelled again here.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);

    for (let turn = 0; turn <= BACKUP_KEEP + 2; turn += 1) {
      await runTurn(shim, stream, `t${String(turn)}`, "!md");
    }

    const directory = backupDir(shim.dirs.stateDir, keyOf(shim));
    expect(readdirSync(directory).length).toBeLessThanOrEqual(BACKUP_KEEP);
    stream.close();
  });
});

describe("a detached shell run's key spellings", () => {
  test("the shim's bash rows are keyed bash:<run> and bash:<run>:terminal, with the VENDOR's run id", async () => {
    // THE RUN ID IS THE BASH CALL'S OWN tool_use_id, which the vendor wrote
    // into the transcript. The sidecar keys the same run off the same id from
    // the file, so a shim that keyed by anything else would write a second
    // lifecycle for one run instead of upserting onto the sidecar's.
    //
    // The `bash:<run>:<offset>` delta spelling has NO shim producer: every
    // output delta comes from the sidecar tailing the spool, so it is asserted
    // where it is minted (`src/store/keys.ts`'s own suite) and stated here
    // rather than faked into existence.
    const shim = await spawnShim();
    const started = sessionStarted(await shim.clients.h1.startSession(freshSession()));
    const stream = await openAgentStream(shim);
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!bash-detach-live" }));
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      return entryFrame(watchAgentEntry(frame))?.result.case === "detachedWork";
    });
    stream.close();

    // The forced kill is what makes the shim — rather than an absent sidecar —
    // write the run's terminal.
    await shim.clients.h1.killSession(create(shimv1.KillSessionRequestSchema, { force: true }));
    await shim.exited;

    const bashKeys = writtenKeys(shim.store?.writes() ?? []).filter((key) =>
      key.startsWith("bash:"),
    );
    expect(bashKeys.length).toBeGreaterThan(0);
    const vendorIds = toolUseIds(readTranscript(shim.dirs, started.vendorSessionId));
    expect(vendorIds.length).toBeGreaterThan(0);
    for (const key of bashKeys) {
      const rest = key.slice("bash:".length);
      const run = rest.endsWith(":terminal") ? rest.slice(0, -":terminal".length) : rest;
      // Not an offset row: the shim writes none.
      expect(rest === run || rest === `${run}:terminal`).toBe(true);
      expect(vendorIds).toContain(run);
    }
    expect(bashKeys.some((key) => key.endsWith(":terminal"))).toBe(true);
  });
});
