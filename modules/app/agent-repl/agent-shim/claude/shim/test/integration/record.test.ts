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
import { afterAll, afterEach, describe, expect, test } from "vitest";
import { conversationv1, shimv1 } from "../../src/proto.js";
import type { LogRecord } from "../integration-support/log.js";
import { cleanupShims, spawnShim, type ShimHandle } from "../integration-support/harness.js";
import {
  canonicalSession,
  endsTurn,
  groupSession,
  releaseShared,
  since,
  uniqueTurn,
  type CanonicalSession,
  type Since,
} from "../integration-support/canonical.js";
import { workspaceLockKey } from "../../src/locks.js";
import { backupDir, backupName } from "../../src/engine/backup.js";
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
  pageBookOf,
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

// THE CASES HERE SHARE SHIMS (agent-shim/claude/shim/AGENTS.md "Integration
// cases share the canonical shim"): every case whose subject is a turn's rows
// runs on the file's canonical session, on its own turns and its own slice of
// the store (`since`). The outage cases share the "store-outage" group shim,
// one after another, and the rotation cases observe ONE rotation on the
// "rotation" group shim. Only the lifecycle cases (a SIGTERM stand-down, a
// resume of a dead process's files, a forced kill) spawn their own.
afterEach(cleanupShims);
afterAll(releaseShared);

type AgentStream = ReturnType<typeof openStream<shimv1.WatchAgentResponse>>;

/** Open the main agent's stream past its opening page. */
async function openAgentStream(
  shim: ShimHandle,
): Promise<AgentStream> {
  const stream = openStream((options) =>
    shim.clients.h1.watchAgent(watchAgentRequest(), options),
  );
  await stream.next();
  return stream;
}

/** Drive one turn to its terminal on the given stream. */
async function runTurn(
  shim: ShimHandle,
  stream: AgentStream,
  turn: string,
  text: string,
): Promise<void> {
  await shim.clients.h1.startTurn(startTurnRequest({ turn, text }));
  await stream.until((frame) => {
    if (frame.frame.case !== "entry") return false;
    const entry = watchAgentEntry(frame);
    // THIS turn's terminal: on a shared session a sibling's lingering work
    // can conclude on the same stream.
    if (entry.turn?.value !== turn) return false;
    const agentFrame = entryFrame(entry);
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
  dirs: ShimHandle["dirs"],
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
function keyOf(shim: ShimHandle): string {
  return workspaceLockKey(workspaceRealPath(shim.dirs));
}

/**
 * The "store-outage" group's session: every case whose subject is a store
 * outage, one after another on its own turn. An outage is the store's, so it
 * would fail a sibling's writes on the canonical shim; here only the outage
 * cases share it, and each recovers the store before it returns.
 */
function outageSession(): Promise<CanonicalSession> {
  return groupSession("store-outage");
}

/** The ERROR record naming the rows a persistent outage holds. */
function heldError(record: LogRecord): boolean {
  return record.level === "error" && record.context.held_upsert_keys !== undefined;
}

/** The degraded windows a session frame's diagnostics state; none for any other frame. */
function windowsOf(frame: shimv1.WatchSessionResponse): conversationv1.SessionDegradedWindow[] {
  const update = sessionUpdate(frame);
  return update.update.case === "diagnostics" ? update.update.value.degradedWindows : [];
}

/**
 * Let the store accept again and wait for the rows this case's outage held to
 * land, through its turn's terminal, so the next outage case starts on a
 * healthy store with nothing held.
 */
async function recover(shim: ShimHandle, mine: Since): Promise<void> {
  shim.store?.failWrites(null);
  await mine.landed((entry) => {
    const line = pageLineOf(entry);
    const frame = line?.agentItem?.item.case === "agentFrame" ? line.agentItem.item.value : undefined;
    return frame?.result.case === "success" || frame?.result.case === "failure";
  });
}

/** What the one rotation of the "rotation" group left, for its observers. */
interface Rotation extends CanonicalSession {
  /** The original transcript as it stood after the turn before the rotation. */
  readonly transcriptBeforeRotation: TranscriptRecord[];
  /** How many backup copies the state dir held after that turn. */
  readonly backupsBeforeRotation: number;
  /** The writes the rotating turn sent. */
  readonly rotateWrites: ReturnType<Since["writes"]>;
}

let rotated: Promise<Rotation> | null = null;

/**
 * ONE ROTATION, SEVERAL OBSERVERS. A session rotates its vendor id at most
 * once for the cases here to read (they assert a vendor-id directory holding
 * exactly one link), so every rotation case observes the same act on the
 * "rotation" group shim: an ordinary turn, then a `!rotate` turn.
 */
function rotation(): Promise<Rotation> {
  rotated ??= (async () => {
    const session = await groupSession("rotation");
    const { shim, started } = session;
    const stream = await openAgentStream(shim);
    await runTurn(shim, stream, uniqueTurn(), "!md");
    const transcriptBeforeRotation = readTranscript(shim.dirs, started.vendorSessionId);
    const backupsBeforeRotation = readdirSync(backupDir(shim.dirs.stateDir, keyOf(shim))).length;
    const mine = since(shim);
    await runTurn(shim, stream, uniqueTurn(), "!rotate");
    stream.close();
    return { ...session, transcriptBeforeRotation, backupsBeforeRotation, rotateWrites: mine.writes() };
  })();
  return rotated;
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

describe("R9: the identity the files carry", () => {
  test("agent-id.json exists BEFORE the first record, with the three ruled fields", async () => {
    // R9 CASE 1. The file is written before the query is created, so a crash
    // between the mint and the first record still leaves the identity
    // recoverable — which is only true if it is on disk while the store's
    // inbox is still empty.
    // THE CANONICAL SESSION'S START, observed before any case has run a turn
    // on it: this describe is the file's first, so the vendor's first record
    // has not been written yet.
    const { shim, started } = await canonicalSession();

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
    // ITS OWN SHIMS: the subject is a resume of a dead process's files.
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

describe("every entry's envelope", () => {
  test("the plane is STREAM on everything the shim writes", async () => {
    // The shim is the stream-plane producer; the sidecar is the file plane. A
    // stream row marked `file` would collide with the sidecar's own row for the
    // same unit under a different provenance.
    const { shim } = await canonicalSession();
    const mine = since(shim);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!read");

    const planes = new Set(
      writtenEntries(mine.writes()).map((entry) => entry.plane?.plane.case),
    );
    expect([...planes]).toEqual(["stream"]);
    stream.close();
  });

  test("the producer is claude-shim:<original vendor session id>", async () => {
    // Keyed by the ORIGINAL id, not the current one: a `/clear` rotates the
    // vendor session id, and a producer name that rotated with it would make one
    // writer look like two and split its deterministic write ids across two
    // namespaces.
    const { shim, started } = await canonicalSession();
    const mine = since(shim, [started.vendorSessionId]);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!md");

    expect(producers(mine.writes())).toEqual([
      `claude-shim:${started.vendorSessionId}`,
    ]);
    stream.close();
  });

  test("every upsert_key uses a ruled prefix", async () => {
    const { shim } = await canonicalSession();
    const mine = since(shim);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!read");

    const unruled = writtenKeys(mine.writes()).filter(
      (key) => !ALLOWED_KEY_PREFIXES.some((prefix) => key.startsWith(prefix)),
    );
    expect(unruled).toEqual([]);
    stream.close();
  });

  test("the prompt row is keyed prompt:<TurnId>", async () => {
    const { shim } = await canonicalSession();
    const mine = since(shim);
    const stream = await openAgentStream(shim);

    const keyed = uniqueTurn("turn-keyed");
    await runTurn(shim, stream, keyed, "!md");

    expect(writtenKeys(mine.writes())).toContain(`prompt:${keyed}`);
    stream.close();
  });

  test("a tool unit's rows are keyed activity:<its tool_use_id>", async () => {
    // Every frame of one unit shares the key, which is what makes a unit that
    // starts and settles appear ONCE in a book instead of twice.
    const { shim } = await canonicalSession();
    const mine = since(shim);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!read");

    const activityKeys = entriesKeyed(mine.writes(), "activity:").map(
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
    const { shim, started } = await canonicalSession();
    const mine = since(shim, [started.vendorSessionId]);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!read");
    await awaitFile(sessionTranscriptPath(shim.dirs, started.vendorSessionId));

    const vendorIds = toolUseIds(mine.transcript(started.vendorSessionId));
    const suffixes = new Set(
      entriesKeyed(mine.writes(), "activity:").map((entry) =>
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
    const { shim, started } = await canonicalSession();
    const mine = since(shim, [started.vendorSessionId]);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!compact");
    await awaitFile(sessionTranscriptPath(shim.dirs, started.vendorSessionId));

    const boundaries = mine.transcript(started.vendorSessionId).filter(
      (record) => record.type === "system" && record.subtype === "compact_boundary",
    );
    expect(boundaries).toHaveLength(1);
    const uuid = boundaries[0]?.uuid as string;
    expect(writtenKeys(mine.writes())).toContain(`session:context_cut:${uuid}`);
    stream.close();
  });

  test("a compaction writes exactly one context-cut row, however many planes saw it", async () => {
    // The duplicate the key fixes is COUNTED here rather than spelled: two keys
    // for one boundary is two rows however either of them is spelled.
    const { shim } = await canonicalSession();
    const mine = since(shim);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!compact");

    const cutKeys = new Set(
      writtenKeys(mine.writes()).filter((key) => key.startsWith("session:context_cut:")),
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
    const { shim, rotateWrites } = await rotation();

    const links = readdirSync(join(shim.dirs.stateDir, "shim", keyOf(shim), "vendor-id"));
    expect(links).toHaveLength(1);
    const rotatedTo = links[0].replace(/\.json$/, "");
    expect(writtenKeys(rotateWrites)).toContain(
      `session:context_cut:${rotatedTo}`,
    );
  });

  test("a clear writes exactly one context-cut row, however many planes saw it", async () => {
    // The duplicate the key fixes is COUNTED here rather than spelled: two keys
    // for one clear is two store rows, two positions, and two "context cleared"
    // dividers in the feed.
    const { rotateWrites } = await rotation();

    const cutKeys = new Set(
      writtenKeys(rotateWrites).filter((key) => key.startsWith("session:context_cut:")),
    );
    expect([...cutKeys]).toHaveLength(1);
  });

  test("a question's rows are keyed question:<the ask's tool_use_id>", async () => {
    const { shim } = await canonicalSession();
    const mine = since(shim);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: t1, text: "!ask-unanswered" }));

    endsTurn(shim, t1);
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      return agentFrame.result.value.update.case === "question";
    });

    expect(
      writtenKeys(mine.writes()).some((key) => key.startsWith("question:")),
    ).toBe(true);
    stream.close();
  });

  test("the question key's SUFFIX is the ASK's own tool_use_id from the transcript", async () => {
    // An ask IS a tool call, so its id is the vendor's, and a question row
    // keyed by anything else could never collide with the sidecar's row for the
    // same line.
    const { shim, started } = await canonicalSession();
    const mine = since(shim, [started.vendorSessionId]);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: t1, text: "!ask-unanswered" }));

    endsTurn(shim, t1);
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      return agentFrame.result.value.update.case === "question";
    });
    await awaitFile(sessionTranscriptPath(shim.dirs, started.vendorSessionId));

    const vendorIds = toolUseIds(mine.transcript(started.vendorSessionId));
    const questionSuffixes = writtenKeys(mine.writes())
      .filter((key) => key.startsWith("question:"))
      .map((key) => key.slice("question:".length));
    expect(questionSuffixes.length).toBeGreaterThan(0);
    for (const suffix of questionSuffixes) expect(vendorIds).toContain(suffix);
    stream.close();
  });

  test("a permission's rows are keyed permission:<the GATED call's id>", async () => {
    const { shim } = await canonicalSession();
    const mine = since(shim);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await shim.clients.h1.startTurn(startTurnRequest({ turn: t1, text: "!perm-deny-policy" }));

    endsTurn(shim, t1);
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const agentFrame = entryFrame(watchAgentEntry(frame));
      if (agentFrame?.result.case !== "update") return false;
      return agentFrame.result.value.update.case === "permission";
    });

    const permissionKeys = writtenKeys(mine.writes()).filter((key) =>
      key.startsWith("permission:"),
    );
    const activityKeys = writtenKeys(mine.writes()).filter((key) =>
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
    const { shim, started } = await canonicalSession();
    const mine = since(shim, [started.vendorSessionId]);
    const t1 = uniqueTurn();
    const t2 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!md");
    await runTurn(shim, stream, t2, "!md");

    const terminals = writtenKeys(mine.writes()).filter((key) =>
      key.startsWith(`terminal:${started.vendorSessionId}:`),
    );
    expect(new Set(terminals).size).toBe(2);
    // AND THE SUFFIX IS THE VENDOR'S RECORD UUID, not a counter: two distinct
    // suffixes could be minted by a shim that numbered its own terminals, and
    // the sidecar reading the same two lines would key them differently.
    const uuids = recordUuids(mine.transcript(started.vendorSessionId));
    expect(uuids.length).toBeGreaterThan(0);
    for (const key of new Set(terminals)) {
      expect(uuids).toContain(key.slice(`terminal:${started.vendorSessionId}:`.length));
    }
    stream.close();
  });

  test("a page line names its book and its top level", async () => {
    // Pageability is the PRODUCER's arm: a page line NAMES its book, and
    // unserveable material has no book at all.
    const { shim, started } = await canonicalSession();
    const mine = since(shim, [started.vendorSessionId]);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!md");

    const lines = writtenEntries(mine.writes())
      .map((entry) => ({ entry, line: pageLineOf(entry) }))
      .filter((pair): pair is { entry: (typeof pair)["entry"]; line: NonNullable<(typeof pair)["line"]> } =>
        pair.line !== null,
      );
    expect(lines.length).toBeGreaterThan(0);
    for (const { entry, line } of lines) {
      expect(pageBookOf(line)).not.toBe("");
      if (entry.entry.case === "agentUpdate") {
        expect(entry.entry.value.topLevel?.value).toBe(started.vendorSessionId);
      }
    }
    stream.close();
  });

  test("a frame's page_agent_id is the FRAME's agent, not always the main one", async () => {
    // A subagent's frames belong to the subagent's book. Filing them under the
    // main agent would make one conversation's page carry another agent's work.
    const { shim, started } = await canonicalSession();
    const mine = since(shim, [started.vendorSessionId]);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!subagent");

    const books = new Set(
      writtenEntries(mine.writes())
        .map((entry) => pageBookOf(pageLineOf(entry)))
        .filter((book): book is string => book !== undefined && book !== ""),
    );
    expect(books.has(started.vendorSessionId)).toBe(true);
    expect(books.size).toBeGreaterThan(1);
    stream.close();
  });

  test("shim-synthesized session facts are NEVER written", async () => {
    // Diagnostics and context usage are the shim's own report about ITSELF, not
    // vendor conversation, so they ride WatchSession and never the record.
    const { shim } = await canonicalSession();
    const mine = since(shim);
    const t1 = uniqueTurn();
    const session = openSessionUpdates((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    // Consume enough to know both were pushed.
    await session.until((frame) => sessionUpdate(frame).update.case === "diagnostics");
    await session.until((frame) => sessionUpdate(frame).update.case === "contextUsage");
    const stream = await openAgentStream(shim);
    await runTurn(shim, stream, t1, "!md");

    const armsWritten = (shim.store?.sessionUpdates() ?? []).map((update) => update.update.case);

    expect(armsWritten).not.toContain("diagnostics");
    expect(armsWritten).not.toContain("contextUsage");
    expect(
      writtenKeys(mine.writes()).filter(
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
    const { shim, started } = await outageSession();
    const mine = since(shim);
    const turn = uniqueTurn();
    const stream = await openAgentStream(shim);
    // Fail writes, run a turn (so the batches are retried), then accept again.
    shim.store?.failWrites("the store is down for this test");
    await shim.clients.h1.startTurn(startTurnRequest({ turn, text: "!md" }));
    // The refused batches are already in the store's inbox; let it accept.
    shim.store?.failWrites(null);
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const entry = watchAgentEntry(frame);
      if (entry.turn?.value !== turn) return false;
      const agentFrame = entryFrame(entry);
      return agentFrame?.result.case === "success" || agentFrame?.result.case === "failure";
    });

    const batches = mine.writeBatches();
    const entries = writtenEntries(mine.writes());
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
    // also satisfy. Counted over EVERY batch the shared store ever ACCEPTED
    // that names this book (its siblings' too), since the book holds them all.
    const book = shim.store?.book(started.vendorSessionId) ?? [];
    const bookKeys = new Set(
      (shim.store?.writeBatches() ?? [])
        .filter((batch) => batch.accepted)
        .flatMap((batch) => batch.request.batch?.entries ?? [])
        .filter((entry) => pageBookOf(pageLineOf(entry)) === started.vendorSessionId)
        .map((entry) => entry.upsertKey),
    );
    expect(book.length).toBe(bookKeys.size);
    stream.close();
  });

  test("a transient outage loses nothing and preserves order", async () => {
    // The bounded in-memory buffer replays in order: a row that jumped the
    // queue would put an answer above the question in the feed.
    const { shim } = await outageSession();
    const mine = since(shim);
    const turn = uniqueTurn();
    const stream = await openAgentStream(shim);

    shim.store?.failWrites("transient");
    await shim.clients.h1.startTurn(startTurnRequest({ turn, text: "!read" }));
    shim.store?.failWrites(null);
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const entry = watchAgentEntry(frame);
      if (entry.turn?.value !== turn) return false;
      const agentFrame = entryFrame(entry);
      return agentFrame?.result.case === "success" || agentFrame?.result.case === "failure";
    });

    const keys = writtenKeys(mine.writes());
    // The prompt row still precedes the first activity row after the replay.
    const promptAt = keys.indexOf(`prompt:${turn}`);
    const firstActivityAt = keys.findIndex((key) => key.startsWith("activity:"));
    expect(promptAt).toBeGreaterThanOrEqual(0);
    expect(firstActivityAt).toBeGreaterThan(promptAt);
    stream.close();
  });

  test("an exhausted schedule is a LOUD ERROR naming the held keys, and nothing is dropped", async () => {
    // There is NO durable spill and NO drop: the held rows are named at ERROR
    // and the shim keeps running. A crash here would lose the live session too.
    const { shim } = await outageSession();
    const mine = since(shim);
    const turn = uniqueTurn();
    const stream = await openAgentStream(shim);

    shim.store?.failWrites("down for good");
    await shim.clients.h1.startTurn(startTurnRequest({ turn, text: "!md" }));
    const held = await mine.record(heldError);

    expect((held.context.held_upsert_keys as string[]).length).toBeGreaterThan(0);
    // Still serving: the persistent failure is a report, not a death.
    expect(shim.child.exitCode).toBeNull();
    stream.close();
    await recover(shim, mine);
  });

  test("an outage opens a degraded window and reports store_unreachable", async () => {
    const { shim } = await outageSession();
    const mine = since(shim);
    const session = openSessionUpdates((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    // The windows the siblings' outages already opened and closed.
    const known = windowsOf(await session.next()).length;

    shim.store?.failWrites("down for good");
    await shim.clients.h1.startTurn(startTurnRequest({ turn: uniqueTurn(), text: "!md" }));
    const degraded = await session.until((frame) => {
      const update = sessionUpdate(frame);
      if (update.update.case !== "diagnostics") return false;
      const health = update.update.value.health;
      return (
        health.case === "unhealthy" &&
        health.value.faults.some((fault) => fault.kind.case === "storeUnreachable")
      );
    });

    // THIS outage's window, beside the ones before it.
    expect(windowsOf(degraded).length).toBeGreaterThan(known);
    session.close();
    await recover(shim, mine);
  });

  test("a recovered outage closes the window with nothing dropped", async () => {
    const { shim } = await outageSession();
    const mine = since(shim);
    const turn = uniqueTurn();
    const session = openSessionUpdates((options) =>
      shim.clients.h1.watchSession(create(shimv1.WatchSessionRequestSchema, {}), options),
    );
    // The windows the siblings' outages already opened and closed.
    const known = windowsOf(await session.next()).length;
    shim.store?.failWrites("down for a while");
    await shim.clients.h1.startTurn(startTurnRequest({ turn, text: "!md" }));
    await mine.record(heldError);

    shim.store?.failWrites(null);
    // THIS outage's window: the first one past the windows before it.
    const closed = await session.until((frame) =>
      windowsOf(frame).slice(known).some((window) => window.extent.case === "closed"),
    );

    const window = windowsOf(closed).slice(known).find((candidate) => candidate.extent.case === "closed");
    expect(window?.extent.case === "closed" ? window.extent.value.droppedCount : undefined).toBe(0n);
    session.close();
    await recover(shim, mine);
  });

  test("the rows a persistent outage held all land once the store answers", async () => {
    // The owner's rule: dropping a store write is data loss. The turn's prompt
    // and its terminal are both still written after the outage.
    const { shim } = await outageSession();
    const mine = since(shim);
    const turn = uniqueTurn();
    shim.store?.failWrites("down for a while");
    await shim.clients.h1.startTurn(startTurnRequest({ turn, text: "!md" }));
    await mine.record(heldError);

    shim.store?.failWrites(null);
    await mine.landed((entry) => {
      const line = pageLineOf(entry);
      const frame = line?.agentItem?.item.case === "agentFrame" ? line.agentItem.item.value : undefined;
      return frame?.result.case === "success" || frame?.result.case === "failure";
    });

    const accepted = mine.writeBatches().filter((batch) => batch.accepted).map((batch) => batch.request);
    expect(writtenKeys(accepted)).toContain(`prompt:${turn}`);
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
    // ITS OWN SHIM: the subject is the shim's SIGTERM stand-down, which ends
    // the process.
    const shim = await spawnShim();
    await shim.clients.h1.startSession(freshSession());
    const stream = await openAgentStream(shim);
    shim.store?.failWrites("briefly down");
    await shim.clients.h1.startTurn(startTurnRequest({ turn: "t1", text: "!md" }));
    // THE STORE'S ANSWER IS HELD across the signal. The stand-down's flush
    // waits for ONE more outcome under a persistent failure, and that outcome
    // was the writer's next retry on its own clock: when it came before this
    // test let the store accept, it was refused and the shim exited 1 with
    // the rows held (1 run in 15 under load, 2026-10-06). Held, the attempt
    // in flight is answered only after the store is back.
    const hold = shim.store?.holdWrites();

    shim.signal("SIGTERM");
    // The stand-down's own record, awaited rather than scanned.
    await shim.log.record((record) => record.context.outcome === "graceful_stand_down_started");
    // STILL RUNNING, with the buffer still unacked.
    expect(shim.child.exitCode).toBeNull();

    shim.store?.failWrites(null);
    hold?.release();
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
    const { shim, started } = await canonicalSession();
    const mine = since(shim, [started.vendorSessionId]);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!md");
    await awaitFile(sessionTranscriptPath(shim.dirs, started.vendorSessionId));

    const records = mine.transcript(started.vendorSessionId);
    // A chained record carries a uuid; an unchained metadata record does not,
    // and writing one through the chained path would orphan the next record.
    expect(records.some((record) => record.uuid !== undefined)).toBe(true);
    stream.close();
  });

  test("a subagent gets its own transcript beside its meta sidecar", async () => {
    const { shim, started } = await canonicalSession();
    const mine = since(shim, [started.vendorSessionId]);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!subagent");

    const created = writtenEntries(mine.writes())
      .map((entry) => pageBookOf(pageLineOf(entry)))
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
    const { shim, started } = await canonicalSession();
    const mine = since(shim, [started.vendorSessionId]);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!subagent");

    const created = writtenEntries(mine.writes())
      .map((entry) => pageBookOf(pageLineOf(entry)))
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
    const { shim, started } = await canonicalSession();
    const mine = since(shim, [started.vendorSessionId]);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    const spoolsBefore = new Set(readSpools(shim.dirs, started.vendorSessionId).map((spool) => spool.taskId));
    await runTurn(shim, stream, t1, "!bash-detach");
    // THE RUN OUTLIVES THE TURN, so the turn's end says nothing about the
    // spool. The shim's processing of the run's notification is the
    // happens-before: the vendor writes the EXIT line, then notifies, and the
    // notification restates the run's `detached:` row under a write identity
    // of its own, after the announcement's.
    const announced = await mine.landed((entry) => entry.upsertKey.startsWith("detached:"));
    await mine.landed(
      (entry) => entry.upsertKey === announced.upsertKey && entry.writeId !== announced.writeId,
    );

    // This case's own run's spool: a sibling's run on the shared session
    // keeps its own.
    const spools = readSpools(shim.dirs, started.vendorSessionId).filter((spool) => !spoolsBefore.has(spool.taskId));
    expect(spools.length).toBeGreaterThan(0);
    expect(spools.some((spool) => /EXIT=\d+\s*$/.test(spool.text.trimEnd()))).toBe(true);
    stream.close();
  });

  test("a run that never ends leaves an UNTERMINATED spool", async () => {
    const { shim, started } = await canonicalSession();
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    const spoolsBefore = new Set(readSpools(shim.dirs, started.vendorSessionId).map((spool) => spool.taskId));
    await shim.clients.h1.startTurn(startTurnRequest({ turn: t1, text: "!bash-detach-live" }));
    endsTurn(shim, t1);
    await stream.until((frame) => {
      if (frame.frame.case !== "entry") return false;
      const entry = watchAgentEntry(frame);
      return entry.turn?.value === t1 && entryFrame(entry)?.result.case === "detachedWork";
    });

    // This case's own run's spool: the run a sibling detached ended with its
    // EXIT= line, and is not this case's.
    const spools = readSpools(shim.dirs, started.vendorSessionId).filter((spool) => !spoolsBefore.has(spool.taskId));
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
    const { shim } = await canonicalSession();
    const mine = since(shim);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!residue");

    const kinds = (mine.unserved())
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
    const { shim, started } = await canonicalSession();
    const mine = since(shim, [started.vendorSessionId]);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!residue");

    const residueKeys = writtenKeys(mine.writes()).filter((key) =>
      key.startsWith("residue:"),
    );
    const attachmentUuids = mine.transcript(started.vendorSessionId)
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
    const { shim } = await canonicalSession();
    const mine = since(shim);
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!away-summary");

    const kinds = (mine.unserved()).map((item) =>
      item.unservedItem.case === "vendorSpecific" ? item.unservedItem.value.kind : "",
    );
    expect(kinds).toContain("system/away_summary");
    stream.close();
  });
});


describe("rotation, on the FILE plane", () => {
  test("a new transcript appears under the POST-CLEAR init id and the old file just stops", async () => {
    // ROTATION AS OBSERVED (capture identity-rotation-clear): the id the
    // session rotates to is the second `system:init`'s session_id, a new file
    // appears under it, and the old file ends with NO closing record of any
    // kind — the "closing system record" was declared and never observed.
    const { shim, started, transcriptBeforeRotation: before } = await rotation();

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
  });

  test("vendor-id/<new>.json holds the three ruled link fields", async () => {
    // The link is the smallest thing that closes the gap the files leave: the
    // reset message is the only place the old and new ids ever appear together,
    // so nobody can recover the link afterwards unless it was written down.
    const { shim, started } = await rotation();

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
  });

  test("the rotation's SessionUpdate row is keyed session:identity_rotated:<vendor uuid>", async () => {
    // The ruled spelling, and the suffix is the VENDOR's record uuid so the
    // sidecar's row for the same reset line collides with this one.
    const { shim, started, rotateWrites } = await rotation();

    const rotatedKeys = writtenKeys(rotateWrites).filter((key) =>
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
  });
});

describe("the transcript backup", () => {
  test("a turn leaves a BYTE-EQUAL copy under the state dir", async () => {
    // The transcript is the one artifact nobody can regenerate, so the copy is
    // asserted byte for byte: a truncated or re-serialized copy would restore
    // a conversation that is not the one that was lost.
    const { shim, started } = await canonicalSession();
    const t1 = uniqueTurn();
    const stream = await openAgentStream(shim);

    await runTurn(shim, stream, t1, "!md");

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
    const { shim, backupsBeforeRotation: afterTurn } = await rotation();
    const directory = backupDir(shim.dirs.stateDir, keyOf(shim));

    expect(readdirSync(directory).length).toBeGreaterThan(afterTurn);
  });

  test("a conversation keeps ONE copy however many turns it takes", async () => {
    // An unbounded backup of a growing file eventually costs more than it
    // protects, so each turn appends to the conversation's one copy rather
    // than taking another; the copy stays byte-equal to the transcript.
    const { shim, started } = await canonicalSession();
    const stream = await openAgentStream(shim);

    for (let turn = 0; turn < 4; turn += 1) {
      await runTurn(shim, stream, uniqueTurn(), "!md");
    }

    const directory = backupDir(shim.dirs.stateDir, keyOf(shim));
    const original = readFileSync(sessionTranscriptPath(shim.dirs, started.vendorSessionId));
    expect([
      readdirSync(directory),
      readFileSync(join(directory, backupName(started.vendorSessionId))).equals(original),
    ]).toEqual([[backupName(started.vendorSessionId)], true]);
    stream.close();
  });
});

describe("a detached shell run's key spellings", () => {
  test("the shim's bash rows are keyed bash:<run>:start and bash:<run>:terminal, with the VENDOR's run id", async () => {
    // THE RUN ID IS THE BASH CALL'S OWN tool_use_id, which the vendor wrote
    // into the transcript. The sidecar keys the same run off the same id from
    // the file, so a shim that keyed by anything else would write a second
    // lifecycle for one run instead of upserting onto the sidecar's.
    //
    // The `bash:<run>:<offset>` delta spelling has NO shim producer: every
    // output delta comes from the sidecar tailing the spool, so it is asserted
    // where it is minted (`src/store/keys.ts`'s own suite) and stated here
    // rather than faked into existence.
    // ITS OWN SHIM: the shim writes the run's terminal only at a forced
    // KillSession, which ends the process.
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
      const run = rest.replace(/:(start|terminal)$/, "");
      // Not a tail row: the shim writes only a run's lifecycle, never its output.
      expect(rest === `${run}:start` || rest === `${run}:terminal`).toBe(true);
      expect(vendorIds).toContain(run);
    }
    // THE SHIM WRITES BOTH ENDS: the start at the announcement, the terminal here.
    expect(bashKeys.some((key) => key.endsWith(":start"))).toBe(true);
    expect(bashKeys.some((key) => key.endsWith(":terminal"))).toBe(true);
  });
});
