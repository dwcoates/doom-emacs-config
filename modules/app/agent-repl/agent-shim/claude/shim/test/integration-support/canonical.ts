/**
 * test/integration-support/canonical.ts — the shims a test file's cases SHARE.
 *
 * A shim hosts exactly one session, but one session runs many turns, and the
 * fake vendor picks each turn's behavior from its prompt text. So a case whose
 * subject is a turn (its frames, its rows, its refusals) runs on the file's ONE
 * canonical shim and session, on its own slice: its own turn ids
 * ({@link uniqueTurn}), its own runs, units and asks, asserting only on what
 * that slice owns. A case that needs a precondition the canonical shim does
 * not have (a fake-vendor env, a store page size, a session-wide mode it
 * changes) shares a GROUP shim with every case needing the same one
 * ({@link groupShim}). Only a case whose subject is the shim's own lifecycle
 * spawns its own (`spawnShim`). The rule is modules/app/agent-repl/AGENTS.md
 * "Integration tests share one canonical process, each case on its own slice";
 * its mechanics here are agent-shim/claude/shim/AGENTS.md "Integration cases
 * share the canonical shim".
 *
 * Cases in one file run one after another, so the shared session never has
 * two of them driving it at once. A case that leaves a turn open or work live
 * ends it before it returns ({@link endTurn}).
 *
 * Every shared shim is torn down by {@link releaseShared}, the file's
 * `afterAll`. A shared shim that dies fails every case that uses it after,
 * with its exit and stderr (spawnShim's record sink ends on the exit).
 */
import { create } from "@bufbuild/protobuf";
import { onTestFinished } from "vitest";
import { conversationv1, shimv1 } from "../../src/proto.js";
import type { storev1 } from "../../src/proto.js";
import type { TranscriptRecord } from "../../src/fake/vendor-files.js";
import type { FakeStoreRead, FakeStoreWrite } from "../fakes/store-server.js";
import { cleanupSharedShims, spawnShim, type ShimHandle, type SpawnShimOptions } from "./harness.js";
import type { LogPredicate, LogRecord } from "./log.js";
import { pageLineOf } from "./store.js";
import { readTranscript } from "./vendor.js";
import { freshSession, turnId } from "./client.js";
import { sessionStarted } from "./expect.js";

/** The canonical shim's session, started once for the whole file. */
export interface CanonicalSession {
  readonly shim: ShimHandle;
  /** The one StartSession's answer, which every start observer reads. */
  readonly response: shimv1.StartSessionResponse;
  readonly started: conversationv1.SessionStarted;
}

let canonical: Promise<ShimHandle> | null = null;
let session: Promise<CanonicalSession> | null = null;
const groups = new Map<string, { readonly options: string; readonly shim: Promise<ShimHandle> }>();
const groupSessions = new Map<string, Promise<CanonicalSession>>();
let turns = 0;

/** The file's canonical shim, spawned on first use. */
function canonicalShim(): Promise<ShimHandle> {
  canonical ??= spawnShim({ shared: true });
  return canonical;
}

/**
 * The canonical shim BEFORE its session starts, for a case whose subject is a
 * refusal of a shim with no session. Such cases run first in their file; one
 * asked for after the session started is a misordered file, which throws.
 */
export async function canonicalShimBeforeSession(): Promise<ShimHandle> {
  if (session !== null) {
    throw new Error("canonicalShimBeforeSession: the canonical session already started; a no-session case must run before the file's first session case");
  }
  return canonicalShim();
}

/** The canonical shim with its one session, started over HTTP/1.1 on first use. */
export function canonicalSession(): Promise<CanonicalSession> {
  session ??= (async () => {
    const shim = await canonicalShim();
    const response = await shim.clients.h1.startSession(freshSession());
    return { shim, response, started: sessionStarted(response) };
  })();
  return session;
}

/**
 * The group shim for one precondition, spawned on first use with OPTIONS and
 * shared by every case of the file that names KEY. The options of a key never
 * change: a second ask with other options names another precondition and
 * throws.
 */
export function groupShim(key: string, options: Omit<SpawnShimOptions, "shared"> = {}): Promise<ShimHandle> {
  const spelled = JSON.stringify(options);
  const known = groups.get(key);
  if (known !== undefined) {
    if (known.options !== spelled) {
      throw new Error(`groupShim: group ${key} was spawned with ${known.options}, not ${spelled}`);
    }
    return known.shim;
  }
  const shim = spawnShim({ ...options, shared: true });
  groups.set(key, { options: spelled, shim });
  return shim;
}

/**
 * The group shim for KEY with its one session, started over HTTP/1.1 on first
 * use: {@link groupShim}'s, as {@link canonicalSession} is the canonical
 * shim's.
 */
export function groupSession(key: string, options: Omit<SpawnShimOptions, "shared"> = {}): Promise<CanonicalSession> {
  const shim = groupShim(key, options);
  let started = groupSessions.get(key);
  if (started === undefined) {
    started = (async () => {
      const handle = await shim;
      const response = await handle.clients.h1.startSession(freshSession());
      return { shim: handle, response, started: sessionStarted(response) };
    })();
    groupSessions.set(key, started);
  }
  return started;
}

/** A turn id no other case of this file uses: `<prefix>-<n>`. */
export function uniqueTurn(prefix = "t"): string {
  turns += 1;
  return `${prefix}-${turns}`;
}

/**
 * Force-end TURN on SHIM (or find it already ended) and wait for its
 * terminal row to LAND, so the next
 * case on the shared session can start a turn and reads none of this one's
 * rows: the shim writes a killed turn's settlements (an ask it denied, its
 * terminal) after KillTurn answers, in order, ending with the terminal.
 */
export async function endTurn(shim: ShimHandle, turn: string): Promise<void> {
  if (shim.store === null) throw new Error("endTurn: this shim has no store to see the turn's terminal land");
  const response = await shim.clients.h1.killTurn(
    create(shimv1.KillTurnRequestSchema, { turn: turnId(turn), force: true }),
  );
  // A turn that already ended on its own is refused no_turn_open: it is
  // ended, and its terminal is awaited below like a killed one's. Any other
  // refusal is a failure.
  if (response.result.case !== "success" && response.result.value?.cause.case !== "noTurnOpen") {
    throw new Error(`endTurn: KillTurn of ${turn} was refused: ${JSON.stringify(response.result.value)}`);
  }

  await shim.store.entryLanded((entry) => {
    if (entry.turn?.value !== turn) return false;
    const line = pageLineOf(entry);
    const frame = line?.agentItem?.item.case === "agentFrame" ? line.agentItem.item.value : undefined;
    return frame?.result.case === "success" || frame?.result.case === "failure";
  });
}

/**
 * End the held TURN on SHIM when the case finishes, whether it passed or
 * failed ({@link endTurn}), so a failing case never leaves the shared session
 * with a turn open for its siblings to be refused on.
 */
export function endsTurn(shim: ShimHandle, ...turns: string[]): void {
  onTestFinished(async () => {
    // AT MOST ONE OF A CASE'S TURNS IS OPEN, and a later one opened only once
    // the earlier ended (or was refused), so they are ended newest first: the
    // open one is killed, and every other is found already ended.
    for (const turn of [...turns].reverse()) {
      if (!(await turnEnded(shim, turn))) await endTurn(shim, turn);
    }
  });
}

/**
 * Whether TURN's terminal already landed in SHIM's store, or TURN never
 * opened (no prompt row of it landed), so there is nothing of it to end.
 */
async function turnEnded(shim: ShimHandle, turn: string): Promise<boolean> {
  const landed = shim.store?.writeBatches().filter((batch) => batch.accepted).flatMap((batch) => batch.request.batch?.entries ?? []) ?? [];
  const mine = landed.filter((entry) => entry.turn?.value === turn);
  if (mine.length === 0) return true;
  return mine.some((entry) => {
    const line = pageLineOf(entry);
    const frame = line?.agentItem?.item.case === "agentFrame" ? line.agentItem.item.value : undefined;
    return frame?.result.case === "success" || frame?.result.case === "failure";
  });
}

/** Tear down every shared shim of the file and forget them: its `afterAll`. */
export async function releaseShared(): Promise<void> {
  canonical = null;
  session = null;
  groups.clear();
  groupSessions.clear();
  await cleanupSharedShims();
}

/**
 * What a shared shim did from one instant on: the slice of its store's writes,
 * its log and a transcript that a case running after its siblings owns. Every
 * view answers only what arrived after {@link since} was called, so a count
 * or a scan over it is the case's own.
 */
export interface Since {
  /** The WriteBatch requests the store received since the mark. */
  writes(): storev1.WriteBatchRequest[];
  /** The batches with the store's verdict, since the mark. */
  writeBatches(): FakeStoreWrite[];
  /** The shim's records since the mark. */
  records(): LogRecord[];
  /** The first record since the mark matching PREDICATE, awaiting one. */
  record(predicate: LogPredicate): Promise<LogRecord>;
  /** The lines of VENDORSESSIONID's transcript written since the mark. */
  transcript(vendorSessionId: string): TranscriptRecord[];
  /** The store's unserved items written since the mark. */
  unserved(): storev1.StoreUnservedItem[];
  /** The READ verbs the store served since the mark. */
  reads(): FakeStoreRead[];
  /**
   * The first entry MATCHES accepts that the store LANDED from a write sent
   * since the mark, awaiting one (the store's `entryLanded`, past the
   * siblings' writes).
   */
  landed(matches: (entry: storev1.StoreEntry) => boolean): Promise<storev1.StoreEntry>;
}

/**
 * Mark SHIM's store and log now, and the transcripts of TRANSCRIPTS (vendor
 * session ids); see {@link Since}. A transcript not marked here has no slice,
 * and asking for one throws.
 */
export function since(shim: ShimHandle, transcripts: readonly string[] = []): Since {
  const writes = shim.store?.writes().length ?? 0;
  const batches = shim.store?.writeBatches().length ?? 0;
  const before = new Set(shim.log.records());
  const unserved = shim.store?.unserved().length ?? 0;
  const reads = shim.store?.reads().length ?? 0;
  const writeIds = new Set(
    (shim.store?.writes() ?? []).flatMap((request) => request.batch?.entries ?? []).map((entry) => entry.writeId),
  );
  const lines = new Map(transcripts.map((id) => [id, readTranscript(shim.dirs, id).length]));
  return {
    writes: () => (shim.store?.writes() ?? []).slice(writes),
    writeBatches: () => (shim.store?.writeBatches() ?? []).slice(batches),
    records: () => shim.log.records().filter((record) => !before.has(record)),
    record: async (predicate) => shim.log.record((record) => !before.has(record) && predicate(record)),
    transcript: (id) => {
      const mark = lines.get(id);
      if (mark === undefined) throw new Error(`since: transcript ${id} was not marked when the slice began`);
      return readTranscript(shim.dirs, id).slice(mark);
    },
    unserved: () => (shim.store?.unserved() ?? []).slice(unserved),
    reads: () => (shim.store?.reads() ?? []).slice(reads),
    landed: async (matches) => {
      if (shim.store === null) throw new Error("since: this shim has no store");
      return shim.store.entryLanded((entry) => !writeIds.has(entry.writeId) && matches(entry));
    },
  };
}
