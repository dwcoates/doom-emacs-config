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
import { conversationv1, shimv1 } from "../../src/proto.js";
import { cleanupSharedShims, spawnShim, type ShimHandle, type SpawnShimOptions } from "./harness.js";
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

/** A turn id no other case of this file uses: `<prefix>-<n>`. */
export function uniqueTurn(prefix = "t"): string {
  turns += 1;
  return `${prefix}-${turns}`;
}

/** Force-end TURN on SHIM, so the next case on the shared session can start one. */
export async function endTurn(shim: ShimHandle, turn: string): Promise<void> {
  await shim.clients.h1.killTurn(create(shimv1.KillTurnRequestSchema, { turn: turnId(turn), force: true }));
}

/** Tear down every shared shim of the file and forget them: its `afterAll`. */
export async function releaseShared(): Promise<void> {
  canonical = null;
  session = null;
  groups.clear();
  await cleanupSharedShims();
}
