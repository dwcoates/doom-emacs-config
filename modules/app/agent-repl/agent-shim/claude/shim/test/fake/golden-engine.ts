/**
 * test/fake/golden-engine.ts — ONE SCENARIO DRIVEN THROUGH THE REAL SESSION
 * ENGINE, every row it writes to its store recorded in order.
 *
 * The scenario goldens' stream plane is what the shim SERVES, and the shim
 * serves its store: every row the session engine writes, not only the rows its
 * fold makes. The engine writes rows the fold never does -- a permission or
 * question ask (the gate), a compaction's failed or held cut, a model change
 * and the other session-observer facts -- so a drive through the bare fold
 * left those out, and a consumer of the goldens saw a turn without them
 * (`!compact-failed`'s cut, every `!perm-*` ask).
 *
 * So the drive runs the engine `main.ts` runs (`createEngine` with the real
 * fold, the vendor chosen by `main.ts`'s own `queryFactory` under `--fake`),
 * in process, over a store that records every write, and answers what the
 * scenario blocks on the way the daemon would: through the engine's own
 * verbs (UpdateAgent, DetachForeground). Every id and instant is fixed at
 * its source, so two drives write the same rows.
 */
import { mkdirSync } from "node:fs";
import { join } from "node:path";

import { create } from "@bufbuild/protobuf";

import { shimv1 } from "../../src/proto.js";
import { createFold } from "../../src/convert/fold.js";
import { createAgentIdentityStore } from "../../src/engine/identity.js";
import { createEngine, type SessionEngine } from "../../src/engine/session.js";
import { createFakeQuery } from "../../src/fake/index.js";
import { workspaceLockKey } from "../../src/locks.js";
import { queryFactory } from "../../src/main.js";
import type { QueryLike } from "../../src/sdk/types.js";
import type { PersistEntry } from "../../src/store/persistence.js";
import { ManualScheduler, RecordingPersistence, ScriptedProbe, unexpectedVendorFork } from "../engine/fakes.js";
import {
  allowOnce,
  answerQuestion,
  denyPermission,
  freshSession,
  startTurnRequest,
  stopAgent,
} from "../integration-support/client.js";
import { testTempDir } from "../temp-dir.js";
import { FakeSkillWatchFs } from "../engine/skill-watch-fake.js";

/** How the drive answers or steers a scenario (goldens.ts's policies). */
export type EnginePolicy = "allow" | "deny" | "answer" | "decline" | "background" | "interrupt";

/** The turn every golden drive runs its scenario in. */
export const GOLDEN_TURN = "turn-1";

/**
 * The conversation's vendor session id, which names the main agent's book:
 * fixed, so every golden's book is the same.
 */
export const GOLDEN_SESSION_ID = "601de000-0000-4000-8000-000000000001";

/** The vendor's message ids for this drive's one spawn. */
const SPAWN_TAG = "g0000000";

/** A store that also keeps every write, durable or buffered, in the order made. */
class OrderedPersistence extends RecordingPersistence {
  readonly log: PersistEntry[] = [];
  private readonly listeners: ((entries: readonly PersistEntry[]) => void)[] = [];

  onWrite(listener: (entries: readonly PersistEntry[]) => void): void {
    this.listeners.push(listener);
  }

  override write(entries: PersistEntry[]): void {
    super.write(entries);
    this.record(entries);
  }

  override writeDurable(entries: PersistEntry[]): Promise<void> {
    this.record(entries);
    return super.writeDurable(entries);
  }

  private record(entries: readonly PersistEntry[]): void {
    this.log.push(...entries);
    for (const listener of this.listeners) listener(entries);
  }
}

/** What one engine drive wrote, and where its vendor files are. */
export interface EngineDriven {
  /** Every row the engine wrote, in order. */
  readonly entries: readonly PersistEntry[];
  /** The mocked vendor's config root (its parent is the drive's temp root). */
  readonly configDir: string;
}

/**
 * Drive SCENARIO's prompt through the session engine, steered by POLICY, on a
 * clock starting at NOW_MS that advances one millisecond per vendor read and
 * is read, without advancing, by the engine.
 */
export async function driveEngineScenario(prompt: string, policy: EnginePolicy, nowMs: number): Promise<EngineDriven> {
  const root = testTempDir("golden-engine-");
  // THE ENGINE'S OWN STATE IS NOT THE VENDOR'S FILE PLANE: it lives beside the
  // root the tree golden is read from, never under it.
  const stateDir = testTempDir("golden-engine-state-");
  const configDir = join(root, "cfg");
  const spoolRoot = join(root, "spools");
  const cwd = join(root, "workspace");
  for (const dir of [configDir, spoolRoot, cwd]) mkdirSync(dir, { recursive: true });

  let now = nowMs;
  let vendorUuids = 0;
  let engineUuids = 0;
  const persistence = new OrderedPersistence();
  const clock = (): number => now;
  const turn = latch();
  const input = latch();
  const vendor = latch();
  // WHERE THE SCENARIO'S ROWS END. Set when the vendor's stream ends after the
  // drive closed its input: the engine reads one message, writes its rows and
  // only then reads the next, so at that end every row the scenario caused is
  // written, and what follows is the engine's reaction to a stdin the drive
  // closed, which no session of the scenario's own would see.
  let scenarioEnd: number | undefined;
  const engine: SessionEngine = createEngine({
    persistence,
    fold: createFold(),
    createQuery: queryFactory(true, goldenEnvironment(configDir, stateDir), cwd, {
      real: () => {
        throw new Error("goldens: a golden drive never reaches the real vendor");
      },
      fake: (prompts, canUseTool, opts) =>
        endObserved(
          createFakeQuery(closedWhen(prompts, input.settled), canUseTool, {
            ...opts,
            newUuid: () => `u${++vendorUuids}`,
            nowMs: () => now++,
            spawnTag: SPAWN_TAG,
            spoolRoot,
          }),
          () => {
            if (input.done) scenarioEnd ??= persistence.log.length;
            vendor.resolve();
          },
        ),
    }),
    forkVendorSession: unexpectedVendorFork,
    runtime: { shimBuildSha: "golden", sdkVersion: "golden" },
    env: { stateDir, configDir, cwd },
    skillWatchFs: new FakeSkillWatchFs(),
    nowMs: clock,
    newUuid: () => `00000000-0000-4000-8000-${String(++engineUuids).padStart(12, "0")}`,
    mintVendorSessionId: () => GOLDEN_SESSION_ID,
    scheduler: new ManualScheduler(),
    networkResumeScheduler: new ManualScheduler(),
    vendorErrorRetryScheduler: new ManualScheduler(),
    identityStore: createAgentIdentityStore(stateDir, workspaceLockKey(cwd), clock),
    acquireLock: () => () => undefined,
    acquireWorkspaceLock: () => () => undefined,
    probeApiReachable: new ScriptedProbe().probe,
  });

  const steer = steering(engine, policy, turn);
  persistence.onWrite((entries) => steer.onWrite(entries));
  const started = await engine.startSession(freshSession());
  if (started.result.case !== "success") {
    throw new Error(`goldens: StartSession for ${prompt} was refused: ${JSON.stringify(started.result)}`);
  }
  await engine.startTurn(startTurnRequest({ turn: GOLDEN_TURN, text: prompt }));
  await steer.done();
  // THE DRIVE'S INPUT CLOSES ONCE THE TURN HAS ENDED, as a CLI's stdin does:
  // the mocked vendor finishes what it started -- a detached shell's spool, a
  // background subagent's transcript -- and then ends its stream.
  input.resolve();
  await vendor.settled;
  const entries = persistence.log.slice(0, scenarioEnd);
  await engine.killSession(create(shimv1.KillSessionRequestSchema, {}));
  return { entries, configDir };
}

/** A one-shot signal: settled once, read as `done` from then on. */
function latch(): { readonly settled: Promise<void>; resolve(): void; readonly done: boolean } {
  let resolve!: () => void;
  let done = false;
  const settled = new Promise<void>((r) => {
    resolve = r;
  });
  return {
    settled,
    resolve(): void {
      done = true;
      resolve();
    },
    get done(): boolean {
      return done;
    },
  };
}

/** SOURCE, ending once CLOSED settles: the vendor's stdin as the drive closes it. */
async function* closedWhen<T>(source: AsyncIterable<T>, closed: Promise<void>): AsyncGenerator<T> {
  const iterator = source[Symbol.asyncIterator]();
  const close = closed.then(() => undefined);
  for (;;) {
    const next = await Promise.race([iterator.next(), close]);
    if (next === undefined || next.done === true) return;
    yield next.value;
  }
}

/** QUERY, telling ON_END the moment its stream ends or fails, before the reader hears it. */
function endObserved(query: QueryLike, onEnd: () => void): QueryLike {
  return new Proxy(query, {
    get(target, property) {
      if (property === Symbol.asyncIterator) {
        return () => {
          const iterator = target[Symbol.asyncIterator]();
          return {
            async next(): Promise<IteratorResult<unknown>> {
              try {
                const result = await iterator.next();
                if (result.done === true) onEnd();
                return result;
              } catch (err) {
                onEnd();
                throw err;
              }
            },
            [Symbol.asyncIterator]() {
              return this;
            },
          };
        };
      }
      const value: unknown = Reflect.get(target, property, target);
      return typeof value === "function" ? (value as (...args: unknown[]) => unknown).bind(target) : value;
    },
  });
}

/**
 * The daemon's side of the drive: it answers each ask the gate writes as the
 * policy says, and steers the scenarios that wait on the user.
 */
function steering(engine: SessionEngine, policy: EnginePolicy, turn: ReturnType<typeof latch>) {
  const pending: Promise<unknown>[] = [];
  // THE TURN'S END IS THE LATCH: the main agent's terminal, or the query
  // dying under it (`!query-eof`, `!query-fail`), whichever the engine writes.
  let mainAgent: string | undefined;
  const ended = (): void => turn.resolve();
  const act = (request: Promise<unknown>): void => {
    pending.push(request);
  };
  return {
    onWrite(entries: readonly PersistEntry[]): void {
      for (const entry of entries) {
        if (entry.item.kind === "prompt" && mainAgent === undefined) mainAgent = entry.agentId?.value;
        if (entry.item.kind === "session_update" && entry.item.update.update.case === "queryDied") ended();
        if (entry.item.kind !== "frame") continue;
        if (
          entry.agentId?.value === mainAgent &&
          (entry.item.frame.result.case === "success" || entry.item.frame.result.case === "failure")
        ) {
          ended();
        }
        const frame = entry.item.frame;
        if (frame.result.case !== "update") continue;
        const update = frame.result.value.update;
        const agent = frame.agentId;
        if (update.case === "permission" && update.value.result.case === "start" && update.value.id !== undefined) {
          const ask = update.value.id;
          act(
            engine.updateAgent(
              policy === "deny" ? denyPermission(ask, "the user declined this call", agent) : allowOnce(ask, agent),
            ),
          );
        }
        if (update.case === "question" && update.value.result.case === "start" && update.value.id !== undefined) {
          const batch = update.value.result.value.batch;
          if (policy === "decline") {
            act(engine.updateAgent(stopAgent(agent)));
            continue;
          }
          act(
            engine.updateAgent(
              answerQuestion(
                update.value.id,
                (batch?.questions ?? []).map((q) => {
                  const first = q.choices.value?.options[0]?.label?.label;
                  return first === undefined
                    ? { question: q.question?.text ?? "", labels: [], freeText: "the user's own answer" }
                    : { question: q.question?.text ?? "", labels: [first] };
                }),
                agent,
              ),
            ),
          );
        }
        if (update.case === "activity" && update.value.item.case === "bash" && update.value.item.value.result.case === "start") {
          const unit = update.value.activityId;
          if (policy === "background" && unit !== undefined) {
            act(engine.detachForeground(create(shimv1.DetachForegroundRequestSchema, { unit })));
          }
        }
        if (policy === "interrupt" && update.case === "activity") {
          act(engine.updateAgent(stopAgent(agent)));
          policy = "allow";
        }
      }
    },
    async done(): Promise<void> {
      await turn.settled;
      await Promise.all(pending);
    },
  };
}

/** The environment `queryFactory` reads: only the vendor account root, under `--fake`. */
function goldenEnvironment(claudeConfigDir: string, stateDir: string) {
  return { claudeConfigDir, stateDir, shimBuildSha: "golden", storeSocket: "unused-by-the-golden-drive" };
}
