/**
 * The mocked-vendor test harness: drive `createFakeQuery` the way the shim
 * does, and collect everything it produced on BOTH planes.
 *
 * A scenario is only half-tested by its message sequence. The other half is on
 * disk, and the REAL sidecar is the consumer there, so every suite asserts the
 * files too — which means every suite needs temp roots for `configDir`, `cwd`
 * and `spoolRoot`. That is what `driveScenario` builds.
 *
 * `newUuid` and `nowMs` are DETERMINISTIC here (`u1`, `u2`, … and a fixed
 * clock) so an assertion can name a uuid instead of matching a pattern.
 */
import { existsSync, readFileSync, readdirSync } from "node:fs";
import { join } from "node:path";

import { createFakeQuery } from "../../src/fake/index.js";
import type { FakeQueryOpts } from "../../src/fake/index.js";
import { cwdSlug } from "../../src/fake/vendor-files.js";
import { testTempDir } from "../temp-dir.js";
import type { CanUseToolLike, QueryLike, SdkMessage } from "../../src/sdk/types.js";

/**
 * The harness's fixed clock, in epoch millis.
 *
 * Exported because a fixture stated as an OFFSET from the fake's own now (the
 * account-usage reset instants) can only be asserted against the clock the
 * fake was handed.
 */
export const HARNESS_NOW_MS = 1_800_000_000_000;

/** A record on either plane, as a plain object. */
export type Line = Record<string, unknown>;

/** What one drive produced. */
export interface Driven {
  /** Every SDK message the query yielded, in order. */
  readonly messages: SdkMessage[];
  /**
   * The error the drive's iterable REJECTED with, when it rejected.
   *
   * A scenario that fails its iterable (`!query-fail`) is a fact under test, so
   * the drive itself does not throw — but a caller that did not ask for a death
   * must not be handed a truncated message list as if it were a whole one.
   * Every caller that expects a clean drive asserts this is undefined; see
   * {@link expectDroveCleanly}.
   */
  readonly failure?: unknown;
  /** The main session transcript's records. */
  transcript(sessionId?: string): Line[];
  /** One subagent's transcript records. */
  subagent(agentId: string, sessionId?: string): Line[];
  /** One subagent's meta sidecar. */
  subagentMeta(agentId: string, sessionId?: string): Line;
  /** One task spool's body, or null when it was never opened. */
  spool(taskId: string, sessionId?: string): string | null;
  /** Every task id with a spool on disk. */
  spoolIds(sessionId?: string): string[];
  /** The roots the drive used. */
  readonly configDir: string;
  readonly cwd: string;
  readonly spoolRoot: string;
  readonly sessionId: string;
}

/** How one drive deviates from the ordinary case. */
export interface DriveOptions {
  /** What `canUseTool` answers. Defaults to a plain allow. */
  readonly canUseTool?: CanUseToolLike;
  /**
   * Run against the LIVE query before the prompts are exhausted.
   *
   * `messages` is the same array the drive is filling, so a test that needs an
   * id the scenario minted (a task id, a tool_use id) can read it out mid-flight
   * instead of guessing it. `roots` is the same {@link Driven.spool}-shaped
   * root information the finished drive reports, handed over early so a test
   * can read a spool's PARTIAL content while the drive is still parked on
   * something, rather than only after it settles.
   */
  readonly during?: (
    query: QueryLike,
    prompts: PromptFeeder,
    messages: Line[],
    roots: { readonly cwd: string; readonly configDir: string; readonly spoolRoot: string; readonly sessionId: string },
  ) => Promise<void> | void;
  /** Extra `createFakeQuery` options. */
  readonly opts?: Partial<FakeQueryOpts>;
  /** Resume this vendor session instead of starting fresh. */
  readonly resume?: string;
  /**
   * The client `uuid` each send carries, by prompt index — what the shim puts
   * on its keep-alive send. A prompt with none (or past the list) carries none.
   */
  readonly clientUuids?: readonly (string | undefined)[];
}

/** A prompt iterable a test can push into while the query runs. */
export class PromptFeeder {
  private readonly buffer: string[] = [];
  private waiter: ((value: IteratorResult<{ text: string }>) => void) | null = null;
  private closed = false;

  push(text: string): void {
    if (this.waiter !== null) {
      const resolve = this.waiter;
      this.waiter = null;
      resolve({ value: { text }, done: false });
      return;
    }
    this.buffer.push(text);
  }

  close(): void {
    this.closed = true;
    if (this.waiter !== null) {
      const resolve = this.waiter;
      this.waiter = null;
      resolve({ value: undefined as never, done: true });
    }
  }

  async *messages(): AsyncGenerator<{ text: string }> {
    for (;;) {
      const next = this.buffer.shift();
      if (next !== undefined) {
        yield { text: next };
        continue;
      }
      if (this.closed) return;
      const result = await new Promise<IteratorResult<{ text: string }>>((resolve) => {
        this.waiter = resolve;
      });
      if (result.done === true) return;
      yield result.value;
    }
  }
}

const ALLOW: CanUseToolLike = async (_name, input) =>
  ({ behavior: "allow", updatedInput: input });

/**
 * Run a fake query over a fixed list of prompts and collect both planes.
 *
 * The iterable is drained to completion, so a scenario that ends the stream or
 * fails it is observable here rather than hanging the suite.
 */
export async function driveScenario(
  prompts: readonly string[],
  options: DriveOptions = {},
): Promise<Driven> {
  const root = testTempDir("fake-drive-");
  const configDir = join(root, "cfg");
  const spoolRoot = join(root, "spools");
  const cwd = join(root, "workspace");
  const sessionId = options.resume ?? "sess-fake-1";

  let counter = 0;
  const feeder = new PromptFeeder();
  for (const prompt of prompts) feeder.push(prompt);

  const query = createFakeQuery(
    (async function* () {
      let index = 0;
      for await (const { text } of feeder.messages()) {
        const uuid = options.clientUuids?.[index];
        index++;
        yield {
          type: "user",
          message: { role: "user", content: text },
          parent_tool_use_id: null,
          ...(uuid === undefined ? {} : { uuid }),
        } as never;
      }
    })(),
    options.canUseTool ?? ALLOW,
    {
      cwd,
      configDir,
      spoolRoot,
      sessionId: "sess-fake-1",
      newUuid: () => `u${++counter}`,
      nowMs: () => HARNESS_NOW_MS,
      gitBranch: "offline",
      ...(options.resume === undefined ? {} : { resume: options.resume }),
      ...options.opts,
    },
  );

  const messages: SdkMessage[] = [];
  let failure: unknown;
  const collect = (async () => {
    try {
      for await (const message of query) messages.push(message);
    } catch (err) {
      // A scenario that FAILS its iterable is a fact under test, not a suite
      // failure; `messages` still holds everything delivered before the death.
      // KEPT, NEVER SWALLOWED: a drive that died unexpectedly used to look
      // exactly like a short scenario, so a conformance row could pass on the
      // handful of messages that arrived before the failure.
      failure = err;
    }
  })();

  if (options.during !== undefined) {
    await options.during(query, feeder, messages, { cwd, configDir, spoolRoot, sessionId });
  }
  feeder.close();
  await collect;

  const linesOf = (path: string): Line[] =>
    existsSync(path)
      ? readFileSync(path, "utf8")
          .split("\n")
          .filter((l) => l.trim() !== "")
          .map((l) => JSON.parse(l) as Line)
      : [];

  const projectDir = join(configDir, "projects", cwdSlug(cwd));
  return {
    messages,
    ...(failure === undefined ? {} : { failure }),
    configDir,
    cwd,
    spoolRoot,
    sessionId,
    transcript: (id = sessionId) => linesOf(join(projectDir, `${id}.jsonl`)),
    subagent: (agentId, id = sessionId) =>
      linesOf(join(projectDir, id, "subagents", `agent-${agentId}.jsonl`)),
    subagentMeta: (agentId, id = sessionId) =>
      JSON.parse(readFileSync(join(projectDir, id, "subagents", `agent-${agentId}.meta.json`), "utf8")) as Line,
    spool: (taskId, id = sessionId) => readSpool({ cwd, spoolRoot }, id, taskId),
    spoolIds: (id = sessionId) => {
      const dir = join(spoolRoot, cwdSlug(cwd), id, "tasks");
      return existsSync(dir) ? readdirSync(dir).map((f) => f.replace(/\.output$/, "")).sort() : [];
    },
  };
}

/**
 * Read one task spool's current body, or null when it was never opened.
 *
 * Shared by {@link Driven.spool} and by a `during` callback that needs to read
 * a spool's PARTIAL content mid-drive, off the `roots` `driveScenario` hands
 * `during` early (before the finished {@link Driven} exists to ask instead).
 */
export function readSpool(
  roots: { readonly cwd: string; readonly spoolRoot: string },
  sessionId: string,
  taskId: string,
): string | null {
  const path = join(roots.spoolRoot, cwdSlug(roots.cwd), sessionId, "tasks", `${taskId}.output`);
  return existsSync(path) ? readFileSync(path, "utf8") : null;
}

/**
 * Assert the drive reached its end without its iterable rejecting.
 *
 * The loud half of {@link Driven.failure}: a caller that expects a whole
 * scenario says so, and a death that would otherwise have been read as "the
 * scenario was short" fails the row with the error that caused it.
 */
export function expectDroveCleanly(driven: Driven): Driven {
  if (driven.failure !== undefined) {
    const detail =
      driven.failure instanceof Error ? driven.failure.message : JSON.stringify(driven.failure);
    throw new Error(`the fake vendor drive REJECTED before the scenario finished: ${detail}`);
  }
  return driven;
}

/** Every message of one `type` (and optional `subtype`). */
export function ofType(driven: Driven, type: string, subtype?: string): Line[] {
  return (driven.messages as unknown as Line[]).filter(
    (m) => m.type === type && (subtype === undefined || m.subtype === subtype),
  );
}

/** Every transcript record of one `type`. */
export function recordsOfType(lines: Line[], type: string): Line[] {
  return lines.filter((l) => l.type === type);
}

/** The `tool_use` blocks the assistant messages carried, in order. */
export function toolUses(driven: Driven): Line[] {
  return (driven.messages as unknown as Line[])
    .filter((m) => m.type === "assistant")
    .flatMap((m) => ((m.message as { content?: Line[] }).content ?? []))
    .filter((block) => block.type === "tool_use");
}

/** The single `result` message of a one-turn drive. */
export function theResult(driven: Driven): Line {
  const results = ofType(driven, "result");
  if (results.length !== 1) {
    throw new Error(`expected exactly one result message, saw ${results.length}`);
  }
  return results[0];
}

/** The `toolUseResult` values the transcript's user records carried, in order. */
export function toolUseResults(lines: Line[]): unknown[] {
  return lines.filter((l) => l.toolUseResult !== undefined).map((l) => l.toolUseResult);
}
