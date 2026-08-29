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
import { existsSync, mkdtempSync, readFileSync, readdirSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { createFakeQuery } from "../../src/fake/index.js";
import type { FakeQueryOpts } from "../../src/fake/index.js";
import { cwdSlug } from "../../src/fake/vendor-files.js";
import type { CanUseToolLike, PermissionResultLike, QueryLike, SdkMessage } from "../../src/sdk/types.js";

/** A record on either plane, as a plain object. */
export type Line = Record<string, unknown>;

/** What one drive produced. */
export interface Driven {
  /** Every SDK message the query yielded, in order. */
  readonly messages: SdkMessage[];
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
   * instead of guessing it.
   */
  readonly during?: (query: QueryLike, prompts: PromptFeeder, messages: Line[]) => Promise<void> | void;
  /** Extra `createFakeQuery` options. */
  readonly opts?: Partial<FakeQueryOpts>;
  /** Resume this vendor session instead of starting fresh. */
  readonly resume?: string;
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
  ({ behavior: "allow", updatedInput: input }) as PermissionResultLike;

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
  const root = mkdtempSync(join(tmpdir(), "fake-drive-"));
  const configDir = join(root, "cfg");
  const spoolRoot = join(root, "spools");
  const cwd = join(root, "workspace");
  const sessionId = options.resume ?? "sess-fake-1";

  let counter = 0;
  const feeder = new PromptFeeder();
  for (const prompt of prompts) feeder.push(prompt);

  const query = createFakeQuery(
    (async function* () {
      for await (const { text } of feeder.messages()) {
        yield { type: "user", message: { role: "user", content: text }, parent_tool_use_id: null } as never;
      }
    })(),
    options.canUseTool ?? ALLOW,
    {
      cwd,
      configDir,
      spoolRoot,
      sessionId: "sess-fake-1",
      newUuid: () => `u${++counter}`,
      nowMs: () => 1_800_000_000_000,
      gitBranch: "offline",
      ...(options.resume === undefined ? {} : { resume: options.resume }),
      ...options.opts,
    },
  );

  const messages: SdkMessage[] = [];
  const collect = (async () => {
    try {
      for await (const message of query) messages.push(message);
    } catch {
      // A scenario that FAILS its iterable is a fact under test, not a suite
      // failure; `messages` still holds everything delivered before the death.
    }
  })();

  if (options.during !== undefined) await options.during(query, feeder, messages as unknown as Line[]);
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
    configDir,
    cwd,
    spoolRoot,
    sessionId,
    transcript: (id = sessionId) => linesOf(join(projectDir, `${id}.jsonl`)),
    subagent: (agentId, id = sessionId) =>
      linesOf(join(projectDir, id, "subagents", `agent-${agentId}.jsonl`)),
    subagentMeta: (agentId, id = sessionId) =>
      JSON.parse(readFileSync(join(projectDir, id, "subagents", `agent-${agentId}.meta.json`), "utf8")) as Line,
    spool: (taskId, id = sessionId) => {
      const path = join(spoolRoot, cwdSlug(cwd), id, "tasks", `${taskId}.output`);
      return existsSync(path) ? readFileSync(path, "utf8") : null;
    },
    spoolIds: (id = sessionId) => {
      const dir = join(spoolRoot, cwdSlug(cwd), id, "tasks");
      return existsSync(dir) ? readdirSync(dir).map((f) => f.replace(/\.output$/, "")).sort() : [];
    },
  };
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
  return results[0]!;
}

/** The `toolUseResult` values the transcript's user records carried, in order. */
export function toolUseResults(lines: Line[]): unknown[] {
  return lines.filter((l) => l.toolUseResult !== undefined).map((l) => l.toolUseResult);
}
