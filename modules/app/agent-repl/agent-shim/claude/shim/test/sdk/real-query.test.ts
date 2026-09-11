/**
 * The real query's OPTIONS are the contract; the factory itself needs the live
 * SDK and is therefore unreachable from a hermetic suite (the vendor guard
 * throws). Every option asserted here is load-bearing per
 * docs/overhaul/shim.md.
 */
import { mkdtempSync, mkdirSync, writeFileSync, writeSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { afterEach, describe, expect, it, vi } from "vitest";
import { realQueryOptions, type RealQuerySpec } from "../../src/sdk/real-query.js";
import { METAPROMPT_REL_PATH, DOOM_CHECKOUT_REL_PATH } from "../../src/metaprompt.js";

function homeWithMetaprompt(text: string): string {
  const home = mkdtempSync(path.join(os.tmpdir(), "shim-real-query-"));
  const file = path.join(home, DOOM_CHECKOUT_REL_PATH, METAPROMPT_REL_PATH);
  mkdirSync(path.dirname(file), { recursive: true });
  writeFileSync(file, text, "utf8");
  return home;
}

function spec(overrides: Partial<RealQuerySpec> = {}): RealQuerySpec {
  return {
    cwd: "/ws",
    claudeConfigDir: "/accounts/primary",
    binding: { kind: "fresh", sessionId: "vendor-1" },
    permissionMode: "default",
    canUseTool: async () => ({ behavior: "deny", message: "unused" }),
    abortController: new AbortController(),
    home: mkdtempSync(path.join(os.tmpdir(), "shim-real-query-empty-")),
    ...overrides,
  };
}

describe("realQueryOptions", () => {
  it("keeps the claude_code preset so the model can resolve ~", () => {
    // Arrange, Act.
    const options = realQueryOptions(spec());

    // Assert.
    expect(options.systemPrompt).toEqual({ type: "preset", preset: "claude_code" });
  });

  it("appends the canonical metaprompt to the preset", () => {
    // Arrange.
    const home = homeWithMetaprompt("BE EXCELLENT");

    // Act.
    const options = realQueryOptions(spec({ home }));

    // Assert.
    expect(options.systemPrompt).toEqual({
      type: "preset",
      preset: "claude_code",
      append: "BE EXCELLENT",
    });
  });

  it("loads user, project and local settings so denied.by_policy exists", () => {
    // Arrange, Act.
    const options = realQueryOptions(spec());

    // Assert.
    expect(options.settingSources).toEqual(["user", "project", "local"]);
  });

  it("asks for partial messages so the fold has stream events", () => {
    // Arrange, Act.
    const options = realQueryOptions(spec());

    // Assert.
    expect(options.includePartialMessages).toBe(true);
  });

  it("asks for subagent text so a spawned agent is not a black box", () => {
    // Arrange, Act.
    const options = realQueryOptions(spec());

    // Assert.
    expect(options.forwardSubagentText).toBe(true);
  });

  it("PRE-MINTS the vendor session id on a fresh start", () => {
    // Arrange, Act.
    const options = realQueryOptions(spec({ binding: { kind: "fresh", sessionId: "vendor-42" } }));

    // Assert.
    expect(options).toMatchObject({ sessionId: "vendor-42" });
  });

  it("never carries a resume handle on a fresh start", () => {
    // Arrange, Act.
    const options = realQueryOptions(spec({ binding: { kind: "fresh", sessionId: "vendor-42" } }));

    // Assert.
    expect(options.resume).toBeUndefined();
  });

  it("resumes by vendor id and mints no session id of its own", () => {
    // Arrange, Act.
    const options = realQueryOptions(
      spec({ binding: { kind: "resume", resumeSessionId: "vendor-old" } }),
    );

    // Assert.
    expect({ resume: options.resume, sessionId: options.sessionId }).toEqual({
      resume: "vendor-old",
      sessionId: undefined,
    });
  });

  it("makes the spawned account root authoritative in the child environment", () => {
    // Arrange, Act.
    const options = realQueryOptions(spec({ claudeConfigDir: "/accounts/secondary" }));

    // Assert.
    expect(options.env?.CLAUDE_CONFIG_DIR).toBe("/accounts/secondary");
  });

  it("passes the rest of the environment through to the child", () => {
    // Arrange.
    process.env.AGENT_REPL_REAL_QUERY_PROBE = "kept";

    // Act.
    const options = realQueryOptions(spec());

    // Assert.
    expect(options.env?.AGENT_REPL_REAL_QUERY_PROBE).toBe("kept");
    delete process.env.AGENT_REPL_REAL_QUERY_PROBE;
  });

  it("NEVER overrides the agent binary the SDK bundles (R12)", () => {
    // Arrange, Act.
    const options = realQueryOptions(spec());

    // Assert.
    expect(options.pathToClaudeCodeExecutable).toBeUndefined();
  });

  it("omits the model entirely when none was requested", () => {
    // Arrange, Act.
    const options = realQueryOptions(spec());

    // Assert.
    expect("model" in options).toBe(false);
  });

  it("states the requested model when one was", () => {
    // Arrange, Act.
    const options = realQueryOptions(spec({ model: "claude-opus-5" }));

    // Assert.
    expect(options.model).toBe("claude-opus-5");
  });

  it("hands the query its cwd, permission mode, gate and abort controller", () => {
    // Arrange.
    const abortController = new AbortController();
    const canUseTool: RealQuerySpec["canUseTool"] = async () => ({
      behavior: "deny",
      message: "no",
    });

    // Act.
    const options = realQueryOptions(
      spec({ cwd: "/ws/feature", permissionMode: "plan", canUseTool, abortController }),
    );

    // Assert.
    expect({
      cwd: options.cwd,
      permissionMode: options.permissionMode,
      canUseTool: options.canUseTool,
      abortController: options.abortController,
    }).toEqual({
      cwd: "/ws/feature",
      permissionMode: "plan",
      canUseTool,
      abortController,
    });
  });
});

describe("realQueryOptions on a resume", () => {
  it("carries the rewind target alongside the resume handle", () => {
    // Arrange, Act — the keep-alive rewind is a vendor option, not a shim
    // intention: it has to reach the SDK to have happened at all.
    const options = realQueryOptions(
      spec({
        binding: { kind: "resume", resumeSessionId: "vendor-old" },
        resumeSessionAt: "msg-uuid-7",
      }),
    );

    // Assert.
    expect({ resume: options.resume, resumeSessionAt: options.resumeSessionAt }).toEqual({
      resume: "vendor-old",
      resumeSessionAt: "msg-uuid-7",
    });
  });

  it("omits the rewind target when the resume named none", () => {
    // Arrange, Act.
    const options = realQueryOptions(
      spec({ binding: { kind: "resume", resumeSessionId: "vendor-old" } }),
    );

    // Assert.
    expect(options.resumeSessionAt).toBeUndefined();
  });
});

/**
 * The factory itself. It routes through the vendor guard, which is the ONE
 * place the SDK can enter this process; the suite mocks that chokepoint so the
 * vendor is never reached and the options the factory hands over are visible.
 */
describe("createRealQuery", () => {
  afterEach(() => {
    vi.doUnmock("../../src/vendor-guard.js");
    vi.resetModules();
  });

  /** The mocked chokepoint, plus the arguments the factory hands the SDK. */
  async function withMockedSdk(): Promise<{
    createRealQuery: typeof import("../../src/sdk/real-query.js").createRealQuery;
    calls: Array<{ prompt: unknown; options: Record<string, unknown> }>;
    sites: string[];
    query: unknown;
  }> {
    const calls: Array<{ prompt: unknown; options: Record<string, unknown> }> = [];
    const sites: string[] = [];
    const query = { interrupt: async (): Promise<void> => {} };
    vi.resetModules();
    vi.doMock("../../src/vendor-guard.js", () => ({
      importRealSDK: (site: string) => {
        sites.push(site);
        return Promise.resolve({
          query: (args: { prompt: unknown; options: Record<string, unknown> }) => {
            calls.push(args);
            return query;
          },
        });
      },
    }));
    const log = await import("../../src/log.js");
    log.configureLog({ fd: 3, cwd: "/ws", workspaceId: "00000000000000ff", agentReplSessionId: "real-query-suite" });
    const mod = await import("../../src/sdk/real-query.js");
    return { createRealQuery: mod.createRealQuery, calls, sites, query };
  }

  it("names its own call site at the vendor chokepoint", async () => {
    // Arrange.
    const { createRealQuery, sites } = await withMockedSdk();

    // Act.
    await createRealQuery(spec(), (async function* () {})());

    // Assert — a blocked call has to say WHICH vendor entry was tripped.
    expect(sites).toEqual(["createRealQuery"]);
  });

  it("hands the SDK the prompt stream and the assembled options", async () => {
    // Arrange.
    const { createRealQuery, calls } = await withMockedSdk();
    const prompt = (async function* () {})();

    // Act.
    await createRealQuery(spec({ binding: { kind: "fresh", sessionId: "vendor-9" } }), prompt);

    // Assert.
    expect(calls).toHaveLength(1);
    expect(calls[0].prompt).toBe(prompt);
    expect(calls[0].options).toMatchObject({ sessionId: "vendor-9", includePartialMessages: true });
  });

  it("names the RESUMED vendor session in the construction record", async () => {
    // Arrange: a resume binding carries its id under a different field than a
    // fresh one, and the record has to name whichever one applies.
    const { createRealQuery } = await withMockedSdk();
    vi.mocked(writeSync).mockClear();

    // Act.
    await createRealQuery(
      spec({ binding: { kind: "resume", resumeSessionId: "vendor-resumed-7" } }),
      (async function* () {})(),
    );

    // Assert.
    const records = (vi.mocked(writeSync).mock.calls as unknown as Array<[number, Buffer, number, number]>)
      .map(([, bytes, offset, length]) =>
        JSON.parse(bytes.subarray(offset, offset + length).toString("utf8")) as {
          message: string;
          context: Record<string, unknown>;
        },
      )
      .filter((record) => record.message === "constructing the real vendor query");
    expect(records.map((record) => record.context.vendor_session_id)).toEqual(["vendor-resumed-7"]);
  });

  it("returns the SDK's own query object rather than a wrapper", async () => {
    // Arrange.
    const { createRealQuery, query } = await withMockedSdk();

    // Act.
    const created = await createRealQuery(spec(), (async function* () {})());

    // Assert.
    expect(created).toBe(query);
  });
});
