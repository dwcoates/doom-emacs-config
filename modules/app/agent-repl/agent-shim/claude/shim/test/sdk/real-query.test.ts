/**
 * The real query's OPTIONS are the contract; the factory itself needs the live
 * SDK and is therefore unreachable from a hermetic suite (the vendor guard
 * throws). Every option asserted here is load-bearing per
 * docs/overhaul/shim.md.
 */
import { mkdtempSync, mkdirSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, it } from "vitest";
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
