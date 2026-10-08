import { beforeEach, vi } from "vitest";

vi.mock("node:fs", async (importOriginal) => {
  const actual = await importOriginal<typeof import("node:fs")>();
  return {
    ...actual,
    writeSync: vi.fn((_fd: number, _bytes: Buffer, _offset: number, length: number) => length),
  };
});

// Behavioral suites assert the remediation trace, including debug decisions.
// Production still defaults to info; test/log.test.ts exercises that default
// explicitly with a fresh logger module.
process.env.AGENT_REPL_LOG_LEVEL = "debug";
// Debug is a window, never a standing level (proto/vocab/log-level-window.json).
process.env.AGENT_REPL_LOG_LEVEL_UNTIL = String(Math.floor(Date.now() / 1000) + 300);

const { configureLog } = await import("../src/log.js");
configureLog({ fd: 3, cwd: "/test/workspace", workspaceId: "00000000000000aa", agentReplSessionId: "test-agent-session" });

// Normal shim records deliberately echo to stderr in production. Suppress
// those expected records centrally so ordinary behavioral tests do not flood
// coverage output. Logging-specific tests install their own spy when they
// need to assert terminal output.
beforeEach(() => {
  vi.spyOn(process.stderr, "write").mockImplementation(() => true);
});
