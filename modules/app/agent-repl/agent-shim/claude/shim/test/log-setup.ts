import { beforeEach, vi } from "vitest";

vi.mock("node:fs", async (importOriginal) => {
  const actual = await importOriginal<typeof import("node:fs")>();
  return {
    ...actual,
    writeSync: vi.fn((_fd: number, _bytes: Buffer, _offset: number, length: number) => length),
  };
});

const { configureLog } = await import("../src/uds/log.js");
configureLog({ fd: 3, cwd: "/test/workspace", agentReplSessionId: "test-agent-session" });

// Normal shim records deliberately echo to stderr in production. Suppress
// those expected records centrally so ordinary behavioral tests do not flood
// coverage output. Logging-specific tests install their own spy when they
// need to assert terminal output.
beforeEach(() => {
  vi.spyOn(process.stderr, "write").mockImplementation(() => true);
});
