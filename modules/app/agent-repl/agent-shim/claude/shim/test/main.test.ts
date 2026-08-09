import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { metapromptPath } from "../src/metaprompt.js";

// runUdsMode's own dependencies (the session lock and the owned SDK session)
// are mocked so its exit-trace test below exercises only the signal/exit
// wiring runUdsMode itself owns, never a real lock file or SDK session.
const udsSessionMocks = vi.hoisted(() => ({
  start: vi.fn(async (): Promise<void> => undefined),
}));
// Both claims are doubled, since runUdsMode takes the session lock AND the
// workspace lock before it constructs anything. The doubles keep the real
// exports' contract — a refusal to run is an exception, and success hands back
// an idempotent release — while the pure helpers (lockPath, workspaceLockKey,
// …) stay real so a drift between double and module surfaces here rather than
// silently.
vi.mock("../src/uds/session-lock.js", async (importOriginal) => {
  const actual = await importOriginal<typeof import("../src/uds/session-lock.js")>();
  return {
    ...actual,
    acquireSessionLock: vi.fn((sessionId: string) => {
      if (sessionId === "") throw new Error("shim-session-lock: a session lock needs a session id");
      return releaseDouble();
    }),
    // Faithful to the real export, which derives the lock file through
    // workspaceLockKey and so refuses an empty workspace directory outright.
    acquireWorkspaceLock: vi.fn((cwd: string) => {
      actual.workspaceLockKey(cwd);
      return releaseDouble();
    }),
  };
});

/** A release that, like the real one, is idempotent — extra calls are no-ops. */
function releaseDouble(): () => void {
  return vi.fn(() => undefined);
}
vi.mock("../src/uds/uds-session.js", async (importOriginal) => {
  const actual = await importOriginal<typeof import("../src/uds/uds-session.js")>();
  return {
    ...actual,
    UdsSession: vi.fn().mockImplementation(() => ({
      start: udsSessionMocks.start,
      shutdown: vi.fn(async (): Promise<void> => undefined),
    })),
  };
});

import {
  makeUdsQueryFactory,
  MAIN_LIFECYCLE_OPERATION,
  parseArgs,
  probeQueryOptions,
  realQueryOptions,
  logMainLifecycle,
  runUdsMode,
  udsShutdownSignalHandlers,
  validateUdsLoggingArgs,
} from "../src/main.js";

// The metaprompt is resolved from the canonical doom checkout under HOME, so
// every test in this file runs against a throwaway home. Without the swap the
// options builders would read the developer's real metaprompt and the
// assertions below would depend on a file outside the repository.
const metapromptHomes: string[] = [];
let realHome: string | undefined;
beforeEach(() => {
  realHome = process.env.HOME;
  process.env.HOME = makeMetapromptHome();
});
afterEach(() => {
  if (realHome === undefined) delete process.env.HOME;
  else process.env.HOME = realHome;
  metapromptHomes.splice(0).forEach((r) => fs.rmSync(r, { recursive: true, force: true }));
});

/** A throwaway HOME, optionally carrying a doom checkout holding BODY. */
function makeMetapromptHome(body?: string): string {
  const home = fs.mkdtempSync(path.join(os.tmpdir(), "shim-main-metaprompt-"));
  metapromptHomes.push(home);
  if (body !== undefined) {
    const file = metapromptPath(home);
    fs.mkdirSync(path.dirname(file), { recursive: true });
    fs.writeFileSync(file, body, "utf8");
  }
  return home;
}

describe("parseArgs", () => {
  it("defaults to real SDK mode with a generated session id and default mode", () => {
    // Arrange + Act
    const args = parseArgs([]);
    // Assert
    expect(args.fake).toBe(false);
    expect(args.permissionMode).toBe("default");
    expect(args.sessionId).toMatch(/[0-9a-f-]{36}/);
  });

  it("parses every supported flag", () => {
    // Arrange + Act
    const args = parseArgs([
      "--fake",
      "--session-id", "sess-42",
      "--permission-mode", "acceptEdits",
      "--cwd", "/tmp/x",
      "--model", "opus",
      "--resume", "prior-session",
    ]);
    // Assert
    expect(args).toEqual({
      fake: true,
      sessionId: "sess-42",
      permissionMode: "acceptEdits",
      cwd: "/tmp/x",
      model: "opus",
      resume: "prior-session",
    });
  });

  it("throws on an invalid permission mode", () => {
    // Arrange + Act + Assert
    expect(() => parseArgs(["--permission-mode", "yolo"])).toThrow(/invalid --permission-mode/);
  });

  it("throws on an unknown argument", () => {
    // Arrange + Act + Assert
    expect(() => parseArgs(["--frob"])).toThrow(/unknown argument/);
  });

  it("throws when a flag is missing its value", () => {
    // Arrange + Act + Assert
    expect(() => parseArgs(["--session-id"])).toThrow(/missing value/);
  });
});

describe("parseArgs rewind lineage", () => {
  /** The daemon's complete rewind spawn contract, minus the flag under test. */
  const LINEAGE = [
    "--resume", "new-vendor",
    "--rewound-from", "old-vendor",
    "--rewind-retained-leaf", "leaf-uuid",
    "--rewind-dropped-turns", "t1,t2,t3",
  ];

  it("accepts the complete trio alongside --resume", () => {
    // Arrange + Act
    const args = parseArgs(["--session-id", "s1", ...LINEAGE]);
    // Assert
    expect(args.rewoundFrom).toBe("old-vendor");
    expect(args.rewindRetainedLeaf).toBe("leaf-uuid");
    expect(args.rewindDroppedTurns).toEqual(["t1", "t2", "t3"]);
  });

  it("preserves dropped-turn submission order rather than sorting", () => {
    // Arrange + Act
    const args = parseArgs([
      "--resume", "new-vendor",
      "--rewound-from", "old-vendor",
      "--rewind-retained-leaf", "leaf-uuid",
      "--rewind-dropped-turns", "t9,t1,t5",
    ]);
    // Assert — KeepAliveDiscard.dropped_turn_ids is "in submission order".
    expect(args.rewindDroppedTurns).toEqual(["t9", "t1", "t5"]);
  });

  it("parses a single dropped turn id with no separator", () => {
    // Arrange + Act
    const args = parseArgs([
      "--resume", "new-vendor",
      "--rewound-from", "old-vendor",
      "--rewind-retained-leaf", "leaf-uuid",
      "--rewind-dropped-turns", "only-turn",
    ]);
    // Assert
    expect(args.rewindDroppedTurns).toEqual(["only-turn"]);
  });

  it("leaves the lineage absent when no rewind flag is supplied", () => {
    // Arrange + Act
    const args = parseArgs(["--session-id", "s1", "--resume", "new-vendor"]);
    // Assert
    expect(args.rewoundFrom).toBeUndefined();
    expect(args.rewindRetainedLeaf).toBeUndefined();
    expect(args.rewindDroppedTurns).toBeUndefined();
  });

  it("fails loudly when --rewound-from arrives without its two companions", () => {
    // Arrange + Act + Assert
    expect(() => parseArgs(["--resume", "new-vendor", "--rewound-from", "old-vendor"]))
      .toThrow(/incomplete rewind lineage/);
  });

  it("fails loudly when only --rewind-retained-leaf is missing", () => {
    // Arrange + Act + Assert
    expect(() => parseArgs([
      "--resume", "new-vendor",
      "--rewound-from", "old-vendor",
      "--rewind-dropped-turns", "t1",
    ])).toThrow(/--rewind-retained-leaf/);
  });

  it("fails loudly when only --rewind-dropped-turns is missing", () => {
    // Arrange + Act + Assert
    expect(() => parseArgs([
      "--resume", "new-vendor",
      "--rewound-from", "old-vendor",
      "--rewind-retained-leaf", "leaf-uuid",
    ])).toThrow(/--rewind-dropped-turns/);
  });

  it("fails loudly when the complete lineage arrives without --resume", () => {
    // Arrange + Act + Assert
    expect(() => parseArgs([
      "--rewound-from", "old-vendor",
      "--rewind-retained-leaf", "leaf-uuid",
      "--rewind-dropped-turns", "t1",
    ])).toThrow(/rewind lineage requires --resume/);
  });

  it("fails loudly when the rewind names one vendor session on both sides", () => {
    // Arrange + Act + Assert
    expect(() => parseArgs([
      "--resume", "same-vendor",
      "--rewound-from", "same-vendor",
      "--rewind-retained-leaf", "leaf-uuid",
      "--rewind-dropped-turns", "t1",
    ])).toThrow(/--rewound-from equals --resume/);
  });

  it("rejects an empty dropped-turn id rather than dropping it silently", () => {
    // Arrange + Act + Assert
    expect(() => parseArgs([
      "--resume", "new-vendor",
      "--rewound-from", "old-vendor",
      "--rewind-retained-leaf", "leaf-uuid",
      "--rewind-dropped-turns", "t1,,t2",
    ])).toThrow(/every comma-separated turn id must be non-empty/);
  });

  it("rejects an entirely empty dropped-turn list", () => {
    // Arrange + Act + Assert — a rewind that dropped nothing is not a rewind.
    expect(() => parseArgs([
      "--resume", "new-vendor",
      "--rewound-from", "old-vendor",
      "--rewind-retained-leaf", "leaf-uuid",
      "--rewind-dropped-turns", "",
    ])).toThrow(/every comma-separated turn id must be non-empty/);
  });
});

describe("realQueryOptions", () => {
  const noopCanUse = (async () => ({ behavior: "allow" as const, updatedInput: {} })) as never;

  it("requests interactive-CLI parity (claude_code preset + all setting sources)", () => {
    // Arrange
    const args = parseArgs(["--session-id", "s1"]);
    // Act
    const opts = realQueryOptions(args, noopCanUse);
    // Assert — without the preset the model has no environment block
    // (and invents paths like /Users/user for `~`); without the
    // sources the user's settings/hooks/CLAUDE.md never load.
    expect(opts.systemPrompt).toEqual({ type: "preset", preset: "claude_code" });
    expect(opts.settingSources).toEqual(["user", "project", "local"]);
    expect(opts.includePartialMessages).toBe(true);
  });

  it("appends the canonical metaprompt to the system prompt", () => {
    // Arrange: a home carrying the doom checkout's metaprompt, and a session
    // rooted somewhere else entirely.
    process.env.HOME = makeMetapromptHome("Answer as a TLDR tree.\n");
    const cwd = makeMetapromptHome();
    // Act
    const opts = realQueryOptions(parseArgs(["--session-id", "s1", "--cwd", cwd]), noopCanUse);
    // Assert — the guidelines reach the agent as system prompt rather than
    // as a read-directive injected into the conversation, so they survive
    // `/clear`, `/compact`, and resume without anything re-firing them.
    expect(opts.systemPrompt).toEqual({
      type: "preset",
      preset: "claude_code",
      append: "Answer as a TLDR tree.",
    });
  });

  it("passes cwd/model/resume through only when provided", () => {
    // Arrange
    const bare = realQueryOptions(parseArgs(["--session-id", "s1"]), noopCanUse);
    const full = realQueryOptions(
      parseArgs(["--session-id", "s1", "--cwd", "/w", "--model", "haiku", "--resume", "cli-1"]),
      noopCanUse,
    );
    // Assert
    expect("cwd" in bare).toBe(false);
    expect("model" in bare).toBe(false);
    expect("resume" in bare).toBe(false);
    expect(full.cwd).toBe("/w");
    expect(full.model).toBe("haiku");
    expect(full.resume).toBe("cli-1");
  });

  it("passes the UDS session-owned abort controller to the Agent SDK", () => {
    // Arrange
    const abortController = new AbortController();
    // Act
    const opts = realQueryOptions(parseArgs(["--session-id", "s1"]), noopCanUse, abortController);
    // Assert
    expect(opts.abortController).toBe(abortController);
  });

  it("omits empty and synthetic model overrides identically", () => {
    // Arrange / Act
    const empty = realQueryOptions(
      parseArgs(["--session-id", "s1", "--model", ""]),
      noopCanUse,
    );
    const synthetic = realQueryOptions(
      parseArgs(["--session-id", "s1", "--model", "<synthetic>"]),
      noopCanUse,
    );
    // Assert
    expect("model" in empty).toBe(false);
    expect("model" in synthetic).toBe(false);
  });
});

describe("probeQueryOptions", () => {
  it("keeps the session's setting sources so the probe resolves the same skills", () => {
    // Arrange — without settingSources the CLI resolves only the 8
    // built-ins, so the probe would offer a menu the session cannot match.
    const args = parseArgs(["--session-id", "s1", "--cwd", "/w"]);
    // Act
    const opts = probeQueryOptions(args, new AbortController());
    // Assert
    expect(opts.settingSources).toEqual(["user", "project", "local"]);
    expect(opts.cwd).toBe("/w");
  });

  it("drops resume, since command resolution never reads the transcript", () => {
    // Arrange — resuming would only point a second process at the live
    // session's transcript for a list that comes from disk and settings.
    const args = parseArgs(["--session-id", "s1", "--resume", "cli-1"]);
    // Act
    const opts = probeQueryOptions(args, new AbortController());
    // Assert
    expect("resume" in opts).toBe(false);
  });

  it("wires the abort controller so the probe's child can be reaped", () => {
    // Arrange — a Query exposes no close(), so aborting the controller is
    // the only way to SIGTERM the `claude` child the probe spawns.
    const controller = new AbortController();
    // Act
    const opts = probeQueryOptions(parseArgs(["--session-id", "s1"]), controller);
    // Assert
    expect(opts.abortController).toBe(controller);
  });
});

describe("makeUdsQueryFactory", () => {
  it("gives fake UDS query shutdown one abort capability that ends its stream", async () => {
    // Arrange
    const factory = makeUdsQueryFactory(parseArgs(["--fake", "--session-id", "s1"]));
    const prompt = (async function* () {})() as never;
    const canUseTool = (async () => ({ behavior: "allow" as const, updatedInput: {} })) as never;
    const owned = factory(prompt, canUseTool);
    const iterator = owned.query[Symbol.asyncIterator]();
    await iterator.next(); // fake SDK init
    const next = iterator.next();
    // Act
    owned.abort();
    // Assert
    await expect(next).resolves.toMatchObject({ done: true });
  });
});

describe("UDS query signal ownership", () => {
  it("allows SIGTERM to end the query exactly once", async () => {
    const shutdown = vi.fn(async () => undefined);
    const signals = udsShutdownSignalHandlers("sess-1", shutdown);

    signals.onSigterm();
    signals.onSigterm();

    await expect(signals.stopping()).resolves.toBeUndefined();
    expect(shutdown).toHaveBeenCalledTimes(1);
    expect(shutdown).toHaveBeenCalledWith("SIGTERM");
  });

  it("refuses SIGINT without ending the query and logs the preserved owner", () => {
    const records: Array<Record<string, unknown>> = [];
    const stderr = vi.spyOn(process.stderr, "write").mockImplementation(((chunk: unknown): boolean => {
      records.push(JSON.parse(String(chunk)) as Record<string, unknown>);
      return true;
    }) as typeof process.stderr.write);
    const shutdown = vi.fn(async () => undefined);
    const signals = udsShutdownSignalHandlers("sess-1", shutdown);

    signals.onSigint();

    expect(signals.stopping()).toBeNull();
    expect(shutdown).not.toHaveBeenCalled();
    expect(records).toContainEqual(expect.objectContaining({
      level: "error",
      agent_repl_session_id: "test-agent-session",
      message: "refused unauthorized signal as an SDK query shutdown condition",
      context: expect.objectContaining({
        signal: "SIGINT",
        outcome: "refused_query_termination",
        query_preserved: true,
      }),
    }));
    stderr.mockRestore();
  });
});

describe("runUdsMode exit trace", () => {
  function spyStderr(): { records: Array<Record<string, unknown>>; restore: () => void } {
    const records: Array<Record<string, unknown>> = [];
    const stderr = vi.spyOn(process.stderr, "write").mockImplementation(((chunk: unknown): boolean => {
      records.push(JSON.parse(String(chunk)) as Record<string, unknown>);
      return true;
    }) as typeof process.stderr.write);
    return { records, restore: () => stderr.mockRestore() };
  }

  const noopCreateQuery = (() => ({})) as unknown as Parameters<typeof runUdsMode>[1];

  it("logs a clean exit when a signal drives shutdown before session.start() resolves", async () => {
    // Arrange: the owned session's start() resolves only after SIGTERM fires,
    // exactly as a real UdsSession's bring-up racing a signal would.
    udsSessionMocks.start.mockImplementation(async () => {
      process.emit("SIGTERM", "SIGTERM");
    });
    const { records, restore } = spyStderr();
    const args = parseArgs(["--session-id", "sess-clean", "--daemon-socket", "/tmp/d.sock", "--cwd", "/tmp", "--log-fd", "3"]);

    // Act
    await runUdsMode(args, noopCreateQuery);

    // Assert
    expect(records).toContainEqual(expect.objectContaining({
      message: "runUdsMode exiting",
      context: expect.objectContaining({ outcome: "uds_main_exit_clean" }),
    }));
    restore();
  });

  it("logs an error exit and rethrows when the session ends without an intentional shutdown", async () => {
    // Arrange: start() resolves with no signal ever fired.
    udsSessionMocks.start.mockImplementation(async () => undefined);
    const { records, restore } = spyStderr();
    const args = parseArgs(["--session-id", "sess-error", "--daemon-socket", "/tmp/d.sock", "--cwd", "/tmp", "--log-fd", "3"]);

    // Act + Assert
    await expect(runUdsMode(args, noopCreateQuery)).rejects.toThrow(/without an intentional shutdown signal/);
    expect(records).toContainEqual(expect.objectContaining({
      level: "error",
      message: "runUdsMode exiting",
      context: expect.objectContaining({
        outcome: "uds_main_exit_error",
        error: expect.stringContaining("without an intentional shutdown signal"),
      }),
    }));
    restore();
  });
});

describe("main entrypoint lifecycle log contract", () => {
  it.each([
    ["normalized launch model", { requested_model: "<synthetic>", effective_model: "" }, "normalized empty-equivalent launch model before constructing SDK options", "info"],
    ["validated startup", { fake: true, daemon_socket: "/tmp/daemon.sock" }, "validated shim startup arguments and configured durable logging", "info"],
    ["selected query implementation", { query_source: "fake" }, "selecting shim query implementation", "info"],
    ["acquired session lock", {}, "exclusive session lock acquired", "info"],
    ["authorized shutdown signal", { signal: "SIGTERM", outcome: "intentional_query_shutdown" }, "received authorized shim shutdown signal", "info"],
    ["refused unauthorized signal", { level: "error", signal: "SIGINT", outcome: "refused_query_termination", query_preserved: true }, "refused unauthorized signal as an SDK query shutdown condition", "error"],
  ])("assigns %s a stable lifecycle operation and %s level", (_caseName, fields, message, expectedLevel) => {
    const records: Array<Record<string, unknown>> = [];
    const stderr = vi.spyOn(process.stderr, "write").mockImplementation(((chunk: unknown): boolean => {
      records.push(JSON.parse(String(chunk)) as Record<string, unknown>);
      return true;
    }) as typeof process.stderr.write);

    logMainLifecycle(fields, message);

    expect(records).toContainEqual(expect.objectContaining({
      operation: MAIN_LIFECYCLE_OPERATION,
      level: expectedLevel,
      message,
    }));
    stderr.mockRestore();
  });
});

describe("CLI-era flags", () => {
  it("accepts permission-mode auto and friends", () => {
    // Arrange + Act + Assert
    expect(parseArgs(["--permission-mode", "auto"]).permissionMode).toBe("auto");
    expect(parseArgs(["--permission-mode", "dontAsk"]).permissionMode).toBe("dontAsk");
    expect(() => parseArgs(["--permission-mode", "yolo"])).toThrow("invalid");
  });

  it("threads --claude-bin into pathToClaudeCodeExecutable", () => {
    // Arrange
    const noopCanUse = (async () => ({ behavior: "allow" as const, updatedInput: {} })) as never;
    // Act
    const withBin = realQueryOptions(
      parseArgs(["--session-id", "s1", "--claude-bin", "/usr/local/bin/claude"]),
      noopCanUse,
    );
    const withoutBin = realQueryOptions(parseArgs(["--session-id", "s1"]), noopCanUse);
    // Assert
    expect(withBin.pathToClaudeCodeExecutable).toBe("/usr/local/bin/claude");
    expect("pathToClaudeCodeExecutable" in withoutBin).toBe(false);
  });
});

describe("UDS durable-log CLI contract", () => {
  it("accepts inherited fd 3 only", () => {
    const args = parseArgs(["--daemon-socket", "/tmp/daemon.sock", "--cwd", "/canonical", "--log-fd", "3"]);
    expect(args.logFd).toBe(3);
    expect(() => validateUdsLoggingArgs(args)).not.toThrow();
  });

  it.each([
    [["--log-fd", "4"], "invalid --log-fd"],
    [["--log-fd", "wat"], "invalid --log-fd"],
    [["--daemon-socket", "/tmp/daemon.sock", "--log-fd", "3"], "requires --cwd"],
    [["--daemon-socket", "/tmp/daemon.sock", "--cwd", "/canonical"], "requires --log-fd 3"],
  ])("rejects UDS logging configuration %j", (argv, expected) => {
    if (expected === "invalid --log-fd") expect(() => parseArgs(argv)).toThrow(expected);
    else expect(() => validateUdsLoggingArgs(parseArgs(argv))).toThrow(expected);
  });
});
