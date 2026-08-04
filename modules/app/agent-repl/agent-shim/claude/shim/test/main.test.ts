import { afterEach, describe, expect, it, vi } from "vitest";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import {
  makeUdsQueryFactory,
  parseArgs,
  probeQueryOptions,
  realQueryOptions,
  udsShutdownSignalHandlers,
  validateUdsLoggingArgs,
} from "../src/main.js";
import { METAPROMPT_REL_PATH } from "../src/metaprompt.js";

const metapromptRoots: string[] = [];
afterEach(() => {
  metapromptRoots.splice(0).forEach((r) => fs.rmSync(r, { recursive: true, force: true }));
});

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

  it("appends the cwd's metaprompt to the system prompt", () => {
    // Arrange: a checkout carrying a metaprompt at the in-repo path.
    const root = fs.mkdtempSync(path.join(os.tmpdir(), "shim-main-metaprompt-"));
    metapromptRoots.push(root);
    const file = path.join(root, METAPROMPT_REL_PATH);
    fs.mkdirSync(path.dirname(file), { recursive: true });
    fs.writeFileSync(file, "Answer as a TLDR tree.\n", "utf8");
    // Act
    const opts = realQueryOptions(parseArgs(["--session-id", "s1", "--cwd", root]), noopCanUse);
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
