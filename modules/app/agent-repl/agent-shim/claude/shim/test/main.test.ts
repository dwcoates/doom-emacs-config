import { describe, expect, it } from "vitest";
import { parseArgs, probeQueryOptions, realQueryOptions, validateUdsLoggingArgs } from "../src/main.js";

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
