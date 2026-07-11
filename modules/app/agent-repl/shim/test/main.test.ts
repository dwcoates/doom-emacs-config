import { describe, expect, it } from "vitest";
import { parseArgs, realQueryOptions } from "../src/main.js";

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
