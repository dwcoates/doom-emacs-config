import { describe, expect, it } from "vitest";
import { parseArgs } from "../src/main.js";

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
