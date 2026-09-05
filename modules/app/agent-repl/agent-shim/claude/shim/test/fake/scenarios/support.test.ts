/**
 * test/fake/scenarios/support.test.ts — the scenario helpers' own edges.
 *
 * `ruleTarget` is the one that has a wrong answer available to it: a tool's
 * input is an untyped bag, and the suggested permission rule it feeds is a
 * string the gate round-trips back as a standing arm. A non-string target
 * rendered through `String()` would put the literal text "[object Object]"
 * into that rule.
 */
import { describe, expect, it } from "vitest";

import { ruleTarget } from "../../../src/fake/scenarios/support.js";

describe("ruleTarget", () => {
  it("names the command a call carries", () => {
    // Arrange / Act / Assert.
    expect(ruleTarget({ command: "go test ./..." })).toBe("go test ./...");
  });

  it("falls back to the file path when there is no command", () => {
    // Arrange / Act / Assert.
    expect(ruleTarget({ file_path: "/w/main.go" })).toBe("/w/main.go");
  });

  it("answers no target when the call names none", () => {
    // Arrange / Act / Assert.
    expect(ruleTarget({})).toBe("");
  });

  it("answers no target for a command that is not a string", () => {
    // Arrange / Act / Assert: the shape a `String()` would have rendered as
    // "[object Object]" straight into a permission rule.
    expect(ruleTarget({ command: { argv: ["go", "test"] } })).toBe("");
  });
});
