/**
 * The vendor tool converter registry (src/convert/tools/registry.ts).
 */
import { describe, expect, it } from "vitest";
import { spawnsSubagent } from "../../../src/convert/tools/registry.js";

describe("spawnsSubagent", () => {
  it.each(["Agent", "Task"])("answers a call of %s spawns a subagent", (name) => {
    expect(spawnsSubagent(name)).toBe(true);
  });

  it.each(["Bash", "SendMessage", "NoSuchTool"])("answers a call of %s spawns none", (name) => {
    expect(spawnsSubagent(name)).toBe(false);
  });
});
