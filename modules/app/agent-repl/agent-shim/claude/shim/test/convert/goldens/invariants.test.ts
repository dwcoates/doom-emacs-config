/**
 * The RULES the fold owes every capture, asserted against all sixty-nine.
 *
 * The per-scenario tables in `scenarios.test.ts` say WHAT each recording folds
 * into. This file says what must be true of ALL of them, which is where a
 * regression that only shows up on one unlucky vendor shape gets caught: a
 * converter defect anywhere, a built-in landing as `AgentUnmodeled`, usage
 * double-counted across a response's units, an exempt tool leaking a frame.
 */
import { describe, expect, it } from "vitest";
import {
  EXEMPT_TOOLS,
  ENGINE_OWNED_TOOLS,
} from "../../../src/convert/tool-calls.js";
import { TOOL_CONVERTERS } from "../../../src/convert/tools/registry.js";
import {
  activityOf,
  captureMeta,
  foldScenario,
  residueKeys,
  scenarioNames,
  sdkMessages,
  subagentFiles,
  toolUses,
} from "./harness.js";

const SCENARIOS = scenarioNames();

describe("the fold never degrades on a real capture", () => {
  it.each(SCENARIOS)("%s reports no converter defect", (scenario) => {
    expect(foldScenario(scenario).faults).toEqual([]);
  });

  it.each(SCENARIOS)("%s writes no unparsed residue", (scenario) => {
    const unparsed = residueKeys(foldScenario(scenario)).filter((key) =>
      key.startsWith("unparsed/"),
    );
    expect(unparsed).toEqual([]);
  });

  it.each(SCENARIOS)("%s writes no unknown-discriminator residue", (scenario) => {
    const unknown = residueKeys(foldScenario(scenario)).filter((key) => key.startsWith("unknown/"));
    expect(unknown).toEqual([]);
  });

  it.each(SCENARIOS)("%s was itself a clean capture", (scenario) => {
    expect(captureMeta(scenario).failure_reasons).toEqual([]);
  });
});

describe("the exempt set is dropped silently", () => {
  it("the captures actually exercise it", () => {
    const exercised = SCENARIOS.flatMap((scenario) => toolUses(scenario))
      .map((call) => call.name)
      .filter((name) => EXEMPT_TOOLS.has(name));
    expect(exercised).toContain("ToolSearch");
  });

  it.each(SCENARIOS)("%s produces no unit for an exempt call", (scenario) => {
    const exempt = new Set(
      toolUses(scenario)
        .filter((call) => EXEMPT_TOOLS.has(call.name))
        .map((call) => call.id),
    );
    const leaked = foldScenario(scenario)
      .entries.filter((entry) => exempt.has(entry.upsertKey.replace(/^activity:/, "")))
      .map((entry) => entry.source.discriminator);
    expect(leaked).toEqual([]);
  });

  it.each(SCENARIOS)("%s produces no residue for an exempt call", (scenario) => {
    const names = toolUses(scenario).map((call) => call.name);
    if (!names.some((name) => EXEMPT_TOOLS.has(name))) return;
    expect(residueKeys(foldScenario(scenario))).not.toContain("unknown/tool_use");
  });
});

describe("the engine's gate keeps its own units", () => {
  it("the captures actually exercise it", () => {
    const asked = SCENARIOS.flatMap((scenario) => toolUses(scenario)).map((call) => call.name);
    expect(asked).toContain("AskUserQuestion");
  });

  it.each(SCENARIOS)("%s leaves an engine-owned call to the gate", (scenario) => {
    const owned = new Set(
      toolUses(scenario)
        .filter((call) => ENGINE_OWNED_TOOLS.has(call.name))
        .map((call) => call.id),
    );
    const leaked = foldScenario(scenario)
      .entries.filter((entry) => owned.has(entry.upsertKey.replace(/^activity:/, "")))
      .map((entry) => entry.source.discriminator);
    expect(leaked).toEqual([]);
  });
});

describe("AgentUnmodeled means a genuinely unknown tool", () => {
  /** The names that reached `unmodeled` across every capture. */
  function unmodeledNames(scenario: string): string[] {
    const byId = new Map(toolUses(scenario).map((call) => [call.id, call.name]));
    const names: string[] = [];
    for (const entry of foldScenario(scenario).entries) {
      if (activityOf(entry)?.item.case !== "unmodeled") continue;
      const name = byId.get(entry.upsertKey.replace(/^activity:/, ""));
      if (name !== undefined && !names.includes(name)) names.push(name);
    }
    return names;
  }

  it.each(SCENARIOS)("%s never files a modelled built-in as unmodeled", (scenario) => {
    for (const name of unmodeledNames(scenario)) {
      expect(TOOL_CONVERTERS.has(name)).toBe(false);
    }
  });

  it.each(SCENARIOS)("%s never files an exempt built-in as unmodeled", (scenario) => {
    for (const name of unmodeledNames(scenario)) {
      expect(EXEMPT_TOOLS.has(name)).toBe(false);
    }
  });

  it("the MCP tool the captures exercised lands there", () => {
    expect(unmodeledNames("mcp-unmodeled-tool")).toEqual([
      "mcp__capture-probe__echo",
      "mcp__capture-probe__slow",
    ]);
  });

  it("an SDK tool with no conversation arm lands there", () => {
    expect(unmodeledNames("turn-stop-max-structured-output-retries")).toEqual(["StructuredOutput"]);
  });
});

describe("usage rides the first block's unit and no other", () => {
  /** Every unit id that carried usage, per API response id. */
  function usageCarriers(scenario: string): string[] {
    const carriers: string[] = [];
    for (const entry of foldScenario(scenario).entries) {
      const activity = activityOf(entry);
      if (activity?.usage === undefined) continue;
      const id = entry.upsertKey.replace(/^activity:/, "");
      if (!carriers.includes(id)) carriers.push(id);
    }
    return carriers;
  }

  it.each(SCENARIOS)("%s stamps usage on a response's FIRST block", (scenario) => {
    for (const carrier of usageCarriers(scenario)) {
      // A block unit spells its index; a first block that is a TOOL CALL is
      // named by the vendor's `tool_use_id` instead and carries no index at all,
      // so the rule is "index 0 when there is an index", never "always `:0`".
      if (/:\d+$/.test(carrier)) expect(carrier).toMatch(/:0$/);
      else expect(carrier.startsWith("toolu_")).toBe(true);
    }
  });

  it.each(SCENARIOS)("%s stamps usage at most once per API response", (scenario) => {
    const carriers = usageCarriers(scenario);
    const responses = carriers.map((id) => id.replace(/:0$/, ""));
    expect(new Set(responses).size).toBe(carriers.length);
  });

  it("a streamed response carries usage at all", () => {
    // REGRESSION GUARD: the streamed path once numbered an `assistant` line's
    // block past the block it restated, so no unit was ever index 0 and usage
    // attached to nothing on every streamed turn.
    expect(usageCarriers("prose-streamed")).toEqual(["msg_011CedLYEopgEZcA1uX224Mi:0"]);
  });

  it("a streamed unit settles under the identity it started with", () => {
    const run = foldScenario("prose-streamed");
    const started = run.entries
      .filter((entry) => entry.source.discriminator === "activity.response.start")
      .map((entry) => entry.upsertKey);
    const settled = run.entries
      .filter((entry) => entry.source.discriminator === "activity.response.success")
      .map((entry) => entry.upsertKey);
    expect(settled).toEqual(started);
  });
});

describe("the identity joins the vendor's own files state", () => {
  it("a tool result settles the unit its call announced", () => {
    const scenario = "read-whole-head-range";
    const announced = new Set(toolUses(scenario).map((call) => call.id));
    const settled = foldScenario(scenario)
      .entries.filter((entry) => entry.source.discriminator === "activity.read.success")
      .map((entry) => entry.upsertKey.replace(/^activity:/, ""));
    expect(settled.length).toBe(3);
    for (const id of settled) expect(announced.has(id)).toBe(true);
  });

  it("a subagent's file names the call that spawned it", () => {
    const files = subagentFiles("subagent-sync-nested-activity");
    expect(files.length).toBeGreaterThan(0);
    const announced = new Set(toolUses("subagent-sync-nested-activity").map((call) => call.id));
    for (const file of files) {
      expect(file.toolUseId).toBeDefined();
      expect(announced.has(file.toolUseId as string)).toBe(true);
    }
  });

  it("the vendor keys a subagent file by a seventeen-hex agent id", () => {
    for (const file of subagentFiles("subagent-sync-nested-activity")) {
      expect(file.agentId).toMatch(/^[0-9a-f]{17}$/);
    }
  });

  it("the subagent unit is filed under the spawning call, not the agent id", () => {
    const spawn = toolUses("subagent-sync-nested-activity").find((call) => call.name === "Agent");
    expect(spawn).toBeDefined();
    const settled = foldScenario("subagent-sync-nested-activity")
      .entries.filter((entry) => activityOf(entry)?.item.case === "subagent")
      .map((entry) => entry.upsertKey);
    expect(settled).toContain(`activity:${(spawn as { id: string }).id}`);
  });

  it("the spawning tool is named `Agent` on the wire", () => {
    const names = SCENARIOS.flatMap((scenario) => toolUses(scenario)).map((call) => call.name);
    expect(names).toContain("Agent");
    expect(names).not.toContain("Task");
  });

  it("`Task` is nonetheless what the session advertises", () => {
    const init = sdkMessages("subagent-sync-nested-activity").find(
      (message) => (message as { subtype?: string }).subtype === "init",
    ) as unknown as { tools: string[] } | undefined;
    expect(init?.tools).toContain("Task");
  });
});

describe("a detached shell task settles as a shell, never as a subagent", () => {
  it("the ctrl-b shell detach writes no subagent frame", () => {
    const kinds = foldScenario("ctrl-b-detach-of-foreground-work").entries.map(
      (entry) => activityOf(entry)?.item.case,
    );
    expect(kinds).not.toContain("subagent");
  });

  it("the ctrl-b subagent detach still writes one", () => {
    const kinds = foldScenario("ctrl-b-detach-of-foreground-subagent").entries.map(
      (entry) => activityOf(entry)?.item.case,
    );
    expect(kinds).toContain("subagent");
  });
});

describe("the residue the captures actually produce", () => {
  it("an unmodelled system subtype is recorded as vendor-specific", () => {
    expect(residueKeys(foldScenario("turn-stop-hook-stop"))).toEqual([
      "vendor_specific/system/notification",
    ]);
  });

  it("the worktree run's vcs beat is recorded as vendor-specific", () => {
    expect(residueKeys(foldScenario("worktree-enter-exit-kept-and-removed"))).toEqual([
      "vendor_specific/system/vcs_state_changed",
    ]);
  });
});
