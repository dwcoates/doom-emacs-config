/**
 * The RULES the fold owes every capture, asserted against all sixty-nine.
 *
 * The per-scenario tables in `scenarios.test.ts` say WHAT each recording folds
 * into. This file says what must be true of ALL of them, which is where a
 * regression that only shows up on one unlucky vendor shape gets caught: a
 * converter defect anywhere, a built-in landing as `AgentUnmodeled`, usage
 * double-counted across a response's units, an exempt tool leaking a frame.
 */
import { writeSync } from "node:fs";
import { toJson } from "@bufbuild/protobuf";
import { describe, expect, it, vi } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import {
  EXEMPT_TOOLS,
  ENGINE_OWNED_TOOLS,
} from "../../../src/convert/tool-calls.js";
import { TOOL_CONVERTERS } from "../../../src/convert/tools/registry.js";
import {
  activityOf,
  arms,
  captureMeta,
  foldScenario,
  residueKeys,
  scenarioNames,
  sdkMessages,
  sessionUpdateArms,
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

  it.each(SCENARIOS)("%s leaves no streamed unit started and unsettled", (scenario) => {
    const before = vi.mocked(writeSync).mock.calls.length;
    foldScenario(scenario);
    const calls = vi.mocked(writeSync).mock.calls.slice(before) as unknown as Array<
      [number, Buffer, number, number]
    >;
    const unsettled = calls
      .map(([, bytes, offset, length]) =>
        JSON.parse(bytes.subarray(offset, offset + length).toString("utf8")) as { message: string },
      )
      .filter((record) => record.message.startsWith("invariant violated: a streamed unit"));
    expect(unsettled).toEqual([]);
  });

  it.each(SCENARIOS)("%s was itself a clean capture", (scenario) => {
    expect(captureMeta(scenario).failure_reasons).toEqual([]);
  });
});

/**
 * The captures that actually make an exempt call.
 *
 * NO VACUOUS ROWS. Running the assertion over every capture made most rows
 * pass on an EMPTY filtered list — they asserted nothing at all, and the suite
 * would have gone on green if the exempt set stopped being exercised anywhere.
 * The captures that exercise it are named here, the assertion runs over those,
 * and the partition itself is asserted below so a capture that stops making an
 * exempt call fails rather than quietly dropping out of the run.
 */
const WITH_EXEMPT_CALLS = SCENARIOS.filter((scenario) =>
  toolUses(scenario).some((call) => EXEMPT_TOOLS.has(call.name)),
);

describe("the exempt set is dropped silently", () => {
  it("the captures actually exercise it", () => {
    const exercised = SCENARIOS.flatMap((scenario) => toolUses(scenario))
      .map((call) => call.name)
      .filter((name) => EXEMPT_TOOLS.has(name));
    expect(exercised).toContain("ToolSearch");
    // And the partition below is not empty, which is what makes its rows real.
    expect(WITH_EXEMPT_CALLS.length).toBeGreaterThan(0);
  });

  it.each(WITH_EXEMPT_CALLS)("%s produces no unit for an exempt call", (scenario) => {
    const exempt = new Set(
      toolUses(scenario)
        .filter((call) => EXEMPT_TOOLS.has(call.name))
        .map((call) => call.id),
    );
    // The row has something to say: it names at least one exempt call.
    expect(exempt.size).toBeGreaterThan(0);
    const leaked = foldScenario(scenario)
      .entries.filter((entry) => exempt.has(entry.upsertKey.replace(/^activity:/, "")))
      .map((entry) => entry.source.discriminator);
    expect(leaked).toEqual([]);
  });

  it.each(WITH_EXEMPT_CALLS)("%s produces no residue for an exempt call", (scenario) => {
    expect(residueKeys(foldScenario(scenario))).not.toContain("unknown/tool_use");
  });
});

/** The captures that actually make an engine-owned call. */
const WITH_ENGINE_OWNED_CALLS = SCENARIOS.filter((scenario) =>
  toolUses(scenario).some((call) => ENGINE_OWNED_TOOLS.has(call.name)),
);

describe("the engine's gate keeps its own units", () => {
  it("the captures actually exercise it", () => {
    const asked = SCENARIOS.flatMap((scenario) => toolUses(scenario)).map((call) => call.name);
    expect(asked).toContain("AskUserQuestion");
    expect(WITH_ENGINE_OWNED_CALLS.length).toBeGreaterThan(0);
  });

  it.each(WITH_ENGINE_OWNED_CALLS)("%s leaves an engine-owned call to the gate", (scenario) => {
    const owned = new Set(
      toolUses(scenario)
        .filter((call) => ENGINE_OWNED_TOOLS.has(call.name))
        .map((call) => call.id),
    );
    expect(owned.size).toBeGreaterThan(0);
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

  // NO VACUOUS ROWS: a capture with no `unmodeled` unit has nothing to say
  // here, and looping over its empty list asserted nothing while reading as a
  // pass. The captures that reach the arm are named, and the partition is
  // asserted non-empty so it cannot silently become one.
  const WITH_UNMODELED = SCENARIOS.filter((scenario) => unmodeledNames(scenario).length > 0);

  it("the captures actually reach the arm", () => {
    expect(WITH_UNMODELED.length).toBeGreaterThan(0);
  });

  it.each(WITH_UNMODELED)("%s never files a modelled built-in as unmodeled", (scenario) => {
    const names = unmodeledNames(scenario);
    expect(names.length).toBeGreaterThan(0);
    expect(names.filter((name) => TOOL_CONVERTERS.has(name))).toEqual([]);
  });

  it.each(WITH_UNMODELED)("%s never files an exempt built-in as unmodeled", (scenario) => {
    expect(unmodeledNames(scenario).filter((name) => EXEMPT_TOOLS.has(name))).toEqual([]);
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

  // Every capture carries usage, so this partition is the whole set — asserted
  // rather than assumed, so a capture that stopped carrying usage is a failure
  // instead of a row that quietly checks nothing.
  const WITH_USAGE = SCENARIOS.filter((scenario) => usageCarriers(scenario).length > 0);

  it("every capture carries usage on some unit", () => {
    expect(WITH_USAGE).toEqual(SCENARIOS);
  });

  it.each(WITH_USAGE)("%s stamps usage on a response's FIRST block", (scenario) => {
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

describe("the shim never invents a context cut", () => {
  // GROUNDED (2026-09-03 re-capture): `compacting` is a STATUS beat, and the
  // cut is the `compact_boundary` record. `compaction-directed` USED TO be
  // the one capture whose session said it compacted but carried NO boundary
  // (the old run answered "Not enough messages to compact."). The re-capture
  // lengthened the shared world's own prompt list until `/compact` had
  // enough transcript to actually cut, so this capture now carries a REAL
  // boundary — the honest output has both the `compacting` session arm AND a
  // real `context_cut.compacted`, because the vendor genuinely cut here. A
  // shim that omitted it would hide a cut the vendor actually made.
  it("compaction-directed says `compacting` and cuts via a real compact_boundary", () => {
    const run = foldScenario("compaction-directed");
    expect(sessionUpdateArms(run)).toContain("compacting");
    expect(arms(run).filter((arm) => arm.includes("context_cut"))).toEqual([
      "agent_update.context_cut.compacted",
    ]);
  });

  // GROUNDED (2026-09-04): the VENDOR's own automatic compaction, provoked by
  // paced window occupancy rather than by a `/compact`. `trigger: "auto"` is
  // the only discriminator on the boundary, so the fold's arm is the same
  // `compacted` one the directed capture grounds.
  it("auto-compaction says `compacting` and cuts via a vendor-initiated compact_boundary", () => {
    const run = foldScenario("auto-compaction");
    expect(sessionUpdateArms(run)).toContain("compacting");
    expect(arms(run).filter((arm) => arm.includes("context_cut"))).toEqual([
      "agent_update.context_cut.compacted",
    ]);
  });

  it("the captures that cut are the /clear and the two real compactions, and the failed-compaction arm stays ungrounded", () => {
    // A `/clear` IS a cut and the vendor records it; so does
    // `compaction-directed`'s real `/compact` (2026-09-03 re-capture) and
    // `auto-compaction`'s vendor-initiated one (2026-09-04).
    // `ContextCompactionFailed` (a FAILED compaction) has no capture at all —
    // the MANIFEST records that gap — so naming the whole set here keeps a
    // manufactured cut from appearing anywhere without a human noticing.
    const withCuts = SCENARIOS.filter((scenario) =>
      arms(foldScenario(scenario)).some((arm) => arm.includes("context_cut")),
    );
    expect(withCuts).toEqual([
      "auto-compaction",
      "compaction-directed",
      "identity-rotation-clear",
    ]);
    const cleared = arms(foldScenario("identity-rotation-clear")).filter((arm) =>
      arm.includes("context_cut"),
    );
    expect(cleared.some((arm) => arm.includes("compact"))).toBe(false);
    const compacted = arms(foldScenario("compaction-directed")).filter((arm) =>
      arm.includes("context_cut"),
    );
    expect(compacted).toEqual(["agent_update.context_cut.compacted"]);
  });
});

/**
 * The unit kinds whose START ARM CARRIES NO INSTANT, so there is no start for
 * their settle to restate: a prose or reasoning block is announced empty.
 */
const STARTLESS_KINDS: ReadonlySet<string> = new Set(["thinking", "response"]);

/** Every settle instant under a value, by the JSON path it sits at. */
function settleInstants(value: unknown, path: string, out: [string, Record<string, unknown>][]): void {
  if (Array.isArray(value)) {
    value.forEach((element, index) => settleInstants(element, `${path}[${index}]`, out));
    return;
  }
  if (typeof value !== "object" || value === null) return;
  for (const [key, child] of Object.entries(value as Record<string, unknown>)) {
    if (key === "settledAt" && typeof child === "object" && child !== null) {
      out.push([`${path}.${key}`, child as Record<string, unknown>]);
    }
    settleInstants(child, `${path}.${key}`, out);
  }
}

describe("every activity stands alone on a real capture", () => {
  it("the captures actually carry settle instants on started units", () => {
    let checked = 0;
    for (const scenario of SCENARIOS) {
      for (const activity of foldScenario(scenario).entries.map(activityOf)) {
        if (activity === undefined || STARTLESS_KINDS.has(activity.item.case ?? "")) continue;
        const instants: [string, Record<string, unknown>][] = [];
        settleInstants(toJson(conversationv1.AgentActivitySchema, activity), "", instants);
        checked += instants.length;
      }
    }
    expect(checked).toBeGreaterThan(0);
  });

  it.each(SCENARIOS)("%s stamps every activity with the stands-alone contract", (scenario) => {
    const unstamped = foldScenario(scenario)
      .entries.map(activityOf)
      .filter((activity) => activity !== undefined)
      .filter((activity) => activity.contract !== conversationv1.AgentActivityContract.SETTLES_STAND_ALONE)
      .map((activity) => activity.activityId?.value);
    expect(unstamped).toEqual([]);
  });

  it.each(SCENARIOS)("%s restates the start on every settle instant a started unit carries", (scenario) => {
    const bare: string[] = [];
    for (const activity of foldScenario(scenario).entries.map(activityOf)) {
      if (activity === undefined || STARTLESS_KINDS.has(activity.item.case ?? "")) continue;
      const instants: [string, Record<string, unknown>][] = [];
      settleInstants(toJson(conversationv1.AgentActivitySchema, activity), activity.item.case ?? "", instants);
      for (const [path, instant] of instants) {
        if (instant.startedAt === undefined) bare.push(`${activity.activityId?.value ?? ""} ${path}`);
      }
    }
    expect(bare).toEqual([]);
  });
});
