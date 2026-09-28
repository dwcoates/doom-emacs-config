/**
 * Prompt selection, and the published table.
 *
 * The table in `AGENTS.md` is the mocked vendor's CONTRACT — which prompt
 * produces which vendor behavior, which files it writes, which conversation.v1
 * arms it exercises. It is generated from the registry, and asserted against it
 * in BOTH directions here: a scenario missing from the table fails, and a row
 * with no scenario behind it fails too. Without that, the table is a stale
 * document the day after it is written.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

import { ALIASES, SCENARIOS, scenarioNames, selectScenario } from "../../src/fake/registry.js";
import { networkResumePrompt } from "../../src/engine/network-resume-prompt.js";
import { renderScenarioTable, TABLE_HEADING } from "../../scripts/scenario-table.js";

const agentsMd = (): string =>
  readFileSync(fileURLToPath(new URL("../../AGENTS.md", import.meta.url)), "utf8");

const manifestMd = (): string =>
  readFileSync(fileURLToPath(new URL("../../testdata/captures/MANIFEST.md", import.meta.url)), "utf8");

/**
 * Every `` `!token` `` mentioned in MANIFEST.md — the golden table's
 * `Scenarios:` column, the `e2ecleanup/fakesdk-ext` additions and the
 * reconciliation section all spell scenario names this way.
 */
function tokensNamedInManifest(): Set<string> {
  const found = new Set<string>();
  for (const m of manifestMd().matchAll(/`!([a-z0-9-]+)`/g)) found.add(m[1]);
  return found;
}

describe("selection", () => {
  it("falls through to plain prose for text naming no scenario", () => {
    // Arrange + Act + Assert. Unknown text is the ORDINARY case, not an error.
    expect(selectScenario("what is the plan?").name).toBe("");
  });

  it("selects an exact !name with no argument", () => {
    // Arrange + Act + Assert
    expect(selectScenario("!read").name).toBe("read");
  });

  it("selects an exact !name followed by an argument", () => {
    // Arrange + Act + Assert
    expect(selectScenario("!bash echo hi").name).toBe("bash");
  });

  it("prefers the LONGEST matching name, so a sibling never swallows one", () => {
    // Arrange + Act + Assert. `!bash` must not eat `!bash-detach-live`.
    expect([
      selectScenario("!bash-detach").name,
      selectScenario("!bash-detach-live").name,
      selectScenario("!read-range").name,
    ]).toEqual(["bash-detach", "bash-detach-live", "read-range"]);
  });

  it("does NOT select on a partial name that runs into other characters", () => {
    // Arrange + Act + Assert. `!bashful` names no scenario.
    expect(selectScenario("!bashful").name).toBe("");
  });

  it("does not select on a name buried mid-prose", () => {
    // Arrange + Act + Assert
    expect(selectScenario("please run !bash for me").name).toBe("");
  });

  it("tolerates leading whitespace", () => {
    // Arrange + Act + Assert
    expect(selectScenario("   !glob").name).toBe("glob");
  });

  it("selects the failing turn from a marker buried in prose", () => {
    // Arrange + Act + Assert
    expect(selectScenario("do the thing e2e-fail-this-turn please").name).toBe("fail-marker");
  });

  it("selects the network-resume answer for the shim's own resume prompt", () => {
    // Arrange + Act + Assert
    expect(selectScenario(networkResumePrompt([{ taskId: "a1", description: "d" }])).name).toBe("network-resume");
  });

  it("lets an explicit !name beat the failure marker", () => {
    // Arrange + Act + Assert. An explicit scenario is a stronger statement of
    // intent than a marker in prose.
    expect(selectScenario("!read e2e-fail-this-turn").name).toBe("read");
  });
});

describe("the registry itself", () => {
  it("registers every name exactly once", () => {
    // Arrange + Act
    const names = scenarioNames();

    // Assert. Two scenarios sharing a name would make one unreachable.
    expect(names.length).toBe(new Set(names).size);
  });

  it("carries exactly one default scenario", () => {
    // Arrange + Act + Assert
    expect(SCENARIOS.filter((s) => s.name === "")).toHaveLength(1);
  });

  it("gives every scenario all four table columns", () => {
    // Arrange + Act
    const incomplete = SCENARIOS.filter(
      (s) => s.prompt === "" || s.emits === "" || s.writes === "" || s.arms === "",
    );

    // Assert
    expect(incomplete.map((s) => s.name)).toEqual([]);
  });

  it("documents a prompt that actually selects the scenario", () => {
    // Arrange + Act. The two exceptions are prose (documented by description)
    // and the marker turn (selected by a marker, not a prefix).
    const wrong = SCENARIOS.filter(
      (s) => s.name !== "" && s.name !== "fail-marker" && selectScenario(s.prompt).name !== s.name,
    );

    // Assert
    expect(wrong.map((s) => `${s.name}: ${s.prompt}`)).toEqual([]);
  });
});

describe("the published table", () => {
  it("appears in AGENTS.md under its heading", () => {
    // Arrange + Act + Assert
    expect(agentsMd()).toContain(TABLE_HEADING);
  });

  it("matches the registry exactly, row for row", () => {
    // Arrange
    const rendered = renderScenarioTable();

    // Act + Assert. On failure the expected block is right here in the output,
    // so regenerating never requires leaving the test run.
    expect(agentsMd()).toContain(rendered);
  });

  it("lists every registered scenario's prompt", () => {
    // Arrange
    const published = agentsMd();

    // Act
    const missing = SCENARIOS.filter((s) => !published.includes(`| \`${s.prompt}\` |`));

    // Assert
    expect(missing.map((s) => s.name)).toEqual([]);
  });

  it("has no table row that no scenario backs", () => {
    // Arrange
    const heading = agentsMd().split(TABLE_HEADING)[1] ?? "";
    const body = heading.split("### What the mock writes")[0] ?? "";
    const prompts = new Set(SCENARIOS.map((s) => s.prompt));

    // Act
    const rows = body
      .split("\n")
      .filter((l) => l.startsWith("| `"))
      .map((l) => l.slice(3, l.indexOf("` |")));
    const orphans = rows.filter((p) => !prompts.has(p));

    // Assert
    expect(orphans).toEqual([]);
  });

  it("publishes one row per scenario and no more", () => {
    // Arrange
    const heading = agentsMd().split(TABLE_HEADING)[1] ?? "";
    const body = heading.split("### What the mock writes")[0] ?? "";

    // Act
    const rows = body.split("\n").filter((l) => l.startsWith("| `"));

    // Assert
    expect(rows).toHaveLength(SCENARIOS.length);
  });
});

describe("MANIFEST.md against the registry (naming-drift guard)", () => {
  // The golden captures under testdata/captures/ and the registry's own
  // `!name` prompts drifted apart once already (`hook-succeeded` vs.
  // `!hook-success`, and a dozen more) before MANIFEST.md's `Scenarios:`
  // column and its reconciliation section closed the gap by hand. These two
  // checks are the STRUCTURAL guard against that drift recurring silently: a
  // scenario renamed or removed without updating MANIFEST.md fails here, and
  // so does a scenario added without a mention there.

  it("every scenario token MANIFEST.md names resolves in the registry", () => {
    // Arrange
    const registered = new Set(scenarioNames());

    // Act. `fail-marker` is spelled `!fail-marker` throughout even though it
    // is selected by a prose marker, not a prefix — still a real registered
    // name.
    const unresolved = [...tokensNamedInManifest()].filter((token) => !registered.has(token));

    // Assert
    expect(unresolved).toEqual([]);
  });

  it("every registered scenario is named at least once in MANIFEST.md", () => {
    // Arrange. The default scenario's name is `""`, which cannot be spelled
    // as a `!token` — MANIFEST.md names it in prose ("the default `\"\"`
    // prose scenario") instead, so it is exempted rather than required to
    // match the token regex.
    const named = tokensNamedInManifest();

    // Act
    const unmentioned = scenarioNames().filter((name) => name !== "" && !named.has(name));

    // Assert
    expect(unmentioned).toEqual([]);
  });

  it("every ALIAS key round-trips to its target scenario through selectScenario", () => {
    // Arrange + Act
    const wrong = Object.entries(ALIASES).filter(
      ([alias, target]) => selectScenario(`!${alias}`).name !== target,
    );

    // Assert
    expect(wrong).toEqual([]);
  });

  it("every ALIAS target is a real registered scenario", () => {
    // Arrange
    const registered = new Set(scenarioNames());

    // Act
    const dangling = Object.values(ALIASES).filter((target) => !registered.has(target));

    // Assert
    expect(dangling).toEqual([]);
  });
});
