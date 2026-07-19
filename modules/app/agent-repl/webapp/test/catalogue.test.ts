import { describe, expect, it } from "vitest";
import {
  Scenario,
  dualBodyShapeAHtml,
  dualBodyShapeBHtml,
  renderScenarioHtml,
  scenarios,
} from "../src/catalogue.js";

/** The scenario registered under SLUG; throws when the catalogue lost it. */
function scenario(slug: string): Scenario {
  const sc = scenarios.find((s) => s.slug === slug);
  if (!sc) throw new Error(`no scenario ${slug}`);
  return sc;
}

const closed = (): boolean => false;

/** Render SLUG with its expanded pane's fold set open. */
function expanded(slug: string): string {
  const sc = scenario(slug);
  const open = new Set(sc.openIds);
  return renderScenarioHtml(sc, (id) => open.has(id));
}

describe("catalogue scenarios", () => {
  it("every scenario renders visible content when collapsed", () => {
    for (const sc of scenarios) {
      expect(renderScenarioHtml(sc, closed), sc.slug).not.toBe("");
    }
  });

  it("agent-activity collapsed shows the ticker without the panel body", () => {
    const html = renderScenarioHtml(scenario("agent-activity"), closed);
    expect(html).toContain("agent-activity");
    expect(html).not.toContain("agent-panel");
  });

  it("agent-activity expanded nests the child feed with its pending permission", () => {
    const html = expanded("agent-activity");
    expect(html).toContain("agent-panel");
    expect(html).toContain("needs permission");
    // The grandchild Agent's own open fold, and the panel text inside it —
    // its description never renders (subagent cards suppress their input).
    expect(html).toContain('data-panel-toggle="s1-agent2"');
    expect(html).toContain("The mac runner agrees");
  });

  it("workflow-journal expanded renders the journal as rows including its error", () => {
    const html = expanded("workflow-journal");
    expect(html).toContain("stream-row");
    expect(html).toContain("agent exhausted retries");
  });

  it("taskcreate-history expanded nests the update transitions", () => {
    const html = expanded("taskcreate-history");
    expect(html).toContain("in progress");
    expect(html).toContain("Fix the flaky watcher test (deflaked)");
  });

  it("shell-fold expanded renders the raw tail inside the async fold", () => {
    const html = expanded("shell-fold");
    expect(html).toContain("async-fold");
    expect(html).toContain("soak: iteration 3 ok");
  });

  it("agent-transcript expanded renders nested bubbles with a depth-two fold", () => {
    const html = expanded("agent-transcript");
    expect(html).toContain("feed-child");
    expect(html).toContain('data-panel-toggle="async:s5-t4"');
  });

  it("agent-transcript folds the transcript's TaskUpdate under its TaskCreate", () => {
    const html = expanded("agent-transcript");
    expect(html).toContain('data-panel-toggle="s5-t2"');
  });

  it("host-catalog collapsed lists badges on an amber host bubble", () => {
    const html = renderScenarioHtml(scenario("host-catalog"), closed);
    expect(html).toContain("async-badge");
    expect(html).toContain("async-live");
    expect(html).not.toContain("watcher-row");
  });

  it("host-catalog expanded opens the agent member's watcher row", () => {
    const html = expanded("host-catalog");
    expect(html).toContain("watcher-row");
    expect(html).toContain("Halfway through");
  });

  it("prompt-host puts the catalog on the amber prompt bubble", () => {
    const html = renderScenarioHtml(scenario("prompt-host"), closed);
    expect(html).toContain("bubble user async-live");
    expect(html).toContain("async-badge");
  });

  it("gns-fold collapsed keeps the upkeep out of the feed, ticker face aside", () => {
    const html = renderScenarioHtml(scenario("gns-fold"), closed);
    expect(html).toContain("gns-fold");
    // The ticker face quotes the last folded line by design; the folded
    // ITEMS (the spawn card, the ack bubble) exist only in the open panel.
    expect(html).not.toContain("agent-panel");
    expect(html).not.toContain("tool-agent");
  });

  it("gns-fold expanded renders the folded bridge spawn and its acknowledgment", () => {
    const html = expanded("gns-fold");
    expect(html).toContain("Bridge listener respawned");
    expect(html).toContain("respawn the slack bridge");
  });

  it("inline-shell renders its tail zero-click with no fold", () => {
    const html = renderScenarioHtml(scenario("inline-shell"), closed);
    expect(html).toContain("task-live-output");
    expect(html).toContain("watch: build ok");
    expect(html).not.toContain("async-fold");
  });

  it("dual-body shape A stacks two separate open panels", () => {
    const html = dualBodyShapeAHtml();
    expect(html.match(/class="agent-panel"/g)?.length).toBe(2);
    expect(html).toContain("async-fold open");
    expect(html).toContain("stream-row");
  });

  it("dual-body shape B merges into a single sectioned panel", () => {
    const html = dualBodyShapeBHtml();
    expect(html.match(/class="agent-panel"/g)?.length).toBe(1);
    expect(html).toContain("cat-panel-heading");
    expect(html).toContain("feed-child");
    expect(html).toContain("stream-row");
  });

  it("thinking renders the streaming disclosure open and the settled one closed", () => {
    const html = renderScenarioHtml(scenario("thinking"), closed);
    expect(html).toContain('<details class="thinking" open>');
    expect(html).toContain('<details class="thinking">');
  });
});
