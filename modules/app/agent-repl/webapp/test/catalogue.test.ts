import { describe, expect, it } from "vitest";
import { Scenario, renderScenarioHtml, scenarios } from "../src/catalogue.js";
import { FIXED_FOLD_CLASS } from "../src/fold.js";

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

  it("agent-activity shows the ticker over a panel no fold state can close", () => {
    const html = renderScenarioHtml(scenario("agent-activity"), closed);
    expect(html).toContain("agent-activity");
    expect(html).toContain(FIXED_FOLD_CLASS);
    expect(html).toContain("agent-panel");
  });

  it("agent-activity nests the child feed with its pending permission", () => {
    const html = expanded("agent-activity");
    expect(html).toContain("agent-panel");
    expect(html).toContain("needs permission");
    // The grandchild Agent card is teal too, so its own nested output is a
    // second fixed panel rather than a toggle, and its text renders with it.
    expect(html).not.toContain('data-panel-toggle="s1-agent2"');
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
    // Every badge closed means no badge DETAIL is mounted inside the catalog.
    // Scoped to the catalog markup, because the feed's own teal Agent card
    // lays a fixed panel out above it, which is not a detail panel.
    expect(html.slice(html.indexOf("async-catalog"))).not.toContain("agent-panel");
  });

  it("host-catalog expanded opens the member's own card with its transcript", () => {
    const html = expanded("host-catalog");
    // The badge detail is the member's ToolCard in the panel inset, its
    // transcript fold open and rendering nested bubbles.
    expect(html).toContain(`<div class="agent-panel"><div class="feed-child">`);
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

  it("inline-shell wears the shared fold dress with an output face", () => {
    const html = renderScenarioHtml(scenario("inline-shell"), closed);
    expect(html).toContain("async-fold");
    expect(html).toContain("output · make watch · done");
    expect(html).not.toContain("task-live-output");
  });

  it("inline-shell streams its tail inside the open fold", () => {
    const html = expanded("inline-shell");
    expect(html).toContain("task-live-output");
    expect(html).toContain("watch: build ok");
  });

  it("workflow-journal stacks two separate open panels (the decided Shape A)", () => {
    // The dual-body decision landed as Shape A: a card owning both an
    // inline child feed and a detached stream stacks one fold per body.
    const html = expanded("workflow-journal");
    expect(html.match(/class="agent-panel"/g)?.length).toBe(2);
    expect(html).toContain("async-fold open");
    expect(html).toContain("stream-row");
  });

  it("drain-banner renders the global bounce banner with its cause", () => {
    const html = renderScenarioHtml(scenario("drain-banner"), closed);
    expect(html).toContain("Daemon bounce scheduled");
    expect(html).toContain("rebuilt the daemon");
  });

  it("drain-banner enumerates a hold that carries a turn AND live tasks", () => {
    const html = renderScenarioHtml(scenario("drain-banner"), closed);
    expect(html).toContain("app-infra — turn in flight, 1 live task");
  });

  it("lease-bubble shows the leased prompt in its own dedicated card", () => {
    const html = renderScenarioHtml(scenario("lease-bubble"), closed);
    expect(html).toContain("lease-card");
    expect(html).toContain("Rebase onto master");
  });

  it("lease-bubble shows the classifier card beside it for contrast", () => {
    const html = renderScenarioHtml(scenario("lease-bubble"), closed);
    expect(html).toContain("will run after this turn");
  });

  it("lease-bubble offers no accept control on the leased prompt", () => {
    // The classifier entry beside it DOES offer one, so this asserts the
    // dispatch and not merely the absence of any accept button.
    const html = renderScenarioHtml(scenario("lease-bubble"), closed);
    expect(html).toContain('data-queue-accept="q-classified"');
    expect(html).not.toContain('data-queue-accept="q-leased"');
  });

  it("thinking renders the streaming disclosure open and the settled one closed", () => {
    const html = renderScenarioHtml(scenario("thinking"), closed);
    expect(html).toContain('<details class="thinking" open>');
    expect(html).toContain('<details class="thinking">');
  });
});
