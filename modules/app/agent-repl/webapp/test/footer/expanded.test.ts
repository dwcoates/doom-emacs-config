// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FooterExpandedSchema,
  FooterStatusIdleActivitySchema,
  type FooterExpanded,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import type { FeedId } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import type { FooterActivity } from "../../src/footer/strip.js";
import {
  EXPANDED_FOOTER_MAX_ROWS,
  FOOTER_PANELS,
  createJumpNotices,
  drawFooterExpanded,
  type ExpandedDeps,
  type FooterPanel,
} from "../../src/footer/expanded.js";
import {
  expanded,
  feedId,
  harness,
  type ExpandedInit,
  type Harness,
} from "./harness.js";
import { createStopControls } from "../../src/footer/stop.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import type { ClientLogRecord } from "../../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";

const NOW = 1_800_000_000_000;

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(NOW);
});
afterEach(() => {
  vi.useRealTimers();
});

interface Drawn {
  panel: HTMLElement;
  revealed: FeedId[];
  h: Harness;
}

/**
 * Draw one panel, recording every jump it asks the feed for. REDRAW draws the
 * same view into the same section, exactly as the mount's redraw does, so a
 * click's outcome lands on the section the test holds.
 */
function drawPanel(
  selection: FooterPanel,
  init: ExpandedInit = {},
  reached: boolean | (() => Promise<boolean>) = true,
  view?: FooterExpanded,
): Drawn {
  const revealed: FeedId[] = [];
  const h = harness();
  const u = view ?? expanded(init);
  let panel: HTMLElement | null = null;
  const deps: ExpandedDeps = {
    ctx: h.ctx,
    stops: createStopControls(h.ctx),
    notices: createJumpNotices(),
    redraw: () => {
      panel = drawFooterExpanded(u, selection, deps, panel);
    },
    selectDetachedWork: async (id) => {
      revealed.push(id);
      return typeof reached === "function" ? reached() : reached;
    },
  };
  panel = drawFooterExpanded(u, selection, deps, null);
  if (panel === null) throw new Error("the panel was not drawn");
  return { panel, revealed, h };
}

/** A jump to a known entry. */
function toEntry(id: string) {
  return { target: { case: "entry" as const, value: feedId(id) } };
}

/** A jump the daemon could not resolve, for REASON. */
function unresolvedFor(reason: "notDrawn") {
  return { target: { case: "unresolved" as const, value: { reason: { case: reason, value: {} } } } };
}

/** One activity, as the strip resolves it, for the usage rows to expand. */
function activity(kindCase: string, kindValue: Record<string, unknown>): FooterActivity {
  return create(FooterStatusIdleActivitySchema, {
    at: { atMs: BigInt(NOW) },
    kind: { case: kindCase as never, value: kindValue as never },
  });
}

/** The tokens panel, drawn with ACTIVITY standing on the strip. */
function drawTokensPanelWith(kind: FooterActivity): HTMLElement {
  const h = harness();
  const panel = drawFooterExpanded(expanded(), "tokens", { ctx: h.ctx, stops: createStopControls(h.ctx), notices: createJumpNotices(), redraw: () => undefined,
    selectDetachedWork: async () => true,
    activity: kind,
  });
  if (panel === null) throw new Error("the panel was not drawn");
  return panel;
}

/** The rate-limit activity the sheet photographs: both windows figured. */
function rateLimited(): FooterActivity {
  return activity("rateLimited", {
    session: { newsworthy: true, utilization: 0.82, resetsAtS: BigInt((NOW + 3_540_000) / 1000) },
    weekly: { newsworthy: false, utilization: 0.63, resetsAtS: BigInt((NOW + 259_200_000) / 1000) },
    figuresReadAtMs: BigInt(NOW - 630_000),
  });
}

/** The same activity, with the overage window the vendor reported too. */
function rateLimitedWithOverage(): FooterActivity {
  return activity("rateLimited", {
    session: { newsworthy: true, utilization: 0.82, resetsAtS: BigInt((NOW + 3_540_000) / 1000) },
    weekly: { newsworthy: false, utilization: 0.63, resetsAtS: BigInt((NOW + 259_200_000) / 1000) },
    overage: { newsworthy: true, utilization: 0.91, resetsAtS: BigInt((NOW + 7_200_000) / 1000) },
    figuresReadAtMs: BigInt(NOW - 630_000),
  });
}

describe("drawFooterUsageRows: what the strip could not fit", () => {
  // THE SECOND WINDOW. The strip cuts it off at 1280 by design; the sheet is
  // where a reader gets to see it.
  it("carries the weekly window the strip cut off", () => {
    const panel = drawTokensPanelWith(rateLimited());
    expect(
      panel.querySelector('[data-usage-allowance="weekly"]')?.textContent,
    ).toContain("weekly 63%");
  });

  it("carries the reset countdown of the newsworthy window", () => {
    const panel = drawTokensPanelWith(rateLimited());
    expect(
      panel.querySelector('[data-usage-allowance="session"]')?.textContent,
    ).toContain("resets in 59m");
  });

  it("leads with the same window the strip leads with", () => {
    const panel = drawTokensPanelWith(rateLimited());
    expect(
      panel.querySelector("[data-usage-allowance]")?.getAttribute("data-usage-allowance"),
    ).toBe("session");
  });

  // THE OVERAGE WINDOW. It has no room on the strip at all, so the sheet is
  // its only drawn home — one more allowance row of exactly the kind the
  // other two windows already draw.
  it("carries the overage window the strip has no room for", () => {
    const panel = drawTokensPanelWith(rateLimitedWithOverage());
    expect(
      panel.querySelector('[data-usage-allowance="overage"]')?.textContent,
    ).toContain("overage 91%");
  });

  // AN UNSET OVERAGE DRAWS NOTHING EXTRA. Most accounts never have an overage
  // window, and a row for one nobody reported would state a figure the vendor
  // never gave.
  it("draws no overage row when the vendor reported no overage window", () => {
    const panel = drawTokensPanelWith(rateLimited());
    expect(panel.querySelector('[data-usage-allowance="overage"]')).toBeNull();
  });

  it("draws the same allowance rows with and without an overage window, bar the overage one", () => {
    const without = drawTokensPanelWith(rateLimited());
    const with_ = drawTokensPanelWith(rateLimitedWithOverage());
    const labels = (panel: HTMLElement) =>
      [...panel.querySelectorAll("[data-usage-allowance]")].map((row) =>
        row.getAttribute("data-usage-allowance"),
      );
    expect(labels(without)).toEqual(["session", "weekly"]);
    expect(labels(with_)).toEqual(["session", "overage", "weekly"]);
  });

  it("carries the context-budget sentence whole", () => {
    const panel = drawTokensPanelWith(
      activity("contextBudget", { text: "The conversation is approaching its context window budget." }),
    );
    expect(panel.querySelector('[data-usage="context-budget"]')?.textContent).toBe(
      "The conversation is approaching its context window budget.",
    );
  });

  // AN ACTIVITY ABOUT THE TURN IS NOT ABOUT THE ACCOUNT: a hook line has no
  // usage to expand, and the sheet says nothing rather than something empty.
  it("draws no usage rows for an activity that is not about usage", () => {
    const panel = drawTokensPanelWith(activity("hook", { text: "PreToolUse" }));
    expect(panel.querySelector("[data-usage]")).toBeNull();
  });

  it("draws no usage rows when no activity stands at all", () => {
    expect(drawPanel("tokens").panel.querySelector("[data-usage]")).toBeNull();
  });
});

/** Every promise the click chain queued. */
async function settle(): Promise<void> {
  for (let i = 0; i < 20; i += 1) await vi.advanceTimersByTimeAsync(0);
}

describe("drawFooterExpanded: the selection picks the panel", () => {
  it("draws NOTHING when no strip element is selected", () => {
    const h = harness();
    expect(
      drawFooterExpanded(expanded(), null, { ctx: h.ctx, stops: createStopControls(h.ctx), notices: createJumpNotices(), redraw: () => undefined, selectDetachedWork: async () => true }),
    ).toBeNull();
  });

  it.each(FOOTER_PANELS.map((panel) => [panel]))("draws the %s panel when selected", (panel) => {
    // The agents panel folds away with no rows, so it needs a live agent to
    // draw at all; every other panel draws its own empty state.
    const init: ExpandedInit = panel === "agents" ? { agents: [AGENT_ROW] } : {};
    expect(drawPanel(panel, init).panel.getAttribute("data-panel")).toBe(panel);
  });

  it("wears the shared list-delimiter class every panel and dropdown uses", () => {
    expect(drawPanel("tasks").panel.classList.contains("list-rows")).toBe(true);
  });

  it("refuses a view whose selected panel message is unset", () => {
    const h = harness();
    expect(() =>
      drawFooterExpanded(create(FooterExpandedSchema, {}), "agents", { ctx: h.ctx, stops: createStopControls(h.ctx), notices: createJumpNotices(), redraw: () => undefined,
        selectDetachedWork: async () => true,
      }),
    ).toThrow(MalformedView);
  });

  it("scrolls its own content past the row ceiling rather than growing the page", () => {
    const rows = Array.from({ length: EXPANDED_FOOTER_MAX_ROWS + 2 }, (_unused, index) => ({
      work: { value: `work-${index}` },
      jump: toEntry(`shell-${index}`),
      command: { text: `cmd ${index}` },
      runtime: { startedAtMs: BigInt(NOW) },
    }));
    expect(drawPanel("shells", { shells: rows }).panel.classList.contains("scrolls")).toBe(true);
  });
});

// ---- the tokens panel ------------------------------------------------------

describe("the tokens panel", () => {
  it.each([
    ["input", "input (uncached)"],
    ["cacheRead", "input (cache read)"],
    ["cacheWrite", "input (cache write)"],
    ["output", "output"],
    ["thinking", "thinking"],
    ["firstToken", "first token"],
  ])("always draws the %s line, named '%s'", (line, label) => {
    const { panel } = drawPanel("tokens");
    expect(panel.querySelector(`[data-token-line="${line}"] .footer-token-name`)?.textContent).toBe(
      label,
    );
  });

  it("draws a line's figure verbatim", () => {
    const { panel } = drawPanel("tokens", {
      tokens: {
        contextGrowth: {},

        input: { value: "18.2k" },
        cacheRead: {},
        cacheWrite: {},
        output: {},
        thinking: {},
        firstToken: {},
      },
    });
    expect(panel.querySelector('[data-token-line="input"] .footer-token-value')?.textContent).toBe(
      "18.2k",
    );
  });

  it("draws an EMPTY SLOT for a figure the turn has not reported", () => {
    const { panel } = drawPanel("tokens");
    expect(
      panel
        .querySelector('[data-token-line="output"] .footer-token-value')
        ?.hasAttribute("data-empty-slot"),
    ).toBe(true);
  });

  it("indents thinking under output, because it is already inside it", () => {
    const { panel } = drawPanel("tokens");
    expect(
      panel
        .querySelector('[data-token-line="thinking"]')
        ?.classList.contains("footer-token-line-indented"),
    ).toBe(true);
  });

  it("draws no alarm line when the alarm did not trip", () => {
    const { panel } = drawPanel("tokens");
    expect(panel.querySelector(".footer-token-alarm")).toBeNull();
  });

  it("draws the alarm's composed sentence when it did", () => {
    const { panel } = drawPanel("tokens", {
      tokens: {
        contextGrowth: {},

        input: {},
        cacheRead: {},
        cacheWrite: {},
        output: {},
        thinking: {},
        firstToken: {},
        alarm: { text: "expensive turn — 41k over 20k" },
      },
    });
    expect(panel.querySelector(".footer-token-alarm")?.textContent).toBe(
      "expensive turn — 41k over 20k",
    );
  });

  it("draws the clean verdict with no evidence to show", () => {
    const { panel } = drawPanel("tokens", {
      tokens: {
        contextGrowth: {},

        input: {},
        cacheRead: {},
        cacheWrite: {},
        output: {},
        thinking: {},
        firstToken: {},
        verdict: { verdict: { case: "complete", value: {} } },
      },
    });
    expect(panel.querySelector('[data-verdict="complete"]')?.textContent).toBe("✓ reconciled");
  });

  it.each([
    ["incomplete", "2 responses missing usage"],
    ["invalid", "output exceeds the reported total"],
  ])("draws the %s verdict's evidence verbatim", (arm, text) => {
    const { panel } = drawPanel("tokens", {
      tokens: {
        contextGrowth: {},

        input: {},
        cacheRead: {},
        cacheWrite: {},
        output: {},
        thinking: {},
        firstToken: {},
        verdict: { verdict: { case: arm as never, value: { text } } },
      },
    });
    expect(panel.querySelector(`[data-verdict="${arm}"]`)?.textContent).toBe(`✗ ${text}`);
  });

  it("refuses a verdict line whose oneof sets no arm", () => {
    expect(() =>
      drawPanel("tokens", {
        tokens: {
          contextGrowth: {},

          input: {},
          cacheRead: {},
          cacheWrite: {},
          output: {},
          thinking: {},
          firstToken: {},
          verdict: {},
        },
      }),
    ).toThrow(MalformedView);
  });

  it("refuses a panel missing one of its always-set lines", () => {
    expect(() =>
      drawPanel("tokens", {
        tokens: { contextGrowth: {}, input: {}, cacheRead: {}, cacheWrite: {}, output: {}, thinking: {} },
      }),
    ).toThrow(MalformedView);
  });
});

// ---- the tokens panel: context growth and per-agent spend -------------------

/** The token lines every panel carries, with nothing reported yet. */
const EMPTY_TOKEN_LINES = {
  contextGrowth: {},
  input: {},
  cacheRead: {},
  cacheWrite: {},
  output: {},
  thinking: {},
  firstToken: {},
};

/** One agent's share, as the daemon ships it. */
function agentShare(label: string, input: string) {
  return {
    label,
    input: { value: input },
    cacheRead: { value: "90k" },
    cacheWrite: { value: "18k" },
    output: { value: "1.5k" },
  };
}

describe("the tokens panel's context growth", () => {
  it("draws the growth line first", () => {
    const { panel } = drawPanel("tokens");
    expect(panel.firstElementChild?.getAttribute("data-token-line")).toBe("contextGrowth");
  });

  it("names the growth line 'context growth'", () => {
    const { panel } = drawPanel("tokens");
    expect(panel.querySelector('[data-token-line="contextGrowth"] .footer-token-name')?.textContent).toBe(
      "context growth",
    );
  });

  it("draws the growth figure verbatim", () => {
    const { panel } = drawPanel("tokens", {
      tokens: { ...EMPTY_TOKEN_LINES, contextGrowth: { value: "18.2k" } },
    });
    expect(panel.querySelector('[data-token-line="contextGrowth"] .footer-token-value')?.textContent).toBe(
      "18.2k",
    );
  });

  it("draws an EMPTY SLOT when no growth is known", () => {
    const { panel } = drawPanel("tokens");
    expect(
      panel.querySelector('[data-token-line="contextGrowth"] .footer-token-value')?.hasAttribute("data-empty-slot"),
    ).toBe(true);
  });

  it("names the line as measured since the cut when the daemon marks one", () => {
    const { panel } = drawPanel("tokens", {
      tokens: { ...EMPTY_TOKEN_LINES, contextGrowth: { value: "5k", sinceCut: {} } },
    });
    const line = panel.querySelector("[data-since-cut]");
    expect(line?.querySelector(".footer-token-name")?.textContent).toBe("context growth (since cut)");
  });

  it("refuses a panel with no growth line", () => {
    const { contextGrowth: _omitted, ...withoutGrowth } = EMPTY_TOKEN_LINES;
    expect(() => drawPanel("tokens", { tokens: withoutGrowth })).toThrow(MalformedView);
  });
});

describe("the tokens panel's per-agent spend", () => {
  it("heads the summed lines 'all agents'", () => {
    const { panel } = drawPanel("tokens");
    const header = panel.querySelector(".footer-token-header");
    expect(header?.textContent).toBe("all agents");
    expect(header?.nextElementSibling?.getAttribute("data-token-line")).toBe("input");
  });

  it("draws no agent entry when the daemon lists none", () => {
    const { panel } = drawPanel("tokens");
    expect(panel.querySelector("[data-token-agent]")).toBeNull();
  });

  it("draws each agent's name verbatim, in the daemon's order", () => {
    const { panel } = drawPanel("tokens", {
      tokens: { ...EMPTY_TOKEN_LINES, agents: [agentShare("main", "18.2k"), agentShare("Explore · find", "2k")] },
    });
    expect([...panel.querySelectorAll("[data-token-agent]")].map((el) => el.textContent)).toEqual([
      "main",
      "Explore · find",
    ]);
  });

  it.each([
    ["input", "2k"],
    ["cacheRead", "90k"],
    ["cacheWrite", "18k"],
    ["output", "1.5k"],
  ])("draws an agent's %s line verbatim", (line, value) => {
    const { panel } = drawPanel("tokens", {
      tokens: { ...EMPTY_TOKEN_LINES, agents: [agentShare("Explore", "2k")] },
    });
    expect(
      panel.querySelector(`[data-token-agent-row="Explore"][data-token-line="${line}"] .footer-token-value`)
        ?.textContent,
    ).toBe(value);
  });

  it("draws the agent entries after the turn's verdict", () => {
    const { panel } = drawPanel("tokens", {
      tokens: {
        ...EMPTY_TOKEN_LINES,
        verdict: { verdict: { case: "complete", value: {} } },
        agents: [agentShare("main", "1k")],
      },
    });
    expect(panel.querySelector("[data-verdict]")?.nextElementSibling?.getAttribute("data-token-agent")).toBe("main");
  });

  it("refuses an agent entry missing one of its lines", () => {
    const { output: _omitted, ...partial } = agentShare("main", "1k");
    expect(() => drawPanel("tokens", { tokens: { ...EMPTY_TOKEN_LINES, agents: [partial] } })).toThrow(
      MalformedView,
    );
  });
});

// ---- the agents panel ------------------------------------------------------

const AGENT_ROW = {
  work: { value: "work-a" },
  jump: toEntry("bubble-1"),
  label: { text: "Explore" },
  description: { text: "sweep the repo" },
  tokens: { text: "12.4k tok" },
  runtime: { startedAtMs: BigInt(NOW - 65_000) },
};

describe("the agents panel", () => {
  it("carries the fan-wide stop in its header", () => {
    const { panel } = drawPanel("agents", { agents: [AGENT_ROW] });
    expect(panel.querySelector(".footer-panel-header [data-interrupt]")).not.toBeNull();
  });

  it("draws the subagent's type label verbatim", () => {
    const { panel } = drawPanel("agents", { agents: [AGENT_ROW] });
    expect(panel.querySelector(".footer-row-label")?.textContent).toBe("Explore");
  });

  it("draws the commission's description when the spawn carried one", () => {
    const { panel } = drawPanel("agents", { agents: [AGENT_ROW] });
    expect(panel.querySelector(".footer-row-description")?.textContent).toBe("sweep the repo");
  });

  it("draws NO description when the spawn carried none — never a synthesized one", () => {
    const { panel } = drawPanel("agents", {
      agents: [{ ...AGENT_ROW, description: undefined }],
    });
    expect(panel.querySelector(".footer-row-description")).toBeNull();
  });

  it("draws the running token sum verbatim", () => {
    const { panel } = drawPanel("agents", { agents: [AGENT_ROW] });
    expect(panel.querySelector(".footer-row-tokens")?.textContent).toBe("12.4k tok");
  });

  it("ticks the row's runtime from the shipped instant", () => {
    const { panel } = drawPanel("agents", { agents: [AGENT_ROW] });
    expect(panel.querySelector(".footer-row-clock")?.textContent).toBe("1m 5s");
  });

  it("re-reads the runtime on the shared tick", () => {
    const { panel } = drawPanel("agents", { agents: [AGENT_ROW] });
    vi.advanceTimersByTime(1000);
    expect(panel.querySelector(".footer-row-clock")?.textContent).toBe("1m 6s");
  });

  it("reads the row clock's nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the row's start does not share the shared ticker's phase.
    const { panel } = drawPanel("agents", {
      agents: [{ ...AGENT_ROW, runtime: { startedAtMs: BigInt(NOW - 4920) } }],
    });
    // Assert: five real seconds of running reads 5s, not the lagging 4s.
    expect(panel.querySelector(".footer-row-clock")?.textContent).toBe("5s");
  });

  it("is a jump target carrying the bubble's id verbatim", () => {
    const { panel } = drawPanel("agents", { agents: [AGENT_ROW] });
    expect(panel.querySelector("[data-jump]")?.getAttribute("data-jump")).toBe("bubble-1");
  });

  it("hands the SERVED id back to the feed's reveal", async () => {
    const { panel, revealed } = drawPanel("agents", { agents: [AGENT_ROW] });
    panel.querySelector<HTMLElement>("[data-jump]")?.dispatchEvent(new MouseEvent("click"));
    await settle();
    expect(revealed[0]?.value).toBe("bubble-1");
  });

  it("says so at the row when the jump could not land", async () => {
    const { panel } = drawPanel("agents", { agents: [AGENT_ROW] }, false);
    panel.querySelector<HTMLElement>("[data-jump]")?.dispatchEvent(new MouseEvent("click"));
    await settle();
    expect(panel.querySelector(".footer-row-unreachable")?.textContent).toBe("not on screen");
  });

  it("leaves no unreachable mark on a jump that landed", async () => {
    const { panel } = drawPanel("agents", { agents: [AGENT_ROW] }, true);
    panel.querySelector<HTMLElement>("[data-jump]")?.dispatchEvent(new MouseEvent("click"));
    await settle();
    expect(panel.querySelector(".footer-row-unreachable")).toBeNull();
  });

  it("folds away entirely when nothing is live, rather than drawing a bare stop-all", () => {
    // The header carries the fan-wide "stop all"; with no rows there is nothing
    // to stop, so the whole panel collapses (null) instead of standing empty.
    const h = harness();
    expect(
      drawFooterExpanded(expanded({ agents: [] }), "agents", {
        ctx: h.ctx,
        stops: createStopControls(h.ctx), notices: createJumpNotices(), redraw: () => undefined,
        selectDetachedWork: async () => true,
      }),
    ).toBeNull();
  });

  it("stays drawn while the fan-wide stop is holding its outcome, even with no rows", () => {
    // The push "stop all" causes has no rows — the set it just emptied — and
    // that is exactly when the control carries "stopped N agents". Folding then
    // would erase the count, so the panel stays until the answer clears.
    const h = harness();
    const stops = createStopControls(h.ctx);
    const note = document.createElement("span");
    note.className = "footer-stop-note";
    note.textContent = "stopped 4 agents";
    stops.allAgents.appendChild(note);
    const panel = drawFooterExpanded(expanded({ agents: [] }), "agents", {
      ctx: h.ctx,
      stops,
      notices: createJumpNotices(),
      redraw: () => undefined,
      selectDetachedWork: async () => true,
    });
    expect(panel).not.toBeNull();
    expect(panel?.querySelector(".footer-stop-note")?.textContent).toBe("stopped 4 agents");
  });

  it("refuses a row with no jump", () => {
    expect(() =>
      drawPanel("agents", { agents: [{ ...AGENT_ROW, jump: undefined }] }),
    ).toThrow(MalformedView);
  });
});

// ---- the tasks panel -------------------------------------------------------

describe("the tasks panel", () => {
  it.each([
    ["pending", "☐"],
    ["completed", "☑"],
  ])("draws the %s glyph", (arm, glyph) => {
    const { panel } = drawPanel("tasks", {
      tasks: [{ status: { status: { case: arm as never, value: {} } }, subject: { text: "ship it" } }],
    });
    expect(panel.querySelector(`[data-glyph="${arm}"]`)?.textContent).toBe(glyph);
  });

  it("breathes the running glyph rather than drawing it still", () => {
    const { panel } = drawPanel("tasks", {
      tasks: [{ status: { status: { case: "running", value: {} } }, subject: { text: "ship it" } }],
    });
    expect(
      panel.querySelector('[data-glyph="running"]')?.classList.contains("footer-glyph-running"),
    ).toBe(true);
  });

  it("draws the subject verbatim", () => {
    const { panel } = drawPanel("tasks", {
      tasks: [{ status: { status: { case: "pending", value: {} } }, subject: { text: "ship it" } }],
    });
    expect(panel.querySelector(".footer-row-label")?.textContent).toBe("ship it");
  });

  it("draws the ACTIVE FORM in place of the subject while running", () => {
    const { panel } = drawPanel("tasks", {
      tasks: [
        {
          status: {
            status: { case: "running", value: { activeForm: { text: "Running the migration…" } } },
          },
          subject: { text: "Run the migration" },
        },
      ],
    });
    expect(panel.querySelector(".footer-row-label")?.textContent).toBe("Running the migration…");
  });

  it("falls back to the subject for a running task with no phrasing", () => {
    const { panel } = drawPanel("tasks", {
      tasks: [
        { status: { status: { case: "running", value: {} } }, subject: { text: "Run it" } },
      ],
    });
    expect(panel.querySelector(".footer-row-label")?.textContent).toBe("Run it");
  });

  it("is NOT a jump target: a tracker task has no feed bubble", () => {
    const { panel } = drawPanel("tasks", {
      tasks: [{ status: { status: { case: "pending", value: {} } }, subject: { text: "x" } }],
    });
    expect(panel.querySelector("[data-jump]")).toBeNull();
  });

  it("refuses a row whose status oneof sets no arm", () => {
    expect(() =>
      drawPanel("tasks", { tasks: [{ status: {}, subject: { text: "x" } }] }),
    ).toThrow(MalformedView);
  });

  it("draws its empty line when the tracker is empty", () => {
    const { panel } = drawPanel("tasks");
    expect(panel.querySelector("[data-empty]")?.textContent).toBe("the task tracker is empty");
  });
});

// ---- the shells panel ------------------------------------------------------

const SHELL_ROW = {
  work: { value: "work-s" },
  jump: toEntry("shell-1"),
  command: { text: "npm run build" },
  runtime: { startedAtMs: BigInt(NOW - 5000) },
};

describe("the shells panel", () => {
  it("draws the command verbatim", () => {
    const { panel } = drawPanel("shells", { shells: [SHELL_ROW] });
    expect(panel.querySelector(".footer-row-command")?.textContent).toBe("npm run build");
  });

  it("ticks the shell's runtime", () => {
    const { panel } = drawPanel("shells", { shells: [SHELL_ROW] });
    expect(panel.querySelector(".footer-row-clock")?.textContent).toBe("5s");
  });

  it("draws NO token figure: a shell has no token cost", () => {
    const { panel } = drawPanel("shells", { shells: [SHELL_ROW] });
    expect(panel.querySelector(".footer-row-tokens")).toBeNull();
  });

  it("jumps to the shell's bubble", async () => {
    const { panel, revealed } = drawPanel("shells", { shells: [SHELL_ROW] });
    panel.querySelector<HTMLElement>("[data-jump]")?.dispatchEvent(new MouseEvent("click"));
    await settle();
    expect(revealed[0]?.value).toBe("shell-1");
  });

  it("degrades to a note when the bubble is collapsed and could not be reached", async () => {
    const { panel } = drawPanel("shells", { shells: [SHELL_ROW] }, false);
    panel.querySelector<HTMLElement>("[data-jump]")?.dispatchEvent(new MouseEvent("click"));
    await settle();
    expect(panel.querySelector("[data-unreachable]")).not.toBeNull();
  });

  it("draws its empty line when nothing is running", () => {
    const { panel } = drawPanel("shells");
    expect(panel.querySelector("[data-empty]")?.textContent).toBe("no live shells");
  });
});

// ---- the monitors panel ----------------------------------------------------

const MONITOR_ROW = {
  work: { value: "work-m" },
  jump: toEntry("monitor-card"),
  description: { text: "watching the deploy" },
  runtime: { startedAtMs: BigInt(NOW - 90_000) },
};

describe("the monitors panel", () => {
  it("draws what is being watched verbatim", () => {
    const { panel } = drawPanel("monitors", { monitors: [MONITOR_ROW] });
    expect(panel.querySelector(".footer-row-description")?.textContent).toBe("watching the deploy");
  });

  it("ticks the monitor's runtime", () => {
    const { panel } = drawPanel("monitors", { monitors: [MONITOR_ROW] });
    expect(panel.querySelector(".footer-row-clock")?.textContent).toBe("1m 30s");
  });

  it("draws the persistent marker only when the watch is persistent", () => {
    const { panel } = drawPanel("monitors", {
      monitors: [{ ...MONITOR_ROW, persistent: {} }],
    });
    expect(panel.querySelector('[data-marker="persistent"]')).not.toBeNull();
  });

  it("draws no marker for an ordinary watch", () => {
    const { panel } = drawPanel("monitors", { monitors: [MONITOR_ROW] });
    expect(panel.querySelector("[data-marker]")).toBeNull();
  });

  it("is a jump row naming its Monitor call's tool-call card", () => {
    const { panel } = drawPanel("monitors", { monitors: [MONITOR_ROW] });
    expect(panel.querySelector("[data-jump]")?.getAttribute("data-jump")).toBe("monitor-card");
  });

  it("states the daemon's unresolved reason for a monitor whose card is not placed", () => {
    const { panel } = drawPanel("monitors", { monitors: [{ ...MONITOR_ROW, jump: unresolvedFor("notDrawn") }] });
    expect(panel.querySelector("[data-jump-unresolved]")?.getAttribute("data-jump-unresolved")).toBe("notDrawn");
  });

  it("draws its empty line when nothing is live", () => {
    const { panel } = drawPanel("monitors");
    expect(panel.querySelector("[data-empty]")?.textContent).toBe("no live monitors");
  });
});

// ---- the crons panel -------------------------------------------------------

const CRON_ROW = {
  schedule: { text: "every 5 min" },
  prompt: { text: "check the queue" },
};

describe("the crons panel", () => {
  it("draws the vendor's schedule verbatim", () => {
    const { panel } = drawPanel("crons", { crons: [CRON_ROW] });
    expect(panel.querySelector(".footer-row-label")?.textContent).toBe("every 5 min");
  });

  it("draws the job's prompt verbatim", () => {
    const { panel } = drawPanel("crons", { crons: [CRON_ROW] });
    expect(panel.querySelector(".footer-row-description")?.textContent).toBe("check the queue");
  });

  it("counts down to the next fire when the daemon resolved one", () => {
    const { panel } = drawPanel("crons", {
      crons: [{ ...CRON_ROW, nextFire: { fireAtMs: BigInt(NOW + 312_000) } }],
    });
    expect(panel.querySelector("[data-countdown]")?.textContent).toBe("5m 12s");
  });

  it("draws the schedule alone when the daemon could not resolve a next fire", () => {
    const { panel } = drawPanel("crons", { crons: [CRON_ROW] });
    expect(panel.querySelector("[data-countdown]")).toBeNull();
  });

  it.each([
    ["recurring", { recurring: {} }],
    ["durable", { durable: {} }],
  ])("draws the %s marker when the job carries it", (marker, extra) => {
    const { panel } = drawPanel("crons", { crons: [{ ...CRON_ROW, ...extra }] });
    expect(panel.querySelector(`[data-marker="${marker}"]`)).not.toBeNull();
  });

  it("is NOT a jump target: a job has no feed bubble", () => {
    const { panel } = drawPanel("crons", { crons: [CRON_ROW] });
    expect(panel.querySelector("[data-jump]")).toBeNull();
  });

  it("draws its empty line when nothing is scheduled", () => {
    const { panel } = drawPanel("crons");
    expect(panel.querySelector("[data-empty]")?.textContent).toBe("nothing scheduled");
  });
});

describe("an arm this build has no case for", () => {
  it("refuses a PANEL selection the bundle has no drawing for", () => {
    // ARRANGE
    const h = harness();
    // ACT / ASSERT
    expect(() =>
      drawFooterExpanded(expanded(), "sessions" as FooterPanel, { ctx: h.ctx, stops: createStopControls(h.ctx), notices: createJumpNotices(), redraw: () => undefined,
        selectDetachedWork: async () => true,
      }),
    ).toThrow(MalformedView);
  });

  it("refuses a TOKENS VERDICT arm the bundle cannot name", () => {
    // ARRANGE — legal, then poked: `create` drops an unknown case.
    const view = expanded({
      tokens: {
        contextGrowth: {},

        input: {},
        cacheRead: {},
        cacheWrite: {},
        output: {},
        thinking: {},
        firstToken: {},
        verdict: { verdict: { case: "complete", value: {} } },
      },
    });
    (
      view.tokens as unknown as { verdict: { verdict: { case: string; value: unknown } } }
    ).verdict.verdict = { case: "unaudited", value: {} };
    // ACT / ASSERT
    expect(() => drawPanel("tokens", {}, true, view)).toThrow(MalformedView);
  });

  it("refuses a TASK STATUS arm the bundle cannot name", () => {
    const view = expanded({
      tasks: [{ status: { status: { case: "pending", value: {} } }, subject: { text: "x" } }],
    });
    const row = (
      view.tasks as unknown as {
        rows: { status: { status: { case: string; value: unknown } } }[];
      }
    ).rows[0];
    row.status.status = { case: "abandoned", value: {} };
    expect(() => drawPanel("tasks", {}, true, view)).toThrow(MalformedView);
  });
});

// ---- the click invariant ---------------------------------------------------

/** One field of a forwarded record's context. */
function field(record: ClientLogRecord, key: string): unknown {
  return record.context?.[key];
}

/** What one click on the first jump row came to. */
interface Outcome {
  selected: boolean;
  notice: boolean;
}

/** Click the first jump row in PANEL and read what the click came to. */
async function clickFirstJump(drawn: Drawn, reached: boolean): Promise<Outcome> {
  drawn.panel.querySelector<HTMLElement>(".footer-row-jump")?.dispatchEvent(new MouseEvent("click"));
  await settle();
  return {
    selected: drawn.revealed.length === 1 && reached,
    notice: drawn.panel.querySelector(".footer-row-unreachable")?.textContent === "not on screen",
  };
}

describe("the click invariant: exactly one outcome, never neither", () => {
  const cases: {
    name: string;
    row: Record<string, unknown>;
    panel: "agents" | "shells" | "monitors";
    select: () => Promise<boolean>;
    reached: boolean;
    want: Outcome;
  }[] = [
    {
      name: "a known entry that lands is selected, with no notice",
      row: AGENT_ROW,
      panel: "agents",
      select: async () => true,
      reached: true,
      want: { selected: true, notice: false },
    },
    {
      name: "a known entry the feed cannot bring on screen shows the notice",
      row: AGENT_ROW,
      panel: "agents",
      select: async () => false,
      reached: false,
      want: { selected: false, notice: true },
    },
    {
      name: "a known entry whose selection answer is unreadable shows the notice",
      row: SHELL_ROW,
      panel: "shells",
      select: async () => {
        throw new MalformedView("OpenFeedResponse.result", "a oneof sets no arm");
      },
      reached: false,
      want: { selected: false, notice: true },
    },
    {
      name: "a known entry whose selection throws shows the notice",
      row: SHELL_ROW,
      panel: "shells",
      select: async () => {
        throw new Error("the reveal broke");
      },
      reached: false,
      want: { selected: false, notice: true },
    },
    {
      name: "an entry the feed has not drawn shows the notice without asking the feed",
      row: { ...AGENT_ROW, jump: unresolvedFor("notDrawn") },
      panel: "agents",
      select: async () => true,
      reached: false,
      want: { selected: false, notice: true },
    },
    {
      name: "a monitor's card that lands is selected and draws no notice",
      row: MONITOR_ROW,
      panel: "monitors",
      select: async () => true,
      reached: true,
      want: { selected: true, notice: false },
    },
    {
      name: "an unplaced monitor shows the notice without asking the feed",
      row: { ...MONITOR_ROW, jump: unresolvedFor("notDrawn") },
      panel: "monitors",
      select: async () => true,
      reached: false,
      want: { selected: false, notice: true },
    },
  ];

  it.each(cases)("$name", async ({ row, panel, select, reached, want }) => {
    // Arrange
    const drawn = drawPanel(panel, { [panel]: [row] }, select);

    // Act
    const got = await clickFirstJump(drawn, reached);

    // Assert
    expect(got).toEqual(want);
  });

  it.each(cases)("never neither: $name", async ({ row, panel, select, reached }) => {
    // Arrange
    const drawn = drawPanel(panel, { [panel]: [row] }, select);

    // Act
    const got = await clickFirstJump(drawn, reached);

    // Assert: exactly one of the two outcomes, every arm.
    expect(Number(got.selected) + Number(got.notice)).toBe(1);
  });

  it("does not ask the feed for an unresolved row", async () => {
    // Arrange
    const drawn = drawPanel("agents", { agents: [{ ...AGENT_ROW, jump: unresolvedFor("notDrawn") }] });

    // Act
    await clickFirstJump(drawn, false);

    // Assert
    expect(drawn.revealed).toHaveLength(0);
  });

  it("refuses a jump that sets no arm", () => {
    expect(() =>
      drawPanel("agents", { agents: [{ ...AGENT_ROW, jump: {} }] }),
    ).toThrow(MalformedView);
  });

  it("refuses an unresolved jump that states no reason", () => {
    expect(() =>
      drawPanel("agents", {
        agents: [{ ...AGENT_ROW, jump: { target: { case: "unresolved" as const, value: {} } } }],
      }),
    ).toThrow(MalformedView);
  });

  it("refuses a row with no work id", () => {
    expect(() => drawPanel("shells", { shells: [{ ...SHELL_ROW, work: undefined }] })).toThrow(
      MalformedView,
    );
  });
});

describe("the not-on-screen record", () => {
  it.each([
    {
      name: "an unresolved row names its work, kind, the missing entry and the reason",
      row: { ...AGENT_ROW, jump: unresolvedFor("notDrawn") },
      panel: "agents" as const,
      want: { work_id: "work-a", kind: "agents", feed_id: "unresolved", jump: "notDrawn", reason: "notDrawn" },
      level: "warn",
    },
    {
      name: "a known entry that did not land names the entry it tried",
      row: AGENT_ROW,
      panel: "agents" as const,
      want: { work_id: "work-a", kind: "agents", feed_id: "bubble-1", jump: "entry", reason: "entry_not_revealed" },
      level: "warn",
    },
    {
      name: "an unplaced monitor names its work, kind and the missing card",
      row: { ...MONITOR_ROW, jump: unresolvedFor("notDrawn") },
      panel: "monitors" as const,
      want: { work_id: "work-m", kind: "monitors", feed_id: "unresolved", jump: "notDrawn", reason: "notDrawn" },
      level: "warn",
    },
  ])("$name", async ({ row, panel, want, level }) => {
    // Arrange
    const capture = captureLogRecords();
    const drawn = drawPanel(panel, { [panel]: [row] }, false);

    // Act
    await clickFirstJump(drawn, false);

    // Assert
    const record = await forwardedRecord(capture, "footer.expanded.jump-unreachable");
    expect(record.level.case).toBe(level);
    for (const [key, value] of Object.entries(want)) expect(field(record, key)).toBe(value);
  });

  it("records an unreadable selection answer at error and files it", async () => {
    // Arrange
    const capture = captureLogRecords();
    const drawn = drawPanel("shells", { shells: [SHELL_ROW] }, async () => {
      throw new MalformedView("OpenFeedResponse.result", "a oneof sets no arm");
    });

    // Act
    await clickFirstJump(drawn, false);

    // Assert
    const record = await forwardedRecord(capture, "footer.expanded.jump-undecodable");
    expect(record.level.case).toBe("error");
    expect(field(record, "work_id")).toBe("work-s");
    expect(drawn.h.sink.reported).toContain("frameUndecodable");
  });

  it("records a thrown selection at error with its cause", async () => {
    // Arrange
    const capture = captureLogRecords();
    const drawn = drawPanel("shells", { shells: [SHELL_ROW] }, async () => {
      throw new Error("the reveal broke");
    });

    // Act
    await clickFirstJump(drawn, false);

    // Assert
    const record = await forwardedRecord(capture, "footer.expanded.jump-failed");
    expect(record.level.case).toBe("error");
    expect(field(record, "cause")).toBe("Error: the reveal broke");
  });

  it("clears a standing notice when a later click selects", async () => {
    // Arrange: the first click misses, the second lands.
    let answer = false;
    const drawn = drawPanel("agents", { agents: [AGENT_ROW] }, async () => answer);
    await clickFirstJump(drawn, false);
    answer = true;

    // Act
    const got = await clickFirstJump(drawn, true);

    // Assert
    expect(got.notice).toBe(false);
  });
});

describe("the section keeps the reader's scroll", () => {
  it("caps the section at four rows", () => {
    expect(EXPANDED_FOOTER_MAX_ROWS).toBe(4);
  });

  it("carries the cap in the markup for the stylesheet to read", () => {
    const { panel } = drawPanel("shells", { shells: [SHELL_ROW] });
    expect(panel.style.getPropertyValue("--pfooter-sheet-rows")).toBe("4");
  });

  it.each([
    { rows: 4, scrolls: false },
    { rows: 5, scrolls: true },
  ])("scrolls its own content only past the cap ($rows rows)", ({ rows, scrolls }) => {
    // Arrange
    const shells = Array.from({ length: rows }, (_unused, index) => ({
      ...SHELL_ROW,
      work: { value: `work-${index}` },
    }));

    // Act
    const { panel } = drawPanel("shells", { shells });

    // Assert
    expect(panel.classList.contains("scrolls")).toBe(scrolls);
  });

  it("redraws a push's rows inside the section it already drew", () => {
    // Arrange
    const h = harness();
    const deps: ExpandedDeps = {
      ctx: h.ctx,
      stops: createStopControls(h.ctx),
      notices: createJumpNotices(),
      redraw: () => undefined,
      selectDetachedWork: async () => true,
    };
    const first = drawFooterExpanded(expanded({ shells: [SHELL_ROW] }), "shells", deps, null);

    // Act
    const second = drawFooterExpanded(expanded({ shells: [SHELL_ROW] }), "shells", deps, first);

    // Assert
    expect(second).toBe(first);
  });

  it("keeps the reader's scroll position across a push", () => {
    // Arrange: the reader has scrolled the section.
    const h = harness();
    const deps: ExpandedDeps = {
      ctx: h.ctx,
      stops: createStopControls(h.ctx),
      notices: createJumpNotices(),
      redraw: () => undefined,
      selectDetachedWork: async () => true,
    };
    const shells = Array.from({ length: 6 }, (_unused, index) => ({ ...SHELL_ROW, work: { value: `w-${index}` } }));
    const first = drawFooterExpanded(expanded({ shells }), "shells", deps, null);
    if (first === null) throw new Error("not drawn");
    document.body.replaceChildren(first);
    first.scrollTop = 40;

    // Act
    drawFooterExpanded(expanded({ shells }), "shells", deps, first);

    // Assert
    expect(first.scrollTop).toBe(40);
  });

  it("starts a fresh section for a different panel", () => {
    // Arrange
    const h = harness();
    const deps: ExpandedDeps = {
      ctx: h.ctx,
      stops: createStopControls(h.ctx),
      notices: createJumpNotices(),
      redraw: () => undefined,
      selectDetachedWork: async () => true,
    };
    const shells = drawFooterExpanded(expanded({ shells: [SHELL_ROW] }), "shells", deps, null);

    // Act
    const monitors = drawFooterExpanded(expanded({ monitors: [MONITOR_ROW] }), "monitors", deps, shells);

    // Assert
    expect(monitors).not.toBe(shells);
  });
});
