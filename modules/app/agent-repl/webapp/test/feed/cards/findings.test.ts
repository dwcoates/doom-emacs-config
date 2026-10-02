// @vitest-environment jsdom
import { type Control } from "../../../src/control.js";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  OpenInEditorResponseSchema,
  type OpenInEditorRequest,
  type OpenInEditorResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_open_in_editor_pb";
import {
  FeedFindingsRowSchema,
  FeedFindingsSchema,
  FeedRowSchema,
  type FeedFindings,
  type FeedFindingsRow,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../../src/clock.js";
import type { FailureSink } from "../../../src/failure/sink.js";
import { createAgentReplClient } from "../../../src/rpc/client.js";
import { testAppContext } from "../../rpc/app-context.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  drawFeedFindings,
  drawFindingOutcome,
  drawFindingVerdict,
  FINDINGS_OUTCOME_ARMS,
  FINDINGS_VERDICT_ARMS,
  NOTHING_FOUND_TEXT,
} from "../../../src/feed/cards/findings.js";
import type { RowContext } from "../../../src/feed/renderers.js";

const SINK: FailureSink = { report: () => {}, retract: () => {} };
const HEADING = "Findings · 3 · high";

interface Harness {
  rc: RowContext;
  opened: OpenInEditorRequest[];
}

/** A row context whose OpenInEditor answers with ANSWER. */
function harness(answer?: OpenInEditorResponse, previous?: HTMLElement): Harness {
  const opened: OpenInEditorRequest[] = [];
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      openInEditor: (req) => {
        opened.push(req);
        return (
          answer ??
          create(OpenInEditorResponseSchema, { result: { case: "success", value: {} } })
        );
      },
    });
  });
  return {
    opened,
    rc: {
      ctx: testAppContext({
        client: createAgentReplClient(transport),
        workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
        ticker: createTicker(1000),
        failures: SINK,
        composerEnabled: false,
      }),
      feed: "root",
      row: create(FeedRowSchema, {}),
      revealRow: async () => false,
      previous,
    },
  };
}

/** One finding, with every optional part switchable. */
function finding(
  opts: {
    verdict?: "confirmed" | "plausible";
    category?: string;
    location?: { text: string; path: string; line?: number };
    summary?: string;
    scenario?: string;
    outcome?: "fixed" | "skipped" | "noChange";
  } = {},
): FeedFindingsRow {
  const location = opts.location ?? { text: "daemon/server.go:214", path: "daemon/server.go", line: 214 };
  return create(FeedFindingsRowSchema, {
    verdict: opts.verdict === undefined ? { case: undefined } : { case: opts.verdict, value: {} },
    category: opts.category === undefined ? undefined : { text: opts.category },
    location: {
      text: location.text,
      path: location.path,
      ...(location.line !== undefined ? { line: location.line } : {}),
    },
    summary: { text: opts.summary ?? "the watch never reopens" },
    scenario: { text: opts.scenario ?? "kill the daemon mid-tail" },
    outcome: opts.outcome === undefined ? { case: undefined } : { case: opts.outcome, value: {} },
  });
}

/** A findings bubble over ROWS. */
function findings(rows: readonly FeedFindingsRow[]): FeedFindings {
  return create(FeedFindingsSchema, { heading: { text: HEADING }, rows: [...rows] });
}

/** Let the scripted answer settle; the router hands it back on a timer. */
async function settle(): Promise<void> {
  for (let i = 0; i < 20; i += 1) await vi.advanceTimersByTimeAsync(0);
}

beforeEach(() => {
  vi.useFakeTimers();
});

// The fake clock is this file's own; hand the real one back so a
// later file sharing this worker never inherits a frozen timer.
afterEach(() => {
  vi.useRealTimers();
});

describe("drawFeedFindings", () => {
  it("is the purple response-styled bubble", () => {
    expect(drawFeedFindings(findings([finding()]), harness().rc).className).toBe(
      "bubble md assistant agentic",
    );
  });

  it("draws the composed heading verbatim", () => {
    const el = drawFeedFindings(findings([finding()]), harness().rc);
    expect(el.querySelector(".agentic-heading")?.textContent).toBe(HEADING);
  });

  it("draws one row per finding", () => {
    const el = drawFeedFindings(findings([finding(), finding(), finding()]), harness().rc);
    expect(el.querySelectorAll("[data-finding]").length).toBe(3);
  });

  it("keeps the served order rather than re-sorting", () => {
    const el = drawFeedFindings(
      findings([finding({ summary: "first" }), finding({ summary: "second" })]),
      harness().rc,
    );
    expect([...el.querySelectorAll(".finding-summary")].map((n) => n.textContent)).toEqual([
      "first",
      "second",
    ]);
  });

  it("delimits the rows with the one shared list rule", () => {
    const el = drawFeedFindings(findings([finding()]), harness().rc);
    expect(el.querySelector(".findings-rows")?.classList.contains("list-rows")).toBe(true);
  });

  it("draws the nothing-found treatment for an empty list", () => {
    const el = drawFeedFindings(findings([]), harness().rc);
    expect(el.querySelector("[data-empty]")?.textContent).toBe(NOTHING_FOUND_TEXT);
  });

  it("marks an empty list as its own state", () => {
    expect(drawFeedFindings(findings([]), harness().rc).getAttribute("data-state")).toBe("empty");
  });

  it("draws no row list at all when there is nothing to list", () => {
    expect(drawFeedFindings(findings([]), harness().rc).querySelector(".findings-rows")).toBeNull();
  });
});

describe("a finding's verdict", () => {
  const verdicts = [
    { arm: "confirmed", text: "confirmed", severity: "verdict-confirmed" },
    { arm: "plausible", text: "plausible", severity: "verdict-plausible" },
  ] as const;

  for (const c of verdicts) {
    it(`badges the ${c.arm} verdict`, () => {
      const el = drawFeedFindings(findings([finding({ verdict: c.arm })]), harness().rc);
      expect(el.querySelector(".finding-head .badge")?.textContent).toBe(c.text);
    });

    it(`puts the ${c.arm} severity register on the row`, () => {
      const el = drawFeedFindings(findings([finding({ verdict: c.arm })]), harness().rc);
      expect(el.querySelector("[data-finding]")?.classList.contains(c.severity)).toBe(true);
    });

    it(`carries ${c.arm} as the row's verdict hook`, () => {
      const el = drawFeedFindings(findings([finding({ verdict: c.arm })]), harness().rc);
      expect(el.querySelector("[data-finding]")?.getAttribute("data-verdict")).toBe(c.arm);
    });
  }

  it("draws no badge for an unverified finding", () => {
    const el = drawFeedFindings(findings([finding()]), harness().rc);
    expect(el.querySelector(".finding-head .badge")).toBeNull();
  });

  it("marks no verdict on an unverified finding", () => {
    const el = drawFeedFindings(findings([finding()]), harness().rc);
    expect(el.querySelector("[data-finding]")?.hasAttribute("data-verdict")).toBe(false);
  });

  it("draws every verdict the schema carries", () => {
    expect([...FINDINGS_VERDICT_ARMS].sort()).toEqual(["confirmed", "plausible"].sort());
  });
});

describe("a finding's outcome", () => {
  const outcomes = [
    { arm: "fixed", text: "fixed" },
    { arm: "skipped", text: "skipped" },
    { arm: "noChange", text: "no change" },
  ] as const;

  for (const c of outcomes) {
    it(`badges the ${c.arm} outcome`, () => {
      const el = drawFeedFindings(findings([finding({ outcome: c.arm })]), harness().rc);
      expect(
        [...el.querySelectorAll(".finding-head .badge")].map((n) => n.textContent),
      ).toContain(c.text);
    });

    it(`carries ${c.arm} as the row's outcome hook`, () => {
      const el = drawFeedFindings(findings([finding({ outcome: c.arm })]), harness().rc);
      expect(el.querySelector("[data-finding]")?.getAttribute("data-outcome")).toBe(c.arm);
    });
  }

  it("draws no outcome badge on a first report", () => {
    const el = drawFeedFindings(findings([finding()]), harness().rc);
    expect(el.querySelector("[data-finding]")?.hasAttribute("data-outcome")).toBe(false);
  });

  it("draws every outcome the schema carries", () => {
    expect([...FINDINGS_OUTCOME_ARMS].sort()).toEqual(["fixed", "skipped", "noChange"].sort());
  });
});

describe("a finding's parts", () => {
  it("draws the category chip verbatim when there is one", () => {
    const el = drawFeedFindings(findings([finding({ category: "correctness" })]), harness().rc);
    expect(el.querySelector(".finding-category")?.textContent).toBe("correctness");
  });

  it("draws no category chip when the review gave none", () => {
    const el = drawFeedFindings(findings([finding()]), harness().rc);
    expect(el.querySelector(".finding-category")).toBeNull();
  });

  it("draws the summary verbatim", () => {
    const el = drawFeedFindings(findings([finding({ summary: "the watch never reopens" })]), harness().rc);
    expect(el.querySelector(".finding-summary")?.textContent).toBe("the watch never reopens");
  });

  it("draws the location line verbatim", () => {
    const el = drawFeedFindings(findings([finding()]), harness().rc);
    expect(el.querySelector(".finding-location a")?.textContent).toBe("daemon/server.go:214");
  });

  it("wears the shared editor-link hook on the location", () => {
    const el = drawFeedFindings(findings([finding()]), harness().rc);
    expect(el.querySelector(".finding-location a")?.hasAttribute("data-editor-link")).toBe(true);
  });

  it("folds the scenario by default", () => {
    const el = drawFeedFindings(findings([finding()]), harness().rc);
    expect(el.querySelector<HTMLElement>(".finding-scenario")?.hidden).toBe(true);
  });

  it("opens the scenario on the reader's click", () => {
    const el = drawFeedFindings(findings([finding()]), harness().rc);
    el.querySelector<Control>('[data-fold="finding-scenario-0"]')?.click();
    expect(el.querySelector<HTMLElement>(".finding-scenario")?.hidden).toBe(false);
  });

  it("folds each finding's scenario independently", () => {
    const el = drawFeedFindings(findings([finding(), finding()]), harness().rc);
    el.querySelector<Control>('[data-fold="finding-scenario-0"]')?.click();
    const bodies = [...el.querySelectorAll<HTMLElement>(".finding-scenario")];
    expect(bodies.map((b) => b.hidden)).toEqual([false, true]);
  });

  it("keeps a scenario the reader opened open across a re-push", () => {
    const first = drawFeedFindings(findings([finding()]), harness().rc);
    first.querySelector<Control>('[data-fold="finding-scenario-0"]')?.click();
    const second = drawFeedFindings(findings([finding()]), harness(undefined, first).rc);
    expect(second.querySelector<HTMLElement>(".finding-scenario")?.hidden).toBe(false);
  });

  it("draws the scenario text verbatim", () => {
    const el = drawFeedFindings(findings([finding({ scenario: "kill the daemon mid-tail" })]), harness().rc);
    expect(el.querySelector(".finding-scenario")?.textContent).toBe("kill the daemon mid-tail");
  });
});

describe("a finding's location click", () => {
  it("raises OpenInEditor with the served path and line as its workspace_file target", async () => {
    const h = harness();
    const el = drawFeedFindings(findings([finding()]), h.rc);
    el.querySelector<HTMLAnchorElement>(".finding-location a")?.click();
    await settle();
    expect(
      h.opened.map((r) => (r.target.case === "workspaceFile" ? [r.target.value.path, r.target.value.line] : null)),
    ).toEqual([["daemon/server.go", 214]]);
  });

  it("names no line in its target when the finding set none", async () => {
    const h = harness();
    const el = drawFeedFindings(
      findings([finding({ location: { text: "daemon/", path: "daemon/" } })]),
      h.rc,
    );
    el.querySelector<HTMLAnchorElement>(".finding-location a")?.click();
    await settle();
    const target = h.opened[0]?.target;
    expect(target?.case === "workspaceFile" ? target.value.line : "not a workspace file").toBeUndefined();
  });

  it("draws the refusal at the location when the editor could not be opened", async () => {
    const h = harness(
      create(OpenInEditorResponseSchema, {
        result: {
          case: "error",
          value: { cause: { case: "pathEscapesWorkspace", value: {} } },
        },
      }),
    );
    const el = drawFeedFindings(findings([finding()]), h.rc);
    const anchor = el.querySelector<HTMLAnchorElement>(".finding-location a");
    anchor?.click();
    await settle();
    expect(
      anchor?.parentElement?.querySelector(".refusal")?.getAttribute("data-arm"),
    ).toBe("pathEscapesWorkspace");
  });
});

describe("drawFeedFindings malformed input", () => {
  it("refuses a bubble with no heading", () => {
    const u = create(FeedFindingsSchema, { rows: [finding()] });
    expect(() => drawFeedFindings(u, harness().rc)).toThrow(MalformedView);
  });

  it("refuses a finding with no location", () => {
    const row = finding();
    (row as unknown as { location: undefined }).location = undefined;
    expect(() => drawFeedFindings(findings([row]), harness().rc)).toThrow(MalformedView);
  });

  it("refuses a finding with no summary", () => {
    const row = finding();
    (row as unknown as { summary: undefined }).summary = undefined;
    expect(() => drawFeedFindings(findings([row]), harness().rc)).toThrow(MalformedView);
  });

  it("refuses a finding with no scenario", () => {
    const row = finding();
    (row as unknown as { scenario: undefined }).scenario = undefined;
    expect(() => drawFeedFindings(findings([row]), harness().rc)).toThrow(MalformedView);
  });

  it("refuses a verdict arm this build does not know", () => {
    const row = finding();
    (row as unknown as { verdict: { case: string; value: unknown } }).verdict = {
      case: "refuted",
      value: {},
    };
    expect(() => drawFeedFindings(findings([row]), harness().rc)).toThrow(MalformedView);
  });

  it("refuses an outcome arm this build does not know", () => {
    const row = finding();
    (row as unknown as { outcome: { case: string; value: unknown } }).outcome = {
      case: "deferred",
      value: {},
    };
    expect(() => drawFeedFindings(findings([row]), harness().rc)).toThrow(MalformedView);
  });
});

// The two badge drawers are exported, and their `?? "unset"` fallbacks are
// reachable ONLY through a direct call: `drawFeedFindingsRow` guards on
// `case !== undefined` before it calls either.
describe("a badge drawn from an unset arm", () => {
  it("names the verdict as unset rather than drawing a badge", () => {
    // Arrange
    const row = document.createElement("div");
    // Act
    const thrown = (() => {
      try {
        drawFindingVerdict({ case: undefined }, row, "FeedFindingsRow.verdict");
        return undefined;
      } catch (err) {
        return err;
      }
    })();
    // Assert
    expect([
      thrown instanceof MalformedView,
      (thrown as MalformedView).path,
      (thrown as MalformedView).detail,
    ]).toEqual([true, "FeedFindingsRow.verdict", "arm 'unset' is not one this build can draw"]);
  });

  it("names the outcome as unset rather than drawing a badge", () => {
    // Arrange
    const row = document.createElement("div");
    // Act
    const thrown = (() => {
      try {
        drawFindingOutcome({ case: undefined }, row, "FeedFindingsRow.outcome");
        return undefined;
      } catch (err) {
        return err;
      }
    })();
    // Assert
    expect([
      thrown instanceof MalformedView,
      (thrown as MalformedView).path,
      (thrown as MalformedView).detail,
    ]).toEqual([true, "FeedFindingsRow.outcome", "arm 'unset' is not one this build can draw"]);
  });
});
