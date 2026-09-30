// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  OpenInEditorResponseSchema,
  type OpenInEditorRequest,
  type OpenInEditorResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_open_in_editor_pb";
import {
  FeedPlanSchema,
  FeedRowSchema,
  type FeedPlan,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../../src/clock.js";
import type { FailureSink } from "../../../src/failure/sink.js";
import { createAgentReplClient } from "../../../src/rpc/client.js";
import { testAppContext } from "../../rpc/app-context.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { drawFeedPlan, EDIT_PLAN_TEXT, PLAN_STATE_ARMS } from "../../../src/feed/cards/plan.js";
import type { RowContext } from "../../../src/feed/renderers.js";
import { FITTING_TREE, WIDE_TREE, stagedCols, treeLineWidths, useTreeLayout } from "../../tree-layout.js";

/**
 * The oneof as an INIT shape rather than a built message: the fixtures below
 * hand plain object literals to `create`, which is what protobuf-es accepts,
 * while the built message type would demand a `$typeName` on every arm.
 */
type InitOfFeedPlan = MessageInitShape<typeof FeedPlanSchema>["state"];

const SINK: FailureSink = { report: () => {}, retract: () => {} };
const PLAN_PATH = "/w/.claude/plan.md";

interface Harness {
  rc: RowContext;
  opened: OpenInEditorRequest[];
}

/** A row context whose OpenInEditor answers with ANSWER. */
function harness(answer?: OpenInEditorResponse): Harness {
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
    },
  };
}

/** A plan bubble in one STATE. */
function plan(state: InitOfFeedPlan): FeedPlan {
  return create(FeedPlanSchema, { state });
}

/** The planned arm, with the plan's markdown and optionally an edit target. */
function planned(markdown: string, path?: string): InitOfFeedPlan {
  return {
    case: "planned",
    value: { prose: { markdown }, edit: path === undefined ? undefined : { path } },
  };
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

describe("drawFeedPlan", () => {
  it("is the purple response-styled bubble", () => {
    expect(drawFeedPlan(plan({ case: "planning", value: {} }), harness().rc).className).toBe(
      "bubble md assistant agentic",
    );
  });

  const states = [
    { arm: "planning", state: { case: "planning", value: {} }, badge: "planning" },
    { arm: "planned", state: planned("# the plan"), badge: "plan" },
    { arm: "failed", state: { case: "failed", value: { text: "turn ended in plan mode" } }, badge: "failed" },
  ] as const;

  for (const c of states) {
    it(`carries ${c.arm} as the bubble's state`, () => {
      const el = drawFeedPlan(plan(c.state), harness().rc);
      expect(el.getAttribute("data-state")).toBe(c.arm);
    });

    it(`badges the ${c.arm} arm`, () => {
      const el = drawFeedPlan(plan(c.state), harness().rc);
      expect(el.querySelector(".badge")?.textContent).toBe(c.badge);
    });
  }

  it("draws every state the schema carries", () => {
    expect([...PLAN_STATE_ARMS].sort()).toEqual(["planning", "planned", "failed"].sort());
  });

  it("draws the still-planning indicator while plan mode is open", () => {
    const el = drawFeedPlan(plan({ case: "planning", value: {} }), harness().rc);
    expect(el.querySelector(".plan-planning")).not.toBeNull();
  });

  it("draws no prose while plan mode is still open", () => {
    const el = drawFeedPlan(plan({ case: "planning", value: {} }), harness().rc);
    expect(el.querySelector(".plan-prose")).toBeNull();
  });

  it("renders the presented plan through the prose renderer", () => {
    const el = drawFeedPlan(plan(planned("# the plan")), harness().rc);
    expect(el.querySelector(".plan-prose")?.innerHTML).toContain("<h1>the plan</h1>");
  });

  it("draws the edit affordance only when the vendor named the file", () => {
    const el = drawFeedPlan(plan(planned("# the plan", PLAN_PATH)), harness().rc);
    expect(el.querySelector(".plan-edit a")?.textContent).toBe(EDIT_PLAN_TEXT);
  });

  it("draws no edit affordance for a plan with no file", () => {
    const el = drawFeedPlan(plan(planned("# the plan")), harness().rc);
    expect(el.querySelector(".plan-edit")).toBeNull();
  });

  it("wears the shared editor-link hook on the edit affordance", () => {
    const el = drawFeedPlan(plan(planned("# the plan", PLAN_PATH)), harness().rc);
    expect(el.querySelector(".plan-edit a")?.hasAttribute("data-editor-link")).toBe(true);
  });

  it("raises OpenInEditor with the served path as its workspace_file target, verbatim", async () => {
    const h = harness();
    const el = drawFeedPlan(plan(planned("# the plan", PLAN_PATH)), h.rc);
    el.querySelector<HTMLAnchorElement>(".plan-edit a")?.click();
    await settle();
    expect(h.opened.map((r) => (r.target.case === "workspaceFile" ? r.target.value.path : null))).toEqual([PLAN_PATH]);
  });

  it("names no line in its target, so the editor opens the plan at its top", async () => {
    const h = harness();
    const el = drawFeedPlan(plan(planned("# the plan", PLAN_PATH)), h.rc);
    el.querySelector<HTMLAnchorElement>(".plan-edit a")?.click();
    await settle();
    const target = h.opened[0]?.target;
    expect(target?.case === "workspaceFile" ? target.value.line : "not a workspace file").toBeUndefined();
  });

  it("draws nothing extra when the editor opened", async () => {
    const h = harness();
    const el = drawFeedPlan(plan(planned("# the plan", PLAN_PATH)), h.rc);
    el.querySelector<HTMLAnchorElement>(".plan-edit a")?.click();
    await settle();
    expect(el.querySelector(".plan-edit .refusal")).toBeNull();
  });

  it("draws the refusal at the link when the editor could not be opened", async () => {
    const h = harness(
      create(OpenInEditorResponseSchema, {
        result: {
          case: "error",
          value: { cause: { case: "pathEscapesWorkspace", value: {} } },
        },
      }),
    );
    const el = drawFeedPlan(plan(planned("# the plan", PLAN_PATH)), h.rc);
    const anchor = el.querySelector<HTMLAnchorElement>(".plan-edit a");
    anchor?.click();
    await settle();
    expect(
      anchor?.parentElement?.querySelector(".refusal")?.getAttribute("data-arm"),
    ).toBe("pathEscapesWorkspace");
  });

  it("draws the failed reason verbatim", () => {
    const el = drawFeedPlan(
      plan({ case: "failed", value: { text: "turn ended in plan mode" } }),
      harness().rc,
    );
    expect(el.querySelector(".plan-failed")?.textContent).toBe("turn ended in plan mode");
  });
});

describe("drawFeedPlan malformed input", () => {
  it("refuses a bubble whose state oneof is unset", () => {
    expect(() => drawFeedPlan(create(FeedPlanSchema, {}), harness().rc)).toThrow(MalformedView);
  });

  it("refuses a presented plan with no prose", () => {
    const u = plan({ case: "planned", value: {} });
    expect(() => drawFeedPlan(u, harness().rc)).toThrow(MalformedView);
  });

  it("refuses a state arm this build does not know", () => {
    const u = plan({ case: "planning", value: {} });
    (u as unknown as { state: { case: string; value: unknown } }).state = {
      case: "abandoned",
      value: {},
    };
    expect(() => drawFeedPlan(u, harness().rc)).toThrow(MalformedView);
  });
});

describe("a tree in the plan", () => {
  const staged = useTreeLayout();

  /** A planned bubble of MARKDOWN, attached under its own column. */
  function mounted(markdown: string): HTMLElement {
    const el = drawFeedPlan(plan(planned(markdown)), harness().rc);
    const column = document.createElement("div");
    column.append(el);
    document.body.append(column);
    return el;
  }

  it("wraps at the agentic bubble's own cap", () => {
    // Arrange / Act
    const el = mounted(WIDE_TREE);
    // Assert
    const widths = treeLineWidths(el);
    expect(widths.length).toBeGreaterThan(3);
    expect(Math.max(...widths)).toBeLessThanOrEqual(stagedCols(staged.layout));
  });

  it("never wraps below its max width", () => {
    // Arrange / Act
    const el = mounted(FITTING_TREE);
    // Assert
    expect(treeLineWidths(el)).toHaveLength(3);
  });
});

describe("a re-push of the plan", () => {
  it("updates the previous draw in place, keeping its scroll box", () => {
    // Arrange
    const h = harness();
    const first = drawFeedPlan(plan({ case: "planning", value: {} }), h.rc);
    const box = first.querySelector(".bubble-scroll");
    // Act
    const again = drawFeedPlan(plan(planned("# the plan")), { ...h.rc, previous: first });
    // Assert
    expect([again, again.querySelector(".bubble-scroll")]).toEqual([first, box]);
  });

  it("carries the new state's content", () => {
    // Arrange
    const h = harness();
    const first = drawFeedPlan(plan({ case: "planning", value: {} }), h.rc);
    // Act
    drawFeedPlan(plan(planned("# the plan")), { ...h.rc, previous: first });
    // Assert
    expect([first.getAttribute("data-state"), first.querySelector(".plan-planning")]).toEqual(["planned", null]);
  });
});
