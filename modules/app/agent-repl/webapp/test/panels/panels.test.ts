// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  SubmitPromptCommandPanelSchema,
  type SubmitPromptCommandPanel,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import {
  ContextPanelHeaderSchema,
  ContextPanelViewSchema,
  type ContextPanelView,
} from "../../../proto/gen/ts/frontend/v1/context_panel_pb";
import {
  FeedCommandPanelSchema,
  FeedRowSchema,
  type FeedCommandPanel,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { McpPanelViewSchema } from "../../../proto/gen/ts/frontend/v1/mcp_panel_pb";
import { StatusPanelViewSchema } from "../../../proto/gen/ts/frontend/v1/status_panel_pb";
import { TodosPanelViewSchema } from "../../../proto/gen/ts/frontend/v1/todos_panel_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../src/clock.js";
import type { FailureSink } from "../../src/failure/sink.js";
import type { RowContext } from "../../src/feed/cards/context.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "../rpc/app-context.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { drawCommandPanel, drawFeedCommandPanel } from "../../src/panels/panels.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const SINK: FailureSink = { report: () => undefined, retract: () => undefined };

function appContext(): AppContext {
  return testAppContext({
    client: createAgentReplClient(createRouterTransport(({ service }) => service(AgentRepl, {}))),
    workspace: WORKSPACE,
    ticker: createTicker(60_000),
    failures: SINK,
    composerEnabled: false,
  });
}

function rowContext(): RowContext {
  return {
    ctx: appContext(),
    feed: "root",
    row: create(FeedRowSchema, {}),
    revealRow: async () => true,
  };
}

const panel = (
  init: MessageInitShape<typeof SubmitPromptCommandPanelSchema>["panel"],
): SubmitPromptCommandPanel => create(SubmitPromptCommandPanelSchema, { panel: init });

const feedPanel = (
  init: MessageInitShape<typeof FeedCommandPanelSchema>["panel"],
): FeedCommandPanel => create(FeedCommandPanelSchema, { panel: init });

describe("drawCommandPanel routing", () => {
  const arms: Array<{ name: string; panel: SubmitPromptCommandPanel }> = [
    { name: "status", panel: panel({ case: "status", value: { rows: [] } }) },
    { name: "todos", panel: panel({ case: "todos", value: { rows: [] } }) },
    { name: "agents", panel: panel({ case: "agents", value: { rows: [] } }) },
    { name: "mcp", panel: panel({ case: "mcp", value: { rows: [] } }) },
    {
      name: "context",
      panel: panel({ case: "context", value: { header: { used: "142k", total: "200k", percent: 71, model: "claude" }, autoCompactLine: "off" } }),
    },
    { name: "help", panel: panel({ case: "help", value: { rows: [] } }) },
  ];

  for (const arm of arms) {
    it(`draws the ${arm.name} panel with its own hook`, () => {
      expect(drawCommandPanel(arm.panel, appContext()).getAttribute("data-panel")).toBe(arm.name);
    });
  }

  it("refuses a panel whose oneof is unset", () => {
    expect(() => drawCommandPanel(create(SubmitPromptCommandPanelSchema, {}), appContext())).toThrow(
      MalformedView,
    );
  });
});

describe("drawFeedCommandPanel routing", () => {
  const arms: Array<{ name: string; panel: FeedCommandPanel }> = [
    { name: "status", panel: feedPanel({ case: "status", value: { rows: [] } }) },
    { name: "todos", panel: feedPanel({ case: "todos", value: { rows: [] } }) },
    { name: "mcp", panel: feedPanel({ case: "mcp", value: { rows: [] } }) },
    {
      name: "context",
      panel: feedPanel({
        case: "context",
        value: { header: { used: "142k", total: "200k", percent: 71, model: "claude" }, autoCompactLine: "off" },
      }),
    },
  ];

  for (const arm of arms) {
    it(`draws the feed's ${arm.name} row through the same drawing`, () => {
      expect(drawFeedCommandPanel(arm.panel, rowContext()).getAttribute("data-panel")).toBe(
        arm.name,
      );
    });
  }

  it("refuses a feed panel whose oneof is unset", () => {
    expect(() => drawFeedCommandPanel(create(FeedCommandPanelSchema, {}), rowContext())).toThrow(
      MalformedView,
    );
  });
});

describe("the /status panel", () => {
  it("draws each served row as a label and a value", () => {
    const view = create(StatusPanelViewSchema, {
      rows: [{ label: "Version", value: "1.2.3" }],
    });
    const drawn = drawCommandPanel(panel({ case: "status", value: view }), appContext());
    expect(drawn.querySelector(".panel-row-label")?.textContent).toBe("Version");
    expect(drawn.querySelector(".panel-row-value")?.textContent).toBe("1.2.3");
  });

  it("says so when no init has landed", () => {
    const drawn = drawCommandPanel(panel({ case: "status", value: { rows: [] } }), appContext());
    expect(drawn.querySelector("[data-empty]")?.textContent).toBe("no session facts yet");
  });

  it("marks every row with the shared row hook", () => {
    const view = create(StatusPanelViewSchema, {
      rows: [
        { label: "Version", value: "1.2.3" },
        { label: "Auth", value: "token" },
      ],
    });
    const drawn = drawCommandPanel(panel({ case: "status", value: view }), appContext());
    expect(drawn.querySelectorAll("[data-row]")).toHaveLength(2);
  });
});

describe("the /todos panel", () => {
  const statuses = ["pending", "running", "completed"] as const;

  for (const status of statuses) {
    it(`marks a ${status} task with its own status hook`, () => {
      const view = create(TodosPanelViewSchema, {
        rows: [{ status: { case: status, value: {} }, subject: "land it" }],
      });
      const drawn = drawCommandPanel(panel({ case: "todos", value: view }), appContext());
      expect(drawn.querySelector("[data-todo-status]")?.getAttribute("data-todo-status")).toBe(
        status,
      );
    });

    it(`gives a ${status} task its own glyph`, () => {
      const view = create(TodosPanelViewSchema, {
        rows: [{ status: { case: status, value: {} }, subject: "land it" }],
      });
      const drawn = drawCommandPanel(panel({ case: "todos", value: view }), appContext());
      expect(drawn.querySelector("[data-glyph]")?.getAttribute("data-glyph")).toBe(status);
    });
  }

  it("draws the subject verbatim", () => {
    const view = create(TodosPanelViewSchema, {
      rows: [{ status: { case: "pending", value: {} }, subject: "land it" }],
    });
    const drawn = drawCommandPanel(panel({ case: "todos", value: view }), appContext());
    expect(drawn.querySelector(".todo-subject")?.textContent).toBe("land it");
  });

  it("says so when the tracker holds nothing", () => {
    const drawn = drawCommandPanel(panel({ case: "todos", value: { rows: [] } }), appContext());
    expect(drawn.querySelector("[data-empty]")).not.toBeNull();
  });

  it("refuses a task whose status oneof is unset", () => {
    const view = create(TodosPanelViewSchema, { rows: [{ subject: "land it" }] });
    expect(() => drawCommandPanel(panel({ case: "todos", value: view }), appContext())).toThrow(
      MalformedView,
    );
  });
});

describe("the /mcp panel", () => {
  const arms = ["connected", "failed", "needsAuth", "pending", "disabled"] as const;

  for (const arm of arms) {
    it(`badges a ${arm} server by its arm`, () => {
      const view = create(McpPanelViewSchema, {
        rows: [{ name: "gns", status: { case: arm, value: {} } }],
      });
      const drawn = drawCommandPanel(panel({ case: "mcp", value: view }), appContext());
      expect(drawn.querySelector("[data-mcp-status]")?.getAttribute("data-mcp-status")).toBe(arm);
    });
  }

  it("draws the vendor's failure detail when it gave one", () => {
    const view = create(McpPanelViewSchema, {
      rows: [
        { name: "gns", status: { case: "failed", value: { detail: { text: "connect: refused" } } } },
      ],
    });
    const drawn = drawCommandPanel(panel({ case: "mcp", value: view }), appContext());
    expect(drawn.querySelector(".mcp-detail")?.textContent).toBe("connect: refused");
  });

  it("draws no detail element when the vendor gave none", () => {
    const view = create(McpPanelViewSchema, {
      rows: [{ name: "gns", status: { case: "failed", value: {} } }],
    });
    const drawn = drawCommandPanel(panel({ case: "mcp", value: view }), appContext());
    expect(drawn.querySelector(".mcp-detail")).toBeNull();
  });

  it("says so when nothing is configured", () => {
    const drawn = drawCommandPanel(panel({ case: "mcp", value: { rows: [] } }), appContext());
    expect(drawn.querySelector("[data-empty]")).not.toBeNull();
  });

  it("refuses a server whose status oneof is unset", () => {
    const view = create(McpPanelViewSchema, { rows: [{ name: "gns" }] });
    expect(() => drawCommandPanel(panel({ case: "mcp", value: view }), appContext())).toThrow(
      MalformedView,
    );
  });
});

describe("the /agents panel", () => {
  it("draws the agent type's name", () => {
    const drawn = drawCommandPanel(
      panel({ case: "agents", value: { rows: [{ name: "Explore" }] } }),
      appContext(),
    );
    expect(drawn.querySelector(".panel-row-label")?.textContent).toBe("Explore");
  });

  it("draws the description when the catalog carried one", () => {
    const drawn = drawCommandPanel(
      panel({
        case: "agents",
        value: { rows: [{ name: "Explore", description: { text: "read-only search" } }] },
      }),
      appContext(),
    );
    expect(drawn.querySelector(".panel-row-detail")?.textContent).toBe("read-only search");
  });

  it("draws no description element when the catalog carried none", () => {
    const drawn = drawCommandPanel(
      panel({ case: "agents", value: { rows: [{ name: "Explore" }] } }),
      appContext(),
    );
    expect(drawn.querySelector(".panel-row-detail")).toBeNull();
  });
});

describe("the /help panel", () => {
  it("draws the command literal", () => {
    const drawn = drawCommandPanel(
      panel({ case: "help", value: { rows: [{ command: "/compact" }] } }),
      appContext(),
    );
    expect(drawn.querySelector(".help-command")?.textContent).toBe("/compact");
  });

  it("draws the description when the catalog carried one", () => {
    const drawn = drawCommandPanel(
      panel({
        case: "help",
        value: { rows: [{ command: "/compact", description: { text: "summarize" } }] },
      }),
      appContext(),
    );
    expect(drawn.querySelector(".panel-row-detail")?.textContent).toBe("summarize");
  });
});

/** A context panel carrying only its header and auto-compact line. */
const contextView = (
  overrides: MessageInitShape<typeof ContextPanelViewSchema> = {},
): ContextPanelView =>
  create(ContextPanelViewSchema, {
    header: create(ContextPanelHeaderSchema, {
      used: "142.3k",
      total: "200k",
      percent: 71,
      model: "claude-opus-5",
    }),
    autoCompactLine: "auto-compact at 160k",
    ...overrides,
  });

const drawContext = (view: ContextPanelView): HTMLElement =>
  drawCommandPanel(panel({ case: "context", value: view }), appContext());

describe("the /context panel header", () => {
  it("composes the one-line header from the daemon's parts", () => {
    expect(drawContext(contextView()).querySelector(".context-header")?.textContent).toBe(
      "142.3k of 200k (71%) · claude-opus-5",
    );
  });

  it("colors only the percent span, not the rest of the header", () => {
    const header = drawContext(contextView()).querySelector<HTMLElement>(".context-header");
    const percent = header?.querySelector<HTMLElement>(".context-header-percent");
    expect(percent?.textContent).toBe("71%");
    expect(percent?.style.color).not.toBe("");
    // The header element itself carries no inline color — only its percent span.
    expect(header?.style.color).toBe("");
  });

  it("omits the model when the daemon stated none", () => {
    const drawn = drawContext(
      contextView({ header: { used: "1k", total: "200k", percent: 1, model: "" } }),
    );
    expect(drawn.querySelector(".context-header")?.textContent).toBe("1k of 200k (1%)");
  });

  it("draws an empty header when the daemon omitted it", () => {
    const drawn = drawContext(contextView({ header: undefined }));
    expect(drawn.querySelector(".context-header")?.textContent).toBe("");
  });

  it("closes with the auto-compact line", () => {
    expect(drawContext(contextView()).querySelector(".context-auto-compact")?.textContent).toBe(
      "auto-compact at 160k",
    );
  });
});

describe("the /context section tree", () => {
  const withOne = (): HTMLElement =>
    drawContext(
      contextView({
        sections: [
          { label: "Messages", figure: "38.1k · 19%", items: [{ label: "assistant", figure: "9k" }] },
        ],
      }),
    );

  it("renders a section with children as a chevron, closed by default", () => {
    const details = withOne().querySelector<HTMLDetailsElement>("details.context-section");
    expect(details).not.toBeNull();
    expect(details?.open).toBe(false);
    expect(details?.querySelector(".context-section-caret")?.textContent).toBe("▸");
  });

  it("gives each top-level section a distinct palette color", () => {
    const drawn = drawContext(
      contextView({
        sections: [
          { label: "A", figure: "1k", items: [{ label: "x", figure: "1" }] },
          { label: "B", figure: "2k", items: [{ label: "y", figure: "2" }] },
        ],
      }),
    );
    const summaries = drawn.querySelectorAll<HTMLElement>("details.context-section > summary");
    const colorA = summaries[0].querySelector<HTMLElement>(".panel-row-label")?.style.color;
    const colorB = summaries[1].querySelector<HTMLElement>(".panel-row-label")?.style.color;
    expect(colorA).not.toBe("");
    expect(colorB).not.toBe("");
    expect(colorA).not.toBe(colorB);
  });

  it("paints a section's label and figure the same color", () => {
    const summary = withOne().querySelector<HTMLElement>("details.context-section > summary");
    const label = summary?.querySelector<HTMLElement>(".panel-row-label")?.style.color;
    const figure = summary?.querySelector<HTMLElement>(".panel-row-value")?.style.color;
    expect(label).not.toBe("");
    expect(figure).toBe(label);
  });

  it("draws detail rows uncolored", () => {
    const item = withOne().querySelector<HTMLElement>(".context-item .panel-row-label");
    expect(item?.style.color).toBe("");
  });

  it("draws a childless section as a bare row with no chevron", () => {
    const drawn = drawContext(contextView({ sections: [{ label: "Free space", figure: "57.7k · 29%" }] }));
    expect(drawn.querySelector("details.context-section")).toBeNull();
    const leaf = drawn.querySelector<HTMLElement>(".context-section-leaf");
    expect(leaf?.querySelector(".panel-row-label")?.textContent).toBe("Free space");
    expect(leaf?.querySelector(".context-section-caret")).toBeNull();
  });

  it("colors a childless top-level section's row", () => {
    const drawn = drawContext(contextView({ sections: [{ label: "Free space", figure: "57.7k" }] }));
    const label = drawn.querySelector<HTMLElement>(".context-section-leaf .panel-row-label");
    expect(label?.style.color).not.toBe("");
  });

  it("flips the caret when a section opens", () => {
    const details = withOne().querySelector<HTMLDetailsElement>("details.context-section");
    if (details === null) throw new Error("no section rendered");
    details.open = true;
    details.dispatchEvent(new Event("toggle"));
    expect(details.querySelector(".context-section-caret")?.textContent).toBe("▾");
  });
});

describe("the /context nested sub-folds", () => {
  const withNested = (): HTMLElement =>
    drawContext(
      contextView({
        sections: [
          {
            label: "Messages",
            figure: "38.1k",
            items: [{ label: "assistant", figure: "9k" }],
            sections: [
              {
                label: "tool calls",
                items: [{ label: "Bash", figure: "call 1.2k · result 3.4k" }],
              },
            ],
          },
        ],
      }),
    );

  it("nests a sub-section as its own collapsible fold", () => {
    // One top-level Messages fold plus one nested tool-calls fold.
    expect(withNested().querySelectorAll("details.context-section")).toHaveLength(2);
  });

  it("closes a nested sub-fold by default", () => {
    const nested = withNested().querySelector<HTMLDetailsElement>(
      ".context-section-body details.context-section",
    );
    expect(nested?.open).toBe(false);
  });

  it("draws a nested sub-fold uncolored", () => {
    const nested = withNested().querySelector<HTMLDetailsElement>(
      ".context-section-body details.context-section",
    );
    const label = nested?.querySelector<HTMLElement>("summary .panel-row-label");
    expect(label?.style.color).toBe("");
  });
});

describe("an arm this build has no case for", () => {
  /** Poke an arm in AFTER `create`, which drops a case its descriptors lack. */
  const poke = (target: object, field: string, arm: string): void => {
    (target as Record<string, unknown>)[field] = { case: arm, value: {} };
  };

  it("refuses a submission panel arm the bundle cannot name", () => {
    // ARRANGE
    const foreign = panel({ case: "status", value: { rows: [] } });
    poke(foreign, "panel", "cost");
    // ACT / ASSERT
    expect(() => drawCommandPanel(foreign, appContext())).toThrow(MalformedView);
  });

  it("refuses a feed panel arm the bundle cannot name", () => {
    const foreign = feedPanel({ case: "status", value: { rows: [] } });
    poke(foreign, "panel", "help");
    expect(() => drawFeedCommandPanel(foreign, rowContext())).toThrow(MalformedView);
  });

  it("refuses a todo status arm the bundle cannot name", () => {
    const view = create(TodosPanelViewSchema, {
      rows: [{ status: { case: "pending", value: {} }, subject: "ship it" }],
    });
    poke(view.rows[0], "status", "abandoned");
    expect(() => drawCommandPanel(panel({ case: "todos", value: view }), appContext())).toThrow(
      MalformedView,
    );
  });

  it("refuses an mcp status arm the bundle has no badge for", () => {
    const view = create(McpPanelViewSchema, {
      rows: [{ name: "github", status: { case: "connected", value: {} } }],
    });
    poke(view.rows[0], "status", "degraded");
    expect(() => drawCommandPanel(panel({ case: "mcp", value: view }), appContext())).toThrow(
      MalformedView,
    );
  });
});
