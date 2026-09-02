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
import { createAppContext, type AppContext } from "../../src/rpc/context.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { drawCommandPanel, drawFeedCommandPanel } from "../../src/panels/panels.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const SINK: FailureSink = { report: () => undefined, retract: () => undefined };

function appContext(): AppContext {
  return createAppContext({
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
      panel: panel({ case: "context", value: { header: "142k of 200k", autoCompactLine: "off" } }),
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
        value: { header: "142k of 200k", autoCompactLine: "off" },
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

/** A context panel carrying only its two required composed lines. */
const contextView = (
  overrides: MessageInitShape<typeof ContextPanelViewSchema> = {},
): ContextPanelView =>
  create(ContextPanelViewSchema, {
    header: "142k of 200k (71%)",
    autoCompactLine: "auto-compact at 160k",
    ...overrides,
  });

const drawContext = (view: ContextPanelView): HTMLElement =>
  drawCommandPanel(panel({ case: "context", value: view }), appContext());

describe("the /context panel", () => {
  it("leads with the resolver-composed header", () => {
    expect(drawContext(contextView()).querySelector(".context-header")?.textContent).toBe(
      "142k of 200k (71%)",
    );
  });

  it("closes with the auto-compact line", () => {
    expect(drawContext(contextView()).querySelector(".context-auto-compact")?.textContent).toBe(
      "auto-compact at 160k",
    );
  });

  it("draws a category's composed figure", () => {
    const drawn = drawContext(
      contextView({ categories: [{ label: "Messages", figure: "38.1k · 19%", color: "red" }] }),
    );
    expect(drawn.querySelector(".context-category .panel-row-value")?.textContent).toBe(
      "38.1k · 19%",
    );
  });

  it("applies a vendor color the platform can read", () => {
    const drawn = drawContext(
      contextView({ categories: [{ label: "Messages", figure: "1k", color: "red" }] }),
    );
    expect(drawn.querySelector<HTMLElement>(".context-category")?.style.color).toBe("red");
  });

  it("ignores a vendor color that does not parse", () => {
    const drawn = drawContext(
      contextView({ categories: [{ label: "Messages", figure: "1k", color: "not-a-color" }] }),
    );
    expect(drawn.querySelector<HTMLElement>(".context-category")?.style.color).toBe("");
  });

  it("draws a section only when it has rows", () => {
    const drawn = drawContext(contextView());
    expect(drawn.querySelector(".context-section-heading")).toBeNull();
  });

  it("draws the memory files section when it has rows", () => {
    const drawn = drawContext(
      contextView({ memoryFiles: [{ label: "CLAUDE.md", figure: "2.1k" }] }),
    );
    expect(drawn.querySelector(".context-section-heading")?.textContent).toBe("memory files");
  });

  it("draws the slash-command roll-up when present", () => {
    const drawn = drawContext(
      contextView({ slashCommands: { line: "12 of 31 commands · 4.2k" } }),
    );
    expect(drawn.querySelector('[data-rollup="slashCommands"]')?.textContent).toBe(
      "12 of 31 commands · 4.2k",
    );
  });

  it("draws no slash-command roll-up when the fact was absent", () => {
    expect(drawContext(contextView()).querySelector('[data-rollup="slashCommands"]')).toBeNull();
  });

  it("draws the skills roll-up and its rows", () => {
    const drawn = drawContext(
      contextView({
        skills: { line: "8 of 19 skills · 6.4k", skills: [{ label: "graphify", figure: "1k" }] },
      }),
    );
    expect(drawn.querySelector('[data-rollup="skills"]')?.textContent).toBe(
      "8 of 19 skills · 6.4k",
    );
    expect(drawn.querySelectorAll(".context-skills .context-item")).toHaveLength(1);
  });

  it("draws the message planes when a breakdown is present", () => {
    const drawn = drawContext(
      contextView({ messageBreakdown: { planes: [{ label: "assistant", figure: "9k" }] } }),
    );
    expect(drawn.querySelector(".context-breakdown .context-item")?.textContent).toContain(
      "assistant",
    );
  });

  it("draws no breakdown when the fact was absent", () => {
    expect(drawContext(contextView()).querySelector(".context-breakdown")).toBeNull();
  });
});

describe("the /context tool-call fold", () => {
  const withToolCalls = (): HTMLElement =>
    drawContext(
      contextView({
        messageBreakdown: {
          planes: [],
          toolCalls: [{ label: "Bash", figure: "3k / 8k" }],
          attachments: [],
        },
      }),
    );

  it("ships folded", () => {
    expect(withToolCalls().querySelector("[data-fold]")?.getAttribute("data-folded")).toBe("true");
  });

  it("hides its rows while folded", () => {
    const body = withToolCalls().querySelector<HTMLElement>(".context-fold-body");
    expect(body?.hidden).toBe(true);
  });

  it("opens on the toggle", () => {
    const drawn = withToolCalls();
    drawn.querySelector<HTMLButtonElement>(".context-fold-toggle")?.click();
    expect(drawn.querySelector("[data-fold]")?.getAttribute("data-folded")).toBe("false");
  });

  it("shows its rows once opened", () => {
    const drawn = withToolCalls();
    drawn.querySelector<HTMLButtonElement>(".context-fold-toggle")?.click();
    expect(drawn.querySelector<HTMLElement>(".context-fold-body")?.hidden).toBe(false);
  });

  it("folds again on a second toggle", () => {
    const drawn = withToolCalls();
    const toggle = drawn.querySelector<HTMLButtonElement>(".context-fold-toggle");
    toggle?.click();
    toggle?.click();
    expect(drawn.querySelector("[data-fold]")?.getAttribute("data-folded")).toBe("true");
  });

  it("draws no fold when there are no tool calls", () => {
    const drawn = drawContext(
      contextView({ messageBreakdown: { planes: [{ label: "user", figure: "1k" }] } }),
    );
    expect(drawn.querySelector("[data-fold]")).toBeNull();
  });

  it("draws the attachments section when it has rows", () => {
    const drawn = drawContext(
      contextView({
        messageBreakdown: { planes: [], toolCalls: [], attachments: [{ label: "image", figure: "1k" }] },
      }),
    );
    expect(drawn.querySelector(".context-breakdown .context-section-heading")?.textContent).toBe(
      "attachments",
    );
  });
});
