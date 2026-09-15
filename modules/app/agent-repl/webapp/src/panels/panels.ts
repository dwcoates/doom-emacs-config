/**
 * panels — the command panels: what a slash command the DAEMON answers itself
 * looks like on screen.
 *
 * TWO DOORS, ONE DRAWING. A recognized command's resolved view arrives twice:
 * as `SubmitPromptSuccess.command_panel`, answering the caller that submitted
 * it, and as the feed's `command_panel` ROW, which is where it is DRAWN for
 * everyone. Both route into the same per-panel functions here, because two
 * drawings of one view would be two things to keep in step.
 *
 * THE ROWS ARE THE DAEMON'S. Every label, value, figure, headline and roll-up
 * line arrives composed: this end never computes a percentage, joins a list,
 * or turns a count into a sentence. The one thing it does own is the
 * PRESENTATION — which is why `/context` is explicitly custom.
 *
 * `/status` DEGRADES BY DESIGN. It carries version plus the account, model and
 * mode rows the daemon splices in, and no more; a thin panel is the settled
 * consequence of deferring the handshake fields, not a bug to pad out.
 *
 * SYNTHESIZED AND NON-DURABLE. A command panel row is minted at the moment the
 * command was answered and never appears in a paged history, so nothing here
 * may assume a second draw of the same panel will ever happen.
 */
import type { SubmitPromptCommandPanel } from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import type {
  AgentsPanelRow,
  AgentsPanelView,
} from "../../../proto/gen/ts/frontend/v1/agents_panel_pb";
import type {
  ContextPanelHeader,
  ContextPanelItem,
  ContextPanelSection,
  ContextPanelView,
} from "../../../proto/gen/ts/frontend/v1/context_panel_pb";
import type { FeedCommandPanel } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { HelpPanelRow, HelpPanelView } from "../../../proto/gen/ts/frontend/v1/help_panel_pb";
import type { McpPanelRow, McpPanelView } from "../../../proto/gen/ts/frontend/v1/mcp_panel_pb";
import type {
  StatusPanelRow,
  StatusPanelView,
} from "../../../proto/gen/ts/frontend/v1/status_panel_pb";
import type {
  TodosPanelRow,
  TodosPanelView,
} from "../../../proto/gen/ts/frontend/v1/todos_panel_pb";
import type { RowContext } from "../feed/cards/context.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { contextPercentColor, contextSectionColor } from "./context-colors.js";

/**
 * The panel a submission answered with, drawn.
 *
 * `ctx` is taken though nothing in a panel calls a verb today: the panels are
 * the one surface whose content grows command by command, and a drawing
 * function that has to change its signature to gain a click is a worse seam
 * than one that already has the context.
 */
export function drawCommandPanel(u: SubmitPromptCommandPanel, ctx: AppContext): HTMLElement {
  const path = "SubmitPromptCommandPanel";
  const panel = requireCase(u.panel, `${path}.panel`);
  log.debug("drawing a command panel", {
    operation: "panels.command-panel",
    context: { panel: panel.case },
  });
  switch (panel.case) {
    case "status":
      return drawStatusPanelView(panel.value, `${path}.status`);
    case "todos":
      return drawTodosPanelView(panel.value, `${path}.todos`);
    case "agents":
      return drawAgentsPanelView(panel.value, `${path}.agents`);
    case "mcp":
      return drawMcpPanelView(panel.value, `${path}.mcp`);
    case "context":
      return drawContextPanelView(panel.value, ctx, `${path}.context`);
    case "help":
      return drawHelpPanelView(panel.value, `${path}.help`);
    default: {
      const other: { case: string } = panel;
      return unreachableArm(`${path}.panel`, other.case);
    }
  }
}

/**
 * The feed's own command-panel row.
 *
 * Its oneof is a SUBSET of the submission's — the daemon mirrors only the four
 * panels it has a row for — and it delegates to the same drawings, so the
 * panel a caller saw and the panel the feed shows are one rendering.
 */
export function drawFeedCommandPanel(u: FeedCommandPanel, rc: RowContext): HTMLElement {
  const path = "FeedCommandPanel";
  const panel = requireCase(u.panel, `${path}.panel`);
  log.debug("drawing a command panel row", {
    operation: "panels.feed-command-panel",
    context: { panel: panel.case },
  });
  switch (panel.case) {
    case "status":
      return drawStatusPanelView(panel.value, `${path}.status`);
    case "todos":
      return drawTodosPanelView(panel.value, `${path}.todos`);
    case "mcp":
      return drawMcpPanelView(panel.value, `${path}.mcp`);
    case "context":
      return drawContextPanelView(panel.value, rc.ctx, `${path}.context`);
    default: {
      const other: { case: string } = panel;
      return unreachableArm(`${path}.panel`, other.case);
    }
  }
}

/** The panel frame every arm draws into. */
function panelElement(arm: string): HTMLElement {
  const panel = document.createElement("div");
  panel.className = "command-panel";
  panel.setAttribute("data-panel", arm);
  return panel;
}

/** The list every panel's rows sit in, delimited by the one shared rule. */
function rowList(): HTMLElement {
  const list = document.createElement("div");
  list.className = "panel-rows list-rows";
  return list;
}

/** Absence, said rather than left as a gap. */
function emptyLine(text: string): HTMLElement {
  const empty = document.createElement("div");
  empty.className = "panel-empty";
  empty.setAttribute("data-empty", "");
  empty.textContent = text;
  return empty;
}

/** One label/value line, the shape most panel rows take. */
function labelValueRow(label: string, value: string): HTMLElement {
  const row = document.createElement("div");
  row.className = "panel-row";
  row.setAttribute("data-row", "");
  const name = document.createElement("span");
  name.className = "panel-row-label";
  name.textContent = label;
  const figure = document.createElement("span");
  figure.className = "panel-row-value";
  figure.textContent = value;
  row.append(name, figure);
  return row;
}

// ---- /status ---------------------------------------------------------------

/** The /status panel: thin label/value rows, and honestly thin. */
export function drawStatusPanelView(u: StatusPanelView, path: string): HTMLElement {
  log.debug("drawing the status panel", {
    operation: "panels.status",
    context: { path, rows: u.rows.length },
  });
  const panel = panelElement("status");
  const list = rowList();
  if (u.rows.length === 0) list.appendChild(emptyLine("no session facts yet"));
  for (const [index, row] of u.rows.entries()) {
    list.appendChild(drawStatusPanelRow(row, `${path}.rows[${index}]`));
  }
  panel.appendChild(list);
  return panel;
}

/** One /status row. The value is a word or a phrase; nothing reformats it. */
export function drawStatusPanelRow(u: StatusPanelRow, path: string): HTMLElement {
  log.debug("drawing a status row", {
    operation: "panels.status.row",
    context: { path, label: u.label },
  });
  return labelValueRow(u.label, u.value);
}

// ---- /todos ----------------------------------------------------------------

/** The /todos panel: the tracker's checklist, printed. */
export function drawTodosPanelView(u: TodosPanelView, path: string): HTMLElement {
  log.debug("drawing the todos panel", {
    operation: "panels.todos",
    context: { path, rows: u.rows.length },
  });
  const panel = panelElement("todos");
  const list = rowList();
  if (u.rows.length === 0) list.appendChild(emptyLine("no tasks tracked"));
  for (const [index, row] of u.rows.entries()) {
    list.appendChild(drawTodosPanelRow(row, `${path}.rows[${index}]`));
  }
  panel.appendChild(list);
  return panel;
}

/**
 * One task's line: the glyph the arm picks, then the subject verbatim.
 *
 * HOLLOW FOR OPEN, FILLED FOR DONE, and the running one breathes — the
 * app-wide vocabulary for "not started / in flight / finished", stated with
 * box glyphs rather than emoji so the row keeps the text's own weight.
 */
export function drawTodosPanelRow(u: TodosPanelRow, path: string): HTMLElement {
  const status = requireCase(u.status, `${path}.status`);
  log.debug("drawing a todos row", {
    operation: "panels.todos.row",
    context: { path, status: status.case },
  });
  const row = document.createElement("div");
  row.className = "panel-row todo-row";
  row.setAttribute("data-row", "");
  row.setAttribute("data-todo-status", status.case);

  const glyph = document.createElement("span");
  glyph.className = "todo-glyph";
  glyph.setAttribute("aria-hidden", "true");
  switch (status.case) {
    case "pending":
      glyph.setAttribute("data-glyph", "pending");
      glyph.textContent = "☐";
      break;
    case "running":
      glyph.setAttribute("data-glyph", "running");
      glyph.textContent = "◐";
      break;
    case "completed":
      glyph.setAttribute("data-glyph", "completed");
      glyph.textContent = "☑";
      break;
    default: {
      const other: { case: string } = status;
      return unreachableArm(`${path}.status`, other.case);
    }
  }
  const subject = document.createElement("span");
  subject.className = "todo-subject";
  subject.textContent = u.subject;
  row.append(glyph, subject);
  return row;
}

// ---- /agents ---------------------------------------------------------------

/** The /agents panel: the configured agent types. */
export function drawAgentsPanelView(u: AgentsPanelView, path: string): HTMLElement {
  log.debug("drawing the agents panel", {
    operation: "panels.agents",
    context: { path, rows: u.rows.length },
  });
  const panel = panelElement("agents");
  const list = rowList();
  if (u.rows.length === 0) list.appendChild(emptyLine("no agent types configured"));
  for (const [index, row] of u.rows.entries()) {
    list.appendChild(drawAgentsPanelRow(row, `${path}.rows[${index}]`));
  }
  panel.appendChild(list);
  return panel;
}

/** One agent type. The description is optional; absent draws nothing. */
export function drawAgentsPanelRow(u: AgentsPanelRow, path: string): HTMLElement {
  log.debug("drawing an agents row", {
    operation: "panels.agents.row",
    context: { path, name: u.name, described: u.description !== undefined },
  });
  const row = document.createElement("div");
  row.className = "panel-row agent-row";
  row.setAttribute("data-row", "");
  const name = document.createElement("span");
  name.className = "panel-row-label";
  name.textContent = u.name;
  row.appendChild(name);
  if (u.description !== undefined) {
    const description = document.createElement("span");
    description.className = "panel-row-detail";
    description.textContent = u.description.text;
    row.appendChild(description);
  }
  return row;
}

// ---- /mcp ------------------------------------------------------------------

/** The /mcp panel: each server and where it stands. */
export function drawMcpPanelView(u: McpPanelView, path: string): HTMLElement {
  log.debug("drawing the mcp panel", {
    operation: "panels.mcp",
    context: { path, rows: u.rows.length },
  });
  const panel = panelElement("mcp");
  const list = rowList();
  if (u.rows.length === 0) list.appendChild(emptyLine("no mcp servers configured"));
  for (const [index, row] of u.rows.entries()) {
    list.appendChild(drawMcpPanelRow(row, `${path}.rows[${index}]`));
  }
  panel.appendChild(list);
  return panel;
}

/** What each badge says. The arm picks it; nothing is inferred from the name. */
const MCP_BADGE_LABELS: Readonly<Record<string, string>> = {
  connected: "connected",
  failed: "failed",
  needsAuth: "needs auth",
  pending: "starting",
  disabled: "disabled",
};

/**
 * One server's line: the name, then the badge the arm picks.
 *
 * The COLOR is semantic and carried by `data-mcp-status` rather than by a
 * class per arm, so the stylesheet holds the one table of arm→tone and a new
 * arm shows up as an unstyled badge rather than a mislabeled one.
 */
export function drawMcpPanelRow(u: McpPanelRow, path: string): HTMLElement {
  const status = requireCase(u.status, `${path}.status`);
  log.debug("drawing an mcp row", {
    operation: "panels.mcp.row",
    context: { path, name: u.name, status: status.case },
  });
  const label = MCP_BADGE_LABELS[status.case];
  if (label === undefined) {
    const other: { case: string } = status;
    return unreachableArm(`${path}.status`, other.case);
  }

  const row = document.createElement("div");
  row.className = "panel-row mcp-row";
  row.setAttribute("data-row", "");
  row.setAttribute("data-mcp-status", status.case);

  const name = document.createElement("span");
  name.className = "panel-row-label";
  name.textContent = u.name;

  const badge = document.createElement("span");
  badge.className = "mcp-badge";
  badge.textContent = label;
  row.append(name, badge);

  // The vendor's own account of the failure, when it gave one. Absent means it
  // said nothing, which draws nothing rather than a stand-in sentence.
  if (status.case === "failed" && status.value.detail !== undefined) {
    const detail = document.createElement("div");
    detail.className = "panel-row-detail mcp-detail";
    detail.textContent = status.value.detail.text;
    row.appendChild(detail);
  }
  return row;
}

// ---- /help -----------------------------------------------------------------

/** The /help panel: the command list. */
export function drawHelpPanelView(u: HelpPanelView, path: string): HTMLElement {
  log.debug("drawing the help panel", {
    operation: "panels.help",
    context: { path, rows: u.rows.length },
  });
  const panel = panelElement("help");
  const list = rowList();
  if (u.rows.length === 0) list.appendChild(emptyLine("no commands listed"));
  for (const [index, row] of u.rows.entries()) {
    list.appendChild(drawHelpPanelRow(row, `${path}.rows[${index}]`));
  }
  panel.appendChild(list);
  return panel;
}

/** One command's line. The description is optional. */
export function drawHelpPanelRow(u: HelpPanelRow, path: string): HTMLElement {
  log.debug("drawing a help row", {
    operation: "panels.help.row",
    context: { path, command: u.command, described: u.description !== undefined },
  });
  const row = document.createElement("div");
  row.className = "panel-row help-row";
  row.setAttribute("data-row", "");
  const command = document.createElement("span");
  command.className = "panel-row-label help-command";
  command.textContent = u.command;
  row.appendChild(command);
  if (u.description !== undefined) {
    const description = document.createElement("span");
    description.className = "panel-row-detail";
    description.textContent = u.description.text;
    row.appendChild(description);
  }
  return row;
}

// ---- /context (CUSTOM) -----------------------------------------------------

/**
 * The /context panel — the one panel whose presentation is this end's.
 *
 * The schema is a resolver-composed TREE of sections, and printing it as a flat
 * list of forty rows would bury the two facts a reader opened it for (how full
 * the window is, and what is filling it). So it is drawn as a COLLAPSIBLE
 * SECTION TREE: the structured header leads with the fill percent colored by
 * pressure, each top-level category follows as a chevron row — CLOSED by
 * default, colored a distinct palette hue by its order so the eye tracks label
 * to figure — and its detail (leaf rows and nested sub-folds like the tool-call
 * list under Messages) unfolds beneath. The auto-compact line closes the panel.
 *
 * THE COLORS ARE THIS END'S. The vendor's category color scheme is not used;
 * the section palette and the percent gradient are minted in `context-colors`.
 */
export function drawContextPanelView(
  u: ContextPanelView,
  _ctx: AppContext,
  path: string,
): HTMLElement {
  log.debug("drawing the context panel", {
    operation: "panels.context",
    context: { path, sections: u.sections.length },
  });
  const panel = panelElement("context");

  panel.appendChild(drawContextHeader(u.header, `${path}.header`));

  for (const [index, section] of u.sections.entries()) {
    panel.appendChild(drawContextSection(section, index, 0, `${path}.sections[${index}]`));
  }

  const autoCompact = document.createElement("div");
  autoCompact.className = "context-auto-compact";
  autoCompact.textContent = u.autoCompactLine;
  panel.appendChild(autoCompact);
  return panel;
}

/**
 * The header line, composed here from the daemon's parts.
 *
 * The daemon states used, total, percent and model; this end joins them into
 * "57.8k of 1M (6%) · claude-opus-5" and colors ONLY the percent, on the
 * pressure gradient. The percent is the one span with a color because it is the
 * one fact that turns into a warning as the window fills — everything else is
 * neutral text.
 */
export function drawContextHeader(u: ContextPanelHeader | undefined, path: string): HTMLElement {
  const header = document.createElement("div");
  header.className = "context-header";
  if (u === undefined) {
    // The header field is a message, so it CAN be unset; a panel without one
    // draws an empty header rather than throwing on a fact the daemon omitted.
    log.warn("the context panel arrived without a header", {
      operation: "panels.context.header",
      context: { path },
    });
    return header;
  }
  header.append(document.createTextNode(`${u.used} of ${u.total} (`));
  const percent = document.createElement("span");
  percent.className = "context-header-percent";
  percent.textContent = `${u.percent}%`;
  percent.style.color = contextPercentColor(u.percent);
  percent.setAttribute("data-percent", String(u.percent));
  header.appendChild(percent);
  let tail = ")";
  if (u.model !== "") {
    tail += ` · ${u.model}`;
  }
  header.append(document.createTextNode(tail));
  return header;
}

/**
 * One section of the tree, drawn as a collapsible fold.
 *
 * A TOP-LEVEL section (depth 0) is colored a distinct palette hue by its order,
 * applied to BOTH its label and its figure so they read as one row across the
 * gap; nested sub-folds and all leaf detail are UNCOLORED. A section with no
 * children draws as a bare colored row with NO chevron — there is nothing to
 * unfold, so offering a control would lie about it. A section with children
 * draws as a `<details>` closed by default, its caret following the open state.
 */
export function drawContextSection(
  u: ContextPanelSection,
  index: number,
  depth: number,
  path: string,
): HTMLElement {
  const color = depth === 0 ? contextSectionColor(index) : null;
  const hasChildren = u.items.length > 0 || u.sections.length > 0;

  log.debug("drawing a context section", {
    operation: "panels.context.section",
    context: { path, label: u.label, depth, children: hasChildren },
  });

  if (!hasChildren) {
    const row = labelValueRow(u.label, u.figure);
    row.classList.add("context-section", "context-section-leaf");
    row.setAttribute("data-depth", String(depth));
    if (color !== null) {
      colorRowSpans(row, color);
    }
    return row;
  }

  const details = document.createElement("details");
  details.className = "context-section";
  details.setAttribute("data-depth", String(depth));
  // Closed by default: the `open` attribute is deliberately never set.

  const summary = document.createElement("summary");
  summary.className = "context-section-summary";
  const caret = document.createElement("span");
  caret.className = "context-section-caret";
  caret.setAttribute("aria-hidden", "true");
  caret.textContent = "▸";
  const label = document.createElement("span");
  label.className = "panel-row-label";
  label.textContent = u.label;
  const figure = document.createElement("span");
  figure.className = "panel-row-value";
  figure.textContent = u.figure;
  if (color !== null) {
    label.style.color = color;
    figure.style.color = color;
  }
  summary.append(caret, label, figure);
  details.appendChild(summary);

  const body = document.createElement("div");
  body.className = "context-section-body";
  if (u.items.length > 0) {
    const list = rowList();
    for (const [i, item] of u.items.entries()) {
      list.appendChild(drawContextPanelItem(item, `${path}.items[${i}]`));
    }
    body.appendChild(list);
  }
  for (const [i, sub] of u.sections.entries()) {
    body.appendChild(drawContextSection(sub, i, depth + 1, `${path}.sections[${i}]`));
  }
  details.appendChild(body);

  details.addEventListener("toggle", () => {
    caret.textContent = details.open ? "▾" : "▸";
    log.debug(`the context section ${u.label} is ${details.open ? "open" : "closed"}`, {
      operation: "panels.context.section-toggle",
      context: { path, open: details.open },
    });
  });
  return details;
}

/** Paint both spans of a label/value row one color, for a colored leaf row. */
function colorRowSpans(row: HTMLElement, color: string): void {
  for (const span of row.querySelectorAll<HTMLElement>(".panel-row-label, .panel-row-value")) {
    span.style.color = color;
  }
}

/** One composed label/figure detail row, drawn UNCOLORED. */
export function drawContextPanelItem(u: ContextPanelItem, path: string): HTMLElement {
  log.debug("drawing a context item", {
    operation: "panels.context.item",
    context: { path, label: u.label },
  });
  const row = labelValueRow(u.label, u.figure);
  row.classList.add("context-item");
  return row;
}
