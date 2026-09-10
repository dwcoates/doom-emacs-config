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
  ContextPanelCategory,
  ContextPanelItem,
  ContextPanelMessageBreakdown,
  ContextPanelRollup,
  ContextPanelSkills,
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
 * The schema is a resolver-composed TREE of sections, and printing it as a
 * flat list of forty rows would bury the two facts a reader opened it for (how
 * full the window is, and what is filling it). So: the composed header line
 * leads, the category overview follows as the at-a-glance band, and the
 * detailed sections come after as delimited lists, each drawn only when it has
 * rows — an absent section draws nothing rather than an empty heading.
 *
 * THE TOOL CALLS ARE AUTO-FOLDED, as ruled: it is the longest section by far
 * and the least often wanted, so it ships closed with its own toggle.
 */
export function drawContextPanelView(
  u: ContextPanelView,
  _ctx: AppContext,
  path: string,
): HTMLElement {
  log.debug("drawing the context panel", {
    operation: "panels.context",
    context: { path, categories: u.categories.length },
  });
  const panel = panelElement("context");

  const header = document.createElement("div");
  header.className = "context-header";
  header.textContent = u.header;
  panel.appendChild(header);

  if (u.categories.length > 0) {
    const overview = document.createElement("div");
    overview.className = "context-categories list-rows";
    for (const [index, category] of u.categories.entries()) {
      overview.appendChild(
        drawContextPanelCategory(category, `${path}.categories[${index}]`),
      );
    }
    panel.appendChild(overview);
  }

  appendSection(panel, "memory files", u.memoryFiles, `${path}.memory_files`);
  appendSection(panel, "mcp tools", u.mcpTools, `${path}.mcp_tools`);
  appendSection(
    panel,
    "deferred builtin tools",
    u.deferredBuiltinTools,
    `${path}.deferred_builtin_tools`,
  );
  appendSection(panel, "system tools", u.systemTools, `${path}.system_tools`);
  appendSection(
    panel,
    "system prompt sections",
    u.systemPromptSections,
    `${path}.system_prompt_sections`,
  );
  appendSection(panel, "agents", u.agents, `${path}.agents`);

  if (u.slashCommands !== undefined) {
    panel.appendChild(
      drawContextPanelRollup(u.slashCommands, "slashCommands", `${path}.slash_commands`),
    );
  }
  if (u.skills !== undefined) {
    panel.appendChild(drawContextPanelSkills(u.skills, `${path}.skills`));
  }
  if (u.messageBreakdown !== undefined) {
    panel.appendChild(
      drawContextPanelMessageBreakdown(u.messageBreakdown, `${path}.message_breakdown`),
    );
  }

  const autoCompact = document.createElement("div");
  autoCompact.className = "context-auto-compact";
  autoCompact.textContent = u.autoCompactLine;
  panel.appendChild(autoCompact);
  return panel;
}

/**
 * One category overview row, in the VENDOR'S color.
 *
 * The color arrives as the vendor's own display string, so it is applied only
 * when the platform recognizes it as a color: a value that does not parse is
 * IGNORED with a warning rather than written into the style attribute, where
 * it would silently do nothing and leave a row claiming a hue it never got.
 */
export function drawContextPanelCategory(u: ContextPanelCategory, path: string): HTMLElement {
  const color = cssColor(u.color);
  if (u.color !== "" && color === null) {
    log.warn(`the context panel category ${u.label} carried an unusable color`, {
      operation: "panels.context.category-color",
      context: { path, color: u.color },
    });
  }
  log.debug("drawing a context category", {
    operation: "panels.context.category",
    context: { path, label: u.label },
  });
  const row = labelValueRow(u.label, u.figure);
  row.classList.add("context-category");
  if (color !== null) {
    row.style.color = color;
    row.setAttribute("data-color", u.color);
  }
  return row;
}

/**
 * The color, if the platform can read it.
 *
 * Assigning to a style declaration and reading it back is the platform's OWN
 * parser answering — no table of color names here, which would be this end
 * deciding what the vendor's vocabulary is.
 */
function cssColor(raw: string): string | null {
  if (raw === "") return null;
  const probe = document.createElement("span");
  probe.style.color = raw;
  return probe.style.color === "" ? null : raw;
}

/** A titled section of composed label/figure rows, drawn only when non-empty. */
function appendSection(
  panel: HTMLElement,
  title: string,
  items: readonly ContextPanelItem[],
  path: string,
): void {
  if (items.length === 0) return;
  panel.appendChild(sectionHeading(title));
  const list = rowList();
  for (const [index, item] of items.entries()) {
    list.appendChild(drawContextPanelItem(item, `${path}[${index}]`));
  }
  panel.appendChild(list);
}

function sectionHeading(title: string): HTMLElement {
  const heading = document.createElement("div");
  heading.className = "context-section-heading";
  heading.textContent = title;
  return heading;
}

/** One composed label/figure row. */
export function drawContextPanelItem(u: ContextPanelItem, path: string): HTMLElement {
  log.debug("drawing a context item", {
    operation: "panels.context.item",
    context: { path, label: u.label },
  });
  const row = labelValueRow(u.label, u.figure);
  row.classList.add("context-item");
  return row;
}

/** A resolver-composed roll-up line, drawn verbatim. */
export function drawContextPanelRollup(
  u: ContextPanelRollup,
  which: string,
  path: string,
): HTMLElement {
  log.debug("drawing a context roll-up", {
    operation: "panels.context.rollup",
    context: { path, which },
  });
  const line = document.createElement("div");
  line.className = "context-rollup";
  line.setAttribute("data-rollup", which);
  line.textContent = u.line;
  return line;
}

/** The skills roll-up line, plus its per-skill rows. */
export function drawContextPanelSkills(u: ContextPanelSkills, path: string): HTMLElement {
  log.debug("drawing the context skills section", {
    operation: "panels.context.skills",
    context: { path, skills: u.skills.length },
  });
  const section = document.createElement("div");
  section.className = "context-skills";
  // The skills line is a bare string field here, not a `ContextPanelRollup`,
  // so it is drawn directly rather than through that message's function: a
  // wrapper minted locally would be this end constructing a message the daemon
  // never sent.
  const line = document.createElement("div");
  line.className = "context-rollup";
  line.setAttribute("data-rollup", "skills");
  line.textContent = u.line;
  section.appendChild(line);
  if (u.skills.length > 0) {
    const list = rowList();
    for (const [index, skill] of u.skills.entries()) {
      list.appendChild(drawContextPanelItem(skill, `${path}.skills[${index}]`));
    }
    section.appendChild(list);
  }
  return section;
}

/**
 * The message-plane breakdown, with the tool calls folded away.
 *
 * The planes are the summary a reader wants; the per-tool rows are the detail
 * they occasionally want and never want first, which is exactly what an
 * automatically folded section is for.
 */
export function drawContextPanelMessageBreakdown(
  u: ContextPanelMessageBreakdown,
  path: string,
): HTMLElement {
  log.debug("drawing the context message breakdown", {
    operation: "panels.context.message-breakdown",
    context: { path, planes: u.planes.length, tool_calls: u.toolCalls.length },
  });
  const section = document.createElement("div");
  section.className = "context-breakdown";

  if (u.planes.length > 0) {
    section.appendChild(sectionHeading("messages"));
    const list = rowList();
    for (const [index, plane] of u.planes.entries()) {
      list.appendChild(drawContextPanelItem(plane, `${path}.planes[${index}]`));
    }
    section.appendChild(list);
  }
  if (u.toolCalls.length > 0) {
    section.appendChild(
      drawContextToolCallsFold(u.toolCalls, `${path}.tool_calls`),
    );
  }
  if (u.attachments.length > 0) {
    section.appendChild(sectionHeading("attachments"));
    const list = rowList();
    for (const [index, attachment] of u.attachments.entries()) {
      list.appendChild(drawContextPanelItem(attachment, `${path}.attachments[${index}]`));
    }
    section.appendChild(list);
  }
  return section;
}

/**
 * The tool-call rows, FOLDED BY DEFAULT.
 *
 * The fold state lives on the element as `data-folded` — a webview-local
 * preference of the most local kind, nothing the wire knows or should know —
 * and the caret follows it so the control announces its own state.
 */
export function drawContextToolCallsFold(
  items: readonly ContextPanelItem[],
  path: string,
): HTMLElement {
  const fold = document.createElement("div");
  fold.className = "context-fold";
  fold.setAttribute("data-fold", "toolCalls");
  fold.setAttribute("data-folded", "true");

  const toggle = document.createElement("button");
  toggle.type = "button";
  toggle.className = "context-fold-toggle";
  const caret = document.createElement("span");
  caret.className = "context-fold-caret";
  caret.setAttribute("aria-hidden", "true");
  caret.textContent = "▸";
  const label = document.createElement("span");
  label.textContent = `tool calls (${items.length})`;
  toggle.append(caret, label);
  fold.appendChild(toggle);

  const body = rowList();
  body.classList.add("context-fold-body");
  body.hidden = true;
  for (const [index, item] of items.entries()) {
    body.appendChild(drawContextPanelItem(item, `${path}[${index}]`));
  }
  fold.appendChild(body);

  toggle.addEventListener("click", (event: MouseEvent) => {
    event.preventDefault();
    const folded = fold.getAttribute("data-folded") !== "false";
    fold.setAttribute("data-folded", folded ? "false" : "true");
    body.hidden = !folded;
    caret.textContent = folded ? "▾" : "▸";
    log.debug(`the context tool-call fold is ${folded ? "open" : "closed"}`, {
      operation: "panels.context.tool-calls-fold",
      context: { open: folded },
    });
  });
  return fold;
}
