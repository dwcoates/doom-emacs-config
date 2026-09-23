/**
 * expanded — the section that opens UNDER the strip when a chip or the tokens
 * cell is selected (owner ruling, 2026-09-13). `footer.ts` orders the dock and
 * draws the one divider above this section; nothing here positions itself.
 *
 * EVERY PANEL ARRIVES ON EVERY PUSH, fully resolved, and the SELECTION IS
 * WEBVIEW-LOCAL — the daemon never learns which one is open. That is the whole
 * point of the folded-menu convention: opening a panel costs no round trip, so
 * this module's job is to draw whichever panel the local selection names and
 * nothing else.
 *
 * THE CHIP'S COUNT IS NOT THIS SECTION'S ROW COUNT. The daemon resolves both
 * and ships both; the duplication is information, not something to reconcile,
 * so a panel never counts its rows to label anything and a chip never reads a
 * panel.
 *
 * TWO PANELS JUMP AND THREE DO NOT. Agents and shells have feed bubbles, so
 * their rows carry the bubble's `FeedId` and reveal it; tasks, monitors and
 * crons have no bubble at all, so their rows carry no jump attribute rather
 * than a jump that would have nowhere to land.
 *
 * CLOCKS TICK HERE TOO: an agent's runtime, a shell's runtime, a monitor's
 * runtime and a cron's next fire are all instants on the wire and durations on
 * screen, animated off the shared ticker.
 */
import type { FeedId } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type {
  FooterAgentRow,
  FooterAgentRowDescription,
  FooterAgentRowLabel,
  FooterAgentRowRuntime,
  FooterAgentRowTokens,
  FooterCronRow,
  FooterCronRowNextFire,
  FooterCronRowPrompt,
  FooterCronRowSchedule,
  FooterExpanded,
  FooterExpandedAgents,
  FooterExpandedCrons,
  FooterExpandedMonitors,
  FooterExpandedShells,
  FooterExpandedTasks,
  FooterExpandedTokens,
  FooterTokensAgent,
  FooterTokensLineContextGrowth,
  FooterMonitorRow,
  FooterMonitorRowDescription,
  FooterMonitorRowRuntime,
  FooterShellRow,
  FooterShellRowCommand,
  FooterShellRowRuntime,
  FooterTaskRow,
  FooterTaskRowStatus,
  FooterTaskRowSubject,
  FooterTokensLineAlarm,
  FooterTokensLineVerdict,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { formatTickedElapsed } from "../duration.js";
import { tick } from "../feed/ticking.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { stopControlHasAnswer, type StopControls } from "./stop.js";
import {
  drawFooterAllowance,
  orderedSheetAllowances,
  remainingLabel,
  type FooterActivity,
} from "./strip.js";

/** The selectable panels, named by the strip element that opens each. */
export type FooterPanel = "tokens" | "agents" | "tasks" | "shells" | "monitors" | "crons";

/** Every panel name, for the suite and for the persisted-selection check. */
export const FOOTER_PANELS: readonly FooterPanel[] = [
  "tokens",
  "agents",
  "tasks",
  "shells",
  "monitors",
  "crons",
];

/** How many rows the section shows before it scrolls its own content. */
export const EXPANDED_FOOTER_MAX_ROWS = 8;

/** What the panels need: the context to call and click through, and the jump. */
export interface ExpandedDeps {
  ctx: AppContext;
  readonly selectDetachedWork: (id: FeedId) => Promise<boolean>;
  /**
   * The activity line the strip is drawing right now, when there is one.
   *
   * NOT A SECOND RESOLUTION OF IT — the same message, handed across — so the
   * tokens sheet's usage rows and the strip's line can never disagree about a
   * figure. The sheet expands the strip's line; the strip is where it lives.
   */
  readonly activity?: FooterActivity;
  /**
   * The footer's stop controls, built once per mount.
   *
   * HANDED IN RATHER THAN BUILT HERE -- see `StopControls`: the footer redraws
   * whole on every push, and the fan-wide stop's own answer (the COUNT it
   * reached) is the one statement of that number anywhere.
   */
  readonly stops: StopControls;
}

/**
 * The selected panel, or nothing.
 *
 * A null selection is the ordinary state — the strip stands alone — and is
 * answered with `null` rather than an empty element, so the footer appends
 * nothing at all and the dock keeps exactly the height it had.
 */
export function drawFooterExpanded(
  u: FooterExpanded,
  selection: FooterPanel | null,
  deps: ExpandedDeps,
): HTMLElement | null {
  if (selection === null) return null;
  const path = "FooterExpanded";
  log.debug(`drawing the expanded footer panel: ${selection}`, {
    operation: "footer.expanded",
    context: { panel: selection },
  });

  switch (selection) {
    case "tokens":
      return panel(selection, [
        ...drawFooterExpandedTokens(requireMessage(u.tokens, `${path}.tokens`), `${path}.tokens`),
        ...drawFooterUsageRows(deps.activity, deps.ctx, "FooterStatus.activity"),
      ]);
    case "agents": {
      // THE AGENTS PANEL FOLDS AWAY WHEN NOTHING IS RUNNING. Its header carries
      // the fan-wide "stop all" control, so drawing the panel with an empty row
      // list left a "stop all" (and an empty expanded region) standing after
      // every subagent had resolved — a control for work that no longer exists.
      // The daemon ships an empty `rows` exactly when no unresolved, non-lost
      // subagent is live (the agents chip is unset in the same view), so an
      // empty list is the fold signal: the expanded region collapses rather than
      // offering a stop for nothing. The panel returns the instant a live agent
      // is back in `rows`.
      //
      // THE ONE EXCEPTION IS A STOP THAT JUST ANSWERED. "stop all" empties the
      // live set, so the push it causes carries no rows — and that push is
      // exactly when the fan-wide control is carrying its outcome ("stopped 4
      // agents"), the one statement of that count anywhere (see `stop.ts`,
      // G51). Folding on empty would erase it the instant it landed, so the
      // panel stays drawn while the control holds an answer; the answer expires
      // on its own timer (`expireOutcome`), and the next push then folds.
      const agents = requireMessage(u.agents, `${path}.agents`);
      if (agents.rows.length === 0 && !stopControlHasAnswer(deps.stops.allAgents)) return null;
      return panel(selection, drawFooterExpandedAgents(agents, deps, `${path}.agents`));
    }
    case "tasks":
      return panel(
        selection,
        drawFooterExpandedTasks(requireMessage(u.tasks, `${path}.tasks`), `${path}.tasks`),
      );
    case "shells":
      return panel(
        selection,
        drawFooterExpandedShells(requireMessage(u.shells, `${path}.shells`), deps, `${path}.shells`),
      );
    case "monitors":
      return panel(
        selection,
        drawFooterExpandedMonitors(
          requireMessage(u.monitors, `${path}.monitors`),
          deps,
          `${path}.monitors`,
        ),
      );
    case "crons":
      return panel(
        selection,
        drawFooterExpandedCrons(requireMessage(u.crons, `${path}.crons`), deps, `${path}.crons`),
      );
    default:
      return unreachableArm(`${path}.selection`, selection);
  }
}

/** The section shell: the baseline sheet, carrying its panel's name. */
function panel(name: FooterPanel, content: readonly HTMLElement[]): HTMLElement {
  const section = document.createElement("div");
  section.className = "pfooter-sheet footer-expanded list-rows";
  section.setAttribute("data-panel", name);
  section.style.setProperty("--pfooter-sheet-rows", String(EXPANDED_FOOTER_MAX_ROWS));
  if (content.length > EXPANDED_FOOTER_MAX_ROWS) section.classList.add("scrolls");
  for (const el of content) section.appendChild(el);
  return section;
}

// ---- the tokens panel -----------------------------------------------------

/** The figure lines' labels. */
const TOKEN_LINE_LABELS: Readonly<Record<string, string>> = {
  contextGrowth: "context growth",
  contextGrowthSinceCut: "context growth (since cut)",
  input: "input (uncached)",
  cacheRead: "input (cache read)",
  cacheWrite: "input (cache write)",
  output: "output",
  thinking: "thinking",
  firstToken: "first token",
};

/**
 * The turn's token accounting: one line per figure, name dim, value bright.
 *
 * NOT A LIST — these are heterogeneous facts, each its own field — so the
 * lines are drawn by name rather than iterated, and every line message is
 * ALWAYS SET so the panel's shape stays stable while the turn runs. A line with
 * no value yet draws its name and an EMPTY SLOT rather than vanishing, which is
 * what keeps the rows from jumping as figures land.
 *
 * THREE FACTS, IN THIS ORDER: the main agent's context growth (the strip
 * cell's figure), the spend summed across every agent under an "all agents"
 * header, and then — after the turn's alarm and verdict — each agent's own
 * spend under its daemon-composed name. The per-agent entries are the one TRUE
 * LIST here, drawn in the daemon's order.
 */
export function drawFooterExpandedTokens(
  u: FooterExpandedTokens,
  path: string,
): HTMLElement[] {
  const rows: HTMLElement[] = [
    drawFooterTokensLineContextGrowth(requireMessage(u.contextGrowth, `${path}.context_growth`)),
    tokenHeader("all agents"),
    tokenLine("input", requireMessage(u.input, `${path}.input`).value),
    tokenLine("cacheRead", requireMessage(u.cacheRead, `${path}.cache_read`).value),
    tokenLine("cacheWrite", requireMessage(u.cacheWrite, `${path}.cache_write`).value),
    tokenLine("output", requireMessage(u.output, `${path}.output`).value),
    tokenLine("thinking", requireMessage(u.thinking, `${path}.thinking`).value, true),
    tokenLine("firstToken", requireMessage(u.firstToken, `${path}.first_token`).value),
  ];
  if (u.alarm !== undefined) rows.push(drawFooterTokensLineAlarm(u.alarm));
  if (u.verdict !== undefined) {
    rows.push(drawFooterTokensLineVerdict(u.verdict, `${path}.verdict`));
  }
  u.agents.forEach((agent, index) => {
    rows.push(...drawFooterTokensAgent(agent, `${path}.agents[${index}]`));
  });
  return rows;
}

/**
 * The context-growth line. The since-cut marker only renames the line: the
 * figure is the daemon's either way.
 */
export function drawFooterTokensLineContextGrowth(u: FooterTokensLineContextGrowth): HTMLElement {
  const sinceCut = u.sinceCut !== undefined;
  const row = tokenLine(sinceCut ? "contextGrowthSinceCut" : "contextGrowth", u.value);
  if (sinceCut) row.setAttribute("data-since-cut", "");
  return row;
}

/** One agent's share: its name as a header, then its four figure lines. */
export function drawFooterTokensAgent(u: FooterTokensAgent, path: string): HTMLElement[] {
  const header = tokenHeader(u.label);
  header.setAttribute("data-token-agent", u.label);
  return [
    header,
    tokenLine("input", requireMessage(u.input, `${path}.input`).value),
    tokenLine("cacheRead", requireMessage(u.cacheRead, `${path}.cache_read`).value),
    tokenLine("cacheWrite", requireMessage(u.cacheWrite, `${path}.cache_write`).value),
    tokenLine("output", requireMessage(u.output, `${path}.output`).value),
  ].map((row) => {
    row.setAttribute("data-token-agent-row", u.label);
    return row;
  });
}

/** A header naming the figure lines beneath it, so a sum is not read as one agent's. */
function tokenHeader(text: string): HTMLElement {
  const header = document.createElement("div");
  header.className = "footer-panel-header footer-token-header";
  header.setAttribute("data-row", "");
  const title = document.createElement("span");
  title.className = "footer-panel-title";
  title.textContent = text;
  header.appendChild(title);
  return header;
}

/**
 * One figure line. INDENTED is the thinking line, which is a SUBCLASS of
 * output — the indent is what says a reader must not add it to the figure
 * above it.
 */
function tokenLine(name: string, value: string | undefined, indented = false): HTMLElement {
  const row = document.createElement("div");
  row.className = indented ? "footer-token-line footer-token-line-indented" : "footer-token-line";
  row.setAttribute("data-row", "");
  row.setAttribute("data-token-line", name);

  const label = document.createElement("span");
  label.className = "footer-token-name";
  label.textContent = TOKEN_LINE_LABELS[name] ?? name;
  row.appendChild(label);

  const figure = document.createElement("span");
  figure.className = "footer-token-value";
  // An unset figure is an EMPTY SLOT, never a zero: the turn has not reported
  // this number yet, and a zero would claim it reported one.
  figure.textContent = value ?? "";
  if (value === undefined) figure.setAttribute("data-empty-slot", "");
  row.appendChild(figure);
  return row;
}

/** The tripped alarm's composed sentence, verbatim. */
export function drawFooterTokensLineAlarm(u: FooterTokensLineAlarm): HTMLElement {
  const row = document.createElement("div");
  row.className = "footer-token-alarm";
  row.setAttribute("data-row", "");
  row.setAttribute("data-alarm", "");
  row.textContent = u.text;
  return row;
}

/**
 * The verdict line: the badge's arm, with the evidence the badge could not
 * carry. The clean arm has none — it draws its ✓ alone.
 */
export function drawFooterTokensLineVerdict(
  u: FooterTokensLineVerdict,
  path: string,
): HTMLElement {
  const verdict = requireCase(u.verdict, `${path}.verdict`);
  const row = document.createElement("div");
  row.className = `footer-token-verdict arm-${verdict.case}`;
  row.setAttribute("data-row", "");
  row.setAttribute("data-verdict", verdict.case);
  switch (verdict.case) {
    case "complete":
      row.textContent = "✓ reconciled";
      return row;
    case "incomplete":
      row.textContent = `✗ ${verdict.value.text}`;
      return row;
    case "invalid":
      row.textContent = `✗ ${verdict.value.text}`;
      return row;
    default: {
      const other: { case: string } = verdict;
      return unreachableArm(`${path}.verdict`, other.case);
    }
  }
}

// ---- the usage rows, under the tokens panel -------------------------------

/**
 * THE USAGE CONTENT THE STRIP CANNOT FIT, drawn in full.
 *
 * The strip is one line capped at the response bubble's width and its rate
 * line is routinely wider than that, so the second allowance window and the
 * tail of a context-budget warning were in the DOM and never on the glass.
 * They belong somewhere a reader can reach them,
 * and this sheet -- the one the tokens cell opens, already the sheet about
 * what the account is spending -- is that place.
 *
 * IT IS THE STRIP'S OWN LINE, not a second resolution of it: the same
 * `FooterActivity` the strip drew, drawn again without a width to fight. The
 * allowance cells are the strip's own drawing (`drawFooterAllowance`) in the
 * strip's own order, so a reader who opens the sheet finds the line they were
 * reading rather than a rearranged one — plus the OVERAGE window, the one
 * allowance the strip has no room for (`orderedSheetAllowances`), drawn as
 * one more row of exactly that kind when the vendor reported it.
 *
 * TWO ARMS ONLY. `rate_limited` and `context_budget` are the activity oneof's
 * usage arms; every other arm is about the turn rather than the account and
 * draws nothing here, as does an absent activity.
 */
export function drawFooterUsageRows(
  activity: FooterActivity | undefined,
  ctx: AppContext,
  path: string,
): HTMLElement[] {
  if (activity === undefined) return [];
  const kind = activity.kind;
  if (kind.case === "rateLimited") {
    const rows: HTMLElement[] = [usageHeader("account usage")];
    for (const allowance of orderedSheetAllowances(kind.value)) {
      const row = usageRow("allowance");
      row.setAttribute("data-usage-allowance", allowance.label);
      row.appendChild(
        drawFooterAllowance(allowance.value, allowance.label, { ctx }, `${path}.${allowance.label}`),
      );
      rows.push(row);
    }
    return rows;
  }
  if (kind.case === "contextBudget") {
    const row = usageRow("context-budget");
    row.textContent = kind.value.text;
    return [usageHeader("context budget"), row];
  }
  return [];
}

/** The usage block's own header, so its rows are not read as token figures. */
function usageHeader(text: string): HTMLElement {
  const header = document.createElement("div");
  header.className = "footer-panel-header footer-usage-header";
  header.setAttribute("data-row", "");
  const title = document.createElement("span");
  title.className = "footer-panel-title";
  title.textContent = text;
  header.appendChild(title);
  return header;
}

/** One usage row, named by what it says. */
function usageRow(usage: string): HTMLElement {
  const row = document.createElement("div");
  row.className = `footer-usage-row footer-usage-${usage}`;
  row.setAttribute("data-row", "");
  row.setAttribute("data-usage", usage);
  return row;
}

// ---- the agents panel -----------------------------------------------------

/**
 * The live subagents, under a header carrying the fan-wide stop.
 *
 * The header exists for the stop control (a working ruling — see `stop.ts`);
 * the rows beneath it are exactly what that control would end, which is what
 * makes this the panel it belongs in.
 */
export function drawFooterExpandedAgents(
  u: FooterExpandedAgents,
  deps: ExpandedDeps,
  path: string,
): HTMLElement[] {
  const header = document.createElement("div");
  header.className = "footer-panel-header";
  const title = document.createElement("span");
  title.className = "footer-panel-title";
  title.textContent = "live agents";
  header.appendChild(title);
  header.appendChild(deps.stops.allAgents);

  if (u.rows.length === 0) return [header, emptyRow("no live agents")];
  return [
    header,
    ...u.rows.map((row, index) => drawFooterAgentRow(row, deps, `${path}.rows[${index}]`)),
  ];
}

/** One live subagent: ⚙ · label · description · tokens · clock · ▸. */
export function drawFooterAgentRow(
  u: FooterAgentRow,
  deps: ExpandedDeps,
  path: string,
): HTMLElement {
  const target = requireMessage(u.target, `${path}.target`);
  const row = jumpRow("agents", target, deps);
  row.appendChild(glyph("agents", "⚙"));
  row.appendChild(drawFooterAgentRowLabel(requireMessage(u.label, `${path}.label`)));
  if (u.description !== undefined) {
    row.appendChild(drawFooterAgentRowDescription(u.description));
  }
  const figures = document.createElement("span");
  figures.className = "footer-row-figures";
  figures.appendChild(drawFooterAgentRowTokens(requireMessage(u.tokens, `${path}.tokens`)));
  figures.appendChild(
    drawFooterAgentRowRuntime(requireMessage(u.runtime, `${path}.runtime`), deps, `${path}.runtime`),
  );
  figures.appendChild(caret());
  row.appendChild(figures);
  return row;
}

/** The subagent's type label, verbatim. */
export function drawFooterAgentRowLabel(u: FooterAgentRowLabel): HTMLElement {
  const label = document.createElement("span");
  label.className = "footer-row-label";
  label.textContent = u.text;
  return label;
}

/** The commission's description, verbatim; never synthesized when unset. */
export function drawFooterAgentRowDescription(u: FooterAgentRowDescription): HTMLElement {
  const description = document.createElement("span");
  description.className = "footer-row-description";
  description.textContent = u.text;
  return description;
}

/** The subagent's running token sum, daemon-formatted. */
export function drawFooterAgentRowTokens(u: FooterAgentRowTokens): HTMLElement {
  const tokens = document.createElement("span");
  tokens.className = "footer-row-tokens";
  tokens.textContent = u.text;
  return tokens;
}

/** The subagent's ticking runtime, from the ORIGINAL start instant. */
export function drawFooterAgentRowRuntime(
  u: FooterAgentRowRuntime,
  deps: ExpandedDeps,
  path: string,
): HTMLElement {
  return runtimeClock(u.startedAtMs, deps, `${path}.started_at_ms`);
}

// ---- the tasks panel ------------------------------------------------------

/**
 * The tracker's checklist. NOT jump targets — a tracker task has no feed
 * bubble — and done and open tasks alike stay listed, because the chip's
 * fraction is this list's summary.
 */
export function drawFooterExpandedTasks(u: FooterExpandedTasks, path: string): HTMLElement[] {
  if (u.rows.length === 0) return [emptyRow("the task tracker is empty")];
  return u.rows.map((row, index) => drawFooterTaskRow(row, `${path}.rows[${index}]`));
}

/** One task: status glyph · subject (or its active form while running). */
export function drawFooterTaskRow(u: FooterTaskRow, path: string): HTMLElement {
  const row = plainRow("tasks");
  const status = requireMessage(u.status, `${path}.status`);
  const subject = requireMessage(u.subject, `${path}.subject`);
  const state = requireCase(status.status, `${path}.status.status`);
  row.setAttribute("data-task-status", state.case);
  row.appendChild(drawFooterTaskRowStatus(status, `${path}.status`));
  row.appendChild(drawFooterTaskRowSubject(subject, status, `${path}.status`));
  return row;
}

/**
 * The status glyph. THE ARM PICKS THE TREATMENT — a bool checkbox could not
 * hold the running state apart from the pending one, which is why the schema
 * makes it a oneof and why the running glyph breathes rather than sitting still.
 */
export function drawFooterTaskRowStatus(u: FooterTaskRowStatus, path: string): HTMLElement {
  const state = requireCase(u.status, `${path}.status`);
  switch (state.case) {
    case "pending":
      return glyph("pending", "☐");
    case "running": {
      const running = glyph("running", "◐");
      running.classList.add("footer-glyph-running");
      return running;
    }
    case "completed":
      return glyph("completed", "☑");
    default: {
      const other: { case: string } = state;
      return unreachableArm(`${path}.status`, other.case);
    }
  }
}

/**
 * The subject, or the ACTIVE FORM in its place while the task runs.
 *
 * The schema says the phrasing is drawn "in place of the subject while
 * running", so the two are never drawn together: a running task reads
 * "Running the migration…" and not the imperative subject beside it.
 */
export function drawFooterTaskRowSubject(
  u: FooterTaskRowSubject,
  status: FooterTaskRowStatus,
  path: string,
): HTMLElement {
  const state = requireCase(status.status, `${path}.status`);
  const subject = document.createElement("span");
  subject.className = "footer-row-label";
  if (state.case === "running" && state.value.activeForm !== undefined) {
    subject.setAttribute("data-active-form", "");
    subject.textContent = state.value.activeForm.text;
    return subject;
  }
  subject.textContent = u.text;
  return subject;
}

// ---- the shells panel -----------------------------------------------------

/** The live detached shells: $ · command · clock · ▸, each a jump target. */
export function drawFooterExpandedShells(
  u: FooterExpandedShells,
  deps: ExpandedDeps,
  path: string,
): HTMLElement[] {
  if (u.rows.length === 0) return [emptyRow("no live shells")];
  return u.rows.map((row, index) => drawFooterShellRow(row, deps, `${path}.rows[${index}]`));
}

/** One live shell's line. No token element: a shell has no token cost. */
export function drawFooterShellRow(
  u: FooterShellRow,
  deps: ExpandedDeps,
  path: string,
): HTMLElement {
  const target = requireMessage(u.target, `${path}.target`);
  const row = jumpRow("shells", target, deps);
  row.appendChild(glyph("shells", "$"));
  row.appendChild(drawFooterShellRowCommand(requireMessage(u.command, `${path}.command`)));
  const figures = document.createElement("span");
  figures.className = "footer-row-figures";
  figures.appendChild(
    drawFooterShellRowRuntime(requireMessage(u.runtime, `${path}.runtime`), deps, `${path}.runtime`),
  );
  figures.appendChild(caret());
  row.appendChild(figures);
  return row;
}

/** The command line, verbatim — what arrives is what is drawn. */
export function drawFooterShellRowCommand(u: FooterShellRowCommand): HTMLElement {
  const command = document.createElement("span");
  command.className = "footer-row-command";
  command.textContent = u.text;
  return command;
}

/** The shell's ticking runtime, from the ORIGINAL start instant. */
export function drawFooterShellRowRuntime(
  u: FooterShellRowRuntime,
  deps: ExpandedDeps,
  path: string,
): HTMLElement {
  return runtimeClock(u.startedAtMs, deps, `${path}.started_at_ms`);
}

// ---- the monitors panel ---------------------------------------------------

/** The live monitors. NOT jump targets: a monitor has no feed bubble. */
export function drawFooterExpandedMonitors(
  u: FooterExpandedMonitors,
  deps: ExpandedDeps,
  path: string,
): HTMLElement[] {
  if (u.rows.length === 0) return [emptyRow("no live monitors")];
  return u.rows.map((row, index) => drawFooterMonitorRow(row, deps, `${path}.rows[${index}]`));
}

/** One monitor: ◉ · description · (persistent) · clock. */
export function drawFooterMonitorRow(
  u: FooterMonitorRow,
  deps: ExpandedDeps,
  path: string,
): HTMLElement {
  const row = plainRow("monitors");
  row.appendChild(glyph("monitors", "◉"));
  row.appendChild(
    drawFooterMonitorRowDescription(requireMessage(u.description, `${path}.description`)),
  );
  if (u.persistent !== undefined) row.appendChild(marker("persistent"));
  const figures = document.createElement("span");
  figures.className = "footer-row-figures";
  figures.appendChild(
    drawFooterMonitorRowRuntime(
      requireMessage(u.runtime, `${path}.runtime`),
      deps,
      `${path}.runtime`,
    ),
  );
  row.appendChild(figures);
  return row;
}

/** What is being watched, verbatim. */
export function drawFooterMonitorRowDescription(u: FooterMonitorRowDescription): HTMLElement {
  const description = document.createElement("span");
  description.className = "footer-row-description";
  description.textContent = u.text;
  return description;
}

/** The monitor's ticking runtime, from the arm instant. */
export function drawFooterMonitorRowRuntime(
  u: FooterMonitorRowRuntime,
  deps: ExpandedDeps,
  path: string,
): HTMLElement {
  return runtimeClock(u.startedAtMs, deps, `${path}.started_at_ms`);
}

// ---- the crons panel ------------------------------------------------------

/** The scheduled jobs. NOT jump targets: a job has no feed bubble. */
export function drawFooterExpandedCrons(
  u: FooterExpandedCrons,
  deps: ExpandedDeps,
  path: string,
): HTMLElement[] {
  if (u.rows.length === 0) return [emptyRow("nothing scheduled")];
  return u.rows.map((row, index) => drawFooterCronRow(row, deps, `${path}.rows[${index}]`));
}

/** One job: ◷ · schedule · prompt · countdown · markers. */
export function drawFooterCronRow(
  u: FooterCronRow,
  deps: ExpandedDeps,
  path: string,
): HTMLElement {
  const row = plainRow("crons");
  row.appendChild(glyph("crons", "◷"));
  row.appendChild(drawFooterCronRowSchedule(requireMessage(u.schedule, `${path}.schedule`)));
  row.appendChild(drawFooterCronRowPrompt(requireMessage(u.prompt, `${path}.prompt`)));
  if (u.recurring !== undefined) row.appendChild(marker("recurring"));
  if (u.durable !== undefined) row.appendChild(marker("durable"));
  if (u.nextFire !== undefined) {
    const figures = document.createElement("span");
    figures.className = "footer-row-figures";
    figures.appendChild(
      drawFooterCronRowNextFire(u.nextFire, deps, `${path}.next_fire`),
    );
    row.appendChild(figures);
  }
  return row;
}

/** The vendor-composed human schedule, verbatim. */
export function drawFooterCronRowSchedule(u: FooterCronRowSchedule): HTMLElement {
  const schedule = document.createElement("span");
  schedule.className = "footer-row-label";
  schedule.textContent = u.text;
  return schedule;
}

/** The job's prompt, daemon-truncated; what arrives is what is drawn. */
export function drawFooterCronRowPrompt(u: FooterCronRowPrompt): HTMLElement {
  const prompt = document.createElement("span");
  prompt.className = "footer-row-description";
  prompt.textContent = u.text;
  return prompt;
}

/**
 * The countdown to the next fire, ticking. The DAEMON resolved the instant from
 * the cron expression; this end only animates the remaining duration.
 */
export function drawFooterCronRowNextFire(
  u: FooterCronRowNextFire,
  deps: ExpandedDeps,
  path: string,
): HTMLElement {
  const countdown = document.createElement("span");
  countdown.className = "footer-row-clock";
  countdown.setAttribute("data-countdown", "");
  const fireAtMs = msOf(u.fireAtMs, `${path}.fire_at_ms`);
  tick(countdown, deps.ctx.ticker, (nowMs) => {
    countdown.textContent = remainingLabel(fireAtMs - nowMs);
  });
  return countdown;
}

// ---- shared row parts -----------------------------------------------------

/** A row that reveals a feed bubble when clicked. */
function jumpRow(panelName: FooterPanel, target: FeedId, deps: ExpandedDeps): HTMLElement {
  const row = plainRow(panelName);
  row.classList.add("footer-row-jump");
  row.setAttribute("data-jump", target.value);
  row.setAttribute("role", "button");
  row.tabIndex = 0;
  row.addEventListener("click", (event: MouseEvent) => {
    event.preventDefault();
    void jump(row, target, deps);
  });
  return row;
}

/**
 * Reveal the row's bubble, saying so at the row when it could not be reached.
 *
 * `selectDetachedWork` answers `false` for a target it could not get to — a jump into a
 * collapsed shell bubble degrades to scroll-if-rendered — and that answer is
 * surfaced at the row rather than swallowed, because the user just clicked and
 * nothing else on the page would tell them the click went nowhere.
 */
async function jump(row: HTMLElement, target: FeedId, deps: ExpandedDeps): Promise<void> {
  row.removeAttribute("data-unreachable");
  const reached = await deps.selectDetachedWork(target);
  if (reached) return;
  row.setAttribute("data-unreachable", "true");
  const note = document.createElement("span");
  note.className = "footer-row-unreachable";
  note.textContent = "not on screen";
  row.appendChild(note);
  log.warn("a footer jump target could not be revealed", {
    operation: "footer.expanded.jump-unreachable",
    context: { target: target.value },
  });
}

/** A plain panel row, carrying the shared row hook. */
function plainRow(panelName: FooterPanel): HTMLElement {
  const row = document.createElement("div");
  row.className = `footer-row footer-row-${panelName}`;
  row.setAttribute("data-row", "");
  return row;
}

/** The line an empty panel draws. Unreachable in practice: its chip is unset. */
function emptyRow(text: string): HTMLElement {
  const empty = document.createElement("div");
  empty.className = "pfooter-sheet-empty footer-row-empty";
  empty.setAttribute("data-empty", "");
  empty.textContent = text;
  return empty;
}

/** A named glyph element, so the suite can target the glyph and not the text. */
function glyph(name: string, character: string): HTMLElement {
  const el = document.createElement("span");
  el.className = "footer-glyph";
  el.setAttribute("data-glyph", name);
  el.setAttribute("aria-hidden", "true");
  el.textContent = character;
  return el;
}

/** A presence marker whose being drawn is the whole fact. */
function marker(name: string): HTMLElement {
  const el = document.createElement("span");
  el.className = "footer-row-marker";
  el.setAttribute("data-marker", name);
  el.textContent = name;
  return el;
}

/** A ticking elapsed clock from an instant on the wire. */
function runtimeClock(startedAtMs: bigint, deps: ExpandedDeps, path: string): HTMLElement {
  const clock = document.createElement("span");
  clock.className = "footer-row-clock";
  const startedMs = msOf(startedAtMs, path);
  tick(clock, deps.ctx.ticker, (nowMs) => {
    clock.textContent = formatTickedElapsed(nowMs - startedMs);
  });
  return clock;
}

/** The jump affordance's caret. */
function caret(): HTMLElement {
  const el = document.createElement("span");
  el.className = "footer-row-caret";
  el.setAttribute("aria-hidden", "true");
  el.textContent = "▸";
  return el;
}
