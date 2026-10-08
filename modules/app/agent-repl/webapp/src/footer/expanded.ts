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
 * THE DETACHED-WORK ROWS JUMP; THE REST DO NOT. Agent, shell and monitor rows
 * are detached work, and each carries the daemon's `FooterJump`: EITHER the
 * entry's `FeedId` (the click selects it and the feed scrolls to it) OR the
 * reason the daemon cannot name one (the click shows "not on screen" at the
 * row and records why). Exactly one of the two happens on every click — a
 * click that does neither is unrepresentable (see `jump`). Tasks and crons
 * have no bubble at all, so their rows carry no jump.
 *
 * THE PANEL KEEPS ITS OWN SCROLL. It shows at most `EXPANDED_FOOTER_MAX_ROWS`
 * rows and scrolls past that; the scroll is the reader's, so a push redraws
 * the rows INSIDE the section it already drew (`previous`) rather than
 * replacing the section, and nothing here writes a scroll position.
 *
 * CLOCKS TICK HERE TOO: an agent's runtime, a shell's runtime, a monitor's
 * runtime and a cron's next fire are all instants on the wire and durations on
 * screen, animated off the shared ticker.
 */
import { armButtonRole } from "../control.js";
import type {
  FooterAgentRow,
  FooterAgentRowDescription,
  FooterAgentRowLabel,
  FooterAgentRowRuntime,
  FooterAgentRowTokens,
  FooterAgentRowWaitingForApi,
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
  FooterJump,
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
import type { FeedId } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { placeChildren } from "../dom.js";
import { columnHeader, COLUMNS_ROW_CLASS } from "../columns.js";
import { liveElapsedClock, settledElapsedClock } from "../elapsed-clock.js";
import { frameUndecodable } from "../failure/sink.js";
import { stopTicking } from "../feed/ticking.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { isMalformedView } from "../rpc/malformed.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { toneClass } from "../vocab.js";
import { footerClockSpan } from "./clock-span.js";
import { stopControlHasAnswer, type StopControls } from "./stop.js";
import {
  drawFooterAllowance,
  drawGivesUpCountdown,
  enduringUsage,
  orderedAllowances,
  remainingLabel,
  type FooterActivity,
} from "./activity.js";

/** The selectable panels, named by the strip element that opens each. */
export type FooterPanel =
  | "tokens"
  | "agents"
  | "tasks"
  | "shells"
  | "monitors"
  | "crons";

/** Every panel name, for the suite and for the persisted-selection check. */
export const FOOTER_PANELS: readonly FooterPanel[] = [
  "tokens",
  "agents",
  "tasks",
  "shells",
  "monitors",
  "crons",
];

/**
 * How many rows the section shows before it scrolls its own content (owner
 * ruling, 2026-09-23: at most four). The stylesheet reads it from the markup
 * (`--pfooter-sheet-rows`), so the cap has one owner.
 */
export const EXPANDED_FOOTER_MAX_ROWS = 4;

/**
 * THE CLICK OUTCOMES THE FOOTER IS HOLDING, per row, across whole-view pushes.
 *
 * WHY IT IS STATE AND NOT A MARK ON THE ROW. The footer redraws whole on every
 * push, and a live subagent's token count pushes continuously; a notice
 * appended to the row element that was clicked landed on an element the next
 * push had already thrown away, so the reader saw neither a selection nor a
 * notice. The outcome is held HERE, keyed by the row, and every draw paints it
 * — so it is on the glass from the redraw the click itself asks for.
 *
 * It stands while the row's jump still resolves the way it did when clicked,
 * and goes when the row leaves the view, its jump changes, or a later click on
 * it selects.
 */
export interface JumpNotices {
  /** Whether a notice stands for KEY at RESOLUTION. */
  standing(key: string, resolution: string): boolean;
  /** Raise the notice for KEY, clicked at RESOLUTION. */
  raise(key: string, resolution: string): void;
  /** Drop KEY's notice. */
  clear(key: string): void;
  /** Drop every notice whose row is not in PRESENT. */
  prune(present: ReadonlySet<string>): void;
}

/** The mount's one notice register. */
export function createJumpNotices(): JumpNotices {
  const held = new Map<string, string>();
  return {
    standing: (key, resolution) => held.get(key) === resolution,
    raise: (key, resolution) => {
      held.set(key, resolution);
    },
    clear: (key) => {
      held.delete(key);
    },
    prune: (present) => {
      for (const key of [...held.keys()]) if (!present.has(key)) held.delete(key);
    },
  };
}

/** What the panels need: the context to call and click through, and the jump. */
export interface ExpandedDeps {
  ctx: AppContext;
  readonly selectDetachedWork: (id: FeedId) => Promise<boolean>;
  /** The mount's click outcomes, painted on every draw. */
  readonly notices: JumpNotices;
  /** Redraw the footer from its last view, so a click's outcome is drawn. */
  readonly redraw: () => void;
  /**
   * The activity cell the strip is drawing right now.
   *
   * NOT A SECOND RESOLUTION OF IT — the same message, handed across — so the
   * tokens sheet's usage rows and the strip's enduring line can never disagree
   * about a figure. The sheet expands the strip's line; the strip is where it
   * lives.
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
  previous: HTMLElement | null = null,
): HTMLElement | null {
  if (selection === null) return null;
  const panel = (name: FooterPanel, content: readonly HTMLElement[]): HTMLElement =>
    drawSection(name, content, previous);
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

/**
 * The section shell: the baseline sheet, carrying its panel's name.
 *
 * THE SAME PANEL'S SECTION IS REUSED. PREVIOUS is the section the last draw
 * returned; when it is this panel's, its rows are replaced INSIDE it, so the
 * section is never detached and its scroll box keeps the reader's position
 * across the push (a detached and re-attached box resets to the top). Every
 * dropped row is stopped first, so no clock outlives its element.
 */
function drawSection(
  name: FooterPanel,
  content: readonly HTMLElement[],
  previous: HTMLElement | null,
): HTMLElement {
  const reuse = previous !== null && previous.getAttribute("data-panel") === name;
  const section = reuse ? previous : document.createElement("div");
  if (!reuse) {
    section.className = "pfooter-sheet footer-expanded list-rows";
    section.setAttribute("data-panel", name);
    section.style.setProperty("--pfooter-sheet-rows", String(EXPANDED_FOOTER_MAX_ROWS));
  }
  section.classList.toggle("scrolls", content.length > EXPANDED_FOOTER_MAX_ROWS);
  const kept = new Set<Node>(content);
  for (const child of [...section.children]) {
    if (!kept.has(child)) stopTicking(child);
  }
  placeChildren(section, content);
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
 * The strip is one line capped at the response bubble's width and its
 * enduring line is routinely wider than that, so an allowance window was in
 * the DOM and never on the glass. It belongs somewhere a reader can reach it,
 * and this sheet -- the one the tokens cell opens, already the sheet about what
 * the account is spending -- is that place.
 *
 * IT IS THE STRIP'S OWN ENDURING USAGE, not a second resolution of it: the
 * same message the strip's cell carries, drawn again without a width to fight.
 * The allowance cells are the strip's own drawing (`drawFooterAllowance`) in
 * the strip's own order (`orderedAllowances`), so a reader who opens the sheet
 * finds the figures they were reading rather than a rearranged list.
 *
 * THE UNPINNED CELL ONLY. The enduring line is shipped beneath the transient
 * whenever no salient line stands, and not at all while one does; a cell with
 * no usage read yet, or none shipped, draws nothing here, as does an absent
 * activity.
 */
export function drawFooterUsageRows(
  activity: FooterActivity | undefined,
  ctx: AppContext,
  path: string,
): HTMLElement[] {
  if (activity === undefined) return [];
  const usage = enduringUsage(activity, path);
  if (usage === undefined) return [];
  const usagePath = `${path}.unpinned.enduring.usage`;
  const rows: HTMLElement[] = [usageHeader("account usage")];
  for (const allowance of orderedAllowances(usage)) {
    const row = usageRow("allowance");
    row.setAttribute("data-usage-allowance", allowance.label);
    row.appendChild(
      drawFooterAllowance(allowance.value, allowance.label, { ctx }, `${usagePath}.${allowance.label}`),
    );
    rows.push(row);
  }
  return rows;
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
 *
 * FIXED COLUMNS (owner request, 2026-10-01). The panel is ONE grid whose rows
 * share its columns (`subgrid`, styles.css): the row's main cell, then tokens,
 * then duration, then the caret. Every row's tokens and duration therefore
 * share one width, at least wide enough for the largest common value and
 * growing with a longer one, so a clock going from "4m 59s" to "5m" no longer
 * moves the token count. The header is the same four cells: the "stop all"
 * control on the LEFT (where the "live agents" title was), then the "tokens"
 * and "duration" column headers above their columns.
 */
export function drawFooterExpandedAgents(
  u: FooterExpandedAgents,
  deps: ExpandedDeps,
  path: string,
): HTMLElement[] {
  const header = document.createElement("div");
  header.className = `footer-panel-header ${COLUMNS_ROW_CLASS}`;
  header.appendChild(deps.stops.allAgents);
  header.appendChild(columnHeader("tokens"));
  header.appendChild(columnHeader("duration"));
  header.appendChild(document.createElement("span"));

  if (u.rows.length === 0) return [header, emptyRow("no live agents")];
  return [
    header,
    ...u.rows.map((row, index) => drawFooterAgentRow(row, deps, `${path}.rows[${index}]`)),
  ];
}

/**
 * One subagent with work ahead of it: ⚙ · label · description · [its wait for
 * the API] · tokens · clock · ▸. The row's STATE is stamped as `data-state`;
 * a running agent draws nothing more, a waiting one its wait.
 */
export function drawFooterAgentRow(
  u: FooterAgentRow,
  deps: ExpandedDeps,
  path: string,
): HTMLElement {
  const row = jumpRow("agents", u.work, u.jump, deps, path);
  row.classList.add(COLUMNS_ROW_CLASS);
  // THE MAIN CELL holds everything before the figure columns, so the row is
  // exactly the grid's four cells whatever optional parts it carries.
  const main = document.createElement("span");
  main.className = "footer-row-main";
  main.appendChild(glyph("agents", "⚙"));
  main.appendChild(drawFooterAgentRowLabel(requireMessage(u.label, `${path}.label`)));
  if (u.description !== undefined) {
    main.appendChild(drawFooterAgentRowDescription(u.description));
  }
  const state = requireCase(u.state, `${path}.state`);
  row.setAttribute("data-state", state.case);
  switch (state.case) {
    case "running":
      break;
    case "waitingForApi":
      main.appendChild(drawFooterAgentRowWaitingForApi(state.value, deps, `${path}.waiting_for_api`));
      break;
    default: {
      const other: { case: string } = state;
      return unreachableArm(`${path}.state`, other.case);
    }
  }
  row.appendChild(main);
  row.appendChild(drawFooterAgentRowTokens(requireMessage(u.tokens, `${path}.tokens`)));
  row.appendChild(
    drawFooterAgentRowRuntime(requireMessage(u.runtime, `${path}.runtime`), deps, `${path}.runtime`),
  );
  row.appendChild(caret());
  return finishJumpRow(row, main);
}


/**
 * An agent waiting for the API: "waiting for the API · gives up in 24m",
 * the deadline ticking down from the shim's shipped instant through the same
 * countdown the network-resume transient draws.
 */
export function drawFooterAgentRowWaitingForApi(
  u: FooterAgentRowWaitingForApi,
  deps: ExpandedDeps,
  path: string,
): HTMLElement {
  const state = document.createElement("span");
  state.className = "footer-row-state";
  state.appendChild(document.createTextNode("waiting for the API · "));
  state.appendChild(drawGivesUpCountdown(u.givesUpAtMs, deps, `${path}.gives_up_at_ms`));
  return state;
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
  const row = jumpRow("shells", u.work, u.jump, deps, path);
  row.appendChild(glyph("shells", "$"));
  row.appendChild(drawFooterShellRowCommand(requireMessage(u.command, `${path}.command`)));
  const figures = document.createElement("span");
  figures.className = "footer-row-figures";
  figures.appendChild(
    drawFooterShellRowRuntime(requireMessage(u.runtime, `${path}.runtime`), deps, `${path}.runtime`),
  );
  figures.appendChild(caret());
  row.appendChild(figures);
  return finishJumpRow(row);
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

/**
 * The live monitors. Each is a jump row, exactly as an agent or shell row is:
 * its jump names the Monitor call's tool-call card, which a click centers.
 */
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
  const row = jumpRow("monitors", u.work, u.jump, deps, path);
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
  figures.appendChild(caret());
  row.appendChild(figures);
  return finishJumpRow(row);
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
  const fireAtMs = msOf(u.fireAtMs, `${path}.fire_at_ms`);
  return footerClockSpan(deps.ctx.ticker, "countdown", "footer-row-clock", (span, nowMs) => {
    span.textContent = remainingLabel(fireAtMs - nowMs);
  });
}

// ---- shared row parts -----------------------------------------------------

/** The attribute naming a row's detached work, verbatim. */
export const WORK_ID_ATTRIBUTE = "data-work-id";

/** The attribute naming why a row's entry is unresolved (the reason arm). */
export const JUMP_UNRESOLVED_ATTRIBUTE = "data-jump-unresolved";

/**
 * A detached-work row: a click target whose outcome the daemon's `FooterJump`
 * decides.
 *
 * THE ARM IS READ AT DRAW, not at click: an unset jump, an unset arm or an
 * unset reason is a `MalformedView` here, so a row that could not say where
 * its click lands is never drawn as one that silently does nothing.
 */
function jumpRow(
  panelName: FooterPanel,
  workMsg: FooterJumpRowWork | undefined,
  jumpMsg: FooterJump | undefined,
  deps: ExpandedDeps,
  path: string,
): HTMLElement {
  const work = requireMessage(workMsg, `${path}.work`).value;
  const jump = requireMessage(jumpMsg, `${path}.jump`);
  const target: JumpTarget = requireCase(jump.target, `${path}.jump.target`);
  const resolution = jumpResolutionOf(target, `${path}.jump`);
  const key = `${panelName}:${work}`;
  const row = plainRow(panelName);
  row.classList.add("footer-row-jump");
  row.setAttribute(WORK_ID_ATTRIBUTE, work);
  if (target.case === "entry") row.setAttribute("data-jump", target.value.value);
  else row.setAttribute(JUMP_UNRESOLVED_ATTRIBUTE, resolution);
  armButtonRole(row);
  if (deps.notices.standing(key, resolution)) {
    row.setAttribute("data-unreachable", "true");
    row.setAttribute(NOTICE_PENDING, "");
  }
  row.addEventListener("click", (event: MouseEvent) => {
    event.preventDefault();
    void jump_({ panel: panelName, key, work, target, resolution }, deps);
  });
  return row;
}

/** The attribute a row carries between its draw and its finish when a notice stands. */
const NOTICE_PENDING = "data-notice-pending";

/**
 * Close a jump row: the standing notice, if any, is the row's LAST element, so
 * it reads after the figures exactly as the old in-place note did.
 */
function finishJumpRow(row: HTMLElement, into: HTMLElement = row): HTMLElement {
  if (!row.hasAttribute(NOTICE_PENDING)) return row;
  row.removeAttribute(NOTICE_PENDING);
  const note = document.createElement("span");
  note.className = "footer-row-unreachable";
  note.textContent = "not on screen";
  into.appendChild(note);
  return row;
}

/** The generated work element's shape, whichever row kind carried it. */
interface FooterJumpRowWork {
  readonly value: string;
}

/** The jump arm as the click reads it. */
type JumpTarget = Exclude<FooterJump["target"], { case: undefined }>;

/** A click, with everything its outcome and its record need. */
interface JumpClick {
  readonly panel: FooterPanel;
  readonly key: string;
  readonly work: string;
  readonly target: JumpTarget;
  readonly resolution: string;
}

/** Name a jump for the record and the notice: "entry", or the reason arm. */
function jumpResolutionOf(target: JumpTarget, path: string): string {
  switch (target.case) {
    case "entry":
      return "entry";
    case "unresolved":
      return requireCase(target.value.reason, `${path}.unresolved.reason`).case;
    default: {
      const other: { case: string } = target;
      return unreachableArm(`${path}.target`, other.case);
    }
  }
}

/**
 * ONE CLICK, EXACTLY ONE OUTCOME (owner ruling, 2026-09-23): the entry is
 * selected and the feed scrolls to it, OR the row says "not on screen" and the
 * reason is recorded with the row's work id, kind and feed id. Never neither.
 *
 * - `entry`: the feed's detached-work selection (`selectDetachedWork`, the
 *   `detachedWorkSelected` scroll cause). If it lands, any standing notice for
 *   the row goes. If it does not — the reveal could not bring the entry onto
 *   the page, the answer was unreadable, or the call threw — the notice is
 *   raised: the entry is known and still not on screen.
 * - `unresolved`: the daemon already said it cannot name the entry, so the
 *   notice is raised at once and nothing is asked of the feed.
 *
 * Every failure is surfaced, never swallowed: an unreadable answer is logged
 * at error and filed as `frame_undecodable` (the click guard's treatment), any
 * other throw is logged at error with its cause, and in every case the notice
 * is drawn and the unreachable record written.
 */
async function jump_(click: JumpClick, deps: ExpandedDeps): Promise<void> {
  if (click.target.case === "unresolved") {
    notice(click, deps, click.resolution);
    return;
  }
  const entry = click.target.value;
  let reached = false;
  let failed = false;
  try {
    reached = await deps.selectDetachedWork(entry);
  } catch (err) {
    failed = true;
    if (isMalformedView(err)) {
      log.error(`the daemon's answer could not be read: ${err.message}`, {
        operation: "footer.expanded.jump-undecodable",
        context: { work_id: click.work, kind: click.panel, feed_id: entry.value, path: err.path, cause: err.detail },
      });
      deps.ctx.failures.report(frameUndecodable(err.detail, err.path));
    } else {
      log.error(`selecting a footer row's entry failed: ${String(err)}`, {
        operation: "footer.expanded.jump-failed",
        context: { work_id: click.work, kind: click.panel, feed_id: entry.value, cause: String(err) },
      });
    }
  }
  if (reached) {
    log.debug("a footer detached-work row's entry was selected", {
      operation: "footer.expanded.jump-selected",
      context: { work_id: click.work, kind: click.panel, feed_id: entry.value },
    });
    deps.notices.clear(click.key);
    deps.redraw();
    return;
  }
  notice(click, deps, failed ? "entry_selection_failed" : "entry_not_revealed");
}

/**
 * Raise the row's notice, redraw so it is on the glass NOW, and write the
 * record a remediation reads: which work, which kind, which entry (if the
 * daemon knew one), and why it is not on screen.
 */
function notice(click: JumpClick, deps: ExpandedDeps, reason: string): void {
  deps.notices.raise(click.key, click.resolution);
  const record = {
    operation: "footer.expanded.jump-unreachable",
    context: {
      work_id: click.work,
      kind: click.panel,
      feed_id: click.target.case === "entry" ? click.target.value.value : "unresolved",
      jump: click.resolution,
      reason,
    },
  };
  // Every kind draws an entry now (a monitor's is its call's tool-call card),
  // so every miss — an entry the daemon has not drawn, or one it named that
  // the page could not show — is WARN.
  log.warn("a footer detached-work row's entry is not on screen", record);
  deps.redraw();
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
  return liveElapsedClock(deps.ctx.ticker, "footer-row-clock", msOf(startedAtMs, path));
}

/** The jump affordance's caret. */
function caret(): HTMLElement {
  const el = document.createElement("span");
  el.className = "footer-row-caret";
  el.setAttribute("aria-hidden", "true");
  el.textContent = "▸";
  return el;
}
