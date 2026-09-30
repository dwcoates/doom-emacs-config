/**
 * strip — the always-drawn footer row: | status | substatus | activity | clock
 * | tokens | chips |.
 *
 * THE THREE STATUS CELLS ARE ONE TREE, not three siblings. `FooterStatus` sets
 * exactly one arm, that arm declares the substatus steps and activity kinds
 * legal beneath it, and this module walks that tree — which is why there is no
 * pairing to validate here: an illegal pairing is unrepresentable on the wire.
 *
 * THE WORD IS THE ARM NAME, LOWERCASE, WITH SPACES. footer.proto states this
 * rendering rule once, for status and substatus alike: the arm's schema
 * spelling is not the label, so `start_failed` draws as `start failed` and
 * never as `start_failed`. That derivation is the CONTRACT's, not a phase→word
 * table of this client's invention — the daemon ships no word for these two
 * cells, only the arm.
 *
 * THE SUBSTATUS CELL MERGES INTO THE STATUS CELL when the status arm has no
 * substatus oneof (background) or leaves it unset, and the ACTIVITY CELL always
 * absorbs the strip's free width. Both are footer.proto's rules, stated there
 * once and implemented here once.
 *
 * THE ACTIVITY CELL IS ITS OWN MODULE (`activity.ts`): the tiered line — the
 * salient line, or the transient over the enduring line — and every kind's
 * drawing live there. This module hands it the arm's cell.
 *
 * COLOUR ON TYPED DATUMS. A statically typed datum inside a cell — a queue
 * position here, and the activity line's own figures there — is drawn as its
 * own coloured span (`tones.ts`), because those are the figures a reader tracks
 * across pushes and the composed prose around them is not.
 *
 * CLOCKS TICK HERE, NOT ON THE WIRE. The turn clock counts up from an instant
 * off the shared ticker, and is never pushed.
 */
import type {
  FooterChipAgents,
  FooterChipCrons,
  FooterChipMergeTests,
  FooterChipMonitors,
  FooterChipShells,
  FooterChipTasks,
  FooterClock,
  FooterLiveWorkChips,
  FooterStatus,
  FooterStatusBackground,
  FooterStatusBlocked,
  FooterStatusClosing,
  FooterStatusDisconnected,
  FooterStatusIdle,
  FooterStatusInterrupted,
  FooterStatusLoading,
  FooterStatusMergeFailed,
  FooterStatusTurnFailed,
  FooterStatusDegraded,
  FooterStatusMerged,
  FooterStatusMerging,
  FooterStatusWorking,
  FooterStatusWaiting,
  FooterStrip,
  FooterTokensCell,
  FooterTokensCellInput,
  FooterTokensCellVerdict,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import {
  STATUS_WAVE_ATTRIBUTE,
  STATUS_WAVE_LETTER_CLASS,
  STATUS_WAVE_PROGRESS,
  STATUS_WAVE_WORD_CLASS,
  statusWaveStyle,
  statusWordWaves,
} from "../breathing.js";
import { formatTickedElapsed } from "../duration.js";
import { tick } from "../feed/ticking.js";
import { log } from "../log.js";
import { tokenHeatColor } from "../token-heat.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { drawFooterStatusActivity, type ActivityDeps, type FooterActivity } from "./activity.js";
import type { FooterPanel } from "./expanded.js";
import { grabber, statusWords, textLine } from "./parts.js";
import { activityDatumClass, statusArmClass, type ActivityDatum } from "./tones.js";
import type { StopControls } from "./stop.js";

/** The label the clock cell shows between turns. The baseline strip's own. */
export const IDLE_CLOCK_LABEL = "--";

/**
 * What the strip needs beyond the view: the context and the expiry timer its
 * activity cell draws with (`ActivityDeps`), and the panel selection.
 */
export interface StripDeps extends ActivityDeps {
  /** Which panel is open right now, or null. Webview-local, never on the wire. */
  selection: FooterPanel | null;
  /** Open a panel, or close the open one. The footer owns the state. */
  onSelect(panel: FooterPanel): void;
  /**
   * The footer's stop controls, built once per mount.
   *
   * HANDED IN RATHER THAN BUILT HERE, because a stop's own answer is drawn at
   * the control and the footer redraws whole on every push -- see
   * `StopControls`. A control rebuilt per draw loses the answer it was just
   * given.
   */
  readonly stops: StopControls;
}

/**
 * The strip's cells, in order.
 *
 * The status family draws one or two cells depending on the merge rule, so the
 * status walk returns its cells as a list rather than the row assuming two.
 */
export function drawFooterStrip(u: FooterStrip, deps: StripDeps): HTMLElement {
  const path = "FooterStrip";
  log.debug("drawing the footer strip", { operation: "footer.strip", context: {} });

  const row = document.createElement("div");
  row.className = "pfooter-cells footer-strip";

  for (const cell of drawFooterStatus(requireMessage(u.status, `${path}.status`), deps)) {
    row.appendChild(cell);
  }
  row.appendChild(drawFooterClock(requireMessage(u.clock, `${path}.clock`), deps));
  row.appendChild(drawFooterTokensCell(requireMessage(u.tokens, `${path}.tokens`), deps));
  row.appendChild(
    drawFooterLiveWorkChips(requireMessage(u.liveWork, `${path}.live_work`), deps),
  );
  return row;
}

/**
 * THE ONE STRIP THIS CLIENT COMPOSES ITSELF — the footer under a link the
 * webapp knows is down (owner ruling, 2026-09-13).
 *
 * Everywhere else the daemon composes and this module draws, and that rule
 * cannot cover this case: the strip that would say the link is down is a strip
 * that would have to arrive over it. So the three status cells are built HERE,
 * out of the client's own verdict (`src/rpc/link.ts`), in the same classes and
 * the same order the pushed strip uses — the reader sees the footer they
 * already know, saying `disconnected`, and not a second widget.
 *
 * ONLY THE THREE STATUS CELLS ARE THE CLIENT'S. The clock, the tokens and the
 * chips are still drawn from the daemon's LAST pushed strip when there is one,
 * because a link that just died does not make the last figures untrue -- it
 * makes them the last ones -- and blanking half the dock would say more than
 * the client knows. With no push yet (LAST is null) the row is the three cells
 * alone.
 */
export function drawClientDisconnectedStrip(
  substatus: string,
  activity: string,
  last: { strip: FooterStrip; deps: StripDeps } | null = null,
): HTMLElement {
  log.debug("drawing the client's own disconnected strip", {
    operation: "footer.strip.client-verdict",
    context: { substatus },
  });
  const row = document.createElement("div");
  row.className = "pfooter-cells footer-strip";

  const word = document.createElement("div");
  word.className = `pfooter-cell pfooter-phase footer-status arm-disconnected ${statusArmClass("disconnected")}`;
  word.setAttribute("data-arm", "disconnected");
  word.textContent = statusWords("disconnected");
  row.appendChild(word);

  const step = document.createElement("div");
  step.className = "pfooter-cell footer-substatus";
  step.textContent = substatus;
  row.appendChild(step);

  const cell = document.createElement("div");
  cell.className = "pfooter-cell pfooter-grow footer-activity";
  cell.appendChild(textLine("footer-activity-client-verdict", activity));
  cell.title = activity;
  cell.appendChild(grabber());
  row.appendChild(cell);

  if (last !== null) {
    const path = "FooterStrip";
    row.appendChild(
      drawFooterClock(requireMessage(last.strip.clock, `${path}.clock`), last.deps),
    );
    row.appendChild(
      drawFooterTokensCell(requireMessage(last.strip.tokens, `${path}.tokens`), last.deps),
    );
    row.appendChild(
      drawFooterLiveWorkChips(requireMessage(last.strip.liveWork, `${path}.live_work`), last.deps),
    );
  }
  return row;
}

// ---- the status family ----------------------------------------------------

/** Every substatus oneof in the contract, as one type to walk. */
type SubStatusOneof =
  | FooterStatusIdle["substatus"]
  | FooterStatusWorking["substatus"]
  | FooterStatusWaiting["substatus"]
  | FooterStatusInterrupted["substatus"]
  | FooterStatusMerging["substatus"]
  | FooterStatusBlocked["substatus"]
  | FooterStatusDisconnected["substatus"]
  | FooterStatusClosing["substatus"]
  | FooterStatusLoading["substatus"]
  | FooterStatusMergeFailed["substatus"]
  | FooterStatusDegraded["substatus"];

/**
 * The activity cell a status carries.
 *
 * ALWAYS SET on every arm (footer.proto: the enduring tier guarantees the cell
 * has something to draw under every status), so an unset one is malformed.
 * The sheet and the compaction publisher read the SAME cell the strip draws,
 * and this is the one walk that knows which arm parks it where.
 */
export function footerStatusActivity(u: FooterStatus): FooterActivity {
  const path = "FooterStatus.status";
  const status = requireCase(u.status, path);
  return requireMessage(statusParts(status, path).activity, `${path}.${status.case}.activity`);
}

/** One status arm's three cells, as the arm carries them. */
export interface StatusParts {
  substatus: SubStatusOneof | undefined;
  /** Required by the schema; `undefined` only on a malformed push. */
  activity: FooterActivity | undefined;
  /** Set when the arm declares no substatus oneof at all (background). */
  substatusless: boolean;
  /**
   * Set when the contract says the substatus is ALWAYS set (merge failed's
   * area), so an unset one is malformed rather than a merged cell.
   */
  substatusRequired: boolean;
}

/**
 * The status, its step and its activity line, as the one or two cells the
 * merge rule leaves.
 *
 * The arm switch is exhaustive over `FooterStatus.status`; each case says only
 * WHAT that arm carries, and the drawing beneath is shared, because the three
 * cells are drawn identically whichever arm produced them.
 */
export function drawFooterStatus(u: FooterStatus, deps: StripDeps): HTMLElement[] {
  const path = "FooterStatus.status";
  const status = requireCase(u.status, path);
  const parts = statusParts(status, path);
  const activityPath = `${path}.${status.case}.activity`;
  const activity = requireMessage(parts.activity, activityPath);
  log.debug(`drawing the footer status: ${status.case}`, {
    operation: "footer.strip.status",
    context: { status: status.case, substatus: parts.substatus?.case },
  });

  const word = document.createElement("div");
  word.className = `pfooter-cell pfooter-phase footer-status arm-${status.case} ${statusArmClass(status.case)}`;
  word.setAttribute("data-arm", status.case);
  drawStatusWord(word, status.case);

  const cells: HTMLElement[] = [word];
  if (parts.substatusRequired) requireCase(parts.substatus ?? {}, `${path}.${status.case}.substatus`);
  const sub =
    parts.substatus === undefined || parts.substatus.case === undefined
      ? null
      : drawFooterSubStatus(parts.substatus, status.case);
  if (sub === null) {
    // THE MERGE RULE: with nothing finer to say, the status cell spans the
    // substatus cell rather than leaving an empty segment in the row.
    word.classList.add("footer-status-merged");
    word.setAttribute("data-merged", "true");
  } else {
    cells.push(sub);
  }

  cells.push(drawFooterStatusActivity(activity, deps, activityPath));
  return cells;
}

/**
 * The status word, split into per-letter spans WHEN THE STATUS MEANS PROGRESS.
 *
 * Owner's idea, 2026-09-14: a status that is getting somewhere says so in the
 * word, with a bulge travelling along the letters and back. The phase comes from
 * the page-global `statusWave` (breathing.ts), emitted inline per letter, so a
 * push that rewrites this whole subtree mid-wave continues the wave instead of
 * snapping it back to the word's head.
 *
 * THAT ONE INLINE DELAY ALSO DRIVES THE COLOUR SWEEP. The letter carries a
 * second CSS animation (`pfooter-status-color`) that sweeps its own `color`
 * through the FULL SPECTRUM — a rainbow hue cycle from the arm tone and back;
 * a single `animation-delay` value applies to every name in the shorthand, so
 * the colour sweep rides the very same phase, epoch and period as the scale
 * bulge with no second clock. The colour is done PER LETTER on the letter's own
 * `color` — never a `background-clip: text` gradient on the holder, which
 * blanked the word once because the transparent text-fill inherited into these
 * split letters. Every keyframe stop is opaque and the rest stop is the arm tone,
 * so a stopped letter is always legible and never transparent.
 *
 * A NON-PROGRESS STATUS IS PLAIN TEXT. Waiting, blocked, disconnected, idle,
 * interrupted and background stand still, and they carry no spans at all rather
 * than spans with a stopped animation: nothing should have to look at a class to
 * know whether the word is moving.
 *
 * THE LETTERS GO INSIDE ONE SPAN because `.pfooter-cell` is a flex container
 * with a `gap`: eight bare letter spans would become eight flex items and the
 * word would come apart. The word span is the single flex item; the letters are
 * inline-block inside it, which is also what lets a transform apply to them.
 *
 * The index advances on SPACES too, so the bulge crosses a two-word status at
 * the same speed it crosses the letters.
 */
function drawStatusWord(word: HTMLElement, armCase: string): void {
  const text = statusWords(armCase);
  if (!statusWordWaves(armCase)) {
    word.textContent = text;
    return;
  }
  word.setAttribute(STATUS_WAVE_ATTRIBUTE, STATUS_WAVE_PROGRESS);
  const holder = document.createElement("span");
  holder.className = STATUS_WAVE_WORD_CLASS;
  [...text].forEach((character, index) => {
    if (character === " ") {
      holder.appendChild(document.createTextNode(" "));
      return;
    }
    const letter = document.createElement("span");
    letter.className = STATUS_WAVE_LETTER_CLASS;
    letter.setAttribute("style", statusWaveStyle(index));
    letter.textContent = character;
    holder.appendChild(letter);
  });
  word.appendChild(holder);
}

/** What each status arm carries, and which of it is required. */
function statusParts(
  status: FooterStatus["status"] & { case: string },
  path: string,
): StatusParts {
  switch (status.case) {
    case "idle":
    case "working":
    case "interrupted":
    case "merging":
    case "blocked":
    case "disconnected":
    case "closing":
    case "degraded":
    case "waiting":
    case "loading":
      return {
        substatus: status.value.substatus,
        activity: status.value.activity,
        substatusless: false,
        substatusRequired: false,
      };
    case "background": {
      // An arm with NO substatus oneof: the chips and panels carry the
      // detail, so the substatus cell merges into the status cell always.
      const background: FooterStatusBackground = status.value;
      return {
        substatus: undefined,
        activity: background.activity,
        substatusless: true,
        substatusRequired: false,
      };
    }
    case "mergeFailed": {
      // WHERE THE MERGE FAILED is the substatus, and footer.proto says exactly
      // one area is set: an unset one is a malformed push, never a merged
      // cell that would hide the area from the reader.
      const failed: FooterStatusMergeFailed = status.value;
      return {
        substatus: failed.substatus,
        activity: failed.activity,
        substatusless: false,
        substatusRequired: true,
      };
    }
    case "merged": {
      // A LANDED merge declares no substatus oneof: the merge bubble carries
      // the account, so the cell merges the same way.
      const settled: FooterStatusMerged = status.value;
      return {
        substatus: undefined,
        activity: settled.activity,
        substatusless: true,
        substatusRequired: false,
      };
    }
    case "turnFailed": {
      // A failed turn declares no substatus oneof: the feed's turn-end row
      // carries the account, so the cell merges the same way.
      const failed: FooterStatusTurnFailed = status.value;
      return {
        substatus: undefined,
        activity: failed.activity,
        substatusless: true,
        substatusRequired: false,
      };
    }
    default: {
      const other: { case: string } = status;
      return unreachableArm(path, other.case);
    }
  }
}

/**
 * The substatus cell.
 *
 * THREE ARMS CARRY A FIGURE and the rest are empty messages whose NAME is the
 * whole fact: a merge's queue place ("enqueued 2/5"), its replay progress
 * ("rebasing 3/7") and its fixing attempt ("fixing attempt 2/3"). The default
 * branch is therefore not a fallback: drawing an empty arm's name is exactly
 * what footer.proto's rendering rule prescribes, so an arm this build has
 * never heard of still draws correctly rather than refusing a frame over a
 * step it can already spell.
 */
export function drawFooterSubStatus(
  sub: SubStatusOneof & { case: string },
  statusCase: string,
): HTMLElement {
  const cell = document.createElement("div");
  cell.className = "pfooter-cell footer-substatus";
  cell.setAttribute("data-arm", sub.case);
  cell.appendChild(document.createTextNode(subStatusWords(statusCase, sub.case)));

  switch (sub.case) {
    case "enqueued":
      // The merge's 1-based place among the merges waiting in its repository.
      cell.appendChild(document.createTextNode(" "));
      cell.appendChild(substatusFigure("position", `${sub.value.place}/${sub.value.waiting}`));
      return cell;
    case "rebasing":
      // How many of the branch's commits have been replayed onto the tip.
      cell.appendChild(document.createTextNode(" "));
      cell.appendChild(substatusFigure("count", `${sub.value.replayed}/${sub.value.total}`));
      return cell;
    case "fixing":
      // This attempt among the most the merge makes before it fails.
      cell.appendChild(document.createTextNode(" attempt "));
      cell.appendChild(substatusFigure("attempt", `${sub.value.attempt}/${sub.value.maxAttempts}`));
      return cell;
    default:
      return cell;
  }
}

/** A substatus figure, coloured as the typed datum it is. */
function substatusFigure(datum: ActivityDatum, text: string): HTMLElement {
  const figure = document.createElement("span");
  figure.className = `footer-substatus-place ${activityDatumClass(datum)}`;
  figure.setAttribute("data-datum", datum);
  figure.textContent = text;
  return figure;
}


// ---- clock, tokens, chips -------------------------------------------------

/**
 * The turn clock, and the stop control that only exists while a turn does.
 *
 * The clock's instant IS the liveness fact this cell has: set means a turn is
 * running, so the stop is mounted; unset means the idle dash and no stop, since
 * a control that could only answer "nothing running" is chrome.
 */
export function drawFooterClock(u: FooterClock, deps: StripDeps): HTMLElement {
  const cell = document.createElement("div");
  cell.className = "pfooter-cell pfooter-clock footer-clock";

  const label = document.createElement("span");
  label.className = "info-time";
  cell.appendChild(label);

  if (u.turnStartedAtMs === undefined) {
    label.textContent = IDLE_CLOCK_LABEL;
    cell.setAttribute("data-live", "false");
    return cell;
  }
  cell.setAttribute("data-live", "true");
  const startedAtMs = msOf(u.turnStartedAtMs, "FooterClock.turn_started_at_ms");
  tick(label, deps.ctx.ticker, (nowMs) => {
    label.textContent = formatTickedElapsed(nowMs - startedAtMs);
  });
  cell.appendChild(deps.stops.turn);
  return cell;
}

/**
 * The tokens cell: one figure and two glyphs, and the click that opens the
 * breakdown panel.
 *
 * The figure is daemon-formatted and drawn verbatim — no arithmetic, no unit
 * rounding — and both glyphs are PRESENCE facts: the alarm's evidence and the
 * verdict's evidence are the panel's lines, never a second copy here.
 */
export function drawFooterTokensCell(u: FooterTokensCell, deps: StripDeps): HTMLElement {
  const path = "FooterTokensCell";
  const cell = document.createElement("div");
  cell.className = "pfooter-cell pfooter-tokens footer-tokens";
  cell.setAttribute("role", "button");
  cell.tabIndex = 0;
  cell.title = "the turn's token breakdown";
  if (deps.selection === "tokens") cell.setAttribute("data-selected", "true");
  cell.appendChild(drawFooterTokensCellInput(requireMessage(u.input, `${path}.input`)));

  if (u.alarm !== undefined) {
    const alarm = document.createElement("span");
    alarm.className = "footer-tokens-alarm";
    alarm.setAttribute("data-alarm", "");
    alarm.title = "this turn crossed the cost threshold";
    alarm.textContent = "⚠";
    cell.appendChild(alarm);
  }
  if (u.verdict !== undefined) {
    cell.appendChild(drawFooterTokensCellVerdict(u.verdict, `${path}.verdict`));
  }
  cell.addEventListener("click", (event: MouseEvent) => {
    event.preventDefault();
    deps.onSelect("tokens");
  });
  return cell;
}

/** The cell's one figure, exactly as the daemon formatted it, in its heat's color. */
export function drawFooterTokensCellInput(u: FooterTokensCellInput): HTMLElement {
  const input = document.createElement("span");
  input.className = "footer-tokens-input";
  input.textContent = u.text;
  if (u.heat !== undefined) {
    const position = u.heat.position;
    input.setAttribute("data-heat", String(position));
    input.style.color = tokenHeatColor(position, "FooterTokensCellInput.heat.position");
  }
  return input;
}

/**
 * The accounting badge. THE ARM PICKS THE GLYPH, and the two failing arms are
 * given DISTINCT titles: a floor and a contradiction are different problems and
 * one shared "✗" hover would collapse them.
 */
export function drawFooterTokensCellVerdict(u: FooterTokensCellVerdict, path: string): HTMLElement {
  const verdict = requireCase(u.verdict, `${path}.verdict`);
  const badge = document.createElement("span");
  badge.className = `footer-tokens-verdict arm-${verdict.case}`;
  badge.setAttribute("data-verdict", verdict.case);
  switch (verdict.case) {
    case "complete":
      badge.textContent = "✓";
      badge.title = "every response's usage reconciled";
      return badge;
    case "incomplete":
      badge.textContent = "✗";
      badge.title = "some responses carried no usage — these figures are a floor";
      return badge;
    case "invalid":
      badge.textContent = "✗";
      badge.title = "the observed usage contradicts itself";
      return badge;
    default: {
      const other: { case: string } = verdict;
      return unreachableArm(`${path}.verdict`, other.case);
    }
  }
}

/**
 * The live-work chips, right-aligned.
 *
 * AN UNSET CHIP IS NOT DRAWN — nothing of that kind is live — so a quiet
 * workspace shows no chips at all, and the counts are the daemon's: this
 * client never counts panel rows to label a chip even though the rows arrive
 * in the same push.
 */
export function drawFooterLiveWorkChips(u: FooterLiveWorkChips, deps: StripDeps): HTMLElement {
  const cell = document.createElement("div");
  cell.className = "pfooter-cell pfooter-counters footer-chips";
  if (u.agents !== undefined) cell.appendChild(drawFooterChipAgents(u.agents, deps));
  if (u.tasks !== undefined) cell.appendChild(drawFooterChipTasks(u.tasks, deps));
  if (u.shells !== undefined) cell.appendChild(drawFooterChipShells(u.shells, deps));
  if (u.monitors !== undefined) cell.appendChild(drawFooterChipMonitors(u.monitors, deps));
  if (u.crons !== undefined) cell.appendChild(drawFooterChipCrons(u.crons, deps));
  if (u.mergeTests !== undefined) cell.appendChild(drawFooterChipMergeTests(u.mergeTests, deps));
  return cell;
}

/**
 * ⚙ N — the agent-spawned subagents with work ahead of them, and, while any
 * of them waits for the API, the waiting glyph with ITS count beside it:
 * "⚙ 3 ⧗ 1". Both counts are the daemon's (the chip's `count` already includes
 * the waiting rows), so an outage stalling background work stays on the strip
 * without opening the panel.
 */
export function drawFooterChipAgents(u: FooterChipAgents, deps: StripDeps): HTMLElement {
  const el = chip("agents", "⚙", String(u.count), deps);
  if (u.waitingForApi === undefined) return el;
  const waiting = document.createElement("span");
  waiting.className = "footer-chip-waiting";
  waiting.setAttribute("data-waiting-for-api", String(u.waitingForApi.count));
  waiting.title = `${u.waitingForApi.count} waiting for the API`;
  waiting.appendChild(document.createTextNode(" "));
  waiting.appendChild(chipGlyph("waitingForApi", WAITING_FOR_API_GLYPH));
  waiting.appendChild(document.createTextNode(` ${u.waitingForApi.count}`));
  el.appendChild(waiting);
  return el;
}

/**
 * The waiting-for-the-API glyph: the geometric hourglass, never the emoji,
 * as the chips' other glyphs are geometric.
 */
export const WAITING_FOR_API_GLYPH = "⧗";

/** ☑ done/total — the task tracker's progress. */
export function drawFooterChipTasks(u: FooterChipTasks, deps: StripDeps): HTMLElement {
  return chip("tasks", "☑", `${u.done}/${u.total}`, deps);
}

/** $ N — the live detached shells. */
export function drawFooterChipShells(u: FooterChipShells, deps: StripDeps): HTMLElement {
  return chip("shells", "$", String(u.count), deps);
}

/**
 * ◉ N — the live background monitors. The schema's comment names an eye; the
 * glyph is the geometric ring, never the emoji.
 */
export function drawFooterChipMonitors(u: FooterChipMonitors, deps: StripDeps): HTMLElement {
  return chip("monitors", "◉", String(u.count), deps);
}

/**
 * ◷ N — the scheduled jobs. The schema's comment names a stopwatch; the glyph
 * is the geometric clock face, never the emoji.
 */
export function drawFooterChipCrons(u: FooterChipCrons, deps: StripDeps): HTMLElement {
  return chip("crons", "◷", String(u.count), deps);
}

/**
 * 🧪 finished/total — the merge's test gate, while it runs. Both figures are
 * the daemon's; the panel's rows are never counted for them.
 */
export function drawFooterChipMergeTests(u: FooterChipMergeTests, deps: StripDeps): HTMLElement {
  return chip("mergeTests", "🧪", `${u.finished}/${u.total}`, deps);
}

/** One chip: glyph, count, and the click that opens its panel. */
function chip(panel: FooterPanel, glyph: string, count: string, deps: StripDeps): HTMLElement {
  const el = document.createElement("span");
  el.className = "footer-chip";
  el.setAttribute("data-chip", panel);
  el.setAttribute("role", "button");
  el.tabIndex = 0;
  if (deps.selection === panel) {
    el.setAttribute("data-selected", "true");
    el.classList.add("footer-chip-selected");
  }
  el.appendChild(chipGlyph(panel, glyph));
  el.appendChild(document.createTextNode(` ${count}`));
  el.addEventListener("click", (event: MouseEvent) => {
    event.preventDefault();
    deps.onSelect(panel);
  });
  return el;
}

/**
 * One chip glyph: the character, named by `data-glyph` and hidden from
 * assistive tech (the count beside it is what is read). The chip's own glyph
 * and the agents chip's waiting glyph are both drawn by it.
 */
export function chipGlyph(name: string, character: string): HTMLElement {
  const mark = document.createElement("span");
  mark.className = "footer-chip-glyph";
  mark.setAttribute("data-glyph", name);
  mark.setAttribute("aria-hidden", "true");
  mark.textContent = character;
  return mark;
}

// ---- shared helpers -------------------------------------------------------
/**
 * The substatus word, with the TWO arms whose bare name would lie or say
 * nothing.
 *
 * `FooterStatusClosing`'s step is spelled `blocked`, which is also a whole
 * STATUS in this same strip — a close that cannot proceed would read exactly
 * like a session the vendor has blocked. The message's own name
 * (`FooterSubStatusCloseBlocked`) is what the schema calls it, so the cell
 * draws that. `FooterStatusMergeFailed`'s catch-all area is spelled `other`,
 * and footer.proto draws it "merge". Both are declared exceptions, not a
 * per-arm label table: every other arm's name is already its label.
 */
export function subStatusWords(statusCase: string, subCase: string): string {
  if (statusCase === "closing" && subCase === "blocked") return "close blocked";
  if (statusCase === "mergeFailed" && subCase === "other") return "merge";
  return statusWords(subCase);
}
