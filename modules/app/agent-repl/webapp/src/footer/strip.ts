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
 * COLOUR ON TYPED DATUMS. A statically typed datum inside an activity — a sha,
 * a retry attempt, a queue position, an allowance percentage — is drawn as its
 * own coloured span (`tones.ts`), because those are the figures a reader tracks
 * across pushes and the composed prose around them is not.
 *
 * CLOCKS TICK HERE, NOT ON THE WIRE. The turn clock counts up from an instant,
 * a wakeup and a rate-limit reset count down to theirs, and every activity
 * carries a relative age — all four animate off the shared ticker, and none of
 * them is ever pushed.
 */
import type {
  FooterAllowance,
  FooterChipAgents,
  FooterChipCrons,
  FooterChipMonitors,
  FooterChipShells,
  FooterChipTasks,
  FooterClock,
  FooterLiveWorkChips,
  FooterStatus,
  FooterStatusActivityAt,
  FooterStatusActivityAuthenticating,
  FooterStatusActivityBlockedOnUser,
  FooterStatusActivityCloseBlocked,
  FooterStatusActivityColdGateCost,
  FooterStatusActivityContextBudget,
  FooterStatusActivityContextInjected,
  FooterStatusActivityGatedCall,
  FooterStatusActivityHook,
  FooterStatusActivityInterrupting,
  FooterStatusActivityMergingCommit,
  FooterStatusActivityNotification,
  FooterStatusActivityQueryDied,
  FooterStatusActivityQuestionLead,
  FooterAllowanceSample,
  FooterStatusActivityRateLimited,
  FooterStatusActivityRetrying,
  FooterStatusActivityWakeup,
  FooterStatusBackground,
  FooterStatusBackgroundActivity,
  FooterStatusBlocked,
  FooterStatusBlockedActivity,
  FooterStatusClosing,
  FooterStatusClosingActivity,
  FooterStatusDisconnected,
  FooterStatusDisconnectedActivity,
  FooterStatusIdle,
  FooterStatusIdleActivity,
  FooterStatusInterrupted,
  FooterStatusInterruptedActivity,
  FooterStatusLoading,
  FooterStatusLoadingActivity,
  FooterStatusMerging,
  FooterStatusMergingActivity,
  FooterStatusThinking,
  FooterStatusThinkingActivity,
  FooterStatusWaiting,
  FooterStatusWaitingActivity,
  FooterStrip,
  FooterTokensCell,
  FooterTokensCellInput,
  FooterTokensCellVerdict,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { formatAge, formatCountdown, formatTickedAge, formatTickedElapsed } from "../duration.js";
import { tick } from "../feed/ticking.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { protoArmName } from "../vocab.js";
import type { FooterPanel } from "./expanded.js";
import { activityDatumClass, allowanceStatusClass, statusArmClass } from "./tones.js";
import { drawTurnStopControl } from "./stop.js";

/** The label the clock cell shows between turns. The baseline strip's own. */
export const IDLE_CLOCK_LABEL = "--";

/** What the strip needs beyond the view: the context, and the panel selection. */
export interface StripDeps {
  ctx: AppContext;
  /** Which panel is open right now, or null. Webview-local, never on the wire. */
  selection: FooterPanel | null;
  /** Open a panel, or close the open one. The footer owns the state. */
  onSelect(panel: FooterPanel): void;
}

/**
 * The strip's cells, in order.
 *
 * The status family draws one or two cells depending on the merge rule, so the
 * status walk returns its cells as a list rather than the row assuming two.
 */
export function drawFooterStrip(u: FooterStrip, deps: StripDeps): HTMLElement {
  const path = "FooterStrip";
  log("debug", "drawing the footer strip", { operation: "footer.strip", context: {} });

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

// ---- the status family ----------------------------------------------------

/** Every substatus oneof in the contract, as one type to walk. */
type SubStatusOneof =
  | FooterStatusIdle["substatus"]
  | FooterStatusThinking["substatus"]
  | FooterStatusWaiting["substatus"]
  | FooterStatusInterrupted["substatus"]
  | FooterStatusMerging["substatus"]
  | FooterStatusBlocked["substatus"]
  | FooterStatusDisconnected["substatus"]
  | FooterStatusClosing["substatus"]
  | FooterStatusLoading["substatus"];

/** Every per-status activity message, as one type to walk. */
export type FooterActivity =
  | FooterStatusIdleActivity
  | FooterStatusThinkingActivity
  | FooterStatusWaitingActivity
  | FooterStatusInterruptedActivity
  | FooterStatusMergingActivity
  | FooterStatusBackgroundActivity
  | FooterStatusBlockedActivity
  | FooterStatusDisconnectedActivity
  | FooterStatusClosingActivity
  | FooterStatusLoadingActivity;

/** One status arm's three cells, already resolved. */
export interface StatusParts {
  substatus: SubStatusOneof | undefined;
  activity: FooterActivity | undefined;
  /** Set when the arm REQUIRES an activity: an unset one is then malformed. */
  activityRequired: boolean;
  /** Set when the arm declares no substatus oneof at all (background). */
  substatusless: boolean;
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
  log("debug", `drawing the footer status: ${status.case}`, {
    operation: "footer.strip.status",
    context: {
      status: status.case,
      substatus: parts.substatus?.case,
      activity: parts.activity === undefined ? undefined : parts.activity.kind.case,
    },
  });

  const word = document.createElement("div");
  word.className = `pfooter-cell pfooter-phase footer-status arm-${status.case} ${statusArmClass(status.case)}`;
  word.setAttribute("data-arm", status.case);
  word.textContent = statusWords(status.case);

  const cells: HTMLElement[] = [word];
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

  cells.push(drawFooterStatusActivity(parts, deps, status.case));
  return cells;
}

/** What each status arm carries, and which of it is required. */
function statusParts(
  status: FooterStatus["status"] & { case: string },
  path: string,
): StatusParts {
  switch (status.case) {
    case "idle":
    case "thinking":
    case "interrupted":
    case "merging":
    case "blocked":
    case "disconnected":
    case "closing":
      return {
        substatus: status.value.substatus,
        activity: status.value.activity,
        activityRequired: false,
        substatusless: false,
      };
    case "waiting":
    case "loading":
      // REQUIRED by the schema: a waiting state always has a composable line
      // (the countdown, the gated call, the question lead) and a loading state
      // always knows the item it is injecting.
      return {
        substatus: status.value.substatus,
        activity: status.value.activity,
        activityRequired: true,
        substatusless: false,
      };
    case "background": {
      // The one arm with NO substatus oneof: the chips and panels carry the
      // detail, so the substatus cell merges into the status cell always.
      const background: FooterStatusBackground = status.value;
      return {
        substatus: undefined,
        activity: background.activity,
        activityRequired: false,
        substatusless: true,
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
 * TWO ARMS CARRY A PAYLOAD and the rest are empty messages whose NAME is the
 * whole fact. The default branch is therefore not a fallback: drawing an empty
 * arm's name is exactly what footer.proto's rendering rule prescribes, so an
 * arm this build has never heard of still draws correctly rather than
 * refusing a frame over a step it can already spell.
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
    case "queued": {
      // The queue place: 1-based position within its repo's depth.
      const place = document.createElement("span");
      place.className = `footer-substatus-place ${activityDatumClass("position")}`;
      place.setAttribute("data-datum", "position");
      place.textContent = `${sub.value.position}/${sub.value.depth}`;
      cell.appendChild(document.createTextNode(" "));
      cell.appendChild(place);
      return cell;
    }
    case "parked": {
      // The daemon's composed standing line, drawn verbatim.
      const line = document.createElement("span");
      line.className = "footer-substatus-line";
      line.textContent = sub.value.line;
      cell.appendChild(document.createTextNode(" "));
      cell.appendChild(line);
      return cell;
    }
    default:
      return cell;
  }
}

/**
 * The activity cell — the strip's one elastic segment.
 *
 * An arm that REQUIRES an activity and did not ship one is a producer bug and
 * refuses the frame; an arm where absence is legitimate simply draws an empty
 * cell, which still owns the row's slack so the strip's geometry never shifts.
 */
export function drawFooterStatusActivity(
  parts: StatusParts,
  deps: StripDeps,
  statusCase: string,
): HTMLElement {
  const path = `FooterStatus${statusCase}.activity`;
  const cell = document.createElement("div");

  if (parts.activityRequired) requireMessage(parts.activity, path);
  const activity = parts.activity;
  if (activity === undefined) {
    // NO LINE, NO ACTIVITY CELL. `activity` is optional on most status arms and
    // absence means draw nothing, so the grow cell stays (it owns the strip's
    // slack, and the grabber notch lives in it) but wears no activity class:
    // there is no activity here for a reader or a query to find.
    cell.className = "pfooter-cell pfooter-grow footer-grabber";
    cell.appendChild(grabber());
    return cell;
  }

  cell.className = "pfooter-cell pfooter-grow footer-activity";
  const kind = requireCase(activity.kind, `${path}.kind`);
  cell.setAttribute("data-arm", kind.case);
  const line = drawActivityKind(kind, deps, `${path}.${kind.case}`);
  cell.appendChild(line);
  cell.appendChild(
    drawFooterStatusActivityAt(requireMessage(activity.at, `${path}.at`), deps, `${path}.at`),
  );
  cell.appendChild(grabber());
  // THE WHOLE LINE AS THE CELL'S HOVER. The cell is the strip's one elastic
  // segment and ellipsizes by design, so whatever it cannot fit is otherwise
  // reachable only by opening a sheet. The title is read off the DRAWN line
  // rather than recomposed, so it can never say something the cell does not,
  // and it rides the shared ticker so a countdown in it stays honest as it
  // counts. Registered AFTER the line's own clocks, so it reads their newest
  // text and not the previous second's.
  tick(cell, deps.ctx.ticker, () => {
    cell.title = line.textContent ?? "";
  });
  return cell;
}

/** The one standing line, by kind. */
function drawActivityKind(
  kind: FooterActivity["kind"] & { case: string },
  deps: StripDeps,
  path: string,
): HTMLElement {
  switch (kind.case) {
    case "notification":
      return drawFooterStatusActivityNotification(kind.value);
    case "contextBudget":
      return drawFooterStatusActivityContextBudget(kind.value);
    case "rateLimited":
      return drawFooterStatusActivityRateLimited(kind.value, deps, path);
    case "wakeup":
      return drawFooterStatusActivityWakeup(kind.value, deps, path);
    case "gatedCall":
      return drawFooterStatusActivityGatedCall(kind.value);
    case "questionLead":
      return drawFooterStatusActivityQuestionLead(kind.value);
    case "blockedOnUser":
      return drawFooterStatusActivityBlockedOnUser(kind.value);
    case "coldGateCost":
      return drawFooterStatusActivityColdGateCost(kind.value);
    case "interrupting":
      return drawFooterStatusActivityInterrupting(kind.value);
    case "hook":
      return drawFooterStatusActivityHook(kind.value);
    case "retrying":
      return drawFooterStatusActivityRetrying(kind.value);
    case "contextInjected":
      return drawFooterStatusActivityContextInjected(kind.value);
    case "mergingCommit":
      return drawFooterStatusActivityMergingCommit(kind.value);
    case "authenticating":
      return drawFooterStatusActivityAuthenticating(kind.value);
    case "queryDied":
      return drawFooterStatusActivityQueryDied(kind.value);
    case "closeBlocked":
      return drawFooterStatusActivityCloseBlocked(kind.value);
    default: {
      const other: { case: string } = kind;
      return unreachableArm(path, other.case);
    }
  }
}

/** The composed notification line, verbatim. */
export function drawFooterStatusActivityNotification(
  u: FooterStatusActivityNotification,
): HTMLElement {
  return textLine("footer-activity-notification", u.text);
}

/** The vendor's context-budget warning, verbatim. */
export function drawFooterStatusActivityContextBudget(
  u: FooterStatusActivityContextBudget,
): HTMLElement {
  return textLine("footer-activity-context-budget", u.text);
}

/** The gated call's composed line, verbatim. */
export function drawFooterStatusActivityGatedCall(u: FooterStatusActivityGatedCall): HTMLElement {
  return textLine("footer-activity-gated-call", u.text);
}

/** The open batch's composed lead, verbatim. */
export function drawFooterStatusActivityQuestionLead(
  u: FooterStatusActivityQuestionLead,
): HTMLElement {
  return textLine("footer-activity-question-lead", u.text);
}

/** The vendor's requires-action detail, verbatim. */
export function drawFooterStatusActivityBlockedOnUser(
  u: FooterStatusActivityBlockedOnUser,
): HTMLElement {
  return textLine("footer-activity-blocked-on-user", u.detail);
}

/** The cold gate's composed cost line, verbatim. */
export function drawFooterStatusActivityColdGateCost(
  u: FooterStatusActivityColdGateCost,
): HTMLElement {
  return textLine("footer-activity-cold-gate-cost", u.text);
}

/** The interrupting line, verbatim. */
export function drawFooterStatusActivityInterrupting(
  u: FooterStatusActivityInterrupting,
): HTMLElement {
  return textLine("footer-activity-interrupting", u.text);
}

/** The dead-query line, verbatim. */
export function drawFooterStatusActivityQueryDied(u: FooterStatusActivityQueryDied): HTMLElement {
  return textLine("footer-activity-query-died", u.text);
}

/** The close-blocked reasons, verbatim. */
export function drawFooterStatusActivityCloseBlocked(
  u: FooterStatusActivityCloseBlocked,
): HTMLElement {
  return textLine("footer-activity-close-blocked", u.text);
}

/** The injected item's composed line, verbatim. */
export function drawFooterStatusActivityContextInjected(
  u: FooterStatusActivityContextInjected,
): HTMLElement {
  return textLine("footer-activity-context-injected", u.text);
}

/** The auth prompt line, verbatim. */
export function drawFooterStatusActivityAuthenticating(
  u: FooterStatusActivityAuthenticating,
): HTMLElement {
  return textLine("footer-activity-authenticating", u.line);
}

/** The running hook's name. */
export function drawFooterStatusActivityHook(u: FooterStatusActivityHook): HTMLElement {
  return textLine("footer-activity-hook", u.name);
}

/**
 * The retry line: "retry #2 · rate limited", with the ATTEMPT coloured — the
 * one figure in the line that changes as the retries mount.
 */
export function drawFooterStatusActivityRetrying(u: FooterStatusActivityRetrying): HTMLElement {
  const line = document.createElement("span");
  line.className = "footer-activity-retrying";
  line.appendChild(document.createTextNode("retry "));
  const attempt = document.createElement("span");
  attempt.className = activityDatumClass("attempt");
  attempt.setAttribute("data-datum", "attempt");
  attempt.textContent = `#${u.attempt}`;
  line.appendChild(attempt);
  line.appendChild(document.createTextNode(` · ${u.status}`));
  return line;
}

/**
 * The commit a merge is landing: "4f2a1c: fold tokens into api", with the SHA
 * coloured as the identity it is.
 */
export function drawFooterStatusActivityMergingCommit(
  u: FooterStatusActivityMergingCommit,
): HTMLElement {
  const line = document.createElement("span");
  line.className = "footer-activity-merging-commit";
  const sha = document.createElement("span");
  sha.className = activityDatumClass("sha");
  sha.setAttribute("data-datum", "sha");
  sha.textContent = u.sha;
  line.appendChild(sha);
  line.appendChild(document.createTextNode(`: ${u.subject}`));
  return line;
}

/**
 * The wakeup countdown: "wakes in 4m 12s", plus the agent's reason when it gave
 * one. A FUTURE deadline is shipped and the remaining duration is drawn, so the
 * span ticks down through the shared clock.
 */
export function drawFooterStatusActivityWakeup(
  u: FooterStatusActivityWakeup,
  deps: StripDeps,
  path: string,
): HTMLElement {
  const line = document.createElement("span");
  line.className = "footer-activity-wakeup";
  const remaining = document.createElement("span");
  remaining.setAttribute("data-countdown", "");
  const wakeAtMs = msOf(u.wakeAtMs, `${path}.wake_at_ms`);
  tick(remaining, deps.ctx.ticker, (nowMs) => {
    remaining.textContent = `wakes in ${remainingLabel(wakeAtMs - nowMs)}`;
  });
  line.appendChild(remaining);
  if (u.reason !== undefined) {
    line.appendChild(document.createTextNode(` · ${u.reason.text}`));
  }
  return line;
}

/**
 * The rate-limit rung: BOTH allowances where both were read, because the
 * newsworthy percentage is meaningless without knowing which window it belongs
 * to — plus the sample outcome when the last read FAILED.
 *
 * AN ALLOWANCE THE PRODUCER LEFT UNSET IS DRAWN ABSENT, never required. The
 * contract says so in as many words ("an unset weekly is representable, and
 * stating a figure nobody reported would be worse than stating none"), and the
 * line is now reachable with NEITHER window figured — a session whose very
 * first usage sample failed has read nothing yet and still has an unread to
 * state.
 *
 * THE NEWSWORTHY WINDOW LEADS, AND THE UNREAD MARKER IS NOT PART OF WHAT THE
 * STRIP CUTS. The dock is one line capped at the response bubble's width, so
 * this line is routinely wider than the cell holding it; the cut used to fall
 * wherever the DOM order happened to put it, which at 1280 left the second
 * window and the whole caveat off the glass. So the figures ride in ONE
 * ELASTIC child that ellipsizes, ordered with the newsworthy window first —
 * it is the figure that changes what the reader does — and the marker rides
 * in a RIGID child beside it, which is what makes it un-cuttable rather than
 * merely early. The sentence the marker condenses is drawn in full in the
 * tokens sheet (expanded.ts), and stands as the marker's own title.
 */
export function drawFooterStatusActivityRateLimited(
  u: FooterStatusActivityRateLimited,
  deps: StripDeps,
  path: string,
): HTMLElement {
  const line = document.createElement("span");
  line.className = "footer-activity-rate-limited";

  const figures = document.createElement("span");
  figures.className = "footer-rate-figures";
  const ordered = orderedAllowances(u);
  ordered.forEach((allowance, index) => {
    if (index > 0) figures.appendChild(document.createTextNode(" | "));
    figures.appendChild(
      drawFooterAllowance(allowance.value, allowance.label, deps, `${path}.${allowance.label}`),
    );
  });
  if (ordered.length > 0) line.appendChild(figures);

  const marker = drawFooterAllowanceSample(u.sample, `${path}.sample`);
  if (marker !== null) {
    if (ordered.length > 0) line.appendChild(document.createTextNode(" | "));
    line.appendChild(marker);
  }
  return line;
}

/** One drawable allowance, under the label the strip and the sheet both use. */
export interface LabelledAllowance {
  readonly label: string;
  readonly value: FooterAllowance;
}

/**
 * The windows the producer figured, NEWSWORTHY FIRST.
 *
 * Stable within each group, so with nothing newsworthy the pair keeps the
 * contract's own session-then-weekly order and nothing moves under a reader
 * for no reason. Exported because the tokens sheet draws the same windows in
 * the same order — a reader who opens the sheet must find the line they were
 * reading on the strip, not a reshuffled one.
 */
export function orderedAllowances(u: FooterStatusActivityRateLimited): LabelledAllowance[] {
  const present: LabelledAllowance[] = [];
  if (u.session !== undefined) present.push({ label: "session", value: u.session });
  if (u.weekly !== undefined) present.push({ label: "weekly", value: u.weekly });
  return [
    ...present.filter((a) => a.value.newsworthy),
    ...present.filter((a) => !a.value.newsworthy),
  ];
}

/** The sentence each unread outcome draws. */
const ALLOWANCE_UNREAD_SENTENCES = {
  serviceUnavailable: "the usage service did not answer",
  windowUnavailable: "no five-hour window was reported",
  utilizationUnavailable: "no utilization figure was reported",
} as const;

/** What the strip's marker condenses: the caveat, whole, in one sentence. */
export const ALLOWANCE_UNREAD_MARKER = "usage unread";

/**
 * THE CAVEAT IN FULL, or null when the last read managed a figure.
 *
 * One composition serving two surfaces: the strip's marker wears it as a
 * title and the tokens sheet draws it as a row, so the words a reader hovers
 * and the words they open the sheet to find can never drift apart.
 */
export function allowanceUnreadSentence(
  u: FooterAllowanceSample | undefined,
  path: string,
): string | null {
  const outcome = u?.outcome;
  if (outcome === undefined || outcome.case === undefined) return null;
  if (outcome.case === "available") return null;
  switch (outcome.case) {
    case "serviceUnavailable":
    case "windowUnavailable":
    case "utilizationUnavailable":
      return `${ALLOWANCE_UNREAD_MARKER} — ${ALLOWANCE_UNREAD_SENTENCES[outcome.case]}`;
    case "samplingFailure":
      // THE SHIM'S CAUSE VERBATIM where it stated one; the fixed half of the
      // sentence carries the meaning when it did not.
      return outcome.value.cause === ""
        ? `${ALLOWANCE_UNREAD_MARKER} — the sampling failed`
        : `${ALLOWANCE_UNREAD_MARKER} — the sampling failed: ${outcome.value.cause}`;
    default: {
      const other: { case: string } = outcome;
      return unreachableArm(path, other.case);
    }
  }
}

/**
 * WHAT THE LAST USAGE SAMPLE MANAGED TO READ, drawn only when it read NOTHING.
 *
 * The figures beside this cell are the last ones READ, not the last ones
 * ATTEMPTED, and a failed read leaves them standing on purpose — the daemon
 * never clears a figure it cannot replace. Without this cell a reader cannot
 * tell a fresh 41% from one the vendor stopped answering about an hour ago,
 * which is exactly the sentence the cell says.
 *
 * `available` DRAWS NOTHING. It is the unremarkable outcome — the figures are
 * as fresh as the sample — and a strip cell saying so would crowd the line to
 * report that nothing is wrong. Unset draws nothing either: no sample has been
 * attempted at all, so there is no read to report on.
 *
 * ON THE STRIP IT IS A MARKER, NOT THE SENTENCE. The strip has room for two
 * words beside a figure and not for a clause, and a caveat that is cut off is
 * a caveat nobody reads. So the cell says `usage unread` and nothing else; the
 * reason rides as its title and as a row of the tokens sheet, and the ARM is
 * still on the cell for anyone querying the drawn surface.
 */
export function drawFooterAllowanceSample(
  u: FooterAllowanceSample | undefined,
  path: string,
): HTMLElement | null {
  const outcome = u?.outcome;
  if (outcome === undefined || outcome.case === undefined) return null;
  const sentence = allowanceUnreadSentence(u, path);
  if (sentence === null) return null;

  const cell = document.createElement("span");
  cell.className = "footer-allowance-unread";
  cell.setAttribute("data-sample", outcome.case);
  cell.textContent = ALLOWANCE_UNREAD_MARKER;
  cell.title = `${sentence} — the figures beside this were the last ones read, and may be stale`;
  return cell;
}

/**
 * One allowance: "session 72% · resets in 1h 5m".
 *
 * `utilization` is 0..1 on the wire and a PERCENTAGE on screen — display
 * formatting of one shipped figure, not a derivation of a second one — and
 * `resets_at_s` is the vendor's own SECONDS, converted once here.
 *
 * THE VENDOR'S STATUS IS AN ARM NOW, not the free string it used to be, so the
 * cell PAINTS it (`tones.ts`) instead of parking a word in a title nobody
 * reads: green while there is headroom, yellow on the vendor's own warning, red
 * once a call would be rejected. The arm rides as `data-arm` and its sentence
 * as the title.
 *
 * AN UNSET STATUS IS LEGAL AND MEANS "NO VERDICT YET". The figures are sampled
 * from the account's usage and are true the moment they are pushed; the vendor's
 * verdict arrives later, on its own rate-limit event. So an allowance with no
 * arm draws its percentage and its reset countdown UNPAINTED and untitled —
 * absence of a verdict, not a verdict of its own — and gains the colour, the
 * title and the `data-arm` on the push that carries one.
 */
export function drawFooterAllowance(
  u: FooterAllowance,
  label: string,
  deps: StripDeps,
  path: string,
): HTMLElement {
  const status = u.status.case === undefined ? null : u.status;
  const span = document.createElement("span");
  span.className =
    status === null
      ? "footer-allowance"
      : `footer-allowance arm-${status.case} ${allowanceStatusClass(status.case)}`;
  span.setAttribute("data-allowance", label);
  if (status !== null) {
    span.setAttribute("data-arm", status.case);
    span.title = allowanceStatusTitle(status, `${path}.status`);
  }
  if (u.newsworthy) span.setAttribute("data-newsworthy", "true");
  if (u.newsworthy) span.classList.add("footer-allowance-newsworthy");
  log("debug", `drawing the ${label} allowance as ${status?.case ?? "unverdicted"}`, {
    operation: "footer.strip.allowance",
    context: { allowance: label, status: status?.case, newsworthy: u.newsworthy },
  });

  span.appendChild(document.createTextNode(`${label} `));
  const percent = document.createElement("span");
  percent.className = activityDatumClass("percent");
  percent.setAttribute("data-datum", "percent");
  percent.textContent = `${Math.round(u.utilization * 100)}%`;
  span.appendChild(percent);

  const resets = document.createElement("span");
  resets.setAttribute("data-countdown", "");
  const resetsAtMs = msOf(u.resetsAtS, `${path}.resets_at_s`) * 1000;
  tick(resets, deps.ctx.ticker, (nowMs) => {
    resets.textContent = ` · resets in ${formatCountdown(resetsAtMs - nowMs)}`;
  });
  span.appendChild(resets);
  return span;
}

/**
 * What each allowance arm SAYS, as the cell's hover.
 *
 * Exhaustive over the oneof: the arms are the vendor's own vocabulary, now in
 * evidence, and an arm a newer daemon set is refused here rather than drawn as
 * an untitled cell wearing whatever colour the table happened to have.
 */
function allowanceStatusTitle(
  status: NonNullable<FooterAllowance["status"]> & { case: string },
  path: string,
): string {
  switch (status.case) {
    case "allowed":
      return "within the allowance";
    case "allowedWarning":
      return "close to the allowance — the vendor is warning";
    case "rejected":
      return "the allowance is spent — calls are being rejected";
    default: {
      const other: { case: string } = status;
      return unreachableArm(path, other.case);
    }
  }
}

/**
 * The activity's relative age, "· 2m ago", ticking from the shipped instant.
 *
 * An instant is shipped and a duration is drawn — the same convention the turn
 * clock and the countdowns follow, so nothing about how long a line has stood
 * ever depends on push cadence.
 */
export function drawFooterStatusActivityAt(
  u: FooterStatusActivityAt,
  deps: StripDeps,
  path: string,
): HTMLElement {
  const age = document.createElement("span");
  age.className = "footer-activity-age";
  age.setAttribute("data-age", "");
  const atMs = msOf(u.atMs, `${path}.at_ms`);
  tick(age, deps.ctx.ticker, (nowMs) => {
    age.textContent = ` · ${formatTickedAge(nowMs - atMs)} ago`;
  });
  return age;
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
  cell.appendChild(drawTurnStopControl(deps.ctx));
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

/** The cell's one figure, exactly as the daemon formatted it. */
export function drawFooterTokensCellInput(u: FooterTokensCellInput): HTMLElement {
  const input = document.createElement("span");
  input.className = "footer-tokens-input";
  input.textContent = u.text;
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
  return cell;
}

/** ⚙ N — the live agent-spawned subagents. */
export function drawFooterChipAgents(u: FooterChipAgents, deps: StripDeps): HTMLElement {
  return chip("agents", "⚙", String(u.count), deps);
}

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
  const mark = document.createElement("span");
  mark.className = "footer-chip-glyph";
  mark.setAttribute("data-glyph", panel);
  mark.setAttribute("aria-hidden", "true");
  mark.textContent = glyph;
  el.appendChild(mark);
  el.appendChild(document.createTextNode(` ${count}`));
  el.addEventListener("click", (event: MouseEvent) => {
    event.preventDefault();
    deps.onSelect(panel);
  });
  return el;
}

// ---- shared helpers -------------------------------------------------------

/**
 * A status or substatus word: the arm's name, lowercase ASCII, with spaces.
 *
 * `protoArmName` reverses protobuf-es's lowerCamel back to the proto's own
 * snake_case, and the underscores become spaces — footer.proto's rule, applied
 * in exactly one place for both cells.
 */
export function statusWords(armCase: string): string {
  return protoArmName(armCase).replace(/_/g, " ");
}

/**
 * The substatus word, with the ONE arm whose bare name would lie.
 *
 * `FooterStatusClosing`'s step is spelled `blocked`, which is also a whole
 * STATUS in this same strip — a close that cannot proceed would read exactly
 * like a session the vendor has blocked. The message's own name
 * (`FooterSubStatusCloseBlocked`) is what the schema calls it, so the cell
 * draws that. This is deliberately a single declared exception rather than a
 * per-arm label table: every other arm's name is already its label.
 */
export function subStatusWords(statusCase: string, subCase: string): string {
  if (statusCase === "closing" && subCase === "blocked") return "close blocked";
  return statusWords(subCase);
}

/** A composed line drawn verbatim, in its own classed span. */
function textLine(className: string, text: string): HTMLElement {
  const span = document.createElement("span");
  span.className = className;
  span.textContent = text;
  return span;
}

/**
 * Time REMAINING at second resolution — "4m 12s", "1h 5m".
 *
 * `formatAge` is the second-resolution two-level formatter; a countdown wants
 * exactly that shape, and a deadline already past floors to `0s` rather than
 * counting backwards. `formatCountdown` is the minute-resolution one and is
 * what the rate-limit resets (hours and days out) use instead.
 */
export function remainingLabel(ms: number): string {
  return formatAge(Math.max(0, ms));
}

/** The baseline strip's grabber notch, which lives inside the grow cell. */
function grabber(): HTMLElement {
  const notch = document.createElement("div");
  notch.className = "pfooter-grab";
  notch.setAttribute("aria-hidden", "true");
  return notch;
}
