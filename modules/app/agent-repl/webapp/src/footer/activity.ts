/**
 * activity — the strip's activity cell: the one elastic segment, and the one
 * place the footer's three activity TIERS are drawn.
 *
 * THE DAEMON RESOLVES THE TIER (footer.proto, the status family's rules): each
 * status arm's activity is a `oneof tier` holding EITHER that arm's salient
 * line OR the unpinned tiers beneath it: the transient over the enduring line.
 * The waiting arm and the two gate arms (permission, question) have no
 * unpinned branch at all, because a session parked on the user always stands
 * on a salient line. This module walks what it was pushed and
 * ranks nothing.
 *
 * THE ONE DECISION THIS CLIENT TAKES is the clock comparison inside the
 * unpinned tiers: the transient is drawn while the clock is before its
 * `expiry.expires_at_ms`, and the enduring line otherwise. A draw that picks
 * the transient asks the mount for exactly one re-render at that instant
 * (`expiry.ts`); nothing here polls, and the daemon pushes nothing at the
 * lapse.
 *
 * COLOUR ON TYPED DATUMS. A statically typed datum inside an activity — a sha,
 * a retry attempt, a count, an allowance percentage, the subagent a transient
 * came from — is drawn as its own coloured span (`tones.ts`), because those are
 * the figures a reader tracks across pushes and the composed prose around them
 * is not.
 *
 * TEXT IS NEVER CUT HERE. The grow cell ellipsizes by CSS (`.pfooter-grow`);
 * the daemon caps a streamed tail from its end, and this module draws what it
 * was sent, whole.
 *
 * CLOCKS TICK HERE, NOT ON THE WIRE. A wakeup, a give-up deadline and a
 * usage reset count down to their instants, and a line's age and a usage
 * reading's age count up from theirs — all off the shared ticker, and none of
 * them is ever pushed.
 */
import type {
  FooterActivityEnduring,
  FooterActivityEnduringSeatSpend,
  FooterActivityEnduringUsage,
  FooterActivityTransient,
  FooterActivityTransientApiRestored,
  FooterActivityTransientCompactionConcluded,
  FooterColdGateFiguredLine,
  FooterActivityTransientContextInjected,
  FooterActivityTransientDaemonError,
  FooterActivityTransientDaemonWarning,
  FooterActivityTransientHook,
  FooterActivityTransientNetworkResume,
  FooterActivityTransientOverEnduring,
  FooterActivityTransientSessionChange,
  FooterActivityTransientTask,
  FooterActivityTransientUpdated,
  FooterAllowance,
  FooterWeeklyAllowance,
  FooterWeeklyModelAllowances,
  FooterStatusActivityAt,
  FooterStatusActivityAuthenticating,
  FooterStatusActivityBlockedOnUser,
  FooterStatusActivityCloseBlocked,
  FooterStatusActivityColdGateCost,
  FooterStatusActivityCompaction,
  FooterStatusActivityFault,
  FooterStatusActivityGatedCall,
  FooterStatusActivityInterrupting,
  FooterStatusActivityNotification,
  FooterStatusActivityQueryDied,
  FooterStatusActivityQuestionLead,
  FooterStatusActivityRetrying,
  FooterStatusActivityStartFailed,
  FooterStatusActivityTurnEnded,
  FooterStatusActivityVendorStart,
  FooterStatusActivityUpdate,
  FooterStatusActivityUpdateComponent,
  FooterStatusActivityUpdateNote,
  FooterStatusActivityUpdateWaiting,
  FooterStatusActivityWakeup,
  FooterStatusBackgroundActivity,
  FooterStatusBackgroundSalient,
  FooterStatusVendorFaultActivity,
  FooterStatusVendorFaultSalient,
  FooterStatusNetworkFaultActivity,
  FooterStatusNetworkFaultSalient,
  FooterStatusActivityNetworkOffline,
  FooterStatusClosingActivity,
  FooterStatusClosingSalient,
  FooterStatusAgentReplFaultActivity,
  FooterStatusStartingActivity,
  FooterStatusAgentReplFaultSalient,
  FooterStatusStartingSalient,
  FooterStatusIdleActivity,
  FooterStatusIdleSalient,
  FooterStatusInterruptedActivity,
  FooterStatusInterruptedSalient,
  FooterStatusLoadingActivity,
  FooterStatusLoadingSalient,
  FooterStatusMergingActivity,
  FooterStatusMergingSalient,
  FooterStatusWorkingActivity,
  FooterStatusWorkingSalient,
  FooterStatusWaitingActivity,
  FooterStatusWaitingSalient,
  FooterStatusPermissionActivity,
  FooterStatusPermissionSalient,
  FooterStatusQuestionActivity,
  FooterStatusQuestionSalient,
  FooterMoney,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { formatAge, formatCountdown, formatTickedAge } from "../duration.js";
import type { Ticker } from "../clock.js";
import { tick } from "../feed/ticking.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import {
  msOf,
  requireCase,
  requireMessage,
  unreachableArm,
} from "../rpc/strict.js";
import type { TransientExpirySchedule } from "./expiry.js";
import { footerClockSpan } from "./clock-span.js";
import { drawFooterStatusActivityMergeStep } from "./merge-step.js";
import { coldGateFiguredLine, grabber, statusWords, textLine } from "./parts.js";
import { pressurePercentColor } from "../pressure-color.js";
import { MalformedView } from "../rpc/malformed.js";
import { activityDatumClass } from "./tones.js";
import { pluralNoun } from "../count-noun.js";

/**
 * What a clocked piece of the cell needs: the ticker its figure rides.
 *
 * Narrower than `ActivityDeps` because the tokens sheet draws the same
 * allowance cells and has no expiry to schedule — one drawing of an allowance,
 * on both surfaces, rather than a second one that could word it differently.
 */
export interface AllowanceDeps {
  ctx: AppContext;
}

/** What the activity cell needs: the ticker, and the mount's one expiry timer. */
export interface ActivityDeps extends AllowanceDeps {
  /** Asks for the one re-render at a drawn transient's expiry instant. */
  readonly expiry: TransientExpirySchedule;
}

// ---- the tier walk --------------------------------------------------------

/** Every per-status activity cell, as one type to walk. */
export type FooterActivity =
  | FooterStatusIdleActivity
  | FooterStatusWorkingActivity
  | FooterStatusWaitingActivity
  | FooterStatusPermissionActivity
  | FooterStatusQuestionActivity
  | FooterStatusInterruptedActivity
  | FooterStatusMergingActivity
  | FooterStatusBackgroundActivity
  | FooterStatusVendorFaultActivity
  | FooterStatusAgentReplFaultActivity
  | FooterStatusStartingActivity
  | FooterStatusNetworkFaultActivity
  | FooterStatusClosingActivity
  | FooterStatusLoadingActivity;

/** Every per-status salient line, as one type to walk. */
export type FooterSalient =
  | FooterStatusIdleSalient
  | FooterStatusWorkingSalient
  | FooterStatusWaitingSalient
  | FooterStatusPermissionSalient
  | FooterStatusQuestionSalient
  | FooterStatusInterruptedSalient
  | FooterStatusMergingSalient
  | FooterStatusBackgroundSalient
  | FooterStatusVendorFaultSalient
  | FooterStatusAgentReplFaultSalient
  | FooterStatusStartingSalient
  | FooterStatusNetworkFaultSalient
  | FooterStatusClosingSalient
  | FooterStatusLoadingSalient;

/** The tier the daemon resolved for a cell. */
export type ActivityTier =
  | { readonly case: "salient"; readonly value: FooterSalient }
  | {
      readonly case: "unpinned";
      readonly value: FooterActivityTransientOverEnduring;
    };

/**
 * The tier a cell carries.
 *
 * THE CELLS OF A SESSION PARKED ON THE USER HAVE NO ONEOF: the waiting,
 * permission and question cells hold their salient line directly, so each is
 * read as the salient tier by construction. Every other cell's `tier` is
 * switched exhaustively, and an unset one is malformed.
 */
export function activityTier(
  activity: FooterActivity,
  path: string,
): ActivityTier {
  if (
    activity.$typeName === "frontend.v1.FooterStatusWaitingActivity" ||
    activity.$typeName === "frontend.v1.FooterStatusPermissionActivity" ||
    activity.$typeName === "frontend.v1.FooterStatusQuestionActivity"
  ) {
    return {
      case: "salient",
      value: requireMessage(activity.salient, `${path}.salient`),
    };
  }
  const tier = requireCase(activity.tier, `${path}.tier`);
  switch (tier.case) {
    case "salient":
      return { case: "salient", value: tier.value };
    case "unpinned":
      return { case: "unpinned", value: tier.value };
    default: {
      const other: { case: string } = tier;
      return unreachableArm(`${path}.tier`, other.case);
    }
  }
}

/**
 * The salient kind a cell is drawing, or undefined when it is unpinned.
 *
 * For a reader of the SALIENT line alone — the cold gate's card waits on the
 * compaction's own line — which must not reach into the transient pair.
 */
export function salientKind(
  activity: FooterActivity,
  path: string,
): (FooterSalient["kind"] & { case: string }) | undefined {
  const tier = activityTier(activity, path);
  if (tier.case !== "salient") return undefined;
  return requireCase(tier.value.kind, `${path}.salient.kind`);
}

/**
 * The enduring usage a cell carries, or undefined.
 *
 * Set only while the cell is unpinned and the daemon chose the usage line by
 * the 80% rule: a standing salient line ships no enduring line at all, and an
 * enduring line the context window claimed carries no usage.
 */
export function enduringUsage(
  activity: FooterActivity,
  path: string,
): FooterActivityEnduringUsage | undefined {
  const tier = activityTier(activity, path);
  if (tier.case !== "unpinned") return undefined;
  const line = requireMessage(
    tier.value.enduring,
    `${path}.unpinned.enduring`,
  ).line;
  return line.case === "usage" ? line.value : undefined;
}

// ---- the cell ---------------------------------------------------------------

/**
 * The activity cell: the salient line, the live transient, or the enduring
 * line, with the tier stamped on the cell as `data-tier` and the drawn kind as
 * `data-arm`.
 */
export function drawFooterStatusActivity(
  activity: FooterActivity,
  deps: ActivityDeps,
  path: string,
): HTMLElement {
  const cell = document.createElement("div");
  cell.className = "pfooter-cell pfooter-grow footer-activity";
  const drawn = drawTier(activityTier(activity, path), deps, path);
  cell.setAttribute("data-tier", drawn.tier);
  cell.setAttribute("data-arm", drawn.arm);
  log.debug(`drawing the ${drawn.tier} activity line: ${drawn.arm}`, {
    operation: "footer.strip.activity",
    context: { tier: drawn.tier, arm: drawn.arm },
  });
  cell.appendChild(drawn.line);
  if (drawn.age !== null) cell.appendChild(drawn.age);
  cell.appendChild(grabber());
  // THE WHOLE LINE AS THE CELL'S HOVER. The cell is the strip's one elastic
  // segment and ellipsizes by design, so whatever it cannot fit is otherwise
  // reachable only by opening a sheet. The title is read off the DRAWN line
  // rather than recomposed, so it can never say something the cell does not,
  // and it rides the shared ticker so a countdown in it stays honest as it
  // counts. Registered AFTER the line's own clocks, so it reads their newest
  // text and not the previous second's.
  const line = drawn.line;
  tick(cell, deps.ctx.ticker, () => {
    cell.title = line.textContent ?? "";
  });
  return cell;
}

/** One drawn line: which tier and kind it is, the line, and its age if any. */
interface DrawnLine {
  readonly tier: "salient" | "transient" | "enduring";
  readonly arm: string;
  readonly line: HTMLElement;
  readonly age: HTMLElement | null;
}

/** The line a resolved tier draws. */
function drawTier(
  tier: ActivityTier,
  deps: ActivityDeps,
  path: string,
): DrawnLine {
  switch (tier.case) {
    case "salient": {
      const salientPath = `${path}.salient`;
      const kind = requireCase(tier.value.kind, `${salientPath}.kind`);
      return {
        tier: "salient",
        arm: kind.case,
        line: drawSalientKind(kind, deps, `${salientPath}.${kind.case}`),
        age: drawFooterStatusActivityAt(
          requireMessage(tier.value.at, `${salientPath}.at`),
          deps,
          `${salientPath}.at`,
        ),
      };
    }
    case "unpinned":
      return drawUnpinned(tier.value, deps, `${path}.unpinned`);
  }
}

/**
 * THE ONE CLOCK DECISION: the transient while the clock is before its expiry
 * instant, then the enduring line.
 *
 * The transient is validated whether or not it is drawn, so a malformed one
 * is refused on the push that carried it rather than silently outlived. A
 * drawn transient schedules the one re-render at its expiry.
 */
function drawUnpinned(
  u: FooterActivityTransientOverEnduring,
  deps: ActivityDeps,
  path: string,
): DrawnLine {
  const enduring = requireMessage(u.enduring, `${path}.enduring`);
  if (u.transient !== undefined) {
    const transientPath = `${path}.transient`;
    const transient = u.transient;
    const expiry = requireMessage(transient.expiry, `${transientPath}.expiry`);
    const expiresAtMs = msOf(
      expiry.expiresAtMs,
      `${transientPath}.expiry.expires_at_ms`,
    );
    const at = requireMessage(transient.at, `${transientPath}.at`);
    const kind = requireCase(transient.kind, `${transientPath}.kind`);
    const nowMs = deps.ctx.ticker.now();
    if (nowMs < expiresAtMs) {
      log.debug(
        `the transient ${kind.case} is live; drawing it until it lapses`,
        {
          operation: "footer.strip.transient-live",
          context: {
            kind: kind.case,
            expires_at_ms: expiresAtMs,
            now_ms: nowMs,
          },
        },
      );
      deps.expiry.schedule(expiresAtMs);
      return {
        tier: "transient",
        arm: kind.case,
        line: drawFooterActivityTransient(transient, deps, transientPath),
        age: drawFooterStatusActivityAt(at, deps, `${transientPath}.at`),
      };
    }
    log.debug(
      `the transient ${kind.case} has lapsed; drawing the enduring line`,
      {
        operation: "footer.strip.transient-lapsed",
        context: { kind: kind.case, expires_at_ms: expiresAtMs, now_ms: nowMs },
      },
    );
  }
  return {
    tier: "enduring",
    arm: "enduring",
    line: drawFooterActivityEnduring(enduring, deps, `${path}.enduring`),
    age: null,
  };
}

// ---- the salient tier -------------------------------------------------------

/** The one standing salient line, by kind. */
function drawSalientKind(
  kind: FooterSalient["kind"] & { case: string },
  deps: ActivityDeps,
  path: string,
): HTMLElement {
  switch (kind.case) {
    case "update":
      return drawFooterStatusActivityUpdate(kind.value, path);
    case "queryDied":
      return drawFooterStatusActivityQueryDied(kind.value);
    case "compaction":
      return drawFooterStatusActivityCompaction(kind.value);
    case "retrying":
      return drawFooterStatusActivityRetrying(kind.value, deps, path);
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
    case "mergeStep":
      return drawFooterStatusActivityMergeStep(kind.value, path);
    case "authenticating":
      return drawFooterStatusActivityAuthenticating(kind.value);
    case "fault":
      return drawFooterStatusActivityFault(kind.value);
    case "startFailed":
      return drawFooterStatusActivityStartFailed(kind.value);
    case "vendorStart":
      return drawFooterStatusActivityVendorStart(kind.value);
    case "turnEnded":
      return drawFooterStatusActivityTurnEnded(kind.value, deps, path);
    case "offline":
      return drawFooterStatusActivityNetworkOffline(kind.value);
    case "closeBlocked":
      return drawFooterStatusActivityCloseBlocked(kind.value);
    case "notification":
      return drawFooterStatusActivityNotification(kind.value);
    default: {
      const other: { case: string } = kind;
      return unreachableArm(path, other.case);
    }
  }
}

/** What the shim observed of the unreachable network, verbatim. */
export function drawFooterStatusActivityNetworkOffline(u: FooterStatusActivityNetworkOffline): HTMLElement {
  return textLine("footer-activity-network-offline", u.text);
}

/** The agent's push notification, verbatim. */
export function drawFooterStatusActivityNotification(
  u: FooterStatusActivityNotification,
): HTMLElement {
  return textLine("footer-activity-notification", u.text);
}

// ---- the transient tier -----------------------------------------------------

/**
 * One transient line, with the subagent it came from in front when it came
 * from one: "Explore · write the tests · 3/7". The label is an identity, so it wears
 * the identity colour; the main agent's work carries no prefix.
 */
export function drawFooterActivityTransient(
  u: FooterActivityTransient,
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  const kind = requireCase(u.kind, `${path}.kind`);
  const body = drawTransientKind(kind, deps, `${path}.${kind.case}`);
  if (u.agent === undefined) return body;
  const line = document.createElement("span");
  line.className = "footer-activity-transient";
  const agent = document.createElement("span");
  agent.className = `footer-activity-agent ${activityDatumClass("agent")}`;
  agent.setAttribute("data-datum", "agent");
  agent.textContent = u.agent.label;
  line.appendChild(agent);
  line.appendChild(document.createTextNode(" · "));
  line.appendChild(body);
  return line;
}

/** One transient's line, by kind. */
function drawTransientKind(
  kind: FooterActivityTransient["kind"] & { case: string },
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  switch (kind.case) {
    case "task":
      return drawFooterActivityTransientTask(kind.value);
    case "hook":
      return drawFooterActivityTransientHook(kind.value);
    case "contextInjected":
      return drawFooterActivityTransientContextInjected(kind.value);
    case "fault":
      return drawFooterStatusActivityFault(kind.value);
    case "daemonWarning":
      return drawFooterActivityTransientDaemonWarning(kind.value);
    case "daemonError":
      return drawFooterActivityTransientDaemonError(kind.value);
    case "sessionChange":
      return drawFooterActivityTransientSessionChange(kind.value);
    case "updated":
      return drawFooterActivityTransientUpdated(kind.value, path);
    case "networkResume":
      return drawFooterActivityTransientNetworkResume(kind.value, deps, path);
    case "compactionConcluded":
      return drawFooterActivityTransientCompactionConcluded(kind.value);
    case "apiRestored":
      return drawFooterActivityTransientApiRestored(kind.value);
    default: {
      const other: { case: string } = kind;
      return unreachableArm(path, other.case);
    }
  }
}

/**
 * The task tracker moving: the task's subject, then the tracker's progress
 * "3/7" as the figure it is — the tasks chip's own fraction.
 */
export function drawFooterActivityTransientTask(
  u: FooterActivityTransientTask,
): HTMLElement {
  const line = document.createElement("span");
  line.className = "footer-activity-task";
  line.appendChild(document.createTextNode(`${u.subject} · `));
  const progress = document.createElement("span");
  progress.className = activityDatumClass("count");
  progress.setAttribute("data-datum", "count");
  progress.textContent = `${u.completed}/${u.total}`;
  line.appendChild(progress);
  return line;
}

/** The running hook's name. */
export function drawFooterActivityTransientHook(
  u: FooterActivityTransientHook,
): HTMLElement {
  return textLine("footer-activity-hook", u.name);
}

/** The injected item's composed line, verbatim. */
export function drawFooterActivityTransientContextInjected(
  u: FooterActivityTransientContextInjected,
): HTMLElement {
  return textLine("footer-activity-context-injected", u.text);
}

/** A daemon warning: the operation its record names, then its message. */
export function drawFooterActivityTransientDaemonWarning(
  u: FooterActivityTransientDaemonWarning,
): HTMLElement {
  return textLine(
    "footer-activity-daemon-warning",
    `${u.operation} · ${u.message}`,
  );
}

/** A daemon error that blocks nothing: the operation, then its message. */
export function drawFooterActivityTransientDaemonError(
  u: FooterActivityTransientDaemonError,
): HTMLElement {
  return textLine(
    "footer-activity-daemon-error",
    `${u.operation} · ${u.message}`,
  );
}

/**
 * A compaction's concluded outcome ("compacted and resumed (101.6k → 12.4k) ·
 * claude-sonnet-5 spent 104.3k tokens"), in the running compaction line's own
 * class, so the outcome reads as that line's last word rather than a new kind
 * of line. Drawn from its parts when it names the summarizer's spend, so the
 * spend takes the cold gate's gradient; verbatim otherwise.
 */
export function drawFooterActivityTransientCompactionConcluded(
  u: FooterActivityTransientCompactionConcluded,
): HTMLElement {
  return compactionLine(u.text, u.parts, "FooterActivityTransientCompactionConcluded.parts");
}

/** A session setting's change, as the daemon composed it. */
export function drawFooterActivityTransientSessionChange(
  u: FooterActivityTransientSessionChange,
): HTMLElement {
  return textLine("footer-activity-session-change", u.text);
}

/**
 * A FINISHED DEPLOY: "agent-repl hot reloaded", then what it left for later on this
 * workspace — the deploy line's own shape and hooks (`.footer-activity-update`,
 * `data-phase="updated"`), since it is that line's last word, now announced by
 * the successor as a transient.
 */
/** The finished deploy's words (owner ruling 2026-10-06: "updated" alone named nothing). */
export const UPDATED_TEXT = "agent-repl hot reloaded";

export function drawFooterActivityTransientUpdated(
  u: FooterActivityTransientUpdated,
  path: string,
): HTMLElement {
  const line = document.createElement("span");
  line.className = "footer-activity-update";
  line.setAttribute("data-phase", "updated");
  line.appendChild(document.createTextNode(UPDATED_TEXT));
  appendUpdateNotes(line, u.notes, path);
  return line;
}

/**
 * The retried vendor call answering: "API answering again after 8 failed
 * attempts", the count the retrying line last stood at.
 */
export function drawFooterActivityTransientApiRestored(
  u: FooterActivityTransientApiRestored,
): HTMLElement {
  return textLine(
    "footer-activity-api-restored",
    `API answering again after ${u.failedAttempts} failed ${pluralNoun(u.failedAttempts, "attempt")}`,
  );
}

/**
 * An edge of a background subagent's wait for the API: "network resume ·
 * waiting · gives up in 24m", "network resume · resumed", "network resume ·
 * gave up", "network resume · abandoned · <the shim's reason>".
 *
 * EVERY WORD IS AN ARM NAME, lowercase with spaces — the kind's, then the
 * edge's — by the rule the deploy line already follows; the give-up deadline
 * is shipped as an instant and counts down here.
 */
export function drawFooterActivityTransientNetworkResume(
  u: FooterActivityTransientNetworkResume,
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  const edge = requireCase(u.edge, `${path}.edge`);
  const line = document.createElement("span");
  line.className = "footer-activity-network-resume";
  line.setAttribute("data-edge", edge.case);
  line.appendChild(
    document.createTextNode(
      `${statusWords("networkResume")} · ${statusWords(edge.case)}`,
    ),
  );
  switch (edge.case) {
    case "waiting":
      line.appendChild(document.createTextNode(" · "));
      line.appendChild(
        drawGivesUpCountdown(
          edge.value.givesUpAtMs,
          deps,
          `${path}.waiting.gives_up_at_ms`,
        ),
      );
      return line;
    case "abandoned":
      line.appendChild(document.createTextNode(` · ${edge.value.reason}`));
      return line;
    case "resumed":
    case "gaveUp":
      return line;
    default: {
      const other: { case: string } = edge;
      return unreachableArm(`${path}.edge`, other.case);
    }
  }
}

/**
 * "gives up in 24m", ticking down to the shim's give-up instant at minute
 * resolution — the usage resets' countdown, since the wait runs for minutes.
 * Shared by the network-resume transient and the agents panel's waiting row,
 * which say the same deadline.
 */
export function drawGivesUpCountdown(
  givesUpAtMs: bigint,
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  const deadlineMs = msOf(givesUpAtMs, path);
  return footerClockSpan(
    deps.ctx.ticker,
    "countdown",
    "footer-gives-up",
    (span, nowMs) => {
      span.textContent = `gives up in ${formatCountdown(deadlineMs - nowMs)}`;
    },
  );
}

// ---- the enduring tier ------------------------------------------------------

/**
 * THE ENDURING LINE: the account's usage BY BILLING MODE (owner ruling,
 * 2026-10-06). A subscription's allowances read "session 41% · resets in
 * 2h 5m | weekly 12% · resets in 3d"; a per-seat account's spend reads
 * "$223.88 of $12,000 this month". It is always true while it stands, so it
 * carries no age.
 *
 * IT IS NEVER EMPTY (owner ruling, 2026-10-06): an account never observed
 * reads "usage not yet seen for this account".
 * The figures ellipsize (in the fixed order of `orderedAllowances`) inside an
 * inline flex box no wider than the cell.
 */
export function drawFooterActivityEnduring(
  u: FooterActivityEnduring,
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  const line = document.createElement("span");
  line.className = "footer-activity-enduring";
  const chosen = requireCase(u.line, `${path}.line`);
  line.setAttribute("data-line", chosen.case);
  switch (chosen.case) {
    case "usage":
      for (const part of drawFooterActivityEnduringUsage(
        chosen.value,
        deps,
        `${path}.usage`,
      )) {
        line.appendChild(part);
      }
      break;
    case "unobserved":
      line.textContent = ENDURING_UNOBSERVED_TEXT;
      break;
    case "seatSpend":
      line.append(...drawFooterActivityEnduringSeatSpend(chosen.value, `${path}.seat_spend`));
      break;
    default: {
      const other: { case: string } = chosen;
      return unreachableArm(`${path}.line`, other.case);
    }
  }
  return line;
}

/** The enduring line before anything is known of the account's usage. */
export const ENDURING_UNOBSERVED_TEXT = "usage not yet seen for this account";

/**
 * The space between a seat's spent figure and "of": a NO-BREAK space, because
 * the enduring line is an inline flex box and an ordinary space leading the
 * text item after the figure is collapsed away (the same reason
 * `FOOTER_RATE_SEPARATOR_GAP` is one).
 */
export const SEAT_SPEND_GAP = "\u00a0";

/** What a seat's spend reads before the vendor has reported one. */
export const SEAT_SPEND_UNSEEN_TEXT = "spend not yet seen";

/**
 * A per-seat account's spend: "$223.88 of $12,000 this month", or "spend not
 * yet seen of $12,000 this month" before the vendor reports one. The vendor
 * states no billing-period reset, so none is drawn.
 *
 * THE SPENT FIGURE WEARS THE PERCENT GRADIENT, driven by spent/allotment
 * (`utilization`) through the one path every footer percentage takes
 * (`paintPressure`); the allotment and the words stay the line's color.
 */
export function drawFooterActivityEnduringSeatSpend(
  u: FooterActivityEnduringSeatSpend,
  path: string,
): (HTMLElement | string)[] {
  const allotment = formatMoney(requireMessage(u.allotment, `${path}.allotment`), `${path}.allotment`);
  // REGRESSION (2026-10-08): the line read "$223.88of $12,000". The enduring
  // line is an inline flex box, where this tail is an anonymous flex item and
  // its leading ordinary space is collapsed away; the no-break space survives.
  const tail = `${SEAT_SPEND_GAP}of ${allotment} this month`;
  if (u.spent === undefined) {
    if (u.utilization !== undefined) {
      throw new MalformedView(`${path}.utilization`, "a utilization is set with no spend");
    }
    return [`${SEAT_SPEND_UNSEEN_TEXT}${tail}`];
  }
  if (u.utilization === undefined) {
    throw new MalformedView(`${path}.utilization`, "a spend is set with no utilization");
  }
  const spent = document.createElement("span");
  spent.className = "footer-seat-spent";
  spent.setAttribute("data-datum", "spent");
  spent.textContent = formatMoney(u.spent, `${path}.spent`);
  paintPressure(spent, u.utilization);
  return [spent, tail];
}

/**
 * An amount of money as the footer draws it: dollars (or the currency's own
 * major unit), with the minor digits only when the amount has any, so
 * 1_200_000 USD cents reads "$12,000" and 22_388 reads "$223.88". A currency
 * code the runtime cannot name is refused at PATH, never drawn.
 */
export function formatMoney(m: FooterMoney, path: string): string {
  let digits: number;
  try {
    digits = new Intl.NumberFormat("en-US", { style: "currency", currency: m.currency })
      .resolvedOptions().maximumFractionDigits ?? 0;
  } catch (err) {
    throw new MalformedView(`${path}.currency`, `${JSON.stringify(m.currency)} is not a currency: ${String(err)}`);
  }
  const scale = 10n ** BigInt(digits);
  const whole = m.amountMinor % scale === 0n;
  const amount = Number(m.amountMinor) / Number(scale);
  return new Intl.NumberFormat("en-US", {
    style: "currency",
    currency: m.currency,
    minimumFractionDigits: whole ? 0 : digits,
    maximumFractionDigits: whole ? 0 : digits,
  }).format(amount);
}

/**
 * The usage line: EVERY window the vendor figured, in the fixed order of
 * `orderedAllowances`.
 *
 * AN ALLOWANCE THE PRODUCER LEFT UNSET IS DRAWN ABSENT, never required: a
 * figure nobody reported is never drawn. The overage window is drawn beside
 * the other two when the vendor reported one, which most accounts never do.
 *
 * THE STRIP DRAWS THE FIGURES LAST READ AND THE AGE OF THAT READING, never a
 * "usage unread" caveat (owner ruling of 2026-09-15).
 */
export function drawFooterActivityEnduringUsage(
  u: FooterActivityEnduringUsage,
  deps: AllowanceDeps,
  path: string,
): HTMLElement[] {
  const parts: HTMLElement[] = [];
  const ordered = orderedAllowances(u);
  if (ordered.length > 0) {
    const figures = document.createElement("span");
    figures.className = "footer-rate-figures";
    ordered.forEach((allowance, index) => {
      if (index > 0) figures.append(...drawFooterRateDivider());
      figures.appendChild(drawLabelledAllowance(allowance, deps, path));
    });
    parts.push(figures);
  }
  return parts;
}

/**
 * The gap on EACH side of the usage line's "|": exactly three spaces (owner
 * ruling, 2026-10-02). They are NO-BREAK spaces, which white-space collapsing
 * never folds into one, and they are drawn in the line's own font and color.
 */
export const FOOTER_RATE_SEPARATOR_GAP = "\u00a0".repeat(3);

/**
 * Everything drawn between two allowances on the enduring usage line: the
 * gap, the "|", the gap. The one place the divider is composed, so no caller
 * can space it differently.
 */
export function drawFooterRateDivider(): [string, HTMLElement, string] {
  return [
    FOOTER_RATE_SEPARATOR_GAP,
    drawFooterRateSeparator(),
    FOOTER_RATE_SEPARATOR_GAP,
  ];
}

/**
 * The "|" between two allowances on the enduring usage line, drawn blue
 * (`--footer-rate-separator`) and bold so the figures it divides read as
 * separate windows (owner rulings, 2026-09-30 and 2026-10-02). The gaps around
 * it stay the line's (`drawFooterRateDivider`).
 */
export function drawFooterRateSeparator(): HTMLElement {
  const separator = document.createElement("span");
  separator.className = "footer-rate-separator";
  separator.textContent = "|";
  return separator;
}

/**
 * A footer percentage, "42%": a 0..1 figure on the wire drawn as a whole
 * percent, colored by how full it is (`pressurePercentColor`). Every percentage
 * the footer draws goes through here, so none is ever drawn unpainted.
 */
export function drawFooterPercent(fraction: number): HTMLElement {
  const figure = Math.round(fraction * 100);
  const percent = document.createElement("span");
  percent.className = "footer-percent";
  percent.setAttribute("data-datum", "percent");
  percent.textContent = `${String(figure)}%`;
  paintPressure(percent, fraction);
  return percent;
}

/**
 * Color a footer figure by how full it is: a 0..1 fraction on the wire, as
 * the whole percent it would be drawn, through `pressurePercentColor`. THE ONE
 * GRADIENT PATH for every footer figure that says how full something is: an
 * allowance's percentage and a seat's spend alike.
 */
export function paintPressure(figure: HTMLElement, fraction: number): void {
  figure.style.color = pressurePercentColor(Math.round(fraction * 100));
}

/**
 * One drawable allowance, under the label the strip and the sheet both use:
 * a single window (session, overage) or the weekly element, which carries
 * the per-model weekly windows beside the account-wide one.
 */
export type LabelledAllowance =
  | { readonly kind: "single"; readonly label: string; readonly value: FooterAllowance }
  | { readonly kind: "weekly"; readonly label: "weekly"; readonly value: FooterWeeklyAllowance };

/**
 * The windows the producer figured, ALWAYS in the one fixed order session,
 * weekly, overage (owner ruling, 2026-10-08): which window stands left of the
 * blue "|" and which right never changes. Nothing about a window's figure —
 * newsworthiness, status, a reset — reorders the line; `newsworthy` only
 * emphasizes. (The line once drew the newsworthy window first, so the weekly
 * figure jumped left of the session's the moment it crossed the threshold.)
 * The strip and the tokens sheet draw exactly this list, each entry through
 * `drawLabelledAllowance`.
 */
export function orderedAllowances(
  u: FooterActivityEnduringUsage,
): LabelledAllowance[] {
  const present: LabelledAllowance[] = [];
  if (u.session !== undefined)
    present.push({ kind: "single", label: "session", value: u.session });
  if (u.weekly !== undefined)
    present.push({ kind: "weekly", label: "weekly", value: u.weekly });
  if (u.overage !== undefined)
    present.push({ kind: "single", label: "overage", value: u.overage });
  return present;
}

/**
 * One entry of `orderedAllowances`, drawn: the one place the strip and the
 * tokens sheet turn an entry into its cell, so neither can draw an entry the
 * other draws differently. `path` is the usage message's own.
 */
export function drawLabelledAllowance(
  allowance: LabelledAllowance,
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  const at = `${path}.${allowance.label}`;
  switch (allowance.kind) {
    case "single":
      return drawFooterAllowance(allowance.value, allowance.label, deps, at);
    case "weekly":
      return drawFooterWeeklyAllowance(allowance.value, deps, at);
    default: {
      const other: { kind: string } = allowance;
      return unreachableArm(path, other.kind);
    }
  }
}

// ---- the kinds' lines -------------------------------------------------------

/**
 * The compaction's own progress line, verbatim.
 *
 * BOTH COMPACTIONS SPEAK THROUGH IT — the vendor's auto-compaction and the cold
 * gate's "compact and resume" — and both draw under the existing
 * `working · compacting` step. The daemon composes the sentence out of the
 * phase it is in; this end adds no word of its own. It stands only while the
 * compaction RUNS: the concluded outcome is the transient
 * `compaction_concluded` (`drawFooterActivityTransientCompactionConcluded`).
 */
export function drawFooterStatusActivityCompaction(
  u: FooterStatusActivityCompaction,
): HTMLElement {
  return compactionLine(u.text, u.parts, "FooterStatusActivityCompaction.parts");
}

/**
 * A compaction line, running or concluded: from its parts when it names what
 * the summary cost, so the spend wears the cold gate's gradient; verbatim when
 * it names no spend.
 */
function compactionLine(
  text: string,
  parts: FooterColdGateFiguredLine | undefined,
  path: string,
): HTMLElement {
  if (parts === undefined) return textLine("footer-activity-compaction", text);
  return coldGateFiguredLine("footer-activity-compaction", text, parts, path);
}

/** The gated call's composed line, verbatim. */
export function drawFooterStatusActivityGatedCall(
  u: FooterStatusActivityGatedCall,
): HTMLElement {
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

/**
 * The cold gate's composed cost line, drawn from its parts so the token figure
 * takes its color: the cold-gate gradient over its window fill
 * (`coldGateFigureColor`, the same call the docked gate's figure makes). The
 * parts must make exactly the composed line; a line that does not is refused.
 */
export function drawFooterStatusActivityColdGateCost(
  u: FooterStatusActivityColdGateCost,
): HTMLElement {
  return coldGateFiguredLine("footer-activity-cold-gate-cost", u.text, u, "FooterStatusActivityColdGateCost");
}

/** The interrupting line, verbatim. */
export function drawFooterStatusActivityInterrupting(
  u: FooterStatusActivityInterrupting,
): HTMLElement {
  return textLine("footer-activity-interrupting", u.text);
}

/** The dead-query line, verbatim. */
export function drawFooterStatusActivityQueryDied(
  u: FooterStatusActivityQueryDied,
): HTMLElement {
  return textLine("footer-activity-query-died", u.text);
}

/**
 * The bring-up failure: the daemon's composed cause, verbatim. A failed
 * bring-up never drops held prompts any more (they wait "after reconnect"), so
 * the line says only what failed.
 */
export function drawFooterStatusActivityStartFailed(
  u: FooterStatusActivityStartFailed,
): HTMLElement {
  return textLine("footer-activity-start-failed", u.detail);
}

/**
 * The vendor's start standing: retrying, refused, or given up. The daemon
 * composes the whole line (attempt, cause, and the restart binding where one
 * applies), so it is drawn verbatim.
 */
/**
 * A TURN FAULT'S LINE (owner ruling, 2026-10-06): the daemon's per-cause
 * sentence for how the last turn ended, verbatim, the same sentence the feed's
 * turn-end row carries. A rate limit or an overload that stated a wait counts
 * down to it beside the sentence, then says the wait is over.
 */
export function drawFooterStatusActivityTurnEnded(
  u: FooterStatusActivityTurnEnded,
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  const line = document.createElement("span");
  line.className = "footer-activity-turn-ended";
  line.append(document.createTextNode(u.text));
  if (u.retryAt !== undefined) {
    const atMs = msOf(u.retryAt.atMs, `${path}.retry_at.at_ms`);
    line.append(document.createTextNode(" · "));
    line.append(
      footerClockSpan(deps.ctx.ticker, "countdown", "footer-turn-retry", (span, nowMs) => {
        const remaining = atMs - nowMs;
        span.toggleAttribute("data-ready", remaining <= 0);
        span.textContent = remaining > 0 ? `retry in ${remainingLabel(remaining)}` : "ready to retry";
      }),
    );
  }
  return line;
}

export function drawFooterStatusActivityVendorStart(
  u: FooterStatusActivityVendorStart,
): HTMLElement {
  return textLine("footer-activity-vendor-start", u.text);
}

/**
 * A STANDING DAEMON FAULT: the kind, then what it says.
 *
 * Drawn in the same cell and the same style as the bring-up failure beside it,
 * because it is the same thing said about the other eighteen fault kinds — the
 * owner's ruling of 2026-09-13 is that every one of them reaches the strip.
 *
 * THE KIND IS RENDERED LOWERCASE WITH SPACES, footer.proto's rule for the arm
 * cells applied to the one activity field that carries an arm's name. The
 * detail is the daemon's own composed sentence, drawn verbatim; a kind whose
 * name says the whole thing carries none, and the line is then the kind alone
 * rather than a dangling separator.
 */
export function drawFooterStatusActivityFault(
  u: FooterStatusActivityFault,
): HTMLElement {
  const kind = u.kind.replace(/_/g, " ");
  if (u.detail === "") return textLine("footer-activity-fault", kind);
  return textLine("footer-activity-fault", `${kind} · ${u.detail}`);
}

/**
 * A DEPLOY'S PROGRESS: the phase, then what it acts on, then what the deploy
 * left for later on this workspace — "building · shim, webapp, daemon",
 * "waiting · 1 turn, 2 background · shim when idle". A FINISHED deploy is not
 * a phase of this standing line but the transient `updated` kind
 * (`drawFooterActivityTransientUpdated`), which ends in the same notes.
 *
 * Every word is an arm NAME, rendered lowercase with spaces by the same rule
 * the status cells follow (footer.proto); the daemon composes no sentence for
 * this line and this end adds only the separators and the count nouns. The
 * counts are the line's typed figures, so they wear the figure colour.
 */
export function drawFooterStatusActivityUpdate(
  u: FooterStatusActivityUpdate,
  path: string,
): HTMLElement {
  const line = document.createElement("span");
  line.className = "footer-activity-update";
  const phase = requireCase(u.phase, `${path}.phase`);
  line.setAttribute("data-phase", phase.case);
  line.appendChild(document.createTextNode(statusWords(phase.case)));
  switch (phase.case) {
    case "building":
      appendComponents(
        line,
        phase.value.components,
        `${path}.building.components`,
      );
      break;
    case "restartingServices":
      appendComponents(
        line,
        phase.value.services,
        `${path}.restarting_services.services`,
      );
      break;
    case "waiting":
      appendWaitingCounts(line, phase.value);
      break;
    case "installing":
    case "handingOver":
      break;
    default: {
      const other: { case: string } = phase;
      return unreachableArm(`${path}.phase`, other.case);
    }
  }
  appendUpdateNotes(line, u.notes, path);
  return line;
}

/**
 * " · shim when idle" — what a deploy left for later on this workspace, by arm
 * name. The standing deploy line and the finished deploy's transient both end
 * in it.
 */
function appendUpdateNotes(
  line: HTMLElement,
  notes: readonly FooterStatusActivityUpdateNote[],
  path: string,
): void {
  notes.forEach((note, i) => {
    const arm = requireCase(note.note, `${path}.notes[${i}].note`);
    line.appendChild(document.createTextNode(` · ${statusWords(arm.case)}`));
  });
}

/** " · shim, webapp" — the components a phase acts on, by arm name. */
function appendComponents(
  line: HTMLElement,
  components: FooterStatusActivityUpdateComponent[],
  path: string,
): void {
  if (components.length === 0) return;
  const names = components.map((c, i) =>
    statusWords(requireCase(c.component, `${path}[${i}].component`).case),
  );
  line.appendChild(document.createTextNode(` · ${names.join(", ")}`));
}

/** " · 1 turn, 2 background" — what the workspace's move waits on. */
function appendWaitingCounts(
  line: HTMLElement,
  waiting: FooterStatusActivityUpdateWaiting,
): void {
  const figures: [number, string, string][] = [
    [waiting.turns, "turn", "turns"],
    [waiting.background, "background", "background"],
  ];
  let first = true;
  for (const [count, one, many] of figures) {
    if (count === 0) continue;
    line.appendChild(document.createTextNode(first ? " · " : ", "));
    first = false;
    const figure = document.createElement("span");
    figure.className = activityDatumClass("count");
    figure.setAttribute("data-datum", "count");
    figure.textContent = String(count);
    line.appendChild(figure);
    line.appendChild(document.createTextNode(` ${count === 1 ? one : many}`));
  }
}

/** The close-blocked reasons, verbatim. */
export function drawFooterStatusActivityCloseBlocked(
  u: FooterStatusActivityCloseBlocked,
): HTMLElement {
  return textLine("footer-activity-close-blocked", u.text);
}

/** The auth prompt line, verbatim. */
export function drawFooterStatusActivityAuthenticating(
  u: FooterStatusActivityAuthenticating,
): HTMLElement {
  return textLine("footer-activity-authenticating", u.line);
}

/**
 * The vendor call being retried: "attempt 9 of 11 · next try in 12s ·
 * overloaded". The attempt is the request's own count, the first retry its
 * second attempt, and the limit the vendor's own, drawn only when it stated
 * one. It read "retry #2" for the first retry until 2026-10-21: an attempt
 * number under a retry's name.
 *
 * THE NEXT ATTEMPT IS SHIPPED AS AN INSTANT and counts down here on the shared
 * ticker. The line is always a countdown (owner ruling, 2026-10-20): once the
 * instant passes with no newer attempt pushed, it holds at "next try in 0s"
 * rather than counting up how late the vendor is. Agent-repl's own retry
 * reads "retrying now" in its place from the moment it is made.
 */
export function drawFooterStatusActivityRetrying(
  u: FooterStatusActivityRetrying,
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  const line = document.createElement("span");
  line.className = "footer-activity-retrying";
  line.appendChild(document.createTextNode("attempt "));
  const attempt = document.createElement("span");
  attempt.className = activityDatumClass("attempt");
  attempt.setAttribute("data-datum", "attempt");
  attempt.textContent = String(u.attempt);
  line.appendChild(attempt);
  if (u.maxAttempt !== undefined) {
    line.appendChild(document.createTextNode(` of ${u.maxAttempt}`));
  }
  // AGENT-REPL'S OWN RETRY BEING MADE says so, stated where the retry is made
  // (owner ruling, 2026-10-21); it carries no countdown.
  if (u.now) {
    line.appendChild(document.createTextNode(" · retrying now"));
  } else if (u.nextAttempt !== undefined) {
    line.appendChild(document.createTextNode(" · "));
    line.appendChild(
      drawNextAttempt(u.nextAttempt, deps, `${path}.next_attempt`),
    );
  }
  line.appendChild(document.createTextNode(` · ${u.status}`));
  return line;
}

/**
 * "next try in 12s" ticking down to the vendor's promised instant at second
 * resolution, holding at "next try in 0s" once it has passed.
 */
function drawNextAttempt(
  u: FooterStatusActivityAt,
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  return footerCountdownSpan(
    deps.ctx.ticker,
    "footer-next-attempt",
    "next try in ",
    msOf(u.atMs, `${path}.at_ms`),
  );
}

/**
 * The wakeup countdown: "wakes in 4m 12s", plus the agent's reason when it gave
 * one. A FUTURE deadline is shipped and the remaining duration is drawn, so the
 * span ticks down through the shared clock.
 */
export function drawFooterStatusActivityWakeup(
  u: FooterStatusActivityWakeup,
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  const line = document.createElement("span");
  line.className = "footer-activity-wakeup";
  line.appendChild(
    footerCountdownSpan(
      deps.ctx.ticker,
      undefined,
      "wakes in ",
      msOf(u.wakeAtMs, `${path}.wake_at_ms`),
    ),
  );
  if (u.reason !== undefined) {
    line.appendChild(document.createTextNode(` · ${u.reason.text}`));
  }
  return line;
}

/**
 * One allowance: "session 72% · resets in 1h 5m".
 *
 * `utilization` is 0..1 on the wire and a PERCENTAGE on screen — display
 * formatting of one shipped figure, not a derivation of a second one — and
 * `resets_at_s` is the vendor's own SECONDS, converted once here.
 *
 * ONLY THE PERCENTAGE IS COLORED (owner ruling, 2026-09-30), by how full it
 * is (`drawFooterPercent`); the label and the reset countdown are plain text.
 * The vendor's verdict rides as `data-arm` and its sentence as the title.
 *
 * AN UNSET STATUS IS LEGAL AND MEANS "NO VERDICT YET": the figures are sampled
 * from the account's usage before the vendor's own rate-limit event arrives,
 * so an allowance with no arm draws untitled, and gains the title and the
 * `data-arm` on the push that carries one.
 *
 * A FIGURE IS TRUE ONLY UNTIL ITS RESET (footer.proto `resets_at_s`). The
 * figures are the last ones read, possibly before a daemon restart, so once
 * the clock passes the reset the allowance draws "session reset since last
 * seen" with NO percentage and no verdict, marked `data-lapsed`, until a newer
 * push carries a fresh figure.
 */
export function drawFooterAllowance(
  u: FooterAllowance,
  label: string,
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  return drawAllowanceCell(u, label, [], deps, path);
}

/**
 * The weekly element: "weekly 40% (overall) (12% opus 30% sonnet) · resets
 * in 3d" (owner ruling, 2026-10-23).
 *
 * The account-wide window draws exactly as any allowance does, and is
 * labelled "(overall)" only when the per-model windows stand beside it; with
 * none it reads "weekly 40% · resets in 3d". The per-model windows follow in
 * parentheses, each its own percentage, verdict and lapse
 * (`drawWeeklyModels`); the countdown is the overall window's. With no
 * overall figure the element reads "weekly (12% opus 30% sonnet)", with no
 * countdown.
 *
 * WHEN THE OVERALL WINDOW LAPSES, THE WHOLE ELEMENT DOES: the per-model
 * windows leave with the overall percentage, since a new seven-day window has
 * begun and the next push carries its figures.
 */
export function drawFooterWeeklyAllowance(
  u: FooterWeeklyAllowance,
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  const models =
    u.models === undefined ? null : drawWeeklyModels(u.models, deps, `${path}.models`);
  if (u.overall === undefined) {
    if (models === null) {
      throw new MalformedView(path, "a weekly allowance carries neither an overall nor a per-model window");
    }
    const span = document.createElement("span");
    span.className = "footer-allowance";
    span.setAttribute("data-allowance", "weekly");
    span.append(document.createTextNode("weekly "), models);
    return span;
  }
  const between: Node[] = [];
  if (models !== null) {
    const overallMark = document.createElement("span");
    overallMark.className = "footer-weekly-overall";
    overallMark.textContent = WEEKLY_OVERALL_TEXT;
    between.push(overallMark, document.createTextNode(" "), models);
  }
  return drawAllowanceCell(u.overall, "weekly", between, deps, `${path}.overall`);
}

/** What marks the account-wide weekly figure when per-model ones stand beside it. */
export const WEEKLY_OVERALL_TEXT = " (overall)";

/**
 * The per-model weekly windows in parentheses: "(12% opus 30% sonnet)", in
 * the fixed order opus, sonnet. A model window whose reset has passed reads
 * "opus reset" with no percentage and no verdict, on its own clock.
 */
function drawWeeklyModels(
  u: FooterWeeklyModelAllowances,
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  const present: [string, FooterAllowance][] = [];
  if (u.opus !== undefined) present.push(["opus", u.opus]);
  if (u.sonnet !== undefined) present.push(["sonnet", u.sonnet]);
  if (present.length === 0) {
    throw new MalformedView(path, "a weekly models element carries no model window");
  }
  const group = document.createElement("span");
  group.className = "footer-weekly-models";
  group.append("(");
  present.forEach(([model, allowance], index) => {
    if (index > 0) group.append(" ");
    group.appendChild(drawWeeklyModel(allowance, model, deps, `${path}.${model}`));
  });
  group.append(")");
  return group;
}

/** One per-model weekly window: "12% opus", lapsing to "opus reset". */
function drawWeeklyModel(
  u: FooterAllowance,
  model: string,
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  const span = document.createElement("span");
  span.setAttribute("data-weekly-model", model);
  markAllowanceVerdict(span, u, "footer-weekly-model", `weekly ${model}`, path);
  const percent = drawFooterPercent(u.utilization);
  const name = document.createTextNode(` ${model}`);
  span.append(percent, name);
  const resetsAtMs = msOf(u.resetsAtS, `${path}.resets_at_s`) * 1000;
  span.appendChild(
    footerClockSpan(deps.ctx.ticker, "countdown", undefined, (countdown, nowMs) => {
      if (resetsAtMs - nowMs > 0) return;
      if (span.hasAttribute("data-lapsed")) return;
      span.setAttribute("data-lapsed", "true");
      percent.remove();
      name.remove();
      unmarkAllowanceVerdict(span, "footer-weekly-model");
      countdown.textContent = `${model} reset`;
    }),
  );
  return span;
}

/**
 * Mark an allowance's cell with what its figure says beyond the percentage:
 * the vendor's verdict as `data-arm`, its class and its sentence as the
 * title, and newsworthiness. The one place a drawn allowance — a whole cell
 * or a per-model weekly figure — wears its verdict.
 */
function markAllowanceVerdict(
  span: HTMLElement,
  u: FooterAllowance,
  baseClass: string,
  label: string,
  path: string,
): void {
  const status = u.status.case === undefined ? null : u.status;
  span.className = status === null ? baseClass : `${baseClass} arm-${status.case}`;
  if (status !== null) {
    span.setAttribute("data-arm", status.case);
    span.title = allowanceStatusTitle(status, `${path}.status`);
  }
  if (u.newsworthy) span.setAttribute("data-newsworthy", "true");
  if (u.newsworthy) span.classList.add("footer-allowance-newsworthy");
  log.debug(
    `drawing the ${label} allowance as ${status?.case ?? "unverdicted"}`,
    {
      operation: "footer.strip.allowance",
      context: {
        allowance: label,
        status: status?.case,
        newsworthy: u.newsworthy,
      },
    },
  );
}

/** Take back everything `markAllowanceVerdict` put on a cell, for a lapse. */
function unmarkAllowanceVerdict(span: HTMLElement, baseClass: string): void {
  span.className = baseClass;
  span.removeAttribute("data-arm");
  span.removeAttribute("data-newsworthy");
  span.removeAttribute("title");
}

/**
 * One allowance cell: LABEL, the percentage, `between` (the weekly element's
 * "(overall)" and per-model windows, empty for any other), then the reset
 * countdown. Its lapse removes all but the countdown's sentence.
 */
function drawAllowanceCell(
  u: FooterAllowance,
  label: string,
  between: Node[],
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  const span = document.createElement("span");
  span.setAttribute("data-allowance", label);
  markAllowanceVerdict(span, u, "footer-allowance", label, path);

  const labelNode = document.createTextNode(`${label} `);
  const percent = drawFooterPercent(u.utilization);
  span.append(labelNode, percent, ...between);
  const resetsAtMs = msOf(u.resetsAtS, `${path}.resets_at_s`) * 1000;
  span.appendChild(
    footerClockSpan(deps.ctx.ticker, "countdown", undefined, (countdown, nowMs) => {
      const remaining = resetsAtMs - nowMs;
      if (remaining > 0) {
        countdown.textContent = ` · resets in ${formatCountdown(remaining)}`;
        return;
      }
      lapseAllowance(span, [labelNode, percent, ...between]);
      countdown.textContent = `${label} reset since last seen`;
    }),
  );
  return span;
}

/**
 * Turn a drawn allowance whose reset has passed into the lapsed one: the
 * percentage and the verdict describe a window that no longer exists, so both
 * leave (with `drawn`, everything drawn before the countdown), and only the
 * reset sentence the countdown draws stays.
 */
function lapseAllowance(span: HTMLElement, drawn: Node[]): void {
  if (span.hasAttribute("data-lapsed")) return;
  span.setAttribute("data-lapsed", "true");
  for (const node of drawn) node.parentNode?.removeChild(node);
  unmarkAllowanceVerdict(span, "footer-allowance");
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
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  const atMs = msOf(u.atMs, `${path}.at_ms`);
  return footerClockSpan(
    deps.ctx.ticker,
    "age",
    "footer-activity-age",
    (span, nowMs) => {
      span.textContent = ` · ${formatTickedAge(nowMs - atMs)} ago`;
    },
  );
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

/**
 * A countdown to ATMS reading LABEL then the time remaining ("next try in
 * 12s"), ticking down on the shared clock and holding at `0s` once the instant
 * passes. Every footer countdown at second resolution is built here, so none
 * of them counts up past its instant.
 */
export function footerCountdownSpan(
  ticker: Ticker,
  className: string | undefined,
  label: string,
  atMs: number,
): HTMLElement {
  return footerClockSpan(ticker, "countdown", className, (span, nowMs) => {
    span.textContent = `${label}${remainingLabel(atMs - nowMs)}`;
  });
}
