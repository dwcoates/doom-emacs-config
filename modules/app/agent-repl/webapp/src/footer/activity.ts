/**
 * activity — the strip's activity cell: the one elastic segment, and the one
 * place the footer's four activity TIERS are drawn.
 *
 * THE DAEMON RESOLVES THE TIER (footer.proto, the status family's rules): each
 * status arm's activity is a `oneof tier` holding EITHER that arm's salient
 * line OR the unpinned tiers beneath it: the transient over the enduring line,
 * and under `working` and `background` the quiet-stretch line between them.
 * The waiting arm has no unpinned branch at all, because a waiting session is
 * always parked on a salient line. This module walks what it was pushed and
 * ranks nothing.
 *
 * THE ONE DECISION THIS CLIENT TAKES is the clock comparison inside the
 * unpinned tiers: the transient is drawn while the clock is before its
 * `expiry.expires_at_ms`, and the quiet-stretch line (when one stands) or the
 * enduring line otherwise. A draw that picks
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
  FooterActivityEnduringContextWindow,
  FooterActivityEnduringUsage,
  FooterActivityQuietStretch,
  FooterActivityTransient,
  FooterActivityTransientCompactionConcluded,
  FooterActivityTransientContextInjected,
  FooterActivityTransientDaemonError,
  FooterActivityTransientDaemonWarning,
  FooterActivityTransientHook,
  FooterActivityTransientNetworkResume,
  FooterActivityTransientOverEnduring,
  FooterActivityTransientOverQuietOverEnduring,
  FooterActivityTransientSessionChange,
  FooterActivityTransientSubmitting,
  FooterActivityTransientSubmittingHeld,
  FooterActivityTransientTask,
  FooterActivityTransientToolCall,
  FooterActivityTransientUpdated,
  FooterAllowance,
  FooterStatusActivityAt,
  FooterStatusActivityAuthenticating,
  FooterStatusActivityBlockedOnUser,
  FooterStatusActivityCloseBlocked,
  FooterStatusActivityColdGateCost,
  FooterStatusActivityCompaction,
  FooterStatusActivityContextBudget,
  FooterStatusActivityFault,
  FooterStatusActivityGatedCall,
  FooterStatusActivityInterrupting,
  FooterStatusActivityMergingCommit,
  FooterStatusActivityNotification,
  FooterStatusActivityQueryDied,
  FooterStatusActivityQuestionLead,
  FooterStatusActivityRateLimit,
  FooterStatusActivityRateLimitWindow,
  FooterStatusActivityRetrying,
  FooterStatusActivityStartFailed,
  FooterStatusActivityUpdate,
  FooterStatusActivityUpdateComponent,
  FooterStatusActivityUpdateNote,
  FooterStatusActivityUpdateWaiting,
  FooterStatusActivityWakeup,
  FooterStatusBackgroundActivity,
  FooterStatusBackgroundSalient,
  FooterStatusBlockedActivity,
  FooterStatusBlockedSalient,
  FooterStatusClosingActivity,
  FooterStatusClosingSalient,
  FooterStatusDisconnectedActivity,
  FooterStatusDisconnectedSalient,
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
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { formatAge, formatCountdown, formatTickedAge } from "../duration.js";
import { tick } from "../feed/ticking.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import type { TransientExpirySchedule } from "./expiry.js";
import { grabber, statusWords, textLine } from "./parts.js";
import { activityDatumClass, allowanceStatusClass, footerPercentColor } from "./tones.js";

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
  | FooterStatusInterruptedActivity
  | FooterStatusMergingActivity
  | FooterStatusBackgroundActivity
  | FooterStatusBlockedActivity
  | FooterStatusDisconnectedActivity
  | FooterStatusClosingActivity
  | FooterStatusLoadingActivity;

/** Every per-status salient line, as one type to walk. */
export type FooterSalient =
  | FooterStatusIdleSalient
  | FooterStatusWorkingSalient
  | FooterStatusWaitingSalient
  | FooterStatusInterruptedSalient
  | FooterStatusMergingSalient
  | FooterStatusBackgroundSalient
  | FooterStatusBlockedSalient
  | FooterStatusDisconnectedSalient
  | FooterStatusClosingSalient
  | FooterStatusLoadingSalient;

/**
 * The unpinned tiers a cell carries: the quiet-stretch line rides only under
 * `working` and `background`.
 */
export type FooterUnpinned =
  | FooterActivityTransientOverEnduring
  | FooterActivityTransientOverQuietOverEnduring;

/** The tier the daemon resolved for a cell. */
export type ActivityTier =
  | { readonly case: "salient"; readonly value: FooterSalient }
  | { readonly case: "unpinned"; readonly value: FooterUnpinned };

/**
 * The tier a cell carries.
 *
 * THE WAITING CELL IS THE ONE WITH NO ONEOF: it holds its salient line
 * directly, so it is read as the salient tier by construction. Every other
 * cell's `tier` is switched exhaustively, and an unset one is malformed.
 */
export function activityTier(activity: FooterActivity, path: string): ActivityTier {
  if (activity.$typeName === "frontend.v1.FooterStatusWaitingActivity") {
    return { case: "salient", value: requireMessage(activity.salient, `${path}.salient`) };
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
  const line = requireMessage(tier.value.enduring, `${path}.unpinned.enduring`).line;
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
  readonly tier: "salient" | "transient" | "quiet" | "enduring";
  readonly arm: string;
  readonly line: HTMLElement;
  readonly age: HTMLElement | null;
}

/** The line a resolved tier draws. */
function drawTier(tier: ActivityTier, deps: ActivityDeps, path: string): DrawnLine {
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
 * instant, then the quiet-stretch line when one stands, then the enduring line.
 *
 * The transient is validated whether or not it is drawn, so a malformed one
 * is refused on the push that carried it rather than silently outlived. A
 * drawn transient schedules the one re-render at its expiry.
 */
function drawUnpinned(
  u: FooterUnpinned,
  deps: ActivityDeps,
  path: string,
): DrawnLine {
  const enduring = requireMessage(u.enduring, `${path}.enduring`);
  if (u.transient !== undefined) {
    const transientPath = `${path}.transient`;
    const transient = u.transient;
    const expiry = requireMessage(transient.expiry, `${transientPath}.expiry`);
    const expiresAtMs = msOf(expiry.expiresAtMs, `${transientPath}.expiry.expires_at_ms`);
    const at = requireMessage(transient.at, `${transientPath}.at`);
    const kind = requireCase(transient.kind, `${transientPath}.kind`);
    const nowMs = deps.ctx.ticker.now();
    if (nowMs < expiresAtMs) {
      log.debug(`the transient ${kind.case} is live; drawing it until it lapses`, {
        operation: "footer.strip.transient-live",
        context: { kind: kind.case, expires_at_ms: expiresAtMs, now_ms: nowMs },
      });
      deps.expiry.schedule(expiresAtMs);
      return {
        tier: "transient",
        arm: kind.case,
        line: drawFooterActivityTransient(transient, deps, transientPath),
        age: drawFooterStatusActivityAt(at, deps, `${transientPath}.at`),
      };
    }
    log.debug(`the transient ${kind.case} has lapsed; drawing the enduring line`, {
      operation: "footer.strip.transient-lapsed",
      context: { kind: kind.case, expires_at_ms: expiresAtMs, now_ms: nowMs },
    });
  }
  const quiet = quietStretchOf(u);
  if (quiet !== undefined) {
    return {
      tier: "quiet",
      arm: "quietStretch",
      line: drawFooterActivityQuietStretch(quiet),
      age: drawFooterStatusActivityAt(
        requireMessage(quiet.at, `${path}.quiet_stretch.at`),
        deps,
        `${path}.quiet_stretch.at`,
      ),
    };
  }
  return {
    tier: "enduring",
    arm: "enduring",
    line: drawFooterActivityEnduring(enduring, deps, `${path}.enduring`),
    age: null,
  };
}

/** The quiet-stretch line an unpinned container carries, if its status admits one. */
function quietStretchOf(u: FooterUnpinned): FooterActivityQuietStretch | undefined {
  return u.$typeName === "frontend.v1.FooterActivityTransientOverQuietOverEnduring"
    ? u.quietStretch
    : undefined;
}

/**
 * The quiet-stretch line, verbatim: what just landed in the feed and what the
 * turn does next, standing until the next feed item surfaces. The daemon
 * words it; this end adds no word of its own.
 */
export function drawFooterActivityQuietStretch(u: FooterActivityQuietStretch): HTMLElement {
  return textLine("footer-activity-quiet-stretch", u.text);
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
      return drawFooterStatusActivityRetrying(kind.value);
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
    case "mergingCommit":
      return drawFooterStatusActivityMergingCommit(kind.value);
    case "authenticating":
      return drawFooterStatusActivityAuthenticating(kind.value);
    case "fault":
      return drawFooterStatusActivityFault(kind.value);
    case "startFailed":
      return drawFooterStatusActivityStartFailed(kind.value);
    case "closeBlocked":
      return drawFooterStatusActivityCloseBlocked(kind.value);
    case "rateLimit":
      return drawFooterStatusActivityRateLimit(kind.value, deps, path);
    case "notification":
      return drawFooterStatusActivityNotification(kind.value);
    case "contextBudget":
      return drawFooterStatusActivityContextBudget(kind.value);
    default: {
      const other: { case: string } = kind;
      return unreachableArm(path, other.case);
    }
  }
}

/** The agent's push notification, verbatim. */
export function drawFooterStatusActivityNotification(
  u: FooterStatusActivityNotification,
): HTMLElement {
  return textLine("footer-activity-notification", u.text);
}

/** The context-budget warning, or a failed compaction's account, verbatim. */
export function drawFooterStatusActivityContextBudget(
  u: FooterStatusActivityContextBudget,
): HTMLElement {
  return textLine("footer-activity-context-budget", u.text);
}

/**
 * The vendor's rate-limit event: which allowance, what the vendor said, and
 * the figures it carried, "weekly nearly spent 85% · resets in 2h". The line
 * wears the allowance verdict's own colour, the enduring allowance cell's
 * palette, so a warning reads yellow and a refusal red wherever it appears.
 */
export function drawFooterStatusActivityRateLimit(
  u: FooterStatusActivityRateLimit,
  deps: AllowanceDeps,
  path: string,
): HTMLElement {
  const verdict = requireCase(u.verdict, `${path}.verdict`);
  const line = document.createElement("span");
  line.className = `footer-activity-rate-limit ${allowanceStatusClass(verdict.case)}`;
  line.setAttribute("data-arm", verdict.case);
  const window = u.window === undefined ? "usage" : rateLimitWindowWords(u.window, `${path}.window`);
  line.appendChild(document.createTextNode(`${window} ${rateLimitVerdictWords(verdict.case, `${path}.verdict`)}`));
  if (u.utilization !== undefined) {
    line.appendChild(document.createTextNode(" "));
    line.appendChild(drawFooterPercent(u.utilization));
  }
  if (u.resetsAtS !== undefined) {
    const resets = document.createElement("span");
    resets.setAttribute("data-countdown", "");
    const resetsAtMs = msOf(u.resetsAtS, `${path}.resets_at_s`) * 1000;
    tick(resets, deps.ctx.ticker, (nowMs) => {
      resets.textContent = ` · resets in ${formatCountdown(resetsAtMs - nowMs)}`;
    });
    line.appendChild(resets);
  }
  return line;
}

/** The allowance a rate-limit event names, lowercase with spaces. */
function rateLimitWindowWords(u: FooterStatusActivityRateLimitWindow, path: string): string {
  const window = requireCase(u.window, `${path}.window`);
  switch (window.case) {
    case "session":
    case "weekly":
    case "overage":
      return window.case;
    case "weeklyOpus":
      return "weekly opus";
    case "weeklySonnet":
      return "weekly sonnet";
    case "weeklyOverageIncluded":
      return "weekly overage included";
    default: {
      const other: { case: string } = window;
      return unreachableArm(`${path}.window`, other.case);
    }
  }
}

/** What the vendor's verdict says about the allowance. */
function rateLimitVerdictWords(verdict: string, path: string): string {
  switch (verdict) {
    case "allowedWarning":
      return "nearly spent";
    case "rejected":
      return "spent";
    default:
      return unreachableArm(path, verdict);
  }
}

// ---- the transient tier -----------------------------------------------------

/**
 * One transient line, with the subagent it came from in front when it came
 * from one: "Explore · Bash: npm test". The label is an identity, so it wears
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
    case "toolCall":
      return drawFooterActivityTransientToolCall(kind.value);
    case "task":
      return drawFooterActivityTransientTask(kind.value);
    case "submitting":
      return drawFooterActivityTransientSubmitting(kind.value, path);
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
    default: {
      const other: { case: string } = kind;
      return unreachableArm(path, other.case);
    }
  }
}

/**
 * A tool call starting: "Bash: npm test", the tool's name then the daemon's
 * gist — the gated call's shape — and the name alone when the call carried
 * nothing worth a line.
 */
export function drawFooterActivityTransientToolCall(
  u: FooterActivityTransientToolCall,
): HTMLElement {
  const text = u.summary === undefined ? u.tool : `${u.tool}: ${u.summary}`;
  return textLine("footer-activity-tool-call", text);
}

/**
 * The task tracker moving: the task's subject, then the tracker's progress
 * "3/7" as the figure it is — the tasks chip's own fraction.
 */
export function drawFooterActivityTransientTask(u: FooterActivityTransientTask): HTMLElement {
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

/**
 * A prompt's delivery moving: the stage it reached, then the prompt's first
 * line, "queued 2/3 · fix the flaky test". The stage leads, because it is what
 * changed and the line ellipsizes from its end.
 */
export function drawFooterActivityTransientSubmitting(
  u: FooterActivityTransientSubmitting,
  path: string,
): HTMLElement {
  const stage = requireCase(u.stage, `${path}.stage`);
  const line = document.createElement("span");
  line.className = "footer-activity-submitting";
  line.setAttribute("data-stage", stage.case);
  switch (stage.case) {
    case "held":
      appendHeldPlace(line, stage.value);
      break;
    case "classifying":
      line.appendChild(document.createTextNode("classifying"));
      break;
    case "interjecting":
      line.appendChild(document.createTextNode("interrupting the turn"));
      break;
    case "coalesced":
      line.appendChild(document.createTextNode("coalesced"));
      break;
    case "delivered":
      line.appendChild(document.createTextNode("sent"));
      break;
    default: {
      const other: { case: string } = stage;
      return unreachableArm(`${path}.stage`, other.case);
    }
  }
  line.appendChild(document.createTextNode(` · ${u.promptLead}`));
  return line;
}

/** "queued 2/3": the prompt's place in the queue, as the figure it is. */
function appendHeldPlace(line: HTMLElement, held: FooterActivityTransientSubmittingHeld): void {
  line.appendChild(document.createTextNode("queued "));
  const place = document.createElement("span");
  place.className = activityDatumClass("position");
  place.setAttribute("data-datum", "position");
  place.textContent = `${held.position}/${held.queued}`;
  line.appendChild(place);
}

/** The running hook's name. */
export function drawFooterActivityTransientHook(u: FooterActivityTransientHook): HTMLElement {
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
  return textLine("footer-activity-daemon-warning", `${u.operation} · ${u.message}`);
}

/** A daemon error that blocks nothing: the operation, then its message. */
export function drawFooterActivityTransientDaemonError(
  u: FooterActivityTransientDaemonError,
): HTMLElement {
  return textLine("footer-activity-daemon-error", `${u.operation} · ${u.message}`);
}

/**
 * A compaction's concluded outcome ("compacted and resumed (101.6k → 12.4k)"),
 * verbatim, in the running compaction line's own class, so the outcome reads
 * as that line's last word rather than a new kind of line.
 */
export function drawFooterActivityTransientCompactionConcluded(
  u: FooterActivityTransientCompactionConcluded,
): HTMLElement {
  return textLine("footer-activity-compaction", u.text);
}

/** A session setting's change, as the daemon composed it. */
export function drawFooterActivityTransientSessionChange(
  u: FooterActivityTransientSessionChange,
): HTMLElement {
  return textLine("footer-activity-session-change", u.text);
}

/**
 * A FINISHED DEPLOY: "updated", then what it left for later on this
 * workspace — the deploy line's own shape and hooks (`.footer-activity-update`,
 * `data-phase="updated"`), since it is that line's last word, now announced by
 * the successor as a transient.
 */
export function drawFooterActivityTransientUpdated(
  u: FooterActivityTransientUpdated,
  path: string,
): HTMLElement {
  const line = document.createElement("span");
  line.className = "footer-activity-update";
  line.setAttribute("data-phase", "updated");
  line.appendChild(document.createTextNode(statusWords("updated")));
  appendUpdateNotes(line, u.notes, path);
  return line;
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
    document.createTextNode(`${statusWords("networkResume")} · ${statusWords(edge.case)}`),
  );
  switch (edge.case) {
    case "waiting":
      line.appendChild(document.createTextNode(" · "));
      line.appendChild(
        drawGivesUpCountdown(edge.value.givesUpAtMs, deps, `${path}.waiting.gives_up_at_ms`),
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
  const countdown = document.createElement("span");
  countdown.className = "footer-gives-up";
  countdown.setAttribute("data-countdown", "");
  tick(countdown, deps.ctx.ticker, (nowMs) => {
    countdown.textContent = `gives up in ${formatCountdown(deadlineMs - nowMs)}`;
  });
  return countdown;
}

// ---- the enduring tier ------------------------------------------------------

/**
 * THE ENDURING LINE: the account's usage allowances with the age of their
 * reading, then how full the context window is — "session 41% · resets in
 * 2h 5m | weekly 12% · resets in 3d · 10m 30s ago | context 42%".
 *
 * Each half is drawn only when the daemon has observed it; a line with
 * neither is an empty line, which the grow cell still holds open.
 *
 * THE FIGURES ARE THE ELASTIC PART. The line lays itself out as the old rate
 * line did: an inline flex box no wider than the cell, the allowance figures
 * ellipsizing (newsworthy window first) and the read-age and the context fill
 * rigid beside them, so a narrow strip cuts the figures a reader can reopen in
 * the tokens sheet rather than the caveat about them.
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
      for (const part of drawFooterActivityEnduringUsage(chosen.value, deps, `${path}.usage`)) {
        line.appendChild(part);
      }
      break;
    case "contextWindow":
      line.appendChild(drawFooterActivityEnduringContextWindow(chosen.value));
      break;
    case "unobserved":
      break;
    default: {
      const other: { case: string } = chosen;
      return unreachableArm(`${path}.line`, other.case);
    }
  }
  return line;
}

/**
 * The usage half: EVERY window the vendor figured, newsworthy first, plus the
 * AGE of the last successful reading, "· 10m 30s ago".
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
      if (index > 0) figures.appendChild(document.createTextNode(" | "));
      figures.appendChild(
        drawFooterAllowance(allowance.value, allowance.label, deps, `${path}.${allowance.label}`),
      );
    });
    parts.push(figures);
  }
  const age = drawFiguresReadAge(u, deps, path);
  if (age !== null) parts.push(age);
  return parts;
}

/**
 * The context window's fill, "context 42%": the daemon's resolved fraction
 * drawn as a percentage, the one figure on it.
 */
export function drawFooterActivityEnduringContextWindow(
  u: FooterActivityEnduringContextWindow,
): HTMLElement {
  const span = document.createElement("span");
  span.className = "footer-context-window";
  span.appendChild(document.createTextNode("context "));
  span.appendChild(drawFooterPercent(u.fill));
  return span;
}

/**
 * A footer percentage, "42%": a 0..1 figure on the wire drawn as a whole
 * percent, colored by how full it is (`footerPercentColor`). Every percentage
 * the footer draws goes through here, so none is ever drawn unpainted.
 */
export function drawFooterPercent(fraction: number): HTMLElement {
  const figure = Math.round(fraction * 100);
  const percent = document.createElement("span");
  percent.className = "footer-percent";
  percent.setAttribute("data-datum", "percent");
  percent.textContent = `${String(figure)}%`;
  percent.style.color = footerPercentColor(figure);
  return percent;
}

/**
 * The age of the last successful usage READING, "· 10m 30s ago", ticking from
 * the shipped instant — the same live-duration mechanism the turn clock and
 * the activity age already use (`tick` + `formatTickedAge`), so how stale the
 * figures look never depends on push cadence.
 *
 * NULL when the figures carry no read instant: they came from a rate-limit
 * EVENT rather than a sample, or none was ever read, so there is no reading to
 * date. The line then draws the figures with no age rather than inventing one.
 * An unreadable sample that leaves the figures standing leaves this instant
 * standing too (the daemon never re-stamps it), so the age stays anchored to
 * the last read rather than jumping to the failed attempt.
 */
export function drawFiguresReadAge(
  u: FooterActivityEnduringUsage,
  deps: AllowanceDeps,
  path: string,
): HTMLElement | null {
  if (u.figuresReadAtMs === undefined) return null;
  const readAtMs = msOf(u.figuresReadAtMs, `${path}.figures_read_at_ms`);
  const age = document.createElement("span");
  age.className = "footer-rate-age";
  age.setAttribute("data-age", "");
  tick(age, deps.ctx.ticker, (nowMs) => {
    age.textContent = ` · ${formatTickedAge(nowMs - readAtMs)} ago`;
  });
  return age;
}

/** One drawable allowance, under the label the strip and the sheet both use. */
export interface LabelledAllowance {
  readonly label: string;
  readonly value: FooterAllowance;
}

/**
 * The windows the producer figured — session, weekly, overage — NEWSWORTHY
 * FIRST, stable within each group, so with nothing newsworthy the windows keep
 * the contract's own order and nothing moves under a reader for no reason.
 * The strip and the tokens sheet draw exactly this list.
 */
export function orderedAllowances(u: FooterActivityEnduringUsage): LabelledAllowance[] {
  const present: LabelledAllowance[] = [];
  if (u.session !== undefined) present.push({ label: "session", value: u.session });
  if (u.weekly !== undefined) present.push({ label: "weekly", value: u.weekly });
  if (u.overage !== undefined) present.push({ label: "overage", value: u.overage });
  return [
    ...present.filter((a) => a.value.newsworthy),
    ...present.filter((a) => !a.value.newsworthy),
  ];
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
  return textLine("footer-activity-compaction", u.text);
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

/**
 * The bring-up failure: the daemon's composed cause, and what the failure cost
 * in held prompts when it dropped any. The count is a fact of the failure the
 * daemon states as a number, so the line says it in words here rather than
 * leaving the user to learn it from an emptied tray.
 */
export function drawFooterStatusActivityStartFailed(
  u: FooterStatusActivityStartFailed,
): HTMLElement {
  if (u.droppedPrompts === 0) return textLine("footer-activity-start-failed", u.detail);
  const prompts = u.droppedPrompts === 1 ? "prompt" : "prompts";
  return textLine(
    "footer-activity-start-failed",
    `${u.detail} · ${u.droppedPrompts} held ${prompts} dropped`,
  );
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
export function drawFooterStatusActivityFault(u: FooterStatusActivityFault): HTMLElement {
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
      appendComponents(line, phase.value.components, `${path}.building.components`);
      break;
    case "restartingServices":
      appendComponents(line, phase.value.services, `${path}.restarting_services.services`);
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
  const names = components.map((c, i) => statusWords(requireCase(c.component, `${path}[${i}].component`).case));
  line.appendChild(document.createTextNode(` · ${names.join(", ")}`));
}

/** " · 1 turn, 2 background" — what the workspace's move waits on. */
function appendWaitingCounts(line: HTMLElement, waiting: FooterStatusActivityUpdateWaiting): void {
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
  deps: AllowanceDeps,
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
  deps: AllowanceDeps,
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
  log.debug(`drawing the ${label} allowance as ${status?.case ?? "unverdicted"}`, {
    operation: "footer.strip.allowance",
    context: { allowance: label, status: status?.case, newsworthy: u.newsworthy },
  });

  span.appendChild(document.createTextNode(`${label} `));
  span.appendChild(drawFooterPercent(u.utilization));

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
  deps: AllowanceDeps,
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
