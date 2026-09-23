/**
 * lifecycle — what this page does when the DAEMON UNDER IT changes.
 *
 * Two standing streams, and neither of them draws a component:
 *
 *   - `WatchWebWorkspace` is this webview's own per-workspace link. It carries
 *     two pushes: `transferred{address}` says this daemon has released the
 *     workspace to a successor, and `session_identity` names the session this
 *     page's forwarded log records belong to.
 *   - `WatchDaemon` is daemon-scoped: the standing drain schedule and the
 *     graceful-rollout shutdown announcement. R3 — every webview holds its
 *     OWN subscription, rather than learning about a restart second-hand.
 *
 * THE WEBAPP DOES NOT REDIAL (project lead, final). A successor on a different
 * loopback port is a DIFFERENT ORIGIN, so this page could not become a client
 * of it even if it wanted to: re-pointing the webview is Emacs's job, and it
 * does it by reloading the view at the new address. So `transferred` — and the
 * `transferring_away` refusal arm, which is the same fact arriving as an
 * answer — draws a standing "workspace moved to <address>" notice, quiesces the
 * context, and stops. No transport to a new address is created anywhere in this
 * module, and nothing retries: retrying is exactly the behavior that would keep
 * a released workspace's page talking to a daemon that has already let it go.
 *
 * ADOPTION HAPPENS ONCE, AT BOOT, ON THE FRESH PAGE — `adoptAtBoot`, before any
 * view stream opens. `no_transfer_announced` is the ORDINARY answer for a page
 * that booted without a handover, so it is a debug line and never an alarm;
 * `not_yet_adopted` is the successor still finishing its rendezvous and is
 * retried with backoff; every other arm is a boot failure, because a page that
 * cannot be adopted cannot honestly show a workspace.
 *
 * THE BANNER IS ONE HOST WITH THREE STANDING NOTICES, in precedence order:
 * moved (terminal — nothing follows it), restarting (an announced outage), and
 * the scheduled drain. They share a host because they are the same sentence to
 * the reader — "this page's daemon is going away" — at three distances, and two
 * of them stacked would read as two separate restarts.
 *
 * THE PAGE'S LOG IDENTITY COMES FROM THE LINK STREAM (landing 15). A browser
 * has no durable sink, so every diagnostic this page raises is written by the
 * daemon into webapp.log — and without a session identity on it, that record
 * could be joined to a workspace and no further, never to the session the page
 * was showing when the fault happened. `session_identity` arrives on the
 * stream's OPENING push and again on every edge that rotates the session, and
 * each one rebinds the logger's context, so a restart's records are filed under
 * the restart rather than under the session it replaced.
 *
 * CLOCKS TICK CLIENT-SIDE: the wire ships `at_ms` and `minted_at_ms`, and the
 * countdowns here are the shared ticker's, never a `setInterval` of this
 * module's own.
 */
import { ConnectError } from "@connectrpc/connect";
import {
  AdoptWebWorkspaceResponseSchema,
  type AdoptWebWorkspaceError,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_adopt_web_workspace_pb";
import {
  WatchDaemonResponseSchema,
  type DaemonDrainScheduled,
  type DaemonShutdownAnnounced,
  type DaemonShutdownCause,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_daemon_pb";
import {
  WatchWebWorkspaceResponseSchema,
  type WebWorkspaceSessionIdentity,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_web_workspace_pb";
import type { DrainReason } from "../../../proto/gen/ts/agentrepl/v1/drain_reason_pb";
import { controlPlaneFailed } from "../failure/sink.js";
import { formatElapsed } from "../duration.js";
import { bindLogContext, log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { isMalformedView } from "../rpc/malformed.js";
import { registerWorkspaceMoved } from "../rpc/moved.js";
import { msOf, requireCase, requireMessage, unreachableArm, unreachablePushArm } from "../rpc/strict.js";
import { watchStream } from "../rpc/streams.js";
import { callUnary } from "../rpc/unary.js";
import { readWebappBuild } from "../webapp-build.js";

/** What every mount answers with. */
export interface Handle {
  dispose(): void;
}

export interface LifecycleDeps {
  /** The page-wide banner host (`[data-component="drain-banner"]`). */
  drainBannerHost: HTMLElement;
}

/** The adoption retry shape, injected by tests running on fake timers. */
export interface AdoptBackoff {
  /** The first wait after `not_yet_adopted`. */
  initialMs?: number;
  /** The ceiling the doubling stops at. */
  maxMs?: number;
  /** How long the whole retry may run before the boot fails. */
  budgetMs?: number;
}

/** 250 ms doubling to 5 s, given up after about a minute. */
export const ADOPT_INITIAL_MS = 250;
export const ADOPT_MAX_MS = 5000;
export const ADOPT_BUDGET_MS = 60_000;

// ---------------------------------------------------------------------------
// THE MOVE NOTICE, reachable from a refusal as well as from the push.
//
// `transferring_away{address}` can come back at ANY control, and it means
// exactly what the `transferred` push means. Rather than teach every call site
// how to draw a page-wide notice, the lifecycle REGISTERS the one handler with
// `src/rpc/moved.ts` and a refusal site raises it by name. The registry lives
// down in the rpc layer rather than here so the shared refusal hook can reach
// it without the rpc layer importing a component.
// ---------------------------------------------------------------------------

export { workspaceMoved } from "../rpc/moved.js";

/**
 * Bind the session the page's forwarded log records belong to.
 *
 * BOTH IDENTITIES ARE BOUND WHATEVER THEY HOLD, the empty string included: an
 * empty one is the daemon saying there is no session (or no vendor
 * conversation) right now, and the logger drops an empty identity rather than
 * stamping it — so binding it is how a page that HAD an identity stops
 * claiming the retired one. Remembering the old value instead is the one
 * outcome that misattributes records.
 */
export function bindSessionIdentity(identity: WebWorkspaceSessionIdentity): void {
  bindLogContext({
    agent_repl_session_id: identity.agentReplSessionId,
    claude_session_id: identity.claudeSessionId,
  });
  log.info("bound the page's session identity", {
    operation: "lifecycle.session_identity",
    context: { has_session: identity.agentReplSessionId !== "" },
  });
}

/**
 * Start the page's lifecycle. Returns the handle that stops both streams.
 */
export function startLifecycle(ctx: AppContext, deps: LifecycleDeps): Handle {
  log.debug("starting the page lifecycle", { operation: "lifecycle.start" });

  // READ ONCE PER PAGE LOAD. The build cannot change under a running page —
  // it is the entry bundle THIS page is executing — so it is read exactly
  // once here and reused on every `WatchWebWorkspace` open the stream retries
  // into, rather than re-parsed from the DOM on every reconnect.
  const webappBuild = readWebappBuild();

  const banner = mountBanner(deps.drainBannerHost, ctx);

  const onMoved = (address: string): void => {
    // THE TERMINAL STATE. The notice goes up first, so the reader has the
    // successor's address in front of them before every stream on the page
    // stops; quiescing after it means nothing can redraw over it.
    log.info(`the workspace moved to ${address}; going quiet`, {
      operation: "lifecycle.transferred",
      context: { address },
    });
    banner.showMoved(address);
    ctx.quiesce();
  };
  const unregisterMoved = registerWorkspaceMoved(onMoved);

  // THE LINK ANSWERING AGAIN IS WHAT ENDS AN OUTAGE, not the countdown, and
  // any stream's frame is that answer: a bounce takes them all down together,
  // so the first one back is the daemon being back. A no-op while no restart
  // notice stands.
  const unsubscribeFromPushes = ctx.onPush(() => banner.clearRestarting());

  const webLink = watchStream(ctx, {
    name: "WatchWebWorkspace",
    schema: WatchWebWorkspaceResponseSchema,
    open: (_client, signal) =>
      ctx.streams.watch("webWorkspace", { workspace: ctx.workspace, webappBuild }, signal),
    onPush: (response) => {
      const push = requireCase(response.push, "WatchWebWorkspaceResponse.push");
      switch (push.case) {
        case "transferred":
          onMoved(push.value.address);
          return;
        case "sessionIdentity":
          bindSessionIdentity(push.value);
          return;
        default: {
          const other: { case: string } = push;
          // A TOP-LEVEL push arm this build cannot draw is forward-compat skew,
          // not a contract violation: skipped quietly by the stream pipeline.
          return unreachablePushArm("WatchWebWorkspaceResponse.push", other.case);
        }
      }
    },
  });

  const daemon = watchStream(ctx, {
    name: "WatchDaemon",
    schema: WatchDaemonResponseSchema,
    open: (_client, signal) => ctx.streams.watch("daemon", {}, signal),
    onPush: (response) => {
      const push = requireCase(response.push, "WatchDaemonResponse.push");
      switch (push.case) {
        case "drainScheduled":
          banner.showDrain(push.value);
          return;
        case "drainCancelled":
          banner.clearDrain();
          return;
        case "shutdownAnnounced":
          announceShutdown(ctx, banner, push.value);
          return;
        default: {
          const other: { case: string } = push;
          // A TOP-LEVEL push arm this build cannot draw is forward-compat skew,
          // not a contract violation: skipped quietly by the stream pipeline.
          return unreachablePushArm("WatchDaemonResponse.push", other.case);
        }
      }
    },
    // THE LINK COMING BACK IS WHAT ENDS AN OUTAGE, not the countdown: a plain
    // bounce's banner clears when this daemon answers again.
    onReconnected: () => banner.clearRestarting(),
  });

  return {
    dispose(): void {
      log.debug("disposing the page lifecycle", { operation: "lifecycle.dispose" });
      unregisterMoved();
      unsubscribeFromPushes();
      webLink.cancel();
      daemon.cancel();
      banner.dispose();
    },
  };
}

/**
 * Read the announcement: size the quiet window, mute the expected failure, and
 * draw the restarting notice.
 *
 * WITH `address` SET this is a handover, and the per-workspace `transferred`
 * push is what actually moves the page — so only the notice is drawn here.
 * WITHOUT it this is a plain bounce: the streams will die, the rpc core's own
 * reconnect handles it, and the notice clears on the first push after.
 */
export function announceShutdown(
  ctx: AppContext,
  banner: BannerHandle,
  announced: DaemonShutdownAnnounced,
): void {
  const nowMs = ctx.ticker.now();
  const quietMs = quietWindowMs(announced, nowMs);
  log.info("the daemon announced a shutdown", {
    operation: "lifecycle.shutdown-announced",
    context: {
      handover: announced.address !== undefined,
      quiet_window_ms: quietMs,
      cause: announced.cause?.kind.case ?? "unset",
    },
  });
  // The link dying inside the window is the announcement coming true, so the
  // card is muted for exactly as long as the daemon said and no longer.
  if (ctx.failures.suppress !== undefined) {
    ctx.failures.suppress("daemonUnreachable", nowMs + quietMs);
  } else {
    log.warn("this page's failure sink cannot suppress; the outage will draw a card", {
      operation: "lifecycle.suppress-unavailable",
    });
  }
  banner.showRestarting(announced, nowMs + quietMs);
}

/**
 * How long the outage is still expected to last, at NOWMS.
 *
 * A LATE RECEIVER SHORTENS ITS WINDOW rather than restarting it: the daemon
 * stamped when it minted the announcement, so a page that read it four seconds
 * later has four seconds less to wait, and a window that has already elapsed is
 * zero rather than a negative that would mute nothing (or, worse, everything).
 */
export function quietWindowMs(announced: DaemonShutdownAnnounced, nowMs: number): number {
  const outage = msOf(announced.expectedOutageMs, "DaemonShutdownAnnounced.expected_outage_ms");
  const minted = msOf(announced.mintedAtMs, "DaemonShutdownAnnounced.minted_at_ms");
  return Math.max(0, outage - (nowMs - minted));
}

// ---------------------------------------------------------------------------
// THE BANNER
// ---------------------------------------------------------------------------

export interface BannerHandle {
  /** The terminal notice: this workspace lives on ADDRESS now. */
  showMoved(address: string): void;
  /** An announced outage, expected to end at UNTILMS. */
  showRestarting(announced: DaemonShutdownAnnounced, untilMs: number): void;
  /** Take the restarting notice down (the daemon is answering again). */
  clearRestarting(): void;
  /** A standing drain schedule. */
  showDrain(scheduled: DaemonDrainScheduled): void;
  /** The schedule was cancelled. */
  clearDrain(): void;
  dispose(): void;
}

/**
 * The banner host, owning at most one notice.
 *
 * Precedence is fixed rather than last-write-wins: `moved` is terminal and
 * nothing may draw over it, and an announced restart is nearer than a schedule
 * that has not fired.
 */
export function mountBanner(host: HTMLElement, ctx: AppContext): BannerHandle {
  let moved: string | null = null;
  let restarting: { announced: DaemonShutdownAnnounced; untilMs: number } | null = null;
  let drain: DaemonDrainScheduled | null = null;
  /** The ticker subscription the CURRENT notice owns. */
  let untick: (() => void) | null = null;

  const redraw = (): void => {
    untick?.();
    untick = null;
    if (moved !== null) {
      host.replaceChildren(drawMovedNotice(moved));
      return;
    }
    if (restarting !== null) {
      const { element, tick } = drawRestartingNotice(restarting.announced, restarting.untilMs);
      host.replaceChildren(element);
      tick(ctx.ticker.now());
      untick = ctx.ticker.subscribe(tick);
      return;
    }
    if (drain !== null) {
      const { element, tick } = drawDrainNotice(drain);
      host.replaceChildren(element);
      tick(ctx.ticker.now());
      untick = ctx.ticker.subscribe(tick);
      return;
    }
    host.replaceChildren();
  };

  redraw();

  return {
    showMoved(address: string): void {
      moved = address;
      redraw();
    },
    showRestarting(announced: DaemonShutdownAnnounced, untilMs: number): void {
      restarting = { announced, untilMs };
      redraw();
    },
    clearRestarting(): void {
      if (restarting === null) return;
      log.info("the daemon is answering again; taking the restart notice down", {
        operation: "lifecycle.restart-over",
      });
      restarting = null;
      redraw();
    },
    showDrain(scheduled: DaemonDrainScheduled): void {
      drain = scheduled;
      redraw();
    },
    clearDrain(): void {
      if (drain === null) return;
      log.info("the drain schedule was cancelled; taking the banner down", {
        operation: "lifecycle.drain-cancelled",
      });
      drain = null;
      redraw();
    },
    dispose(): void {
      untick?.();
      untick = null;
      moved = null;
      restarting = null;
      drain = null;
      host.replaceChildren();
    },
  };
}

/** The base element every notice wears, so the three read as one register. */
function notice(): HTMLElement {
  const element = document.createElement("div");
  element.className = "lifecycle-banner";
  element.setAttribute("role", "status");
  // Every notice in this host says a restart is in play, near or far, so the
  // integration suite's one marker covers all three.
  element.setAttribute("data-restarting", "");
  return element;
}

/**
 * "workspace moved to 127.0.0.1:8123" — the terminal notice.
 *
 * The address is drawn because it is the ONE fact the reader may need: if the
 * host does not reload the view, this is what tells them (or whoever debugs it)
 * where the workspace actually went.
 */
export function drawMovedNotice(address: string): HTMLElement {
  const element = notice();
  element.classList.add("lifecycle-moved");
  element.setAttribute("data-moved", address);
  element.textContent = `workspace moved to ${address}`;
  return element;
}

/**
 * "daemon restarting · rollout · expected back in 8s", ticking to UNTILMS.
 *
 * Past the window it reads "any moment now" rather than counting up: the
 * daemon's estimate has run out, which says nothing new about when it returns.
 */
export function drawRestartingNotice(
  announced: DaemonShutdownAnnounced,
  untilMs: number,
): { element: HTMLElement; tick: (nowMs: number) => void } {
  const cause = requireMessage(announced.cause, "DaemonShutdownAnnounced.cause");
  const armCase = requireCase(cause.kind, "DaemonShutdownCause.kind").case;
  const element = notice();
  element.classList.add("lifecycle-restarting");
  element.setAttribute("data-shutdown-cause", armCase);
  const words = shutdownCauseText(cause);
  const tick = (nowMs: number): void => {
    const remaining = untilMs - nowMs;
    element.textContent =
      remaining > 0
        ? `daemon restarting · ${words} · expected back in ${formatElapsed(remaining)}`
        : `daemon restarting · ${words} · any moment now`;
  };
  return { element, tick };
}

/**
 * "daemon restart scheduled · deploy · in 4m 12s", ticking to `at_ms`.
 */
export function drawDrainNotice(scheduled: DaemonDrainScheduled): {
  element: HTMLElement;
  tick: (nowMs: number) => void;
} {
  const reason = requireMessage(scheduled.reason, "DaemonDrainScheduled.reason");
  const atMs = msOf(scheduled.atMs, "DaemonDrainScheduled.at_ms");
  const element = notice();
  element.classList.add("lifecycle-drain");
  element.setAttribute("data-drain-scheduled", "");
  const words = drainReasonText(reason, "DaemonDrainScheduled.reason");
  // THE ARM RIDES THE REASON, drawn as its own element: the words are the
  // daemon's sentence, and the arm is the fact behind them.
  const lead = document.createElement("span");
  lead.className = "lifecycle-banner-lead";
  const reasonElement = document.createElement("span");
  reasonElement.className = "lifecycle-banner-reason";
  reasonElement.setAttribute(
    "data-arm",
    requireCase(reason.kind, "DaemonDrainScheduled.reason.kind").case,
  );
  reasonElement.textContent = words;
  const when = document.createElement("span");
  when.className = "lifecycle-banner-when";
  lead.textContent = "daemon restart scheduled · ";
  element.append(lead, reasonElement, when);
  const tick = (nowMs: number): void => {
    const remaining = atMs - nowMs;
    when.textContent =
      remaining > 0 ? ` · in ${formatElapsed(remaining)}` : " · any moment now";
  };
  return { element, tick };
}

/**
 * What a shutdown cause says in the banner.
 *
 * The two scheduled arms carry a `DrainReason` and say what IT says, so a
 * deploy-triggered drain and a deploy-triggered immediate shutdown read the
 * same way — the reader cares which reason, not which verb armed it.
 */
export function shutdownCauseText(cause: DaemonShutdownCause): string {
  const arm = requireCase(cause.kind, "DaemonShutdownCause.kind");
  switch (arm.case) {
    case "selfMergeRollout":
      return "rollout";
    case "scheduledDrain":
      return drainReasonText(
        requireMessage(arm.value.reason, "DaemonShutdownScheduledDrain.reason"),
        "DaemonShutdownScheduledDrain.reason",
      );
    case "immediate":
      return drainReasonText(
        requireMessage(arm.value.reason, "DaemonShutdownImmediate.reason"),
        "DaemonShutdownImmediate.reason",
      );
    default: {
      const other: { case: string } = arm;
      return unreachableArm("DaemonShutdownCause.kind", other.case);
    }
  }
}

/**
 * What a drain reason says.
 *
 * The operator arm's note is drawn VERBATIM — it is the one arm with a human's
 * own words in it, and the proto requires it non-blank, so there is nothing to
 * fall back to and nothing to compose.
 */
export function drainReasonText(reason: DrainReason, path: string): string {
  const arm = requireCase(reason.kind, `${path}.kind`);
  switch (arm.case) {
    case "deploy":
      return "deploy";
    case "maintenance":
      return "maintenance";
    case "operator":
      return arm.value.note;
    default: {
      const other: { case: string } = arm;
      return unreachableArm(`${path}.kind`, other.case);
    }
  }
}

// ---------------------------------------------------------------------------
// ADOPTION AT BOOT
// ---------------------------------------------------------------------------

/** Why a boot could not proceed. main.ts mints `boot_failed` from it. */
export class AdoptionFailed extends Error {
  /** The refusal arm, or "transport". */
  readonly arm: string;
  constructor(arm: string, detail: string) {
    super(`AdoptWebWorkspace refused (${arm}): ${detail}`);
    this.name = "AdoptionFailed";
    this.arm = arm;
  }
}

/**
 * Adopt this workspace on THIS daemon, once, before any view stream opens.
 *
 * WHY BEFORE ANYTHING ELSE. A daemon that has just joined refuses every
 * per-workspace rpc with `not_yet_adopted` until the rendezvous completes, so a
 * page that opened its streams first would spend the whole rendezvous filing
 * refusals for calls that were simply early. Adopting first turns that race
 * into a wait.
 *
 * `no_transfer_announced` IS THE ORDINARY ANSWER — most boots are not
 * handovers — so it resolves quietly at debug and is never an alarm.
 */
export async function adoptAtBoot(
  ctx: AppContext,
  backoff: AdoptBackoff = {},
): Promise<"adopted" | "no-transfer"> {
  const initialMs = backoff.initialMs ?? ADOPT_INITIAL_MS;
  const maxMs = backoff.maxMs ?? ADOPT_MAX_MS;
  const budgetMs = backoff.budgetMs ?? ADOPT_BUDGET_MS;
  const startedAt = ctx.ticker.now();
  let waitMs = initialMs;

  log.debug("adopting this workspace on the daemon this page booted against", {
    operation: "lifecycle.adopt-at-boot",
  });

  for (;;) {
    let response;
    try {
      response = await callUnary(
        ctx,
        "AdoptWebWorkspace",
        (client) => client.adoptWebWorkspace({ workspace: ctx.workspace }),
        AdoptWebWorkspaceResponseSchema,
      );
    } catch (err) {
      // A MALFORMED ANSWER IS NOT A TRANSPORT FAILURE and must not be dressed
      // as one: the daemon answered, and what it said this build cannot read.
      // It propagates as itself so the boot names the real fault.
      if (isMalformedView(err)) throw err;
      // The transport failed at boot, which is not a rendezvous still running:
      // there is nothing to wait for, so the page fails where the fault is.
      const connectError = ConnectError.from(err);
      throw new AdoptionFailed("transport", connectError.rawMessage);
    }
    const result = requireCase(response.result, "AdoptWebWorkspaceResponse.result");
    if (result.case === "success") {
      log.info("this workspace was adopted", { operation: "lifecycle.adopted" });
      return "adopted";
    }
    if (result.case !== "error") {
      const other: { case: string } = result;
      return unreachableArm("AdoptWebWorkspaceResponse.result", other.case);
    }
    const outcome = classifyAdoptionRefusal(result.value);
    if (outcome.kind === "no-transfer") {
      log.debug("no transfer was announced for this workspace; a plain boot", {
        operation: "lifecycle.adopt-no-transfer",
      });
      return "no-transfer";
    }
    if (outcome.kind === "terminal") {
      log.error(`this page cannot be adopted: ${outcome.detail}`, {
        operation: "lifecycle.adopt-refused",
        context: { arm: outcome.arm },
      });
      ctx.failures.report(controlPlaneFailed("adopt web workspace", outcome.detail));
      throw new AdoptionFailed(outcome.arm, outcome.detail);
    }
    // RETRY: the successor is still finishing its rendezvous.
    if (ctx.ticker.now() - startedAt >= budgetMs) {
      const detail = `the daemon was still adopting after ${formatElapsed(budgetMs)}`;
      log.error(detail, { operation: "lifecycle.adopt-gave-up", context: { arm: outcome.arm } });
      ctx.failures.report(controlPlaneFailed("adopt web workspace", detail));
      throw new AdoptionFailed(outcome.arm, detail);
    }
    log.info(`the daemon is still adopting this workspace; retrying in ${waitMs} ms`, {
      operation: "lifecycle.adopt-retry",
      context: { arm: outcome.arm, backoff_ms: waitMs },
    });
    await new Promise<void>((resolve) => setTimeout(resolve, waitMs));
    waitMs = Math.min(waitMs * 2, maxMs);
  }
}

/** What the adoption loop does about one refusal. */
export type AdoptionOutcome =
  | { kind: "no-transfer" }
  | { kind: "retry"; arm: string }
  | { kind: "terminal"; arm: string; detail: string };

/**
 * Which arms wait and which give up.
 *
 * `not_yet_adopted` is the ONLY arm that describes a state that resolves on its
 * own — the successor is mid-rendezvous. Every other arm is a statement about
 * this page or this workspace that no amount of waiting changes, so retrying
 * one would be a spinner over a fixed fact.
 */
export function classifyAdoptionRefusal(error: AdoptWebWorkspaceError): AdoptionOutcome {
  const arm = requireCase(error.cause, "AdoptWebWorkspaceError.cause");
  switch (arm.case) {
    case "noTransferAnnounced":
      return { kind: "no-transfer" };
    case "notYetAdopted":
      return { kind: "retry", arm: arm.case };
    case "unknownWorkspace":
      return {
        kind: "terminal",
        arm: arm.case,
        detail: "the daemon's registry has no such workspace",
      };
    case "workspaceRefMismatch":
      return {
        kind: "terminal",
        arm: arm.case,
        detail: `the registry holds ${arm.value.registryDir} for this workspace id`,
      };
    case "transferringAway":
      return {
        kind: "terminal",
        arm: arm.case,
        detail: `this workspace is transferring to ${arm.value.address}`,
      };
    case "participantNotExpected":
      return {
        kind: "terminal",
        arm: arm.case,
        detail: "this page's stream was not open when the transfer was announced",
      };
    default: {
      const other: { case: string } = arm;
      return unreachableArm("AdoptWebWorkspaceError.cause", other.case);
    }
  }
}
