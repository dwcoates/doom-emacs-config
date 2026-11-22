/**
 * lifecycle — what this page does when the DAEMON UNDER IT changes.
 *
 * Two standing streams, and neither of them draws a component:
 *
 *   - `WatchWebWorkspace` is this webview's own per-workspace link. It carries
 *     two pushes: `transferred{address}` says this daemon has released the
 *     workspace to a successor, and `session_identity` names the session this
 *     page's forwarded log records belong to.
 *   - `WatchDaemon` is daemon-scoped: the standing drain schedule, the
 *     graceful-rollout shutdown announcement, and the news digest standing
 *     (handed to the overlay that draws it). R3 — every webview holds its
 *     OWN subscription, rather than learning about a restart second-hand.
 *
 * THE PAGE MOVES ITSELF (owner ruling, 2026-11-22). Every daemon serves one
 * shared address, and the daemon a page's calls reach forwards each workspace
 * call and relays each page subscription to the daemon serving the workspace,
 * so no handover reloads the page. `transferred` — and the `transferring_away`
 * refusal arm, which is the same fact arriving as an answer — makes the page
 * adopt its workspace in place (`adoptInPlace`, the boot's own adoption loop):
 * on a daemon serving this page's webapp build it re-homes every stream
 * (`AppContext.rehome`), and on one serving another it draws the standing
 * "workspace moved" notice and quiesces, because that daemon's frames carry
 * fields this build has no descriptor for; the deploy's one host reload then
 * brings the page onto that build.
 *
 * ADOPTION RUNS AT BOOT, before any view stream opens (`adoptAtBoot`), and
 * again in place at every handover. `no_transfer_announced` is the ORDINARY
 * answer for a page that booted without a handover, so it is a debug line and
 * never an alarm; `not_yet_adopted` is the successor still finishing its
 * rendezvous and is retried with backoff; every other arm is a boot failure,
 * because a page that cannot be adopted cannot honestly show a workspace.
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
  type WebWorkspacePage,
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
import type { NewsDigestHandle } from "../news-digest/news-digest.js";
import { formatElapsed } from "../duration.js";
import { bindLogContext, log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { isMalformedView, MalformedView } from "../rpc/malformed.js";
import { registerWorkspaceMoved } from "../rpc/moved.js";
import { msOf, requireCase, requireMessage, unreachableArm, unreachablePushArm } from "../rpc/strict.js";
import type { RevealRelay } from "./reveal-relay.js";
import type { PromptEcho } from "../prompt-echo.js";
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
  /** The news digest overlay the daemon stream's `news_digest` standing is drawn on. */
  newsDigest: Pick<NewsDigestHandle, "apply">;
  /** Where the web stream's `reveal_row` asks go until the feed reveals them. */
  reveals: Pick<RevealRelay, "request">;
  /** Where the web stream's `prompt_echo` submit instants go (prompt-echo.ts). */
  promptEcho: Pick<PromptEcho, "echoed">;
  /**
   * The sidebar rail, whose title pulses for as long as a deploy is in
   * flight (`data-deploying`, styles.css). A deploy draws no banner.
   */
  deployPulseHost: HTMLElement;
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

  const banner = mountBanner(deps.drainBannerHost, ctx, (deploying) => {
    deps.deployPulseHost.toggleAttribute("data-deploying", deploying);
  });

  // THE PAGE MOVES ITSELF (owner ruling, 2026-11-22): no handover reloads
  // it. `moving` holds while one move is being answered, so the push and every
  // refusal that says the same thing start one adoption between them.
  let moving = false;
  const goQuiet = (address: string): void => {
    // THE TERMINAL STATE. The notice goes up first, so the reader has the
    // successor's address in front of them before every stream on the page
    // stops; quiescing after it means nothing can redraw over it.
    banner.showMoved(address);
    ctx.quiesce();
  };
  const onMoved = (address: string): void => {
    if (moving) {
      log.debug(`the workspace moved to ${address}; this page is already moving`, {
        operation: "lifecycle.transferred-again",
        context: { address },
      });
      return;
    }
    moving = true;
    log.info(`the workspace moved to ${address}; adopting it in place`, {
      operation: "lifecycle.transferred",
      context: { address },
    });
    void adoptInPlace(ctx, webappBuild).then(
      (outcome) => {
        moving = false;
        if (outcome === "rehomed") {
          ctx.rehome();
          return;
        }
        log.info(`the daemon serving this workspace runs another webapp; going quiet until the host reloads`, {
          operation: "lifecycle.transferred-other-build",
          context: { address },
        });
        goQuiet(address);
      },
      (err: unknown) => {
        // `adoptAtBoot` recorded and reported the refusal it threw; a page
        // whose workspace could not be adopted cannot honestly show it.
        moving = false;
        log.error(`this page could not adopt its workspace in place: ${String(err)}`, {
          operation: "lifecycle.adopt-in-place-failed",
          context: { address },
        });
        goQuiet(address);
      },
    );
  };
  const unregisterMoved = registerWorkspaceMoved(onMoved);

  // THE LINK ANSWERING AGAIN IS WHAT ENDS AN OUTAGE, not the countdown, and
  // any stream coming back is that answer: a bounce takes them all down
  // together, so the first one to read again is the daemon being back. A no-op
  // while no restart notice stands.
  //
  // IT IS A STREAM COMING BACK, NEVER MERELY A FRAME. The announcing daemon
  // goes on pushing on its standing streams until it exits -- the drain's own
  // lease change republishes views -- and a notice cleared by any frame was
  // taken down by the very daemon that announced it (webapp layer, roster:
  // "the drain banner was never drawn", the host still reading
  // drain-scheduled).
  const unsubscribeFromRestored = ctx.onLinkRestored(() => banner.clearRestarting());

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
        case "revealRow":
          deps.reveals.request(requireMessage(push.value.row, "WebWorkspaceRevealRow.row"));
          return;
        case "promptEcho":
          deps.promptEcho.echoed(
            requireMessage(push.value.turn, "WebWorkspacePromptEcho.turn").value,
            Number(push.value.submittedAtUnixMs),
          );
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
    open: (_client, signal) =>
      ctx.streams.watch("daemon", { client: { case: "webview", value: {} } }, signal),
    plannedEnding: (response) => response.push.case === "ending",
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
        case "newsDigest":
          deps.newsDigest.apply(push.value);
          return;
        default: {
          const other: { case: string } = push;
          // A TOP-LEVEL push arm this build cannot draw is forward-compat skew,
          // not a contract violation: skipped quietly by the stream pipeline.
          return unreachablePushArm("WatchDaemonResponse.push", other.case);
        }
      }
    },
  });

  return {
    dispose(): void {
      log.debug("disposing the page lifecycle", { operation: "lifecycle.dispose" });
      unregisterMoved();
      unsubscribeFromRestored();
      webLink.cancel();
      daemon.cancel();
      banner.dispose();
    },
  };
}

/**
 * Read the announcement: size the quiet window, mute the expected failure, and
 * draw the restarting notice — unless the announcement is a DEPLOY'S HANDOVER.
 *
 * WITH `address` SET this is a handover, and the per-workspace `transferred`
 * push is what actually moves the page — so only the notice is drawn here.
 * WITHOUT it this is a plain bounce: the streams will die, the rpc core's own
 * reconnect handles it, and the notice clears on the first push after.
 *
 * A DEPLOY'S HANDOVER DRAWS NO BANNER (owner request, 2026-09-27): its
 * progress is the footer's update line, which the daemon publishes on every
 * workspace's strip and the successor finishes with `updated`. The expected
 * `daemonUnreachable` is still muted for the announced window, because the
 * link does drop at the transfer. Every other announcement — an unplanned
 * restart, a scheduled drain firing, a plain bounce — keeps the banner.
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
  if (isDeployHandover(announced)) {
    log.info("a deploy's handover: its progress is on the footer, so no banner is drawn", {
      operation: "lifecycle.deploy-handover",
      context: { address: announced.address ?? "" },
    });
    // No notice is drawn, but the sidebar title's pulse stands until the page moves.
    banner.showRestarting(announced, nowMs + quietMs);
    return;
  }
  banner.showRestarting(announced, nowMs + quietMs);
}

/**
 * Whether an announcement is a DEPLOY'S HANDOVER: the rollout cause with a
 * successor's address. The daemon hands over only for a deploy, and the same
 * cause WITHOUT an address is a plain bounce (a layout restart), which keeps
 * its banner. A causeless announcement is malformed and refused here.
 */
export function isDeployHandover(announced: DaemonShutdownAnnounced): boolean {
  const cause = requireMessage(announced.cause, "DaemonShutdownAnnounced.cause");
  const arm = requireCase(cause.kind, "DaemonShutdownCause.kind");
  return arm.case === "selfMergeRollout" && announced.address !== undefined;
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
export function mountBanner(
  host: HTMLElement,
  ctx: AppContext,
  onDeploying: (deploying: boolean) => void = () => {},
): BannerHandle {
  let moved: string | null = null;
  let restarting: { announced: DaemonShutdownAnnounced; untilMs: number } | null = null;
  let drain: DaemonDrainScheduled | null = null;
  /** The ticker subscription the CURRENT notice owns. */
  let untick: (() => void) | null = null;

  const redraw = (): void => {
    untick?.();
    untick = null;
    // A DEPLOY DRAWS NO BANNER: the sidebar title's pulse says it instead, for
    // exactly as long as the deploy's restart or drain stands. `moved` is
    // terminal and the page is going away, so the pulse stops with it.
    const deploying =
      moved === null &&
      ((restarting !== null && isDeployShutdown(restarting.announced)) ||
        (drain !== null && isDeployDrain(drain)));
    onDeploying(deploying);
    if (moved !== null) {
      host.replaceChildren(drawMovedNotice(moved));
      return;
    }
    if (restarting !== null && !isDeployShutdown(restarting.announced)) {
      const { element, tick } = drawRestartingNotice(restarting.announced, restarting.untilMs);
      host.replaceChildren(element);
      tick(ctx.ticker.now());
      untick = ctx.ticker.subscribe(tick);
      return;
    }
    if (drain !== null && !isDeployDrain(drain)) {
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

/** Whether a standing drain schedule is a deploy's. */
export function isDeployDrain(scheduled: DaemonDrainScheduled): boolean {
  const reason = requireMessage(scheduled.reason, "DaemonDrainScheduled.reason");
  return requireCase(reason.kind, "DaemonDrainScheduled.reason.kind").case === "deploy";
}

/**
 * Whether an announced shutdown is a deploy's: a rollout handing over to a
 * successor (the same cause with no address is a plain bounce), or a drain or
 * immediate stop for a deploy.
 */
export function isDeployShutdown(announced: DaemonShutdownAnnounced): boolean {
  const cause = requireMessage(announced.cause, "DaemonShutdownAnnounced.cause");
  const arm = requireCase(cause.kind, "DaemonShutdownCause.kind");
  switch (arm.case) {
    case "selfMergeRollout":
      return announced.address !== undefined;
    case "scheduledDrain":
    case "immediate": {
      const reason = requireMessage(arm.value.reason, `DaemonShutdown.${arm.case}.reason`);
      return requireCase(reason.kind, `DaemonShutdown.${arm.case}.reason.kind`).case === "deploy";
    }
    default: {
      const other: { case: string } = arm;
      return unreachableArm("DaemonShutdownCause.kind", other.case);
    }
  }
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
): Promise<AdoptedPage> {
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
      const page = webPage(result.value.page, "AdoptWebWorkspaceSuccess.page");
      const webappBuild = result.value.webappBuild;
      if (webappBuild === "") {
        throw new MalformedView("AdoptWebWorkspaceSuccess.webapp_build", "REQUIRED and empty");
      }
      log.info("this workspace was adopted", { operation: "lifecycle.adopted", context: { page, webapp_build: webappBuild } });
      return { outcome: "adopted", page, webappBuild };
    }
    if (result.case !== "error") {
      const other: { case: string } = result;
      return unreachableArm("AdoptWebWorkspaceResponse.result", other.case);
    }
    const outcome = classifyAdoptionRefusal(result.value);
    if (outcome.kind === "no-transfer") {
      log.debug("no transfer was announced for this workspace; a plain boot", {
        operation: "lifecycle.adopt-no-transfer",
        context: { page: outcome.page },
      });
      return { outcome: "no-transfer", page: outcome.page };
    }
    if (outcome.kind === "deferred") {
      // THE INCUMBENT KEPT THIS WORKSPACE until it is free, and serves it
      // meanwhile, so the page goes on to mount its body there. Nothing here
      // is wrong and nothing will change by waiting, so nothing is retried.
      log.info("the incumbent kept this workspace until it is free; this page goes on there", {
        operation: "lifecycle.adopt-deferred",
        context: { page: outcome.page },
      });
      return { outcome: "deferred", page: outcome.page };
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

/**
 * Answer a handover IN PLACE: adopt the workspace where it is served now,
 * through the one adoption loop the boot runs, and say whether the page can
 * reopen its subscriptions there.
 *
 * `rehomed` is every answer that leaves the workspace showable on this build:
 * an adoption by a daemon serving BUILD, the incumbent keeping the workspace
 * (`deferred`), or no transfer left to join (the rendezvous completed without
 * this page). `other-build` is an adoption by a daemon serving another
 * webapp, whose frames this page has no descriptor for: the page goes quiet
 * and the deploy's host reload brings it onto that build. A refusal throws, as
 * the boot's does.
 */
export async function adoptInPlace(
  ctx: AppContext,
  build: string,
  backoff: AdoptBackoff = {},
): Promise<"rehomed" | "other-build"> {
  const adopted = await adoptAtBoot(ctx, backoff);
  if (adopted.outcome !== "adopted") return "rehomed";
  return adopted.webappBuild === build ? "rehomed" : "other-build";
}

/** Which body a workspace page mounts below its topbar: a feed, or a chat. */
export type WebPage = "agent" | "thread";

/** A page that goes on to mount its body, and which body that is. */
export type AdoptedPage =
  | {
      outcome: "adopted";
      page: WebPage;
      /** The webapp build the adopting daemon serves (AdoptWebWorkspaceSuccess.webapp_build). */
      webappBuild: string;
    }
  | { outcome: "no-transfer" | "deferred"; page: WebPage };

/** The page the daemon named (WebWorkspacePage), as the boot switches on it. */
export function webPage(page: WebWorkspacePage | undefined, path: string): WebPage {
  const kind = requireCase(requireMessage(page, path).kind, `${path}.kind`);
  switch (kind.case) {
    case "agent":
      return "agent";
    case "thread":
      return "thread";
    default: {
      const other: { case: string } = kind;
      return unreachableArm(`${path}.kind`, other.case);
    }
  }
}

/** What the adoption loop does about one refusal. */
export type AdoptionOutcome =
  | { kind: "no-transfer"; page: WebPage }
  | { kind: "deferred"; page: WebPage }
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
      return { kind: "no-transfer", page: webPage(arm.value.page, "AdoptWebWorkspaceNoTransferAnnounced.page") };
    case "notYetAdopted":
      return { kind: "retry", arm: arm.case };
    // The successor refused a mid-work adoption and the incumbent took the
    // workspace back: not a rendezvous under way, so never retried.
    case "transferDeferred":
      return { kind: "deferred", page: webPage(arm.value.page, "AdoptWebWorkspaceTransferDeferred.page") };
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
    // The call reached the daemon that handed the workspace away before it
    // forwards there: the successor is still taking the workspace up, so the
    // adoption waits for it rather than giving up.
    case "transferringAway":
      return { kind: "retry", arm: arm.case };
    // THIS PAGE IS NOT PART OF THE TRANSFER: it was not open when the handover
    // was announced (Emacs loaded it mid-handover, re-pointing it at the
    // successor). Nothing is wrong with it; it waits for the rendezvous the
    // transfer's own participants complete, and the successor then answers
    // its adoption at once. Failing here left the page dead with a boot
    // failure in its chip (2026-10-25, ship-gns).
    case "participantNotExpected":
      return { kind: "retry", arm: arm.case };
    default: {
      const other: { case: string } = arm;
      return unreachableArm("AdoptWebWorkspaceError.cause", other.case);
    }
  }
}
