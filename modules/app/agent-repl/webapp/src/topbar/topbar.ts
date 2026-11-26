/**
 * The topbar: one thin strip, redrawn whole on every push.
 *
 * LEFT TIGHT, RIGHT TIGHT, AND THE TITLE IN WHAT IS BETWEEN THEM — the
 * status run (the queue-bypass chip on a personal account, the remote-control
 * chip, the persistent-wifi chip, then the production-checks chip in a
 * repository that checks its production) at the left edge, then the account cell
 * (its connectivity glyph, then its label). The model selector, the effort
 * selector, the permission-mode picker and the context chip sit at the right
 * edge.
 *
 * THE STATUS CHIPS RIDE THE LEFT (owner requests, 2026-11-09 and
 * 2026-11-10): the right group was crowded and the left nearly empty, so the
 * centered heading read as off-center; the status chips open the strip.
 *
 * THE TOPBAR DRAWS NO WARNINGS (owner ruling, 2026-11-25). Neither heading
 * line ever changes with what is wrong: the summary is always the summary's
 * purple and the name always the foreground white.
 *
 * THE RULE IS EQUAL CLEAR SPACE, NOT A SHARED MIDPOINT (owner ruling,
 * 2026-09-14, superseding the "true center" rule of 2026-09-13). The row is a
 * three-track grid whose flanks are `auto` — each sized to the group it
 * holds — and whose middle track is `minmax(0, 1fr)`, so the title gets
 * exactly the space between the two groups and `text-align: center` centers
 * the text inside it. The whitespace on either side of the visible text is
 * then the same measure by construction, whatever the groups measure.
 *
 * ONE LAYER, NOT TWO. The grid now knows what the flanks measure, so there is
 * no measured cap on top of it: a `1fr` track takes only what the two `auto`
 * flanks left, the title can never reach a group, and a long one ellipsizes
 * symmetrically inside the middle track. The stylesheet is the whole layout of
 * record and no JS participates in it.
 *
 * EVERY FIELD OF THE VIEW IS AN ELEMENT MESSAGE, so reading `drawTopbarView`
 * enumerates the topbar's subcomponents and each one's props are its own
 * message. Nothing rides bare and nothing is derived here: the glyph, the
 * tooltip, the tone name, the title, its description and the figure are all
 * composed daemon-side.
 *
 * WHAT SURVIVES A PUSH IS THE REVEAL, and only by name. The strip itself is
 * replaced entirely — that is the whole-view-push contract — while the reveal
 * layer is a sibling that outlives it, and each control re-registers its reveal
 * as it is drawn so a reader with the model list open sees the NEW list rather
 * than the one from the push before.
 */
import { WatchTopbarResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_topbar_pb";
import type { TopbarView } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { stopTicking } from "../feed/ticking.js";
import { createLogger } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { requireMessage } from "../rpc/strict.js";
import { watchStream, type StreamHandle } from "../rpc/streams.js";
import { drawTopbarContextChip } from "./context-chip.js";
import type { TopbarContext } from "./context.js";
import { drawTopbarEffortSelector } from "./effort.js";
import { drawTopbarModelSelector } from "./model.js";
import { drawTopbarPermissionModePicker } from "./permission-mode.js";
import { drawTopbarPersistentWifi } from "./persistent-wifi.js";
import { drawTopbarProductionChecks } from "./production-checks.js";
import { drawTopbarRemoteControl } from "./remote-control.js";
import { drawTopbarQueueBypass } from "./queue-bypass.js";
import { bindAccountReveal } from "./account.js";
import type { FocusLeft } from "../focus-left.js";
import { mountRevealLayer, type RevealGeometry } from "./reveal.js";
import { bindAgentReplSessionReveal } from "./session.js";
import {
  bindHeadingSessionReveal,
  drawTopbarAccount,
  drawTopbarConnectivity,
  drawTopbarHeading,
} from "./strip.js";

const log = createLogger("webapp.topbar.topbar");

/** What every mount answers with. */
export interface Handle {
  dispose(): void;
}

export interface TopbarDeps {
  /** The page's focus-left signal, which closes the open reveal. */
  readonly focusLeft: FocusLeft;
  /** Injected by tests, where jsdom reports every rect as zero. */
  geometry?: RevealGeometry;
}

export interface TopbarWatchDeps {
  /**
   * Raise the login overlay — the account chip's click, which hands its own
   * control, or the daemon's request for a sign-in, which hands none.
   */
  readonly openLogin: (control?: HTMLElement) => void;
}

/**
 * THE LOGIN OPENS BY ITSELF when the daemon asks for it
 * (`TopbarAccount.sign_in_request`: the account must sign in again). It answers
 * the request id to open the login for, or null: no request stands, or this
 * page already opened the login for that request, so a later push of the same
 * strip does not reopen a login the reader closed.
 */
export function signInRequestToOpen(view: TopbarView, opened: string | null): string | null {
  const id = view.account?.signInRequest?.requestId;
  if (id === undefined || id === "" || id === opened) return null;
  return id;
}

export interface TopbarHandle extends Handle {
  /**
   * Open the topbar's stream and draw its pushes. Called ONCE, after
   * adoption: a joining daemon refuses every per-workspace rpc until then.
   */
  watch(ctx: AppContext, deps: TopbarWatchDeps): void;
}

/**
 * Mount the topbar on HOST, empty until `watch` hands it a stream.
 *
 * The stream is STANDING: it never concludes on its own, so `dispose()` is the
 * only thing that closes it.
 */
export function mountTopbar(host: HTMLElement, deps: TopbarDeps): TopbarHandle {
  // AT INFO, LIKE feed.mount. A mount is a lifecycle edge a person asks
  // about — "did the topbar ever come up in this webview" is the first
  // question a blank strip raises, and it was only answerable at DEBUG.
  log.info("mounting the topbar", { operation: "topbar.mount" });

  const strip = document.createElement("div");
  strip.className = "topbar-strip";
  strip.setAttribute("data-topbar-strip", "");
  host.replaceChildren(strip);

  // Mounted AFTER the strip so the layer is the later sibling and paints over
  // it; both live inside the host, which is the reveal's positioning context.
  const reveals = mountRevealLayer(host, deps.focusLeft, deps.geometry);

  let stream: StreamHandle | null = null;

  /** Draw ROW (or nothing) as the whole strip, keeping the open reveal. */
  const show = (row: HTMLElement | null): void => {
    // The old strip's clock subscriptions come down BEFORE the new one goes
    // up, so nothing ticks against an element already detached.
    stopTicking(strip);
    strip.replaceChildren(...(row === null ? [] : [row]));
    // The controls re-registered their reveals as they were drawn; whatever
    // the reader had open re-opens against the NEW anchors and content.
    reveals.refresh();
  };

  return {
    watch(ctx: AppContext, watchDeps: TopbarWatchDeps): void {
      if (stream !== null) {
        log.error("the topbar was asked to watch twice", { operation: "topbar.watch-twice" });
        throw new Error("the topbar was asked to watch twice");
      }
      log.info("watching the topbar", { operation: "topbar.watch" });
      const watching: TopbarContext = { reveals, ctx, openLogin: watchDeps.openLogin };
      /** The sign-in request this page last opened the login for. */
      let openedSignIn: string | null = null;
      stream = watchStream(ctx, {
        name: "WatchTopbar",
        schema: WatchTopbarResponseSchema,
        open: (_client, signal) =>
          ctx.streams.watch("topbar", { workspace: ctx.workspace }, signal),
        onPush: (response) => {
          const next = requireMessage(response.topbar, "WatchTopbarResponse.topbar");
          show(drawTopbarView(next, watching));
          const request = signInRequestToOpen(next, openedSignIn);
          if (request !== null) {
            openedSignIn = request;
            log.info("the daemon asked for a sign-in; opening the login", {
              operation: "topbar.sign-in-requested",
              context: { request_id: request },
            });
            watchDeps.openLogin();
          }
        },
      });
    },

    dispose(): void {
      log.debug("disposing the topbar", { operation: "topbar.dispose" });
      stream?.cancel();
      reveals.dispose();
      stopTicking(strip);
      host.replaceChildren();
    },
  };
}

/**
 * One run of chips: the right group, or the left group's status run. Every
 * run carries `topbar-chips`, the one class the stylesheet spaces chips by, so
 * a chip stands the same distance from its neighbors whichever run holds it.
 */
export function drawChipRun(className: string, chips: readonly HTMLElement[]): HTMLElement {
  const run = document.createElement("div");
  run.className = `${className} topbar-chips`;
  run.append(...chips);
  return run;
}

/** The whole strip, in its three groups. */
export function drawTopbarView(u: TopbarView, tc: TopbarContext): HTMLElement {
  log.debug("drawing the topbar view", { operation: "topbar.draw" });

  const row = document.createElement("div");
  row.className = "topbar-row";

  const left = document.createElement("div");
  left.className = "topbar-left";
  // THE GLYPH AND THE LABEL ARE ONE CELL, in that order (owner ruling 3,
  // 2026-09-13): the connectivity mark is the ACCOUNT'S status indicator, so
  // it reads to the left of the label it qualifies and closer to it than to
  // anything else in the strip. One element also means one reveal anchor —
  // the options hang under the PAIR, not under half of it.
  const accountCell = document.createElement("div");
  accountCell.className = "topbar-account-cell";
  const accountView = requireMessage(u.account, "TopbarView.account");
  const connectivity = requireMessage(u.connectivity, "TopbarView.connectivity");
  const glyph = drawTopbarConnectivity(connectivity);
  // THE GLYPH OPENS AGENT-REPL'S SESSION (owner ruling, 2026-10-06), when the
  // daemon states one; the rest of the cell keeps the login options.
  bindAgentReplSessionReveal(glyph, connectivity.session, tc);
  accountCell.append(glyph, drawTopbarAccount(accountView));
  // THE CELL'S CLICK IS THE LOGIN OPTIONS, in both arms (owner ruling,
  // 2026-09-13). The session line rides the HEADING, which carries it whatever
  // the account state.
  bindAccountReveal(accountCell, accountView, tc);
  // THE STATUS RUN OPENS THE STRIP, LEFT OF THE ACCOUNT (owner request,
  // 2026-11-10): the remote-control chip, IMMEDIATELY LEFT of the
  // persistent-wifi chip (owner ruling, 2026-10-31), on every strip alike.
  const status = drawChipRun("topbar-status", [
    // THE QUEUE-BYPASS CHIP OPENS THE STRIP, on a personal account only.
    ...(u.queueBypass !== undefined ? [drawTopbarQueueBypass(u.queueBypass, tc)] : []),
    drawTopbarRemoteControl(requireMessage(u.remoteControl, "TopbarView.remoteControl"), tc),
    drawTopbarPersistentWifi(requireMessage(u.persistentWifi, "TopbarView.persistentWifi"), tc),
    // THE PRODUCTION-CHECKS CHIP CLOSES THE RUN, right of the wifi chip, in
    // a repository that checks its production only.
    ...(u.productionChecks !== undefined ? [drawTopbarProductionChecks(u.productionChecks, tc)] : []),
  ]);
  left.append(status, accountCell);

  const center = drawTopbarHeading(
    u.title,
    requireMessage(u.workspaceName, "TopbarView.workspaceName"),
    tc,
  );
  // The heading opens the session line too, in every account state.
  bindHeadingSessionReveal(center, u.sessionLine, tc);

  // THE STRIP HAS ONE SHAPE AND ONLY ONE (topbar.proto, FIXED SCHEMA AND
  // ORGANIZATION; owner ruling 2026-09-13). There is no branch here: every
  // cell is drawn on every push and in the same slot, whether or not the
  // workspace has a session. A control the daemon left ABSENT states so in
  // its own slot — each draw function answers with `no-session.ts`'s dash —
  // and the context chip carries the session-less facts itself, its hover
  // leading with the reason.
  const right = drawChipRun("topbar-right", [
    drawTopbarModelSelector(u.modelSelector, tc),
    drawTopbarEffortSelector(u.effortSelector, tc),
    drawTopbarPermissionModePicker(u.permissionModePicker, tc),
    drawTopbarContextChip(requireMessage(u.context, "TopbarView.context"), tc),
  ]);

  row.append(left, center, right);
  return row;
}
