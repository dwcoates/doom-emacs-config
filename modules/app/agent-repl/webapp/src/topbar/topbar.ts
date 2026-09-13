/**
 * The topbar: one thin strip, redrawn whole on every push.
 *
 * LEFT TIGHT, CENTER CONTENT-CENTERED, RIGHT TIGHT — the account cell (its
 * connectivity glyph, then its label) at the left edge, then the model
 * selector, the permission-mode picker, the context chip and the warning chip
 * at the right edge. The flank groups never spread, and the title between them
 * is centered on its OWN content against the whole strip rather than on what
 * the two groups leave over: the row is a three-track grid whose outer tracks
 * are equal, so a wide right group does not push the title left (styles.css,
 * owner ruling 2).
 *
 * EVERY FIELD OF THE VIEW IS AN ELEMENT MESSAGE, so reading `drawTopbarView`
 * enumerates the topbar's subcomponents and each one's props are its own
 * message. Nothing rides bare and nothing is derived here: the glyph, the
 * tooltip, the tone name, the title, the figure and every warning sentence are
 * all composed daemon-side.
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
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { requireMessage } from "../rpc/strict.js";
import { watchStream } from "../rpc/streams.js";
import { drawTopbarContextChip } from "./context-chip.js";
import type { TopbarContext } from "./context.js";
import { drawTopbarModelSelector } from "./model.js";
import { drawTopbarPermissionModePicker } from "./permission-mode.js";
import { drawTopbarFastMode } from "./fast-mode.js";
import { drawTopbarHibernated } from "./hibernated.js";
import { drawTopbarColdGate } from "./cold-gate.js";
import { bindAccountReveal } from "./account.js";
import { mountRevealLayer, type RevealGeometry } from "./reveal.js";
import {
  bindTitleSessionReveal,
  drawTopbarAccount,
  drawTopbarConnectivity,
  drawTopbarTitle,
} from "./strip.js";
import { drawTopbarWarningStrip } from "./warnings.js";

/** What every mount answers with. */
export interface Handle {
  dispose(): void;
}

export interface TopbarDeps {
  /** Raise the login overlay — the logged-out account chip's click. */
  readonly openLogin: (control: HTMLElement) => void;
  /** Injected by tests, where jsdom reports every rect as zero. */
  geometry?: RevealGeometry;
}

/**
 * Mount the topbar on HOST and keep it drawn.
 *
 * The stream is STANDING: it never concludes on its own, so `dispose()` is the
 * only thing that closes it.
 */
export function mountTopbar(host: HTMLElement, ctx: AppContext, deps: TopbarDeps): Handle {
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
  const reveals = mountRevealLayer(host, deps.geometry);
  const tc: TopbarContext = { ctx, reveals, openLogin: deps.openLogin };

  const stream = watchStream(ctx, {
    name: "WatchTopbar",
    schema: WatchTopbarResponseSchema,
    open: (_client, signal) => ctx.streams.watch("topbar", { workspace: ctx.workspace }, signal),
    onPush: (response) => {
      const view = requireMessage(response.topbar, "WatchTopbarResponse.topbar");
      // The old strip's clock subscriptions come down BEFORE the new one goes
      // up, so nothing ticks against an element already detached.
      stopTicking(strip);
      strip.replaceChildren(drawTopbarView(view, tc));
      // The controls re-registered their reveals as they were drawn; whatever
      // the reader had open re-opens against the NEW anchors and content.
      reveals.refresh();
    },
  });

  return {
    dispose(): void {
      log.debug("disposing the topbar", { operation: "topbar.dispose" });
      stream.cancel();
      reveals.dispose();
      stopTicking(strip);
      host.replaceChildren();
    },
  };
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
  accountCell.append(
    drawTopbarConnectivity(requireMessage(u.connectivity, "TopbarView.connectivity")),
    drawTopbarAccount(accountView),
  );
  // THE CELL'S CLICK IS THE LOGIN OPTIONS, in both arms (owner ruling,
  // 2026-09-13). The session line rides the TITLE, which carries it whatever
  // the account state.
  bindAccountReveal(accountCell, accountView, tc);
  left.append(accountCell);

  const center = drawTopbarTitle(requireMessage(u.title, "TopbarView.title"));
  // The title opens the session line too, in every account state.
  bindTitleSessionReveal(center, u.sessionLine, tc);

  const right = document.createElement("div");
  right.className = "topbar-right";

  // THE COLD-GATE STATE IS THE WHOLE RIGHT-HAND GROUP, for the same reason
  // the hibernated one is: a workspace standing at the gate has no session,
  // so the daemon sent no session-scoped element to draw. The daemon never
  // sets both states at once and states this one when both hold, so the two
  // branches cannot disagree about which strip the reader gets.
  if (u.coldGate !== undefined) {
    right.append(drawTopbarColdGate(u.coldGate, tc));
    row.append(left, center, right);
    return row;
  }

  // THE HIBERNATED STATE IS THE WHOLE RIGHT-HAND GROUP. Every element it
  // replaces is session-scoped and the daemon sent none of them, so this is
  // not a strip with holes in it: it is the same strip saying there is no
  // session to describe. The left group and the title above are drawn either
  // way, because a workspace has an account, a route and a name whether or
  // not a session is up.
  if (u.hibernated !== undefined) {
    right.append(drawTopbarHibernated(u.hibernated, tc));
    row.append(left, center, right);
    return row;
  }

  right.append(
    drawTopbarModelSelector(requireMessage(u.modelSelector, "TopbarView.model_selector"), tc),
    drawTopbarPermissionModePicker(
      requireMessage(u.permissionModePicker, "TopbarView.permission_mode_picker"),
      tc,
    ),
  );
  // The fast-mode cell sits directly after the mode picker it is the sibling
  // of. It is ABSENT, not empty, when the vendor has stated no fast mode.
  const fastMode = drawTopbarFastMode(u.fastMode);
  if (fastMode !== null) right.append(fastMode);
  right.append(drawTopbarContextChip(requireMessage(u.context, "TopbarView.context"), tc));
  // NOTHING IS DRAWN WHEN NOTHING IS WRONG: an empty warning list yields no
  // chip at all, not a quiet one.
  const warnings = drawTopbarWarningStrip(requireMessage(u.warnings, "TopbarView.warnings"), tc);
  if (warnings !== null) right.append(warnings);

  row.append(left, center, right);
  return row;
}
