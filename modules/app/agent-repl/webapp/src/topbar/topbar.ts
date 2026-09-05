/**
 * The topbar: one thin strip, redrawn whole on every push.
 *
 * LEFT TIGHT, CENTER FLEXING, RIGHT TIGHT — account and connectivity at the
 * left, the title taking all the free width, then the model selector, the
 * permission-mode picker, the context chip and the warning chip at the far
 * edge. The flank groups never spread; only the title does.
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
import { mountRevealLayer, type RevealGeometry } from "./reveal.js";
import {
  bindSessionReveal,
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
  log("debug", "mounting the topbar", { operation: "topbar.mount" });

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
    open: (client, signal) => client.watchTopbar({ workspace: ctx.workspace }, { signal }),
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
      log("debug", "disposing the topbar", { operation: "topbar.dispose" });
      stream.cancel();
      reveals.dispose();
      stopTicking(strip);
      host.replaceChildren();
    },
  };
}

/** The whole strip, in its three groups. */
export function drawTopbarView(u: TopbarView, tc: TopbarContext): HTMLElement {
  log("debug", "drawing the topbar view", { operation: "topbar.draw" });

  const row = document.createElement("div");
  row.className = "topbar-row";

  const left = document.createElement("div");
  left.className = "topbar-left";
  const account = drawTopbarAccount(requireMessage(u.account, "TopbarView.account"), tc);
  // The session line is the LOGGED-IN chip's reveal; a logged-out chip's click
  // is the login, and binding both would give one button two answers.
  if (account.getAttribute("data-arm") === "loggedIn") {
    bindSessionReveal(account, u.sessionLine, tc);
  }
  left.append(
    account,
    drawTopbarConnectivity(requireMessage(u.connectivity, "TopbarView.connectivity")),
  );

  const center = drawTopbarTitle(requireMessage(u.title, "TopbarView.title"));
  // The title opens the session line too, in every account state.
  bindTitleSessionReveal(center, u.sessionLine, tc);

  const right = document.createElement("div");
  right.className = "topbar-right";
  right.append(
    drawTopbarModelSelector(requireMessage(u.modelSelector, "TopbarView.model_selector"), tc),
    drawTopbarPermissionModePicker(
      requireMessage(u.permissionModePicker, "TopbarView.permission_mode_picker"),
      tc,
    ),
    drawTopbarContextChip(requireMessage(u.context, "TopbarView.context"), tc),
  );
  // NOTHING IS DRAWN WHEN NOTHING IS WRONG: an empty warning list yields no
  // chip at all, not a quiet one.
  const warnings = drawTopbarWarningStrip(requireMessage(u.warnings, "TopbarView.warnings"), tc);
  if (warnings !== null) right.append(warnings);

  row.append(left, center, right);
  return row;
}
