/**
 * The topbar: one thin strip, redrawn whole on every push.
 *
 * LEFT TIGHT, RIGHT TIGHT, AND THE TITLE IN WHAT IS BETWEEN THEM — the
 * account cell (its connectivity glyph, then its label) at the left edge, then
 * the model selector, the permission-mode picker, the context chip and the
 * warning chip at the right edge.
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
 * tooltip, the tone name, the title, the figure and every warning sentence are
 * all composed daemon-side.
 *
 * THE WARNING CHIP DOES NOT WAIT FOR A PUSH. It is where the page's own
 * client-local failures are shown (`src/failure/local.ts`), and those are
 * exactly the failures that can stop a push arriving — a boot that could not
 * adopt, a link that dropped. So the strip is MOUNTED at boot, before
 * adoption and before any stream, and redraws on every change to that set:
 * over the last view drawn if there was one (stale while the link is down,
 * which is the point), or as the chip alone if there never was. `watch` opens
 * the stream once adoption has succeeded.
 *
 * WHAT SURVIVES A PUSH IS THE REVEAL, and only by name. The strip itself is
 * replaced entirely — that is the whole-view-push contract — while the reveal
 * layer is a sibling that outlives it, and each control re-registers its reveal
 * as it is drawn so a reader with the model list open sees the NEW list rather
 * than the one from the push before.
 */
import { WatchTopbarResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_topbar_pb";
import type { TopbarView } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import type { LocalFailureView } from "../failure/local.js";
import { stopTicking } from "../feed/ticking.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { requireMessage } from "../rpc/strict.js";
import { watchStream, type StreamHandle } from "../rpc/streams.js";
import { drawTopbarContextChip } from "./context-chip.js";
import type { TopbarContext, WarningChipContext } from "./context.js";
import { drawTopbarModelSelector } from "./model.js";
import { drawTopbarPermissionModePicker } from "./permission-mode.js";
import { drawTopbarFastMode } from "./fast-mode.js";
import { bindAccountReveal } from "./account.js";
import { mountRevealLayer, type RevealGeometry } from "./reveal.js";
import {
  bindTitleSessionReveal,
  drawTopbarAccount,
  drawTopbarConnectivity,
  drawTopbarTitle,
} from "./strip.js";
import { drawLocalWarningStrip, drawTopbarWarningStrip } from "./warnings.js";

/** What every mount answers with. */
export interface Handle {
  dispose(): void;
}

export interface TopbarDeps {
  /** The client-local failures the warning chip lists beside the pushed ones. */
  readonly failures: LocalFailureView;
  /** Injected by tests, where jsdom reports every rect as zero. */
  geometry?: RevealGeometry;
}

export interface TopbarWatchDeps {
  /** Raise the login overlay — the logged-out account chip's click. */
  readonly openLogin: (control: HTMLElement) => void;
}

export interface TopbarHandle extends Handle {
  /**
   * Open the topbar's stream and draw its pushes. Called ONCE, after
   * adoption: a joining daemon refuses every per-workspace rpc until then.
   */
  watch(ctx: AppContext, deps: TopbarWatchDeps): void;
}

/**
 * Mount the topbar on HOST and keep its warning chip drawn from DEPS.failures
 * until `watch` hands it a stream.
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
  const reveals = mountRevealLayer(host, deps.geometry);
  const cc: WarningChipContext = { reveals, localFailures: () => deps.failures.standing() };

  /** The context `watch` built, and the last view drawn over it. */
  let tc: TopbarContext | null = null;
  let view: TopbarView | null = null;
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

  // A CHANGE TO THE CLIENT-LOCAL FAILURES REDRAWS WITHOUT A PUSH: over the
  // last view if there is one, which may be stale because the link is what
  // failed, or as the chip alone if no view was ever drawn.
  const redraw = (): void => {
    log.debug("redrawing the topbar for a client-local failure change", {
      operation: "topbar.local-failures-changed",
      context: { has_view: view !== null, local_failures: cc.localFailures().length },
    });
    show(view !== null && tc !== null ? drawTopbarView(view, tc) : drawTopbarFailuresOnly(cc));
  };
  const unsubscribe = deps.failures.subscribe(redraw);
  // Failures filed before this mount are drawn by it.
  show(drawTopbarFailuresOnly(cc));

  return {
    watch(ctx: AppContext, watchDeps: TopbarWatchDeps): void {
      if (stream !== null) {
        log.error("the topbar was asked to watch twice", { operation: "topbar.watch-twice" });
        throw new Error("the topbar was asked to watch twice");
      }
      log.info("watching the topbar", { operation: "topbar.watch" });
      const watching: TopbarContext = { ...cc, ctx, openLogin: watchDeps.openLogin };
      tc = watching;
      stream = watchStream(ctx, {
        name: "WatchTopbar",
        schema: WatchTopbarResponseSchema,
        open: (_client, signal) =>
          ctx.streams.watch("topbar", { workspace: ctx.workspace }, signal),
        onPush: (response) => {
          const next = requireMessage(response.topbar, "WatchTopbarResponse.topbar");
          show(drawTopbarView(next, watching));
          // Remembered only once it drew, so a malformed push never becomes
          // the view a failure change redraws over.
          view = next;
        },
      });
    },

    dispose(): void {
      log.debug("disposing the topbar", { operation: "topbar.dispose" });
      unsubscribe();
      stream?.cancel();
      reveals.dispose();
      stopTicking(strip);
      host.replaceChildren();
    },
  };
}

/**
 * The strip before any view: the warning chip alone, in the right-hand group
 * it always occupies, or NOTHING when no client-local failure stands.
 */
export function drawTopbarFailuresOnly(cc: WarningChipContext): HTMLElement | null {
  const chip = drawLocalWarningStrip(cc);
  if (chip === null) return null;
  const row = document.createElement("div");
  row.className = "topbar-row";
  const left = document.createElement("div");
  left.className = "topbar-left";
  const center = document.createElement("div");
  const right = document.createElement("div");
  right.className = "topbar-right";
  right.append(chip);
  row.append(left, center, right);
  return row;
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

  // THE STRIP HAS ONE SHAPE AND ONLY ONE (topbar.proto, FIXED SCHEMA AND
  // ORGANIZATION; owner ruling 2026-09-13). There is no branch here: every
  // cell is drawn on every push and in the same slot, whether or not the
  // workspace has a session. A control the daemon left ABSENT states so in
  // its own slot — each draw function answers with `no-session.ts`'s dash —
  // and the context chip and the warning strip carry the session-less facts
  // themselves, the chip's hover leading with the reason and the strip
  // carrying it as a line.
  right.append(
    drawTopbarModelSelector(u.modelSelector, tc),
    drawTopbarPermissionModePicker(u.permissionModePicker, tc),
    // The fast-mode cell sits directly after the mode picker it is the
    // sibling of.
    drawTopbarFastMode(u.fastMode),
    drawTopbarContextChip(requireMessage(u.context, "TopbarView.context"), tc),
  );
  // NOTHING IS DRAWN WHEN NOTHING IS WRONG: an empty warning list with no
  // client-local failure standing yields no chip at all, not a quiet one.
  const warnings = drawTopbarWarningStrip(requireMessage(u.warnings, "TopbarView.warnings"), tc);
  if (warnings !== null) right.append(warnings);

  row.append(left, center, right);
  return row;
}
