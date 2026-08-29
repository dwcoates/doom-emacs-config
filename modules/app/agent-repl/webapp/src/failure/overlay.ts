/**
 * The client-local failure overlay: where a user whose page stopped working
 * finds out why.
 *
 * WHAT IT DRAWS AND WHAT IT DOES NOT. Exactly the six `FailureKind` arms a
 * FRONTEND may mint — the failures of its own machinery, which the daemon
 * definitionally cannot observe. The daemon's own eleven arms never come here:
 * a shim that died or a session that was superseded reaches the user through
 * the footer's disconnected and blocked families and the topbar's warning
 * strip, which are pushed views. This overlay is the residue those surfaces
 * cannot carry, because when it fires they may be exactly what is broken.
 *
 * ONE CARD PER ARM. A repeat report of the same arm REPLACES its card rather
 * than stacking: a reconnect loop that appended a card per attempt would bury
 * the page under its own alarm, and the second report of a condition is the
 * same condition. That is the old local-failure uuid-per-arm idea, reduced to
 * what it always was — a Map keyed by arm.
 *
 * RETRACTION IS PER ARM AND IS THE FILER'S CALL. `daemon_unreachable` is
 * window-shaped and `watchStream` retracts it on the first successful push, so
 * a flapping link leaves no ghost. `workspace_gone` and `stale_bundle` are
 * deliberately unretractable — there is nothing to come back, and a
 * self-clearing version of either would hide a page that is silently wrong.
 * This module enforces nothing about which: a caller that never retracts gets
 * a standing card, which is the correct outcome for those two.
 *
 * BLUE, FROM THE SHARED VOCABULARY. Every client-local arm is on the
 * `client_local` side, which `render-colors.json#failure_sides` paints blue
 * beside `machinery`: both mean the route to a working session is broken and
 * neither is the vendor's doing. The color is looked up rather than written
 * down, so a card can never explain a workspace in a color the workspace is
 * not.
 */
import { create } from "@bufbuild/protobuf";
import { FailureKindSchema, type FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { log } from "../log.js";
import { requireCase } from "../rpc/strict.js";
import { failureSideColor, toneClass } from "../vocab.js";
import { isClientFailureArm, type ClientFailureArm, type FailureSink } from "./sink.js";

export {
  bootFailed,
  controlPlaneFailed,
  daemonUnreachable,
  frameUndecodable,
  staleBundle,
  workspaceGone,
} from "./sink.js";

export interface Handle {
  dispose(): void;
}

/** The overlay is the app's FailureSink and owns its own host. */
export type FailureOverlayHandle = FailureSink & Handle;

/**
 * The sentence each arm's card leads with.
 *
 * COMPOSED HERE, and this is the one place in the webapp that is allowed to
 * compose a sentence rather than draw one the daemon composed — because the
 * daemon does not know about these failures at all. Six fixed strings, one per
 * arm, chosen by the arm and never assembled from evidence.
 */
const ARM_HEADLINE: Readonly<Record<ClientFailureArm, string>> = {
  daemonUnreachable: "lost the connection to the daemon; reconnecting",
  workspaceGone: "this workspace no longer exists on the daemon",
  bootFailed: "this page could not start",
  controlPlaneFailed: "a request this page made outside the streams failed",
  frameUndecodable: "a frame could not be read and was skipped, so conversation may be missing",
  staleBundle: "this page cannot read the daemon's state",
};

/**
 * The evidence rows a card shows, per arm, as label/value pairs read straight
 * off the arm's own typed fields.
 *
 * VERBATIM, NEVER INTERPRETED. Each of these is the producer's own account —
 * a close code, a decode failure, the head of the frame — and the card's job
 * is to put it where whoever debugs this can read it, not to explain it.
 * An empty value is omitted, because the protos state outright that several of
 * these are allowed to be empty and an empty row reads as missing data.
 */
function evidenceRows(kind: FailureKind): ReadonlyArray<readonly [string, string]> {
  const arm = requireCase(kind.kind, "FailureKind.kind");
  switch (arm.case) {
    case "daemonUnreachable":
      return [
        ["close code", String(arm.value.closeCode)],
        ["close reason", arm.value.closeReason],
      ];
    case "workspaceGone":
      return [];
    case "bootFailed":
      return [["cause", arm.value.cause]];
    case "controlPlaneFailed":
      return [
        ["request", arm.value.what],
        ["cause", arm.value.cause],
      ];
    case "frameUndecodable":
      return [
        ["cause", arm.value.cause],
        ["frame", arm.value.frameHead],
      ];
    case "staleBundle":
      return [["detail", arm.value.detail]];
    default:
      // A daemon-minted arm reaching this overlay is a producer violating the
      // split, not a card to draw: neither producer may set the other's arms.
      return [];
  }
}

/**
 * Mount the overlay on HOST and return it as the app's failure sink.
 *
 * The host ships empty and stays empty while the page is healthy, so the
 * overlay costs nothing until something files a card.
 */
export function mountFailureOverlay(host: HTMLElement): FailureOverlayHandle {
  log("debug", "mounting the failure overlay", { operation: "failure-overlay.mount" });
  const cards = new Map<ClientFailureArm, HTMLElement>();

  const redraw = (): void => {
    host.replaceChildren(...cards.values());
    host.toggleAttribute("data-empty", cards.size === 0);
  };

  redraw();

  return {
    report(kind: FailureKind): void {
      const arm = requireCase(kind.kind, "FailureKind.kind").case;
      if (!isClientFailureArm(arm)) {
        // The daemon never mints one of these, and a frontend never mints the
        // daemon's. Refusing loudly is the whole point of splitting the
        // vocabulary by producer.
        log("error", `the failure overlay was handed the non-client arm '${arm}'`, {
          operation: "failure-overlay.foreign-arm",
          context: { arm },
        });
        return;
      }
      const replacing = cards.has(arm);
      log(replacing ? "debug" : "error", `${replacing ? "replacing" : "filing"} the ${arm} failure card`, {
        operation: replacing ? "failure-overlay.replace" : "failure-overlay.report",
        context: { arm },
      });
      cards.set(arm, drawFailureCard(kind, arm));
      redraw();
    },

    retract(arm: ClientFailureArm): void {
      if (!cards.delete(arm)) return;
      log("info", `retracting the ${arm} failure card`, {
        operation: "failure-overlay.retract",
        context: { arm },
      });
      redraw();
    },

    dispose(): void {
      log("debug", "disposing the failure overlay", { operation: "failure-overlay.dispose" });
      cards.clear();
      host.replaceChildren();
      host.removeAttribute("data-empty");
    },
  };
}

/**
 * One card.
 *
 * The look is the old system-failure card's, ported: a rounded outline with a
 * thick left edge in the failure's own color, a mark and a headline on the
 * head line, and the evidence beneath in the quieter monospaced register that
 * says "this is for whoever debugs it" rather than "this is the sentence you
 * are meant to read".
 */
export function drawFailureCard(kind: FailureKind, arm: ClientFailureArm): HTMLElement {
  const card = document.createElement("div");
  card.className = `failure-card ${toneClass(failureSideColor("client_local"))}`;
  card.setAttribute("data-arm", arm);
  card.setAttribute("role", "status");

  const head = document.createElement("div");
  head.className = "failure-head";
  const mark = document.createElement("span");
  mark.className = "failure-mark";
  // A GLYPH, not an emoji: a heavy vertical bar reads as an alert marker at
  // any size and inherits the card's color, which an emoji cannot do.
  mark.textContent = "❙";
  mark.setAttribute("aria-hidden", "true");
  const headline = document.createElement("span");
  headline.className = "failure-message";
  headline.textContent = ARM_HEADLINE[arm];
  head.append(mark, headline);
  card.append(head);

  for (const [label, value] of evidenceRows(kind)) {
    if (value === "") continue;
    const row = document.createElement("div");
    row.className = "failure-detail";
    const name = document.createElement("span");
    name.className = "failure-detail-label";
    name.textContent = `${label}:`;
    const text = document.createElement("span");
    text.className = "failure-detail-value";
    text.textContent = value;
    row.append(name, text);
    card.append(row);
  }
  return card;
}

/** An empty FailureKind, for a test asserting the unset-oneof refusal. */
export function emptyFailureKind(): FailureKind {
  return create(FailureKindSchema, {});
}
