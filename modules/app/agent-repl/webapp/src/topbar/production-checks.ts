/**
 * The production-checks chip: one pulse glyph immediately right of the
 * persistent-wifi chip, stating whether the workspace's REPOSITORY'S
 * production checks run or are paused (`agentrepl.v1.UpdateProductionPause`).
 * The daemon sets it only in a repository whose lifecycle config turns
 * production checks on, so the strip draws nothing in its place otherwise.
 *
 * THE SAME LOOK AND FEEL AS THE PERSISTENT-WIFI AND REMOTE-CONTROL CHIPS,
 * ANOTHER SYMBOL. The glyph is always black and the standing arm paints the
 * disc: running → a green disc, paused → a white one. The glyph and its disc
 * are drawn through the topbar's one disc drawing (src/topbar/disc-glyph.ts),
 * and the stylesheet's chip rules name this chip's hooks beside the others'.
 *
 * A CONTROL AS WELL AS A STATUS: clicking the glyph sends
 * `UpdateProductionPause` for the repository the chip carries, `pause` while
 * running and `resume` while paused. The standing is the repository's, and
 * the arm sent is the one the chip drew; a click on a chip a sibling's click
 * already changed is refused (`already_paused`, `not_paused`) and stated at the
 * chip, and every chip redraws from the push the change causes.
 */
import { createControl, type Control } from "../control.js";
import { UpdateProductionPauseResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_production_pause_pb";
import type { TopbarProductionChecks } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { createLogger } from "../log.js";
import type { SentenceTable } from "../rpc/refuse.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { sendChipToggle } from "./chip-toggle.js";
import type { TopbarContext } from "./context.js";
import { drawDiscGlyph, type DiscGlyph } from "./disc-glyph.js";

const log = createLogger("webapp.topbar.production-checks");

/**
 * The pulse glyph: a flat line with one heartbeat spike, stroked in the
 * current color on the same 24-unit grid as the wifi glyph. It spans x 3..21
 * and y 4..20, symmetric about (12, 12), inside the disc.
 */
export const PULSE_GLYPH_PATHS: readonly string[] = ["M3 12h4l3-8 4 16 3-8h4"];

/** The pulse glyph on its disc, drawn through the topbar's one disc drawing. */
const PULSE_GLYPH: DiscGlyph = {
  glyphClass: "topbar-production-checks-glyph",
  discClass: "topbar-production-checks-disc",
  paths: PULSE_GLYPH_PATHS,
};

/**
 * The standing arm's attribute value. The standing is REQUIRED on a chip the
 * daemon set, so an unset arm is a malformed view.
 */
function standingArm(u: TopbarProductionChecks): "running" | "paused" {
  const standing = requireCase(u.standing, "TopbarProductionChecks.standing");
  switch (standing.case) {
    case "running":
      return "running";
    case "paused":
      return "paused";
    default: {
      const other: { case: string } = standing;
      return unreachableArm("TopbarProductionChecks.standing", other.case);
    }
  }
}

/** Every cause UpdateProductionPause can answer with. */
const UPDATE_PRODUCTION_PAUSE_CAUSES = {
  alreadyPaused: () => "the production checks are already paused",
  notPaused: () => "the production checks are not paused",
  unknownRepository: () => "the daemon holds no such repository",
  checksOff: () => "this repository turns no production checks on",
} as unknown as SentenceTable;

/**
 * The chip, painted from its standing arm, with the daemon's tooltip, and its
 * click pausing or resuming the repository's checks.
 *
 * THE WRAP CARRIES THE ARM AND THE BUTTON IS THE CONTROL, as the other chips'
 * do: a refusal is drawn inside the wrap beside the button.
 */
export function drawTopbarProductionChecks(u: TopbarProductionChecks, tc: TopbarContext): HTMLElement {
  const standing = standingArm(u);
  const repository = requireMessage(u.repository, "TopbarProductionChecks.repository");
  const chip = document.createElement("span");
  chip.className = "topbar-production-checks";
  chip.setAttribute("data-standing", standing);
  chip.title = requireMessage(u.tooltip, "TopbarProductionChecks.tooltip").text;

  const button = createControl();
  button.className = "topbar-production-checks-button";
  button.setAttribute("aria-label", standing === "running" ? "pause production checks" : "resume production checks");
  button.append(drawDiscGlyph(PULSE_GLYPH));
  button.addEventListener("click", () => {
    void switchProductionChecks(tc, chip, button, u, standing);
  });
  chip.append(button);

  log.debug("drawing the production-checks chip", {
    operation: "topbar.production-checks",
    context: { standing, repository: repository.dir },
  });
  return chip;
}

/**
 * Send `UpdateProductionPause` for the chip's repository: `pause` when DRAWN
 * is running, `resume` when it is paused.
 */
async function switchProductionChecks(
  tc: TopbarContext,
  chip: HTMLElement,
  button: Control,
  u: TopbarProductionChecks,
  drawn: "running" | "paused",
): Promise<void> {
  const action = drawn === "running" ? "pause" : "resume";
  // AT INFO: a person's click that stops or starts the production mediator
  // is exactly the edge someone asks about afterwards.
  log.info("the reader switched the production checks", {
    operation: "topbar.production-checks-switch",
    context: { drawn, action, repository: u.repository?.dir ?? "" },
  });
  await sendChipToggle(tc, chip, button, {
    rpc: "UpdateProductionPause",
    operation: "topbar.production-checks",
    send: (client) =>
      client.updateProductionPause({
        repository: u.repository,
        action: action === "pause" ? { case: "pause", value: {} } : { case: "resume", value: {} },
      }),
    schema: UpdateProductionPauseResponseSchema,
    causes: UPDATE_PRODUCTION_PAUSE_CAUSES,
    onSuccess: (success) => {
      const applied = requireCase(success.applied, "UpdateProductionPauseSuccess.applied");
      log.info("the production checks were switched", {
        operation: "topbar.production-checks-switched",
        context: { applied: applied.case },
      });
    },
  });
}
