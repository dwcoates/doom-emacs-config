/**
 * The queue-bypass chip: the first chip of the strip, left of the
 * remote-control chip, stating whether THIS WORKSPACE holds its repository's
 * queue bypass mode — the merge queue hard-stopped, and every change landed
 * straight on the main branch from the workspace's bypass conversation.
 *
 * THE SAME LOOK AND FEEL AS THE REMOTE-CONTROL CHIP, ANOTHER SYMBOL. The glyph
 * sits on a disc drawn through the topbar's one disc drawing
 * (src/topbar/disc-glyph.ts): on → a lit disc, off → a white one,
 * held_elsewhere → muted, and no click. The arm rides as a data attribute and
 * the stylesheet does the painting.
 *
 * ABSENT ON A WORK ACCOUNT: the daemon sends no chip for a workspace whose
 * session spends as one, and the strip then draws nothing in its place.
 *
 * A CONTROL AS WELL AS A STATUS: clicking sends
 * `agentrepl.v1.UpdateQueueBypassMode`'s `toggle` arm for this page's
 * workspace. The daemon resolves it, switches the workspace's conversation,
 * and the chip redraws from the push the change causes.
 */
import { createControl, type Control } from "../control.js";
import { UpdateQueueBypassModeResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_queue_bypass_mode_pb";
import type { TopbarQueueBypass } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { log } from "../log.js";
import type { SentenceTable } from "../rpc/refuse.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { sendChipToggle } from "./chip-toggle.js";
import type { TopbarContext } from "./context.js";
import { drawDiscGlyph, type DiscGlyph } from "./disc-glyph.js";

/**
 * The bypass glyph: a curved arrow stepping over a bar, stroked in the current
 * color on the same 24-unit grid as the other disc glyphs.
 */
export const BYPASS_GLYPH_PATHS: readonly string[] = [
  "M4 16h16",
  "M5 12c2-6 12-6 14 0",
  "M16 9l3 3 2-4",
];

/** The bypass glyph on its disc, drawn through the topbar's one disc drawing. */
const BYPASS_GLYPH: DiscGlyph = {
  glyphClass: "topbar-queue-bypass-glyph",
  discClass: "topbar-queue-bypass-disc",
  paths: BYPASS_GLYPH_PATHS,
};

/** The mode arm's attribute value. The mode is REQUIRED whenever the chip is sent. */
function modeArm(u: TopbarQueueBypass): string {
  const mode = requireCase(u.mode, "TopbarQueueBypass.mode");
  switch (mode.case) {
    case "on":
      return "on";
    case "off":
      return "off";
    case "heldElsewhere":
      return "held-elsewhere";
    default: {
      const other: { case: string } = mode;
      return unreachableArm("TopbarQueueBypass.mode", other.case);
    }
  }
}

/** The causes only UpdateQueueBypassMode can answer with. */
const UPDATE_QUEUE_BYPASS_MODE_CAUSES = {
  workAccount: () => "a work account's workspace never bypasses the merge queue",
  heldElsewhere: () => "another workspace of this repository holds queue bypass mode",
  turnInFlight: () => "a turn is running; end it or interrupt it first",
  switchFailed: (value: { detail: string }) => `the switch did not complete: ${value.detail}`,
} as unknown as SentenceTable;

/** The chip, painted from its mode arm, with the daemon's tooltip. */
export function drawTopbarQueueBypass(u: TopbarQueueBypass, tc: TopbarContext): HTMLElement {
  const mode = modeArm(u);
  const chip = document.createElement("span");
  chip.className = "topbar-queue-bypass";
  chip.setAttribute("data-mode", mode);
  chip.title = requireMessage(u.tooltip, "TopbarQueueBypass.tooltip").text;

  const button = createControl();
  button.className = "topbar-queue-bypass-button";
  button.setAttribute("aria-label", "toggle queue bypass mode");
  button.append(drawDiscGlyph(BYPASS_GLYPH));
  if (mode !== "held-elsewhere") {
    button.addEventListener("click", () => {
      void toggleQueueBypassMode(tc, chip, button, mode);
    });
  }
  chip.append(button);

  log.debug("drawing the queue-bypass chip", {
    operation: "topbar.queue-bypass",
    context: { mode },
  });
  return chip;
}

/** Send `UpdateQueueBypassMode{toggle}` for this page's workspace. */
async function toggleQueueBypassMode(tc: TopbarContext, chip: HTMLElement, button: Control, drawn: string): Promise<void> {
  log.info("the reader toggled queue bypass mode", {
    operation: "topbar.queue-bypass-toggle",
    context: { mode: drawn },
  });
  await sendChipToggle(tc, chip, button, {
    rpc: "UpdateQueueBypassMode",
    operation: "topbar.queue-bypass",
    send: (client) =>
      client.updateQueueBypassMode({
        workspace: tc.ctx.workspace,
        action: { case: "toggle", value: {} },
      }),
    schema: UpdateQueueBypassModeResponseSchema,
    causes: UPDATE_QUEUE_BYPASS_MODE_CAUSES,
    onSuccess: (success) => {
      log.info("queue bypass mode was toggled", {
        operation: "topbar.queue-bypass-toggled",
        context: { on: success.on },
      });
    },
  });
}
