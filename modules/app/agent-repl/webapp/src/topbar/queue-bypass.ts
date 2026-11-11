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
 * A CONTROL AS WELL AS A STATUS. Off, a click opens the menu of how the
 * bypass session gets its context, drawn exactly as the topbar's other menus
 * are (the reveal layer, `list-rows`, the shared row rule, the one pick path):
 * its rows are the served `frontend.v1.TopbarQueueBypassOff.options`, in the
 * served order, and a pick sends `agentrepl.v1.UpdateQueueBypassMode`'s `on`
 * arm echoing the row's context verbatim. The daemon decides which options
 * are offered; this file never infers one. On, a click sends the `toggle`
 * arm, which releases the mode. Either way the daemon switches the
 * workspace's conversation, and the chip redraws from the push the change
 * causes.
 */
import { createControl, type Control } from "../control.js";
import { UpdateQueueBypassModeResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_queue_bypass_mode_pb";
import type {
  TopbarQueueBypass,
  TopbarQueueBypassContextOption,
  TopbarQueueBypassOff,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { log } from "../log.js";
import type { SentenceTable } from "../rpc/refuse.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { sendChipToggle } from "./chip-toggle.js";
import type { TopbarContext } from "./context.js";
import { drawDiscGlyph, type DiscGlyph } from "./disc-glyph.js";
import { sendPick } from "./pick.js";
import { asAnchor } from "./strip.js";

/** The reveal's name, and the anchor the chip carries. */
export const QUEUE_BYPASS_REVEAL = "queue-bypass";

/** The hook naming a menu row's context: its arm's name. */
export const QUEUE_BYPASS_CONTEXT_ATTRIBUTE = "data-queue-bypass-context";

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
  noBypassConversation: () => "this workspace has no bypass conversation to resume",
  compactionFailed: (value: { detail: string }) => `the compaction did not complete: ${value.detail}`,
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
  button.append(drawDiscGlyph(BYPASS_GLYPH));
  const arm = u.mode;
  if (arm.case === "off") {
    // The chip is the control; see the model selector for the reasoning.
    button.setAttribute("aria-label", "choose how queue bypass mode gets its context");
    asAnchor(chip, QUEUE_BYPASS_REVEAL);
    const body = (): HTMLElement => drawQueueBypassOptions(arm.value, tc, chip, button);
    tc.reveals.register(QUEUE_BYPASS_REVEAL, QUEUE_BYPASS_REVEAL, body);
    button.addEventListener("click", () => {
      tc.reveals.toggle(QUEUE_BYPASS_REVEAL, QUEUE_BYPASS_REVEAL, body);
    });
  } else if (arm.case === "on") {
    button.setAttribute("aria-label", "release queue bypass mode");
    button.addEventListener("click", () => {
      void toggleQueueBypassMode(tc, chip, button, mode);
    });
  } else {
    button.setAttribute("aria-label", "queue bypass mode is held by another workspace");
  }
  chip.append(button);

  log.debug("drawing the queue-bypass chip", {
    operation: "topbar.queue-bypass",
    context: { mode },
  });
  return chip;
}

/**
 * A row's context as a DOM hook: its arm's name. A row with no arm set is a
 * malformed push, never a nameless row.
 */
export function queueBypassContextToken(option: TopbarQueueBypassContextOption): string {
  const context = requireMessage(option.context, "TopbarQueueBypassContextOption.context");
  return requireCase(context.context, "TopbarQueueBypassContext.context").case;
}

/** The reveal: exactly the served options, in the served order. */
export function drawQueueBypassOptions(
  off: TopbarQueueBypassOff,
  tc: TopbarContext,
  chip: HTMLElement,
  button: Control,
): HTMLElement {
  const list = document.createElement("div");
  list.className = "topbar-queue-bypass-options list-rows";
  for (const option of off.options) {
    list.append(drawQueueBypassOption(option, tc, chip, button));
  }
  return list;
}

/** One offered context. */
export function drawQueueBypassOption(
  option: TopbarQueueBypassContextOption,
  tc: TopbarContext,
  chip: HTMLElement,
  button: Control,
): Control {
  const row = createControl();
  row.className = "topbar-queue-bypass-option";
  row.setAttribute(QUEUE_BYPASS_CONTEXT_ATTRIBUTE, queueBypassContextToken(option));
  row.textContent = option.displayName;
  row.addEventListener("click", () => {
    void pickQueueBypassContext(option, tc, chip, button, row);
  });
  return row;
}

/** Take the mode, echoing the picked row's context verbatim. */
export async function pickQueueBypassContext(
  option: TopbarQueueBypassContextOption,
  tc: TopbarContext,
  chip: HTMLElement,
  button: Control,
  row: Control,
): Promise<void> {
  log.info(`the reader took queue bypass mode with ${option.displayName}`, {
    operation: "topbar.queue-bypass-picked",
    context: { context: queueBypassContextToken(option) },
  });
  await sendPick(
    {
      rpc: "UpdateQueueBypassMode",
      send: (client) =>
        client.updateQueueBypassMode({
          workspace: tc.ctx.workspace,
          action: { case: "on", value: { context: option.context } },
        }),
      schema: UpdateQueueBypassModeResponseSchema,
      causes: UPDATE_QUEUE_BYPASS_MODE_CAUSES,
      malformedOperation: "topbar.queue-bypass-pick",
      unreadableOperation: "topbar.queue-bypass-malformed-refusal",
    },
    tc,
    chip,
    button,
    row,
  );
}

/** Send `UpdateQueueBypassMode{toggle}` for this page's workspace: a release. */
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
