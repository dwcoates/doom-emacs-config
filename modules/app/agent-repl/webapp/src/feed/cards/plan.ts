/**
 * plan — the plan bubble: purple, response-styled, READ-ONLY.
 *
 * ONE BUBBLE PER PLAN EPISODE. The daemon keys the enter-plan-mode and
 * exit-plan-mode calls onto ONE `FeedId`, so entering draws the planning state
 * and the exit fills the SAME bubble with the document. Nothing here creates a
 * second row for the second call, and nothing accumulates: the row upserts
 * through its states.
 *
 * THE BUBBLE IS NOT AN EDITOR. Revisions happen in the composer — the reader
 * says what to change and the agent rewrites the plan — so there is no editable
 * surface here and no save. The one affordance is the EDIT LINK, and even that
 * does not edit: it raises `OpenInEditor` so the file opens in the user's own
 * editor, through the ONE shared link component (the same component a findings
 * row's location and a worktree divider's path use).
 *
 * THE EDIT LINK DRAWS ONLY WHEN THE VENDOR NAMED THE FILE. `edit` is optional
 * precisely because a plan the vendor did not put on disk has no path to hand
 * to an editor; a button that opened nothing would be worse than no button.
 * NOTHING IS DRAWN ON SUCCESS — the result of the click is an editor window the
 * user is already looking at — and the refusal draws at the link, which the
 * shared component owns.
 */
import type {
  FeedPlan,
  FeedPlanEditTarget,
  FeedPlanFailed,
  FeedPlanPlanned,
  FeedPlanProse,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { renderEditorLink } from "../../link.js";
import { log } from "../../log.js";
import { renderMarkdown } from "../../markdown.js";
import type { AppContext } from "../../rpc/context.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { agenticBubble } from "./controls.js";

const PATH = "FeedPlan";

/** The word the edit affordance says. */
export const EDIT_PLAN_TEXT = "edit plan";

/** The badge each state wears, and the word it says. */
const STATE_BADGES = {
  planning: { className: "badge run", text: "planning" },
  planned: { className: "badge ok", text: "plan" },
  failed: { className: "badge err", text: "failed" },
} as const satisfies Record<string, { className: string; text: string }>;

/** Every state this build draws, for the suite to hold to the schema. */
export const PLAN_STATE_ARMS: readonly string[] = Object.keys(STATE_BADGES);

/** The plan bubble. */
export function drawFeedPlan(u: FeedPlan, rc: RowContext): HTMLElement {
  const state = requireCase(u.state, `${PATH}.state`);
  log.debug("drawing a plan bubble", {
    operation: "feed.cards.plan",
    context: { state: state.case },
  });

  // THE ARM IS CHECKED BEFORE ANYTHING IS DRAWN FROM IT, so an arm a newer
  // daemon set reaches the refusal that quotes its name rather than a table
  // lookup that has no entry for it.
  switch (state.case) {
    case "planning":
      // The planning treatment: the badge, and the same animated ellipsis every
      // other live face on this page wears. A breathing wash of its own would
      // be a second visual language for "still working".
      return agenticBubble({ state: state.case, content: [badge(state.case), planningIndicator()] });
    case "planned":
      return agenticBubble({
        state: state.case,
        content: [badge(state.case), drawFeedPlanPlanned(state.value, rc.ctx, `${PATH}.planned`)],
      });
    case "failed":
      return agenticBubble({
        state: state.case,
        content: [badge(state.case), drawFeedPlanFailed(state.value, `${PATH}.failed`)],
      });
    default:
      return unreachableArm(`${PATH}.state`, armName(state));
  }
}

/** The presented plan: its prose, and the edit link when there is a file. */
export function drawFeedPlanPlanned(
  u: FeedPlanPlanned,
  ctx: AppContext,
  path: string,
): HTMLElement {
  log.debug("drawing a presented plan", {
    operation: "feed.cards.plan.planned",
    context: { path, edit: u.edit !== undefined },
  });
  const el = document.createElement("div");
  el.className = "plan-planned";
  el.append(drawFeedPlanProse(requireMessage(u.prose, `${path}.prose`), `${path}.prose`));
  if (u.edit !== undefined) {
    el.append(drawFeedPlanEditTarget(u.edit, ctx, `${path}.edit`));
  }
  return el;
}

/**
 * The plan document, through the shared prose renderer.
 *
 * The same markdown treatment a response bubble's prose gets — never shown raw
 * — and the same cap, which the bubble's own body class states.
 */
export function drawFeedPlanProse(u: FeedPlanProse, path: string): HTMLElement {
  log.debug("drawing a plan's prose", {
    operation: "feed.cards.plan.prose",
    context: { path, length: u.markdown.length },
  });
  const el = document.createElement("div");
  el.className = "plan-prose";
  el.innerHTML = renderMarkdown(u.markdown);
  return el;
}

/**
 * The edit affordance: the ONE shared editor link, pointed at the plan's file.
 *
 * The path is ECHOED, never parsed and never drawn as prose — it is a location
 * on the daemon's host that the editor resolves, and this end has no business
 * interpreting it. No `line`: a plan file opens at its top.
 */
export function drawFeedPlanEditTarget(
  u: FeedPlanEditTarget,
  ctx: AppContext,
  path: string,
): HTMLElement {
  log.debug("drawing a plan's edit affordance", {
    operation: "feed.cards.plan.edit",
    context: { path },
  });
  const el = document.createElement("div");
  el.className = "plan-edit";
  el.append(renderEditorLink(ctx, { text: EDIT_PLAN_TEXT, path: u.path }));
  return el;
}

/** The failed state's composed reason, drawn verbatim. */
export function drawFeedPlanFailed(u: FeedPlanFailed, path: string): HTMLElement {
  log.debug("drawing a failed plan episode", {
    operation: "feed.cards.plan.failed",
    context: { path },
  });
  const el = document.createElement("div");
  el.className = "plan-failed";
  el.textContent = u.text;
  return el;
}

/** The still-planning indicator. */
function planningIndicator(): HTMLElement {
  const el = document.createElement("span");
  el.className = "animated-ellipsis plan-planning";
  el.setAttribute("aria-hidden", "true");
  return el;
}

/** One of the three badges, by arm. */
function badge(arm: keyof typeof STATE_BADGES): HTMLElement {
  const spec = STATE_BADGES[arm];
  const el = document.createElement("span");
  el.className = spec.className;
  el.textContent = spec.text;
  return el;
}
