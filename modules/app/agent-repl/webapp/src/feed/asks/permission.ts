/**
 * permission — THE CONSENT CARD: the turn blocked on the user's permission for
 * one gated call.
 *
 * EVERY WORD ON IT IS RESOLVED UPSTREAM. The headline is the vendor's own
 * rendered sentence, the subtitle is the vendor's, the trigger note is the
 * daemon's composed explanation of why the gate fired, and the argument preview
 * is one composed line per argument. Nothing here assembles a sentence from a
 * tool name, and nothing infers the tone from the words: an ask-rule trigger is
 * worded so the reader knows it was user-configured, and re-wording it would
 * destroy exactly the distinction it exists to make.
 *
 * "ALWAYS ALLOW" IS DRAWN ONLY WHEN THE VENDOR OFFERED A STANDING FORM.
 * `standing_offered`'s PRESENCE is the whole fact — the echo token itself never
 * reaches this page — so a card without it must not show the button, and the
 * verb would be refused if it did.
 *
 * THE CARD'S NEW STATE ARRIVES AS A RE-PUSH, NEVER FROM THE RESPONSE. The
 * success arm is empty on purpose: the feed is the one authority on what the
 * card now says, and drawing "allowed" from the answer would be a second
 * authority that could disagree with the first. So a landed answer draws
 * nothing here and leaves the buttons latched — the row is about to be replaced.
 *
 * A DENIAL IS AN ANSWER, NOT AN ERROR. Both denied arms are success states of
 * the ask, and the policy denial is worded by the daemon precisely so it never
 * reads as the user's own act.
 *
 * THE CARD IS A TOOL CARD (owner ruling, 2026-10-11), AND HEADS AS ONE
 * (owner ruling, 2026-10-13): the grey `.tool-card` chrome a Bash call wears,
 * and the one tool card head (`drawToolCardHead`) -- the vendor's headline
 * where a call names its tool, the ask's state as its badge ("waiting", or the
 * verdict), and the head's clock, which counts the wait while the ask stands
 * and freezes at the answer.
 *
 * EVERY BUTTON IS NUMBERED, `1.` first in drawn order, because a number typed
 * into the editor's empty composer presses the button it names
 * (agentrepl.v1.HostAskPermission states the order: allow once, always allow
 * when offered, deny).
 *
 * A DENY WITH A REASON IS TYPED IN THE EDITOR. The card's deny button refuses
 * with no reason; a plain send made while the card stands denies it with the
 * text as the reason relayed to the agent (agentrepl.v1.
 * SubmitPromptAskAnswered), and the card says so. It has no text field.
 */
import { drawToolCardBadge, drawToolCardHead } from "../cards/tool-head.js";
import { ASK_WAITING_WORD } from "./ask-words.js";
import { drawAskHint } from "./hint.js";
import { drawChoiceNumeral } from "./choice-number.js";
import { createControl, type Control } from "../../control.js";
import { log } from "../../log.js";
import {
  AnswerPermissionResponseSchema,
  type AnswerPermissionResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_permission_pb";
import type {
  FeedPermission,
  FeedPermissionAbandoned,
  FeedPermissionAnswered,
  FeedPermissionArguments,
  FeedPermissionHeadline,
  FeedPermissionSubtitle,
  FeedPermissionTriggerNote,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { callUnary } from "../../rpc/unary.js";
import {
  clearRefusals,
  drawMalformedRefusal,
  refusal,
  whileInFlight,
} from "../cards/controls.js";
import { refusalOf, type SentenceTable } from "../../rpc/refuse.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { buildAnswerPermissionRequest, type PermissionAnswer } from "./requests.js";

const PATH = "FeedPermission";

/** The three buttons, in the order they are drawn. */
export const PERMISSION_BUTTONS = ["allowOnce", "allowStanding", "deny"] as const;

/** What each button says. */
const BUTTON_LABELS = {
  allowOnce: "allow once",
  allowStanding: "always allow",
  deny: "deny",
} as const satisfies Record<(typeof PERMISSION_BUTTONS)[number], string>;

/** Where a deny's reason is entered: the card says so under its buttons. */
export const DENY_IN_EDITOR_HINT = "to deny with a reason, type it in Emacs and send it";

/** The verdict line each allowing arm draws. */
const VERDICT_WORDS = {
  allowedOnce: "allowed once",
  allowedStanding: "allowed with standing",
} as const satisfies Record<string, string>;

/** Every answered arm this build draws, for the suite to hold to the schema. */
export const PERMISSION_ANSWERED_ARMS: readonly string[] = [
  ...Object.keys(VERDICT_WORDS),
  "deniedByUser",
  "deniedByPolicy",
  "deniedUndecidable",
];

/** What the badge of a card nobody answered says. */
export const ABANDONED_WORD = "went away unanswered";

/** The attribute the verdict badge names its answered arm with. */
export const VERDICT_ATTRIBUTE = "data-permission-verdict";

/** The consent card. */
export function drawFeedPermission(u: FeedPermission, rc: RowContext): HTMLElement {
  const state = requireCase(u.state, `${PATH}.state`);
  log.debug("drawing a permission card", {
    operation: "feed.asks.permission",
    context: { state: state.case, standing: u.standingOffered !== undefined },
  });

  const card = document.createElement("div");
  card.setAttribute("data-state", state.case);

  card.append(
    drawToolCardHead({
      title: drawFeedPermissionHeadline(requireMessage(u.headline, `${PATH}.headline`), `${PATH}.headline`),
      badge: drawFeedPermissionBadge(u),
      clock: u.clock,
      clockPath: `${PATH}.clock`,
      ticker: rc.ctx.ticker,
      classes: ["perm-head"],
    }),
  );

  if (u.subtitle !== undefined) {
    card.append(drawFeedPermissionSubtitle(u.subtitle, `${PATH}.subtitle`));
  }
  if (u.trigger !== undefined) {
    card.append(drawFeedPermissionTriggerNote(u.trigger, `${PATH}.trigger`));
  }
  card.append(
    drawFeedPermissionArguments(
      requireMessage(u.arguments, `${PATH}.arguments`),
      `${PATH}.arguments`,
    ),
  );

  switch (state.case) {
    case "open":
      card.className = "tool-card ask-card permission pending";
      card.append(drawOpenActions(u, rc));
      card.append(drawAskHint(DENY_IN_EDITOR_HINT));
      return card;
    case "answered": {
      card.className = "tool-card ask-card permission resolved";
      // An answered card's STATE is the answer given: "answered" alone says
      // only that the ask is over, never whether it was allowed or refused.
      const answer = requireCase(state.value.answer, `${PATH}.answered.answer`);
      card.setAttribute("data-state", answer.case);
      return card;
    }
    case "abandoned":
      card.className = "tool-card ask-card permission resolved";
      return card;
    default:
      return unreachableArm(`${PATH}.state`, armName(state));
  }
}

/** The vendor's rendered sentence, verbatim. */
export function drawFeedPermissionHeadline(
  u: FeedPermissionHeadline,
  path: string,
): HTMLElement {
  log.debug("drawing a permission headline", {
    operation: "feed.asks.permission.headline",
    context: { path },
  });
  const el = document.createElement("span");
  el.className = "perm-headline";
  el.textContent = u.text;
  return el;
}

/** The qualifying subtitle, verbatim. */
export function drawFeedPermissionSubtitle(
  u: FeedPermissionSubtitle,
  path: string,
): HTMLElement {
  log.debug("drawing a permission subtitle", {
    operation: "feed.asks.permission.subtitle",
    context: { path },
  });
  const el = document.createElement("div");
  el.className = "perm-subtitle";
  el.textContent = u.text;
  return el;
}

/**
 * Why the gate fired, composed daemon-side and drawn VERBATIM.
 *
 * An ask-rule trigger is worded so a reader knows the prompt was
 * user-configured and must not be auto-approved. This end therefore neither
 * summarizes it nor decorates it with a judgment of its own.
 */
export function drawFeedPermissionTriggerNote(
  u: FeedPermissionTriggerNote,
  path: string,
): HTMLElement {
  log.debug("drawing a permission trigger note", {
    operation: "feed.asks.permission.trigger",
    context: { path },
  });
  const el = document.createElement("div");
  el.className = "perm-trigger";
  el.textContent = u.text;
  return el;
}

/** The gated call's argument preview: one composed line each, in order. */
export function drawFeedPermissionArguments(
  u: FeedPermissionArguments,
  path: string,
): HTMLElement {
  log.debug("drawing a permission argument preview", {
    operation: "feed.asks.permission.arguments",
    context: { path, lines: u.lines.length },
  });
  const el = document.createElement("div");
  el.className = "perm-args list-rows";
  for (const line of u.lines) {
    const row = document.createElement("div");
    row.className = "perm-arg";
    row.textContent = line;
    el.append(row);
  }
  return el;
}

/** The head's badge: the ask's state, in the consent card's own words. */
export function drawFeedPermissionBadge(u: FeedPermission): HTMLElement {
  const state = requireCase(u.state, `${PATH}.state`);
  switch (state.case) {
    case "open":
      return drawToolCardBadge(ASK_WAITING_WORD);
    case "answered":
      return drawFeedPermissionAnswered(state.value, `${PATH}.answered`);
    case "abandoned":
      return drawFeedPermissionAbandoned(state.value, `${PATH}.abandoned`);
    default:
      return unreachableArm(`${PATH}.state`, armName(state));
  }
}

/** The answered state's badge: the verdict. */
export function drawFeedPermissionAnswered(u: FeedPermissionAnswered, path: string): HTMLElement {
  const answer = requireCase(u.answer, `${path}.answer`);
  log.debug("drawing an answered permission", {
    operation: "feed.asks.permission.answered",
    context: { path, answer: answer.case },
  });
  const verdict = (word: string, tones: readonly string[]): HTMLElement => {
    const badge = drawToolCardBadge(word, tones, answer.case);
    badge.setAttribute(VERDICT_ATTRIBUTE, answer.case);
    return badge;
  };
  switch (answer.case) {
    case "allowedOnce":
    case "allowedStanding":
      return verdict(VERDICT_WORDS[answer.case], ["ok"]);
    case "deniedByUser":
      // A DENIAL BY THE USER IS AN ANSWER, NOT AN ERROR (owner ruling,
      // 2026-10-06): its badge is neutral, never red, and says it in the
      // daemon's own words.
      return verdict(
        requireMessage(
          requireMessage(answer.value.marker, `${path}.denied_by_user.marker`).label,
          `${path}.denied_by_user.marker.label`,
        ).text,
        ["muted"],
      );
    case "deniedByPolicy":
      // The daemon's own wording, so a policy refusal never reads as the user's
      // act — which is the whole reason this arm carries text at all.
      return verdict(answer.value.text, ["err"]);
    case "deniedUndecidable":
      // ITS OWN ARM, NOT A POLICY DENIAL. Nobody decided — no rule refused and
      // no user refused — so it carries its own verdict value and its own
      // class, and like the policy arm its wording is the daemon's, verbatim,
      // so it can never read as the user's act.
      return verdict(answer.value.text, ["err", "arm-deniedUndecidable"]);
    default:
      return unreachableArm(`${path}.answer`, armName(answer));
  }
}

/** The abandoned state's badge: the ask went away unanswered. */
export function drawFeedPermissionAbandoned(_u: FeedPermissionAbandoned, path: string): HTMLElement {
  log.debug("drawing an abandoned permission", {
    operation: "feed.asks.permission.abandoned",
    context: { path },
  });
  return drawToolCardBadge(ABANDONED_WORD, ["muted"], "abandoned");
}

/** The open state's buttons. */
function drawOpenActions(u: FeedPermission, rc: RowContext): HTMLElement {
  const actions = document.createElement("div");
  actions.className = "perm-actions";

  const buttons: Control[] = [];
  for (const kind of PERMISSION_BUTTONS) {
    // The standing button exists ONLY when the vendor offered a standing form:
    // presence of `standing_offered` is what makes it drawable, and the verb
    // would be refused for a card that never carried one.
    if (kind === "allowStanding" && u.standingOffered === undefined) continue;
    const button = createControl();
    button.className = `perm-button perm-${kind}`;
    button.setAttribute("data-permission", kind);
    // NUMBERED IN DRAWN ORDER, the order agentrepl.v1.HostAskPermission
    // states, because a number typed into the editor's empty composer presses
    // the button it names.
    const label = document.createElement("span");
    label.className = "perm-label";
    label.textContent = BUTTON_LABELS[kind];
    button.append(drawChoiceNumeral(buttons.length + 1, "perm-number", "data-permission-number"), label);
    button.addEventListener("click", () => {
      void answer(rc, actions, buttons, { kind });
    });
    buttons.push(button);
    actions.append(button);
  }
  return actions;
}


/** Send the answer, and draw a refusal at the buttons if it was refused. */
async function answer(
  rc: RowContext,
  actions: HTMLElement,
  buttons: readonly Control[],
  chosen: PermissionAnswer,
): Promise<void> {
  const id = requireMessage(rc.row.id, "FeedRow.id");
  clearRefusals(actions);
  log.info(`answering a permission card with ${chosen.kind}`, {
    operation: "feed.asks.permission.answer",
    context: { row: id.value, answer: chosen.kind },
  });
  const answered = await whileInFlight(buttons, () =>
    callUnary(
      rc.ctx,
      "AnswerPermission",
      (client) =>
        client.answerPermission(
          buildAnswerPermissionRequest(rc.ctx.workspace, id, chosen),
        ),
      AnswerPermissionResponseSchema,
    ),
  );
  if ("failed" in answered) {
    // callUnary already logged the transport failure once, as its owner; what
    // is left is telling the reader their click did not land.
    actions.append(refusal("transport", "the daemon could not be reached"));
    return;
  }
  try {
    drawAnswerOutcome(answered.value, actions, buttons);
  } catch (err) {
    // A refusal this build cannot read is still a failure the reader owns; it is
    // stated at the control and reported once rather than becoming an unhandled
    // rejection inside a click handler.
    if (!drawMalformedRefusal(rc.ctx, actions, "feed.asks.permission.malformed-refusal", err)) throw err;
  }
}

/**
 * The causes only THIS verb can answer with. The four cross-cutting ones are
 * worded once, in `src/rpc/refuse.ts`, so they read identically everywhere.
 */
const OWN_CAUSES = {
  askNotStanding: () => "this ask is no longer standing",
  noStandingOffer: () => "no standing allow was offered for this call",
  noSession: () => "the workspace has no session to answer",
} as unknown as SentenceTable;

/**
 * What came back: nothing to draw on success, this click's own refusal on error.
 *
 * The cause is TYPED, so the refusal names the arm and says what it means. An
 * unset cause is a malformed view, not a generic "it was refused".
 */
function drawAnswerOutcome(
  response: AnswerPermissionResponse,
  actions: HTMLElement,
  buttons: readonly Control[],
): void {
  const result = requireCase(response.result, "AnswerPermissionResponse.result");
  switch (result.case) {
    case "success":
      // The card's new state is the row's re-push. Nothing is drawn here, and
      // the buttons stay latched: this ask has been answered.
      return;
    case "error": {
      const said = refusalOf(result.value.cause, OWN_CAUSES, "AnswerPermissionError.cause");
      actions.append(refusal(said.arm, said.text));
      // The buttons come back: every cause here is one the reader may be able to
      // act on (reconnect, reconcile, pick the other button), and a card that
      // refused a click while staying inert would be a dead end.
      for (const button of buttons) button.disabled = false;
      return;
    }
    default:
      unreachableArm("AnswerPermissionResponse.result", armName(result));
  }
}
