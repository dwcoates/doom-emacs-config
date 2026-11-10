/**
 * response-question — EVERY CARD'S QUESTION-MARK CONTROL: ask a question about
 * the card, answered in a workspace of its own.
 *
 * Every card the feed draws carries this control (owner request, 2026-11-09,
 * extending it from final responses), through ONE helper: `withCardQuestion`
 * wraps every card renderer the registry holds (renderers.ts), so a card kind
 * added without it fails the registry's test. A final response that carries a
 * copy control (cards/response.ts) draws it in its own corner, just left of
 * the copy control; every other card wears it at its top right. A click asks
 * the daemon to toggle the workspace's question about that card
 * (agentrepl.v1.AskAboutResponse):
 * while a question stands, Emacs's composer has set its draft aside and the
 * next prompt sent is that question, sent to the home repository's main
 * workspace. A second click withdraws the question, and Emacs puts the draft
 * back.
 *
 * THE DAEMON IS THE ONLY HOLDER of the question. This module never presses or
 * releases a control on its own: a click only asks, and the root feed's watch
 * pushes what stands (frontend.v1.FeedResponseQuestion), which
 * `applyResponseQuestion` draws on every control of the page. A control drawn
 * later (a response's every repaint draws its corner afresh) reads what stands
 * as it is made, so a repaint never drops the pressed look.
 *
 * It is an `<ar-button>` (control.ts), so a click on it never selects, expands
 * or folds the bubble it sits in.
 */
import { create } from "@bufbuild/protobuf";
import {
  AskAboutResponseRequestSchema,
  AskAboutResponseResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_ask_about_response_pb";
import type { FeedResponseQuestion } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { RowContext } from "./cards/context.js";
import { queryAll } from "../dom.js";
import { log } from "../log.js";
import { perDocument } from "../per-document.js";
import type { AppContext } from "../rpc/context.js";
import { controlCall } from "../rpc/control-call.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";

/** The class every question control wears. */
export const QUESTION_CONTROL_CLASS = "question-control";

/** The class placing a bubble's question control beside its copy control. */
export const BUBBLE_QUESTION_CLASS = "bubble-question";

/** The class placing every other card's question control at its top right. */
export const CARD_QUESTION_CLASS = "card-question";

/** The attribute naming the response row a question control asks about. */
export const QUESTION_ROW_ATTRIBUTE = "data-asks-about";

/** The attribute a question control wears while the question about its response stands. */
export const ASKING_ATTRIBUTE = "data-asking";

/** AskAboutResponse's own refusal arm, in the reader's words. */
const ASK_ABOUT_RESPONSE_REFUSALS = {
  notACard: () => "the card is no longer in the feed",
};

/** A question mark in a circle: the control's icon. */
const QUESTION_ICON =
  `<svg class="question-control-icon" viewBox="0 0 16 16" aria-hidden="true">` +
  `<circle cx="8" cy="8" r="6.25"/>` +
  `<path d="M6.1 6.2a1.95 1.95 0 0 1 3.8 0.55c0 1.3-1.9 1.6-1.9 2.85"/>` +
  `<circle class="question-control-dot" cx="8" cy="11.6" r="0.55"/>` +
  `</svg>`;

/** The label a control wears at rest, and while its question stands. */
const ASK_LABEL = "Ask a question about this card";
const WITHDRAW_LABEL = "Withdraw the question about this card";

/** The row each page's question stands about, as the daemon last pushed it; null for none. */
const standing = perDocument((): { row: string | null } => ({ row: null }));

/** Draw CONTROL pressed or at rest, by whether the question about its row stands on its page. */
function drawControl(control: Element, askedRow: string | null): void {
  const asking = askedRow !== null && control.getAttribute(QUESTION_ROW_ATTRIBUTE) === askedRow;
  control.toggleAttribute(ASKING_ATTRIBUTE, asking);
  control.setAttribute("aria-pressed", String(asking));
  const label = asking ? WITHDRAW_LABEL : ASK_LABEL;
  control.setAttribute("title", label);
  control.setAttribute("aria-label", label);
}

/**
 * The question control for the card ROW on DOC (the page's own document by
 * default), placed by PLACEMENT (a bubble's corner by default), drawn as the
 * question standing on DOC says.
 */
export function questionControl(
  row: string,
  doc: Document = document,
  placement: string = BUBBLE_QUESTION_CLASS,
): HTMLElement {
  if (row === "") throw new Error("a question control names no card row");
  const control = doc.createElement("ar-button");
  control.className = `${QUESTION_CONTROL_CLASS} ${placement}`;
  control.setAttribute(QUESTION_ROW_ATTRIBUTE, row);
  control.innerHTML = QUESTION_ICON;
  drawControl(control, standing(doc).row);
  return control;
}

/** A card renderer: a message and its row's context to the card's element. */
type CardRenderer<U> = (unit: U, rc: RowContext) => HTMLElement;

/** Every renderer `withCardQuestion` made, so the registry's test can ask. */
const carrying = new WeakSet<object>();

/** Whether RENDER is a card renderer `withCardQuestion` made. */
export function carriesCardQuestion(render: unknown): boolean {
  return typeof render === "function" && carrying.has(render);
}

/**
 * THE ONE PATH BY WHICH A CARD CARRIES ITS QUESTION CONTROL: RENDER, with the
 * card it draws given the control for its row (`placeCardQuestion`).
 */
export function withCardQuestion<U>(render: CardRenderer<U>): CardRenderer<U> {
  const wrapped: CardRenderer<U> = (unit, rc) => {
    const card = render(unit, rc);
    placeCardQuestion(card, requireRow(rc));
    return card;
  };
  carrying.add(wrapped);
  return wrapped;
}

/** The id of the row RC draws; a row without one cannot be asked about. */
function requireRow(rc: RowContext): string {
  const row = rc.row.id?.value ?? "";
  if (row === "") throw new Error("a card's row names no id, so its question control has nothing to ask about");
  return row;
}

/**
 * Give CARD the question control for ROW. A card that placed its own in its
 * corner (a final response, beside its copy control) keeps it; any other card
 * wears it at its top right, restated on a card its renderer redrew in place.
 * A response redrawn in place as it settles gains its corner control, and the
 * top-right one its earlier draw wore leaves.
 */
export function placeCardQuestion(card: HTMLElement, row: string): void {
  const placed = card.querySelector(`:scope > .${CARD_QUESTION_CLASS}`);
  const own = card.querySelector(`.${BUBBLE_QUESTION_CLASS}`);
  if (own !== null) {
    if (own.getAttribute(QUESTION_ROW_ATTRIBUTE) !== row) {
      throw new Error(`a card's corner asks about ${own.getAttribute(QUESTION_ROW_ATTRIBUTE) ?? "nothing"}, not its own row ${row}`);
    }
    placed?.remove();
    return;
  }
  if (placed !== null) {
    placed.setAttribute(QUESTION_ROW_ATTRIBUTE, row);
    drawControl(placed, standing(card.ownerDocument).row);
    return;
  }
  card.append(questionControl(row, card.ownerDocument, CARD_QUESTION_CLASS));
}

/** The row QUESTION asks about, or null for none. A push with no state, or an ask naming no row, is refused. */
function askedRowOf(question: FeedResponseQuestion): string | null {
  const state = requireCase(question.state, "FeedResponseQuestion.state");
  switch (state.case) {
    case "none":
      return null;
    case "asking":
      return requireMessage(state.value.row, "FeedResponseQuestionAsking.row").value;
    default: {
      const other: { case: string } = state;
      return unreachableArm("FeedResponseQuestion.state", other.case);
    }
  }
}

/** Draw the pushed QUESTION on every question control of DOC, and on every one drawn later. */
export function applyResponseQuestion(doc: Document, question: FeedResponseQuestion): void {
  const row = askedRowOf(question);
  standing(doc).row = row;
  const controls = queryAll<HTMLElement>(doc, `.${QUESTION_CONTROL_CLASS}`);
  for (const control of controls) drawControl(control, row);
  log.debug("drew the question about a card", {
    operation: "feed.response-question.applied",
    context: { row, controls: controls.length },
  });
}

/** Ask the daemon to toggle the question about ROW; a refusal or a transport failure reaches the chip. */
async function askAboutResponse(ctx: AppContext, row: string): Promise<void> {
  const success = await controlCall(ctx, {
    rpc: "AskAboutResponse",
    what: "toggle the question about the card",
    send: (client) =>
      client.askAboutResponse(create(AskAboutResponseRequestSchema, { workspace: ctx.workspace, row: { value: row } })),
    schema: AskAboutResponseResponseSchema,
    result: (response) => response.result,
    refusals: ASK_ABOUT_RESPONSE_REFUSALS,
    refusedOperation: "feed.response-question.refused",
    context: { row },
  });
  if (success === undefined) return;
  log.debug("the daemon toggled the question about a card", {
    operation: "feed.response-question.answered",
    context: { row },
  });
}

/**
 * Answer a click on any question control inside HOST: ask the daemon to toggle
 * the question about the control's response. Answers the uninstall.
 */
export function installResponseQuestion(host: HTMLElement, ctx: AppContext): () => void {
  const onClick = (event: MouseEvent): void => {
    if (!(event.target instanceof Element)) return;
    const control = event.target.closest(`.${QUESTION_CONTROL_CLASS}`);
    if (control === null || !host.contains(control)) return;
    const row = control.getAttribute(QUESTION_ROW_ATTRIBUTE);
    if (row === null || row === "") throw new Error("a question control names no card row");
    log.info("the reader pressed a card's question control", {
      operation: "feed.response-question.pressed",
      context: { row, asking: control.hasAttribute(ASKING_ATTRIBUTE) },
    });
    void askAboutResponse(ctx, row);
  };
  host.addEventListener("click", onClick);
  return () => host.removeEventListener("click", onClick);
}
