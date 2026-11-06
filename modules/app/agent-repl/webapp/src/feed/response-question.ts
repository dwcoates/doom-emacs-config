/**
 * response-question — A FINAL RESPONSE'S QUESTION-MARK CONTROL: ask a
 * question about the response, answered in a workspace of its own.
 *
 * Every final response that carries a copy control (cards/response.ts) also
 * carries this control, just left of it. A click asks the daemon to toggle the
 * workspace's question about that response (agentrepl.v1.AskAboutResponse):
 * while a question stands, Emacs's composer has set its draft aside and the
 * next prompt sent is that question, forked into a child workspace that
 * answers it. A second click withdraws the question, and Emacs puts the draft
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

/** The attribute naming the response row a question control asks about. */
export const QUESTION_ROW_ATTRIBUTE = "data-asks-about";

/** The attribute a question control wears while the question about its response stands. */
export const ASKING_ATTRIBUTE = "data-asking";

/** AskAboutResponse's own refusal arm, in the reader's words. */
const ASK_ABOUT_RESPONSE_REFUSALS = {
  notAFinalResponse: () => "the response is not a settled final response",
};

/** A question mark in a circle: the control's icon. */
const QUESTION_ICON =
  `<svg class="question-control-icon" viewBox="0 0 16 16" aria-hidden="true">` +
  `<circle cx="8" cy="8" r="6.25"/>` +
  `<path d="M6.1 6.2a1.95 1.95 0 0 1 3.8 0.55c0 1.3-1.9 1.6-1.9 2.85"/>` +
  `<circle class="question-control-dot" cx="8" cy="11.6" r="0.55"/>` +
  `</svg>`;

/** The label a control wears at rest, and while its question stands. */
const ASK_LABEL = "Ask a question about this response";
const WITHDRAW_LABEL = "Withdraw the question about this response";

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
 * The question control for the final response ROW on DOC (the page's own
 * document by default), drawn as the question standing on DOC says.
 */
export function questionControl(row: string, doc: Document = document): HTMLElement {
  if (row === "") throw new Error("a question control names no response row");
  const control = doc.createElement("ar-button");
  control.className = `${QUESTION_CONTROL_CLASS} ${BUBBLE_QUESTION_CLASS}`;
  control.setAttribute(QUESTION_ROW_ATTRIBUTE, row);
  control.innerHTML = QUESTION_ICON;
  drawControl(control, standing(doc).row);
  return control;
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
  log.debug("drew the question about a final response", {
    operation: "feed.response-question.applied",
    context: { row, controls: controls.length },
  });
}

/** Ask the daemon to toggle the question about ROW; a refusal or a transport failure reaches the chip. */
async function askAboutResponse(ctx: AppContext, row: string): Promise<void> {
  const success = await controlCall(ctx, {
    rpc: "AskAboutResponse",
    what: "toggle the question about the response",
    send: (client) =>
      client.askAboutResponse(create(AskAboutResponseRequestSchema, { workspace: ctx.workspace, row: { value: row } })),
    schema: AskAboutResponseResponseSchema,
    result: (response) => response.result,
    refusals: ASK_ABOUT_RESPONSE_REFUSALS,
    refusedOperation: "feed.response-question.refused",
    context: { row },
  });
  if (success === undefined) return;
  log.debug("the daemon toggled the question about a final response", {
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
    if (row === null || row === "") throw new Error("a question control names no response row");
    log.info("the reader pressed a response's question control", {
      operation: "feed.response-question.pressed",
      context: { row, asking: control.hasAttribute(ASKING_ATTRIBUTE) },
    });
    void askAboutResponse(ctx, row);
  };
  host.addEventListener("click", onClick);
  return () => host.removeEventListener("click", onClick);
}
