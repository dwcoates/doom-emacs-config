/**
 * question — THE QUESTION CARD: the turn blocked on a choice the agent posed.
 *
 * THE CARD IS A TOOL CARD (owner ruling, 2026-10-11). It wears the grey
 * `.tool-card` chrome a Bash call wears, and a batch's questions are the SAME
 * tabs a run of tool calls is shown behind (src/feed/tabs.ts), so a question
 * reads as one more call the agent made rather than a panel of its own.
 *
 * THE DAEMON OWNS THE DRAFT. Which question is shown and what each has been
 * given so far arrive on the row (frontend.v1.FeedQuestionOpen), and every
 * click is an `UpdateQuestionDraft` the daemon applies and re-pushes: a tab
 * shows its question, an option click answers a single-select (a second click
 * clears it) or ticks a multi-select, and a multi-select's confirm answers it.
 * Nothing is held here, so nothing here can disagree with the record.
 *
 * THERE IS NO SUBMIT. The click that answers the batch's last question is the
 * one the daemon delivers the batch on, and the card's settled state arrives as
 * the row's re-push.
 *
 * A TYPED ANSWER IS THE EDITOR'S. The free-text "Other" escape is always
 * available, and it is typed in the editor's composer: a plain send made while
 * the card stands answers its shown question (agentrepl.v1.
 * SubmitPromptAskAnswered). The card draws what was typed; it has no text field
 * of its own.
 *
 * AN ANSWER IS SEEN THE MOMENT IT IS GIVEN. Answering a batch's question moves
 * the card on to the next unanswered one, which would hide what was just sent
 * behind its tab; so a batch's open card draws every answered question but the
 * shown one as a verdict line, the settled card's own (typed text italic).
 *
 * EVERY OPTION IS NUMBERED, `1.` first in the served order, because a send of
 * only option numbers picks by them: the daemon reads it (feed/questiondraft.go)
 * as the shown question's choice rather than as typed text.
 *
 * SERVED STRINGS ARE DRAWN AS INLINE MARKDOWN (setInlineMarkdown), as a response
 * bubble draws its prose, so a `code span` in a question reads as code.
 *
 * EVERY VALUE IS ECHOED VERBATIM — the question's own text names the question,
 * an option's own label names the choice. The daemon reconstructs the ask from
 * the values it served, so a label this end trimmed or re-cased is a label it
 * can no longer match. Markdown is presentation only: what a click echoes is
 * the served string, never the drawn one.
 */
import { drawToolCardBadge, drawToolCardHead } from "../cards/tool-head.js";
import { ASK_WAITING_WORD } from "./ask-words.js";
import { drawChoiceNumeral } from "./choice-number.js";
import { createControl, type Control } from "../../control.js";
import { create } from "@bufbuild/protobuf";
import { log } from "../../log.js";
import { setInlineMarkdown } from "../../markdown.js";
import {
  UpdateQuestionDraftResponseSchema,
  type UpdateQuestionDraftResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_update_question_draft_pb";
import {
  FeedQuestionGivenAnswerSchema,
  type FeedQuestion,
  type FeedQuestionAnswered,
  type FeedQuestionDraft,
  type FeedQuestionGivenAnswer,
  type FeedQuestionHeader,
  type FeedQuestionItem,
  type FeedQuestionOpen,
  type FeedQuestionOption,
  type FeedQuestionText,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../rpc/malformed.js";
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
import { drawFeedTab, drawFeedTabStrip } from "../tabs.js";
import { buildUpdateQuestionDraftRequest, type QuestionDraftEdit } from "./requests.js";

const PATH = "FeedQuestion";

/** The card's head, where a tool card names its tool. */
export const QUESTION_TITLE = "Question";

/** What a multi-select's confirm control says. */
export const CONFIRM_TEXT = "done";

/** The label in front of an answer typed in the editor. */
export const TYPED_LABEL = "typed:";

/** What an expired card's badge says. */
export const EXPIRED_TEXT = "expired unanswered";

/** What an answered card's badge says. */
export const ANSWERED_TEXT = "answered";

/** The glyph an answered question's tab wears, and an unanswered one's. */
export const TAB_GLYPHS = { answered: "✓", pending: "?" } as const;

/** Every state this build draws, for the suite to hold to the schema. */
export const QUESTION_STATE_ARMS: readonly string[] = ["open", "answered", "expired"];

/** The question card. */
export function drawFeedQuestion(u: FeedQuestion, rc: RowContext): HTMLElement {
  const state = requireCase(u.state, `${PATH}.state`);
  log.debug("drawing a question card", {
    operation: "feed.asks.question",
    context: { state: state.case, questions: u.questions.length },
  });

  const card = document.createElement("div");
  card.setAttribute("data-state", state.case);
  card.append(
    drawToolCardHead({
      title: QUESTION_TITLE,
      badge: drawFeedQuestionBadge(state.case),
      clock: u.clock,
      clockPath: `${PATH}.clock`,
      ticker: rc.ctx.ticker,
    }),
  );

  switch (state.case) {
    case "open":
      card.className = "tool-card ask-card question pending";
      card.append(drawOpenBatch(u, state.value, rc));
      return card;
    case "answered":
      card.className = "tool-card ask-card question resolved";
      card.append(drawFeedQuestionAnswered(state.value, `${PATH}.answered`));
      return card;
    case "expired":
      // The badge says it all: an expired ask drew no answers.
      card.className = "tool-card ask-card question resolved";
      return card;
    default:
      return unreachableArm(`${PATH}.state`, armName(state));
  }
}

/**
 * THE OPEN BATCH: the tab strip (a batch of two or more), the shown question's
 * panel, and the slot a refused click is stated in.
 *
 * Every question is still VALIDATED up front, shown or not: a malformed
 * question in tab three refuses the card rather than waiting to be clicked.
 */
function drawOpenBatch(u: FeedQuestion, open: FeedQuestionOpen, rc: RowContext): HTMLElement {
  const wrap = document.createElement("div");
  wrap.className = "q-tabbed";

  if (open.drafts.length !== u.questions.length) {
    // The daemon states one draft per question; a count that disagrees is a
    // view this build cannot draw honestly.
    throw new MalformedView(
      `${PATH}.open.drafts`,
      `${String(open.drafts.length)} drafts for ${String(u.questions.length)} questions`,
    );
  }
  if (open.active >= u.questions.length) {
    throw new MalformedView(
      `${PATH}.open.active`,
      `${String(open.active)} names no question of ${String(u.questions.length)}`,
    );
  }

  const status = document.createElement("div");
  status.className = "perm-actions q-status";
  const controls: Control[] = [];
  const edit = (change: QuestionDraftEdit): void => {
    void send(rc, status, controls, change);
  };

  const panels = u.questions.map((item, index) =>
    drawFeedQuestionItem(item, index, `${PATH}.questions[${index}]`, {
      draft: open.drafts[index],
      edit,
      controls,
    }),
  );

  if (u.questions.length > 1) {
    const strip = drawFeedTabStrip("q-tabs");
    strip.setAttribute("data-question-tabs", "");
    u.questions.forEach((item, index) => {
      const answered = open.drafts[index].answered;
      const text = requireMessage(item.text, `${PATH}.questions[${index}].text`).text;
      const tab = drawFeedTab({
        icon: answered ? TAB_GLYPHS.answered : TAB_GLYPHS.pending,
        index: index + 1,
        label: panels[index].header,
        active: index === open.active,
        onPick: () => edit({ kind: "show", questionText: text }),
      });
      tab.setAttribute("data-question-tab", String(index));
      tab.setAttribute("data-answered", answered ? "true" : "false");
      controls.push(tab);
      strip.append(tab);
    });
    wrap.append(strip);
    const given = drawGivenSoFar(u.questions, open);
    if (given !== null) wrap.append(given);
  }

  const shown = document.createElement("div");
  shown.className = "q-panels";
  panels.forEach((panel, index) => {
    panel.el.classList.add("q-panel");
    panel.el.setAttribute("data-question-panel", String(index));
    panel.el.hidden = index !== open.active;
    shown.append(panel.el);
  });
  wrap.append(shown);

  wrap.append(status);
  return wrap;
}

/**
 * THE ANSWERS GIVEN SO FAR: one verdict line per answered question other than
 * the shown one (whose panel already draws its answer), in the batch's order,
 * drawn exactly as the settled card draws it. Null when there is none.
 */
function drawGivenSoFar(items: readonly FeedQuestionItem[], open: FeedQuestionOpen): HTMLElement | null {
  const lines = items.flatMap((item, index) => {
    const draft = open.drafts[index];
    if (index === open.active || !draft.answered) return [];
    const path = `${PATH}.questions[${index}]`;
    const given = create(FeedQuestionGivenAnswerSchema, {
      header: requireMessage(item.header, `${path}.header`),
      chosen: draft.chosen,
      otherText: draft.otherText,
    });
    const line = drawFeedQuestionGivenAnswer(given, `${PATH}.open.drafts[${index}]`);
    line.setAttribute("data-question-given", String(index));
    return [line];
  });
  if (lines.length === 0) return null;
  const el = document.createElement("div");
  el.className = "q-verdicts q-given list-rows";
  el.setAttribute("data-question-given-so-far", "");
  el.append(...lines);
  return el;
}

/** One drawn question: its panel and the header its tab is labelled with. */
interface DrawnItem {
  el: HTMLElement;
  header: string;
}

/** What one question's panel draws from and sends through. */
export interface QuestionItemContext {
  /** What the question has been given so far, as the daemon states it. */
  draft: FeedQuestionDraft;
  /** Sends one click to the daemon. */
  edit: (change: QuestionDraftEdit) => void;
  /** Every control of the card, latched together while a click is in flight. */
  controls: Control[];
}

/**
 * One question: its text, its options in the served order with the draft's
 * choices marked, what was typed for it, and a multi-select's confirm.
 */
export function drawFeedQuestionItem(
  u: FeedQuestionItem,
  index: number,
  path: string,
  ctx: QuestionItemContext,
): DrawnItem {
  const options = requireCase(u.options, `${path}.options`);
  const text = requireMessage(u.text, `${path}.text`);
  const header = requireMessage(u.header, `${path}.header`);
  log.debug("drawing a question item", {
    operation: "feed.asks.question.item",
    context: { path, mode: options.case, options: options.value.options.length },
  });
  // THE ARM PICKS THE MODE. Checked before anything is drawn from it, so an arm
  // a newer daemon set reaches the refusal that quotes its name.
  if (options.case !== "singleSelect" && options.case !== "multiSelect") {
    return unreachableArm(`${path}.options`, armName(options));
  }

  const el = document.createElement("div");
  el.className = "q-block";
  el.setAttribute("data-question", String(index));
  el.setAttribute("data-question-mode", options.case);
  el.setAttribute("data-answered", ctx.draft.answered ? "true" : "false");
  el.append(drawFeedQuestionText(text, `${path}.text`));

  const list = document.createElement("div");
  list.className = "q-opts";
  const chosen = new Set(ctx.draft.chosen);
  options.value.options.forEach((option, optionIndex) => {
    const optionPath = `${path}.options.options[${optionIndex}]`;
    const drawn = drawFeedQuestionOption(option, optionIndex + 1, chosen, optionPath);
    const label = requireMessage(option.label, `${optionPath}.label`).text;
    drawn.addEventListener("click", () => ctx.edit({ kind: "pick", questionText: text.text, label }));
    ctx.controls.push(drawn);
    list.append(drawn);
  });
  el.append(list);

  if (ctx.draft.otherText !== undefined) {
    el.append(drawTyped(ctx.draft.otherText.text));
  }

  // A MULTI-SELECT HAS NO MOMENT ITS PICKING IS FINISHED, so the reader says so:
  // a tick is not an answer, and this is.
  if (options.case === "multiSelect") {
    const confirm = createControl();
    confirm.className = "q-confirm";
    confirm.setAttribute("data-question-confirm", "");
    confirm.textContent = CONFIRM_TEXT;
    confirm.addEventListener("click", () => ctx.edit({ kind: "confirm", questionText: text.text }));
    ctx.controls.push(confirm);
    el.append(confirm);
  }
  return { el, header: header.text };
}

/** What was typed in the editor for a question, verbatim. */
function drawTyped(typed: string): HTMLElement {
  const el = document.createElement("div");
  el.className = "q-typed";
  el.setAttribute("data-question-typed", "");
  const label = document.createElement("span");
  label.className = "q-typed-label";
  label.textContent = TYPED_LABEL;
  const value = document.createElement("span");
  value.className = "q-other-given";
  value.textContent = typed;
  el.append(label, value);
  return el;
}

/** The question's short chip label, verbatim. */
export function drawFeedQuestionHeader(
  u: FeedQuestionHeader,
  path: string,
): HTMLElement {
  log.debug("drawing a question header chip", {
    operation: "feed.asks.question.header",
    context: { path },
  });
  const el = document.createElement("span");
  el.className = "badge q-chip";
  setInlineMarkdown(el, u.text);
  return el;
}

/** The question text, drawn as inline markdown (and echoed verbatim by every click). */
export function drawFeedQuestionText(u: FeedQuestionText, path: string): HTMLElement {
  log.debug("drawing a question's text", {
    operation: "feed.asks.question.text",
    context: { path },
  });
  const el = document.createElement("div");
  el.className = "q-text";
  setInlineMarkdown(el, u.text);
  return el;
}

/**
 * One option: a control carrying its NUMBER (1-based, the one a send of option
 * numbers picks it by), its label, and the description under it when the agent
 * gave one, marked `selected` when the draft holds it.
 *
 * THE HOOK'S VALUE IS THE SERVED LABEL, so what a click echoes back is exactly
 * what was served rather than what was drawn.
 */
export function drawFeedQuestionOption(
  u: FeedQuestionOption,
  number: number,
  chosen: ReadonlySet<string>,
  path: string,
): Control {
  const label = requireMessage(u.label, `${path}.label`);
  log.debug("drawing a question option", {
    operation: "feed.asks.question.option",
    context: { path, description: u.description !== undefined },
  });
  const el = createControl();
  el.className = "q-opt";
  el.setAttribute("data-question-option", label.text);
  const selected = chosen.has(label.text);
  el.classList.toggle("selected", selected);
  el.setAttribute("aria-pressed", selected ? "true" : "false");

  const numeral = drawChoiceNumeral(number, "q-opt-number", "data-question-option-number");

  const text = document.createElement("span");
  text.className = "q-opt-label";
  setInlineMarkdown(text, label.text);
  el.append(numeral, text);

  if (u.description !== undefined) {
    const description = document.createElement("span");
    description.className = "q-opt-description";
    setInlineMarkdown(description, u.description.text);
    el.append(description);
  }
  return el;
}

/** The head's badge: the ask's state, in the question card's own words. */
export function drawFeedQuestionBadge(state: string): HTMLElement {
  switch (state) {
    case "open":
      return drawToolCardBadge(ASK_WAITING_WORD, [], state);
    case "answered":
      return drawToolCardBadge(ANSWERED_TEXT, ["ok"], state);
    case "expired":
      return drawToolCardBadge(EXPIRED_TEXT, ["muted"], state);
    default:
      return unreachableArm(`${PATH}.state`, state);
  }
}

/** The answered state: one verdict line per question, in the batch's order. */
export function drawFeedQuestionAnswered(u: FeedQuestionAnswered, path: string): HTMLElement {
  log.debug("drawing an answered question card", {
    operation: "feed.asks.question.answered",
    context: { path, answers: u.answers.length },
  });
  const el = document.createElement("div");
  el.className = "q-verdicts list-rows";
  u.answers.forEach((answer, index) => {
    el.append(drawFeedQuestionGivenAnswer(answer, `${path}.answers[${index}]`));
  });
  return el;
}

/** One question's given answer: its chip, the labels chosen, the text typed. */
export function drawFeedQuestionGivenAnswer(
  u: FeedQuestionGivenAnswer,
  path: string,
): HTMLElement {
  log.debug("drawing a given answer", {
    operation: "feed.asks.question.given",
    context: { path, chosen: u.chosen.length, other: u.otherText !== undefined },
  });
  const el = document.createElement("div");
  el.className = "q-verdict";
  el.append(
    drawFeedQuestionHeader(requireMessage(u.header, `${path}.header`), `${path}.header`),
  );
  if (u.chosen.length > 0) {
    const chosen = document.createElement("span");
    chosen.className = "q-chosen";
    // The labels as served, joined for reading. The joiner is presentation, and
    // each label is drawn on its own so one label's markup cannot reach into the
    // next.
    u.chosen.forEach((served, index) => {
      if (index > 0) chosen.append(", ");
      const one = document.createElement("span");
      setInlineMarkdown(one, served);
      chosen.append(one);
    });
    el.append(chosen);
  }
  if (u.otherText !== undefined) {
    const other = document.createElement("span");
    other.className = "q-other-given";
    other.textContent = u.otherText.text;
    el.append(other);
  }
  return el;
}

/** Send one click, latching every control until the daemon answers. */
async function send(
  rc: RowContext,
  status: HTMLElement,
  controls: readonly Control[],
  change: QuestionDraftEdit,
): Promise<void> {
  const id = requireMessage(rc.row.id, "FeedRow.id");
  clearRefusals(status);
  log.info(`editing a question draft with ${change.kind}`, {
    operation: "feed.asks.question.edit",
    context: { row: id.value, edit: change.kind },
  });
  const answered = await whileInFlight(controls, () =>
    callUnary(
      rc.ctx,
      "UpdateQuestionDraft",
      (client) =>
        client.updateQuestionDraft(buildUpdateQuestionDraftRequest(rc.ctx.workspace, id, change)),
      UpdateQuestionDraftResponseSchema,
    ),
  );
  if ("failed" in answered) {
    // callUnary already logged the transport failure once, as its owner.
    status.append(refusal("transport", "the daemon could not be reached"));
    return;
  }
  try {
    drawEditOutcome(answered.value, status, controls);
  } catch (err) {
    // A refusal this build cannot read is still a failure the reader owns; it is
    // stated at the card and reported once rather than becoming an unhandled
    // rejection inside a click handler.
    if (!drawMalformedRefusal(rc.ctx, status, "feed.asks.question.malformed-refusal", err)) throw err;
  }
}

/** The causes only UpdateQuestionDraft can answer with; the four are shared. */
const OWN_CAUSES = {
  askNotStanding: () => "this question is no longer standing",
  unservedValue: (value: { text: string }) => `the question never served "${value.text}"`,
  noSession: () => "the workspace has no session to answer",
} as unknown as SentenceTable;

/**
 * Nothing on success (the row re-pushes, and its controls with it); this
 * click's refusal on error, with the controls given back.
 */
function drawEditOutcome(
  response: UpdateQuestionDraftResponse,
  status: HTMLElement,
  controls: readonly Control[],
): void {
  const result = requireCase(response.result, "UpdateQuestionDraftResponse.result");
  switch (result.case) {
    case "success":
      return;
    case "error": {
      const said = refusalOf(result.value.cause, OWN_CAUSES, "UpdateQuestionDraftError.cause");
      status.append(refusal(said.arm, said.text));
      for (const control of controls) control.disabled = false;
      return;
    }
    default:
      unreachableArm("UpdateQuestionDraftResponse.result", armName(result));
  }
}
