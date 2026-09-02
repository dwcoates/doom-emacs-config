/**
 * question — THE QUESTION CARD: the turn blocked on a choice the agent posed.
 *
 * A BATCH IS ONE ASK. One to four questions arrive together and are answered
 * together, in ONE verb: a per-question submission would let a batch be half
 * answered, which is a state neither this card nor the shim behind it has a
 * meaning for. So there is one submit button, and it answers everything.
 *
 * THE SELECTION MODE IS PER QUESTION, because one batch can mix them: the
 * `options` oneof is radios or checkboxes, and the ARM decides which — never a
 * count of the options, never a flag inferred from the text.
 *
 * THE FREE-TEXT ESCAPE IS ALWAYS DRAWN, whether or not the agent asked for one.
 * That is deliberate and it is the schema's own statement: a person answering a
 * multiple-choice question posed by a machine must always be able to say
 * something the machine did not think of, and a note beside a chosen option is
 * as legitimate as an answer instead of one.
 *
 * EVERY VALUE IS ECHOED VERBATIM — the question's own text names the question,
 * an option's own label names the choice. The daemon reconstructs the ask from
 * the values it served, so a label this end trimmed, re-cased or re-rendered is
 * a label it can no longer match. (The DRAWN text may be marked up; the ECHOED
 * text is the served string.)
 *
 * A SINGLE-SELECT NEVER SENDS TWO. Radios make that structurally true in the
 * DOM, and the collector states it again by taking at most one: the refusal for
 * a multi-pick on a single-select exists at the wave, and this end must not be
 * the thing that provokes it.
 *
 * AN EMPTY ANSWER BLOCKS SUBMIT, in place, with a note beside the question that
 * is missing. It does not send a partial batch and it does not silently drop the
 * question — either would answer for the user.
 */
import { formatAge } from "../../duration.js";
import { log } from "../../log.js";
import {
  AnswerQuestionResponseSchema,
  type AnswerQuestionResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_question_pb";
import type {
  FeedQuestion,
  FeedQuestionAnswered,
  FeedQuestionExpired,
  FeedQuestionGivenAnswer,
  FeedQuestionHeader,
  FeedQuestionItem,
  FeedQuestionOption,
  FeedQuestionText,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
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
import { tick } from "../ticking.js";
import { buildAnswerQuestionRequest, type QuestionAnswer } from "./requests.js";

const PATH = "FeedQuestion";

/** What the submit button says. */
export const SUBMIT_TEXT = "answer";

/** What a question with nothing given says, in place. */
export const UNANSWERED_NOTE = "pick an option or type an answer";

/** What an expired card says. */
export const EXPIRED_TEXT = "expired unanswered";

/** Every state this build draws, for the suite to hold to the schema. */
export const QUESTION_STATE_ARMS: readonly string[] = ["open", "answered", "expired"];

/** The question card. */
export function drawFeedQuestion(u: FeedQuestion, rc: RowContext): HTMLElement {
  const state = requireCase(u.state, `${PATH}.state`);
  log("debug", "drawing a question card", {
    operation: "feed.asks.question",
    context: { state: state.case, questions: u.questions.length },
  });

  const card = document.createElement("div");
  card.setAttribute("data-state", state.case);

  switch (state.case) {
    case "open": {
      card.className = "permission pending question";
      const collectors: QuestionCollector[] = [];
      u.questions.forEach((item, index) => {
        const block = drawFeedQuestionItem(item, index, `${PATH}.questions[${index}]`);
        collectors.push(block.collect);
        card.append(block.el);
      });
      card.append(drawSubmit(rc, collectors));
      return card;
    }
    case "answered":
      card.className = "permission resolved question";
      card.append(drawFeedQuestionAnswered(state.value, rc, `${PATH}.answered`));
      return card;
    case "expired":
      card.className = "permission resolved question";
      card.append(drawFeedQuestionExpired(state.value, rc, `${PATH}.expired`));
      return card;
    default:
      return unreachableArm(`${PATH}.state`, armName(state));
  }
}

/** How one question's block reports what the reader gave it. */
export type QuestionCollector = () => {
  /** The answer, when anything was given. */
  answer?: QuestionAnswer;
  /** Where to put the note when nothing was. */
  note: HTMLElement;
};

/**
 * One question: chip, text, its options in the served order, and the free-text
 * escape.
 *
 * INDEX names the radio group, so two single-selects in one batch cannot share
 * a group and steal each other's selection. It is used for nothing else.
 */
export function drawFeedQuestionItem(
  u: FeedQuestionItem,
  index: number,
  path: string,
): { el: HTMLElement; collect: QuestionCollector } {
  const options = requireCase(u.options, `${path}.options`);
  const text = requireMessage(u.text, `${path}.text`);
  log("debug", "drawing a question item", {
    operation: "feed.asks.question.item",
    context: { path, mode: options.case, options: options.value.options.length },
  });

  const el = document.createElement("div");
  el.className = "q-block";
  el.setAttribute("data-question", String(index));

  el.append(
    drawFeedQuestionHeader(requireMessage(u.header, `${path}.header`), `${path}.header`),
  );
  el.append(drawFeedQuestionText(text, `${path}.text`));

  // THE ARM PICKS THE INPUT TYPE. Checked before anything is drawn from it, so
  // an arm a newer daemon set reaches the refusal that quotes its name.
  if (options.case !== "singleSelect" && options.case !== "multiSelect") {
    return unreachableArm(`${path}.options`, armName(options as { case: string }));
  }
  const single = options.case === "singleSelect";
  el.setAttribute("data-question-mode", options.case);

  const list = document.createElement("div");
  // The options are pill CHIPS laid out in a wrapping row, not a list of
  // rows, so the shared row delimiter would draw lines between wrapped chips.
  list.className = "q-opts";
  const inputs: HTMLInputElement[] = [];
  options.value.options.forEach((option, optionIndex) => {
    const drawn = drawFeedQuestionOption(
      option,
      { single, group: `q-${index}` },
      `${path}.options.options[${optionIndex}]`,
    );
    inputs.push(drawn.input);
    list.append(drawn.el);
  });
  el.append(list);

  // ALWAYS DRAWN — see the file header. It is a field, not an "other" option,
  // so it can carry a note beside a chosen label as easily as an answer instead
  // of one.
  const other = document.createElement("input");
  other.type = "text";
  other.className = "q-other";
  other.setAttribute("data-question-other", "");
  other.placeholder = "something else";
  el.append(other);

  const note = document.createElement("div");
  note.className = "q-note";
  note.hidden = true;
  note.textContent = UNANSWERED_NOTE;
  el.append(note);

  const collect: QuestionCollector = () => {
    // A single-select takes AT MOST ONE label even though the radios already
    // guarantee it: the wave refuses a multi-pick on a single-select, and this
    // end must not be what provokes that refusal.
    const picked = inputs.filter((i) => i.checked).map((i) => i.value);
    const chosen = single ? picked.slice(0, 1) : picked;
    const typed = other.value.trim();
    if (chosen.length === 0 && typed === "") return { note };
    return {
      note,
      answer: {
        questionText: text.text,
        chosen,
        ...(typed === "" ? {} : { otherText: typed }),
      },
    };
  };
  return { el, collect };
}

/** The question's short chip label, verbatim. */
export function drawFeedQuestionHeader(
  u: FeedQuestionHeader,
  path: string,
): HTMLElement {
  log("debug", "drawing a question header chip", {
    operation: "feed.asks.question.header",
    context: { path },
  });
  const el = document.createElement("span");
  el.className = "badge q-chip";
  el.textContent = u.text;
  return el;
}

/** The question text, drawn verbatim (and echoed verbatim by the answer). */
export function drawFeedQuestionText(u: FeedQuestionText, path: string): HTMLElement {
  log("debug", "drawing a question's text", {
    operation: "feed.asks.question.text",
    context: { path },
  });
  const el = document.createElement("div");
  el.className = "q-text";
  el.textContent = u.text;
  return el;
}

/**
 * One option: its input, its label and the description under it when the agent
 * gave one.
 *
 * THE INPUT'S VALUE IS THE SERVED LABEL, so what is echoed back is exactly what
 * was served rather than what was drawn.
 */
export function drawFeedQuestionOption(
  u: FeedQuestionOption,
  mode: { single: boolean; group: string },
  path: string,
): { el: HTMLElement; input: HTMLInputElement } {
  const label = requireMessage(u.label, `${path}.label`);
  log("debug", "drawing a question option", {
    operation: "feed.asks.question.option",
    context: { path, description: u.description !== undefined },
  });

  const el = document.createElement("label");
  el.className = "q-opt";

  const input = document.createElement("input");
  input.type = mode.single ? "radio" : "checkbox";
  if (mode.single) input.name = mode.group;
  input.value = label.text;
  input.setAttribute("data-question-option", label.text);
  el.append(input);

  const text = document.createElement("span");
  text.className = "q-opt-label";
  text.textContent = label.text;
  el.append(text);

  if (u.description !== undefined) {
    const description = document.createElement("span");
    description.className = "q-opt-description";
    description.textContent = u.description.text;
    el.append(description);
  }
  return { el, input };
}

/** The answered state: one verdict line per question, in the batch's order. */
export function drawFeedQuestionAnswered(
  u: FeedQuestionAnswered,
  rc: RowContext,
  path: string,
): HTMLElement {
  log("debug", "drawing an answered question card", {
    operation: "feed.asks.question.answered",
    context: { path, answers: u.answers.length },
  });
  const el = document.createElement("div");
  el.className = "q-verdicts list-rows";
  u.answers.forEach((answer, index) => {
    el.append(drawFeedQuestionGivenAnswer(answer, `${path}.answers[${index}]`));
  });
  el.append(stampedAge(u.atMs, `${path}.at_ms`, rc));
  return el;
}

/** One question's given answer: its chip, the labels chosen, the text typed. */
export function drawFeedQuestionGivenAnswer(
  u: FeedQuestionGivenAnswer,
  path: string,
): HTMLElement {
  log("debug", "drawing a given answer", {
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
    // The labels as served, joined for reading. The joiner is presentation; the
    // labels themselves are untouched.
    chosen.textContent = u.chosen.join(", ");
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

/** The expired state: the ask timed out, and the agent went on without it. */
export function drawFeedQuestionExpired(
  u: FeedQuestionExpired,
  rc: RowContext,
  path: string,
): HTMLElement {
  log("debug", "drawing an expired question card", {
    operation: "feed.asks.question.expired",
    context: { path },
  });
  const el = document.createElement("div");
  el.className = "q-verdict";
  el.setAttribute("data-arm", "expired");
  const word = document.createElement("span");
  word.className = "badge muted";
  word.textContent = EXPIRED_TEXT;
  el.append(word, stampedAge(u.atMs, `${path}.at_ms`, rc));
  return el;
}

/** The batch's one submit button. */
function drawSubmit(rc: RowContext, collectors: readonly QuestionCollector[]): HTMLElement {
  const actions = document.createElement("div");
  actions.className = "perm-actions";

  const submit = document.createElement("button");
  submit.type = "button";
  submit.className = "q-submit";
  submit.setAttribute("data-question-submit", "");
  submit.textContent = SUBMIT_TEXT;
  actions.append(submit);

  submit.addEventListener("click", () => {
    void send(rc, actions, submit, collectors);
  });
  return actions;
}

/** Collect the batch, block on anything missing, and send the rest. */
async function send(
  rc: RowContext,
  actions: HTMLElement,
  submit: HTMLButtonElement,
  collectors: readonly QuestionCollector[],
): Promise<void> {
  const id = requireMessage(rc.row.id, "FeedRow.id");
  clearRefusals(actions);

  const collected = collectors.map((collect) => collect());
  for (const one of collected) one.note.hidden = true;
  const missing = collected.filter((one) => one.answer === undefined);
  if (missing.length > 0) {
    for (const one of missing) one.note.hidden = false;
    log("info", "a question batch was submitted with unanswered questions", {
      operation: "feed.asks.question.incomplete",
      context: { row: id.value, missing: missing.length, of: collected.length },
    });
    return;
  }

  const answers = collected.map((one) => one.answer as QuestionAnswer);
  log("info", "answering a question batch", {
    operation: "feed.asks.question.answer",
    context: { row: id.value, answers: answers.length },
  });
  const answered = await whileInFlight([submit], () =>
    callUnary(
      rc.ctx,
      "AnswerQuestion",
      (client) =>
        client.answerQuestion(buildAnswerQuestionRequest(rc.ctx.workspace, id, answers)),
      AnswerQuestionResponseSchema,
    ),
  );
  if ("failed" in answered) {
    // callUnary already logged the transport failure once, as its owner.
    actions.append(refusal("transport", "the daemon could not be reached"));
    return;
  }
  try {
    drawAnswerOutcome(answered.value, actions, submit);
  } catch (err) {
    // A refusal this build cannot read is still a failure the reader owns; it is
    // stated at the control and reported once rather than becoming an unhandled
    // rejection inside a click handler.
    if (!drawMalformedRefusal(rc.ctx, actions, "feed.asks.question.malformed-refusal", err)) throw err;
  }
}

/** The causes only AnswerQuestion can answer with; the four are shared. */
const OWN_CAUSES = {
  askNotStanding: () => "this ask is no longer standing",
  unservedValue: (value: { text: string }) => `the batch never served "${value.text}"`,
  multiPickOnSingleSelect: () => "several options were picked on a single-select question",
  noSession: () => "the workspace has no session to answer",
} as unknown as SentenceTable;

/** Nothing on success (the row re-pushes); this click's refusal on error. */
function drawAnswerOutcome(
  response: AnswerQuestionResponse,
  actions: HTMLElement,
  submit: HTMLButtonElement,
): void {
  const result = requireCase(response.result, "AnswerQuestionResponse.result");
  switch (result.case) {
    case "success":
      return;
    case "error": {
      const said = refusalOf(result.value.cause, OWN_CAUSES, "AnswerQuestionError.cause");
      actions.append(refusal(said.arm, said.text));
      submit.disabled = false;
      return;
    }
    default:
      unreachableArm("AnswerQuestionResponse.result", armName(result));
  }
}

/** A settled instant as a ticking relative age ("3m ago"). */
function stampedAge(atMs: bigint, path: string, rc: RowContext): HTMLElement {
  const at = msOf(atMs, path);
  const el = document.createElement("span");
  el.className = "q-when";
  tick(el, rc.ctx.ticker, (nowMs) => {
    el.textContent = `${formatAge(nowMs - at)} ago`;
  });
  return el;
}
