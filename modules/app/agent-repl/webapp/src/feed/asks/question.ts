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
import { createControl, type Control } from "../../control.js";
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
import { stampedAge } from "./stamped-age.js";
import { buildAnswerQuestionRequest, type QuestionAnswer } from "./requests.js";

const PATH = "FeedQuestion";

/** What the submit button says. */
export const SUBMIT_TEXT = "answer";

/**
 * What a multi-select's per-question commit button says.
 *
 * A single-select commits the instant a radio is picked, so it needs no button;
 * a multi-select has no single "the pick is finished" moment, so the reader says
 * so explicitly. Pressing it is the multi-select's AUTO-ADVANCE trigger.
 */
export const CONFIRM_TEXT = "next";

/** What a question with nothing given says, in place. */
export const UNANSWERED_NOTE = "pick an option or type an answer";

/** What an expired card says. */
export const EXPIRED_TEXT = "expired unanswered";

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

  switch (state.case) {
    case "open":
      card.className = "permission pending question";
      card.append(drawOpenBatch(u, rc));
      return card;
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

/** One tab's live handle: its selector button and the panel it shows. */
interface QuestionTab {
  /** The tab strip button that reveals this question. */
  tab: Control;
  /** The `.q-block` panel this tab reveals; hidden while another is active. */
  block: HTMLElement;
  /** Reports whether this question currently carries an answer. */
  collect: QuestionCollector;
}

/**
 * THE OPEN BATCH, DRAWN AS TABS: one tab per question, one panel shown at a
 * time, and the batch's single submit below.
 *
 * The layout is the only thing that changed. Every question is still drawn and
 * VALIDATED up front (a malformed question in tab three must refuse the card,
 * not wait to be clicked), every collector is still gathered, and the one
 * submit still answers the whole batch in ONE verb — the wire contract is
 * untouched.
 *
 * AUTO-ADVANCE: answering a question moves to the next UNANSWERED tab. A
 * single-select commits the moment a radio is picked; a multi-select commits
 * only when its explicit `CONFIRM_TEXT` button is pressed (see the item). A
 * DESELECT is not a commit and never advances — it returns the question to
 * pending. When every question is answered the reader stays put and submits.
 */
function drawOpenBatch(u: FeedQuestion, rc: RowContext): HTMLElement {
  const wrap = document.createElement("div");
  wrap.className = "q-tabbed";

  const strip = document.createElement("div");
  strip.className = "q-tabs";
  strip.setAttribute("role", "tablist");
  strip.setAttribute("data-question-tabs", "");

  const panels = document.createElement("div");
  panels.className = "q-panels";

  const tabs: QuestionTab[] = [];
  const collectors: QuestionCollector[] = [];

  // The answered/pending marker on every tab tracks LIVE content, so a mere
  // tick — or a deselect that empties the question — updates the strip at once,
  // even before a multi-select's commit button is pressed.
  function refreshMarkers(): void {
    for (const t of tabs) {
      const answered = t.collect().answer !== undefined;
      t.tab.classList.toggle("answered", answered);
      t.tab.setAttribute("data-answered", answered ? "true" : "false");
      const marker = t.tab.querySelector(".q-tab-marker");
      if (marker !== null) marker.textContent = answered ? "✓" : "";
    }
  }

  // Show one panel, hide the rest. Clicking any tab routes here, which is what
  // lets the reader step BACK to an earlier question to change its answer.
  function activate(index: number): void {
    tabs.forEach((t, i) => {
      const active = i === index;
      t.tab.classList.toggle("active", active);
      t.tab.setAttribute("aria-selected", active ? "true" : "false");
      t.block.hidden = !active;
    });
  }

  // The next UNANSWERED tab after this one, wrapping to the front; if every
  // question is answered, stay put so the reader can submit the batch.
  function advanceFrom(index: number): void {
    const n = tabs.length;
    for (let step = 1; step <= n; step += 1) {
      const i = (index + step) % n;
      if (tabs[i]?.collect().answer === undefined) {
        activate(i);
        return;
      }
    }
  }

  u.questions.forEach((item, index) => {
    const block = drawFeedQuestionItem(item, index, `${PATH}.questions[${index}]`, {
      // COMMIT POINT: a single-select radio pick or a multi-select's confirm
      // press lands here and moves to the next unanswered tab.
      onCommit: () => {
        refreshMarkers();
        advanceFrom(index);
      },
      // Any input change (a tick, a keystroke, a deselect) refreshes the strip
      // markers but never advances.
      onInput: refreshMarkers,
    });
    collectors.push(block.collect);

    const tab = createControl();
    tab.className = "q-tab";
    tab.setAttribute("role", "tab");
    tab.setAttribute("data-question-tab", String(index));
    const marker = document.createElement("span");
    marker.className = "q-tab-marker";
    marker.setAttribute("aria-hidden", "true");
    const label = document.createElement("span");
    label.className = "q-tab-label";
    label.textContent = block.header;
    tab.append(marker, label);
    tab.addEventListener("click", () => activate(index));
    strip.append(tab);

    block.el.classList.add("q-panel");
    block.el.setAttribute("data-question-panel", String(index));
    panels.append(block.el);

    tabs.push({ tab, block: block.el, collect: block.collect });
  });

  wrap.append(strip, panels);
  if (tabs.length > 0) activate(0);
  refreshMarkers();

  // A blocked submit reveals the FIRST unanswered tab, so the note it drops
  // beside the missing question is on the panel the reader is looking at.
  wrap.append(drawSubmit(rc, collectors, activate));
  return wrap;
}

/** How one question's block reports what the reader gave it. */
export type QuestionCollector = () => {
  /** The answer, when anything was given. */
  answer?: QuestionAnswer;
  /** Where to put the note when nothing was. */
  note: HTMLElement;
};

/** How one question's block reports interaction back to the batch's tabs. */
export interface QuestionItemHooks {
  /**
   * The COMMIT POINT: fired when this question's answer is finished — a
   * single-select radio pick, or a multi-select's confirm press. This is what
   * drives AUTO-ADVANCE. A deselect is not a commit and never fires it.
   */
  onCommit?: () => void;
  /** Any input change (tick, keystroke, deselect); refreshes markers only. */
  onInput?: () => void;
}

/**
 * One question: chip, text, its options in the served order, and the free-text
 * escape.
 *
 * INDEX names the radio group, so two single-selects in one batch cannot share
 * a group and steal each other's selection. It is used for nothing else.
 *
 * SELECTIONS TOGGLE. Clicking a chosen option CLEARS it — for radios (which the
 * DOM would otherwise leave stuck on) this is done by hand, and for checkboxes
 * it is the native toggle. Clearing the last choice returns the question to
 * pending without advancing; an empty question sends no selection.
 */
export function drawFeedQuestionItem(
  u: FeedQuestionItem,
  index: number,
  path: string,
  hooks: QuestionItemHooks = {},
): { el: HTMLElement; collect: QuestionCollector; header: string } {
  const options = requireCase(u.options, `${path}.options`);
  const text = requireMessage(u.text, `${path}.text`);
  log.debug("drawing a question item", {
    operation: "feed.asks.question.item",
    context: { path, mode: options.case, options: options.value.options.length },
  });

  const el = document.createElement("div");
  el.className = "q-block";
  el.setAttribute("data-question", String(index));

  const header = requireMessage(u.header, `${path}.header`);
  el.append(drawFeedQuestionHeader(header, `${path}.header`));
  el.append(drawFeedQuestionText(text, `${path}.text`));

  // THE ARM PICKS THE INPUT TYPE. Checked before anything is drawn from it, so
  // an arm a newer daemon set reaches the refusal that quotes its name.
  if (options.case !== "singleSelect" && options.case !== "multiSelect") {
    return unreachableArm(`${path}.options`, armName(options));
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

  // The radio the group currently holds, tracked by hand so a second click on
  // it can DESELECT it — the DOM leaves a checked radio checked otherwise.
  let selectedRadio: HTMLInputElement | null = null;
  inputs.forEach((input) => {
    input.addEventListener("click", () => {
      if (single) {
        if (selectedRadio === input) {
          // A second click on the held radio clears it: back to pending, no
          // commit, so a deselect never advances.
          input.checked = false;
          selectedRadio = null;
          hooks.onInput?.();
          return;
        }
        // A fresh pick: the DOM has already checked it and cleared its siblings.
        selectedRadio = input;
        hooks.onInput?.();
        hooks.onCommit?.();
        return;
      }
      // Multi-select: the checkbox toggled natively (select OR deselect). Either
      // way it only refreshes markers; the CONFIRM button is the commit point.
      hooks.onInput?.();
    });
  });

  // ALWAYS DRAWN — see the file header. It is a field, not an "other" option,
  // so it can carry a note beside a chosen label as easily as an answer instead
  // of one.
  const other = document.createElement("input");
  other.type = "text";
  other.className = "q-other";
  other.setAttribute("data-question-other", "");
  other.placeholder = "something else";
  other.addEventListener("input", () => hooks.onInput?.());
  el.append(other);

  // MULTI-SELECT'S COMMIT POINT. A single-select advances on its pick, so it
  // gets no button; a multi-select has no natural "the pick is finished"
  // moment, so the reader presses this to commit and advance.
  if (!single) {
    const confirm = createControl();
    confirm.className = "q-confirm";
    confirm.setAttribute("data-question-confirm", "");
    confirm.textContent = CONFIRM_TEXT;
    confirm.addEventListener("click", () => {
      hooks.onInput?.();
      hooks.onCommit?.();
    });
    el.append(confirm);
  }

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
  return { el, collect, header: header.text };
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
  el.textContent = u.text;
  return el;
}

/** The question text, drawn verbatim (and echoed verbatim by the answer). */
export function drawFeedQuestionText(u: FeedQuestionText, path: string): HTMLElement {
  log.debug("drawing a question's text", {
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
  log.debug("drawing a question option", {
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
  log.debug("drawing an answered question card", {
    operation: "feed.asks.question.answered",
    context: { path, answers: u.answers.length },
  });
  const el = document.createElement("div");
  el.className = "q-verdicts list-rows";
  u.answers.forEach((answer, index) => {
    el.append(drawFeedQuestionGivenAnswer(answer, `${path}.answers[${index}]`));
  });
  el.append(stampedAge(u.atMs, `${path}.at_ms`, rc, "q-when"));
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
  log.debug("drawing an expired question card", {
    operation: "feed.asks.question.expired",
    context: { path },
  });
  const el = document.createElement("div");
  el.className = "q-verdict";
  el.setAttribute("data-arm", "expired");
  const word = document.createElement("span");
  word.className = "badge muted";
  word.textContent = EXPIRED_TEXT;
  el.append(word, stampedAge(u.atMs, `${path}.at_ms`, rc, "q-when"));
  return el;
}

/**
 * The batch's one submit button.
 *
 * `reveal` steps the tab strip to a given question; a blocked submit uses it to
 * bring the first unanswered question into view beside its note.
 */
function drawSubmit(
  rc: RowContext,
  collectors: readonly QuestionCollector[],
  reveal?: (index: number) => void,
): HTMLElement {
  const actions = document.createElement("div");
  actions.className = "perm-actions";

  const submit = createControl();
  submit.className = "q-submit";
  submit.setAttribute("data-question-submit", "");
  submit.textContent = SUBMIT_TEXT;
  actions.append(submit);

  submit.addEventListener("click", () => {
    void send(rc, actions, submit, collectors, reveal);
  });
  return actions;
}

/** Collect the batch, block on anything missing, and send the rest. */
async function send(
  rc: RowContext,
  actions: HTMLElement,
  submit: Control,
  collectors: readonly QuestionCollector[],
  reveal?: (index: number) => void,
): Promise<void> {
  const id = requireMessage(rc.row.id, "FeedRow.id");
  clearRefusals(actions);

  const collected = collectors.map((collect) => collect());
  for (const one of collected) one.note.hidden = true;
  const missing = collected.filter((one) => one.answer === undefined);
  if (missing.length > 0) {
    for (const one of missing) one.note.hidden = false;
    // Bring the first unanswered question into view, so its note is on the panel
    // the reader is looking at rather than on a hidden tab.
    const firstMissing = collected.findIndex((one) => one.answer === undefined);
    if (firstMissing >= 0) reveal?.(firstMissing);
    log.info("a question batch was submitted with unanswered questions", {
      operation: "feed.asks.question.incomplete",
      context: { row: id.value, missing: missing.length, of: collected.length },
    });
    return;
  }

  const answers = collected.map((one) => one.answer as QuestionAnswer);
  log.info("answering a question batch", {
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
  submit: Control,
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

