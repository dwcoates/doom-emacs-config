/**
 * cold-gate — THE COLD-CONTEXT GATE: the one row this page words itself.
 *
 * A DELIBERATE DEPARTURE FROM "THE DAEMON COMPOSES THE SENTENCE". Everywhere
 * else the wire carries resolved presentation and this end draws it verbatim.
 * Here the wire carries DATA — a raw token count, an instant, a model identity,
 * a menu of echo tokens — because the gate's facts are counts and instants
 * rather than prose, and the schema says so explicitly. So the wording, the
 * token formatting and the ticking lapse are this module's, and they are kept in
 * ONE exported `COLD_GATE_COPY` object so a ruling on the wording is a change to
 * one literal rather than a hunt through a renderer.
 *
 * IT IS A BLOCKING GATE, NOT A NOTICE. Nothing else can be done with this
 * session until one of the three remediations is chosen, so it draws as a ringed
 * card in the palette's "you must decide something" band — the same treatment
 * the revival gate wore, which is the gate's visual ancestor. There is no
 * dismiss: a dismissed gate would come back on the next push having taught the
 * reader that the block is optional.
 *
 * THE THREE CHOICES ARE THE SCHEMA'S THREE ARMS, and each says what it KEEPS
 * rather than only what it costs — the reader is deciding what the conversation
 * loses, and a label that names only the price cannot be weighed against one
 * that names only the loss.
 *
 * THE WAIT IS THE DAEMON'S SENTENCE, NOT THIS CARD'S. "compact and resume"
 * starts a compaction that runs for as long as it runs — a minute is ordinary —
 * with every button on this card latched inert. The daemon composes the phase
 * line for exactly that on the footer's own stream
 * (`FooterStatusWorkingActivity.compaction`), so the card SUBSCRIBES to the
 * line the footer already has (`src/footer/progress.ts`) and draws it verbatim
 * in its progress slot. It opens no stream of its own, it composes no sentence
 * of its own, and a moment the footer carries no compaction line is a moment
 * the slot is empty rather than one this end fills with a placeholder.
 *
 * EVERY COMPACT VALUE IS ECHOED FROM THE MENU. The summarizer is the served
 * `AgentModel` handed back whole; the scope is one of the served enum values.
 * UNSPECIFIED is never offered and never sent, and a resolved trace carrying it
 * is a MALFORMED VIEW rather than a scope this end quietly words as "everything".
 */
import { createControl, type Control } from "../../control.js";
import { formatTickedAge } from "../../duration.js";
import { onCompactionProgress } from "../../footer/progress.js";
import { formatTokens } from "../../format.js";
import { log } from "../../log.js";
import {
  AnswerColdGateResponseSchema,
  type AnswerColdGateResponse,
  type AnswerColdGateReopenFailed,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_cold_gate_pb";
import type { AgentModel } from "../../../../proto/gen/ts/conversation/v1/api_pb";
import { SessionCompactScope } from "../../../../proto/gen/ts/conversation/v1/session_pb";
import type {
  FeedColdGate,
  FeedColdGateCompactMenu,
  FeedColdGateContextTokens,
  FeedColdGateLastRequest,
  FeedColdGateModel,
  FeedColdGateResolved,
  FeedColdGateResolvedCompact,
  FeedColdGateStanding,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../rpc/malformed.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { callUnary } from "../../rpc/unary.js";
import {
  clearRefusals,
  drawMalformedRefusal,
  refusal,
  whileInFlight,
} from "../cards/controls.js";
import { callFailure, refusalOf, type SentenceTable } from "../../rpc/refuse.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { tickWhileShown } from "../ticking.js";
import { stampedAge } from "./stamped-age.js";
import { buildAnswerColdGateRequest, type ColdGateChoice } from "./requests.js";

const PATH = "FeedColdGate";

/**
 * EVERY STRING THIS CARD SAYS, in one place.
 *
 * The gate is the one client-worded surface, so its wording is the one thing a
 * ruling is likely to rewrite. Keeping it here means a rewrite touches this
 * object and nothing else — and it means the suite can assert the drawn text
 * against the source of the words rather than against a copy of them.
 */
export const COLD_GATE_COPY = {
  title: "context is cold",
  /** {tokens} and {model} are filled from the served facts. */
  lead: "resuming this session re-reads {tokens} tokens of context at full price on {model}.",
  /** {age} is the ticking lapse since the last vendor request. */
  lapse: "last vendor request {age} ago",
  pay: { label: "pay and resume", hint: "re-read everything now, in the background" },
  clear: { label: "clear and start fresh", hint: "drop the conversation; keep the worktree" },
  compact: { label: "compact first", hint: "summarize, then resume from the summary" },
  submenu: { model: "summarizer", scope: "what to summarize", send: "compact and resume" },
  scopes: {
    ALL: "everything",
    PROMPTS: "my prompts only",
    RESPONSES: "the agent's responses only",
  },
  resolved: {
    pay: "paid the cold read",
    clear: "cleared the context",
    /** {scope} and {model} are the trace's own served values. */
    compact: "compacted {scope} with {model}",
  },
} as const;

/** The scopes this build can word. UNSPECIFIED is deliberately absent. */
const SCOPE_NAMES = {
  [SessionCompactScope.ALL]: "ALL",
  [SessionCompactScope.PROMPTS]: "PROMPTS",
  [SessionCompactScope.RESPONSES]: "RESPONSES",
} as const satisfies Partial<Record<SessionCompactScope, keyof typeof COLD_GATE_COPY.scopes>>;

/** The cold-context gate row. */
export function drawFeedColdGate(u: FeedColdGate, rc: RowContext): HTMLElement {
  const state = requireCase(u.state, `${PATH}.state`);
  log.debug("drawing a cold-context gate", {
    operation: "feed.asks.cold-gate",
    context: { state: state.case },
  });

  switch (state.case) {
    case "standing":
      return drawFeedColdGateStanding(state.value, rc, `${PATH}.standing`);
    case "resolved":
      return drawFeedColdGateResolved(state.value, rc, `${PATH}.resolved`);
    default:
      return unreachableArm(`${PATH}.state`, armName(state));
  }
}

/**
 * The standing gate: the facts, then the remediations. The compact choice is
 * drawn only when the gate serves a compact menu; an account that is not
 * offered compaction gets pay and clear alone.
 */
export function drawFeedColdGateStanding(
  u: FeedColdGateStanding,
  rc: RowContext,
  path: string,
): HTMLElement {
  const tokens = requireMessage(u.contextTokens, `${path}.context_tokens`);
  const lastRequest = requireMessage(u.lastRequest, `${path}.last_request`);
  const model = requireMessage(u.model, `${path}.model`);
  const menu = u.compact;
  log.debug("drawing a standing cold gate", {
    operation: "feed.asks.cold-gate.standing",
    context: {
      path,
      compaction_offered: menu !== undefined,
      models: menu?.models.length ?? 0,
      scopes: menu?.scopes.length ?? 0,
    },
  });

  const card = document.createElement("div");
  card.className = "cold-gate";
  card.setAttribute("data-state", "standing");

  const head = document.createElement("div");
  head.className = "hibernation-head";
  const heading = document.createElement("span");
  heading.className = "hibernation-heading";
  heading.textContent = COLD_GATE_COPY.title;
  head.append(heading, drawFeedColdGateLastRequest(lastRequest, rc, `${path}.last_request`));
  card.append(head);

  card.append(
    drawLead(
      drawFeedColdGateContextTokens(tokens, `${path}.context_tokens`),
      drawFeedColdGateModel(model, `${path}.model`),
    ),
  );

  card.append(drawActions(rc, menu, `${path}.compact`));
  return card;
}

/**
 * The lead sentence, with the token figure as an element of its own.
 *
 * The figure is the one number on this page the CLIENT scaled, so it is drawn
 * in its own span rather than interpolated into a string: a reader can see what
 * was formatted, and a test can hold that one figure to the ruled table without
 * reading the sentence around it.
 */
function drawLead(tokens: string, model: string): HTMLElement {
  const lead = document.createElement("div");
  lead.className = "hibernation-context";
  const [before, rest] = COLD_GATE_COPY.lead.split("{tokens}");
  const figure = document.createElement("span");
  figure.className = "cold-gate-tokens";
  figure.setAttribute("data-context-tokens", "");
  figure.textContent = tokens;
  lead.append(
    document.createTextNode(before),
    figure,
    document.createTextNode(rest.replace("{model}", model)),
  );
  return lead;
}

/** The token figure, formatted client-side from the raw count. */
export function drawFeedColdGateContextTokens(
  u: FeedColdGateContextTokens,
  path: string,
): string {
  log.debug("reading a cold gate's context size", {
    operation: "feed.asks.cold-gate.tokens",
    context: { path },
  });
  return formatTokens(tokenCountOf(u.tokens, `${path}.tokens`));
}

/** The session's model NAME, drawn verbatim. */
export function drawFeedColdGateModel(u: FeedColdGateModel, path: string): string {
  const model = requireMessage(u.model, `${path}.model`);
  log.debug("reading a cold gate's model", {
    operation: "feed.asks.cold-gate.model",
    context: { path, model: model.name },
  });
  return model.name;
}

/**
 * The lapse since the last vendor request, TICKING.
 *
 * The wire ships the instant and the client derives the "ago" — the one clock
 * discipline this page has — so a gate left standing keeps telling the truth
 * about how long it has been standing.
 */
export function drawFeedColdGateLastRequest(
  u: FeedColdGateLastRequest,
  rc: RowContext,
  path: string,
): HTMLElement {
  const at = msOf(u.atMs, `${path}.at_ms`);
  const el = document.createElement("span");
  el.className = "hibernation-since";
  tickWhileShown(el, rc.ctx.ticker, (nowMs) => {
    el.textContent = COLD_GATE_COPY.lapse.replace("{age}", formatTickedAge(nowMs - at));
  });
  return el;
}

/** The resolved trace: which remediation was taken, and when. */
export function drawFeedColdGateResolved(
  u: FeedColdGateResolved,
  rc: RowContext,
  path: string,
): HTMLElement {
  const choice = requireCase(u.choice, `${path}.choice`);
  log.debug("drawing a resolved cold gate", {
    operation: "feed.asks.cold-gate.resolved",
    context: { path, choice: choice.case },
  });

  const el = document.createElement("div");
  el.className = "cold-gate-resolved";
  // The card's state IS the resolution taken (preamble §5: a card carries its
  // state/outcome arm), so a resolved gate reads `pay`, `clear` or `compact`
  // rather than the word "resolved", which says nothing a reader needs.
  el.setAttribute("data-state", choice.case);
  el.setAttribute("data-arm", choice.case);

  const word = document.createElement("span");
  word.className = "cold-gate-trace";
  switch (choice.case) {
    case "pay":
      word.textContent = COLD_GATE_COPY.resolved.pay;
      break;
    case "clear":
      word.textContent = COLD_GATE_COPY.resolved.clear;
      break;
    case "compact":
      word.textContent = drawFeedColdGateResolvedCompact(choice.value, `${path}.compact`);
      // The trace states WHICH scope was summarized as a datum of its own, so
      // the choice is readable without parsing the sentence it was worded into.
      word.setAttribute(
        "data-compact-scope",
        scopeName(choice.value.scope, `${path}.compact.scope`),
      );
      break;
    default:
      return unreachableArm(`${path}.choice`, armName(choice));
  }

  el.append(word, stampedAge(u.atMs, `${path}.at_ms`, rc, "cold-gate-when"));
  return el;
}

/**
 * The compact trace: what was summarized, and by which summarizer.
 *
 * The scope is worded with THE SAME labels the submenu offered, so the trace
 * reads back the choice the reader made in the words they made it in.
 */
export function drawFeedColdGateResolvedCompact(
  u: FeedColdGateResolvedCompact,
  path: string,
): string {
  const model = requireMessage(u.model, `${path}.model`);
  log.debug("reading a compact resolution", {
    operation: "feed.asks.cold-gate.resolved-compact",
    context: { path, scope: u.scope },
  });
  return COLD_GATE_COPY.resolved.compact
    .replace("{scope}", scopeLabel(u.scope, `${path}.scope`))
    .replace("{model}", drawFeedColdGateModel(model, `${path}.model`));
}

/** The one row of action buttons, plus the compact submenu the third one opens. */
function drawActions(
  rc: RowContext,
  menu: FeedColdGateCompactMenu | undefined,
  path: string,
): HTMLElement {
  const actions = document.createElement("div");
  actions.className = "hibernation-actions";

  // THE PROGRESS SLOT, empty until an answer is in flight. It is built here —
  // once, with the card — rather than appended when a click lands, so an answer
  // in flight changes the card's TEXT and its disabled buttons and nothing
  // else. `hibernation-pending` is the gate's ancestor's own class for exactly
  // this line ("a decision in flight"), already in the sheet: no new style, no
  // new colour. It is HIDDEN while empty, because an empty element still
  // carries that class's margin and an always-on gap under the buttons would
  // be a layout change of this end's invention.
  const progress = document.createElement("div");
  progress.className = "hibernation-pending";
  progress.setAttribute("data-cold-gate-progress", "");
  progress.hidden = true;

  // ONE LINE OF EQUAL BUTTONS (owner request, 2026-10-03): side by side, each
  // as wide as the widest label needs, the label centred. What each choice
  // keeps is the button's hover tooltip rather than a sentence beside it.
  const row = document.createElement("div");
  row.className = "cold-gate-buttons";
  actions.append(row);

  const buttons: Control[] = [];
  const pay = actionButton("pay", COLD_GATE_COPY.pay, buttons);
  const clear = actionButton("clear", COLD_GATE_COPY.clear, buttons);
  row.append(pay, clear);
  pay.addEventListener("click", () => {
    void answer(rc, actions, buttons, progress, { kind: "pay" });
  });
  clear.addEventListener("click", () => {
    void answer(rc, actions, buttons, progress, { kind: "clear" });
  });
  if (menu === undefined) {
    actions.append(progress);
    return actions;
  }

  // The compact path needs two values before it can be sent, so its row opens a
  // submenu rather than firing on the first click. The opener is not the verb's
  // control — the submenu's send button is — so the two carry different hooks.
  const opener = createControl();
  opener.className = "hibernation-compact";
  opener.setAttribute("data-compact-open", "");
  opener.textContent = COLD_GATE_COPY.compact.label;
  opener.title = COLD_GATE_COPY.compact.hint;
  // COMPACT LEADS THE ROW (owner request, 2026-10-03): the first button on
  // the left.
  row.prepend(opener);

  const submenu = drawCompactSubmenu(rc, menu, buttons, actions, progress, path);
  submenu.el.hidden = true;
  actions.append(submenu.el, progress);
  buttons.push(opener);
  opener.addEventListener("click", () => {
    submenu.el.hidden = !submenu.el.hidden;
    log.debug(`the reader ${submenu.el.hidden ? "closed" : "opened"} the compact submenu`, {
      operation: "feed.asks.cold-gate.submenu-toggled",
      context: { open: !submenu.el.hidden },
    });
  });
  return actions;
}

/** One remediation button; what it keeps is its hover tooltip. */
function actionButton(
  arm: "pay" | "clear",
  copy: { label: string; hint: string },
  buttons: Control[],
): Control {
  const button = createControl();
  button.className = arm === "clear" ? "hibernation-clear" : "hibernation-direct";
  button.setAttribute("data-cold-gate", arm);
  button.textContent = copy.label;
  button.title = copy.hint;
  buttons.push(button);
  return button;
}

/**
 * The compact submenu: the summarizer radios, the scope radios, and the send.
 *
 * BOTH LISTS COME FROM THE SERVED MENU and the first of each is pre-selected, so
 * the send is always a legal answer: an unselected submenu would either need a
 * disabled send or a default this end invented.
 */
function drawCompactSubmenu(
  rc: RowContext,
  menu: FeedColdGateCompactMenu,
  buttons: Control[],
  actions: HTMLElement,
  progress: HTMLElement,
  path: string,
): { el: HTMLElement } {
  const el = document.createElement("div");
  el.className = "cold-gate-submenu";

  const models: { input: HTMLInputElement; model: AgentModel }[] = [];
  const modelList = document.createElement("div");
  modelList.className = "cold-gate-choices list-rows";
  const modelLabel = document.createElement("div");
  modelLabel.className = "cold-gate-submenu-label";
  modelLabel.textContent = COLD_GATE_COPY.submenu.model;
  menu.models.forEach((option, index) => {
    const model = requireMessage(
      requireMessage(option, `${path}.models[${index}]`).model,
      `${path}.models[${index}].model`,
    );
    const row = document.createElement("label");
    row.className = "cold-gate-choice";
    const input = document.createElement("input");
    input.type = "radio";
    input.name = "cold-gate-model";
    input.value = model.name;
    input.setAttribute("data-compact-model", model.name);
    if (index === 0) input.checked = true;
    const text = document.createElement("span");
    text.textContent = model.name;
    row.append(input, text);
    modelList.append(row);
    models.push({ input, model });
  });
  el.append(modelLabel, modelList);

  const scopes: { input: HTMLInputElement; scope: SessionCompactScope }[] = [];
  const scopeList = document.createElement("div");
  scopeList.className = "cold-gate-choices list-rows";
  const scopeLabelEl = document.createElement("div");
  scopeLabelEl.className = "cold-gate-submenu-label";
  scopeLabelEl.textContent = COLD_GATE_COPY.submenu.scope;
  menu.scopes.forEach((scope, index) => {
    // A menu offering UNSPECIFIED (or an enum value this build has no word for)
    // is a malformed view: there is no honest label to draw, and picking one
    // would offer a scope the daemon did not.
    const name = scopeName(scope, `${path}.scopes[${index}]`);
    const row = document.createElement("label");
    row.className = "cold-gate-choice";
    const input = document.createElement("input");
    input.type = "radio";
    input.name = "cold-gate-scope";
    input.value = name;
    input.setAttribute("data-compact-scope", name);
    if (index === 0) input.checked = true;
    const text = document.createElement("span");
    text.textContent = COLD_GATE_COPY.scopes[name];
    row.append(input, text);
    scopeList.append(row);
    scopes.push({ input, scope });
  });
  el.append(scopeLabelEl, scopeList);

  const send = createControl();
  send.className = "hibernation-compact";
  send.setAttribute("data-cold-gate", "compact");
  send.textContent = COLD_GATE_COPY.submenu.send;
  el.append(send);
  buttons.push(send);

  send.addEventListener("click", () => {
    const model = models.find((m) => m.input.checked)?.model ?? models[0]?.model;
    const scope = scopes.find((s) => s.input.checked)?.scope ?? scopes[0]?.scope;
    if (model === undefined || scope === undefined) {
      // A menu with no summarizers or no scopes cannot be answered, and this end
      // must not invent either value.
      throw new MalformedView(path, "the compact menu offered no model or no scope");
    }
    void answer(rc, actions, buttons, progress, { kind: "compact", model, scope });
  });
  return { el };
}

/** Send the choice, and draw a refusal at the buttons if it was refused. */
async function answer(
  rc: RowContext,
  actions: HTMLElement,
  buttons: readonly Control[],
  progress: HTMLElement,
  choice: ColdGateChoice,
): Promise<void> {
  const id = requireMessage(rc.row.id, "FeedRow.id");
  clearRefusals(actions);
  log.info(`answering a cold gate with ${choice.kind}`, {
    operation: "feed.asks.cold-gate.answer",
    context: {
      row: id.value,
      choice: choice.kind,
      model: choice.kind === "compact" ? choice.model.name : undefined,
      scope: choice.kind === "compact" ? choice.scope : undefined,
    },
  });
  // THE FEEDBACK FOR THE WAIT. The buttons go inert the moment the call leaves
  // (`whileInFlight`), and a compaction behind a `compact` answer can hold them
  // there for a minute; without this the card would say nothing at all for that
  // whole minute. The sentence is whatever the footer's last push carried,
  // redrawn as the daemon pushes the next phase.
  const unsubscribe = onCompactionProgress((text) => {
    progress.textContent = text ?? "";
    progress.hidden = progress.textContent === "";
  });
  const answered = await whileInFlight(buttons, () =>
    callUnary(
      rc.ctx,
      "AnswerColdGate",
      (client) =>
        client.answerColdGate(buildAnswerColdGateRequest(rc.ctx.workspace, id, choice)),
      AnswerColdGateResponseSchema,
    ),
  );
  // THE WAIT IS OVER, whichever way it ended: the slot is emptied and the
  // subscription dropped before the outcome is drawn, so a refusal is never
  // read underneath a progress line about a compaction that has stopped.
  unsubscribe();
  progress.textContent = "";
  progress.hidden = true;
  if ("failed" in answered) {
    // callUnary already logged the failure once, as its owner. What is drawn
    // here is the FEEDBACK AT THE CLICK: the buttons are already back (
    // `whileInFlight` gives them up on a throw), so the gate stays answerable,
    // and the line beside them says what actually happened rather than
    // reporting an unreachable daemon that answered.
    const said = callFailure(answered.failed);
    actions.append(refusal(said.arm, said.text));
    return;
  }
  try {
    drawAnswerOutcome(answered.value, actions, buttons);
  } catch (err) {
    // A refusal this build cannot read is still a failure the reader owns; it is
    // stated at the control and reported once rather than becoming an unhandled
    // rejection inside a click handler.
    if (!drawMalformedRefusal(rc.ctx, actions, "feed.asks.cold-gate.malformed-refusal", err)) throw err;
  }
}

/** The causes only AnswerColdGate can answer with; the four are shared. */
const OWN_CAUSES = {
  noColdGate: () => "no cold gate is standing for this workspace",
  unservedRemediation: () => "the gate never offered that remediation",
  noSession: () => "the workspace has no session to answer",
  // THE RE-OPEN THE CLICK ASKED FOR FAILED, and the daemon says why. Before
  // this arm existed the whole branch arrived as a Connect internal and the
  // line beside the buttons read "the daemon could not be reached" about a
  // daemon that had answered — twice on 2026-09-13
  // (docs/FOOTER-TOPOLOGY-AUDIT.md section 4). The gate stays answerable and
  // the footer carries the same sentence as a standing fault.
  reopenFailed: (v: AnswerColdGateReopenFailed) =>
    v.detail === ""
      ? "the session did not come back from the re-open"
      : `the session did not come back from the re-open: ${v.detail}`,
} as unknown as SentenceTable;

/** Nothing on success (the daemon retires the gate row as it answers); the refusal on error. */
function drawAnswerOutcome(
  response: AnswerColdGateResponse,
  actions: HTMLElement,
  buttons: readonly Control[],
): void {
  const result = requireCase(response.result, "AnswerColdGateResponse.result");
  switch (result.case) {
    case "success":
      return;
    case "error": {
      const said = refusalOf(result.value.cause, OWN_CAUSES, "AnswerColdGateError.cause");
      actions.append(refusal(said.arm, said.text));
      for (const button of buttons) button.disabled = false;
      return;
    }
    default:
      unreachableArm("AnswerColdGateResponse.result", armName(result));
  }
}

/**
 * One scope's ENUM NAME — the single spelling `[data-compact-scope]` carries,
 * on the standing gate's radios and on the resolved trace alike.
 *
 * UNSPECIFIED (or an enum value this build has no word for) is a malformed
 * view: there is no honest name to draw.
 */
export function scopeName(
  scope: SessionCompactScope,
  path: string,
): keyof typeof COLD_GATE_COPY.scopes {
  const name = SCOPE_NAMES[scope as keyof typeof SCOPE_NAMES];
  if (name === undefined) {
    throw new MalformedView(
      path,
      `compaction scope ${String(scope)} is not one this build can word`,
    );
  }
  return name;
}

/** One scope's offered words. */
export function scopeLabel(scope: SessionCompactScope, path: string): string {
  return COLD_GATE_COPY.scopes[scopeName(scope, path)];
}

/**
 * An `int64` count as the number the formatter takes.
 *
 * `Number` silently rounds past 2^53 and a negative count is not a size, so both
 * are refused rather than drawn as a figure that would be a lie.
 */
export function tokenCountOf(v: bigint, path: string): number {
  if (v < 0n) throw new MalformedView(path, `token count ${v.toString()} is negative`);
  if (v > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new MalformedView(path, `token count ${v.toString()} does not fit a JS safe integer`);
  }
  return Number(v);
}
