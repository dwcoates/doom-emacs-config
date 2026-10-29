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
 * THE CHOICES ARE THE SCHEMA'S ARMS, and each says what it KEEPS rather than
 * only what it costs — the reader is deciding what the conversation loses, and
 * a label that names only the price cannot be weighed against one that names
 * only the loss. NO CHOICE SENDS A HELD PROMPT BUT "compact and resume" (owner
 * ruling, 2026-10-13): clear discards them, pay and compact keep them for the
 * reader to send, and the compact-and-resume button stands only while the
 * daemon says prompts are held at the gate.
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
 * HIDING THE WORKSPACE IS NOT A REMEDIATION (owner request, 2026-10-13). The
 * light grey "hide workspace" button past pay sends `SetWorkspaceHidden`
 * — the same hide `SPC TAB h` sends, which moves the workspace into the
 * sidebar's hidden section and lands the user elsewhere — and answers nothing
 * about the gate: the gate keeps standing for whenever the workspace is shown
 * again.
 *
 * KILLING THE WORKSPACE IS NOT A REMEDIATION EITHER (owner request,
 * 2026-10-17). The grey "kill workspace" button ending the row sends
 * `KillWorkspace` — the same forced session death the sidebar's Kill sends,
 * which leaves the worktree and branch standing — and answers nothing about the
 * gate.
 *
 * EVERY COMPACT VALUE IS ECHOED FROM THE MENU. The summarizer is the served
 * `AgentModel` handed back whole; the scope is one of the served enum values.
 * UNSPECIFIED is never offered and never sent, and a resolved trace carrying it
 * is a MALFORMED VIEW rather than a scope this end quietly words as "everything".
 */
import type { DescMessage, Message } from "@bufbuild/protobuf";
import { closeStandingNewsDigest } from "../../news-digest/news-digest.js";
import { createControl, type Control } from "../../control.js";
import { formatTickedAge } from "../../duration.js";
import { onCompactionProgress } from "../../footer/progress.js";
import { formatTokens } from "../../format.js";
import { coldGateFigureColor } from "../../pressure-color.js";
import { log } from "../../log.js";
import {
  AnswerColdGateResponseSchema,
  type AnswerColdGateReopenFailed,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_answer_cold_gate_pb";
import { KillWorkspaceResponseSchema } from "../../../../proto/gen/ts/agentrepl/v1/endpoint_kill_workspace_pb";
import { SetWorkspaceHiddenResponseSchema } from "../../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_hidden_pb";
import type { AgentModel } from "../../../../proto/gen/ts/conversation/v1/api_pb";
import { SessionCompactScope } from "../../../../proto/gen/ts/conversation/v1/session_pb";
import type {
  FeedColdGate,
  FeedColdGateCompactAndResume,
  FeedColdGateCompactMenu,
  FeedColdGateContextTokens,
  FeedColdGateLastRequest,
  FeedColdGateModel,
  FeedColdGateResolved,
  FeedColdGateResolvedCompact,
  FeedColdGateStanding,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import type { AgentReplClient } from "../../rpc/client.js";
import { MalformedView } from "../../rpc/malformed.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { callUnary } from "../../rpc/unary.js";
import {
  clearRefusals,
  drawMalformedRefusal,
  refusal,
  release,
  whileInFlight,
} from "../cards/controls.js";
import { callFailure, refusalOf, type Cause, type SentenceTable } from "../../rpc/refuse.js";
import {
  buildKillWorkspaceRequest,
  buildSetWorkspaceHiddenRequest,
  KILL_WORKSPACE_SENTENCES,
  SET_WORKSPACE_HIDDEN_SENTENCES,
} from "../../sidebar/verbs.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { tickWhileShown } from "../ticking.js";
import { stampedAge } from "./stamped-age.js";
import { buildAnswerColdGateRequest, type ColdGateChoice } from "./requests.js";

const PATH = "FeedColdGate";

/** The event a card raises, bubbling, the moment the user answers its gate. */
export const GATE_ANSWERING_EVENT = "agent-repl:cold-gate-answering";

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
  pay: {
    label: "pay and resume",
    hint: "re-read everything now, in the background; held prompts stay held for you to send",
  },
  clear: {
    label: "clear context",
    hint: "drop the conversation and the held prompts; keep the worktree",
  },
  compact: {
    label: "compact first",
    hint: "summarize, then resume from the summary; held prompts stay held for you to send",
  },
  hide: {
    label: "hide workspace",
    hint: "move this workspace to the sidebar's hidden section; the gate stands until it is shown again",
  },
  kill: {
    label: "kill workspace",
    hint: "end this workspace's session; the worktree and branch survive",
  },
  /** {held} is the served count of prompts held at the gate, worded by heldPromptsPhrase. */
  compactAndResume: {
    label: "compact and resume",
    hint: "summarize, then send {held} once the session is back",
  },
  submenu: {
    model: "summarizer",
    scope: "what to summarize",
    /** The send's label is the label of the button that opened the submenu. */
    send: { compact: "compact", compactAndResume: "compact and resume" },
  },
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
  const resume = u.compactAndResume;
  log.debug("drawing a standing cold gate", {
    operation: "feed.asks.cold-gate.standing",
    context: {
      path,
      compaction_offered: menu !== undefined,
      compact_and_resume_offered: resume !== undefined,
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

  const figure = drawFeedColdGateContextTokens(tokens, `${path}.context_tokens`);
  // THE CARD'S RULES TAKE THE FIGURE'S HEAT (owner, 2026-10-06): its frame --
  // the separator line when docked -- and the submenu's divider wear the very
  // color the token figure was given, read off the figure rather than computed
  // twice, so the two can never disagree.
  card.style.setProperty(COLD_GATE_HEAT_PROPERTY, figure.style.color);
  card.append(drawLead(figure, drawFeedColdGateModel(model, `${path}.model`)));

  card.append(drawActions(rc, menu, resume, path));
  return card;
}

/** The card property its frame and dividers read the token figure's color from. */
export const COLD_GATE_HEAT_PROPERTY = "--cold-gate-heat";

/**
 * The lead sentence, with the token figure as an element of its own.
 *
 * The figure is the one number on this page the CLIENT scaled, so it is drawn
 * in its own span rather than interpolated into a string: a reader can see what
 * was formatted, and a test can hold that one figure to the ruled table without
 * reading the sentence around it.
 */
function drawLead(figure: HTMLElement, model: string): HTMLElement {
  const lead = document.createElement("div");
  lead.className = "hibernation-context";
  const [before, rest] = COLD_GATE_COPY.lead.split("{tokens}");
  lead.append(
    document.createTextNode(before),
    figure,
    document.createTextNode(rest.replace("{model}", model)),
  );
  return lead;
}

/**
 * The token figure, formatted client-side from the raw count, colored by its
 * window fill on the cold-gate gradient (`coldGateFigureColor`, the same call
 * the footer's cold-gate line makes): how dear paying to pass the gate is.
 */
export function drawFeedColdGateContextTokens(
  u: FeedColdGateContextTokens,
  path: string,
): HTMLElement {
  log.debug("reading a cold gate's context size", {
    operation: "feed.asks.cold-gate.tokens",
    context: { path, window_fill: u.windowFill },
  });
  const figure = document.createElement("span");
  figure.className = "cold-gate-tokens";
  figure.setAttribute("data-context-tokens", "");
  figure.textContent = formatTokens(tokenCountOf(u.tokens, `${path}.tokens`));
  // INLINE: the figure's color IS its reading (as the context chip's).
  figure.style.color = coldGateFigureColor(u.windowFill, `${path}.window_fill`);
  return figure;
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

/**
 * The compact-and-resume button's props: how many held prompts it would send,
 * worded into its hint. The element stands only while the daemon serves it.
 */
export function drawFeedColdGateCompactAndResume(
  u: FeedColdGateCompactAndResume,
  path: string,
): string {
  log.debug("reading a cold gate's compact-and-resume offer", {
    operation: "feed.asks.cold-gate.compact-and-resume",
    context: { path, held_prompts: u.heldPrompts },
  });
  if (u.heldPrompts < 1) {
    throw new MalformedView(`${path}.held_prompts`, "compact and resume was offered with no prompt held");
  }
  return COLD_GATE_COPY.compactAndResume.hint.replace("{held}", heldPromptsPhrase(u.heldPrompts));
}

/** "the held prompt", or "the N held prompts". */
export function heldPromptsPhrase(held: number): string {
  return held === 1 ? "the held prompt" : `the ${held} held prompts`;
}

/** Which compact button opened the submenu, and so what its send answers. */
type CompactMode = "compact" | "compactAndResume";

/** The one row of action buttons, plus the compact submenu the compact buttons open. */
function drawActions(
  rc: RowContext,
  menu: FeedColdGateCompactMenu | undefined,
  resume: FeedColdGateCompactAndResume | undefined,
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
  // PAY GOES LAST OF THE REMEDIATIONS, in red (owner request, 2026-10-06): it
  // is the weightiest choice on the gate, so it takes the end of their run and
  // the red border; clear takes the place and the border pay had.
  // HIDE AND KILL WORKSPACE FOLLOW PAY, in grey (owner requests, 2026-10-13
  // and 2026-10-17): neither is a remediation, so both sit past pay, the last
  // of them. KILL ENDS THE ROW, with hide to its left in a lighter grey (owner
  // request, 2026-10-19).
  const hide = gateButton("cold-gate-hide", ["data-cold-gate-hide", ""], COLD_GATE_COPY.hide, buttons);
  const kill = gateButton("cold-gate-kill", ["data-cold-gate-kill", ""], COLD_GATE_COPY.kill, buttons);
  row.append(clear, pay, hide, kill);
  pay.addEventListener("click", () => {
    void answer(rc, actions, buttons, progress, { kind: "pay" });
  });
  clear.addEventListener("click", () => {
    void answer(rc, actions, buttons, progress, { kind: "clear" });
  });
  kill.addEventListener("click", () => {
    void killWorkspace(rc, actions, buttons);
  });
  hide.addEventListener("click", () => {
    void hideWorkspace(rc, actions, buttons);
  });
  if (menu === undefined) {
    actions.append(progress);
    return actions;
  }

  // The compact path needs two values before it can be sent, so each compact
  // button opens the ONE submenu rather than firing on the first click, and
  // the button that opened it decides what its send answers. An opener is not
  // the verb's control — the submenu's send button is — so the two carry
  // different hooks.
  const submenu = drawCompactSubmenu(rc, menu, buttons, actions, progress, `${path}.compact`);
  submenu.el.hidden = true;
  actions.append(submenu.el, progress);
  const opener = (mode: CompactMode, label: string, hint: string): Control => {
    // BOTH WEAR THE COMPACT BUTTON'S COLOR (owner ruling, 2026-10-13).
    const button = gateButton("hibernation-compact", ["data-compact-open", mode], { label, hint }, buttons);
    button.addEventListener("click", () => {
      const open = submenu.el.hidden || submenu.mode() !== mode;
      submenu.setMode(mode);
      submenu.el.hidden = !open;
      log.debug(`the reader ${open ? "opened" : "closed"} the compact submenu`, {
        operation: "feed.asks.cold-gate.submenu-toggled",
        context: { open, mode },
      });
    });
    return button;
  };
  // COMPACT LEADS THE ROW (owner request, 2026-10-03): the first button on
  // the left, with compact-and-resume beside it when it stands.
  const compactFirst = opener("compact", COLD_GATE_COPY.compact.label, COLD_GATE_COPY.compact.hint);
  if (resume === undefined) {
    row.prepend(compactFirst);
    return actions;
  }
  const compactAndResume = opener(
    "compactAndResume",
    COLD_GATE_COPY.compactAndResume.label,
    drawFeedColdGateCompactAndResume(resume, `${path}.compact_and_resume`),
  );
  compactAndResume.setAttribute("data-cold-gate-held-prompts", String(resume.heldPrompts));
  row.prepend(compactFirst, compactAndResume);
  return actions;
}

/** One remediation button; what it keeps is its hover tooltip. */
function actionButton(
  arm: "pay" | "clear",
  copy: { label: string; hint: string },
  buttons: Control[],
): Control {
  return gateButton(arm === "clear" ? "hibernation-clear" : "hibernation-direct", ["data-cold-gate", arm], copy, buttons);
}

/**
 * One button on the gate's row: its color class, its hook, its label, and what
 * it does as its hover tooltip. It joins BUTTONS, which an answer in flight
 * latches inert together.
 */
function gateButton(
  className: string,
  [hook, value]: [string, string],
  copy: { label: string; hint: string },
  buttons: Control[],
): Control {
  const button = createControl();
  button.className = className;
  button.setAttribute(hook, value);
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
 *
 * ITS MODE IS THE BUTTON THAT OPENED IT: the send answers `compact` or
 * `compact_and_resume`, and is labelled as that button is.
 */
function drawCompactSubmenu(
  rc: RowContext,
  menu: FeedColdGateCompactMenu,
  buttons: Control[],
  actions: HTMLElement,
  progress: HTMLElement,
  path: string,
): { el: HTMLElement; mode: () => CompactMode; setMode: (mode: CompactMode) => void } {
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

  let mode: CompactMode = "compact";
  const send = createControl();
  send.className = "hibernation-compact";
  const setMode = (next: CompactMode): void => {
    mode = next;
    send.setAttribute("data-cold-gate", next);
    send.textContent = COLD_GATE_COPY.submenu.send[next];
  };
  setMode("compact");
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
    void answer(rc, actions, buttons, progress, { kind: mode, model, scope });
  });
  return { el, mode: () => mode, setMode };
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
  // ANSWERING THE GATE CLOSES A STANDING NEWS DIGEST, as sending a prompt does
  // (owner, 2026-10-06): the user has moved on to the conversation.
  closeStandingNewsDigest(actions.ownerDocument);
  // A GATE THE WEBHOST DRAWS is in another document than its page's digest:
  // the webhost hears this and closes the shown page's (host/gate.ts).
  actions.dispatchEvent(new CustomEvent(GATE_ANSWERING_EVENT, { bubbles: true }));
  log.info(`answering a cold gate with ${choice.kind}`, {
    operation: "feed.asks.cold-gate.answer",
    context: {
      row: id.value,
      choice: choice.kind,
      model: "model" in choice ? choice.model.name : undefined,
      scope: "scope" in choice ? choice.scope : undefined,
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
  await callFromGate(rc, actions, buttons, {
    rpc: "AnswerColdGate",
    call: (client) => client.answerColdGate(buildAnswerColdGateRequest(rc.ctx.workspace, id, choice)),
    schema: AnswerColdGateResponseSchema,
    own: OWN_CAUSES,
    malformedOperation: "feed.asks.cold-gate.malformed-refusal",
    // THE WAIT IS OVER, whichever way it ended: the slot is emptied and the
    // subscription dropped before the outcome is drawn, so a refusal is never
    // read underneath a progress line about a compaction that has stopped.
    settled: () => {
      unsubscribe();
      progress.textContent = "";
      progress.hidden = true;
    },
  });
}

/**
 * Hide the workspace, and nothing else: the gate is not answered, so it stands
 * again when the workspace is shown. The landing elsewhere is the daemon's, as
 * for every hide.
 */
function hideWorkspace(rc: RowContext, actions: HTMLElement, buttons: readonly Control[]): Promise<void> {
  return workspaceVerbFromGate(rc, actions, buttons, {
    verb: "hide",
    message: "hiding the workspace from a cold gate",
    rpc: "SetWorkspaceHidden",
    call: (client) => client.setWorkspaceHidden(buildSetWorkspaceHiddenRequest(rc.ctx.workspace, true)),
    schema: SetWorkspaceHiddenResponseSchema,
    own: SET_WORKSPACE_HIDDEN_SENTENCES,
  });
}

/**
 * Kill the workspace's session, and nothing else: the gate is not answered,
 * and the worktree and branch survive, as for every kill.
 */
function killWorkspace(rc: RowContext, actions: HTMLElement, buttons: readonly Control[]): Promise<void> {
  return workspaceVerbFromGate(rc, actions, buttons, {
    verb: "kill",
    message: "killing the workspace from a cold gate",
    rpc: "KillWorkspace",
    // NO op_id: the synchronous form, which answers `success` or `error` and
    // never `accepted`.
    call: (client) => client.killWorkspace(buildKillWorkspaceRequest(rc.ctx.workspace)),
    schema: KillWorkspaceResponseSchema,
    own: KILL_WORKSPACE_SENTENCES,
  });
}

/**
 * ONE WORKSPACE VERB FROM THE GATE'S ROW, which is not a remediation: logged
 * under `feed.asks.cold-gate.<verb>` with the workspace it acts on, and refused
 * with the cross-cutting four and the verb's own arms (SPEC.own).
 */
export async function workspaceVerbFromGate<Res extends GateResponse>(
  rc: RowContext,
  actions: HTMLElement,
  buttons: readonly Control[],
  spec: {
    verb: "hide" | "kill";
    message: string;
    rpc: string;
    call: (client: AgentReplClient) => Promise<Res>;
    schema: DescMessage;
    /** The verb's own arms, worded as the sidebar words the same verb. */
    own: SentenceTable;
  },
): Promise<void> {
  log.info(spec.message, {
    operation: `feed.asks.cold-gate.${spec.verb}`,
    context: { workspace: rc.ctx.workspace.id },
  });
  await callFromGate(rc, actions, buttons, {
    rpc: spec.rpc,
    call: spec.call,
    schema: spec.schema,
    own: spec.own,
    malformedOperation: `feed.asks.cold-gate.${spec.verb}.malformed-refusal`,
  });
}

/**
 * A response whose result is the `success` or `error` arm every gate call
 * answers. `accepted` is KillWorkspace's asynchronous form, answered only to a
 * request carrying an op_id, which no gate call sends: it is typed here so the
 * kill response fits, and is refused as an unreachable arm if it ever arrives.
 */
type GateResponse = Message & {
  result:
    | { case: "success"; value: unknown }
    | { case: "error"; value: { cause: Cause } }
    | { case: "accepted"; value: unknown }
    | { case: undefined; value?: undefined };
};

/**
 * ONE CALL FROM THE GATE'S ROW, whichever button made it: every button latched
 * inert while it is in flight, and what came back drawn at the buttons.
 *
 * Success draws nothing (the daemon's push redraws the row, or the workspace
 * leaves the tab bar) and keeps the buttons latched. A refusal is worded from OWN
 * plus the cross-cutting four and gives the buttons back. A call that threw is
 * drawn as what actually happened (`callUnary` already logged it, as its
 * owner), and an answer this build cannot read is reported once rather than
 * becoming an unhandled rejection inside a click handler.
 */
async function callFromGate<Res extends GateResponse>(
  rc: RowContext,
  actions: HTMLElement,
  buttons: readonly Control[],
  spec: {
    rpc: string;
    call: (client: AgentReplClient) => Promise<Res>;
    schema: DescMessage;
    own: SentenceTable;
    malformedOperation: string;
    /** Runs the moment the call is over, whichever way it ended, before anything is drawn. */
    settled?: () => void;
  },
): Promise<void> {
  clearRefusals(actions);
  const answered = await whileInFlight(buttons, () =>
    callUnary(rc.ctx, spec.rpc, spec.call, spec.schema),
  );
  spec.settled?.();
  if ("failed" in answered) {
    // The buttons are already back (`whileInFlight` gives them up on a throw),
    // so the gate stays answerable, and the line beside them says what
    // actually happened rather than reporting an unreachable daemon that
    // answered.
    const said = callFailure(answered.failed);
    actions.append(refusal(said.arm, said.text));
    return;
  }
  try {
    const result = requireCase(answered.value.result, `${spec.rpc}Response.result`);
    switch (result.case) {
      case "success":
        return;
      case "error": {
        const said = refusalOf(result.value.cause, spec.own, `${spec.rpc}Error.cause`);
        actions.append(refusal(said.arm, said.text));
        release(buttons);
        return;
      }
      default:
        unreachableArm(`${spec.rpc}Response.result`, armName(result));
    }
  } catch (err) {
    if (!drawMalformedRefusal(rc.ctx, actions, spec.malformedOperation, err)) throw err;
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
