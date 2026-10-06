/**
 * marker — THE OUTCOME MARKER (frontend.v1.FeedOutcomeMarker; owner ruling,
 * 2026-10-06): the ONE component the feed draws for an event that is not a
 * message — how a turn ended, a permission the user denied, a plan episode
 * that broke, a compaction that failed.
 *
 * A small inline pill, left-aligned in the feed column, no wider than its text
 * and truncating rather than overflowing: the family's GLYPH, the LABEL, then
 * the optional DETAIL. Color is only on the glyph and a thin left edge; the
 * text keeps the normal foreground. No bubble, no background fill.
 *
 * THE FAMILY DECIDES THE GLYPH AND THE COLOR, from the shared vocabulary
 * (`render-colors.json#feed_outcome_marker` and `…_glyphs`, read through
 * `src/vocab.ts`), never from a table here. ONLY A FAULT FAMILY EXPANDS: the
 * neutral arm carries no expansion in the contract, so it draws no chevron and
 * nothing to open. A fault's pill is a button that opens its expansion in
 * place, inside the column. Every word of the expansion is the daemon's,
 * drawn verbatim; the captions beside them and the formatting of an instant
 * are this renderer's.
 *
 * TWO ACTIONS REUSE EXISTING REQUESTS: "sign in" opens the workspace's login
 * flow (agentrepl.v1.OpenLogin, the topbar account cell's request, reached
 * through the page's login overlay), and "resend this prompt" submits the
 * prompt as said through the ordinary SubmitPrompt.
 */
import { create } from "@bufbuild/protobuf";
import type {
  FeedOutcomeAgentReplFaultExpansion,
  FeedOutcomeMarker,
  FeedOutcomeResend,
  FeedOutcomeVendorFaultExpansion,
  FeedOutcomeWhatDied,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { PromptOrigin } from "../../../proto/gen/ts/conversation/v1/prompt_origin_pb";
import {
  SubmitPromptRequestSchema,
  SubmitPromptResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";
import { submitPromptRefusal } from "../composer/composer.js";
import { formatDurationCeil } from "../duration.js";
import { log } from "../log.js";
import { requestLogin } from "../login/request.js";
import { callUnary } from "../rpc/unary.js";
import { isMalformedView } from "../rpc/malformed.js";
import { callFailure, clearRefusals, refusal } from "../rpc/refuse.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import type { AppContext } from "../rpc/context.js";
import { createControl } from "../control.js";
import { feedOutcomeMarkerColor, feedOutcomeMarkerGlyph, toneClass } from "../vocab.js";
import { stopTicking, tickWhileShown } from "./ticking.js";

/** The root class every outcome marker carries. */
export const OUTCOME_MARKER_CLASS = "outcome-marker";

/** The attribute every outcome marker carries, naming its family arm. */
export const OUTCOME_MARKER_ATTRIBUTE = "data-outcome-marker";

/**
 * The character each shared glyph NAME is drawn with. The NAMES are the
 * vocabulary file's; which mark stands for one is this surface's own.
 */
export const MARKER_GLYPH_CHARS: Readonly<Record<string, string>> = Object.freeze({
  stop: "◼",
  diamond: "◆",
  cross: "✕",
});

/** The chevron a fault's pill draws, closed and open. */
const CHEVRON_CLOSED = "›";
const CHEVRON_OPEN = "⌄";

/** What a marker needs from the page it is drawn on. */
export interface MarkerContext {
  /** The page's one context: client, workspace, ticker. */
  ctx: AppContext;
  /** The marker's previous draw, whose expanded state a re-push keeps. */
  previous?: Element | null;
}

/** The outcome marker. PATH is the message tree path, for refusals. */
export function drawOutcomeMarker(m: FeedOutcomeMarker, mc: MarkerContext, path: string): HTMLElement {
  const family = requireCase(m.family, `${path}.family`);
  const label = requireMessage(m.label, `${path}.label`);
  const glyphName = feedOutcomeMarkerGlyph(family.case);
  const glyphChar = MARKER_GLYPH_CHARS[glyphName];
  if (glyphChar === undefined) {
    return unreachableArm(`${path}.family (glyph '${glyphName}')`, family.case);
  }

  const root = document.createElement("div");
  root.className = `${OUTCOME_MARKER_CLASS} ${toneClass(feedOutcomeMarkerColor(family.case))}`;
  root.setAttribute(OUTCOME_MARKER_ATTRIBUTE, family.case);
  root.setAttribute("data-family", family.case);

  const fault = family.case !== "neutral";
  // A FAULT'S PILL IS A CONTROL (the one control, never a <button>); a
  // neutral pill is plain text with nothing to open.
  const pill = fault ? createControl() : document.createElement("span");
  pill.className = "outcome-marker-pill";

  const glyph = document.createElement("span");
  glyph.className = "outcome-marker-glyph";
  glyph.setAttribute("data-glyph", glyphName);
  glyph.textContent = glyphChar;
  const text = document.createElement("span");
  text.className = "outcome-marker-text";
  const labelEl = document.createElement("span");
  labelEl.className = "outcome-marker-label";
  labelEl.textContent = label.text;
  text.append(labelEl);
  if (m.detail !== undefined) {
    const detail = document.createElement("span");
    detail.className = "outcome-marker-detail";
    detail.textContent = ` · ${m.detail.text}`;
    text.append(detail);
  }
  pill.append(glyph, text);
  root.append(pill);

  switch (family.case) {
    case "neutral":
      log.debug("drew a neutral outcome marker", {
        operation: "feed.outcome-marker",
        context: { family: family.case, label: label.text },
      });
      return root;
    case "vendorFault":
      attachExpansion(root, pill, drawVendorExpansion(
        requireMessage(family.value.expansion, `${path}.vendor_fault.expansion`), mc, `${path}.vendor_fault.expansion`,
      ), mc);
      break;
    case "agentReplFault":
      attachExpansion(root, pill, drawAgentReplExpansion(
        requireMessage(family.value.expansion, `${path}.agent_repl_fault.expansion`), mc, `${path}.agent_repl_fault.expansion`,
      ), mc);
      break;
    default: {
      const other: { case: string } = family;
      return unreachableArm(`${path}.family`, other.case);
    }
  }
  log.debug("drew a fault outcome marker", {
    operation: "feed.outcome-marker",
    context: { family: family.case, label: label.text },
  });
  return root;
}

/**
 * Give a fault's pill its chevron and its expansion, opened or closed as the
 * previous draw left it: a re-push never undoes the reader's own click.
 */
function attachExpansion(root: HTMLElement, pill: HTMLElement, expansion: HTMLElement, mc: MarkerContext): void {
  const chevron = document.createElement("span");
  chevron.className = "outcome-marker-chevron";
  pill.append(chevron);
  root.append(expansion);
  const wasOpen = mc.previous?.querySelector(`.outcome-marker-pill`)?.getAttribute("aria-expanded") === "true";
  const show = (open: boolean): void => {
    pill.setAttribute("aria-expanded", String(open));
    expansion.hidden = !open;
    chevron.textContent = open ? CHEVRON_OPEN : CHEVRON_CLOSED;
    root.toggleAttribute("data-expanded", open);
  };
  show(wasOpen);
  pill.addEventListener("click", (event) => {
    event.preventDefault();
    const open = pill.getAttribute("aria-expanded") !== "true";
    show(open);
    log.info(open ? "opened an outcome marker's expansion" : "closed an outcome marker's expansion", {
      operation: "feed.outcome-marker-toggle",
      context: { family: root.getAttribute("data-family"), open },
    });
  });
}

/** One captioned line of an expansion. */
function line(name: string, caption: string, value: string | HTMLElement): HTMLElement {
  const el = document.createElement("div");
  el.className = "outcome-marker-line";
  el.setAttribute("data-line", name);
  const key = document.createElement("span");
  key.className = "outcome-marker-key";
  key.textContent = caption;
  const val = document.createElement("span");
  val.className = "outcome-marker-value";
  if (typeof value === "string") val.textContent = value;
  else val.append(value);
  el.append(key, val);
  return el;
}

/** An instant as the reader's local wall-clock time. */
export function formatMarkerTime(atMs: number): string {
  const at = new Date(atMs);
  const pad = (n: number): string => String(n).padStart(2, "0");
  return `${pad(at.getHours())}:${pad(at.getMinutes())}:${pad(at.getSeconds())}`;
}

/** The expansion's container. */
function expansionBox(): HTMLElement {
  const box = document.createElement("div");
  box.className = "outcome-marker-expansion";
  return box;
}

/** A vendor fault's expansion, field by field, each only when sent. */
function drawVendorExpansion(x: FeedOutcomeVendorFaultExpansion, mc: MarkerContext, path: string): HTMLElement {
  const box = expansionBox();
  if (x.time !== undefined) box.append(line("time", "time", formatMarkerTime(msOf(x.time.atMs, `${path}.time.at_ms`))));
  const type = requireMessage(x.errorType, `${path}.error_type`);
  box.append(line("error-type", "error", type.text));
  if (x.message !== undefined) box.append(line("message", "message", x.message.text));
  if (x.retries !== undefined) box.append(line("retries", "retries made", String(x.retries.count)));
  if (x.retryAt !== undefined) {
    box.append(line("retry", "retry", drawRetryCountdown(msOf(x.retryAt.atMs, `${path}.retry_at.at_ms`), mc)));
  }
  if (x.model !== undefined) box.append(line("model", "model", x.model.name));
  if (x.account !== undefined) box.append(line("account", "account", x.account.email));
  const actions = document.createElement("div");
  actions.className = "outcome-marker-actions";
  if (x.signIn !== undefined) actions.append(signInAction());
  if (x.resend !== undefined) actions.append(resendAction(x.resend, mc, `${path}.resend`));
  if (actions.childElementCount > 0) box.append(actions);
  return box;
}

/** An agent-repl fault's expansion. */
function drawAgentReplExpansion(x: FeedOutcomeAgentReplFaultExpansion, mc: MarkerContext, path: string): HTMLElement {
  const box = expansionBox();
  box.append(line("time", "time", formatMarkerTime(msOf(requireMessage(x.time, `${path}.time`).atMs, `${path}.time.at_ms`))));
  drawWhatDied(box, requireMessage(x.whatDied, `${path}.what_died`), `${path}.what_died`);
  if (x.restarted !== undefined) {
    box.append(line("restarted", "restarted", `the session started again at ${formatMarkerTime(msOf(x.restarted.atMs, `${path}.restarted.at_ms`))}`));
  }
  if (x.resend !== undefined) {
    const actions = document.createElement("div");
    actions.className = "outcome-marker-actions";
    actions.append(resendAction(x.resend, mc, `${path}.resend`));
    box.append(actions);
  }
  return box;
}

/** What died, and what it threw when it threw something. */
function drawWhatDied(box: HTMLElement, died: FeedOutcomeWhatDied, path: string): void {
  const what = requireCase(died.what, `${path}.what`);
  switch (what.case) {
    case "query":
      box.append(line("what-died", "query", requireMessage(what.value.line, `${path}.query.line`).text));
      if (what.value.thrown !== undefined) box.append(line("thrown", "threw", what.value.thrown.text));
      return;
    case "process":
      box.append(line("what-died", "process", requireMessage(what.value.line, `${path}.process.line`).text));
      return;
    default: {
      const other: { case: string } = what;
      unreachableArm(`${path}.what`, other.case);
    }
  }
}

/**
 * The vendor's wait, counting down from the shipped instant to "ready to
 * retry", where the ticking stops: nothing after it can change the line.
 */
function drawRetryCountdown(atMs: number, mc: MarkerContext): HTMLElement {
  const el = document.createElement("span");
  el.className = "outcome-marker-retry";
  tickWhileShown(el, mc.ctx.ticker, (nowMs) => {
    const remaining = atMs - nowMs;
    if (remaining > 0) {
      el.textContent = `retry in ${formatDurationCeil(remaining)}`;
      return;
    }
    el.textContent = "ready to retry";
    stopTicking(el);
  });
  return el;
}

/** The sign-in action: the page's login flow, as the account cell opens it. */
function signInAction(): HTMLElement {
  const button = createControl();
  button.className = "outcome-marker-action";
  button.setAttribute("data-action", "sign-in");
  button.textContent = "sign in";
  button.addEventListener("click", (event) => {
    event.preventDefault();
    log.info("the reader asked to sign in from an outcome marker", { operation: "feed.outcome-marker-sign-in" });
    requestLogin(button);
  });
  return button;
}

/** The resend action: the prompt as said, through the ordinary submission. */
function resendAction(resend: FeedOutcomeResend, mc: MarkerContext, path: string): HTMLElement {
  const said = requireMessage(resend.said, `${path}.said`);
  const button = createControl();
  button.className = "outcome-marker-action";
  button.setAttribute("data-action", "resend");
  button.textContent = "resend this prompt";
  // ONE KEY PER MARKER DRAW: a second click on the same marker is the same
  // submission, which the daemon refuses as a duplicate rather than running
  // the prompt twice.
  const key = crypto.randomUUID();
  button.addEventListener("click", (event) => {
    event.preventDefault();
    const host = button.parentElement ?? button;
    clearRefusals(host);
    log.info("the reader resent a prompt from an outcome marker", { operation: "feed.outcome-marker-resend" });
    void submitResend(mc.ctx, button, host, key, said);
  });
  return button;
}

/** Submit the resend, drawing any refusal at the action. */
async function submitResend(
  ctx: AppContext,
  button: HTMLElement,
  host: HTMLElement,
  key: string,
  said: NonNullable<FeedOutcomeResend["said"]>,
): Promise<void> {
  try {
    const response = await callUnary(
      ctx,
      "SubmitPrompt",
      (client) =>
        client.submitPrompt(
          create(SubmitPromptRequestSchema, {
            workspace: ctx.workspace,
            said,
            idempotencyKey: key,
            origin: PromptOrigin.WEBAPP_CARD_ACTION,
          }),
        ),
      SubmitPromptResponseSchema,
    );
    const result = requireCase(response.result, "SubmitPromptResponse.result");
    if (result.case === "error") {
      const reason = requireCase(result.value.reason, "SubmitPromptError.reason");
      const say = submitPromptRefusal(reason);
      host.append(refusal(reason.case, say));
      log.warn("a resend from an outcome marker was refused", {
        operation: "feed.outcome-marker-resend-refused",
        context: { arm: reason.case, sentence: say },
      });
      return;
    }
    button.setAttribute("data-resent", "true");
    log.info("a resend from an outcome marker was accepted", {
      operation: "feed.outcome-marker-resent",
      context: { outcome: result.value.outcome.case ?? "unset" },
    });
  } catch (err) {
    if (isMalformedView(err)) throw err;
    const failure = callFailure(err);
    host.append(refusal(failure.arm, failure.text));
    log.error(`a resend from an outcome marker failed: ${String(err)}`, {
      operation: "feed.outcome-marker-resend-failed",
      context: { cause: err },
    });
  }
}
