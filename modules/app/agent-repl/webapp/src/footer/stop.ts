/**
 * stop — the footer's two Interrupt controls, deliberately alone in their own
 * module.
 *
 * WHERE THE STOP LIVES IS A WORKING RULING, NOT A SETTLED ONE. The turn stop
 * sits beside the clock (the cell that already says a turn is running) and the
 * fan-wide stop sits in the agents panel's header (the list of what it would
 * stop). Both placements are pending the user's review, so both live HERE
 * rather than inline in `strip.ts` and `expanded.ts`: moving either one is then
 * a change of where a returned element is appended, not a rewrite of the rpc,
 * the confirm step and the outcome notes.
 *
 * STOPPING IS ALWAYS AN RPC. `Interrupt` is the one verb; the ARM IS THE
 * TARGET (`turn` vs `all_agents`) and the response's ARM IS THE OUTCOME.
 * Cancelling a stream stops nothing and is never a stop.
 *
 * NOTHING-RUNNING IS AN ANSWER. `nothing_running` is a SUCCESS arm — the stop
 * found the session already quiet — so it draws a calm note, never a refusal.
 * The refusals are `InterruptError`'s arms, worded once in `interrupt-error.ts`
 * and drawn here as `.refusal[data-arm]`. Exactly one of them is a CHALLENGE
 * rather than a dead end — `confirm_required`, which names how many detached
 * agents a TURN stop would also end and is answered by the same request re-sent
 * with `confirm_agents` — and it is a challenge only at the turn control.
 *
 * EVERY OUTCOME DRAWS AT THE CONTROL, per the call-site rule: the user clicked
 * here and is looking here, and no view is pushed to tell them what happened.
 */
import { createControl } from "../control.js";
import { create } from "@bufbuild/protobuf";
import {
  InterruptResponseSchema,
  type InterruptRequest,
  type InterruptResponse,
  type InterruptSuccess,
  InterruptRequestSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import { frameUndecodable } from "../failure/sink.js";
import { interruptErrorSentence, logInterruptRefusal } from "../interrupt-error.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { guardMalformed } from "../rpc/guard.js";
import { isMalformedView } from "../rpc/malformed.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import { INTERRUPT_OUTCOME_MS } from "../feed/rows/subagent.js";
import { stopTicking, tick } from "../feed/ticking.js";

/** The stop glyph: a filled square, the one shape a stop control has. */
export const STOP_GLYPH = "■";

/** Which target arm a control sets. */
export type InterruptTarget = "turn" | "allAgents";

/**
 * The request, built in ONE place for both controls.
 *
 * The workspace is echoed from the context — the page's single WorkspaceRef —
 * and `confirm_agents` is only ever true on the re-send that answers the
 * challenge, never speculatively on a first ask.
 */
export function buildInterruptRequest(
  ctx: AppContext,
  target: InterruptTarget,
  confirmAgents: boolean,
): InterruptRequest {
  return create(InterruptRequestSchema, {
    workspace: ctx.workspace,
    target: { case: target, value: {} },
    confirmAgents,
  });
}

/**
 * THE FOOTER'S TWO STOP CONTROLS, BUILT ONCE AND RE-PARENTED, NEVER REBUILT.
 *
 * WHY THEY OUTLIVE A REDRAW, and it is a defect rather than an optimization.
 * The footer draws its whole view on EVERY push (`footer.ts`), and a stop is
 * the one thing on it whose answer is not pushed: the note, the refusal and
 * the confirm challenge are this click's own, drawn at the control. A stop
 * ALWAYS causes the next push — the live set it just emptied is in the view —
 * so a control rebuilt per draw loses its answer within milliseconds of
 * receiving it. Measured in the G51 playbook: the daemon answered
 * `interrupted_detached count=3`, and the footer that came back carried a bare
 * "stop all" with no note anywhere on it. The fan-wide stop's COUNT is the one
 * place that number is ever stated, so it was unreadable in practice.
 *
 * So the footer builds these ONCE per mount and each draw appends the SAME
 * element, which moves it into the fresh dock with whatever it is carrying.
 * Nothing here is remembered across a workspace: the controls belong to the
 * mount, and disposing the footer drops them with it.
 */
export interface StopControls {
  /**
   * The turn stop, drawn beside the clock while a turn is live.
   *
   * The strip mounts it only when the clock's instant is set, so the control's
   * mere presence is the statement that there is something to stop; it never
   * draws a disabled stop for an idle session.
   */
  readonly turn: HTMLElement;
  /**
   * The agents panel's fan-wide stop, drawn in the panel header.
   *
   * The panel is the list of live subagents, so the header is where a "stop all
   * of these" belongs — the rows beneath it are exactly what the click ends.
   */
  readonly allAgents: HTMLElement;
}

/** Build the pair. One call per footer mount. */
export function createStopControls(ctx: AppContext): StopControls {
  return {
    turn: interruptControl(ctx, {
      target: "turn",
      className: "footer-stop footer-stop-turn",
      label: "stop",
      title: "stop the running turn",
      operation: "footer.stop.turn",
    }),
    allAgents: interruptControl(ctx, {
      target: "allAgents",
      className: "footer-stop footer-stop-all",
      label: "stop all",
      title: "stop every live agent",
      operation: "footer.stop.all-agents",
    }),
  };
}

interface ControlSpec {
  target: InterruptTarget;
  className: string;
  label: string;
  title: string;
  operation: string;
}

/**
 * One stop control: the button, the confirm step it may grow, and the note or
 * refusal its answer leaves behind.
 *
 * The wrapper owns all three so the caller appends ONE element and every
 * subsequent state of the interaction stays anchored to the clicked control.
 */
function interruptControl(ctx: AppContext, spec: ControlSpec): HTMLElement {
  const wrapper = document.createElement("span");
  wrapper.className = spec.className;

  const button = createControl();
  button.className = "footer-stop-button";
  button.setAttribute("data-interrupt", "");
  button.title = spec.title;
  const glyph = document.createElement("span");
  glyph.className = "footer-stop-glyph";
  glyph.setAttribute("aria-hidden", "true");
  glyph.textContent = STOP_GLYPH;
  button.appendChild(glyph);
  button.appendChild(document.createTextNode(` ${spec.label}`));
  wrapper.appendChild(button);

  button.addEventListener("click", (event: MouseEvent) => {
    event.preventDefault();
    event.stopPropagation();
    void guardMalformed(ctx, "footer.stop", issue(ctx, spec, wrapper, false));
  });
  return wrapper;
}

/**
 * Issue the stop and draw its answer.
 *
 * CONFIRMED is the second leg of the challenge: the identical request with
 * `confirm_agents` set, which is why the two legs share one function rather
 * than one calling the other's copy.
 */
async function issue(
  ctx: AppContext,
  spec: ControlSpec,
  wrapper: HTMLElement,
  confirmed: boolean,
): Promise<void> {
  clearAnswer(wrapper);
  log.info(`interrupting: ${spec.target}`, {
    operation: spec.operation,
    context: { target: spec.target, confirm_agents: confirmed },
  });
  try {
    const response = await callUnary(
      ctx,
      "Interrupt",
      (client) => client.interrupt(buildInterruptRequest(ctx, spec.target, confirmed)),
      InterruptResponseSchema,
    );
    drawAnswer(ctx, spec, wrapper, response);
  } catch (err) {
    // TWO FAILURES REACH HERE AND THEY ARE DIFFERENT. An ANSWER this build
    // cannot read (an unset `result`, an unset `kind`, an arm this bundle has
    // no case for — raised by callUnary's own strict pass or by the walk
    // below) is the same condition an unreadable push is: filed as
    // `frame_undecodable` so the user is told conversation may be missing. A
    // transport failure is the link, and files nothing here — the stream layer
    // owns the daemon_unreachable window. BOTH draw at the control, because a
    // stop that silently did nothing is the one reading this must never leave.
    if (isMalformedView(err)) {
      log.error(`the Interrupt answer could not be read: ${err.detail}`, {
        operation: `${spec.operation}-undecodable`,
        context: { target: spec.target, path: err.path, cause: err.detail },
      });
      ctx.failures.report(frameUndecodable(err.detail, err.path));
      drawRefusal(wrapper, "malformed", "the daemon's answer could not be read");
      return;
    }
    drawRefusal(wrapper, "transport", "the daemon could not be reached");
    log.error(`Interrupt failed at the transport: ${String(err)}`, {
      operation: `${spec.operation}-failed`,
      context: { target: spec.target, cause: err },
    });
  }
}

/** Walk the answer's arms. Every refusal here is a MalformedView. */
function drawAnswer(
  ctx: AppContext,
  spec: ControlSpec,
  wrapper: HTMLElement,
  response: { result: InterruptResponse["result"] },
): void {
  const result = requireCase(response.result, "InterruptResponse.result");
  switch (result.case) {
    case "success":
      drawOutcome(ctx, wrapper, result.value, spec);
      return;
    case "error": {
      const kind = requireCase(result.value.kind, "InterruptError.kind");
      // THE CHALLENGE IS THE TURN STOP'S ALONE. `confirm_agents` is meaningless
      // on the fan-wide target, so an arm that arrives there is a refusal like
      // any other rather than a second button that would resend and be refused
      // again.
      if (kind.case === "confirmRequired" && spec.target === "turn") {
        drawConfirm(ctx, spec, wrapper, kind.value.liveAgentCount);
        return;
      }
      const sentence = interruptErrorSentence(kind, "InterruptError.kind");
      logInterruptRefusal(kind, sentence, `${spec.operation}-refused`);
      drawRefusal(wrapper, kind.case, sentence);
      return;
    }
    default: {
      const other: { case: string } = result;
      return unreachableArm("InterruptResponse.result", other.case);
    }
  }
}

/**
 * The success note. THE ARM IS WHAT THE STOP DID, so each arm gets its own
 * sentence rather than one "done" that hides which of three things happened.
 */
export function drawInterruptSuccess(success: InterruptSuccess, path: string): HTMLElement {
  const outcome = requireCase(success.outcome, `${path}.outcome`);
  const note = document.createElement("span");
  note.className = "footer-stop-note";
  note.setAttribute("data-stop-outcome", outcome.case);
  switch (outcome.case) {
    case "interruptedTurn":
      note.textContent = "turn stopped";
      break;
    case "interruptedDetached":
      note.textContent =
        outcome.value.count === 1n ? "stopped 1 agent" : `stopped ${outcome.value.count} agents`;
      break;
    case "nothingRunning":
      note.textContent = "nothing running";
      break;
    default: {
      const other: { case: string } = outcome;
      return unreachableArm(`${path}.outcome`, other.case);
    }
  }
  return note;
}

function drawOutcome(
  ctx: AppContext,
  wrapper: HTMLElement,
  success: InterruptSuccess,
  spec: ControlSpec,
): void {
  const note = drawInterruptSuccess(success, "InterruptSuccess");
  log.info(`the stop answered ${note.getAttribute("data-stop-outcome") ?? ""}`, {
    operation: `${spec.operation}-answered`,
    context: { target: spec.target, outcome: note.getAttribute("data-stop-outcome") },
  });
  wrapper.appendChild(note);
  // THE CONTROL NOW OUTLIVES A REDRAW, so the answer needs an end of its own:
  // it is a statement about a click, not standing state, and the same span the
  // detached bubble's stop uses is the one it gets. Without this the note would
  // sit on the control until the next stop replaced it.
  expireOutcome(ctx, note);
}

/** Clear a stop's answer once its say has been had. */
function expireOutcome(ctx: AppContext, note: HTMLElement): void {
  const deadline = ctx.ticker.now() + INTERRUPT_OUTCOME_MS;
  tick(note, ctx.ticker, (nowMs) => {
    if (nowMs < deadline) return;
    stopTicking(note);
    note.remove();
  });
}

/**
 * The challenge, as a second button.
 *
 * It NAMES THE COUNT, because that count is the whole reason the daemon
 * refused: the user asked to stop a turn and is being told the stop reaches
 * further than they asked. The re-send is the only way to say yes.
 */
function drawConfirm(
  ctx: AppContext,
  spec: ControlSpec,
  wrapper: HTMLElement,
  liveAgentCount: bigint,
): void {
  log.warn("the stop needs confirmation: live agents would also end", {
    operation: `${spec.operation}-confirm-required`,
    context: { target: spec.target, live_agent_count: String(liveAgentCount) },
  });
  // THE REFUSAL IS STATED FIRST. The daemon refused this stop, and it refused
  // it for a reason the user is entitled to read in words — the confirm button
  // is the ANSWER to that refusal, not a substitute for stating it.
  drawRefusal(
    wrapper,
    "confirmRequired",
    interruptErrorSentence(
      { case: "confirmRequired", value: { liveAgentCount } } as Parameters<
        typeof interruptErrorSentence
      >[0],
      "InterruptError.kind",
    ),
  );
  const confirm = createControl();
  confirm.className = "footer-stop-confirm";
  confirm.setAttribute("data-interrupt-confirm", "");
  confirm.setAttribute("data-live-agent-count", String(liveAgentCount));
  confirm.textContent =
    liveAgentCount === 1n
      ? "also stop 1 live agent?"
      : `also stop ${liveAgentCount} live agents?`;
  confirm.addEventListener("click", (event: MouseEvent) => {
    event.preventDefault();
    event.stopPropagation();
    void guardMalformed(ctx, "footer.stop-confirm", issue(ctx, spec, wrapper, true));
  });
  wrapper.appendChild(confirm);
}

/** The refusal, at the control, wearing the shared marker. */
function drawRefusal(wrapper: HTMLElement, arm: string, message: string): void {
  const refusal = document.createElement("span");
  refusal.className = "refusal footer-stop-refusal";
  refusal.setAttribute("data-arm", arm);
  refusal.textContent = message;
  wrapper.appendChild(refusal);
}

/** The three things a click can leave on a control: a note, a confirm, a refusal. */
const STOP_ANSWER_SELECTOR = ".footer-stop-note, .footer-stop-confirm, .footer-stop-refusal";

/** Drop whatever the previous click left, so a re-click starts clean. */
function clearAnswer(wrapper: HTMLElement): void {
  for (const el of wrapper.querySelectorAll(STOP_ANSWER_SELECTOR)) {
    el.remove();
  }
}

/**
 * Whether a stop control is currently carrying an answer (a note, a confirm
 * challenge, or a refusal).
 *
 * The agents panel reads this to decide whether it may fold: the fan-wide stop
 * empties the live set, so the push it causes has no rows, and folding then
 * would erase the outcome the control just drew. While the control holds an
 * answer the panel stays; the answer expires on its own and the next push
 * folds.
 */
export function stopControlHasAnswer(wrapper: HTMLElement): boolean {
  return wrapper.querySelector(STOP_ANSWER_SELECTOR) !== null;
}
