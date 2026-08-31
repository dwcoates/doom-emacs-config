/**
 * shell — THE DETACHED SHELL BUBBLE'S BODY: a background command and its spool.
 *
 * A SHELL IS NOT A FEED. It has no rows, only output, so unlike a subagent
 * bubble there is nothing to open as a sub-feed: the body rides the row itself
 * and the whole of it is drawn here. That asymmetry is the schema's, and it is
 * why this renderer sits among the cards rather than among the bubbles.
 *
 * THE SPOOL IS A SNAPSHOT, REPLACED WHOLE. The daemon caps the tail and pushes
 * what it now stands at; this end appends nothing, splices nothing and
 * remembers no offset. What it does do is FOLLOW THE TAIL — a redrawn box is
 * scrolled to its bottom, because the whole point of a live spool is the line
 * that just arrived.
 *
 * THE OMITTED LINE SITS ABOVE THE BOX, outside it, so the count stays put while
 * the box scrolls. It is display only: it implies no retrieval, and there is no
 * "show earlier" control, because nothing on the wire could answer one.
 *
 * `lost` NEVER READS AS FAILURE. "We stopped being able to see it" is not "it
 * failed", and a non-zero `exit` is not an arm at all — the process COMPLETED,
 * and whether the code is a failure is the reader's judgment. So the exit chip
 * takes its tone from zero-vs-non-zero and the outcome word comes from the arm,
 * and the two are never conflated.
 *
 * THE STOP CONTROL IS AN RPC, never a stream close, and it is drawn only while
 * the shell is live. RULED: the daemon answers `confirm_required` ONLY to the
 * turn target, so there is NO confirm step here — every error arm is an
 * ordinary call-site refusal beside the button.
 */
import { formatElapsed } from "../../duration.js";
import { log } from "../../log.js";
import type {
  FeedShell,
  FeedShellCommand,
  FeedShellExit,
  FeedShellLastProgress,
  FeedShellRuntime,
  FeedShellSettled,
  FeedShellSpool,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  InterruptResponseSchema,
  type InterruptResponse,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { callUnary } from "../../rpc/unary.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { buildInterruptDetachedRequest } from "../requests.js";
import { stopTicking, tick } from "../ticking.js";
import { refusalOf, type SentenceTable } from "../refusal-text.js";
import {
  clearRefusals,
  drawMalformedRefusal,
  refusal,
  release,
  whileInFlight,
} from "./controls.js";

const PATH = "FeedShell";

/** How long a stop's own answer stays on the control before it clears. */
export const STOP_OUTCOME_MS = 4000;

/** The `$` chrome the command line wears — the client's, not the wire's. */
export const PROMPT_CHROME = "$";

/** The word each settled outcome says. */
const SETTLED_WORDS = {
  completed: "completed",
  cancelled: "stopped",
  lost: "lost sight of",
} as const satisfies Record<string, string>;

/** The class each settled outcome's word wears. */
const SETTLED_CLASSES = {
  completed: "shell-outcome-completed",
  cancelled: "shell-outcome-cancelled",
  lost: "shell-outcome-lost",
} as const satisfies Record<string, string>;

/** Every settled outcome this build draws, for the suite to hold to the schema. */
export const SHELL_SETTLED_ARMS: readonly string[] = Object.keys(SETTLED_WORDS);

/** The shell bubble's body. */
export function drawFeedShell(u: FeedShell, rc: RowContext): HTMLElement {
  const state = requireCase(u.state, `${PATH}.state`);
  log("debug", "drawing a shell bubble", {
    operation: "feed.cards.shell",
    context: { state: state.case, spool: u.spool !== undefined },
  });

  const el = document.createElement("div");
  el.className = "shell-bubble";
  el.setAttribute("data-state", state.case);

  const head = document.createElement("div");
  head.className = "shell-head";
  el.append(head);

  const dot = document.createElement("span");
  dot.className = "agent-dot";
  dot.setAttribute("aria-hidden", "true");
  dot.textContent = "●";
  head.append(dot);

  head.append(
    drawFeedShellCommand(requireMessage(u.command, `${PATH}.command`), `${PATH}.command`),
  );

  const runtime = requireMessage(u.runtime, `${PATH}.runtime`);
  switch (state.case) {
    case "live":
      dot.classList.add("agent-running");
      head.append(drawLiveClock(runtime, rc));
      if (state.value.lastProgress !== undefined) {
        head.append(drawFeedShellLastProgress(state.value.lastProgress, rc));
      }
      head.append(drawStopControl(rc));
      break;
    case "settled": {
      const settled = state.value;
      const outcome = requireCase(settled.outcome, `${PATH}.settled.outcome`);
      if (outcome.case !== "completed" && outcome.case !== "cancelled" && outcome.case !== "lost") {
        return unreachableArm(`${PATH}.settled.outcome`, armName(outcome as { case: string }));
      }
      el.setAttribute("data-state", outcome.case);
      dot.classList.add(outcome.case === "lost" ? "agent-lost" : "agent-done");
      head.append(drawSettledClock(runtime, settled));
      if (settled.exit !== undefined) {
        head.append(drawFeedShellExit(settled.exit, `${PATH}.settled.exit`));
      }
      const word = document.createElement("span");
      word.className = `shell-outcome ${SETTLED_CLASSES[outcome.case]}`;
      word.textContent = SETTLED_WORDS[outcome.case];
      head.append(word);
      break;
    }
    default:
      return unreachableArm(`${PATH}.state`, armName(state));
  }

  if (u.spool !== undefined) {
    el.append(drawFeedShellSpool(u.spool, `${PATH}.spool`));
  }
  return el;
}

/** The command line: the client's `$` chrome, then the command verbatim. */
export function drawFeedShellCommand(u: FeedShellCommand, path: string): HTMLElement {
  log("debug", "drawing a shell command line", {
    operation: "feed.cards.shell.command",
    context: { path },
  });
  const el = document.createElement("span");
  el.className = "shell-command";
  const chrome = document.createElement("span");
  chrome.className = "shell-prompt";
  chrome.setAttribute("aria-hidden", "true");
  chrome.textContent = PROMPT_CHROME;
  const text = document.createElement("span");
  text.className = "shell-command-text";
  text.textContent = u.text;
  el.append(chrome, text);
  return el;
}

/**
 * The spool tail, and the truncation line above it.
 *
 * The box is scrolled to its bottom on every draw, so the newest output is what
 * a redraw shows — which is what makes a live spool worth having on screen at
 * all. `scrollTop` on a jsdom element is a plain number, so the suite can read
 * back that the follow happened without a layout engine.
 */
export function drawFeedShellSpool(u: FeedShellSpool, path: string): HTMLElement {
  log("debug", "drawing a shell spool", {
    operation: "feed.cards.shell.spool",
    context: { path, length: u.text.length, omitted: u.omitted !== undefined },
  });
  const wrap = document.createElement("div");
  wrap.className = "shell-spool";
  if (u.omitted !== undefined) {
    const omitted = document.createElement("div");
    omitted.className = "tool-omitted shell-omitted";
    omitted.textContent = u.omitted.text;
    wrap.append(omitted);
  }
  const box = document.createElement("pre");
  box.className = "tool-output bash-output shell-tail";
  box.textContent = u.text;
  followTail(box);
  wrap.append(box);
  return wrap;
}

/** The exit chip. Tone is zero-vs-non-zero; absence draws no chip at all. */
export function drawFeedShellExit(u: FeedShellExit, path: string): HTMLElement {
  log("debug", "drawing a shell exit chip", {
    operation: "feed.cards.shell.exit",
    context: { path, code: u.code },
  });
  const el = document.createElement("span");
  el.className = u.code === 0 ? "badge ok shell-exit" : "badge err shell-exit";
  el.setAttribute("data-exit-code", String(u.code));
  el.textContent = `exit ${u.code}`;
  return el;
}

/**
 * "quiet for N s", ticking from the last append the daemon observed.
 *
 * A QUIETNESS AFFORDANCE, NOT A VERDICT: no threshold is set here and nothing
 * is concluded from it — a silent command is still live until an arm says
 * otherwise.
 */
export function drawFeedShellLastProgress(
  u: FeedShellLastProgress,
  rc: RowContext,
): HTMLElement {
  const atMs = msOf(u.atMs, `${PATH}.live.last_progress.at_ms`);
  const el = document.createElement("span");
  el.className = "shell-quiet";
  tick(el, rc.ctx.ticker, (nowMs) => {
    el.textContent = `quiet for ${formatElapsed(nowMs - atMs)}`;
  });
  return el;
}

/** The live clock, counting up from the original start. */
function drawLiveClock(runtime: FeedShellRuntime, rc: RowContext): HTMLElement {
  const startedMs = msOf(runtime.startedAtMs, `${PATH}.runtime.started_at_ms`);
  const el = document.createElement("span");
  el.className = "shell-clock";
  tick(el, rc.ctx.ticker, (nowMs) => {
    el.textContent = formatElapsed(nowMs - startedMs);
  });
  return el;
}

/** The settled clock: the span that ran, stopped where the command stopped. */
function drawSettledClock(
  runtime: FeedShellRuntime,
  settled: FeedShellSettled,
): HTMLElement {
  const startedMs = msOf(runtime.startedAtMs, `${PATH}.runtime.started_at_ms`);
  const endedMs = msOf(settled.endedAtMs, `${PATH}.settled.ended_at_ms`);
  const el = document.createElement("span");
  el.className = "shell-clock";
  el.textContent = formatElapsed(endedMs - startedMs);
  return el;
}

/** Show the newest end of a scrolling box. */
function followTail(box: HTMLElement): void {
  box.scrollTop = box.scrollHeight;
}

/** The stop control on a live shell. */
function drawStopControl(rc: RowContext): HTMLElement {
  const wrap = document.createElement("span");
  wrap.className = "shell-stop";

  const button = document.createElement("button");
  button.type = "button";
  button.className = "shell-stop-button";
  button.setAttribute("data-interrupt", requireMessage(rc.row.id, "FeedRow.id").value);
  button.textContent = "stop";
  wrap.append(button);

  button.addEventListener("click", () => {
    void stop(rc, wrap, button);
  });
  return wrap;
}

/** Issue the stop and draw whatever came back beside the control. */
async function stop(
  rc: RowContext,
  wrap: HTMLElement,
  button: HTMLButtonElement,
): Promise<void> {
  const id = requireMessage(rc.row.id, "FeedRow.id");
  clearOutcome(wrap);
  log("info", "stopping a detached shell", {
    operation: "feed.cards.shell.stop",
    context: { row: id.value },
  });
  const answered = await whileInFlight([button], () =>
    callUnary(
      rc.ctx,
      "Interrupt",
      (client) => client.interrupt(buildInterruptDetachedRequest(rc.ctx.workspace, id)),
      InterruptResponseSchema,
    ),
  );
  if ("failed" in answered) {
    // callUnary already logged the transport failure once, as its owner; what
    // is left is telling the reader their click did not land.
    wrap.append(refusal("transport", "the daemon could not be reached"));
    return;
  }
  try {
    drawAnswer(answered.value, rc, wrap, button);
  } catch (err) {
    // A refusal this build cannot read is still a failure the reader owns; it is
    // stated at the control and reported once rather than becoming an unhandled
    // rejection inside a click handler.
    if (!drawMalformedRefusal(rc.ctx, wrap, "feed.cards.shell.malformed-refusal", err)) throw err;
  }
}

/**
 * The causes only Interrupt can answer with; the four are shared.
 *
 * `confirm_required` is here because the ARM EXISTS on the wire, not because
 * this control expects it: the daemon raises the challenge only for the TURN
 * target, so on a shell it is a fact worth stating plainly rather than a
 * challenge to offer a second button for.
 */
const OWN_CAUSES = {
  confirmRequired: (value: { liveAgentCount: bigint }) =>
    `stopping would also end ${value.liveAgentCount.toString()} live agent(s)`,
  notDetachedWork: () => "this row names no detached work",
  noSession: () => "the workspace has no session to interrupt",
  shimRefused: (value: { detail: string }) => value.detail,
} as unknown as SentenceTable;

/** The stop's answer: a success arm's word, or this click's own refusal. */
function drawAnswer(
  response: InterruptResponse,
  rc: RowContext,
  wrap: HTMLElement,
  button: HTMLButtonElement,
): void {
  const result = requireCase(response.result, "InterruptResponse.result");
  switch (result.case) {
    case "success": {
      const outcome = requireCase(result.value.outcome, "InterruptSuccess.outcome");
      const el = document.createElement("span");
      el.className = "shell-stop-outcome";
      el.setAttribute("data-outcome", outcome.case);
      switch (outcome.case) {
        case "interruptedTurn":
          el.textContent = "turn interrupted";
          break;
        case "interruptedDetached":
          el.textContent = `stopped ${outcome.value.count.toString()}`;
          break;
        case "nothingRunning":
          el.textContent = "nothing was running";
          break;
        default:
          unreachableArm("InterruptSuccess.outcome", armName(outcome as { case: string }));
      }
      expire(el, rc);
      wrap.append(el);
      return;
    }
    case "error": {
      // RULED: `confirm_required` is answered only to the TURN target, so this
      // control draws no confirm step — every arm is an ordinary refusal here,
      // and the button comes back so the user can try again.
      const said = refusalOf(result.value.kind, OWN_CAUSES, "InterruptError.kind");
      wrap.append(refusal(said.arm, said.text));
      release([button]);
      return;
    }
    default:
      unreachableArm("InterruptResponse.result", armName(result));
  }
}

/** Drop whatever the previous click left beside the control. */
function clearOutcome(wrap: HTMLElement): void {
  for (const stale of wrap.querySelectorAll(".shell-stop-outcome")) stale.remove();
  clearRefusals(wrap);
}

/**
 * Take EL off screen once the outcome has been on it long enough to read.
 *
 * Through the shared ticker rather than a timer of this module's own: there is
 * exactly one clock on the page, and a component that starts its own is the
 * thing the ticker exists to prevent.
 */
function expire(el: HTMLElement, rc: RowContext): void {
  const deadline = rc.ctx.ticker.now() + STOP_OUTCOME_MS;
  tick(el, rc.ctx.ticker, (nowMs) => {
    if (nowMs < deadline) return;
    stopTicking(el);
    el.remove();
  });
}
