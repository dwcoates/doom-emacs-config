/**
 * shell — THE DETACHED SHELL BUBBLE, in two halves. Ruled 2026-09-14: a
 * detached shell is a CANONICAL BUBBLE, exactly like a subagent's. The HEAD
 * (drawFeedShellHead) carries the command, the clock and — while live — the
 * stop, on the parent feed; the BODY (drawFeedShellBody) is the spool alone,
 * streamed on the shell's OWN sub-feed the head's FeedId addresses. THE BUBBLE
 * IS A FEED: the head opens the sub-feed on expand, and a collapsed bubble
 * streams nothing.
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
import { reservedLiveElapsedClock, reservedQuietClock, reservedSettledElapsedClock } from "../../elapsed-clock.js";
import { log } from "../../log.js";
import type {
  FeedShell,
  FeedShellCommand,
  FeedShellExit,
  FeedShellLastProgress,
  FeedShellLost,
  FeedShellRuntime,
  FeedShellSettled,
  FeedShellSpool,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { msOf, requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import { feedShellDotColor } from "../../vocab.js";
import { drawWorkDot } from "../work-dot.js";
import { asHeadClock } from "./tool-head.js";
import { armName } from "../renderers.js";
import type { RowContext } from "../renderers.js";
import { buildInterruptDetachedRequest } from "../requests.js";
import { stopTicking } from "../ticking.js";
import { foldTitle } from "../title-fold.js";
import { drawDetachedWorkId } from "../work-id.js";
import { drawStopControl, type StopSite } from "../stop-control.js";

const PATH = "FeedShell";

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

/**
 * The cause each `lost` arm names — the sidecar's staleness ruling, said in
 * words rather than left as the bare "lost sight of".
 */
const LOST_CAUSE_WORDS = {
  fileVanished: "file vanished",
  wentSilent: "went silent",
  sweptUp: "swept up at boot",
  processEnded: "process ended",
} as const satisfies Record<string, string>;

/** Every lost cause this build words, for the suite to hold to the schema. */
export const SHELL_LOST_CAUSE_ARMS: readonly string[] = Object.keys(LOST_CAUSE_WORDS);

/**
 * The clause the lost cause adds to the outcome word.
 *
 * An UNSET `how` is an older daemon that never ruled, not a malformed row: the
 * outcome then stays the plain word it has always been.
 */
function lostCauseClause(lost: FeedShellLost): string {
  const how = lost.how;
  if (how.case === undefined) return "";
  return `: ${LOST_CAUSE_WORDS[how.case]}`;
}

/** Every settled outcome this build draws, for the suite to hold to the schema. */
export const SHELL_SETTLED_ARMS: readonly string[] = Object.keys(SETTLED_WORDS);

/**
 * THE SHELL BUBBLE'S HEAD: command, clock, and — while live — the stop; no
 * spool (the spool is the body, on the sub-feed). This is the collapsed head
 * the parent feed carries, drawn by the bubble machinery exactly like a
 * subagent's head.
 */
export function drawFeedShellHead(u: FeedShell, rc: RowContext): HTMLElement {
  const state = requireCase(u.state, `${PATH}.state`);
  log.debug("drawing a shell bubble head", {
    operation: "feed.cards.shell.head",
    context: { state: state.case },
  });

  const el = document.createElement("div");
  el.className = "shell-bubble";
  el.setAttribute("data-state", state.case);

  const head = document.createElement("div");
  head.className = "shell-head";
  el.append(head);

  // THE STATE DOT: the shared vocabulary's color for `live` or the settled
  // outcome; hollow when it spends none (work-dot.ts).
  head.append(drawWorkDot(feedShellDotColor(shellDotState(state, PATH)), state.case === "live"));

  const command = drawFeedShellCommand(requireMessage(u.command, `${PATH}.command`), `${PATH}.command`);
  head.append(command);

  const runtime = requireMessage(u.runtime, `${PATH}.runtime`);
  switch (state.case) {
    case "live":
      head.append(drawLiveClock(runtime, rc));
      if (state.value.lastProgress !== undefined) {
        head.append(drawFeedShellLastProgress(state.value.lastProgress, rc));
      }
      head.append(drawStopControl(rc, SHELL_STOP, requireMessage(rc.row.id, "FeedRow.id")));
      break;
    case "settled": {
      const settled = state.value;
      const outcome = requireCase(settled.outcome, `${PATH}.settled.outcome`);
      if (outcome.case !== "completed" && outcome.case !== "cancelled" && outcome.case !== "lost") {
        return unreachableArm(`${PATH}.settled.outcome`, armName(outcome));
      }
      el.setAttribute("data-state", outcome.case);
      if (settled.exit !== undefined) {
        head.append(drawFeedShellExit(settled.exit, `${PATH}.settled.exit`));
      }
      const word = document.createElement("span");
      word.className = `shell-outcome ${SETTLED_CLASSES[outcome.case]}`;
      word.textContent =
        outcome.case === "lost"
          ? `${SETTLED_WORDS.lost}${lostCauseClause(outcome.value)}`
          : SETTLED_WORDS[outcome.case];
      // THE BADGES COME BEFORE THE CLOCK (owner ruling, 2026-10-11): the head
      // reads title, exit and outcome, then how long it ran.
      head.append(word, drawSettledClock(runtime, settled));
      // A CARD'S TIMER STOPS THE MOMENT ITS UNIT SETTLES. The settled clock is
      // frozen at the span the MESSAGE reports (ended - started), never at the
      // wall clock, and this element holds no live subscription from here on —
      // including one a previous draw left on a REUSED element.
      stopTicking(el);
      break;
    }
    default:
      return unreachableArm(`${PATH}.state`, armName(state));
  }

  // THE DETACHED-WORK ID closes the head, drawn verbatim: a shell bubble is
  // always detached work, so the daemon names it on every head.
  const workId = drawDetachedWorkId(u.workId);
  if (workId !== null) head.append(workId);

  // THE COMMAND IS THE BUBBLE'S TITLE (owner ruling, 2026-09-23): the
  // one-line title fold, owned by the bubble's fold (bubble.ts). Folded AFTER a
  // settled draw's stop, which would otherwise tear down the fold's measurer.
  foldTitle(command, "card", rc);
  return el;
}

/**
 * THE SHELL BUBBLE'S BODY: the spool alone, on the shell's own sub-feed. The
 * command, clock and stop live on the head; drawing them here too would
 * duplicate them, so this reads ONLY the spool (feed.proto: FeedDetachedShell
 * is the spool BODY row). A body with no spool draws an empty box rather than
 * failing — the daemon publishes a body row only once there is output, so this
 * is the defensive case, not the ordinary one.
 */
export function drawFeedShellBody(u: FeedShell, _rc: RowContext): HTMLElement {
  log.debug("drawing a shell bubble body", {
    operation: "feed.cards.shell.body",
    context: { spool: u.spool !== undefined },
  });
  const el = document.createElement("div");
  el.className = "shell-bubble-body";
  if (u.spool !== undefined) {
    el.append(drawFeedShellSpool(u.spool, `${PATH}.spool`));
  }
  return el;
}

/** The command line: the client's `$` chrome, then the command verbatim. */
export function drawFeedShellCommand(u: FeedShellCommand, path: string): HTMLElement {
  log.debug("drawing a shell command line", {
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
 * The box is never scrolled for the reader (owner rule, 2026-09-23: the user
 * owns the scroll, and a bubble's box moves only on their input). The spool the
 * daemon serves is already its newest end, and output appended beneath a
 * reader scrolled inside the expanded box does not move what they are reading.
 */
export function drawFeedShellSpool(u: FeedShellSpool, path: string): HTMLElement {
  log.debug("drawing a shell spool", {
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
  wrap.append(box);
  return wrap;
}

/** The exit chip. Tone is zero-vs-non-zero; absence draws no chip at all. */
export function drawFeedShellExit(u: FeedShellExit, path: string): HTMLElement {
  log.debug("drawing a shell exit chip", {
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
  return reservedQuietClock(rc.ctx.ticker, "shell-quiet", atMs);
}

/** The live clock, counting up from the original start. */
function drawLiveClock(runtime: FeedShellRuntime, rc: RowContext): HTMLElement {
  const startedMs = msOf(runtime.startedAtMs, `${PATH}.runtime.started_at_ms`);
  return asHeadClock(reservedLiveElapsedClock(rc.ctx.ticker, "shell-clock", startedMs));
}

/** The settled clock: the span that ran, stopped where the command stopped. */
function drawSettledClock(
  runtime: FeedShellRuntime,
  settled: FeedShellSettled,
): HTMLElement {
  const startedMs = msOf(runtime.startedAtMs, `${PATH}.runtime.started_at_ms`);
  const endedMs = msOf(settled.endedAtMs, `${PATH}.settled.ended_at_ms`);
  return asHeadClock(reservedSettledElapsedClock("shell-clock", endedMs - startedMs));
}

/** The stop a live shell offers: its own row, stopped as detached work. */
const SHELL_STOP: StopSite = {
  prefix: "shell-stop",
  operation: "feed.cards.shell.stop",
  refusedOperation: "feed.cards.shell.stop-refused",
  describe: "stopping a detached shell",
  malformedOperation: "feed.cards.shell.malformed-refusal",
  request: buildInterruptDetachedRequest,
};

/**
 * The `feed_shell_dot` key a head's state draws: `live`, or the settled
 * outcome arm, with a completed run's daemon judgment appended
 * (`completed_failed`). An unjudged completion is plain `completed`.
 */
export function shellDotState(state: NonNullable<FeedShell["state"]> & { case: string }, path: string): string {
  if (state.case !== "settled") return state.case;
  const outcome = requireCase(state.value.outcome, `${path}.settled.outcome`);
  if (outcome.case === "completed" && outcome.value.judgment.case !== undefined) {
    return `completed_${outcome.value.judgment.case}`;
  }
  return outcome.case;
}
