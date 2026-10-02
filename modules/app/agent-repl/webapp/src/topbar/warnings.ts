/**
 * The warning strip: everything the topbar has to warn about right now.
 *
 * NOTHING IS DRAWN WHEN NOTHING IS WRONG. An empty list is the daemon SAYING
 * there is nothing wrong, so the chip is absent entirely — not a quiet version
 * of itself. A control over an empty list only invites the click that proves it
 * is empty.
 *
 * TWO LEVELS, because the list line and the detail answer different questions.
 * The dropdown says WHAT is wrong, one composed sentence per warning, newest
 * first as served. The detail overlay behind a row says enough to ACT on it,
 * and its content is the kind arm's — an unmodeled tool's abbreviated
 * arguments, a fault's component and detail, a degraded window's span. A back
 * affordance returns to the list, because the reader picked one row out of
 * several and losing the list would make them reopen the chip to see the rest.
 *
 * THE UNMODELED-TOOL OVERLAY IS NOT A FAILURE REPORT. The tool very likely ran
 * fine; the only thing wrong is that this contract cannot describe it. So it
 * reads as a legible account of what ran, never as an error.
 *
 * THE SPANS TICK CLIENT-SIDE from the instants the wire ships, through the
 * shared ticker.
 *
 * THE CHIP IS THE ONE PLACE AN ERROR IS SHOWN (owner ruling, 2026-09-23). The
 * page's own client-local failures (`src/failure/local.ts`) — the ones the
 * daemon cannot push because it may be what is unreachable — are listed here
 * too, AHEAD of the pushed warnings and counted in the same badge. They are
 * this page's own facts, so they merge in client-side; the pushed warnings are
 * still drawn verbatim. And they never wait on a push: before the first topbar
 * view, or over a stale one while the link is down, the chip still draws them
 * (`drawLocalWarningStrip`).
 */
import { createControl } from "../control.js";
import type {
  TopbarAccountingWarningDetail,
  TopbarDegradedWindowWarningDetail,
  TopbarDeployFailedWarningDetail,
  TopbarDetachedUnmodeledWarningDetail,
  TopbarSessionFaultWarningDetail,
  TopbarUnmodeledToolWarningDetail,
  TopbarWarning,
  TopbarWarningStrip,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { formatTickedAge } from "../duration.js";
import { tick } from "../feed/ticking.js";
import type { LocalFailure } from "../failure/local.js";
import { log } from "../log.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import type { TopbarContext, WarningChipContext } from "./context.js";
import { asAnchor } from "./strip.js";

/**
 * The chip, or NOTHING.
 *
 * Returns null when the served list is empty AND no client-local failure
 * stands, rather than an empty element, so the caller appends nothing at all
 * and the strip reserves no control.
 */
export function drawTopbarWarningStrip(
  u: TopbarWarningStrip,
  tc: TopbarContext,
): HTMLElement | null {
  return drawWarningChip(tc, u.warnings.length, () => drawWarningList(u, tc));
}

/**
 * The chip before any topbar view has been drawn: the client-local failures
 * alone, or NOTHING.
 *
 * A page whose boot failed, or whose link dropped before the first push, has
 * no view to draw the chip inside — and those are exactly the moments these
 * failures exist to explain.
 */
export function drawLocalWarningStrip(cc: WarningChipContext): HTMLElement | null {
  return drawWarningChip(cc, 0, () => drawLocalWarningList(cc));
}

/** The chip over PUSHED warnings plus whatever client-local failures stand. */
function drawWarningChip(
  cc: WarningChipContext,
  pushed: number,
  body: () => HTMLElement,
): HTMLElement | null {
  const local = cc.localFailures();
  if (pushed === 0 && local.length === 0) {
    log.debug("nothing to warn about; drawing no chip", {
      operation: "topbar.warnings-empty",
    });
    return null;
  }
  log.debug("drawing the topbar warning chip", {
    operation: "topbar.warnings",
    context: { warnings: pushed, local_failures: local.length },
  });

  const wrap = document.createElement("div");
  wrap.className = "topbar-warnings";
  // The standing client-local arms, readable without opening the list: the
  // hook a harness or a probe reads to say which failures the page is showing.
  if (local.length > 0) {
    wrap.setAttribute("data-local-arms", local.map((failure) => failure.arm).join(" "));
  }

  const button = createControl();
  button.className = "topbar-warning-chip";

  // A GLYPH, not an emoji: a triangle that inherits the chip's color.
  const mark = document.createElement("span");
  mark.className = "topbar-warning-mark";
  mark.setAttribute("aria-hidden", "true");
  mark.textContent = "⚠";

  const badge = document.createElement("span");
  badge.className = "topbar-warning-count";
  // THE COUNT IS THE LIST'S LENGTH — the served warnings plus the standing
  // client-local failures, never a tally this end keeps across pushes.
  badge.textContent = String(pushed + local.length);

  button.append(mark, badge);
  wrap.append(button);

  // The wrap is the control; see `drawTopbarModelSelector` for the reasoning.
  asAnchor(wrap, "warnings");
  cc.reveals.register("warnings", "warnings", body);
  wrap.addEventListener("click", () => {
    cc.reveals.toggle("warnings", "warnings", body);
  });
  return wrap;
}

/**
 * The first level: the client-local failures, then one row per pushed
 * warning, newest first as served.
 */
export function drawWarningList(u: TopbarWarningStrip, tc: TopbarContext): HTMLElement {
  const list = drawLocalRows(tc, () => drawWarningList(u, tc));
  for (const [index, warning] of u.warnings.entries()) {
    list.append(drawTopbarWarning(warning, tc, u, `TopbarWarningStrip.warnings[${index}]`));
  }
  return list;
}

/** The first level with no view drawn yet: the client-local failures alone. */
export function drawLocalWarningList(cc: WarningChipContext): HTMLElement {
  return drawLocalRows(cc, () => drawLocalWarningList(cc));
}

/** A fresh list holding one row per standing client-local failure. */
function drawLocalRows(cc: WarningChipContext, relist: () => HTMLElement): HTMLElement {
  const list = document.createElement("div");
  list.className = "topbar-warning-list list-rows";
  for (const failure of cc.localFailures()) list.append(drawLocalFailureRow(failure, cc, relist));
  return list;
}

/**
 * One client-local failure's row: its headline, and its evidence behind the
 * click — the same two levels a pushed warning has. An arm with no evidence
 * (`workspace_gone`, or an arm whose fields were all empty) is a statement.
 */
export function drawLocalFailureRow(
  failure: LocalFailure,
  cc: WarningChipContext,
  relist: () => HTMLElement,
): HTMLElement {
  if (failure.evidence.length === 0) {
    const statement = drawTopbarWarningStatement(failure.headline);
    statement.setAttribute("data-arm", failure.arm);
    statement.setAttribute("data-local", "");
    return statement;
  }
  const row = createControl();
  row.className = "topbar-warning-row";
  row.setAttribute("data-row", "");
  row.setAttribute("data-arm", failure.arm);
  row.setAttribute("data-local", "");
  row.textContent = failure.headline;
  row.addEventListener("click", (event) => {
    // Inside the reveal: this click must not reach the layer's outside-click
    // handler as a close.
    event.stopPropagation();
    cc.reveals.open("warning-detail", "warnings", () =>
      drawLocalFailureDetailOverlay(failure.arm, cc, relist),
    );
  });
  return row;
}

/**
 * The second level for a client-local failure: its headline and its evidence,
 * read from the STANDING set each time it is drawn — a repeat report's newer
 * evidence replaces the older, and a failure retracted while its detail was
 * open falls back to the list it no longer appears in.
 */
export function drawLocalFailureDetailOverlay(
  arm: LocalFailure["arm"],
  cc: WarningChipContext,
  relist: () => HTMLElement,
): HTMLElement {
  const failure = cc.localFailures().find((standing) => standing.arm === arm);
  if (failure === undefined) return relist();

  const overlay = document.createElement("div");
  overlay.className = "topbar-warning-detail";
  overlay.setAttribute("data-detail", "");
  overlay.append(drawWarningBack(cc, relist));

  const body = document.createElement("div");
  body.className = "topbar-warning-body";
  body.setAttribute("data-arm", failure.arm);
  body.append(detailName(failure.headline));
  for (const [label, value] of failure.evidence) body.append(detailLine(`${label}: ${value}`));
  overlay.append(body);
  return overlay;
}

/**
 * One warning row: its composed line, and the detail behind the click.
 *
 * A WARNING WITH NO DETAIL IS A STATEMENT, NOT A CONTROL. `TopbarWarning.detail`
 * is unset for the workspace's own session-less state — "hibernated since
 * 14:03", "cold context, awaiting your answer" — which the FIXED SCHEMA ruling
 * put here when the whole-view states were retired. There is nothing further
 * to reveal and, for the cold gate, nothing to answer HERE: the feed's gate
 * card is the one place a gate is answered. So the row is drawn as text with
 * no click rather than as a button that opens an empty overlay.
 */
export function drawTopbarWarning(
  u: TopbarWarning,
  tc: TopbarContext,
  strip: TopbarWarningStrip,
  path: string,
): HTMLElement {
  const line = requireMessage(u.line, `${path}.line`);
  if (u.detail.case === undefined) return drawTopbarWarningStatement(line.text);
  const detail = requireCase(u.detail, `${path}.detail`);

  const row = createControl();
  row.className = "topbar-warning-row";
  row.setAttribute("data-row", "");
  row.setAttribute("data-arm", detail.case);
  row.textContent = line.text;

  row.addEventListener("click", (event) => {
    // The list is inside the reveal, so this click must not reach the layer's
    // outside-click handler as a close.
    event.stopPropagation();
    tc.reveals.open("warning-detail", "warnings", () =>
      drawWarningDetailOverlay(u, tc, strip, path),
    );
  });
  return row;
}

/** A warning that only STATES something: the line, and no affordance. */
export function drawTopbarWarningStatement(text: string): HTMLElement {
  const row = document.createElement("div");
  row.className = "topbar-warning-row";
  row.setAttribute("data-row", "");
  row.setAttribute("data-statement", "");
  row.textContent = text;
  return row;
}

/** The second level: the arm's own content, with a way back to the list. */
export function drawWarningDetailOverlay(
  u: TopbarWarning,
  tc: TopbarContext,
  strip: TopbarWarningStrip,
  path: string,
): HTMLElement {
  const overlay = document.createElement("div");
  overlay.className = "topbar-warning-detail";
  overlay.setAttribute("data-detail", "");
  overlay.append(drawWarningBack(tc, () => drawWarningList(strip, tc)));
  overlay.append(drawWarningDetail(u, tc, path));
  return overlay;
}

/** The detail's way back to the list it was opened from. */
function drawWarningBack(cc: WarningChipContext, relist: () => HTMLElement): HTMLElement {
  const back = createControl();
  back.className = "topbar-warning-back";
  back.setAttribute("data-back", "");
  back.textContent = "‹ warnings";
  back.addEventListener("click", (event) => {
    event.stopPropagation();
    cc.reveals.open("warnings", "warnings", relist);
  });
  return back;
}

/** THE ARM IS THE CONCERN. */
export function drawWarningDetail(
  u: TopbarWarning,
  tc: TopbarContext,
  path: string,
): HTMLElement {
  const detail = requireCase(u.detail, `${path}.detail`);
  switch (detail.case) {
    case "accounting":
      return drawAccountingDetail(detail.value);
    case "unmodeledTool":
      return drawUnmodeledToolDetail(detail.value, `${path}.unmodeled_tool`);
    case "detachedUnmodeled":
      return drawDetachedUnmodeledDetail(detail.value, tc, `${path}.detached_unmodeled`);
    case "sessionFault":
      return drawSessionFaultDetail(detail.value, `${path}.session_fault`);
    case "degradedWindow":
      return drawDegradedWindowDetail(detail.value, tc, `${path}.degraded_window`);
    case "deployFailed":
      return drawDeployFailedDetail(detail.value, `${path}.deploy_failed`);
    default: {
      const other: { case: string } = detail;
      return unreachableArm(`${path}.detail`, other.case);
    }
  }
}

/** The accounting overlay: the reconciliation's composed evidence lines. */
export function drawAccountingDetail(u: TopbarAccountingWarningDetail): HTMLElement {
  const body = document.createElement("div");
  body.className = "topbar-warning-body list-rows";
  for (const line of u.lines) body.append(detailLine(line.text));
  return body;
}

/**
 * The unmodeled-tool overlay: the tool's name, and the daemon's abbreviated
 * account of its arguments where it could compose one.
 */
export function drawUnmodeledToolDetail(
  u: TopbarUnmodeledToolWarningDetail,
  path: string,
): HTMLElement {
  const body = document.createElement("div");
  body.className = "topbar-warning-body";
  body.append(detailName(requireMessage(u.toolName, `${path}.tool_name`).text));
  if (u.argumentLines.length > 0) {
    const lines = document.createElement("div");
    lines.className = "topbar-warning-lines list-rows";
    for (const line of u.argumentLines) lines.append(detailLine(line.text));
    body.append(lines);
  }
  // EMPTY IS THE NAME ALONE. The proto says so: the daemon could compose no
  // legible argument line, and an empty list would read as arguments lost.
  return body;
}

/** The detached-unmodeled overlay: what is running, and for how long. */
export function drawDetachedUnmodeledDetail(
  u: TopbarDetachedUnmodeledWarningDetail,
  tc: TopbarContext,
  path: string,
): HTMLElement {
  const body = document.createElement("div");
  body.className = "topbar-warning-body";
  body.append(detailName(requireMessage(u.toolName, `${path}.tool_name`).text));

  const startedAt = msOf(u.startedAtMs, `${path}.started_at_ms`);
  const clock = document.createElement("div");
  clock.className = "topbar-warning-clock";
  tick(clock, tc.ctx.ticker, (nowMs) => {
    clock.textContent = `running ${formatTickedAge(nowMs - startedAt)}`;
  });
  body.append(clock);
  return body;
}

/** The session-fault overlay: which shim component, and what it said. */
export function drawSessionFaultDetail(
  u: TopbarSessionFaultWarningDetail,
  path: string,
): HTMLElement {
  const body = document.createElement("div");
  body.className = "topbar-warning-body";
  body.append(detailName(requireMessage(u.component, `${path}.component`).text));
  body.append(detailLine(requireMessage(u.detail, `${path}.detail`).text));
  return body;
}

/**
 * The degraded-window overlay: which component, why, and the span.
 *
 * THE ARM IS THE EXTENT. An OPEN window is the loudest state and ticks; a
 * CLOSED one is settled and reports what it cost — the dropped count is the
 * whole reason a closed window is still worth showing.
 */
export function drawDegradedWindowDetail(
  u: TopbarDegradedWindowWarningDetail,
  tc: TopbarContext,
  path: string,
): HTMLElement {
  const body = document.createElement("div");
  body.className = "topbar-warning-body";
  body.append(detailName(requireMessage(u.component, `${path}.component`).text));
  body.append(detailLine(requireMessage(u.reason, `${path}.reason`).text));

  const beganAt = msOf(u.beganAtMs, `${path}.began_at_ms`);
  const extent = requireCase(u.extent, `${path}.extent`);
  const span = document.createElement("div");
  span.className = "topbar-warning-span";
  span.setAttribute("data-extent", extent.case);

  switch (extent.case) {
    case "open":
      tick(span, tc.ctx.ticker, (nowMs) => {
        span.textContent = `degraded since ${formatTickedAge(nowMs - beganAt)}`;
      });
      break;
    case "closed": {
      const endedAt = msOf(extent.value.endedAtMs, `${path}.closed.ended_at_ms`);
      const dropped = msOf(extent.value.droppedCount, `${path}.closed.dropped_count`);
      span.textContent =
        `${clockTime(beganAt)}–${clockTime(endedAt)}, ` +
        `${dropped} observation${dropped === 1 ? "" : "s"} dropped`;
      break;
    }
    default: {
      const other: { case: string } = extent;
      return unreachableArm(`${path}.extent`, other.case);
    }
  }
  body.append(span);
  return body;
}

/**
 * The failed-deploy overlay: the step that failed and what it failed on, what
 * became of the install, the failure's WHOLE account (the row's line carries
 * only its last line), and where a build archived its output. Every line is
 * the daemon's, drawn verbatim; an absent log draws nothing.
 */
export function drawDeployFailedDetail(
  u: TopbarDeployFailedWarningDetail,
  path: string,
): HTMLElement {
  const body = document.createElement("div");
  body.className = "topbar-warning-body";
  body.append(datum(detailName(requireMessage(u.step, `${path}.step`).text), "step"));
  body.append(datum(detailLine(requireMessage(u.component, `${path}.component`).text), "component"));
  body.append(datum(detailLine(requireMessage(u.rollback, `${path}.rollback`).text), "rollback"));
  const whole = detailLine(requireMessage(u.detail, `${path}.detail`).text);
  whole.classList.add("topbar-warning-whole");
  body.append(datum(whole, "detail"));
  if (u.log !== undefined) body.append(datum(detailLine(u.log.text), "log"));
  return body;
}

/** Mark ELEMENT as the overlay's NAME datum, the hook a probe reads. */
function datum(element: HTMLElement, name: string): HTMLElement {
  element.setAttribute("data-datum", name);
  return element;
}

/** One failed deploy a topbar push carries: its row's line and its overlay. */
interface CarriedDeployFailure {
  line: string;
  detail: TopbarDeployFailedWarningDetail;
}

/**
 * The failed deploys a topbar push carries, keyed by their line and whole
 * account: what "the same failure" means across two pushes of one page.
 */
function deployFailures(
  u: TopbarWarningStrip | undefined,
  path: string,
): Map<string, CarriedDeployFailure> {
  const out = new Map<string, CarriedDeployFailure>();
  for (const [index, warning] of (u?.warnings ?? []).entries()) {
    if (warning.detail.case !== "deployFailed") continue;
    const at = `${path}.warnings[${index}]`;
    const line = requireMessage(warning.line, `${at}.line`).text;
    const detail = warning.detail.value;
    const whole = requireMessage(detail.detail, `${at}.deploy_failed.detail`).text;
    out.set(JSON.stringify([line, whole]), { line, detail });
  }
  return out;
}

/**
 * A FAILED DEPLOY IS LOGGED LOUDLY WHERE IT FIRST APPEARS (owner request,
 * 2026-09-28): one ERROR record per failed deploy the NEXT push carries that
 * the PREVIOUS one did not, so a page that opens over a standing failure logs
 * it once and a failure that stands across many pushes is logged once. The
 * chip's own `warning-chip.report` covers client-local failures only; this is
 * the pushed warning's record.
 */
export function reportAppearedDeployFailures(
  previous: TopbarWarningStrip | undefined,
  next: TopbarWarningStrip,
): void {
  const seen = deployFailures(previous, "TopbarWarningStrip");
  for (const [key, { line, detail }] of deployFailures(next, "TopbarWarningStrip")) {
    if (seen.has(key)) continue;
    const at = "TopbarWarning.deploy_failed";
    log.error("a failed deploy appeared on the topbar", {
      operation: "topbar.deploy-failed",
      context: {
        line,
        step: requireMessage(detail.step, `${at}.step`).text,
        component: requireMessage(detail.component, `${at}.component`).text,
        rollback: requireMessage(detail.rollback, `${at}.rollback`).text,
        detail: requireMessage(detail.detail, `${at}.detail`).text,
        // An absent log is a failure that archived none: no field at all.
        ...(detail.log === undefined ? {} : { log: detail.log.text }),
      },
    });
  }
}

/** A wall-clock reading for a settled span's bounds. */
export function clockTime(atMs: number): string {
  return new Date(atMs).toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" });
}

function detailName(text: string): HTMLElement {
  const element = document.createElement("div");
  element.className = "topbar-warning-name";
  element.textContent = text;
  return element;
}

function detailLine(text: string): HTMLElement {
  const element = document.createElement("div");
  element.className = "topbar-warning-detail-line";
  element.textContent = text;
  return element;
}
