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
 */
import type {
  TopbarAccountingWarningDetail,
  TopbarDegradedWindowWarningDetail,
  TopbarDetachedUnmodeledWarningDetail,
  TopbarSessionFaultWarningDetail,
  TopbarUnmodeledToolWarningDetail,
  TopbarWarning,
  TopbarWarningStrip,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { formatTickedAge } from "../duration.js";
import { tick } from "../feed/ticking.js";
import { log } from "../log.js";
import { msOf, requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import type { TopbarContext } from "./context.js";
import { asAnchor } from "./strip.js";

/**
 * The chip, or NOTHING.
 *
 * Returns null on an empty list rather than an empty element, so the caller
 * appends nothing at all and the strip reserves no control.
 */
export function drawTopbarWarningStrip(
  u: TopbarWarningStrip,
  tc: TopbarContext,
): HTMLElement | null {
  if (u.warnings.length === 0) {
    log.debug("the daemon reports nothing to warn about; drawing no chip", {
      operation: "topbar.warnings-empty",
    });
    return null;
  }
  log.debug("drawing the topbar warning chip", {
    operation: "topbar.warnings",
    context: { warnings: u.warnings.length },
  });

  const wrap = document.createElement("div");
  wrap.className = "topbar-warnings";

  const button = document.createElement("button");
  button.type = "button";
  button.className = "topbar-warning-chip";

  // A GLYPH, not an emoji: a triangle that inherits the chip's color.
  const mark = document.createElement("span");
  mark.className = "topbar-warning-mark";
  mark.setAttribute("aria-hidden", "true");
  mark.textContent = "⚠";

  const badge = document.createElement("span");
  badge.className = "topbar-warning-count";
  // THE COUNT IS THE LIST'S LENGTH — display of what was served, not a tally
  // this end keeps across pushes.
  badge.textContent = String(u.warnings.length);

  button.append(mark, badge);
  wrap.append(button);

  // The wrap is the control; see `drawTopbarModelSelector` for the reasoning.
  asAnchor(wrap, "warnings");
  const body = (): HTMLElement => drawWarningList(u, tc);
  tc.reveals.register("warnings", "warnings", body);
  wrap.addEventListener("click", () => {
    tc.reveals.toggle("warnings", "warnings", body);
  });
  return wrap;
}

/** The first level: one row per warning, newest first as served. */
export function drawWarningList(u: TopbarWarningStrip, tc: TopbarContext): HTMLElement {
  const list = document.createElement("div");
  list.className = "topbar-warning-list list-rows";
  for (const [index, warning] of u.warnings.entries()) {
    list.append(drawTopbarWarning(warning, tc, u, `TopbarWarningStrip.warnings[${index}]`));
  }
  return list;
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

  const row = document.createElement("button");
  row.type = "button";
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

  const back = document.createElement("button");
  back.type = "button";
  back.className = "topbar-warning-back";
  back.setAttribute("data-back", "");
  back.textContent = "‹ warnings";
  back.addEventListener("click", (event) => {
    event.stopPropagation();
    tc.reveals.open("warnings", "warnings", () => drawWarningList(strip, tc));
  });
  overlay.append(back);

  overlay.append(drawWarningDetail(u, tc, path));
  return overlay;
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
