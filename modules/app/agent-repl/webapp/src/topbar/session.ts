/**
 * The connectivity indicator's dropdown: agent-repl's SESSION (owner ruling,
 * 2026-10-06) — how long it has run and the vendor traffic since it began.
 *
 * THE DAEMON STATES THE INSTANT, THE CLIENT TICKS THE SPAN. `started_at_ms` is
 * the session's start; the duration is this page's now against it, repainted
 * by the one shared ticker, and the daemon never pushes a tick.
 *
 * THE ARM IS THE CAUSE. A login made through agent-repl or this Emacs starting
 * began the session; the row says which.
 *
 * NO SESSION, NO DROPDOWN. Before any session began the indicator carries none
 * (topbar.proto), and the glyph binds nothing: it stays what it was, the left
 * half of the account cell, whose click opens the account options. A menu over
 * nothing only invites the click that proves it empty.
 *
 * THE GLYPH IS ITS OWN DOOR. It sits inside the account cell, whose click opens
 * the account options; the glyph's click is taken here and goes no further, so
 * one click opens one dropdown.
 */
import type { TopbarAgentReplSession } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { formatTickedAge } from "../duration.js";
import { tickWhileShown } from "../feed/ticking.js";
import { log } from "../log.js";
import { msOf, requireCase, unreachableArm } from "../rpc/strict.js";
import type { TopbarContext } from "./context.js";
import { asAnchor } from "./strip.js";

/** The reveal's name, and its anchor's. */
export const SESSION_REVEAL = "agent-repl-session";

/** Bytes in a megabyte and a gigabyte: decimal, as the operating system counts. */
const MB = 1_000_000;
const GB = 1_000_000_000;

/**
 * Wire the connectivity glyph's dropdown, when the view carries a session.
 */
export function bindAgentReplSessionReveal(
  glyph: HTMLElement,
  session: TopbarAgentReplSession | undefined,
  tc: TopbarContext,
): void {
  if (session === undefined) return;
  asAnchor(glyph, SESSION_REVEAL);
  const body = (): HTMLElement => drawAgentReplSession(session, tc);
  // Registered as it is drawn, so a push arriving while the dropdown is open
  // re-opens it with THIS push's traffic.
  tc.reveals.register(SESSION_REVEAL, SESSION_REVEAL, body);
  glyph.addEventListener("click", (event) => {
    event.stopPropagation();
    tc.reveals.toggle(SESSION_REVEAL, SESSION_REVEAL, body);
  });
}

/** The dropdown: the session's span, ticking, and its traffic. */
export function drawAgentReplSession(u: TopbarAgentReplSession, tc: TopbarContext): HTMLElement {
  const began = requireCase(u.began, "TopbarAgentReplSession.began");
  const startedAt = msOf(u.startedAtMs, "TopbarAgentReplSession.started_at_ms");
  log.debug("drawing agent-repl's session", {
    operation: "topbar.agent-repl-session",
    context: { began: began.case, bytes_received: u.bytesReceived.toString(), bytes_sent: u.bytesSent.toString() },
  });

  const panel = document.createElement("div");
  panel.className = "topbar-agent-repl-session list-rows";
  panel.setAttribute("data-began", began.case);

  const duration = row("topbar-session-duration", beganLabel(began.case));
  const span = duration.value;
  tickWhileShown(span, tc.ctx.ticker, (nowMs) => {
    span.textContent = formatTickedAge(nowMs - startedAt);
  });

  const traffic = row("topbar-session-traffic", "vendor traffic");
  traffic.value.textContent = formatTraffic(u.bytesReceived, u.bytesSent);

  panel.append(duration.element, traffic.element);
  return panel;
}

/** What began the session, as its row's label. */
function beganLabel(arm: string): string {
  switch (arm) {
    case "login":
      return "since login";
    case "editorStart":
      return "since Emacs started";
    default:
      return unreachableArm("TopbarAgentReplSession.began", arm);
  }
}

/** One label-and-value row. */
function row(className: string, label: string): { element: HTMLElement; value: HTMLElement } {
  const element = document.createElement("div");
  element.className = `topbar-session-row ${className}`;
  const name = document.createElement("span");
  name.className = "topbar-session-label";
  name.textContent = label;
  const value = document.createElement("span");
  value.className = "topbar-session-value";
  element.append(name, value);
  return { element, value };
}

/** The traffic line: received first, then sent. */
export function formatTraffic(received: bigint, sent: bigint): string {
  return `${formatBytes(received)} ↓ · ${formatBytes(sent)} ↑`;
}

/**
 * A byte count as the reader weighs it, in decimal megabytes or gigabytes:
 * `0 MB`, `< 0.1 MB`, `3.4 MB`, `412 MB`, `1.2 GB`, `38 GB`, `120 GB`.
 *
 * One fractional digit while the figure is under ten of its unit, none above
 * it, so the line keeps two or three significant figures whatever the size.
 * THE UNIT IS CHOSEN BY THE RENDERED VALUE: 999.95 MB rounds to 1000 MB, which
 * no reader should be shown, so it reads `1 GB`.
 */
export function formatBytes(n: bigint): string {
  if (n < 0n) throw new RangeError(`a byte count is never negative: ${n.toString()}`);
  if (n === 0n) return "0 MB";
  const bytes = Number(n);
  if (bytes < MB / 10) return "< 0.1 MB";
  const mb = round(bytes / MB);
  if (mb < 1000) return `${trim(mb)} MB`;
  return `${trim(round(bytes / GB))} GB`;
}

/** One fractional digit under ten, none at or above it. */
function round(value: number): number {
  return value < 10 ? Math.round(value * 10) / 10 : Math.round(value);
}

/** The figure, with a bare ".0" dropped. */
function trim(value: number): string {
  return value.toFixed(value < 10 ? 1 : 0).replace(/\.0$/, "");
}
