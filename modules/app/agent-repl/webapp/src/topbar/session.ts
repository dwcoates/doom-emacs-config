/**
 * The connectivity indicator's dropdown: agent-repl's SESSION (owner ruling,
 * 2026-10-06) — how long it has run and what began it. The vendor traffic it
 * once also drew was removed with the traffic measurement (owner ruling,
 * 2026-10-06), whose kernel sockets are the inferred cause of the kernel's
 * network buffer exhaustion.
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
  // re-opens it with THIS push's session.
  tc.reveals.register(SESSION_REVEAL, SESSION_REVEAL, body);
  glyph.addEventListener("click", (event) => {
    event.stopPropagation();
    tc.reveals.toggle(SESSION_REVEAL, SESSION_REVEAL, body);
  });
}

/** The dropdown: the session's span, ticking, labeled by what began it. */
export function drawAgentReplSession(u: TopbarAgentReplSession, tc: TopbarContext): HTMLElement {
  const began = requireCase(u.began, "TopbarAgentReplSession.began");
  const startedAt = msOf(u.startedAtMs, "TopbarAgentReplSession.started_at_ms");
  log.debug("drawing agent-repl's session", {
    operation: "topbar.agent-repl-session",
    context: { began: began.case, started_at_ms: startedAt },
  });

  const panel = document.createElement("div");
  panel.className = "topbar-agent-repl-session list-rows";
  panel.setAttribute("data-began", began.case);

  const duration = row("topbar-session-duration", beganLabel(began.case));
  const span = duration.value;
  tickWhileShown(span, tc.ctx.ticker, (nowMs) => {
    span.textContent = formatTickedAge(nowMs - startedAt);
  });

  panel.append(duration.element);
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
