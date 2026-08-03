/**
 * The SCHEDULED-SHUTDOWN DRAIN LEASE surface: the one place the webapp says
 * out loud that a daemon bounce is already scheduled and is waiting for work
 * to finish before it executes.
 *
 * WHY IT IS GLOBAL. The lease is the DAEMON's, not a workspace's: while it is
 * held, no new turn may start ANYWHERE, and the bounce fires the moment the
 * last hold clears. Rendering it per-workspace would tell a reader whose own
 * session happens to be idle that nothing is happening, when in fact their
 * next prompt will be parked until a bounce they cannot see has run. So the
 * banner is chrome above the central column, filled from the daemon's own
 * broadcast and empty (collapsed) on `idle`.
 *
 * WHAT IT NEVER DOES. It derives nothing. `holds` is the complete, live answer
 * to "what is the bounce waiting on", so the banner enumerates that list and
 * does not cross-check it against per-workspace states, which would make the
 * webapp a second, divergent authority on drain progress.
 */

import { formatAge } from "./duration.js";
import { escapeHtml } from "./highlight.js";
import type {
  ShutdownHold,
  ShutdownScheduleDraining,
  ShutdownScheduleView,
} from "./frontend-proto.js";

/** The document-wide marker the chrome paints against while a bounce drains. */
export const DRAINING_BODY_CLASS = "draining";

/**
 * The live draining lease, or null when the lease is idle or unknown.
 *
 * `null` covers BOTH "the daemon says idle" and "no daemon has spoken yet",
 * deliberately: neither is a drain, and the banner's only question is whether
 * one is in force. The distinction that matters — absent means no information,
 * so nothing is seeded — is enforced upstream in the decoder and the adapter,
 * which never manufacture an `idle` the daemon did not send.
 */
export type DrainLease = ShutdownScheduleDraining | null;

/** The draining arm's payload, or null when the view says `idle`. */
export function drainingOf(view: ShutdownScheduleView): DrainLease {
  return view.state.case === "draining" ? view.state.value : null;
}

/**
 * The last path segment of an absolute workspace CWD — what a reader calls the
 * workspace. The full path is kept as the row's tooltip rather than dropped,
 * since two worktrees of the same repo can share a leaf name.
 */
export function workspaceShortName(workspace: string): string {
  const trimmed = workspace.replace(/\/+$/, "");
  const at = trimmed.lastIndexOf("/");
  const leaf = at === -1 ? trimmed : trimmed.slice(at + 1);
  // An empty leaf can only come from a path that is nothing but separators,
  // which the decoder's non-empty check already refuses; keeping the raw value
  // means a row can never render as a blank name.
  return leaf === "" ? workspace : leaf;
}

/**
 * Why this workspace is holding the drain, in the reader's words.
 *
 * `turn` and `tasks` are CO-OCCURRING, so both are named when both are set —
 * showing only one would understate what the bounce is actually waiting for.
 * The decoder guarantees at least one is present, so this never returns "".
 */
export function holdReason(hold: ShutdownHold): string {
  const reasons: string[] = [];
  if (hold.turn !== undefined) reasons.push("turn in flight");
  if (hold.tasks !== undefined) {
    reasons.push(`${hold.tasks.count} live task${hold.tasks.count === 1 ? "" : "s"}`);
  }
  return reasons.join(", ");
}

/** One hold's whole line: `workspace — turn in flight, 2 live tasks`. */
export function holdLine(hold: ShutdownHold): string {
  return `${workspaceShortName(hold.workspace)} — ${holdReason(hold)}`;
}

/**
 * The banner's headline, exported separately from the markup so the wording is
 * assertable without parsing HTML (the same split `ungated.ts` uses).
 *
 * `nowMs` is passed in rather than read from a clock here: the banner is
 * repainted on the chrome cadence, and a helper with its own clock could not
 * be tested at a fixed elapsed reading.
 */
export function drainHeadline(lease: NonNullable<DrainLease>, nowMs: number): string {
  const elapsed = formatAge(nowMs - lease.scheduledAtMs);
  return `Daemon bounce scheduled — ${lease.cause} · draining ${elapsed}`;
}

/**
 * What the executed bounce will do to the running session shims. Stated
 * because the two outcomes differ for the reader: a preserved shim reattaches
 * to the next daemon with the conversation intact, a stopped one does not.
 */
export function drainShimNote(lease: NonNullable<DrainLease>): string {
  return lease.stopShims
    ? "sessions will be stopped and restarted"
    : "sessions are preserved across the bounce";
}

/**
 * The banner element's inner HTML, or "" when no drain is in force (which
 * collapses the slot via `:empty`).
 *
 * There is no dismiss control, by design: the banner is a pure function of the
 * live lease and is re-rendered from state on every chrome frame, so it cannot
 * be closed while the bounce is pending. Every value the daemon chose — the
 * cause and each workspace path — is escaped, since all of it is free text.
 */
export function drainBannerHtml(lease: DrainLease, nowMs: number): string {
  if (lease === null) return "";
  const holds = lease.holds
    .map(
      (hold) =>
        `<li class="drain-hold" title="${escapeHtml(hold.workspace)}">` +
        `${escapeHtml(holdLine(hold))}</li>`,
    )
    .join("");
  return (
    `<span class="drain-mark" aria-hidden="true">⏻</span>` +
    `<div class="drain-body">` +
    `<div class="drain-head">${escapeHtml(drainHeadline(lease, nowMs))}</div>` +
    `<div class="drain-note">Waiting on ${lease.holds.length} workspace` +
    `${lease.holds.length === 1 ? "" : "s"}; ${escapeHtml(drainShimNote(lease))}.` +
    `</div>` +
    `<ul class="drain-holds">${holds}</ul>` +
    `</div>`
  );
}
