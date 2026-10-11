/**
 * handback-badge — the small inline badge a HANDBACK leaves in a feed: a
 * subagent reporting back to its parent, or a detached merge reporting back to
 * the workspace that asked for it.
 *
 * It is the shared `.badge` pill, quiet, carrying the daemon's label and
 * nothing else: no body, no toggle, no bubble chrome (owner ruling,
 * 2026-09-27: "a little marker in the feed is good, but it shouldnt be a bubble
 * or card, just a little badge"). What was handed back is read where it lives,
 * in the subagent's card or the merge's bubble, and never drawn twice.
 */
import { log } from "../../log.js";

/** Where one kind of handback badge is hooked and recorded. */
export interface HandbackBadgeKind {
  /** The class the badge's hooks and its tone know it by. */
  readonly className: string;
  /** The record's operation. */
  readonly operation: string;
  /** What the badge marks, for the record ("a subagent hand-back"). */
  readonly describe: string;
}

/** A handback badge: LABEL, the daemon's words, drawn verbatim in the shared pill. */
export function drawHandbackBadge(label: string, kind: HandbackBadgeKind): HTMLElement {
  log.info(`drawing ${kind.describe} badge`, {
    operation: kind.operation,
    context: { label },
  });
  const el = document.createElement("div");
  el.className = `badge ${kind.className}`;
  el.textContent = label;
  return el;
}
