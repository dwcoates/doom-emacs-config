/**
 * counter-menu — the shared facade behind every topbar dropdown counter
 * (subagents, tasks, and any future roster).
 *
 * A counter is a chip in the session-datapoint run: a count, a caret, an
 * optional "N still working" badge, and an overlay listing one row per
 * entry with a status dot. Each concrete counter (see `agents.ts`,
 * `tasks.ts`) is a thin specialization: it projects `StoreState.items`
 * into `CounterEntry[]` and supplies a `CounterSpec` naming its DOM
 * classes and nouns. Everything visual and every retention rule lives
 * HERE, so the two counters can never drift in look or behavior.
 *
 * Retention: a terminal entry is not dropped the instant it settles. It
 * lingers in the roster for `RETENTION_TURNS` counted turns after it went
 * terminal, greyed and stamped with how long ago that was, then falls off
 * the list (and out of the count) entirely. An entry that is still active
 * never prunes.
 */
import { escapeHtml } from "./highlight.js";

/**
 * Where an entry sits in its life, shared across every counter. Concrete
 * counters map their own vocabulary onto these four: a task's `pending`
 * reads as `starting`, its `in_progress` as `running`, its `completed`
 * as `done`; a subagent uses all four directly.
 */
export type CounterStatus = "starting" | "running" | "done" | "error";

/** The two statuses that count an entry as still working (active). */
const ACTIVE_STATUSES: ReadonlySet<CounterStatus> = new Set([
  "starting",
  "running",
]);

/**
 * How many counted turns a terminal entry survives in the roster after it
 * settles. At turn-distance 0 through `RETENTION_TURNS` it still renders
 * (greyed, with its age); beyond that it is pruned from both the overlay
 * and the chip's count.
 */
export const RETENTION_TURNS = 5;

/** One row of a counter's roster: the projection every counter feeds in. */
export interface CounterEntry {
  /** Stable key: the tool-use id, task id, or whatever the counter tracks. */
  id: string;
  /** The row's headline text (a subagent's description, a task's subject). */
  summary: string;
  /** A secondary chip beside the summary (a subagent's type), else empty. */
  detail: string;
  status: CounterStatus;
  /** Whether the row is indented as spawned-by-another (subagent nesting). */
  nested: boolean;
  /**
   * The counted-turn ordinal at which the entry went terminal, or null
   * while it is still active. Drives both the age label and the prune.
   */
  deactivatedAtTurn: number | null;
}

/** The DOM/vocabulary knobs that make one counter look like itself. */
export interface CounterSpec {
  /** Plural class stem for chip/menu/overlay (e.g. `agents` → `agents-menu`). */
  menu: string;
  /** Singular class stem for a row and its parts (e.g. `agent` → `agent-row`). */
  item: string;
  /** Chip label noun (e.g. `agent` → `3 agents`). */
  noun: string;
  /** The working-count badge noun (e.g. `running` → `2 running`). */
  busyNoun: string;
  /** The chip's `title` attribute. */
  title: string;
  /** Row headline shown when `summary` is still empty. */
  placeholder: string;
}

/** Whether the entry is still working (not yet terminal). */
export function isActive(entry: CounterEntry): boolean {
  return ACTIVE_STATUSES.has(entry.status);
}

/** How many of the roster's entries are still working. */
export function activeCount(entries: readonly CounterEntry[]): number {
  return entries.filter(isActive).length;
}

/**
 * Counted turns since the entry went terminal, or null while it is still
 * active. Never negative: a stamp from a later turn than `currentTurn`
 * (which cannot happen in order) clamps to 0.
 */
export function turnsSince(
  entry: CounterEntry,
  currentTurn: number,
): number | null {
  if (entry.deactivatedAtTurn === null) return null;
  return Math.max(0, currentTurn - entry.deactivatedAtTurn);
}

/**
 * Whether the entry still belongs in the roster: every active entry, plus
 * a terminal one whose age has not yet passed `RETENTION_TURNS`.
 */
export function isVisible(entry: CounterEntry, currentTurn: number): boolean {
  const age = turnsSince(entry, currentTurn);
  return age === null || age <= RETENTION_TURNS;
}

/** The roster minus entries that have aged out of the retention window. */
export function pruneByRecency(
  entries: readonly CounterEntry[],
  currentTurn: number,
): CounterEntry[] {
  return entries.filter((e) => isVisible(e, currentTurn));
}

/** `just now` / `1 turn ago` / `N turns ago` — a terminal entry's age. */
export function turnsAgoLabel(age: number): string {
  if (age <= 0) return "just now";
  return `${age} turn${age === 1 ? "" : "s"} ago`;
}

/** `1 agent` / `3 tasks` — the chip's own label. */
export function countLabel(noun: string, count: number): string {
  return `${count} ${noun}${count === 1 ? "" : "s"}`;
}

/**
 * A counter's chip and (when open) its overlay. Renders to nothing when
 * the pruned roster is empty: a zero chip is a control over an empty list,
 * so it hides until the session has something to count.
 *
 * OPEN is renderer-owned disclosure state (like the feed's question
 * selections), so the overlay survives the per-frame topbar re-render.
 * CURRENT-TURN is the counted-turn clock the retention window measures
 * against.
 */
export function counterMenuHtml(
  spec: CounterSpec,
  entries: readonly CounterEntry[],
  open: boolean,
  currentTurn: number,
): string {
  const visible = pruneByRecency(entries, currentTurn);
  if (visible.length === 0) return "";
  const busy = activeCount(visible);
  const badge =
    busy > 0
      ? ` <span class="${spec.menu}-running">${busy} ${escapeHtml(spec.busyNoun)}</span>`
      : "";
  return `<span class="${spec.menu}-menu">
      <button type="button" class="info-${spec.menu}" data-${spec.menu}-toggle aria-expanded="${open}" aria-haspopup="true" title="${escapeHtml(
        spec.title,
      )}">${escapeHtml(countLabel(spec.noun, visible.length))}${badge} <span class="${spec.menu}-caret" aria-hidden="true">${open ? "▴" : "▾"}</span></button>
      ${open ? counterOverlayHtml(spec, visible, currentTurn) : ""}
    </span>`;
}

/**
 * The dropped roster: one row per visible entry, in projection order. A
 * terminal (inactive) row is greyed via the `inactive` class and trades
 * its bare status word for `<status> · <age>`, so the reader sees both
 * what happened and how long ago.
 */
export function counterOverlayHtml(
  spec: CounterSpec,
  entries: readonly CounterEntry[],
  currentTurn: number,
): string {
  const rows = entries
    .map((e) => {
      const detail = e.detail
        ? `<span class="${spec.item}-type">${escapeHtml(e.detail)}</span>`
        : "";
      const headline = e.summary === "" ? spec.placeholder : e.summary;
      const age = turnsSince(e, currentTurn);
      const inactive = age !== null;
      const statusText = inactive
        ? `${e.status} · ${turnsAgoLabel(age)}`
        : e.status;
      const rowClass = `${spec.item}-row${e.nested ? " nested" : ""}${
        inactive ? " inactive" : ""
      }`;
      return `<li class="${rowClass}">
        <span class="${spec.item}-dot ${spec.item}-${e.status}" aria-hidden="true">●</span>
        <span class="${spec.item}-desc">${escapeHtml(headline)}</span>
        ${detail}
        <span class="${spec.item}-status">${escapeHtml(statusText)}</span>
      </li>`;
    })
    .join("");
  return `<ul class="${spec.menu}-overlay" role="menu">${rows}</ul>`;
}
