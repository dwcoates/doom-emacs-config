/**
 * counter-menu — the shared facade behind every topbar dropdown counter
 * (subagents, tasks, and any future roster).
 *
 * A counter is a chip in the session-datapoint run: a count, a caret, and
 * an overlay listing one row per entry with a status dot. Each concrete
 * counter (see `agents.ts`, `tasks.ts`) is a thin specialization: it
 * projects `StoreState.items` into `CounterEntry[]` and supplies a
 * `CounterSpec` naming its DOM classes and nouns. Everything visual lives
 * HERE, so the two counters can never drift in look or behavior.
 *
 * The roster shows ONLY entries that are still actively running: a terminal
 * entry (done or errored) drops out the instant it settles, so the topbar
 * is a live view of what async is running right now, never a log of what
 * has finished.
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
}

/** The DOM/vocabulary knobs that make one counter look like itself. */
export interface CounterSpec {
  /** Plural class stem for chip/menu/overlay (e.g. `agents` → `agents-menu`). */
  menu: string;
  /** Singular class stem for a row and its parts (e.g. `agent` → `agent-row`). */
  item: string;
  /** Chip label noun (e.g. `agent` → `3 agents`). */
  noun: string;
  /** The chip's `title` attribute. */
  title: string;
  /** Row headline shown when `summary` is still empty. */
  placeholder: string;
}

/** Whether the entry is still working (not yet terminal). */
export function isActive(entry: CounterEntry): boolean {
  return ACTIVE_STATUSES.has(entry.status);
}

/** `1 agent` / `3 tasks` — the chip's own label. */
export function countLabel(noun: string, count: number): string {
  return `${count} ${noun}${count === 1 ? "" : "s"}`;
}

/** How long a missing-bubble notice holds the topbar slot before clearing. */
export const MISSING_BUBBLE_NOTICE_MS = 4000;

/**
 * The topbar notice for a roster row whose entry has no bubble in the
 * current feed: a subagent discarded by `/clear`, or a task created before
 * one (the task roster is harness-global, so its rows outlive the cut that
 * removed their cards). A click on such a row must say why nothing moved
 * rather than silently doing nothing.
 */
export function missingBubbleNotice(spec: CounterSpec): string {
  return `${spec.noun} has no bubble in the current feed (discarded by /clear)`;
}

/**
 * The chip-and-overlay shell every topbar dropdown hangs from: a button
 * in the session-datapoint run (label, caret, matching data-toggle) and,
 * when open, the overlay it drops. STEM names the DOM family (`agents` →
 * `.agents-menu`, `data-agents-toggle`, `.agents-caret`); LABEL-HTML is
 * the caller's chip content, already escaped where it needs to be; the
 * OVERLAY thunk runs only while the chip is open, so a closed chip never
 * pays for the list it is not showing. Shared by the counter menus here
 * and the tokens menu (`tokens.ts`), so the chips can never drift in
 * shape.
 */
export function dropdownChipHtml(
  stem: string,
  labelHtml: string,
  title: string,
  open: boolean,
  overlay: () => string,
): string {
  return `<span class="${stem}-menu">
      <button type="button" class="info-${stem}" data-${stem}-toggle aria-expanded="${open}" aria-haspopup="true" title="${escapeHtml(
        title,
      )}">${labelHtml} <span class="${stem}-caret" aria-hidden="true">${open ? "▴" : "▾"}</span></button>
      ${open ? overlay() : ""}
    </span>`;
}

/**
 * A counter's chip and (when open) its overlay. The roster is filtered to
 * the entries still actively running (`isActive`): a settled entry drops
 * out at once, so the chip counts only live async. Renders to nothing when
 * no entry is active — a zero chip is a control over an empty list, so it
 * hides until the scope has something running.
 *
 * OPEN is renderer-owned disclosure state (like the feed's question
 * selections), so the overlay survives the per-frame topbar re-render.
 */
export function counterMenuHtml(
  spec: CounterSpec,
  entries: readonly CounterEntry[],
  open: boolean,
): string {
  const visible = entries.filter(isActive);
  if (visible.length === 0) return "";
  return dropdownChipHtml(
    spec.menu,
    escapeHtml(countLabel(spec.noun, visible.length)),
    spec.title,
    open,
    () => counterOverlayHtml(spec, visible),
  );
}

/**
 * The dropped roster: one row per active entry, in projection order, each
 * carrying its status dot and bare status word.
 */
export function counterOverlayHtml(
  spec: CounterSpec,
  entries: readonly CounterEntry[],
): string {
  const rows = entries
    .map((e) => {
      const detail = e.detail
        ? `<span class="${spec.item}-type">${escapeHtml(e.detail)}</span>`
        : "";
      const headline = e.summary === "" ? spec.placeholder : e.summary;
      const rowClass = `${spec.item}-row${e.nested ? " nested" : ""}`;
      // The id is the entry's stable key (a subagent's tool-use id, a
      // task's id): it addresses the row so a click can act on the entry
      // it names — both rosters jump the feed to the entry's bubble.
      return `<li class="${rowClass}" data-${spec.item}-id="${escapeHtml(e.id)}">
        <span class="${spec.item}-dot ${spec.item}-${e.status}" aria-hidden="true">●</span>
        <span class="${spec.item}-desc">${escapeHtml(headline)}</span>
        ${detail}
        <span class="${spec.item}-status">${escapeHtml(e.status)}</span>
      </li>`;
    })
    .join("");
  return `<ul class="${spec.menu}-overlay" role="menu">${rows}</ul>`;
}
