/**
 * Workspace sidebar — the Emacs drawer ported into the GUI.
 *
 * Emacs owns every fact and ships a pre-computed view-model (see
 * shared/protocol.md, "Workspace sidebar stream"); this module renders it
 * verbatim and never re-derives ordering, bucketing, or tree structure.
 * The only page-local state is the cursor (which the drawer already snaps
 * to the active workspace on every switch, so page-local is behaviorally
 * identical), a dismissable toast, and the clock tick.
 *
 * Design mirrors the rest of the webapp: pure string-building render
 * functions and pure targeting/cursor helpers (node-testable, no DOM),
 * with a thin DOM/WS shell (`initSidebar`) around them. All snapshot
 * text is escaped on the way into HTML.
 */
import { escapeHtml } from "./highlight.js";
import type {
  ActionRequest,
  Detail,
  Entry,
  Group,
  MergeCommitRow,
  MergeQueue,
  Section,
  SidebarActionName,
  Snapshot,
} from "./sidebar-types.js";
import { WsClient } from "./ws.js";

/** Seconds without a fresh snapshot before the stale badge appears. */
export const STALE_AFTER_SECONDS = 90;
/** How long an action-failure toast stays up before auto-dismissing. */
const TOAST_MS = 8000;
/** How long a `C-c` interrupt prefix stays armed waiting for `C-k`. */
const INTERRUPT_PREFIX_MS = 2000;

/* ------------------------------------------------------------------ *
 * Clock formatting                                                    *
 * ------------------------------------------------------------------ */

/**
 * Compound relative "… ago" string: every unit from the largest non-zero
 * one down to whole seconds, keeping intermediate zeros so the readout
 * stays contiguous (1820 → "30m 20s ago", 3601 → "1h 0m 1s ago").
 * Seconds are always the trailing unit, even when zero; negatives clamp
 * to 0 so a just-completed merge reads "0s ago". The drawer twin is
 * `agent-repl-drawer--format-merge-ago`.
 */
export function formatMergeAgo(seconds: number): string {
  const total = Math.max(0, Math.round(seconds));
  const units: Array<[number, string]> = [
    [Math.floor(total / 86400), "d"],
    [Math.floor((total % 86400) / 3600), "h"],
    [Math.floor((total % 3600) / 60), "m"],
    [total % 60, "s"],
  ];
  const parts: string[] = [];
  let started = false;
  for (const [value, suffix] of units) {
    // Skip leading zero units, but once a non-zero unit appears keep
    // every finer one (including zeros) down to the seconds field.
    if (started || value > 0 || suffix === "s") {
      started = true;
      parts.push(`${value}${suffix}`);
    }
  }
  return `${parts.join(" ")} ago`;
}

/**
 * Single-coarse-unit "… ago" string, the drawer's
 * `agent-repl-drawer--format-duration`: whole seconds under a minute,
 * whole minutes under an hour, then one-decimal hours and days.
 * Negatives (a stamp fractionally ahead of this clock) clamp to 0.
 */
export function formatDuration(seconds: number): string {
  const s = Math.max(0, seconds);
  if (s < 60) return `${Math.round(s)}s ago`;
  if (s < 3600) return `${Math.round(s / 60)}m ago`;
  if (s < 86400) return `${(s / 3600).toFixed(1)}h ago`;
  return `${(s / 86400).toFixed(1)}d ago`;
}

/** Elapsed merge clock as `M:SS` (drawer `--merge-elapsed-string` shape). */
export function formatMergeClock(seconds: number): string {
  const total = Math.max(0, Math.floor(seconds));
  return `${Math.floor(total / 60)}:${String(total % 60).padStart(2, "0")}`;
}

/* ------------------------------------------------------------------ *
 * Cursor + targeting (pure)                                           *
 * ------------------------------------------------------------------ */

/**
 * Cursor refs are the rendered rows' `data-entry` values: `ws:<name>`
 * for workspace rows, `repo:<key>` for repo group headers.
 */
export function wsOf(ref: string | null): string | null {
  return ref !== null && ref.startsWith("ws:") ? ref.slice(3) : null;
}

export function repoOf(ref: string | null): string | null {
  return ref !== null && ref.startsWith("repo:") ? ref.slice(5) : null;
}

/**
 * The ordered list of navigable entries, exactly as rendered: group
 * headers and workspace rows in snapshot order, with a folded group
 * contributing only its header. Merge-queue rows are never navigable,
 * matching the drawer (commit rows are invisible to j/k).
 */
export function navigableRefs(snap: Snapshot): string[] {
  const refs: string[] = [];
  for (const section of snap.sections) {
    for (const group of section.groups) {
      refs.push(`repo:${group.key}`);
      if (!group.folded) {
        for (const entry of group.entries) refs.push(`ws:${entry.ws}`);
      }
    }
  }
  return refs;
}

/**
 * Move the cursor by DELTA entries, clamped at the list edges (the
 * drawer clamps rather than wraps). A cursor not in the list (or null)
 * lands on the first entry; an empty list has no cursor.
 */
export function stepCursor(refs: string[], cursor: string | null, delta: number): string | null {
  if (refs.length === 0) return null;
  const at = cursor === null ? -1 : refs.indexOf(cursor);
  if (at === -1) return refs[0];
  return refs[Math.min(refs.length - 1, Math.max(0, at + delta))];
}

/**
 * Reconcile the page-local cursor against a fresh snapshot. When
 * `current_ws` CHANGED between snapshots, the cursor snaps to it (the
 * drawer's cursor-follows-switch rule, R2); otherwise a still-valid
 * cursor is kept, and a vanished one falls back to the first entry.
 */
export function syncCursor(opts: {
  cursor: string | null;
  refs: string[];
  prevCurrent: string | null;
  current: string | null;
}): string | null {
  const { cursor, refs, prevCurrent, current } = opts;
  if (current !== prevCurrent && current !== null && refs.includes(`ws:${current}`)) {
    return `ws:${current}`;
  }
  if (cursor !== null && refs.includes(cursor)) return cursor;
  return refs.length > 0 ? refs[0] : null;
}

/**
 * Marks-or-point targeting (drawer K21): the marked set when non-empty,
 * else the workspace under the cursor. Empty when neither exists (e.g.
 * no marks and the cursor sits on a group header) — the caller no-ops.
 */
export function actionTargets(marks: string[], cursor: string | null): string[] {
  if (marks.length > 0) return [...marks];
  const ws = wsOf(cursor);
  return ws !== null ? [ws] : [];
}

/** The subset of TARGETS that sit in the MERGED section. */
export function mergedTargets(snap: Snapshot, targets: string[]): string[] {
  const merged = new Set<string>();
  for (const section of snap.sections) {
    for (const group of section.groups) {
      for (const entry of group.entries) {
        if (entry.section === "merged") merged.add(entry.ws);
      }
    }
  }
  return targets.filter((t) => merged.has(t));
}

/**
 * Whether the snapshot carries any client-ticked clock: a merge commit's
 * elapsed/resolver stamp, or a visible (expanded) entry's merged-ago /
 * last-prompt-ago stamp. Nothing else in the render moves with time, so
 * the 1 s re-render interval runs only while this is true.
 */
export function snapshotTicks(snap: Snapshot): boolean {
  const rows = snap.merge_queue?.rows ?? [];
  for (const row of rows) {
    if (
      row.kind === "commit" &&
      (typeof row.started_at === "number" || typeof row.resolver_started_at === "number")
    ) {
      return true;
    }
  }
  for (const section of snap.sections) {
    for (const group of section.groups) {
      for (const entry of group.entries) {
        const d = entry.detail;
        if (d && (typeof d.merge_completed_at === "number" || typeof d.last_prompt_at === "number")) {
          return true;
        }
      }
    }
  }
  return false;
}

/* ------------------------------------------------------------------ *
 * HTML (pure string building — node-testable)                         *
 * ------------------------------------------------------------------ */

/** Section rule width, matching the drawer's 12-`─` rule line. */
const SECTION_RULE = "─".repeat(12);

function lineHtml(gutter: string, indentRem: number, body: string, extraClass = ""): string {
  const cls = extraClass === "" ? "sidebar-line" : `sidebar-line ${extraClass}`;
  const pad = indentRem > 0 ? ` style="padding-left:${indentRem}rem"` : "";
  return (
    `<div class="${cls}"><span class="sidebar-gutter">${gutter}</span>` +
    `<span class="sidebar-line-body"${pad}>${body}</span></div>`
  );
}

/**
 * The gutter glyph for a row: the red mark dot takes the gutter over the
 * cursor arrow (drawer C15 — mark precedence), the arrow marks the
 * page-local cursor, and everything else gets an empty fixed-width span.
 */
function gutterHtml(marked: boolean, atCursor: boolean): string {
  if (marked) return `<span class="sidebar-marked">●</span>`;
  if (atCursor) return `<span class="sidebar-cursor-arrow">▶</span>`;
  return "";
}

function detailLinesHtml(detail: Detail, indentRem: number, now: number): string {
  const lines: string[] = [];
  const line = (label: string, value: string, cls: string): void => {
    lines.push(
      lineHtml(
        "",
        indentRem + 1.6,
        `<span class="sidebar-detail-label">${escapeHtml(label)}</span> ` +
          `<span class="sidebar-${cls}">${escapeHtml(value)}</span>`,
        "sidebar-detail",
      ),
    );
  };
  if (detail.merge_status) line("merge:", detail.merge_status, "detail-merge-status");
  if (detail.branch) line("branch:", detail.branch, "detail-branch");
  if (detail.merged_into) line("merged into:", detail.merged_into, "detail-merge-target");
  if (typeof detail.merge_completed_at === "number") {
    line("merged:", formatMergeAgo(now - detail.merge_completed_at), "detail-merged-time");
  }
  if (typeof detail.ahead_master === "number") {
    line(`ahead ${detail.trunk ?? "master"}:`, String(detail.ahead_master), "detail-ahead-master");
  }
  // Named after the actual source branch, and suppressed when the source
  // IS the trunk (the line would restate "ahead <trunk>" verbatim).
  if (
    typeof detail.ahead_source === "number" &&
    detail.source_branch &&
    detail.source_branch !== detail.trunk
  ) {
    line(`ahead ${detail.source_branch}:`, String(detail.ahead_source), "detail-ahead-source");
  }
  if (detail.last_commit) {
    line(
      "last commit:",
      detail.last_commit_time ? `${detail.last_commit} (${detail.last_commit_time})` : detail.last_commit,
      "detail-last-commit",
    );
  }
  if (typeof detail.dirty_count === "number" && detail.dirty_count > 0) {
    line("dirty:", `${detail.dirty_count} files`, "detail-dirty");
  }
  if (typeof detail.last_prompt_at === "number") {
    line("last prompt:", formatDuration(now - detail.last_prompt_at), "detail-last-prompt");
  }
  if (typeof detail.pending_prompts === "number" && detail.pending_prompts > 0) {
    line("pending:", `${detail.pending_prompts} prompt(s)`, "detail-pending");
  }
  for (const ws of detail.merged_in ?? []) line("merged in:", ws, "detail-merged-in");
  return lines.join("");
}

function entryHtml(entry: Entry, cursor: string | null, now: number): string {
  const ref = `ws:${entry.ws}`;
  const atCursor = cursor === ref;
  const indentRem = entry.depth * 0.8;
  const classes = ["sidebar-row", "sidebar-ws"];
  if (entry.hidden) classes.push("sidebar-dim");
  if (atCursor) classes.push("sidebar-at-cursor");
  const priority = entry.priority
    ? `<span class="sidebar-priority">${escapeHtml(entry.priority)}</span> `
    : "";
  const nameStyle = entry.name_color ? ` style="color:${escapeHtml(entry.name_color)}"` : "";
  const dirty = entry.dirty ? ` <span class="sidebar-dirty">●</span>` : "";
  const header = lineHtml(
    gutterHtml(entry.marked, atCursor),
    indentRem,
    `<span class="sidebar-glyph">${escapeHtml(entry.glyph)}</span>  ${priority}` +
      `<span class="sidebar-name"${nameStyle}>${escapeHtml(entry.ws)}</span>${dirty}`,
    "sidebar-ws-header",
  );
  const summary = lineHtml(
    "",
    indentRem + 0.8,
    escapeHtml(entry.summary),
    "sidebar-ws-summary",
  );
  const detail = entry.detail ? detailLinesHtml(entry.detail, indentRem, now) : "";
  return (
    `<div class="${classes.join(" ")}" data-entry="${escapeHtml(ref)}"` +
    ` title="${escapeHtml(entry.help)}">${header}${summary}${detail}</div>`
  );
}

function groupHtml(group: Group, cursor: string | null, now: number): string {
  const ref = `repo:${group.key}`;
  const atCursor = cursor === ref;
  const glyph = group.folded ? "▸" : "▾";
  const help = `Repo: ${group.label} (Tab to ${group.folded ? "unfold" : "fold"})`;
  const header =
    `<div class="sidebar-row sidebar-group${atCursor ? " sidebar-at-cursor" : ""}"` +
    ` data-entry="${escapeHtml(ref)}" title="${escapeHtml(help)}">` +
    lineHtml(
      gutterHtml(false, atCursor),
      0,
      `<span class="sidebar-fold">${glyph}</span> ` +
        `<span class="sidebar-group-label">${escapeHtml(group.label)}</span>`,
    ) +
    `</div>`;
  // A folded group renders its header and nothing else.
  if (group.folded) return header;
  return header + group.entries.map((entry) => entryHtml(entry, cursor, now)).join("");
}

function sectionHeaderHtml(label: string): string {
  return (
    `<div class="sidebar-section-title">${escapeHtml(label)}</div>` +
    `<div class="sidebar-section-rule">${SECTION_RULE}</div>`
  );
}

function sectionHtml(section: Section, cursor: string | null, now: number): string {
  const header = sectionHeaderHtml(`${section.label} (${section.count})`);
  const body =
    section.groups.length === 0
      ? `<div class="sidebar-empty">(none)</div>`
      : section.groups.map((group) => groupHtml(group, cursor, now)).join("");
  return `<section class="sidebar-section" data-section="${escapeHtml(section.id)}">${header}${body}</section>`;
}

function conflictDetailText(row: MergeCommitRow, threshold: number, now: number): string | null {
  const parts: string[] = [];
  if (typeof row.conflict_files === "number") {
    parts.push(`${row.conflict_files} file${row.conflict_files === 1 ? "" : "s"} unmerged`);
  }
  if (row.resolver_phase) {
    let resolver = `resolver: ${row.resolver_phase}`;
    if (typeof row.resolver_started_at === "number") {
      const s = now - row.resolver_started_at;
      if (s >= threshold) resolver += ` ${formatMergeClock(s)}`;
    }
    parts.push(resolver);
  }
  return parts.length > 0 ? parts.join(" · ") : null;
}

const MERGE_GLYPHS: Record<MergeCommitRow["state"], string> = {
  current: "⟳",
  conflict: "💥",
  halted: "⛔",
  pending: "",
};

function mergeCommitHtml(row: MergeCommitRow, threshold: number, now: number): string {
  const pending = row.state === "pending" || row.state === "halted";
  let elapsed = "";
  if (typeof row.started_at === "number") {
    const s = now - row.started_at;
    // The clock appears only once the commit is slow — its presence is
    // itself the signal, so a fast queue stays quiet.
    if (s >= threshold) {
      elapsed = `<span class="sidebar-mq-elapsed">${formatMergeClock(s)}</span>`;
    }
  }
  let html =
    `<div class="sidebar-line sidebar-mq-commit"><span class="sidebar-gutter"></span>` +
    `<span class="sidebar-mq-glyph">${MERGE_GLYPHS[row.state]}</span>` +
    `<span class="sidebar-mq-sha">${escapeHtml(row.sha)}</span>` +
    `<span class="sidebar-mq-subject ${pending ? "sidebar-mq-pending" : "sidebar-mq-current"}"` +
    ` title="${escapeHtml(row.subject)}">${escapeHtml(row.subject)}</span>${elapsed}</div>`;
  if (row.state === "conflict") {
    const detail = conflictDetailText(row, threshold, now);
    if (detail) {
      html += lineHtml("", 1.6, `<span class="sidebar-mq-conflict">${escapeHtml(detail)}</span>`);
    }
  }
  return html;
}

function mergeQueueHtml(queue: MergeQueue, threshold: number, now: number): string {
  const rows = queue.rows
    .map((row) =>
      row.kind === "separator"
        ? // Styled like a repo group header but non-navigable (no
          // data-entry): folding a project inside a commit stream is
          // meaningless, exactly as in the drawer.
          `<div class="sidebar-mq-separator">` +
          lineHtml(
            "",
            0,
            `<span class="sidebar-fold">▾</span> ` +
              `<span class="sidebar-group-label">${escapeHtml(row.project ?? "(no repo)")}</span>`,
          ) +
          `</div>`
        : mergeCommitHtml(row, threshold, now),
    )
    .join("");
  return (
    `<section class="sidebar-section sidebar-merge-queue" data-section="merge-queue">` +
    sectionHeaderHtml(`MERGE QUEUE (${queue.count})`) +
    rows +
    `</section>`
  );
}

/**
 * The whole sidebar as one HTML string for SNAP at NOW (epoch seconds),
 * with the page-local CURSOR ref and an optional TOAST error strip.
 * Rebuilt in full per snapshot — the list is small and one snapshot
 * always supersedes the previous one.
 */
export function snapshotHtml(
  snap: Snapshot,
  cursor: string | null,
  now: number,
  toast: string | null = null,
): string {
  const parts: string[] = [];
  if (toast !== null) {
    parts.push(
      `<div class="sidebar-toast">${escapeHtml(toast)}` +
        `<button type="button" class="sidebar-toast-close" data-toast-close` +
        ` title="dismiss">×</button></div>`,
    );
  }
  if (now - snap.generated_at > STALE_AFTER_SECONDS) {
    parts.push(
      `<div class="sidebar-stale">⚠ stale — last update ` +
        `${escapeHtml(formatDuration(now - snap.generated_at))}</div>`,
    );
  }
  if (snap.merge_queue) {
    parts.push(mergeQueueHtml(snap.merge_queue, snap.merge_slow_threshold, now));
  }
  for (const section of snap.sections) parts.push(sectionHtml(section, cursor, now));
  return parts.join("");
}

/* ------------------------------------------------------------------ *
 * DOM / WS shell                                                      *
 * ------------------------------------------------------------------ */

export interface SidebarOptions {
  el: HTMLElement;
  httpBase: string;
  wsBase: string;
  /** WebSocket constructor (injectable for tests). */
  wsFactory?: (url: string) => WebSocket;
}

export function initSidebar(opts: SidebarOptions): void {
  const { el, httpBase } = opts;
  let snap: Snapshot | null = null;
  let cursor: string | null = null;
  let prevCurrent: string | null = null;
  let toast: string | null = null;
  let lastToastedId: string | null = null;
  let toastTimer: ReturnType<typeof setTimeout> | null = null;
  let tickTimer: ReturnType<typeof setInterval> | null = null;
  let staleTimer: ReturnType<typeof setTimeout> | null = null;
  let interruptArmedUntil = 0;

  const nowSeconds = (): number => Date.now() / 1000;

  const render = (): void => {
    if (snap === null) return;
    // The aside tracks the Emacs-owned visibility flag: SPC o d and the
    // sidebar's own `q` both flip it, and every webview follows.
    el.hidden = snap.sidebar_visible === false;
    el.innerHTML = snapshotHtml(snap, cursor, nowSeconds(), toast);
  };

  const scrollCursorIntoView = (): void => {
    if (cursor === null) return;
    for (const node of Array.from(el.querySelectorAll("[data-entry]"))) {
      if (node.getAttribute("data-entry") === cursor) {
        node.scrollIntoView({ block: "center" });
        return;
      }
    }
  };

  const moveCursorTo = (ref: string | null): void => {
    if (ref === null || ref === cursor) return;
    cursor = ref;
    render();
    scrollCursorIntoView();
  };

  // Fire-and-forget: Emacs executes the action and reports refusals
  // through the snapshot's last_action_result, which toasts below.
  const post = (
    action: SidebarActionName,
    targets: string[],
    args: Record<string, unknown> = {},
    confirmed = false,
  ): void => {
    const body: ActionRequest = { action, targets, args, confirmed };
    void fetch(`${httpBase}/workspaces/action`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body),
    })
      .then((resp) => {
        if (!resp.ok) console.error(`sidebar action ${action}: HTTP ${resp.status}`);
      })
      .catch((err: unknown) => {
        console.error(`sidebar action ${action} failed`, err);
      });
  };

  const dismissToast = (): void => {
    toast = null;
    if (toastTimer !== null) {
      clearTimeout(toastTimer);
      toastTimer = null;
    }
  };

  // Clocks tick client-side, but ONLY while something on screen moves
  // with time; the stale badge gets its own one-shot at the threshold
  // so it appears even when no clock interval is running.
  const armClocks = (): void => {
    const need = snap !== null && snapshotTicks(snap);
    if (need && tickTimer === null) tickTimer = setInterval(render, 1000);
    if (!need && tickTimer !== null) {
      clearInterval(tickTimer);
      tickTimer = null;
    }
    if (staleTimer !== null) {
      clearTimeout(staleTimer);
      staleTimer = null;
    }
    if (snap !== null) {
      const delay = (snap.generated_at + STALE_AFTER_SECONDS - nowSeconds()) * 1000;
      if (delay > 0) {
        staleTimer = setTimeout(() => {
          staleTimer = null;
          render();
        }, delay);
      }
    }
  };

  const applySnapshot = (data: string): void => {
    let next: Snapshot;
    try {
      next = JSON.parse(data) as Snapshot;
    } catch (err: unknown) {
      console.error("sidebar: unparseable snapshot", err);
      return;
    }
    // Unknown frame types are ignored (forward compatibility); a version
    // the renderer was not built for is surfaced rather than mis-drawn.
    if (next.type !== "workspace-snapshot") return;
    if (next.sidebar_version !== 1) {
      console.error(`sidebar: unsupported sidebar_version ${next.sidebar_version}`);
      return;
    }
    snap = next;
    const result = next.last_action_result;
    if (result !== null && result.ok === false && result.id !== lastToastedId) {
      lastToastedId = result.id;
      dismissToast();
      toast = result.error ?? "sidebar action failed";
      toastTimer = setTimeout(() => {
        toastTimer = null;
        toast = null;
        render();
      }, TOAST_MS);
    }
    const refs = navigableRefs(next);
    const synced = syncCursor({ cursor, refs, prevCurrent, current: next.current_ws });
    prevCurrent = next.current_ws;
    const moved = synced !== cursor;
    cursor = synced;
    render();
    if (moved) scrollCursorIntoView();
    armClocks();
  };

  const onKeydown = (e: KeyboardEvent): void => {
    if (snap === null) return;
    // Interrupt is the literal drawer chord `C-c C-k`. `C-c` over a real
    // highlight belongs to the copy handler (copy.ts) and falls through
    // untouched; over an empty selection it arms the prefix.
    if (e.ctrlKey && !e.metaKey && !e.altKey && e.key.toLowerCase() === "c") {
      const selection = window.getSelection()?.toString() ?? "";
      if (selection !== "") return;
      interruptArmedUntil = Date.now() + INTERRUPT_PREFIX_MS;
      e.preventDefault();
      return;
    }
    if (e.ctrlKey && !e.metaKey && !e.altKey && e.key.toLowerCase() === "k") {
      if (Date.now() <= interruptArmedUntil) {
        interruptArmedUntil = 0;
        const targets = actionTargets(snap.marks, cursor);
        if (targets.length > 0) post("interrupt", targets);
        e.preventDefault();
      }
      return;
    }
    if (e.ctrlKey || e.metaKey || e.altKey) return;

    const refs = navigableRefs(snap);
    const ws = wsOf(cursor);
    switch (e.key) {
      case "j":
      case "ArrowDown":
        moveCursorTo(stepCursor(refs, cursor, +1));
        break;
      case "k":
      case "ArrowUp":
        moveCursorTo(stepCursor(refs, cursor, -1));
        break;
      case "Enter":
        if (ws !== null) post("visit", [ws]);
        break;
      case "Tab": {
        const repo = repoOf(cursor);
        if (repo !== null) post("toggle-fold", [repo]);
        else if (ws !== null) post("toggle-expand", [ws]);
        break;
      }
      case "g":
        post("refresh", []);
        break;
      case "q":
        post("hide-sidebar", []);
        break;
      case "x": {
        const targets = actionTargets(snap.marks, cursor);
        if (targets.length === 0) break;
        const merged = mergedTargets(snap, targets);
        let confirmed = false;
        if (merged.length > 0) {
          // The drawer's y-or-n-p, browser-side: finishing a MERGED
          // workspace is destructive and aborts entirely on cancel.
          const ok = window.confirm(
            `Finish merged workspace ${merged.join(", ")}? ` +
              `This removes the worktree directory and the hash entry.`,
          );
          if (!ok) break;
          confirmed = true;
        }
        post("nuke", targets, {}, confirmed);
        break;
      }
      case "d": {
        const targets = actionTargets(snap.marks, cursor);
        if (targets.length > 0) post("kill", targets);
        break;
      }
      case "i": {
        const targets = actionTargets(snap.marks, cursor);
        if (targets.length === 0) break;
        const text = window.prompt(`Send to ${targets.join(", ")}:`);
        if (text === null || text.trim() === "") break;
        post("send-prompt", targets, { prompt: text });
        break;
      }
      case "M": {
        const targets = actionTargets(snap.marks, cursor);
        if (targets.length > 0) post("merge-into-source", targets);
        break;
      }
      case "m":
        if (ws !== null) post("merge-child", [ws]);
        break;
      case "n":
        if (ws !== null) post("new-child", [ws]);
        break;
      case "f":
        if (ws !== null) post("new-fork", [ws]);
        break;
      case "H":
        if (ws !== null) post("toggle-hidden", [ws]);
        break;
      case "+":
        if (ws !== null) post("priority-up", [ws]);
        break;
      case "-":
        if (ws !== null) post("priority-down", [ws]);
        break;
      case "t":
        if (ws !== null) {
          post("toggle-mark", [ws]);
          moveCursorTo(stepCursor(refs, cursor, +1));
        }
        break;
      case "u":
        post("clear-marks", []);
        break;
      default:
        return; // unhandled: leave the event alone
    }
    e.preventDefault();
  };

  // The drawer's click-in-then-type model: the aside is keyboard-
  // inaccessible (tabindex -1, no tab stop) until a mousedown focuses it.
  el.addEventListener("mousedown", () => {
    el.focus();
  });
  el.addEventListener("click", (e) => {
    const target = e.target as HTMLElement;
    if (target.closest("[data-toast-close]")) {
      dismissToast();
      render();
      return;
    }
    const ref = target.closest("[data-entry]")?.getAttribute("data-entry");
    if (ref) moveCursorTo(ref);
  });
  el.addEventListener("keydown", onKeydown);

  // The snapshot stream: no seq, no replay, no session probe — every
  // snapshot supersedes the last, and WsClient's backoff handles daemon
  // bounces.
  const client = new WsClient({
    url: `${opts.wsBase}/workspaces/stream`,
    onMessage: (data) => {
      applySnapshot(data);
      return undefined;
    },
    wsFactory: opts.wsFactory,
  });
  client.connect();
}
