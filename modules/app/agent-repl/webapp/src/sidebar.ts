/**
 * Workspace sidebar — the Emacs drawer ported into the GUI.
 *
 * Emacs owns every fact and ships a pre-computed view-model (see
 * shared/protocol.md, "Workspace sidebar stream"); this module renders it
 * verbatim and never re-derives ordering, bucketing, or tree structure.
 * The only page-local state is the cursor (which the drawer already snaps
 * to the active workspace on every switch, so page-local is behaviorally
 * identical), the search query, a dismissable toast, and the clock tick.
 * The `GET /sessions` roster is a decoration joined onto rows by the
 * snapshot's per-entry `session_id`, never a second source of structure.
 *
 * Design mirrors the rest of the webapp: pure string-building render
 * functions and pure targeting/cursor/op helpers (node-testable, no DOM),
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
  RosterEntry,
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
/** How often the `GET /sessions` roster is re-joined while visible. */
const ROSTER_POLL_MS = 10_000;
/** Hover dwell before the row's hover card appears. */
const HOVER_DELAY_MS = 350;

/**
 * Name of the global the Emacs host calls to drive the sidebar (the
 * `C-S-*` chords, dispatched when no drawer buffer is open). The lisp
 * end of the contract MUST use exactly this string — see host.ts for
 * why the surface is one named global rather than DOM queries.
 */
export const SIDEBAR_HOOK = "agentReplSidebar";

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
 * matching the drawer (commit rows are invisible to j/k). Callers apply
 * `filterSnapshot` first, so the cursor's list respects the filter.
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
  for (const entry of allEntries(snap)) {
    if (entry.section === "merged") merged.add(entry.ws);
  }
  return targets.filter((t) => merged.has(t));
}

/** Every workspace row of SNAP, in render order. */
function* allEntries(snap: Snapshot): Generator<Entry> {
  for (const section of snap.sections) {
    for (const group of section.groups) {
      for (const entry of group.entries) yield entry;
    }
  }
}

/** The entry named WS, or null when the snapshot has no such row. */
export function findEntry(snap: Snapshot, ws: string): Entry | null {
  for (const entry of allEntries(snap)) {
    if (entry.ws === ws) return entry;
  }
  return null;
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
  for (const entry of allEntries(snap)) {
    const d = entry.detail;
    if (d && (typeof d.merge_completed_at === "number" || typeof d.last_prompt_at === "number")) {
      return true;
    }
  }
  return false;
}

/* ------------------------------------------------------------------ *
 * Width / standalone / search (pure)                                  *
 * ------------------------------------------------------------------ */

/**
 * The aside's flex-basis for SNAP: the Emacs-owned `width_px` when the
 * snapshot carries one, else "" so the stylesheet default
 * (`var(--sidebar-width, 340px)`) stays in force.
 */
export function sidebarWidth(snap: Snapshot): string {
  return typeof snap.width_px === "number" && snap.width_px > 0 ? `${snap.width_px}px` : "";
}

/**
 * Whether this page is a standalone browser view of the sidebar: no
 * `parent_ws` param means no Emacs host is driving the webview, so
 * Emacs-only actions have nothing to land on.
 */
export function isStandalone(params: URLSearchParams): boolean {
  return !params.has("parent_ws");
}

/** Toast shown when a standalone view attempts an Emacs-only action. */
export const STANDALONE_TOAST = "Emacs-only action (standalone view)";

/**
 * The action verbs that only make sense with a live Emacs host on the
 * other end (persp switches, worktree surgery, magit, drawer state).
 * Everything else — navigation, expand/fold, refresh, marks,
 * send-prompt, interrupt, the roster's permission buttons — still works
 * from a standalone browser tab.
 */
export const EMACS_ONLY_ACTIONS: ReadonlySet<SidebarActionName> = new Set<SidebarActionName>([
  "visit",
  "nuke",
  "kill",
  "merge-into-source",
  "merge-child",
  "new-child",
  "new-fork",
  "toggle-hidden",
  "priority-up",
  "priority-down",
  "set-priority",
  "show-commit",
  "hide-sidebar",
]);

/** How an action attempt is delivered to the daemon (or refused). */
export type PostFn = (
  action: SidebarActionName,
  targets: string[],
  args?: Record<string, unknown>,
  confirmed?: boolean,
) => void;

/**
 * Wrap SEND with the standalone gate: in a standalone view an
 * Emacs-only action never reaches the wire — ONBLOCKED gets the toast
 * text instead. Every action path (keys, host hook, clicks, drag) posts
 * through one gated function, so the gate cannot be bypassed.
 */
export function makeGatedPost(
  standalone: boolean,
  send: PostFn,
  onBlocked: (toast: string) => void,
): PostFn {
  return (action, targets, args = {}, confirmed = false) => {
    if (standalone && EMACS_ONLY_ACTIONS.has(action)) {
      onBlocked(STANDALONE_TOAST);
      return;
    }
    send(action, targets, args, confirmed);
  };
}

/** Case-insensitive substring match over the row's name and summary. */
export function entryMatches(entry: Entry, query: string): boolean {
  const q = query.toLowerCase();
  return entry.ws.toLowerCase().includes(q) || entry.summary.toLowerCase().includes(q);
}

/**
 * The snapshot as the search filter sees it: workspace rows that match
 * QUERY (name or summary, case-insensitive substring), group headers
 * with zero surviving rows dropped. Section headers — and their counts,
 * which are the SNAPSHOT's totals, not the filtered view's — survive. A
 * blank query is the identity, returning SNAP itself. Folded groups
 * carry no rows to match and so hide under any non-blank query.
 */
export function filterSnapshot(snap: Snapshot, query: string): Snapshot {
  const q = query.trim();
  if (q === "") return snap;
  const sections = snap.sections.map((section) => ({
    ...section,
    groups: section.groups
      .map((group) => ({ ...group, entries: group.entries.filter((e) => entryMatches(e, q)) }))
      .filter((group) => group.entries.length > 0),
  }));
  return { ...snap, sections };
}

/* ------------------------------------------------------------------ *
 * Roster join (pure)                                                  *
 * ------------------------------------------------------------------ */

/** Index a `GET /sessions` listing by session id for the row join. */
export function indexRoster(sessions: RosterEntry[]): Record<string, RosterEntry> {
  const index: Record<string, RosterEntry> = {};
  for (const s of sessions) index[s.session_id] = s;
  return index;
}

/**
 * The row decorations a roster entry contributes: a `⧉ N` queue-depth
 * badge, one-click ✓/✗ buttons for the FIRST pending permission (the
 * shim gates ids itself, so first-pending is always the actionable one),
 * and a two-decimal cost chip. Empty string when nothing applies.
 */
export function rosterBadgesHtml(r: RosterEntry | null | undefined): string {
  if (!r) return "";
  const parts: string[] = [];
  if (r.queue.length > 0) {
    parts.push(
      `<span class="sidebar-queue-badge" title="${r.queue.length} queued message(s)">⧉ ${r.queue.length}</span>`,
    );
  }
  if (r.pending_permissions.length > 0) {
    const sid = escapeHtml(r.session_id);
    const rid = escapeHtml(r.pending_permissions[0]);
    parts.push(
      `<button type="button" class="sidebar-perm-allow" data-perm="allow"` +
        ` data-perm-sid="${sid}" data-perm-rid="${rid}" title="allow the pending permission">✓</button>` +
        `<button type="button" class="sidebar-perm-deny" data-perm="deny"` +
        ` data-perm-sid="${sid}" data-perm-rid="${rid}" title="deny the pending permission">✗</button>`,
    );
  }
  if (r.total_cost_usd > 0) {
    parts.push(
      `<span class="sidebar-cost-chip" title="cumulative session cost">$${r.total_cost_usd.toFixed(2)}</span>`,
    );
  }
  return parts.join("");
}

/* ------------------------------------------------------------------ *
 * Desktop notifications (pure detection)                              *
 * ------------------------------------------------------------------ */

export interface SidebarNotification {
  ws: string;
  kind: "permission" | "merged";
  title: string;
  body: string;
}

/** The set of `<kind>:<ws>` conditions in force for SNAP. */
function conditionSet(snap: Snapshot): Set<string> {
  const set = new Set<string>();
  for (const entry of allEntries(snap)) {
    if (entry.status === "permission") set.add(`permission:${entry.ws}`);
    if (entry.section === "merged") set.add(`merged:${entry.ws}`);
  }
  return set;
}

/**
 * The notifications a PREV→NEXT snapshot transition warrants: one per
 * workspace ENTERING the `permission` status and one per workspace
 * ENTERING the merged section. Edge-triggered on purpose — a condition
 * that persists across snapshots fires exactly once, and only clearing
 * and re-entering it can fire again.
 */
export function notificationEvents(prev: Snapshot, next: Snapshot): SidebarNotification[] {
  const before = conditionSet(prev);
  const out: SidebarNotification[] = [];
  for (const entry of allEntries(next)) {
    if (entry.status === "permission" && !before.has(`permission:${entry.ws}`)) {
      out.push({
        ws: entry.ws,
        kind: "permission",
        title: "Permission requested",
        body: `${entry.ws} — ${entry.summary}`,
      });
    }
    if (entry.section === "merged" && !before.has(`merged:${entry.ws}`)) {
      out.push({
        ws: entry.ws,
        kind: "merged",
        title: "Workspace merged",
        body: `${entry.ws} — ${entry.summary}`,
      });
    }
  }
  return out;
}

/* ------------------------------------------------------------------ *
 * Drag-to-reprioritize + merge-commit click (pure payloads)           *
 * ------------------------------------------------------------------ */

/**
 * The `set-priority` request dropping SOURCE onto TARGET implies:
 * source adopts the target row's priority (string or null). Null when
 * the drop is meaningless — self-drop, an unknown row, or either side
 * outside the MAIN section (only main rows are draggable/droppable).
 * No optimistic reorder happens: the next snapshot is the reorder.
 */
export function dragPayload(
  snap: Snapshot,
  source: string,
  target: string,
): { action: "set-priority"; targets: [string]; args: { priority: string | null } } | null {
  if (source === target) return null;
  const src = findEntry(snap, source);
  const dst = findEntry(snap, target);
  if (src === null || dst === null) return null;
  if (src.section !== "main" || dst.section !== "main") return null;
  return { action: "set-priority", targets: [source], args: { priority: dst.priority } };
}

/** The `show-commit` request a merge-queue commit click sends. */
export function showCommitRequest(
  row: MergeCommitRow,
): { action: "show-commit"; targets: [string]; args: { sha: string } } {
  return { action: "show-commit", targets: [row.ws], args: { sha: row.sha } };
}

/* ------------------------------------------------------------------ *
 * Shared op dispatch (keyboard chords + host hook)                    *
 * ------------------------------------------------------------------ */

/**
 * Everything an op needs to run. The DOM shell builds one per dispatch
 * from live state; tests build them from stubs. `confirmFn`/`promptFn`
 * are `window.confirm`/`window.prompt` under the shell — injected so
 * the destructive-nuke and send-prompt flows are node-testable.
 */
export interface OpContext {
  /** The full (unfiltered) snapshot; ops no-op while none has arrived. */
  snap: Snapshot | null;
  /** The filter-respecting navigable list, exactly as rendered. */
  refs: string[];
  cursor: string | null;
  /** The standalone-gated post — see makeGatedPost. */
  post: PostFn;
  moveCursor: (ref: string | null) => void;
  confirmFn: (message: string) => boolean;
  promptFn: (message: string) => string | null;
  /**
   * Whether ACTION would be refused by the standalone gate. Consulted
   * before user-facing dialogs only (confirming a nuke that can never
   * post would be a lie); the gate itself still lives in `post`.
   */
  blocked: (action: SidebarActionName) => boolean;
}

/**
 * Run one sidebar op against CTX. This is the SINGLE code path behind
 * both the keyboard chords and the Emacs host hook
 * (`window.agentReplSidebar`), so the two can never drift: `visit` here
 * IS the Enter key, `nuke` IS `x` (including the merged-workspace
 * confirm), `send-prompt` IS `i` (including the prompt dialog).
 * Unknown ops are ignored (forward compatibility).
 */
export function runSidebarOp(op: string, ctx: OpContext): void {
  const snap = ctx.snap;
  if (snap === null) return;
  const ws = wsOf(ctx.cursor);
  switch (op) {
    case "visit":
      if (ws !== null) ctx.post("visit", [ws]);
      break;
    case "nuke": {
      const targets = actionTargets(snap.marks, ctx.cursor);
      if (targets.length === 0) break;
      const merged = mergedTargets(snap, targets);
      let confirmed = false;
      // The gate would refuse regardless of the answer, so a standalone
      // view skips straight to the refusal toast instead of asking.
      if (merged.length > 0 && !ctx.blocked("nuke")) {
        // The drawer's y-or-n-p, browser-side: finishing a MERGED
        // workspace is destructive and aborts entirely on cancel.
        const ok = ctx.confirmFn(
          `Finish merged workspace ${merged.join(", ")}? ` +
            `This removes the worktree directory and the hash entry.`,
        );
        if (!ok) break;
        confirmed = true;
      }
      ctx.post("nuke", targets, {}, confirmed);
      break;
    }
    case "kill": {
      const targets = actionTargets(snap.marks, ctx.cursor);
      if (targets.length > 0) ctx.post("kill", targets);
      break;
    }
    case "send-prompt": {
      const targets = actionTargets(snap.marks, ctx.cursor);
      if (targets.length === 0) break;
      const text = ctx.promptFn(`Send to ${targets.join(", ")}:`);
      if (text === null || text.trim() === "") break;
      ctx.post("send-prompt", targets, { prompt: text });
      break;
    }
    case "merge-into-source": {
      const targets = actionTargets(snap.marks, ctx.cursor);
      if (targets.length > 0) ctx.post("merge-into-source", targets);
      break;
    }
    case "toggle-hidden":
      if (ws !== null) ctx.post("toggle-hidden", [ws]);
      break;
    case "toggle-mark":
      if (ws !== null) {
        ctx.post("toggle-mark", [ws]);
        // The drawer advances past a just-marked row so marking a run of
        // workspaces is one keypress per row.
        ctx.moveCursor(stepCursor(ctx.refs, ctx.cursor, +1));
      }
      break;
    case "clear-marks":
      ctx.post("clear-marks", []);
      break;
    case "priority-up":
      if (ws !== null) ctx.post("priority-up", [ws]);
      break;
    case "priority-down":
      if (ws !== null) ctx.post("priority-down", [ws]);
      break;
    default:
      break; // unknown op: ignored, like unknown frame types
  }
}

/**
 * Plant the sidebar hook on TARGET. RUN receives the op name; the shell
 * passes `(op) => runSidebarOp(op, <live context>)`. Mirrors
 * installHostTailHook / installChessNavHook (host.ts, chess-game.ts).
 */
export function installSidebarHook(
  target: Record<string, unknown>,
  run: (op: string) => void,
): void {
  target[SIDEBAR_HOOK] = (op: string): void => {
    run(op);
  };
}

/* ------------------------------------------------------------------ *
 * HTML (pure string building — node-testable)                         *
 * ------------------------------------------------------------------ */

/** Section rule width, matching the drawer's 12-`─` rule line. */
const SECTION_RULE = "─".repeat(12);

/** Roster/image decorations threaded through the render tree. */
interface RenderCtx {
  cursor: string | null;
  now: number;
  /** Snapshot `priority_images`: data URIs keyed by priority name. */
  images: Record<string, string>;
  /** `GET /sessions` roster keyed by session id. */
  roster: Record<string, RosterEntry>;
}

/** Optional decorations for snapshotHtml; {} renders bare rows. */
export interface SnapshotView {
  /** The `GET /sessions` roster join, keyed by session id. */
  roster?: Record<string, RosterEntry>;
}

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

/**
 * The priority badge: the image form when `priority_images` carries this
 * priority's name (src attribute-escaped verbatim — the data URI is
 * Emacs-owned and never second-guessed), the text chip otherwise.
 */
function priorityHtml(priority: string | null, images: Record<string, string>): string {
  if (priority === null) return "";
  const img = images[priority];
  if (typeof img === "string" && img !== "") {
    return (
      `<img class="sidebar-priority-img" src="${escapeHtml(img)}"` +
      ` alt="${escapeHtml(priority)}"> `
    );
  }
  return `<span class="sidebar-priority">${escapeHtml(priority)}</span> `;
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

function entryHtml(entry: Entry, ctx: RenderCtx): string {
  const ref = `ws:${entry.ws}`;
  const atCursor = ctx.cursor === ref;
  const indentRem = entry.depth * 0.8;
  const classes = ["sidebar-row", "sidebar-ws"];
  if (entry.hidden) classes.push("sidebar-dim");
  if (atCursor) classes.push("sidebar-at-cursor");
  const priority = priorityHtml(entry.priority, ctx.images);
  const nameStyle = entry.name_color ? ` style="color:${escapeHtml(entry.name_color)}"` : "";
  const dirty = entry.dirty ? ` <span class="sidebar-dirty">●</span>` : "";
  const roster = entry.session_id ? ctx.roster[entry.session_id] : undefined;
  const badgesHtml = rosterBadgesHtml(roster);
  const badges = badgesHtml === "" ? "" : ` ${badgesHtml}`;
  const header = lineHtml(
    gutterHtml(entry.marked, atCursor),
    indentRem,
    `<span class="sidebar-glyph">${escapeHtml(entry.glyph)}</span>  ${priority}` +
      `<span class="sidebar-name"${nameStyle}>${escapeHtml(entry.ws)}</span>${dirty}${badges}`,
    "sidebar-ws-header",
  );
  const summary = lineHtml(
    "",
    indentRem + 0.8,
    escapeHtml(entry.summary),
    "sidebar-ws-summary",
  );
  const detail = entry.detail ? detailLinesHtml(entry.detail, indentRem, ctx.now) : "";
  // Only MAIN rows drag: reprioritizing is a main-section concept, and
  // making merged/hidden rows draggable would imply an op that no-ops.
  const drag = entry.section === "main" ? ` draggable="true"` : "";
  return (
    `<div class="${classes.join(" ")}" data-entry="${escapeHtml(ref)}"${drag}` +
    ` title="${escapeHtml(entry.help)}">${header}${summary}${detail}</div>`
  );
}

function groupHtml(group: Group, ctx: RenderCtx): string {
  const ref = `repo:${group.key}`;
  const atCursor = ctx.cursor === ref;
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
  return header + group.entries.map((entry) => entryHtml(entry, ctx)).join("");
}

function sectionHeaderHtml(label: string): string {
  return (
    `<div class="sidebar-section-title">${escapeHtml(label)}</div>` +
    `<div class="sidebar-section-rule">${SECTION_RULE}</div>`
  );
}

function sectionHtml(section: Section, ctx: RenderCtx): string {
  const header = sectionHeaderHtml(`${section.label} (${section.count})`);
  const body =
    section.groups.length === 0
      ? `<div class="sidebar-empty">(none)</div>`
      : section.groups.map((group) => groupHtml(group, ctx)).join("");
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
  // data-mq-commit makes the row clickable: the shell resolves the sha
  // back to this row and posts `show-commit` (magit in the workspace).
  let html =
    `<div class="sidebar-line sidebar-mq-commit" data-mq-commit` +
    ` data-sha="${escapeHtml(row.sha)}"><span class="sidebar-gutter"></span>` +
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
 * The sidebar's persistent header row: the standalone chip (only in a
 * standalone browser view) over the search input. Built ONCE by the
 * shell, outside the per-snapshot innerHTML rebuild, so the input keeps
 * its value, focus, and caret across snapshots.
 */
export function sidebarHeaderHtml(standalone: boolean): string {
  const chip = standalone
    ? `<div class="sidebar-chip-row"><span class="sidebar-standalone-chip">standalone</span></div>`
    : "";
  return (
    chip +
    `<div class="sidebar-search-row"><input type="search" class="sidebar-search"` +
    ` placeholder="filter workspaces" aria-label="filter workspaces"></div>`
  );
}

/**
 * The row-hover card: identity (name, status, priority, summary), the
 * short session id, the roster join's live figures (queue depth, cost,
 * turn preview — when present), and whatever cached detail fields the
 * snapshot carries. Pure; the shell positions and shows it after the
 * hover dwell.
 */
export function hoverCardHtml(entry: Entry, roster: RosterEntry | null = null): string {
  const rows: string[] = [];
  const row = (label: string, value: string): void => {
    rows.push(
      `<div class="sidebar-hover-row"><span class="sidebar-hover-label">${escapeHtml(label)}</span> ` +
        `<span class="sidebar-hover-value">${escapeHtml(value)}</span></div>`,
    );
  };
  rows.push(`<div class="sidebar-hover-name">${escapeHtml(entry.ws)}</div>`);
  if (entry.status !== null) row("status:", entry.status);
  if (entry.priority !== null) row("priority:", entry.priority);
  row("summary:", entry.summary);
  if (entry.session_id) row("session:", entry.session_id.slice(0, 10));
  if (roster) {
    if (roster.queue.length > 0) row("queued:", String(roster.queue.length));
    if (roster.total_cost_usd > 0) row("cost:", `$${roster.total_cost_usd.toFixed(2)}`);
    if (roster.turn_preview !== "") row("turn:", roster.turn_preview);
  }
  const d = entry.detail;
  if (d) {
    if (d.merge_status) row("merge:", d.merge_status);
    if (d.branch) row("branch:", d.branch);
    if (d.merged_into) row("merged into:", d.merged_into);
    if (typeof d.ahead_master === "number") row(`ahead ${d.trunk ?? "master"}:`, String(d.ahead_master));
    if (d.last_commit) row("last commit:", d.last_commit);
    if (typeof d.dirty_count === "number" && d.dirty_count > 0) row("dirty:", `${d.dirty_count} files`);
    if (typeof d.pending_prompts === "number" && d.pending_prompts > 0) {
      row("pending:", `${d.pending_prompts} prompt(s)`);
    }
  }
  return rows.join("");
}

/**
 * The whole sidebar list as one HTML string for SNAP at NOW (epoch
 * seconds), with the page-local CURSOR ref, an optional TOAST error
 * strip, and the VIEW decorations (roster join). Rebuilt in full per
 * snapshot — the list is small and one snapshot always supersedes the
 * previous one. Callers apply `filterSnapshot` first when a search
 * query is active.
 */
export function snapshotHtml(
  snap: Snapshot,
  cursor: string | null,
  now: number,
  toast: string | null = null,
  view: SnapshotView = {},
): string {
  const ctx: RenderCtx = {
    cursor,
    now,
    images: snap.priority_images ?? {},
    roster: view.roster ?? {},
  };
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
  for (const section of snap.sections) parts.push(sectionHtml(section, ctx));
  return parts.join("");
}

/* ------------------------------------------------------------------ *
 * DOM / WS shell                                                      *
 * ------------------------------------------------------------------ */

export interface SidebarOptions {
  el: HTMLElement;
  httpBase: string;
  wsBase: string;
  /**
   * Standalone browser view (no Emacs host): Emacs-only actions toast
   * instead of posting. main.ts computes this via isStandalone().
   */
  standalone?: boolean;
  /** WebSocket constructor (injectable for tests). */
  wsFactory?: (url: string) => WebSocket;
}

export function initSidebar(opts: SidebarOptions): void {
  const { el, httpBase } = opts;
  const standalone = opts.standalone ?? false;
  let snap: Snapshot | null = null;
  let cursor: string | null = null;
  let prevCurrent: string | null = null;
  let query = "";
  let roster: Record<string, RosterEntry> = {};
  let toast: string | null = null;
  let lastToastedId: string | null = null;
  let toastTimer: ReturnType<typeof setTimeout> | null = null;
  let tickTimer: ReturnType<typeof setInterval> | null = null;
  let staleTimer: ReturnType<typeof setTimeout> | null = null;
  let interruptArmedUntil = 0;
  let hoverTimer: ReturnType<typeof setTimeout> | null = null;
  let hoverRef: string | null = null;
  let hoverCard: HTMLElement | null = null;
  let notifyAsked = false;

  const nowSeconds = (): number => Date.now() / 1000;

  // The persistent header (standalone chip + search input) lives OUTSIDE
  // the per-snapshot innerHTML rebuild, so typing in the input survives
  // every snapshot; only listEl is rewritten per render.
  if (standalone) el.classList.add("sidebar-standalone");
  el.innerHTML = "";
  const headerEl = document.createElement("div");
  headerEl.className = "sidebar-header";
  headerEl.innerHTML = sidebarHeaderHtml(standalone);
  const listEl = document.createElement("div");
  listEl.className = "sidebar-list";
  el.append(headerEl, listEl);
  const searchEl = headerEl.querySelector<HTMLInputElement>("input.sidebar-search");
  if (searchEl === null) throw new Error("sidebar header lost its search input");

  /** SNAP through the search filter — what is actually on screen. */
  const visibleSnap = (s: Snapshot): Snapshot => filterSnapshot(s, query);

  const render = (): void => {
    if (snap === null) return;
    // The aside tracks the Emacs-owned visibility flag: SPC o d and the
    // sidebar's own `q` both flip it, and every webview follows.
    el.hidden = snap.sidebar_visible === false;
    // Emacs owns the width; absent → the stylesheet default.
    el.style.flexBasis = sidebarWidth(snap);
    listEl.innerHTML = snapshotHtml(visibleSnap(snap), cursor, nowSeconds(), toast, { roster });
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

  const dismissToast = (): void => {
    toast = null;
    if (toastTimer !== null) {
      clearTimeout(toastTimer);
      toastTimer = null;
    }
  };

  /** Raise MESSAGE in the toast strip, auto-dismissing after TOAST_MS. */
  const showToast = (message: string): void => {
    dismissToast();
    toast = message;
    toastTimer = setTimeout(() => {
      toastTimer = null;
      toast = null;
      render();
    }, TOAST_MS);
    render();
  };

  /* ---- roster join ---- */

  const refreshRoster = (): void => {
    void fetch(`${httpBase}/sessions`)
      .then((resp) => {
        if (!resp.ok) throw new Error(`GET /sessions: ${resp.status}`);
        return resp.json();
      })
      .then((body) => {
        roster = indexRoster((body as { sessions?: RosterEntry[] }).sessions ?? []);
        render();
      })
      .catch((err: unknown) => {
        console.error("sidebar roster poll failed", err);
      });
  };
  refreshRoster();
  setInterval(() => {
    // Only while the sidebar is actually on screen: a hidden aside has
    // no rows to decorate, and the join is pure decoration.
    if (snap !== null && snap.sidebar_visible !== false) refreshRoster();
  }, ROSTER_POLL_MS);

  /* ---- action channel ---- */

  // Fire-and-forget: Emacs executes the action and reports refusals
  // through the snapshot's last_action_result, which toasts below. The
  // roster is re-joined after every POST so queue/permission badges
  // track the action's effect without waiting out the poll interval.
  const sendAction: PostFn = (action, targets, args = {}, confirmed = false): void => {
    const body: ActionRequest = { action, targets, args, confirmed };
    void fetch(`${httpBase}/workspaces/action`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body),
    })
      .then((resp) => {
        if (!resp.ok) console.error(`sidebar action ${action}: HTTP ${resp.status}`);
        refreshRoster();
      })
      .catch((err: unknown) => {
        console.error(`sidebar action ${action} failed`, err);
      });
  };
  // EVERY action path posts through the standalone gate.
  const post = makeGatedPost(standalone, sendAction, showToast);

  /* ---- desktop notifications ---- */

  // Permission is requested lazily on the FIRST user interaction with
  // the sidebar (capture phase, so an input's stopPropagation cannot
  // starve it) — never on load, per notification etiquette.
  const lazyNotifyPermission = (): void => {
    if (notifyAsked) return;
    notifyAsked = true;
    if (typeof Notification !== "undefined" && Notification.permission === "default") {
      void Notification.requestPermission();
    }
  };
  el.addEventListener("mousedown", lazyNotifyPermission, true);
  el.addEventListener("keydown", lazyNotifyPermission, true);

  const fireNotifications = (events: readonly SidebarNotification[]): void => {
    // No Notification API (xwidget webviews) or no grant: skip entirely.
    if (typeof Notification === "undefined" || Notification.permission !== "granted") return;
    for (const ev of events) new Notification(ev.title, { body: ev.body });
  };

  /* ---- snapshot intake ---- */

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
    const prev = snap;
    snap = next;
    // Edge-triggered desktop notifications; never on the first snapshot
    // (a page load must not replay every standing condition as news).
    if (prev !== null) fireNotifications(notificationEvents(prev, next));
    const result = next.last_action_result;
    if (result !== null && result.ok === false && result.id !== lastToastedId) {
      lastToastedId = result.id;
      showToast(result.error ?? "sidebar action failed");
    }
    const refs = navigableRefs(visibleSnap(next));
    const synced = syncCursor({ cursor, refs, prevCurrent, current: next.current_ws });
    prevCurrent = next.current_ws;
    const moved = synced !== cursor;
    cursor = synced;
    render();
    if (moved) scrollCursorIntoView();
    armClocks();
  };

  /* ---- shared op dispatch (keys + host hook) ---- */

  const opContext = (): OpContext => ({
    snap,
    refs: snap === null ? [] : navigableRefs(visibleSnap(snap)),
    cursor,
    post,
    moveCursor: moveCursorTo,
    confirmFn: (message) => window.confirm(message),
    promptFn: (message) => window.prompt(message),
    blocked: (action) => standalone && EMACS_ONLY_ACTIONS.has(action),
  });
  const runOp = (op: string): void => {
    runSidebarOp(op, opContext());
  };
  // The Emacs host's C-S-* chords land here (frontend.el evaluates
  // `window.agentReplSidebar("<op>")` against the live document).
  installSidebarHook(window as unknown as Record<string, unknown>, runOp);

  const onKeydown = (e: KeyboardEvent): void => {
    if (snap === null) return;
    // Keys typed into the search input belong to the input, never to the
    // sidebar keymap (its own Escape handler also stops propagation).
    if (e.target === searchEl) return;
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

    const refs = navigableRefs(visibleSnap(snap));
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
        runOp("visit");
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
      case "x":
        runOp("nuke");
        break;
      case "d":
        runOp("kill");
        break;
      case "i":
        runOp("send-prompt");
        break;
      case "M":
        runOp("merge-into-source");
        break;
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
        runOp("toggle-hidden");
        break;
      case "+":
        runOp("priority-up");
        break;
      case "-":
        runOp("priority-down");
        break;
      case "t":
        runOp("toggle-mark");
        break;
      case "u":
        runOp("clear-marks");
        break;
      default:
        return; // unhandled: leave the event alone
    }
    e.preventDefault();
  };

  /* ---- search input ---- */

  const onQueryChange = (value: string): void => {
    query = value;
    if (snap !== null) {
      // A cursor filtered off screen would strand the keymap; it falls
      // to the first surviving row (or nothing) instead.
      const refs = navigableRefs(visibleSnap(snap));
      if (cursor !== null && !refs.includes(cursor)) {
        cursor = refs.length > 0 ? refs[0] : null;
      }
    }
    render();
  };
  searchEl.addEventListener("input", () => {
    onQueryChange(searchEl.value);
  });
  searchEl.addEventListener("keydown", (e) => {
    // The input owns its keys — nothing typed here reaches the keymap.
    e.stopPropagation();
    if (e.key === "Escape") {
      searchEl.value = "";
      if (query !== "") onQueryChange("");
      el.focus();
      e.preventDefault();
    }
  });

  /* ---- hover cards ---- */

  const hideHoverCard = (): void => {
    if (hoverTimer !== null) {
      clearTimeout(hoverTimer);
      hoverTimer = null;
    }
    if (hoverCard !== null) hoverCard.hidden = true;
  };

  const showHoverCard = (ws: string, rowEl: HTMLElement): void => {
    if (snap === null) return;
    const entry = findEntry(snap, ws);
    if (entry === null) return;
    if (hoverCard === null) {
      hoverCard = document.createElement("div");
      hoverCard.className = "sidebar-hovercard";
      document.body.appendChild(hoverCard);
    }
    const joined = entry.session_id ? roster[entry.session_id] ?? null : null;
    hoverCard.innerHTML = hoverCardHtml(entry, joined);
    const rect = rowEl.getBoundingClientRect();
    hoverCard.style.left = `${Math.round(rect.right + 8)}px`;
    hoverCard.style.top = `${Math.round(Math.max(0, Math.min(rect.top, window.innerHeight - 280)))}px`;
    hoverCard.hidden = false;
  };

  el.addEventListener("mouseover", (e) => {
    const rowEl = (e.target as HTMLElement).closest<HTMLElement>('[data-entry^="ws:"]');
    const ref = rowEl?.getAttribute("data-entry") ?? null;
    if (ref === hoverRef) return;
    hoverRef = ref;
    hideHoverCard();
    const ws = wsOf(ref);
    if (ws === null || rowEl === null) return;
    hoverTimer = setTimeout(() => {
      hoverTimer = null;
      showHoverCard(ws, rowEl);
    }, HOVER_DELAY_MS);
  });
  el.addEventListener("mouseleave", () => {
    hoverRef = null;
    hideHoverCard();
  });

  /* ---- drag to reprioritize ---- */

  const clearDropTargets = (): void => {
    for (const node of Array.from(el.querySelectorAll(".sidebar-drop-target"))) {
      node.classList.remove("sidebar-drop-target");
    }
  };
  el.addEventListener("dragstart", (e) => {
    const rowEl = (e.target as HTMLElement).closest<HTMLElement>('[draggable="true"]');
    const ws = wsOf(rowEl?.getAttribute("data-entry") ?? null);
    if (ws === null || e.dataTransfer === null) return;
    e.dataTransfer.setData("text/plain", ws);
    e.dataTransfer.effectAllowed = "move";
    hideHoverCard();
  });
  el.addEventListener("dragover", (e) => {
    const rowEl = (e.target as HTMLElement).closest<HTMLElement>('[draggable="true"]');
    if (rowEl === null) return;
    e.preventDefault(); // this row accepts the drop
    if (!rowEl.classList.contains("sidebar-drop-target")) {
      clearDropTargets();
      rowEl.classList.add("sidebar-drop-target");
    }
  });
  el.addEventListener("drop", (e) => {
    const rowEl = (e.target as HTMLElement).closest<HTMLElement>('[draggable="true"]');
    clearDropTargets();
    if (rowEl === null || snap === null) return;
    e.preventDefault();
    const source = e.dataTransfer?.getData("text/plain") ?? "";
    const target = wsOf(rowEl.getAttribute("data-entry"));
    if (source === "" || target === null) return;
    const payload = dragPayload(snap, source, target);
    // No optimistic reorder: the next snapshot IS the reorder.
    if (payload !== null) post(payload.action, payload.targets, payload.args);
  });
  el.addEventListener("dragend", clearDropTargets);

  /* ---- clicks ---- */

  // The drawer's click-in-then-type model: the aside is keyboard-
  // inaccessible (tabindex -1, no tab stop) until a mousedown focuses it.
  // The search input keeps its own focus — stealing it on mousedown
  // would make the filter untypable.
  el.addEventListener("mousedown", (e) => {
    if ((e.target as HTMLElement).closest(".sidebar-search")) return;
    el.focus();
  });
  el.addEventListener("click", (e) => {
    const target = e.target as HTMLElement;
    if (target.closest("[data-toast-close]")) {
      dismissToast();
      render();
      return;
    }
    // The roster's one-click permission buttons ride the dedicated
    // HTTP endpoint (not the action channel), then re-join the roster
    // so the buttons clear as soon as the daemon resolves the request.
    const perm = target.closest("[data-perm]");
    if (perm) {
      const behavior = perm.getAttribute("data-perm") === "deny" ? "deny" : "allow";
      const sid = perm.getAttribute("data-perm-sid") ?? "";
      const rid = perm.getAttribute("data-perm-rid") ?? "";
      void fetch(`${httpBase}/sessions/${sid}/permission`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ request_id: rid, behavior }),
      })
        .then((resp) => {
          if (!resp.ok) console.error(`sidebar permission ${behavior}: HTTP ${resp.status}`);
          refreshRoster();
        })
        .catch((err: unknown) => {
          console.error("sidebar permission decision failed", err);
        });
      return;
    }
    // A merge-queue commit opens in magit inside its workspace.
    const mq = target.closest("[data-mq-commit]");
    if (mq && snap !== null) {
      const sha = mq.getAttribute("data-sha") ?? "";
      const row = (snap.merge_queue?.rows ?? []).find(
        (r): r is MergeCommitRow => r.kind === "commit" && r.sha === sha,
      );
      if (row) {
        const req = showCommitRequest(row);
        post(req.action, req.targets, req.args);
      }
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
