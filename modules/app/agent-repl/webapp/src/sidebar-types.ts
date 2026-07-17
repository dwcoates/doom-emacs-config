/**
 * Workspace-sidebar snapshot schema — the "Workspace sidebar stream
 * (Emacs → daemon → webapp)" section of shared/protocol.md, verbatim.
 *
 * The snapshot is a pre-computed VIEW-MODEL: Emacs owns every fact and
 * ships the drawer's full structure (section order, repo grouping, tree
 * flattening with depth, merge-stream ordering and truncation). The
 * webapp renders it as-is and never re-derives any of it.
 */

/** One workspace row, tree pre-flattened with `depth`. */
export interface Entry {
  ws: string;
  depth: number;
  section: "main" | "hidden" | "merging" | "merged";
  /**
   * The daemon session bound to this workspace, or null. The join key
   * into the `GET /sessions` roster (queue depth, permissions, cost).
   */
  session_id: string | null;
  /** Render-status keyword sans colon (e.g. "thinking"), or null. */
  status: string | null;
  /** Exact drawer state glyph (⏳/⌛/✅/…). */
  glyph: string;
  /** Drawer name-face foreground (hex), or null for the default face. */
  name_color: string | null;
  priority: string | null;
  /** Exactly what the drawer's summary line shows (incl. `…` / `—`). */
  summary: string;
  summary_pending: boolean;
  dirty: boolean;
  hidden: boolean;
  marked: boolean;
  expanded: boolean;
  current: boolean;
  help: string;
  /** Null unless expanded; cached fields only, each optional. */
  detail: Detail | null;
}

/** Expanded-entry detail fields (drawer C11). All best-effort caches. */
export interface Detail {
  merge_status?: string;
  branch?: string;
  merged_into?: string;
  /** Epoch seconds; the browser ticks the "merged: … ago" clock. */
  merge_completed_at?: number;
  trunk?: string;
  ahead_master?: number;
  source_branch?: string;
  ahead_source?: number;
  last_commit?: string;
  last_commit_time?: string;
  dirty_count?: number;
  /** Epoch seconds; the browser ticks the "last prompt: … ago" clock. */
  last_prompt_at?: number;
  pending_prompts?: number;
  merged_in?: string[];
}

/** One repo group inside a section. Folded groups carry `entries: []`. */
export interface Group {
  /** Canonical repo key (git common-dir path); the toggle-fold target. */
  key: string;
  label: string;
  folded: boolean;
  entries: Entry[];
}

/** One drawer section, in drawer order. */
export interface Section {
  id: string;
  label: string;
  count: number;
  groups: Group[];
}

export interface MergeSeparatorRow {
  kind: "separator";
  project: string | null;
}

export interface MergeCommitRow {
  kind: "commit";
  sha: string;
  subject: string;
  state: "current" | "conflict" | "pending" | "halted";
  ws: string;
  /** Epoch seconds; present on the current commit only. */
  started_at?: number;
  /** Conflict rows only: how many files are unmerged. */
  conflict_files?: number;
  resolver_phase?: string;
  resolver_started_at?: number;
}

export type MergeRow = MergeSeparatorRow | MergeCommitRow;

/** The MERGE QUEUE commit stream; omitted entirely when the queue is idle. */
export interface MergeQueue {
  count: number;
  rows: MergeRow[];
}

/** Most recent sidebar action outcome, echoed by Emacs. */
export interface LastActionResult {
  id: string;
  ok: boolean;
  error?: string;
}

/** The full snapshot Emacs pushes and the daemon relays. */
export interface Snapshot {
  type: "workspace-snapshot";
  sidebar_version: number;
  /** Emacs float-time at build (epoch seconds). */
  generated_at: number;
  /** Active workspace name, or null. */
  current_ws: string | null;
  /** The drawer/sidebar shared visibility flag. */
  sidebar_visible: boolean;
  /** Seconds before a merge commit's elapsed clock appears. */
  merge_slow_threshold: number;
  /** Fixed sidebar width in px (Emacs custom); CSS default when absent. */
  width_px?: number;
  /**
   * Optional priority-badge images as data URIs, keyed by priority name.
   * Text chips are the fallback for any priority not carried here.
   */
  priority_images?: Record<string, string>;
  /** Always an array, never null. */
  marks: string[];
  last_action_result: LastActionResult | null;
  merge_queue?: MergeQueue;
  sections: Section[];
}

/** The action verbs `POST /workspaces/action` accepts. */
export type SidebarActionName =
  | "visit"
  | "nuke"
  | "kill"
  | "send-prompt"
  | "interrupt"
  | "merge-into-source"
  | "merge-child"
  | "new-child"
  | "new-fork"
  | "toggle-hidden"
  | "priority-up"
  | "priority-down"
  | "set-priority"
  | "show-commit"
  | "toggle-mark"
  | "clear-marks"
  | "toggle-expand"
  | "toggle-fold"
  | "refresh"
  | "hide-sidebar";

/**
 * One `GET /sessions` listing entry, reduced to the fields the sidebar
 * roster join reads (the endpoint carries more; unknown fields are
 * ignored). Joined onto workspace rows via the snapshot's per-entry
 * `session_id` — see protocol.md "Sidebar roster joins".
 */
export interface RosterEntry {
  session_id: string;
  turn_active: boolean;
  /** Pending permission request ids, oldest first. */
  pending_permissions: string[];
  /** The §2.13 in-flight queue; only its depth is rendered. */
  queue: unknown[];
  /** Tail (≤200 chars) of the active turn's streamed text; "" when idle. */
  turn_preview: string;
  /** Cumulative cost of the session's completed turns. */
  total_cost_usd: number;
  hibernated: boolean;
}

/**
 * The `POST /workspaces/action` body. The daemon mints the action id and
 * timestamp itself; the client sends only the intent.
 */
export interface ActionRequest {
  action: SidebarActionName;
  /** Workspace names (repo keys for toggle-fold). */
  targets: string[];
  /** Action-specific arguments, may be {} (send-prompt carries {prompt}). */
  args: Record<string, unknown>;
  /** True = the user confirmed a destructive prompt. */
  confirmed: boolean;
}
