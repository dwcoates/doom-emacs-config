/**
 * The workspaces rail: a left dock listing every editor workspace Emacs
 * knows about, grouped by repo, with lifecycle dots, family nesting, and
 * per-row detail panels (ported from design-workspace-sidebar-mock.html).
 *
 * Emacs is the single state authority. It pushes the WHOLE roster through
 * the window hook installed below whenever workspace state changes, and
 * every user gesture here (switch, fold) is a command POSTed back to the
 * daemon — never an optimistic local mutation, so the rail can only ever
 * show what Emacs last asserted. Until the first push arrives the mount
 * stays hidden, so a bare-browser session keeps the single-column layout.
 */
import { HostGlobal } from "./host.js";
import { escapeHtml } from "./highlight.js";

/** A workspace's lifecycle, as the Emacs side classifies it. */
export type WorkspaceStatus =
  | "thinking"
  | "permission"
  | "done"
  | "done-viewed"
  | "idle"
  | "init"
  | "dead"
  | "merging"
  | "merge-queued"
  | "merge-conflict"
  | "merge-failed"
  | "merged"
  | "none";

/** Every status the roster may carry; anything else is a contract breach. */
const WORKSPACE_STATUSES: ReadonlySet<string> = new Set([
  "thinking",
  "permission",
  "done",
  "done-viewed",
  "idle",
  "init",
  "dead",
  "merging",
  "merge-queued",
  "merge-conflict",
  "merge-failed",
  "merged",
  "none",
]);

/** The statuses whose dot is the recycle glyph rather than a disc.
 * `merged` is deliberately absent: a settled merge is no longer part of the
 * merge pipeline the glyph denotes, and it renders in its own Recently
 * Merged section where the recycle mark would misread as still-queued. */
const MERGE_GLYPH_STATUSES: ReadonlySet<WorkspaceStatus> = new Set([
  "merging",
  "merge-queued",
  "merge-conflict",
  "merge-failed",
]);

export interface WorkspaceRow {
  name: string;
  dir: string;
  status: WorkspaceStatus;
  closed: boolean;
  current: boolean;
  /** Epoch SECONDS (Emacs time), or null for a workspace never viewed. */
  lastViewedAt: number | null;
  /** Epoch SECONDS the merge completed, or null when never merged. Drives
   * the when-column for Recently Merged rows, where merge age is the fact
   * worth showing rather than when the workspace was last looked at. */
  mergedAt: number | null;
  branch: string | null;
  parentBranch: string | null;
  summary: string | null;
  children: WorkspaceRow[];
}

export interface RepoGroup {
  key: string;
  label: string;
  folded: boolean;
  rows: WorkspaceRow[];
}

export interface WorkspaceRoster {
  repos: RepoGroup[];
  /** Workspaces merged inside the current activity window, newest first, or
   * null when none qualify. Shaped as a RepoGroup so it folds and validates
   * through the repo path; Emacs wipes it after a 6h inactivity gap. */
  recentlyMerged: RepoGroup | null;
  /** The keyboard cursor: the dir C-S-n / C-S-p currently point at. */
  navDir: string | null;
}

/** A gesture relayed to Emacs via POST /workspace-command (a JSON array). */
export type WorkspaceCommand =
  | { type: "switch"; dir: string }
  | { type: "fold"; repo_key: string; folded: boolean };

function fail(path: string, want: string, got: unknown): never {
  throw new Error(
    `workspace roster: ${path} must be ${want}, got ${JSON.stringify(got) ?? String(got)}`,
  );
}

function isRecord(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}

function asString(v: unknown, path: string): string {
  if (typeof v !== "string") fail(path, "a string", v);
  return v;
}

function asNullableString(v: unknown, path: string): string | null {
  if (v !== null && typeof v !== "string") fail(path, "a string or null", v);
  return v;
}

function asBoolean(v: unknown, path: string): boolean {
  if (typeof v !== "boolean") fail(path, "a boolean", v);
  return v;
}

function asNullableNumber(v: unknown, path: string): number | null {
  if (v !== null && typeof v !== "number") fail(path, "a number or null", v);
  return v;
}

function validateRow(value: unknown, path: string): WorkspaceRow {
  if (!isRecord(value)) fail(path, "an object", value);
  const status = asString(value.status, `${path}.status`);
  // An unrecognized status is an invariant violation, never a default: a
  // new Emacs-side lifecycle state must be taught here, not silently
  // painted as something it is not.
  if (!WORKSPACE_STATUSES.has(status)) {
    throw new Error(`workspace roster: unknown status ${JSON.stringify(status)} at ${path}.status`);
  }
  if (!Array.isArray(value.children)) fail(`${path}.children`, "an array", value.children);
  return {
    name: asString(value.name, `${path}.name`),
    dir: asString(value.dir, `${path}.dir`),
    status: status as WorkspaceStatus,
    closed: asBoolean(value.closed, `${path}.closed`),
    current: asBoolean(value.current, `${path}.current`),
    lastViewedAt: asNullableNumber(value.lastViewedAt, `${path}.lastViewedAt`),
    mergedAt: asNullableNumber(value.mergedAt, `${path}.mergedAt`),
    branch: asNullableString(value.branch, `${path}.branch`),
    parentBranch: asNullableString(value.parentBranch, `${path}.parentBranch`),
    summary: asNullableString(value.summary, `${path}.summary`),
    children: value.children.map((c, i) => validateRow(c, `${path}.children[${i}]`)),
  };
}

function validateGroup(value: unknown, path: string): RepoGroup {
  if (!isRecord(value)) fail(path, "an object", value);
  if (!Array.isArray(value.rows)) fail(`${path}.rows`, "an array", value.rows);
  return {
    key: asString(value.key, `${path}.key`),
    label: asString(value.label, `${path}.label`),
    folded: asBoolean(value.folded, `${path}.folded`),
    rows: value.rows.map((r, i) => validateRow(r, `${path}.rows[${i}]`)),
  };
}

/** The roster as pushed by Emacs, checked field-by-field. Throws on any
 * breach of the contract — the push is machine-built, so a malformed one
 * is a bug to surface, not an input to accommodate. */
export function validateWorkspaceRoster(value: unknown): WorkspaceRoster {
  if (!isRecord(value)) fail("roster", "an object", value);
  if (!Array.isArray(value.repos)) fail("roster.repos", "an array", value.repos);
  return {
    repos: value.repos.map((g, i) => validateGroup(g, `roster.repos[${i}]`)),
    // Explicit null is the "no recent merges" signal Emacs sends; anything
    // else must be a well-formed group, so an absent key falls through to
    // validateGroup and fails loudly rather than defaulting to empty.
    recentlyMerged:
      value.recentlyMerged === null
        ? null
        : validateGroup(value.recentlyMerged, "roster.recentlyMerged"),
    navDir: asNullableString(value.navDir, "roster.navDir"),
  };
}

/**
 * A viewed-stamp as a compact single-unit age: `now` under a minute, then
 * `5m` / `3h` / `2d`, truncated toward zero. One unit, unlike duration.ts's
 * two-level formatters — the rail's stamp column is a glance target, and
 * a second unit is a digit it has no room for. A null stamp (never viewed)
 * renders as nothing; a stamp ahead of the reader's clock floors to `now`.
 */
export function formatRecency(lastViewedAt: number | null, nowMs: number): string {
  if (lastViewedAt === null) return "";
  const seconds = Math.max(0, Math.floor(nowMs / 1000) - lastViewedAt);
  if (seconds < 60) return "now";
  if (seconds < 3600) return `${Math.floor(seconds / 60)}m`;
  if (seconds < 86400) return `${Math.floor(seconds / 3600)}h`;
  return `${Math.floor(seconds / 86400)}d`;
}

/**
 * A row's lifecycle dot. The status name doubles as the CSS class stem
 * (`st-thinking`, `st-merge-queued`, …) — the class list is the whole
 * visual contract, with the stylesheet mapping each stem to its color,
 * breath, or spin. The merge family carries the recycle glyph as content;
 * every disc status renders an empty span the stylesheet shapes.
 */
export function statusDotHtml(status: WorkspaceStatus): string {
  const glyph = MERGE_GLYPH_STATUSES.has(status) ? "⟳" : "";
  return `<span class="st st-${status}" title="${status}">${glyph}</span>`;
}

/** Rows in a family tree, the whole tree counted (the chip's arithmetic). */
function countRows(rows: readonly WorkspaceRow[]): number {
  return rows.reduce((n, r) => n + 1 + countRows(r.children), 0);
}

/** The expanded detail panel: only the facts the roster actually carries. */
function detailHtml(row: WorkspaceRow): string {
  const pair = (label: string, value: string | null): string =>
    value === null ? "" : `<dt>${label}</dt><dd><code>${escapeHtml(value)}</code></dd>`;
  const summary =
    row.summary === null ? "" : `<p class="summary">${escapeHtml(row.summary)}</p>`;
  return `<div class="detail"><dl>${pair("branch", row.branch)}${pair(
    "parent",
    row.parentBranch,
  )}${pair("dir", row.dir)}</dl>${summary}</div>`;
}

/**
 * One workspace and, nested under it, its family. The wrapper's class
 * list carries the row states the stylesheet draws: `current` (accent
 * left edge + wash), `gone` (closed OR merged — both recede), `navsel`
 * (the keyboard cursor's dashed ring), `open` (detail panel shown).
 * Children nest inside `.kids`, whose left guide line and margin compound
 * per generation.
 */
function workspaceHtml(
  row: WorkspaceRow,
  navDir: string | null,
  open: ReadonlySet<string>,
  nowMs: number,
): string {
  const cls = ["ws"];
  if (row.current) cls.push("current");
  if (row.closed || row.status === "merged") cls.push("gone");
  if (navDir !== null && row.dir === navDir) cls.push("navsel");
  if (open.has(row.dir)) cls.push("open");
  const kids =
    row.children.length === 0
      ? ""
      : `<div class="kids">${row.children
          .map((c) => workspaceHtml(c, navDir, open, nowMs))
          .join("")}</div>`;
  // The dir addresses the row for the mount's delegated click handler:
  // a body click switches to it, the chevron click toggles its panel.
  return `<div class="${cls.join(" ")}">
    <div class="row" data-row-dir="${escapeHtml(row.dir)}">
      ${statusDotHtml(row.status)}<span class="name">${escapeHtml(row.name)}</span>
      <span class="when">${formatRecency(row.mergedAt ?? row.lastViewedAt, nowMs)}</span>
      <span class="chev" data-chev>▸</span>
    </div>
    ${detailHtml(row)}
    ${kids}
  </div>`;
}

/** A repo section: fold-toggling header plus its workspace family trees.
 * A folded section keeps its rows in the DOM — the stylesheet hides them
 * — so unfolding is a class flip, not a rebuild. */
function repoSectionHtml(
  group: RepoGroup,
  navDir: string | null,
  open: ReadonlySet<string>,
  nowMs: number,
  extraClass = "",
): string {
  const rows = group.rows.map((r) => workspaceHtml(r, navDir, open, nowMs)).join("");
  return `<section class="repo${group.folded ? " folded" : ""}${
    extraClass === "" ? "" : ` ${extraClass}`
  }">
    <div class="repo-head" data-repo-key="${escapeHtml(group.key)}">
      <span class="tri">▾</span> ${escapeHtml(group.label)} <span class="n">(${countRows(
        group.rows,
      )})</span>
    </div>
    <div class="rows">${rows}</div>
  </section>`;
}

/**
 * The whole rail: header (title, total count, any transient command-failure
 * note) over the scrolling repo sections. Pure so the render is testable
 * without a DOM; OPEN is the caller-owned expansion state that must survive
 * every roster push.
 */
export function sidebarHtml(
  roster: WorkspaceRoster,
  open: ReadonlySet<string>,
  nowMs: number,
  errorNote: string | null,
): string {
  const total = roster.repos.reduce((n, g) => n + countRows(g.rows), 0);
  const err = errorNote === null ? "" : `<span class="sb-err">${escapeHtml(errorNote)}</span>`;
  const repos = roster.repos
    .map((g) => repoSectionHtml(g, roster.navDir, open, nowMs))
    .join("");
  // Settled merges render last, in their own section, so they read as
  // history rather than as live work sitting in the repo they came from.
  const merged =
    roster.recentlyMerged === null
      ? ""
      : repoSectionHtml(roster.recentlyMerged, roster.navDir, open, nowMs, "merged-section");
  return `<div class="sb-head">
      <span class="sb-title">Workspaces</span>
      <span class="sb-count">${total}</span>
      ${err}
    </div>
    <div class="sb-scroll">${repos}${merged}</div>`;
}

/** The header note shown when a POSTed command did not land. */
export const COMMAND_FAILED_NOTICE = "workspace command failed";

/** How long the failure note holds the header before clearing. */
export const COMMAND_FAILED_NOTICE_MS = 4000;

export interface WorkspaceSidebarOptions {
  /** The daemon's HTTP base (main.ts derives it from ?daemon). */
  httpBase: string;
  /** Injectable for tests. */
  fetchFn?: typeof fetch;
  /** Wall clock for the recency stamps, injectable for tests. */
  now?: () => number;
}

export class WorkspaceSidebar {
  private readonly mount: HTMLElement;
  private readonly httpBase: string;
  private readonly fetchFn: typeof fetch;
  private readonly now: () => number;
  private roster: WorkspaceRoster | null = null;
  /**
   * Dirs whose detail panel is open. Sidebar-owned (like the feed's open
   * panels), NOT derived from the roster, so a push mid-read cannot slam
   * a panel shut; keyed by dir because rows have no other stable key.
   */
  private readonly openDirs = new Set<string>();
  private errorNote: string | null = null;

  constructor(mount: HTMLElement, opts: WorkspaceSidebarOptions) {
    this.mount = mount;
    this.httpBase = opts.httpBase;
    this.fetchFn = opts.fetchFn ?? fetch;
    this.now = opts.now ?? Date.now;
    // One delegated listener: every render rewrites the mount's children,
    // so per-row bindings would die with the first push after them.
    this.mount.addEventListener("click", (e) => {
      this.onClick(e.target as HTMLElement);
    });
  }

  /** Adopt a pushed roster (validated — a malformed push throws) and
   * paint. The first push is what reveals the rail. */
  update(roster: unknown): void {
    this.roster = validateWorkspaceRoster(roster);
    this.mount.hidden = false;
    this.render();
  }

  private render(): void {
    if (this.roster === null) return;
    this.mount.innerHTML = sidebarHtml(this.roster, this.openDirs, this.now(), this.errorNote);
  }

  /**
   * Flip DIR's detail panel open or shut and repaint. The single
   * mutation point for `openDirs`, shared by the chevron click and the
   * Emacs expand hook (C-S-RET) so the two entry points can never
   * disagree. A dir with no rendered row still records in `openDirs`; it
   * simply has no panel to show until such a row appears.
   */
  toggleDetail(dir: string): void {
    if (this.openDirs.has(dir)) this.openDirs.delete(dir);
    else this.openDirs.add(dir);
    this.render();
  }

  private onClick(target: HTMLElement): void {
    // The chevron sits inside the row, so it must claim the click first
    // or every expansion would also switch workspaces.
    if (target.closest("[data-chev]")) {
      const dir = target.closest("[data-row-dir]")?.getAttribute("data-row-dir");
      if (dir == null) throw new Error("workspace sidebar: chevron outside a row");
      this.toggleDetail(dir);
      return;
    }
    const rowEl = target.closest("[data-row-dir]");
    if (rowEl) {
      const dir = rowEl.getAttribute("data-row-dir");
      if (dir === null) throw new Error("workspace sidebar: row without a dir");
      this.post([{ type: "switch", dir }]);
      return;
    }
    const head = target.closest("[data-repo-key]");
    if (head) {
      const key = head.getAttribute("data-repo-key");
      if (key === null) throw new Error("workspace sidebar: repo head without a key");
      const group = this.roster?.repos.find((g) => g.key === key);
      if (!group) throw new Error(`workspace sidebar: no repo group for key ${key}`);
      // The DESIRED state is asked for, never applied here: Emacs owns
      // the fold and answers with a fresh roster push.
      this.post([{ type: "fold", repo_key: key, folded: !group.folded }]);
    }
  }

  private post(commands: readonly WorkspaceCommand[]): void {
    void this.fetchFn(`${this.httpBase}/workspace-command`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(commands),
    })
      .then((resp) => {
        if (!resp.ok) throw new Error(`POST /workspace-command: ${resp.status}`);
      })
      .catch((err: unknown) => {
        // A command that did not land must say so: the user just clicked
        // expecting Emacs to move, and silence would read as a dead rail.
        console.error("workspace command failed", err);
        this.showError(COMMAND_FAILED_NOTICE);
      });
  }

  private showError(note: string): void {
    this.errorNote = note;
    this.render();
    // The timed clear checks the header still shows THIS note, so it
    // never wipes a newer failure that landed meanwhile.
    setTimeout(() => {
      if (this.errorNote === note) {
        this.errorNote = null;
        this.render();
      }
    }, COMMAND_FAILED_NOTICE_MS);
  }
}

/**
 * Name of the global Emacs pushes the roster through: the host evaluates
 * `window.agentReplWorkspaceRoster(<json literal>)`, so the hook receives
 * the already-parsed roster object. The lisp side MUST match this string.
 */
export const ROSTER_HOOK = "agentReplWorkspaceRoster";

/** Plants the roster hook on the host global (main.ts boot). */
export function installWorkspaceRosterHook(target: HostGlobal, sidebar: WorkspaceSidebar): void {
  target[ROSTER_HOOK] = (roster: unknown): void => {
    sidebar.update(roster);
  };
}

/**
 * Name of the global Emacs fires to toggle a row's detail panel from the
 * keyboard (C-S-RET): the host evaluates
 * `window.agentReplWorkspaceExpand(<dir json literal>)`, so the hook
 * receives the row's canonical project dir as a string. The lisp side
 * (`agent-repl--sidebar-expand-hook`) MUST match this string.
 */
export const EXPAND_HOOK = "agentReplWorkspaceExpand";

/** Plants the expand hook on the host global (main.ts boot). */
export function installWorkspaceExpandHook(target: HostGlobal, sidebar: WorkspaceSidebar): void {
  target[EXPAND_HOOK] = (dir: unknown): void => {
    // The call is machine-built by Emacs, so a non-string dir is a
    // contract breach to surface, not an input to coerce.
    if (typeof dir !== "string") {
      throw new Error(`workspace expand hook: dir must be a string, got ${JSON.stringify(dir)}`);
    }
    sidebar.toggleDetail(dir);
  };
}
