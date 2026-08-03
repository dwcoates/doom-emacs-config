/**
 * The workspaces rail: a left dock listing every editor workspace Emacs
 * knows about, grouped by repo, with lifecycle dots, family nesting, and
 * per-row detail panels (ported from design-workspace-sidebar-mock.html).
 *
 * Emacs is the single state authority. It publishes the WHOLE roster —
 * never a delta — whenever workspace state changes, and every user gesture
 * here (switch, fold) is a command POSTed back to the daemon, never an
 * optimistic local mutation, so the rail can only ever show what Emacs last
 * asserted. Until the first roster arrives the mount stays hidden, so a
 * bare-browser session keeps the single-column layout.
 *
 * ONE PUBLISH PATH. The roster reaches the rail as a
 * `frontend.v1.WorkspaceRoster` frame off the same websocket the feed and the
 * footer read, decoded by `frontend-proto.ts` and mapped here by
 * `rosterFromFrame`. The legacy `window.agentReplWorkspaceRoster` script
 * injection is GONE: it could not carry the revision the gate ranks by, and a
 * second ingress meant the reveal and the diagnostics could differ by path.
 * With one ingress the frame's validation is the only door in, so a malformed
 * roster fails at the decoder rather than halfway down the render.
 */
import type { RosterRow as FrameRosterRow, WorkspaceRoster as RosterFrame } from "./frontend-proto.js";
import { ROSTER_ROW_STATUS_KEYWORD } from "./frontend-proto.js";
import { HostGlobal } from "./host.js";
import { escapeHtml } from "./highlight.js";
import { mergeFacts, mergeStatusLogValue } from "./merge-status.js";
import type { MergeStatus, WebRenderState } from "./state-adapter.js";
import { log } from "./wslog.js";

/**
 * A workspace's lifecycle, as the Emacs side classifies it.
 *
 * The dots speak the SAME five-color vocabulary the tab-bar does, so a
 * workspace can never read one way in the rail and another in the tabs:
 *
 * - blue   `init`, `dead`, `start-failed`, `degraded` — the route is
 *   compromised on agent-repl's side.
 * - purple `vendor-blocked` — blocked on the vendor or the account.
 * - red    `thinking` — a turn is in flight.
 * - yellow `idle-async` — no foreground turn, live detached work.
 * - green  `ready`, `done`, `permission` — the route is proven usable and
 *   the history painted. A pending permission is green: the agent is ready
 *   for the user to look at the response.
 *
 * The merge statuses are untouched — the spinning recycle glyph already
 * says what the merge pipeline needs to, without spending a color.
 */
export type WorkspaceStatus =
  | "submitting"
  | "thinking"
  | "clearing"
  | "compacting"
  | "permission"
  | "done"
  | "interrupted"
  | "ready"
  | "idle-async"
  | "vendor-blocked"
  | "init"
  | "severed"
  | "hibernated"
  | "start-failed"
  | "degraded"
  | "dead"
  | "merge-enqueuing"
  | "merging"
  | "merge-queued"
  | "merge-conflict"
  | "merge-failed"
  | "merged"
  | "none"
  // A registered workspace with no open perspective: it shows in the sidebar
  // roster but not the tab-bar. No live session backs it, so it carries a
  // question mark rather than a lifecycle dot (see `statusDotHtml`).
  | "inactive";

/**
 * Every status the roster may carry; anything else is a contract breach.
 *
 * EXPORTED so the cross-language color test can assert it against the shared
 * fixture. `sidebar.el` claims in a comment that its wire table and this set
 * "are one contract and MUST stay in sync" — this is what lets that claim be
 * checked rather than merely asserted.
 */
export const WORKSPACE_STATUSES: ReadonlySet<string> = new Set([
  "submitting",
  "thinking",
  "clearing",
  "compacting",
  "permission",
  "done",
  "interrupted",
  "ready",
  "idle-async",
  "vendor-blocked",
  "init",
  "severed",
  "hibernated",
  "start-failed",
  "degraded",
  "dead",
  "merge-enqueuing",
  "merging",
  "merge-queued",
  "merge-conflict",
  "merge-failed",
  "merged",
  "none",
  "inactive",
]);

/** Map the daemon wire spelling onto the roster's CSS spelling. */
export function workspaceStatusFromRenderState(state: WebRenderState): WorkspaceStatus {
  switch (state) {
    case "idle": return "ready";
    case "idle_async": return "idle-async";
    case "vendor_blocked": return "vendor-blocked";
    case "merge_enqueuing": return "merge-enqueuing";
    case "merge_queued": return "merge-queued";
    case "merge_conflict": return "merge-conflict";
    case "merge_failed": return "merge-failed";
    default: return state;
  }
}

/** The statuses whose dot is the recycle glyph rather than a disc.
 * `merged` is deliberately absent: a settled merge is no longer part of the
 * merge pipeline the glyph denotes, and it renders in its own Recently
 * Merged section where the recycle mark would misread as still-queued. */
const MERGE_GLYPH_STATUSES: ReadonlySet<WorkspaceStatus> = new Set([
  "merge-enqueuing",
  "merging",
  "merge-queued",
  "merge-conflict",
  "merge-failed",
]);

/**
 * The quiescent statuses the session's own MONITORING overlay may repaint:
 * a workspace at rest whose background async still lives shows the
 * breathing amber dot instead of its resting disc. Active and merge
 * states stay untouched — thinking's red breath and the merge glyphs are
 * strictly more specific signals than "background work continues".
 * Session-local chrome on top of the Emacs-pushed roster, NOT a roster
 * status: Emacs never asserts "monitoring", the webview overlays it for
 * its OWN current row exactly as the topbar datapoint it replaces did.
 */
const MONITORABLE_STATUSES: ReadonlySet<WorkspaceStatus> = new Set([
  "ready",
  "done",
]);

export interface WorkspaceRow {
  name: string;
  dir: string;
  status: WorkspaceStatus;
  closed: boolean;
  current: boolean;
  /** Epoch SECONDS, or null for a workspace never viewed. The WIRE carries
   * milliseconds; `stampFromMs` converts at the mapping seam so the render
   * path below stays in the one unit `formatRecency` has always spoken. */
  lastViewedAt: number | null;
  /** Epoch SECONDS the merge completed, or null when never merged (same
   * millisecond conversion as `lastViewedAt`). Drives the when-column for
   * Recently Merged rows, where merge age is the fact worth showing rather
   * than when the workspace was last looked at. */
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
  /** The section's done state. Meaningful only for task sections (the Task
   * view); repo and Recently-Merged sections always carry `false`. Emacs
   * sends it on every group so validation stays one code path. */
  done: boolean;
  rows: WorkspaceRow[];
}

/** Which grouping the rail shows: repositories (default) or user tasks. */
export type SidebarView = "repository" | "task";

/** Section key of the catch-all "No task" group (matches the Emacs
 * `agent-repl--sidebar-no-task-key`): workspaces in no task collect here,
 * and it carries no checkbox / open / add affordances. */
export const NO_TASK_KEY = "__no_task__";

export interface WorkspaceRoster {
  /** The active grouping; the webapp renders `repos` or `tasks` per this. */
  view: SidebarView;
  repos: RepoGroup[];
  /** Task sections when `view === "task"`, otherwise empty. Shaped as
   * RepoGroups (plus the `done` flag) so they validate and render through
   * the same group path the repos use. */
  tasks: RepoGroup[];
  /** Workspaces merged inside the current activity window, newest first, or
   * null when none qualify. Shaped as a RepoGroup so it folds and validates
   * through the repo path; Emacs wipes it after a 6h inactivity gap. */
  recentlyMerged: RepoGroup | null;
  /** The keyboard cursor: the dir C-S-n / C-S-p currently point at. */
  navDir: string | null;
}

function rosterWithCurrentStatus(
  roster: WorkspaceRoster,
  status: WorkspaceStatus | null,
): WorkspaceRoster {
  if (status === null) return roster;
  const mapRow = (row: WorkspaceRow): WorkspaceRow => ({
    ...row,
    status: row.current ? status : row.status,
    children: row.children.map(mapRow),
  });
  const mapGroup = (group: RepoGroup): RepoGroup => ({
    ...group,
    rows: group.rows.map(mapRow),
  });
  return {
    ...roster,
    repos: roster.repos.map(mapGroup),
    tasks: roster.tasks.map(mapGroup),
    recentlyMerged: roster.recentlyMerged === null ? null : mapGroup(roster.recentlyMerged),
  };
}

/** A gesture relayed to Emacs via POST /workspace-command (a JSON array). */
export type WorkspaceCommand =
  | { type: "switch"; dir: string }
  | { type: "fold"; repo_key: string; folded: boolean }
  | { type: "set-view"; view: SidebarView }
  | { type: "task-create" }
  | { type: "task-toggle-done"; id: string }
  | { type: "task-open"; id: string }
  | { type: "task-add-workspace"; id: string };

/** Cap on the payload snippet a breach log line carries: the daemon
 * clamps per-message size anyway, and a full roster/row dump would drown
 * the line the operator actually needs to spot the bug in. */
const BREACH_PAYLOAD_MAX = 300;

/** A JSON rendering of V for a breach log line, truncated to a sane length. */
function describeBreach(v: unknown): string {
  const s = JSON.stringify(v) ?? String(v);
  return s.length > BREACH_PAYLOAD_MAX ? `${s.slice(0, BREACH_PAYLOAD_MAX)}…` : s;
}

// --- the roster frame -------------------------------------------------------

/**
 * Roster key of the Recently Merged section.
 *
 * SYNTHESIZED, and deliberately still so: `frontend.v1.RosterSection` carries
 * `folded` and `label` but no key, because the section is a fixed singleton
 * rather than one of an open set of groups. The fold gesture nonetheless POSTs
 * a `repo_key` back, and Emacs looks the section up by it, so this constant
 * must match `agent-repl--sidebar-merged-key` exactly. The LABEL and the FOLD
 * are no longer synthesized alongside it — the author resolves both and the
 * frame carries them.
 */
export const RECENTLY_MERGED_KEY = "__recently_merged__";

/**
 * A wire epoch-MILLISECOND stamp as the rail's internal epoch SECONDS, or null
 * for the proto3 zero that spells "never".
 *
 * THE UNIT CHANGES HERE, and only here. `frontend.v1.RosterRow` carries
 * milliseconds; `formatRecency` and the `WorkspaceRow` fields feeding it have
 * always been seconds. Converting at this single seam keeps the factor of 1000
 * out of the render path, where getting it wrong reads not as a crash but as
 * every workspace being eternally "now" — a wrong answer that looks plausible.
 */
function stampFromMs(ms: number): number | null {
  return ms === 0 ? null : Math.floor(ms / 1000);
}

/**
 * A proto3 string field as the rail's nullable text.
 *
 * The empty string is the wire's only way to say "unknown" for `branch`,
 * `parentBranch`, and `summary` (proto3 scalars have no presence), and the
 * detail panel omits a line entirely rather than printing an empty one.
 */
function textFromWire(s: string): string | null {
  return s === "" ? null : s;
}

/**
 * A frame row's status arm mapped onto the rail's status vocabulary.
 *
 * `ROSTER_ROW_STATUS_KEYWORD` is total over the arm union, so this cannot
 * miss an arm. The membership check guards the OTHER direction — the two
 * tables are separately maintained spellings of one closed set, and a drift
 * between them must surface here rather than paint a row as a status the
 * stylesheet has no rule for.
 */
function statusFromFrameRow(row: FrameRosterRow, path: string): WorkspaceStatus {
  const keyword = ROSTER_ROW_STATUS_KEYWORD[row.status.case];
  if (!WORKSPACE_STATUSES.has(keyword)) {
    log(
      "error",
      `workspace roster: frame status arm ${JSON.stringify(row.status.case)} maps to unknown status ` +
        `${JSON.stringify(keyword)} at ${path} — row: ${describeBreach(row)}`,
      {
        operation: "sidebar.roster-frame-status-invalid",
        context: { path, arm: row.status.case, status: keyword, row },
      },
    );
    throw new Error(
      `workspace roster: frame status arm ${JSON.stringify(row.status.case)} maps to unknown status ` +
        `${JSON.stringify(keyword)} at ${path}`,
    );
  }
  return keyword as WorkspaceStatus;
}

/**
 * One frame row as a rail row.
 *
 * The frame now carries EVERY fact the rail knows how to draw — the identity
 * and family, the status arm, the two timestamps behind the when-column, the
 * detail panel's branch lines and summary, and `closed`. Nothing is defaulted
 * away here any more; the gaps this mapping used to record (a blank
 * when-column, an empty detail panel, a closed row that refused to recede)
 * closed with the display fields landing on `frontend.v1.RosterRow`.
 *
 * The two nullable spellings are wire-shaped, not lost data: 0 is the only
 * "never" a proto3 int64 can express and "" the only "unknown" a string can,
 * so `stampFromMs` and `textFromWire` translate them at this seam rather than
 * leaving every render site to test for the sentinel.
 */
function rowFromFrame(row: FrameRosterRow, path: string): WorkspaceRow {
  return {
    name: row.name,
    dir: row.dir,
    status: statusFromFrameRow(row, path),
    closed: row.closed,
    current: row.current,
    lastViewedAt: stampFromMs(row.lastViewedAtMs),
    mergedAt: stampFromMs(row.mergedAtMs),
    branch: textFromWire(row.branch),
    parentBranch: textFromWire(row.parentBranch),
    summary: textFromWire(row.summary),
    children: row.children.map((child, i) => rowFromFrame(child, `${path}.children[${i}]`)),
  };
}

/**
 * A decoded `frontend.v1.WorkspaceRoster` as the rail's internal roster.
 *
 * The view ONEOF's set arm is the grouping — there is no separate mode flag
 * to disagree with it — so the inactive grouping's section list is empty by
 * construction rather than by convention. Repo sections carry a display
 * `label` alongside the `repoKey` that stays their fold identity, and an empty
 * label means the author has none, so the key shows through as the header
 * text; task sections carry `title` as the label and `taskId` as the key,
 * which is what every task command addresses.
 *
 * The frame's `currentDir` is not read: the rail marks the current row from
 * the row's own `current` flag, exactly as it always has, and consuming both
 * would create two authorities on one fact.
 */
export function rosterFromFrame(frame: RosterFrame): WorkspaceRoster {
  const repos: RepoGroup[] =
    frame.view.case === "repository"
      ? frame.view.value.sections.map((section, i) => ({
          key: section.repoKey,
          // The key is the fallback, not a placeholder string: a section with
          // no author label still has to head with something the fold gesture
          // and the operator can both recognize.
          label: section.label === "" ? section.repoKey : section.label,
          folded: section.folded,
          // Repo sections have no done axis; the shared group shape carries
          // the field, and Emacs sends `false` on every repo group today.
          done: false,
          rows: section.rows.map((r, j) => rowFromFrame(r, `roster.repos[${i}].rows[${j}]`)),
        }))
      : [];
  const tasks: RepoGroup[] =
    frame.view.case === "task"
      ? frame.view.value.sections.map((section, i) => ({
          key: section.taskId,
          label: section.title,
          // Tasks do not fold: `taskSectionHtml` renders a `.task-head`, not
          // the fold-toggling `.repo-head`, so no fold state can apply.
          folded: false,
          done: section.done,
          rows: section.rows.map((r, j) => rowFromFrame(r, `roster.tasks[${i}].rows[${j}]`)),
        }))
      : [];
  // An empty section is the frame's "no recent merges": proto3 has no absent
  // repeated field, so emptiness is the only signal available, and the rail
  // renders no section at all rather than an empty header. The FOLD and the
  // LABEL now come off the frame — the author resolves both — leaving only the
  // key synthesized, because the fold command has to address the section by a
  // name the wire has no field for.
  const mergedRows = frame.recentlyMerged.rows.map((r, i) =>
    rowFromFrame(r, `roster.recentlyMerged.rows[${i}]`),
  );
  return {
    view: frame.view.case,
    repos,
    tasks,
    recentlyMerged:
      mergedRows.length === 0
        ? null
        : {
            key: RECENTLY_MERGED_KEY,
            label: frame.recentlyMerged.label,
            folded: frame.recentlyMerged.folded,
            done: false,
            rows: mergedRows,
          },
    // An empty dir is the frame's "no cursor" (proto3 strings have no null),
    // which the rail's nullable navDir spells as null.
    navDir: frame.navDir === "" ? null : frame.navDir,
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
export function statusDotHtml(status: WorkspaceStatus, monitoring = false): string {
  if (monitoring && MONITORABLE_STATUSES.has(status)) {
    return `<span class="st st-monitoring" title="monitoring"></span>`;
  }
  // A perspective-less workspace (in the sidebar, not the tab-bar) has no live
  // session whose status a disc could report, so it shows a question mark.
  if (status === "inactive") {
    return `<span class="st st-inactive" title="inactive">❓</span>`;
  }
  const glyph = MERGE_GLYPH_STATUSES.has(status) ? "⟳" : "";
  return `<span class="st st-${status}" title="${status}">${glyph}</span>`;
}

/**
 * The current row's merge, as the two lines the rail has room for, or "" when
 * no merge run touches this session.
 *
 * THE RAIL LEADS ON MERGES (the footer's phase-cell note says so explicitly),
 * and until now the only thing it could say was the spinning recycle glyph:
 * every phase of every merge looked exactly alike, including the one parked on
 * a human. These lines are what the glyph could never carry — how far the run
 * has got, which commit is in hand, and why it stopped.
 *
 * Session-local chrome over the Emacs-pushed roster, exactly like the
 * monitoring overlay: it is drawn for THIS webview's own row only, because the
 * revisioned `WorkspaceState` it comes from describes only this session.
 */
export function mergeRowHtml(status: MergeStatus | null): string {
  const facts = mergeFacts(status);
  if (facts === null) return "";
  const head = [facts.word, facts.count, facts.activity].filter((part) => part !== "").join(" · ");
  const note =
    facts.note === ""
      ? ""
      : `<div class="ws-merge-note ${facts.tone}">${escapeHtml(facts.note)}</div>`;
  return `<div class="ws-merge ${facts.tone}">${escapeHtml(head)}</div>${note}`;
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
  monitoring: boolean,
  mergeStatus: MergeStatus | null = null,
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
          .map((c) => workspaceHtml(c, navDir, open, nowMs, monitoring, mergeStatus))
          .join("")}</div>`;
  // The dir addresses the row for the mount's delegated click handler:
  // a body click switches to it, the chevron click toggles its panel.
  // The monitoring overlay touches only THIS session's row: the roster's
  // other rows keep exactly the status Emacs asserted.
  return `<div class="${cls.join(" ")}">
    <div class="row" data-row-dir="${escapeHtml(row.dir)}">
      ${statusDotHtml(row.status, monitoring && row.current)}<span class="name">${escapeHtml(row.name)}</span>
      <span class="when">${formatRecency(row.mergedAt ?? row.lastViewedAt, nowMs)}</span>
      <span class="chev" data-chev>▸</span>
    </div>
    ${row.current ? mergeRowHtml(mergeStatus) : ""}
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
  monitoring = false,
  mergeStatus: MergeStatus | null = null,
): string {
  const rows = group.rows
    .map((r) => workspaceHtml(r, navDir, open, nowMs, monitoring, mergeStatus))
    .join("");
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
 * A task section: a checkbox + label (the label opens the task's org notes,
 * the checkbox toggles done), a row count, and an add-workspace `+`. A done
 * task greys and strikes through. The catch-all "No task" group carries none
 * of those affordances — it is a bucket, not a task. Reuses the `.repo`
 * section shell (and thus the shared `.rows` / `.ws` styling) but its header
 * is `.task-head`, not the fold-toggling `.repo-head`: tasks do not fold.
 */
export function taskSectionHtml(
  group: RepoGroup,
  navDir: string | null,
  open: ReadonlySet<string>,
  nowMs: number,
  monitoring = false,
  mergeStatus: MergeStatus | null = null,
): string {
  const rows = group.rows
    .map((r) => workspaceHtml(r, navDir, open, nowMs, monitoring, mergeStatus))
    .join("");
  const isNoTask = group.key === NO_TASK_KEY;
  const doneCls = group.done ? " done" : "";
  const id = escapeHtml(group.key);
  const label = escapeHtml(group.label);
  // The catch-all bucket is inert: no checkbox, no notes to open, nothing
  // to add a workspace to (a workspace "in no task" is the absence of a
  // membership, not a task with an org file).
  const head = isNoTask
    ? `<span class="task-label no-task">${label}</span><span class="n">(${countRows(group.rows)})</span>`
    : `<span class="task-check${doneCls}" data-task-check data-task-id="${id}" title="toggle done">${
        group.done ? "✓" : ""
      }</span><span class="task-label${doneCls}" data-task-open data-task-id="${id}" title="open notes">${label}</span><span class="n">(${countRows(
        group.rows,
      )})</span><span class="task-add" data-task-add data-task-id="${id}" title="add workspace to task">+</span>`;
  return `<section class="repo task-section${doneCls}">
    <div class="task-head">${head}</div>
    <div class="rows">${rows}</div>
  </section>`;
}

/** The header's view selector: a two-button segmented control plus, in the
 * Task view, the `+` that creates a new task. Emacs owns the view and the
 * task model, so each control POSTs a command rather than mutating locally. */
function viewSelectorHtml(view: SidebarView): string {
  const btn = (v: SidebarView, text: string, title: string): string =>
    `<button class="sb-view-btn${view === v ? " active" : ""}" data-set-view="${v}" title="${title}">${text}</button>`;
  const add =
    view === "task"
      ? `<button class="sb-add" data-add-task title="New task">+</button>`
      : "";
  return `<span class="sb-views">${btn("repository", "Repos", "Group by repository")}${btn(
    "task",
    "Tasks",
    "Group by task",
  )}</span>${add}`;
}

/**
 * The whole rail: header (title, total count, view selector) over the
 * scrolling sections, with any transient failure note as the rail's FOOTER.
 * The active view (`roster.view`) picks the grouping — repo sections or task
 * sections — while the Recently-Merged section renders under both. Pure so
 * the render is testable without a DOM; OPEN is the caller-owned expansion
 * state that must survive every roster push.
 *
 * The note sits at the bottom rather than beside the control that failed:
 * every sidebar gesture (view selector, task controls, row switch, repo
 * fold) reports through one fixed slot, so the user learns a single place
 * to look instead of hunting for an inline note next to whatever they last
 * clicked — and a long message can never shove the header's controls around.
 */
export function sidebarHtml(
  roster: WorkspaceRoster,
  open: ReadonlySet<string>,
  nowMs: number,
  errorNote: string | null,
  monitoring = false,
  mergeStatus: MergeStatus | null = null,
): string {
  const taskView = roster.view === "task";
  const groups = taskView ? roster.tasks : roster.repos;
  const total = groups.reduce((n, g) => n + countRows(g.rows), 0);
  const err =
    errorNote === null ? "" : `<div class="sb-err" role="alert">${escapeHtml(errorNote)}</div>`;
  const sections = taskView
    ? roster.tasks
        .map((g) => taskSectionHtml(g, roster.navDir, open, nowMs, monitoring, mergeStatus))
        .join("")
    : roster.repos
        .map((g) => repoSectionHtml(g, roster.navDir, open, nowMs, "", monitoring, mergeStatus))
        .join("");
  // Settled merges render last, in their own section, so they read as
  // history rather than as live work sitting in the repo they came from.
  const merged =
    roster.recentlyMerged === null
      ? ""
      : repoSectionHtml(roster.recentlyMerged, roster.navDir, open, nowMs, "merged-section");
  return `<div class="sb-head">
      <span class="sb-title">${taskView ? "Tasks" : "Workspaces"}</span>
      <span class="sb-count">${total}</span>
      ${viewSelectorHtml(roster.view)}
    </div>
    <div class="sb-scroll">${sections}${merged}</div>
    ${err}`;
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
  /**
   * Host feed pinning probe: true when the feed sits at its tail. Read
   * synchronously at the reveal, BEFORE the rail's first appearance
   * reflows the feed — the reveal flips the rail into the flex row and
   * narrows the feed, so a feed the user had scrolled up must be left
   * where they put it rather than yanked back down.
   */
  isPinned?: () => boolean;
  /**
   * Host feed re-park, invoked after the reveal narrowed a pinned feed.
   * The boot render already parked the feed at its tail, but the rail's
   * first appearance reflows it and strands that park a rail-width above
   * the bottom — so the gui's two halves (output + sidebar) finish
   * appearing together at the newest message. Injected (like
   * configureChessGames' parkFeed) so the sidebar never reaches for the
   * feed element itself.
   */
  parkFeed?: () => void;
}

export class WorkspaceSidebar {
  private readonly mount: HTMLElement;
  private readonly httpBase: string;
  private readonly fetchFn: typeof fetch;
  private readonly now: () => number;
  private readonly isPinned: () => boolean;
  private readonly parkFeed: () => void;
  private roster: WorkspaceRoster | null = null;
  /**
   * Revision of the held roster, or null before the first adoption. Null is
   * distinct from revision 0: nothing has been asserted yet, so the very
   * first roster is adopted whatever it claims.
   */
  private rosterRevision: number | null = null;
  /**
   * Dirs whose detail panel is open. Sidebar-owned (like the feed's open
   * panels), NOT derived from the roster, so a push mid-read cannot slam
   * a panel shut; keyed by dir because rows have no other stable key.
   */
  private readonly openDirs = new Set<string>();
  private errorNote: string | null = null;
  /** The session's own idle-with-live-async gate (see setMonitoring). */
  private monitoring = false;
  /** Current row status from this webview's revisioned WorkspaceState. */
  private authoritativeCurrentStatus: WorkspaceStatus | null = null;
  /** The current row's merge run, from that same revisioned message. */
  private mergeStatus: MergeStatus | null = null;

  constructor(mount: HTMLElement, opts: WorkspaceSidebarOptions) {
    this.mount = mount;
    this.httpBase = opts.httpBase;
    // Bound to the window, NOT stored bare: post() calls this through
    // `this.fetchFn(...)`, which would hand fetch the sidebar as its `this`,
    // and WKWebView rejects that outright ("Can only call Window.fetch on
    // instances of Window") — every POSTing gesture dies in the real webview
    // while a this-insensitive test fetch keeps passing.
    this.fetchFn = opts.fetchFn ?? fetch.bind(globalThis);
    this.now = opts.now ?? Date.now;
    // Default to a no-op park: a bare-builder test (or a bare-browser
    // session with no feed handle wired) reveals the rail without a feed
    // to re-park, and the reveal must stay a plain paint there.
    this.isPinned = opts.isPinned ?? ((): boolean => false);
    this.parkFeed = opts.parkFeed ?? ((): void => {});
    // One delegated listener: every render rewrites the mount's children,
    // so per-row bindings would die with the first push after them.
    this.mount.addEventListener("click", (e) => {
      // A gesture that could not even be classified is still a gesture the
      // user made and expects an answer to, so it reports in the SAME footer
      // slot a failed POST does — then rethrows, because a contract breach in
      // the render/handler pair must stay as loud as it ever was.
      try {
        this.onClick(e.target as HTMLElement);
      } catch (err: unknown) {
        this.showError(String(err));
        throw err;
      }
    });
  }

  /**
   * Adopt a decoded `frontend.v1.WorkspaceRoster` frame and paint.
   *
   * THE ONLY INGRESS. The roster arrives on the SAME websocket the feed and
   * the footer read, so a connect snapshot paints all three surfaces from one
   * burst rather than leaving the rail waiting on a separate script injection.
   */
  adoptRosterFrame(frame: RosterFrame): void {
    this.adopt(rosterFromFrame(frame), frame.revision);
  }

  /**
   * THE adoption point.
   *
   * REVISION GATE. The daemon retains the roster and rebroadcasts it, so a
   * reconnect replays a snapshot that may reorder against what this page
   * already holds; a roster older than the held one is stale by definition
   * and is dropped. An EQUAL revision is adopted, not dropped: a redelivery
   * of the held revision asserts the same picture, so re-adopting it is a
   * no-op repaint while dropping it would strand a page whose publisher has
   * not moved. A drop is always logged with both revisions, never silent.
   *
   * THE REVEAL is the gui's other half arriving: while the rail is hidden it
   * sits out of the flex row and the feed fills the frame, so flipping it
   * visible narrows the feed and reflows it. The boot render already parked
   * the feed at its tail, and that reflow strands the park a rail-width above
   * the bottom — the "sidebar pops in and the output is no longer at the
   * newest message" symptom. So on the reveal (and only then, since later
   * adoptions leave the layout untouched) the feed's pin is read BEFORE the
   * reflow and re-parked AFTER, forcing the intended sequencing: output +
   * sidebar rendered, then scroll to the bottom.
   */
  private adopt(roster: WorkspaceRoster, revision: number): void {
    // The frame is the only ingress now, but the diagnostics keep naming the
    // path: an operator reading the log should not have to know which release
    // retired the hook to tell what painted the rail.
    const path = "frame";
    const held = this.rosterRevision;
    if (held !== null && revision < held) {
      log(
        "warn",
        `workspace roster dropped: revision ${revision} is older than the held revision ${held} ` +
          `(path=${path})`,
        {
          operation: "sidebar.roster-stale-dropped",
          context: { revision, held_revision: held, path },
        },
      );
      return;
    }
    const revealing = this.mount.hidden;
    const wasPinned = revealing && this.isPinned();
    this.roster = roster;
    this.rosterRevision = revision;
    const rows = rosterRows(roster);
    const current = rows.find((row) => row.current);
    log(
      "info",
      `workspace roster adopted path=${path} revision=${revision} rows=${rows.length} ` +
        `current_dir=${current?.dir ?? "none"} current_status=${current?.status ?? "none"} ` +
        `view=${roster.view} nav_dir=${roster.navDir ?? "none"} revealing=${revealing}`,
      {
        operation: "sidebar.roster-adopted",
        context: {
          path,
          revision,
          rows: rows.length,
          current_dir: current?.dir ?? "",
          current_status: current?.status ?? "",
          view: roster.view,
          nav_dir: roster.navDir ?? "",
          revealing,
        },
      },
    );
    this.mount.hidden = false;
    this.render();
    if (wasPinned) this.parkFeed();
  }

  private render(): void {
    if (this.roster === null) return;
    this.mount.innerHTML = sidebarHtml(
      rosterWithCurrentStatus(this.roster, this.authoritativeCurrentStatus),
      this.openDirs,
      this.now(),
      this.errorNote,
      this.monitoring,
      this.mergeStatus,
    );
  }

  /**
   * Replace only the current row's Emacs-carried status copy. Emacs remains
   * the authority for membership and every non-current row, while the footer
   * and current sidebar dot now consume the same webapp WorkspaceState lease.
   */
  setAuthoritativeCurrentStatus(status: WorkspaceStatus): void {
    const previous = this.authoritativeCurrentStatus;
    if (previous === status) return;
    this.authoritativeCurrentStatus = status;
    log("info", "current sidebar status authority changed", {
      operation: "sidebar.current-status-changed",
      context: { previous: previous ?? "none", next: status },
    });
    this.render();
  }

  /**
   * Adopt the session's monitoring gate (FeedRenderer.isMonitoring) and
   * repaint when it moved: the CURRENT row's quiescent dot breathes amber
   * while this session idles with live background async — the sidebar
   * home of the signal the topbar `monitoring…` datapoint used to carry.
   * Session-local chrome layered over the roster, so every other row
   * still shows exactly what Emacs last asserted.
   */
  /**
   * Adopt the current row's structured merge status and repaint when it moved.
   *
   * Session-local chrome layered over the Emacs roster, exactly like the
   * monitoring gate: the revisioned `WorkspaceState` it comes from describes
   * only THIS session, so every other row keeps what Emacs asserted.
   *
   * The comparison is on the log identity (phase, run, refresh stamp) rather
   * than object identity: a merge ticks once per landed commit, and a repaint
   * per arriving frame regardless of whether anything moved is the churn the
   * rail's other setters exist to avoid.
   */
  setMergeStatus(status: MergeStatus | null): void {
    const previous = mergeStatusLogValue(this.mergeStatus);
    const next = mergeStatusLogValue(status);
    if (previous === next) return;
    this.mergeStatus = status;
    log("info", "current row merge status changed", {
      operation: "sidebar.merge-status-changed",
      context: { previous, next },
    });
    this.render();
  }

  setMonitoring(on: boolean): void {
    if (on === this.monitoring) return;
    this.monitoring = on;
    this.render();
  }

  /**
   * Redraw the rail from the roster it already holds (F5).
   *
   * A hidden webview has no animation frames, so the rail on screen is the
   * frame from before it was hidden while the roster behind it has moved on.
   * The repaint-on-show pass calls this so the rail shows the CURRENT roster
   * rather than the stale frame — the same correction the feed and the footer
   * get, applied to the one surface whose state is pushed rather than
   * streamed. A no-op before the first push, which has nothing to draw.
   */
  repaint(): void {
    this.render();
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

  /** The task id an ancestor element addresses, or throw — a task control
   * without its id means the render and the handler disagree. */
  private taskId(el: Element): string {
    const id = el.getAttribute("data-task-id");
    if (id === null) throw new Error("workspace sidebar: task control without an id");
    return id;
  }

  private onClick(target: HTMLElement): void {
    // View selector + task controls sit in the header, outside any row, so
    // they are classified first and each POSTs its command to Emacs — the
    // roster (view, task list, membership) is Emacs-owned, never mutated
    // optimistically here.
    const viewBtn = target.closest("[data-set-view]");
    if (viewBtn) {
      const view = viewBtn.getAttribute("data-set-view");
      if (view !== "repository" && view !== "task") {
        throw new Error(`workspace sidebar: bad view ${JSON.stringify(view)}`);
      }
      this.post([{ type: "set-view", view }]);
      return;
    }
    if (target.closest("[data-add-task]")) {
      this.post([{ type: "task-create" }]);
      return;
    }
    const check = target.closest("[data-task-check]");
    if (check) {
      this.post([{ type: "task-toggle-done", id: this.taskId(check) }]);
      return;
    }
    const add = target.closest("[data-task-add]");
    if (add) {
      this.post([{ type: "task-add-workspace", id: this.taskId(add) }]);
      return;
    }
    const openTask = target.closest("[data-task-open]");
    if (openTask) {
      this.post([{ type: "task-open", id: this.taskId(openTask) }]);
      return;
    }
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
        log("error", `workspace command failed: ${String(err)}`, { operation: "sidebar.workspace-command-failed", context: { cause: err } });
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

/** Flatten every row, including nested workspace families, for diagnostics. */
function rosterRows(roster: WorkspaceRoster): WorkspaceRow[] {
  const out: WorkspaceRow[] = [];
  const visit = (row: WorkspaceRow): void => {
    out.push(row);
    row.children.forEach(visit);
  };
  const groups = roster.view === "task" ? roster.tasks : roster.repos;
  groups.forEach((group) => group.rows.forEach(visit));
  roster.recentlyMerged?.rows.forEach(visit);
  return out;
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
