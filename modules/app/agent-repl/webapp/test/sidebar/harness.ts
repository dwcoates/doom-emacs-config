/**
 * The rail's test harness: an AppContext over a scripted daemon, a fake
 * ticker, and roster fixtures built with `create` from the generated code.
 *
 * Not a test file — the shared arrangement every sidebar suite reuses, so no
 * suite builds its own idea of what a roster looks like.
 */
import { createDeploysFold, type DeploysFold } from "../../src/sidebar/deploys-fold.js";
import type { MergedGroupPages, MergedPages } from "../../src/sidebar/context.js";
import { MERGED_PAGE_SIZE } from "../../src/sidebar/merged-pages.js";
import type { RosterStreamName, RosterStreams } from "../../src/sidebar/roster-streams.js";
import { clone, create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport, type ServiceImpl } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { UpdateSidebarViewResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import { WatchWorkspaceRosterResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_workspace_roster_pb";
import {
  ListMergedWorkspacesResponseSchema,
  type ListMergedWorkspacesResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_list_merged_workspaces_pb";
import {
  RosterRowSchema,
  RosterRowDetailSchema,
  RosterRowWhenSchema,
  RosterHiddenSectionSchema,
  RosterOpenSectionSchema,
  RosterMergedRepoGroupSchema,
  RosterMergedRowSchema,
  RosterMergedSectionSchema,
  RosterMergeQueueRepoGroupSchema,
  RosterMergeQueueSectionSchema,
  RosterPushSchema,
  RosterClosedRowsSchema,
  RosterRepoEntrySchema,
  RosterMergeQueueActiveClockSchema,
  RosterMergeQueueHeadClockSchema,
  RosterMergeQueueHeadSchema,
  RosterMergeQueueSubjectSchema,
  RosterMergeQueueWaitingClockSchema,
  RosterMergeQueueWaitingSchema,
  RosterMergeQueueFixingTestsSchema,
  RosterMergeQueueResolvingConflictsSchema,
  RosterMergeQueueAwaitingFixingTestsSchema,
  RosterRepoMergeQueueSchema,
  RosterRepoSectionSchema,
  RosterSubsectionSchema,
  WorkspaceRosterSchema,
  type RosterHiddenSection,
  type RosterMergeQueueHead,
  type RosterMergeQueueHeadClock,
  type RosterMergeQueueSubject,
  type RosterMergeQueueWaiting,
  type RosterOpenSection,
  type RosterMergedRepoGroup,
  type RosterMergedRow,
  type RosterMergedSection,
  type RosterPush,
  type RosterMergeQueueRepoGroup,
  type RosterMergeQueueSection,
  type RosterRepoSection,
  type RosterRow,
  type WorkspaceRoster,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { hiddenRow, numberTabs, openRow, rowEntries, sectionTabRegions, withFamily } from "../roster-families.js";

export { hiddenRow, openRow };
import type { Ticker } from "../../src/clock.js";
import { createDrawnStatusLog } from "../../src/drawn-status.js";
import type { FailureSink } from "../../src/failure/sink.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "../rpc/app-context.js";
import { AttentionRegistry } from "../../src/sidebar/attention.js";
import type { Timers } from "../../src/timers.js";
import type { RailContext, SidebarContext } from "../../src/sidebar/context.js";
import { createSidebarView, type DrawnView, type SidebarView } from "../../src/sidebar/view.js";
import { createDropdowns } from "../../src/sidebar/dropdowns.js";
import { createRowHover } from "../../src/sidebar/row-hover.js";
import { createRowMenus } from "../../src/sidebar/row-menu.js";
import { createArrangeHold } from "../../src/sidebar/arrange.js";
import { createSelectionFade } from "../../src/sidebar/selection-fade.js";
import { createFoldFade } from "../../src/sidebar/fold-fade.js";
import { rosterApps, type TestThread } from "../roster-apps.js";
import type { PriorityArm } from "../../src/name-prefix.js";

/** The webview's own workspace. */
export const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-self", dir: "/w/self" });

/** A fixed "now", so every age in a fixture is exact. */
export const NOW = 1_700_000_000_000;

/**
 * Local midnight at the start of NOW's day, and of the day after: a merge
 * case pinned to either sits on a known side of a date line whatever zone
 * the suite runs in.
 */
export function startOfToday(): number {
  const midnight = new Date(NOW);
  midnight.setHours(0, 0, 0, 0);
  return midnight.getTime();
}
export function startOfTomorrow(): number {
  const midnight = new Date(startOfToday());
  midnight.setDate(midnight.getDate() + 1);
  return midnight.getTime();
}

/** A sink that swallows: no sidebar test asserts on client-local failures. */
export const SINK: FailureSink = { report: () => undefined, retract: () => undefined };

/** A ticker under the test's control, with the subscriber count exposed. */
export function fakeTicker(now: number = NOW): Ticker & {
  tick(at: number): void;
  subscribers(): number;
} {
  const listeners = new Set<(nowMs: number) => void>();
  return {
    now: () => now,
    subscribe(fn) {
      listeners.add(fn);
      return () => listeners.delete(fn);
    },
    tick(at: number): void {
      for (const fn of [...listeners]) fn(at);
    },
    subscribers: () => listeners.size,
  };
}

/**
 * Give EL a scroll offset that clamps between 0 and MAX, as the engine
 * clamps it: jsdom keeps no scroll offset of its own.
 */
export function scrollable<T extends HTMLElement>(el: T, max = 1000): T {
  let top = 0;
  Object.defineProperty(el, "scrollTop", {
    configurable: true,
    get: () => top,
    set: (v: number) => {
      top = Math.max(0, Math.min(v, max));
    },
  });
  return el;
}

/** Timers a suite advances by hand, for the blink cadence. */
export function fakeTimers(): Timers & { run(): boolean } {
  let next = 1;
  const pending = new Map<number, () => void>();
  return {
    setTimeout(fn) {
      const handle = next++;
      pending.set(handle, fn);
      return handle;
    },
    clearTimeout(handle) {
      pending.delete(handle);
    },
    /** Fire every timer armed right now. Answers whether any was. */
    run(): boolean {
      const due = [...pending.entries()];
      pending.clear();
      for (const [, fn] of due) fn();
      return due.length > 0;
    },
  };
}

/**
 * An AppContext whose daemon is IMPL. UpdateSidebarView answers success unless
 * IMPL says otherwise: every hover and fold asks it, so a suite about
 * something else need not script it.
 */
export function appContext(impl: Partial<ServiceImpl<typeof AgentRepl>> = {}, ticker?: Ticker): AppContext {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      updateSidebarView: () => create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } }),
      ...impl,
    });
  });
  return testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker: ticker ?? fakeTicker(),
    failures: SINK,
    composerEnabled: false,
  });
}

/**
 * A context over CTX, with a fresh view and no ask in flight, that serves both
 * as a rail's (`RailContext`, what the roster drawer takes) and as one drawn
 * section's (`SidebarContext`, what a section or row draws through). As the
 * latter, its copies of the view are one scope's and its teardowns are kept
 * in `disposers` for the test to read.
 */
export function sidebarContext(
  ctx: AppContext = appContext(),
  timers: Timers = fakeTimers(),
  view: SidebarView = createSidebarView(),
  openDetails: Set<string> = new Set(),
  deploysFold: DeploysFold = createDeploysFold(document, () => undefined),
  mergedPages: MergedPages = allMergesLoaded(),
): SidebarContext & RailContext & { view: DrawnView & SidebarView; attention: AttentionRegistry; disposers: Array<() => void> } {
  const disposers: Array<() => void> = [];
  const dropdowns = createDropdowns();
  const own = view.scope();
  return {
    ctx,
    view: {
      track: (key, wire, paint) => own.track(key, wire, paint),
      ask: (key, value) => view.ask(key, value),
      abandon: (key, token) => view.abandon(key, token),
      scope: () => view.scope(),
    },
    openDetails,
    drawnStatus: createDrawnStatusLog("sidebar row", "sidebar.drawn-status"),
    dropdowns,
    rowHover: createRowHover(dropdowns, openDetails),
    rowMenus: createRowMenus(dropdowns),
    attention: new AttentionRegistry(timers),
    selectionFade: createSelectionFade(document),
    foldFade: createFoldFade(document),
    shownAhead: () => null,
    arrange: {
      hold: createArrangeHold(() => undefined),
      layer: document.createElement("div"),
      status: document.createElement("div"),
    },
    deploysFold,
    mergedPages,
    disposers,
    onDispose: (fn) => {
      disposers.push(fn);
    },
  };
}

/**
 * The push carrying STREAMS' snapshots: every stream unless ONLY names which,
 * at STREAMS' revision.
 */
export function pushOf(streams: RosterStreams, only?: readonly RosterStreamName[]): RosterPush {
  const carries = (name: RosterStreamName): boolean => only === undefined || only.includes(name);
  return create(RosterPushSchema, {
    revision: streams.revision,
    ...(carries("workspaces") ? { workspaces: streams.workspaces } : {}),
    ...(carries("merge_queue") ? { mergeQueue: streams.mergeQueue } : {}),
    ...(carries("recently_merged") ? { recentlyMerged: streams.recentlyMerged } : {}),
    ...(carries("closed") ? { closed: streams.closed } : {}),
  });
}

/**
 * A response stream that yields PUSHES (a `RosterStreams` is the push
 * carrying every stream of it) and then never concludes.
 */
export function rosterStream(
  pushes: ReadonlyArray<RosterStreams | RosterPush>,
): Partial<ServiceImpl<typeof AgentRepl>> {
  return {
    watchWorkspaceRoster: async function* () {
      for (const push of pushes) {
        const value = "$typeName" in push ? push : pushOf(push);
        yield create(WatchWorkspaceRosterResponseSchema, { push: { case: "streams", value } });
      }
      // A standing stream never ends on its own.
      await new Promise<never>(() => undefined);
    },
  };
}

/** A minimal row: a workspace, a name, a status, and the required boxes. */
export function row(init: {
  id: string;
  name?: string;
  /** The emoji-ack glyph the name's prefix wears; omitted = none. */
  glyph?: string;
  dir?: string;
  status?: MessageInitShape<typeof RosterRowSchema>["status"];
  current?: boolean;
  closed?: boolean;
  attention?: boolean;
  /** The VIEWED marker: present means the daemon holds this row PARTIAL. */
  viewed?: boolean;
  /** The UNREAD-TURN-END marker's turn end; omitted = no marker. */
  idleAsyncUnread?: MessageInitShape<typeof RosterRowSchema>["idleAsyncUnread"];
  /** The REVIVING marker: present while the daemon revives this workspace. */
  reviving?: boolean;
  /** The ONE-SHOT mark's outcome; omitted = the workspace is no one-shot. */
  oneShot?: boolean;
  opening?: boolean;
  /** The ASYNC-MERGING marker: present while the workspace's own branch merges async. */
  asyncMerging?: boolean;
  /** The MERGE-PAUSED marker: present while the queue holding the row's merge is paused. */
  mergePaused?: boolean;
  mergeTurn?: boolean;
  /** The OPEN-OWNED-PULL-REQUEST marker: present while the workspace owns a pull request still open. */
  openOwnedPullRequest?: boolean;
  /** The context-heat marker's window fill; absent draws no marker. */
  contextHeat?: number;
  /** The durable last-selected instant, epoch ms; omitted = never selected. */
  lastSelectedAtMs?: bigint;
  /** The priority icon the name's prefix wears; omitted = unprioritized. */
  priority?: PriorityArm;
  /** The when-column's ARM; omitted = no turn running, as a live row with no turn draws. */
  when?: MessageInitShape<typeof RosterRowWhenSchema>["shown"];
  detail?: MessageInitShape<typeof RosterRowDetailSchema>;
  children?: RosterRow[];
}): RosterRow {
  const built = create(RosterRowSchema, {
    workspace: { workspace: { id: init.id, dir: init.dir ?? `/w/${init.id}` } },
    name: {
      text: init.name ?? init.id,
      ...(init.priority === undefined && init.glyph === undefined
        ? {}
        : {
            prefix: {
              ...(init.priority === undefined ? {} : { priority: { level: { case: init.priority, value: {} } } }),
              ...(init.glyph === undefined ? {} : { emojiAck: { text: init.glyph } }),
            },
          }),
    },
    status: init.status ?? { case: "ready", value: {} },
    current: { current: init.current ?? false },
    closed: { closed: init.closed ?? false },
    when: { shown: init.when ?? { case: "noTurnRunning", value: {} } },
    detail: init.detail ?? {},
    ...(init.attention === true ? { attention: {} } : {}),
    ...(init.viewed === true ? { viewed: {} } : {}),
    ...(init.idleAsyncUnread === undefined ? {} : { idleAsyncUnread: init.idleAsyncUnread }),
    ...(init.reviving === true ? { reviving: {} } : {}),
    ...(init.oneShot === true ? { oneShot: {} } : {}),
    ...(init.opening === true ? { opening: {} } : {}),
    ...(init.asyncMerging === true ? { asyncMerging: {} } : {}),
    ...(init.mergePaused === true ? { mergePaused: {} } : {}),
    ...(init.mergeTurn === true ? { mergeTurn: {} } : {}),
    ...(init.openOwnedPullRequest === true ? { openOwnedPullRequest: {} } : {}),
    ...(init.contextHeat !== undefined ? { contextHeat: { windowFill: init.contextHeat } } : {}),
    ...(init.lastSelectedAtMs === undefined ? {} : { lastSelected: { atMs: init.lastSelectedAtMs } }),
  });
  return withFamily(built, init.children ?? []);
}

/** One repository section. */
export function repoSection(init: {
  id: string;
  label?: string;
  rows?: RosterRow[];
  /** The daemon-held fold; expanded unless said. */
  collapsed?: boolean;
  /** The daemon-resolved folded count; the row count unless said. */
  count?: number;
  /** The daemon-resolved count of rows no fold hides; the count unless said. */
  unfolded?: number;
  /** The label hover's measurement; pending unless said. */
  divergence?: MessageInitShape<typeof RosterRepoSectionSchema>["divergence"];
  /** Whether the expanded section holds the current workspace; not unless said. */
  holdsCurrent?: boolean;
  /**
   * The repository's subsections, in order; none unless said. Each stands
   * just before the row BEFORE names, or after every row unless said.
   */
  subsections?: Array<{
    id: string;
    label?: string;
    rows?: RosterRow[];
    collapsed?: boolean;
    holdsCurrent?: boolean;
    before?: string;
    /** The daemon-resolved count of rows no fold hides; every row unless said. */
    unfolded?: number;
  }>;
}): RosterRepoSection {
  const rows = init.rows ?? [];
  const entries = rowEntries(rows);
  for (const sub of init.subsections ?? []) {
    const entry = create(RosterRepoEntrySchema, {
      entry: {
        case: "subsection",
        value: {
          kind: { case: "named", value: { key: { subsectionId: sub.id } } },
          header: {
            label: { text: sub.label ?? sub.id },
            count: { workspaces: sub.rows?.length ?? 0, unfolded: sub.unfolded ?? sub.rows?.length ?? 0 },
          },
          openRows: { rows: (sub.rows ?? []).map(openRow) },
          fold: sectionFold(sub.collapsed, sub.holdsCurrent),
        },
      },
    });
    const at = sub.before === undefined ? -1 : entries.findIndex((e) => e.entry.case === "row" && e.entry.value.row?.workspace?.workspace?.id === sub.before);
    if (sub.before !== undefined && at < 0) throw new Error(`harness: no row ${sub.before} for subsection ${sub.id} to stand before`);
    entries.splice(at < 0 ? entries.length : at, 0, entry);
  }
  return create(RosterRepoSectionSchema, {
    key: { repository: { id: init.id, dir: `/repo/${init.id}` } },
    header: {
      label: { text: init.label ?? init.id },
      count: { workspaces: init.count ?? rows.length, unfolded: init.unfolded ?? init.count ?? rows.length },
    },
    fold: sectionFold(init.collapsed, init.holdsCurrent),
    divergence: init.divergence ?? { state: { case: "pending", value: {} } },
    entries,
  });
}

/**
 * A repository section's or subsection's fold: collapsed when COLLAPSED, or
 * expanded and holding the current workspace when HOLDS_CURRENT. The two
 * messages spell the fold alike, so one shape builds both.
 */
function sectionFold(
  collapsed = false,
  holdsCurrent = false,
): MessageInitShape<typeof RosterRepoSectionSchema>["fold"] & MessageInitShape<typeof RosterSubsectionSchema>["fold"] {
  if (collapsed) return { case: "collapsed", value: {} };
  return {
    case: "expanded",
    value: { collapse: holdsCurrent ? { case: "holdsCurrent", value: {} } : { case: "collapsible", value: {} } },
  };
}

/**
 * The Recently Merged section with one group holding ROWS, or none. Its
 * daemon-held fold is COLLAPSED unless said, as the rail leaves it once the
 * focus has left the page.
 */
export function mergedSection(rows: RosterRow[] = [], collapsed: boolean = true, sized: number | null = null): RosterMergedSection {
  return mergedBand(rows.length === 0 ? [] : [mergedGroup({ id: "repo-1", rows })], collapsed, sized);
}

/**
 * A band section's height arm: SIZED lines, or FITTED (null) as the daemon
 * draws a section nobody has sized.
 */
export function bandHeight(sized: number | null): { case: "fitted"; value: object } | { case: "sized"; value: { lines: number } } {
  return sized === null ? { case: "fitted", value: {} } : { case: "sized", value: { lines: sized } };
}

/**
 * The Recently Merged section carrying GROUPS' summaries, its fold and its
 * height (`bandHeight`) stated, and its set of merged workspaces named TOKEN
 * (1 unless said). Its header counts every group's merges, as the daemon
 * draws it.
 */
export function mergedBand(
  groups: RosterMergedRepoGroup[],
  collapsed: boolean = true,
  sized: number | null = null,
  token: bigint = 1n,
): RosterMergedSection {
  return create(RosterMergedSectionSchema, {
    header: {
      label: { text: "Recently Merged" },
      count: { workspaces: groups.reduce((sum, group) => sum + (group.summary?.count?.merges ?? 0), 0) },
    },
    groups,
    mergedRevision: { revision: token },
    fold: collapsed ? { case: "collapsed", value: {} } : { case: "expanded", value: {} },
    height: bandHeight(sized),
  });
}

/** The merged rows each harness group was built from: what its pages hold. */
const MERGED_ROWS = new WeakMap<RosterMergedRepoGroup, RosterMergedRow[]>();

/** The merged rows GROUP (a `mergedGroup`) was built from, most recent first. */
export function mergedRowsOf(group: RosterMergedRepoGroup): RosterMergedRow[] {
  return MERGED_ROWS.get(group) ?? [];
}

/** ROWS as pages of ten, as ListMergedWorkspaces answers them. */
export function pagesOfTen<T>(rows: readonly T[]): T[][] {
  const pages: T[][] = [];
  for (let i = 0; i < rows.length; i += MERGED_PAGE_SIZE) pages.push(rows.slice(i, i + MERGED_PAGE_SIZE));
  return pages;
}

/**
 * Merged pages where every merge of every harness group (`mergedGroup`) is
 * loaded and every page agrees with its summary: what a drawer test draws
 * through, so it reads the drawing and not the loading (merged-pages.ts has
 * its own suite).
 */
export function allMergesLoaded(): MergedPages {
  return {
    agree: () => "draw",
    loaded: (group): MergedGroupPages => {
      const rows = mergedRowsOf(group);
      return { pages: pagesOfTen(rows), more: false, rows: rows.length };
    },
    version: () => 0,
    check: () => undefined,
    foldsChanged: () => undefined,
    dispose: () => undefined,
  };
}

/**
 * A daemon answering ListMergedWorkspaces from SECTIONS' harness groups
 * (`mergedGroup`): each group's rows ten at a time, at the token of the last
 * section listing it.
 */
export function listMergedFrom(sections: readonly RosterMergedSection[]): Partial<ServiceImpl<typeof AgentRepl>> {
  return {
    listMergedWorkspaces: (request) => {
      let found: { group: RosterMergedRepoGroup; token: bigint } | null = null;
      for (const section of sections) {
        const group = section.groups.find((g) => g.key?.repository?.id === request.repository?.id);
        if (group !== undefined) found = { group, token: section.mergedRevision?.revision ?? 0n };
      }
      if (found === null) {
        return create(ListMergedWorkspacesResponseSchema, {
          result: { case: "error", value: { cause: { case: "unknownRepository", value: { repository: request.repository } } } },
        });
      }
      return mergedPageAnswer(mergedRowsOf(found.group), request.page, found.token);
    },
  };
}

/** The answer for page PAGE of ROWS, at TOKEN: `more` while rows follow it, else `last`. */
export function mergedPageAnswer(rows: readonly RosterMergedRow[], page: number, token: bigint): ListMergedWorkspacesResponse {
  const slice = rows.slice(page * MERGED_PAGE_SIZE, (page + 1) * MERGED_PAGE_SIZE);
  const omitted = Math.max(0, rows.length - (page + 1) * MERGED_PAGE_SIZE);
  return create(ListMergedWorkspacesResponseSchema, {
    result: {
      case: "success",
      value: {
        revision: { revision: token },
        extent:
          omitted > 0
            ? { case: "more", value: { rows: { rows: slice }, omitted } }
            : { case: "last", value: { rows: { rows: slice } } },
      },
    },
  });
}

/** Merged pages where nothing is loaded: a folded band's, or one whose fetch failed. */
export function noMergesLoaded(): MergedPages {
  return { ...allMergesLoaded(), loaded: () => ({ pages: [], more: false, rows: 0 }) };
}

/**
 * The Merge Queue section carrying GROUPS, none unless said, and its fold
 * stated: EXPANDED unless said, as the daemon draws a section nobody has
 * folded. Its header's count is the web app's, summed over GROUPS' queues.
 */
export function mergeQueueBand(groups: RosterMergeQueueRepoGroup[] = [], collapsed: boolean = false, sized: number | null = null): RosterMergeQueueSection {
  return create(RosterMergeQueueSectionSchema, {
    header: { label: { text: "Merge Queue" } },
    groups,
    fold: collapsed ? { case: "collapsed", value: {} } : { case: "expanded", value: {} },
    height: bandHeight(sized),
  });
}

/** One repository's group inside the Merge Queue section: its QUEUE, an empty one unless said. */
export function queueGroup(init: {
  id: string;
  label?: string;
  queue?: MessageInitShape<typeof RosterRepoMergeQueueSchema>;
}): RosterMergeQueueRepoGroup {
  return create(RosterMergeQueueRepoGroupSchema, {
    key: { repository: { id: init.id, dir: `/r/${init.id}` } },
    header: { label: { text: init.label ?? init.id } },
    queue: create(RosterRepoMergeQueueSchema, init.queue ?? {}),
  });
}

/**
 * One repository's group inside the Recently Merged section, its summary
 * built from ROWS as the daemon builds it: their count (COUNT unless said),
 * the first's instant as the latest, and every one of them as a merge of the
 * last day (RECENT unless said), with its commits. A row left on `row`'s
 * default when arm (no turn running) is stamped on the merged arm, as the
 * daemon stamps every merged row, a minute apart and most recent first. The
 * rows are what its pages hold (`mergedRowsOf`).
 */
export function mergedGroup(init: {
  id: string;
  label?: string;
  rows?: RosterRow[];
  count?: number;
  latestMs?: number;
  recent?: Array<{ atMs: number; commits?: number }>;
}): RosterMergedRepoGroup {
  const rows = (init.rows ?? []).map((r, i) => {
    if (r.when === undefined || r.when.shown.case !== "noTurnRunning") return r;
    const stamped = clone(RosterRowSchema, r);
    stamped.when = create(RosterRowWhenSchema, { shown: { case: "merged", value: { atMs: BigInt(1_000_000_000 - i * 60_000) } } });
    return stamped;
  });
  const atMs = (r: RosterRow): number => (r.when?.shown.case === "merged" ? Number(r.when.shown.value.atMs) : 0);
  const recent =
    init.recent ??
    rows.map((r) => ({ atMs: atMs(r), ...(r.detail?.commits === undefined ? {} : { commits: r.detail.commits.count }) }));
  const group = create(RosterMergedRepoGroupSchema, {
    key: { repository: { id: init.id, dir: `/r/${init.id}` } },
    header: { label: { text: init.label ?? init.id } },
    summary: {
      count: { merges: init.count ?? rows.length },
      latest: { mergedAtMs: BigInt(init.latestMs ?? (rows.length === 0 ? 1_000_000_000 : Math.max(...rows.map(atMs)))) },
      recent: recent.map((merge) => ({ mergedAtMs: BigInt(merge.atMs), ...(merge.commits === undefined ? {} : { commits: merge.commits }) })),
    },
  });
  MERGED_ROWS.set(group, rows.map((row) => create(RosterMergedRowSchema, { row })));
  return group;
}

/**
 * The hidden-workspaces section. Its daemon-held fold is EXPANDED unless
 * said, as the daemon draws a section nobody has folded; its count is the
 * rows it carries unless said.
 */
export function hiddenSection(
  rows: RosterRow[] = [],
  count: number = rows.length,
  collapsed: boolean = false,
  sized: number | null = null,
): RosterHiddenSection {
  return create(RosterHiddenSectionSchema, {
    header: { label: { text: "Hidden" }, count: { workspaces: count } },
    hiddenRows: { rows: rows.map(hiddenRow) },
    fold: collapsed ? { case: "collapsed", value: {} } : { case: "expanded", value: {} },
    height: bandHeight(sized),
  });
}

/**
 * The open-workspaces section. Its daemon-held fold is EXPANDED unless said,
 * as the daemon draws a section nobody has folded.
 */
export function openSection(collapsed: boolean = false): RosterOpenSection {
  return create(RosterOpenSectionSchema, {
    label: { text: "Open" },
    fold: collapsed ? { case: "collapsed", value: {} } : { case: "expanded", value: {} },
  });
}

/**
 * A whole roster, every stream of it as of one push (REVISION, 1 unless
 * said), with every required box present.
 */
export function roster(init: {
  revision?: bigint;
  repos?: RosterRepoSection[];
  merged?: RosterMergedSection;
  mergeQueue?: RosterMergeQueueSection;
  hidden?: RosterHiddenSection;
  open?: RosterOpenSection;
  /** The daemon-resolved open count; unless said, the sum of the sections' counts. */
  openCount?: number;
  /** The daemon-resolved open count's unfolded rows; unless said, the sum of the sections'. */
  openUnfolded?: number;
  current?: string;
  /** The daemon-resolved agent-repl version; omitted = unset. */
  version?: string;
  /**
   * Which open rows have a numbered tab, first tab first: each listed DRAWN
   * row is numbered by its place among them, and every other open row is
   * folded. Unless said, every open row of an unfolded repository is
   * numbered in the Open section's walk, as the daemon numbers it.
   */
  tabs?: Array<{ id: string; drawn?: "drawn" | "withheldCollapsed" }>;
  /** The Chat app is shown, with these threads (id and name). */
  chat?: TestThread[];
} = {}): RosterStreams {
  const repos = init.repos ?? [];
  const counted = repos.reduce((sum, section) => sum + (section.header?.count?.workspaces ?? 0), 0);
  const unfolded = repos.reduce((sum, section) => sum + (section.header?.count?.unfolded ?? 0), 0);
  numberTabs(
    repos.map(sectionTabRegions),
    init.tabs?.filter((tab) => (tab.drawn ?? "drawn") === "drawn").map((tab) => tab.id),
  );
  const workspaces: WorkspaceRoster = create(WorkspaceRosterSchema, {
    repository: { sections: repos, openCount: { workspaces: init.openCount ?? counted, unfolded: init.openUnfolded ?? init.openCount ?? unfolded } },
    open: init.open ?? openSection(),
    hidden: init.hidden ?? hiddenSection(),
    ...rosterApps(init.chat),
    ...(init.current === undefined
      ? {}
      : { current: { workspace: { id: init.current, dir: `/w/${init.current}` } } }),
    ...(init.version === undefined ? {} : { version: { text: init.version } }),
  });
  return {
    revision: init.revision ?? 1n,
    workspaces,
    mergeQueue: init.mergeQueue ?? mergeQueueBand(),
    recentlyMerged: init.merged ?? mergedSection(),
    closed: create(RosterClosedRowsSchema, {}),
  };
}

export { primaryPointer, settleOwedDragClick } from "../drag-pointer.js";

/** The drawn entries of a repository section's list, in order: a row by its id, a named subsection as [id], the one-shot subsection as <one-shot>. */
export function drawnEntries(list: Element): string[] {
  return [...list.children].map((el) =>
    el.classList.contains("sb-subsection")
      ? el.hasAttribute("data-one-shot")
        ? "<one-shot>"
        : `[${el.getAttribute("data-subsection")}]`
      : (el.getAttribute("data-roster-row") ?? "?"),
  );
}

/**
 * A merge queue subject credited to workspace ID, named NAME (ID unless
 * said), running sync unless ASYNC, revealing REVEAL when given, and standing
 * on its queue as MERGE (ID unless said).
 */
export function queueSubject(id: string, init: { name?: string; async?: boolean; requester?: string; reveal?: string; merge?: string } = {}): RosterMergeQueueSubject {
  return create(RosterMergeQueueSubjectSchema, {
    workspace: { id, dir: `/w/${id}` },
    name: init.name ?? id,
    reveal: init.reveal === undefined ? undefined : { value: init.reveal },
    mode: {
      mode: init.async === true ? { case: "async", value: { requester: { name: init.requester ?? id } } } : { case: "sync", value: {} },
    },
    merge: { id: init.merge ?? id },
  });
}

/** The clock of a merge active since SINCE_MS. */
export function activeClock(sinceMs: number) {
  return create(RosterMergeQueueActiveClockSchema, { activeSinceMs: BigInt(sinceMs) });
}

/** The clock of a merge waiting since SINCE_MS, having waited WAITED_MS before. */
export function waitingClock(sinceMs: number, waitedMs = 0) {
  return create(RosterMergeQueueWaitingClockSchema, { waitingSinceMs: BigInt(sinceMs), waitedBeforeMs: BigInt(waitedMs) });
}

/** The head's clock, active since SINCE_MS. */
export function activeHeadClock(sinceMs: number): RosterMergeQueueHeadClock {
  return create(RosterMergeQueueHeadClockSchema, { stretch: { case: "active", value: activeClock(sinceMs) } });
}

/** The head's clock, waiting since SINCE_MS, having waited WAITED_MS before. */
export function waitingHeadClock(sinceMs: number, waitedMs = 0): RosterMergeQueueHeadClock {
  return create(RosterMergeQueueHeadClockSchema, { stretch: { case: "waiting", value: waitingClock(sinceMs, waitedMs) } });
}

/** A merge queue head being worked on, credited to ID, with STAGES, on CLOCK (active since NOW by default). */
export function mergingHead(
  id: string,
  init: {
    stages?: MessageInitShape<typeof RosterMergeQueueHeadSchema>["stages"];
    subject?: RosterMergeQueueSubject;
    clock?: RosterMergeQueueHeadClock;
  } = {},
): RosterMergeQueueHead {
  return create(RosterMergeQueueHeadSchema, {
    subject: init.subject ?? queueSubject(id),
    stages: init.stages ?? [],
    clock: init.clock ?? activeHeadClock(NOW),
    standing: { case: "merging", value: {} },
  });
}

/** A merge queue head waiting to be admitted, credited to ID, on CLOCK (waiting since NOW by default). */
export function waitingHead(id: string, clock: RosterMergeQueueHeadClock = waitingHeadClock(NOW)): RosterMergeQueueHead {
  return create(RosterMergeQueueHeadSchema, { subject: queueSubject(id), clock, standing: { case: "waiting", value: {} } });
}

/** The merge holding the fixing-tests slot, active since SINCE. */
export function fixingTestsMerge(ws: string, since: number) {
  return create(RosterMergeQueueFixingTestsSchema, { subject: queueSubject(ws), clock: activeClock(since) });
}

/** A merge resolving its conflicts, active since SINCE. */
export function resolvingConflictsMerge(ws: string, since: number) {
  return create(RosterMergeQueueResolvingConflictsSchema, { subject: queueSubject(ws), clock: activeClock(since) });
}

/** A merge awaiting the fixing-tests slot, waiting since SINCE, having waited WAITED before. */
export function awaitingFixingTestsMerge(ws: string, since: number, waited = 0) {
  return create(RosterMergeQueueAwaitingFixingTestsSchema, { subject: queueSubject(ws), clock: waitingClock(since, waited) });
}

/** A merge waiting in line, credited to ID, on CLOCK (waiting since 0 by default). */
export function waitingMerge(
  id: string,
  init: { clock?: ReturnType<typeof waitingClock>; subject?: RosterMergeQueueSubject } = {},
): RosterMergeQueueWaiting {
  return create(RosterMergeQueueWaitingSchema, {
    subject: init.subject ?? queueSubject(id),
    clock: init.clock ?? waitingClock(0),
  });
}
