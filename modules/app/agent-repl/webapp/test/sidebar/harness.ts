/**
 * The rail's test harness: an AppContext over a scripted daemon, a fake
 * ticker, and roster fixtures built with `create` from the generated code.
 *
 * Not a test file — the shared arrangement every sidebar suite reuses, so no
 * suite builds its own idea of what a roster looks like.
 */
import { clone, create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport, type ServiceImpl } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { UpdateSidebarViewResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import { WatchWorkspaceRosterResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_workspace_roster_pb";
import {
  RosterRowSchema,
  RosterRowDetailSchema,
  RosterRowWhenSchema,
  RosterHiddenSectionSchema,
  RosterOpenSectionSchema,
  RosterMergedRepoGroupSchema,
  RosterMergedSectionSchema,
  RosterRepoSectionSchema,
  WorkspaceRosterSchema,
  type RosterHiddenSection,
  type RosterOpenSection,
  type RosterMergedRepoGroup,
  type RosterMergedSection,
  type RosterRepoSection,
  type RosterRow,
  type WorkspaceRoster,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { hiddenRow, numberTabs, openRow, withFamily } from "../roster-families.js";

export { hiddenRow, openRow };
import type { Ticker } from "../../src/clock.js";
import { createDrawnStatusLog } from "../../src/drawn-status.js";
import type { FailureSink } from "../../src/failure/sink.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "../rpc/app-context.js";
import { AttentionRegistry, type BlinkTimers } from "../../src/sidebar/attention.js";
import type { RailContext, SidebarContext } from "../../src/sidebar/context.js";
import { createSidebarView, type DrawnView, type SidebarView } from "../../src/sidebar/view.js";
import { createDropdowns } from "../../src/sidebar/dropdowns.js";
import { createRowHover } from "../../src/sidebar/row-hover.js";
import { createArrangeHold } from "../../src/sidebar/arrange.js";
import type { RowReveal } from "../../src/sidebar/reveal.js";
import { createSelectionFade } from "../../src/sidebar/selection-fade.js";

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

/** Timers a suite advances by hand, for the blink cadence. */
export function fakeTimers(): BlinkTimers & { run(): boolean } {
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

/** A reveal that draws every row the moment it is waited on, as if every row were in view. */
export const drawsAtOnce: RowReveal = {
  whenShown: (_target, draw) => {
    draw();
    return () => undefined;
  },
};

/** A reveal the test runs by hand: `show(target)` draws a waiting row as if it came into view. */
export function manualReveal(): RowReveal & { waiting(): Element[]; show(target: Element): void } {
  const waiting = new Map<Element, () => void>();
  return {
    whenShown: (target, draw) => {
      waiting.set(target, draw);
      return () => void waiting.delete(target);
    },
    waiting: () => [...waiting.keys()],
    show: (target) => {
      const draw = waiting.get(target);
      if (draw === undefined) throw new Error("manualReveal: nothing waits on that element");
      waiting.delete(target);
      draw();
    },
  };
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
  timers: BlinkTimers = fakeTimers(),
  view: SidebarView = createSidebarView(),
  openDetails: Set<string> = new Set(),
  reveal: RowReveal = drawsAtOnce,
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
    attention: new AttentionRegistry(timers),
    reveal,
    selectionFade: createSelectionFade(document),
    shownAhead: () => null,
    arrange: {
      hold: createArrangeHold(() => undefined),
      layer: document.createElement("div"),
      status: document.createElement("div"),
    },
    disposers,
    onDispose: (fn) => {
      disposers.push(fn);
    },
  };
}

/** A response stream that yields ROSTERS and then never concludes. */
export function rosterStream(
  rosters: readonly WorkspaceRoster[],
): Partial<ServiceImpl<typeof AgentRepl>> {
  return {
    watchWorkspaceRoster: async function* () {
      for (const roster of rosters) {
        yield create(WatchWorkspaceRosterResponseSchema, { push: { case: "roster", value: roster } });
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
  dir?: string;
  status?: MessageInitShape<typeof RosterRowSchema>["status"];
  current?: boolean;
  closed?: boolean;
  attention?: boolean;
  /** The VIEWED marker: present means the daemon holds this row PARTIAL. */
  viewed?: boolean;
  /** The REVIVING marker: present while the daemon revives this workspace. */
  reviving?: boolean;
  /** The ONE-SHOT mark's outcome; omitted = the workspace is no one-shot. */
  oneShot?: boolean;
  opening?: boolean;
  /** The ASYNC-MERGING marker: present while the workspace's own branch merges async. */
  asyncMerging?: boolean;
  /** The MERGE-PAUSED marker: present while the queue holding the row's merge is paused. */
  mergePaused?: boolean;
  /** The durable last-selected instant, epoch ms; omitted = never selected. */
  lastSelectedAtMs?: bigint;
  priority?: string;
  /** The when-column's ARM; omitted = no turn running, as a live row with no turn draws. */
  when?: MessageInitShape<typeof RosterRowWhenSchema>["shown"];
  detail?: MessageInitShape<typeof RosterRowDetailSchema>;
  children?: RosterRow[];
}): RosterRow {
  const built = create(RosterRowSchema, {
    workspace: { workspace: { id: init.id, dir: init.dir ?? `/w/${init.id}` } },
    name: { text: init.name ?? init.id },
    status: init.status ?? { case: "ready", value: {} },
    current: { current: init.current ?? false },
    closed: { closed: init.closed ?? false },
    when: { shown: init.when ?? { case: "noTurnRunning", value: {} } },
    detail: init.detail ?? {},
    ...(init.attention === true ? { attention: {} } : {}),
    ...(init.viewed === true ? { viewed: {} } : {}),
    ...(init.reviving === true ? { reviving: {} } : {}),
    ...(init.oneShot === true ? { oneShot: {} } : {}),
    ...(init.opening === true ? { opening: {} } : {}),
    ...(init.asyncMerging === true ? { asyncMerging: {} } : {}),
    ...(init.mergePaused === true ? { mergePaused: {} } : {}),
    ...(init.lastSelectedAtMs === undefined ? {} : { lastSelected: { atMs: init.lastSelectedAtMs } }),
    ...(init.priority === undefined ? {} : { priority: { label: init.priority } }),
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
  /** The label hover's measurement; pending unless said. */
  divergence?: MessageInitShape<typeof RosterRepoSectionSchema>["divergence"];
  /** The repository's subsections, in order; none unless said. */
  subsections?: Array<{ id: string; label?: string; rows?: RosterRow[] }>;
}): RosterRepoSection {
  return create(RosterRepoSectionSchema, {
    key: { repository: { id: init.id, dir: `/repo/${init.id}` } },
    header: { label: { text: init.label ?? init.id }, count: { workspaces: init.count ?? init.rows?.length ?? 0 } },
    openRows: { rows: (init.rows ?? []).map(openRow) },
    fold: init.collapsed === true ? { case: "collapsed", value: {} } : { case: "expanded", value: {} },
    divergence: init.divergence ?? { state: { case: "pending", value: {} } },
    subsections: (init.subsections ?? []).map((sub) => ({
      key: { subsectionId: sub.id },
      header: { label: { text: sub.label ?? sub.id }, count: { workspaces: sub.rows?.length ?? 0 } },
      openRows: { rows: (sub.rows ?? []).map(openRow) },
    })),
  });
}

/**
 * The recently-merged band. Its daemon-held fold is COLLAPSED unless said, as
 * the daemon draws a band nobody has unfolded.
 */
export function mergedSection(
  rows: RosterRow[] = [],
  count: number = rows.length,
  collapsed: boolean = true,
): RosterMergedSection {
  return mergedBand(rows.length === 0 ? [] : [mergedGroup({ id: "repo-1", rows })], count, collapsed);
}

/** The recently-merged band carrying GROUPS, its count and its fold stated. */
export function mergedBand(
  groups: RosterMergedRepoGroup[],
  count: number = groups.filter((g) => g.drawn.case === "shown").reduce((sum, g) => sum + (g.mergedRows?.rows.length ?? 0), 0),
  collapsed: boolean = true,
): RosterMergedSection {
  return create(RosterMergedSectionSchema, {
    header: { label: { text: "Recently Merged" }, count: { workspaces: count } },
    groups,
    fold: collapsed ? { case: "collapsed", value: {} } : { case: "expanded", value: {} },
  });
}

/**
 * One repository's group inside the recently-merged band, SHOWN unless its
 * repository is said to be collapsed. A row left on `row`'s default when arm
 * (no turn running) is stamped on the merged arm, as the daemon stamps every
 * merged row, a minute apart and most recent first.
 */
export function mergedGroup(init: { id: string; label?: string; rows?: RosterRow[]; repositoryCollapsed?: boolean }): RosterMergedRepoGroup {
  const rows = (init.rows ?? []).map((r, i) => {
    if (r.when === undefined || r.when.shown.case !== "noTurnRunning") return r;
    const stamped = clone(RosterRowSchema, r);
    stamped.when = create(RosterRowWhenSchema, { shown: { case: "merged", value: { atMs: BigInt(1_000_000_000 - i * 60_000) } } });
    return stamped;
  });
  return create(RosterMergedRepoGroupSchema, {
    key: { repository: { id: init.id, dir: `/r/${init.id}` } },
    header: { label: { text: init.label ?? init.id } },
    mergedRows: { rows: rows.map((row) => ({ row })) },
    drawn: init.repositoryCollapsed === true ? { case: "repositoryCollapsed", value: {} } : { case: "shown", value: {} },
  });
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
): RosterHiddenSection {
  return create(RosterHiddenSectionSchema, {
    header: { label: { text: "Hidden" }, count: { workspaces: count } },
    hiddenRows: { rows: rows.map(hiddenRow) },
    fold: collapsed ? { case: "collapsed", value: {} } : { case: "expanded", value: {} },
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

/** A whole roster, with every required box present. */
export function roster(init: {
  repos?: RosterRepoSection[];
  merged?: RosterMergedSection;
  hidden?: RosterHiddenSection;
  open?: RosterOpenSection;
  /** The daemon-resolved open count; unless said, the sum of the sections' counts. */
  openCount?: number;
  current?: string;
  /**
   * Which open rows have a numbered tab, first tab first: each listed DRAWN
   * row is numbered by its place among them, and every other open row is
   * folded. Unless said, every open row of an unfolded repository is
   * numbered in the Open section's walk, as the daemon numbers it.
   */
  tabs?: Array<{ id: string; drawn?: "drawn" | "withheldCollapsed" }>;
} = {}): WorkspaceRoster {
  const repos = init.repos ?? [];
  const counted = repos.reduce((sum, section) => sum + (section.header?.count?.workspaces ?? 0), 0);
  numberTabs(
    repos.map((section) => ({
      regions: [section.openRows, ...section.subsections.map((sub) => sub.openRows)],
      folded: section.fold.case === "collapsed",
    })),
    init.tabs?.filter((tab) => (tab.drawn ?? "drawn") === "drawn").map((tab) => tab.id),
  );
  return create(WorkspaceRosterSchema, {
    repository: { sections: repos, openCount: { workspaces: init.openCount ?? counted } },
    open: init.open ?? openSection(),
    recentlyMerged: init.merged ?? mergedSection(),
    hidden: init.hidden ?? hiddenSection(),
    closed: {},
    ...(init.current === undefined
      ? {}
      : { current: { workspace: { id: init.current, dir: `/w/${init.current}` } } }),
  });
}

/**
 * Dispatch a primary, left-button pointer event of TYPE at TARGET, at height
 * Y: the one pointer a drag test drives (`src/sidebar/drag.ts`).
 */
export function primaryPointer(type: string, target: EventTarget, y: number): void {
  target.dispatchEvent(new PointerEvent(type, { bubbles: true, pointerId: 1, isPrimary: true, button: 0, clientX: 10, clientY: y }));
}
