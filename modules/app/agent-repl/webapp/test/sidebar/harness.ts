/**
 * The rail's test harness: an AppContext over a scripted daemon, a fake
 * ticker, and roster fixtures built with `create` from the generated code.
 *
 * Not a test file — the shared arrangement every sidebar suite reuses, so no
 * suite builds its own idea of what a roster looks like.
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport, type ServiceImpl } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { UpdateSidebarViewResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import { WatchWorkspaceRosterResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_workspace_roster_pb";
import {
  RosterRowSchema,
  RosterRowDetailSchema,
  RosterRowWhenSchema,
  RosterMergedSectionSchema,
  RosterRepoSectionSchema,
  RosterTaskSectionSchema,
  WorkspaceRosterSchema,
  type RosterMergedSection,
  type RosterRepoSection,
  type RosterRow,
  type RosterTaskSection,
  type WorkspaceRoster,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { Ticker } from "../../src/clock.js";
import type { FailureSink } from "../../src/failure/sink.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "../rpc/app-context.js";
import { AttentionRegistry, type BlinkTimers } from "../../src/sidebar/attention.js";
import type { Grouping, SidebarContext } from "../../src/sidebar/context.js";
import { createSidebarView, type SidebarView } from "../../src/sidebar/view.js";
import { createDropdowns } from "../../src/sidebar/dropdowns.js";

/** The webview's own workspace. */
export const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-self", dir: "/w/self" });

/** A fixed "now", so every age in a fixture is exact. */
export const NOW = 1_700_000_000_000;

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

/** A SidebarContext over CTX, with a fresh view and no ask in flight. */
export function sidebarContext(
  ctx: AppContext = appContext(),
  timers: BlinkTimers = fakeTimers(),
  view: SidebarView = createSidebarView(),
  openDetails: Set<string> = new Set(),
): SidebarContext & { disposers: Array<() => void> } {
  const disposers: Array<() => void> = [];
  return {
    ctx,
    view,
    openDetails,
    dropdowns: createDropdowns(),
    attention: new AttentionRegistry(timers),
    tasks: [],
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
  /** The durable last-selected instant, epoch ms; omitted = never selected. */
  lastSelectedAtMs?: bigint;
  priority?: string;
  /** The when-column's ARM; omitted leaves the column empty. */
  when?: MessageInitShape<typeof RosterRowWhenSchema>["shown"];
  detail?: MessageInitShape<typeof RosterRowDetailSchema>;
  children?: RosterRow[];
}): RosterRow {
  return create(RosterRowSchema, {
    workspace: { workspace: { id: init.id, dir: init.dir ?? `/w/${init.id}` } },
    name: { text: init.name ?? init.id },
    status: init.status ?? { case: "ready", value: {} },
    current: { current: init.current ?? false },
    closed: { closed: init.closed ?? false },
    when: { shown: init.when },
    detail: init.detail ?? {},
    children: init.children ?? [],
    ...(init.attention === true ? { attention: {} } : {}),
    ...(init.viewed === true ? { viewed: {} } : {}),
    ...(init.reviving === true ? { reviving: {} } : {}),
    ...(init.lastSelectedAtMs === undefined ? {} : { lastSelected: { atMs: init.lastSelectedAtMs } }),
    ...(init.priority === undefined ? {} : { priority: { label: init.priority } }),
  });
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
}): RosterRepoSection {
  return create(RosterRepoSectionSchema, {
    key: { repository: { id: init.id, dir: `/repo/${init.id}` } },
    header: { label: { text: init.label ?? init.id }, count: { workspaces: init.count ?? init.rows?.length ?? 0 } },
    rows: { rows: init.rows ?? [] },
    fold: init.collapsed === true ? { case: "collapsed", value: {} } : { case: "expanded", value: {} },
  });
}

/** One task section. */
export function taskSection(init: {
  id: string;
  label?: string;
  done?: boolean;
  rows?: RosterRow[];
  /** The daemon-held fold; expanded unless said, as a task never folded is. */
  collapsed?: boolean;
}): RosterTaskSection {
  return create(RosterTaskSectionSchema, {
    key: { taskId: init.id },
    header: { label: { text: init.label ?? init.id }, done: { done: init.done ?? false } },
    rows: { rows: init.rows ?? [] },
    fold: init.collapsed === true ? { case: "collapsed", value: {} } : { case: "expanded", value: {} },
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
  return create(RosterMergedSectionSchema, {
    header: { label: { text: "Recently Merged" }, count: { workspaces: count } },
    rows: { rows },
    fold: collapsed ? { case: "collapsed", value: {} } : { case: "expanded", value: {} },
  });
}

/** A whole roster, with every required box present. */
export function roster(init: {
  repos?: RosterRepoSection[];
  tasks?: RosterTaskSection[];
  merged?: RosterMergedSection;
  current?: string;
  /** The daemon-held grouping every page shows; the repository's unless said. */
  shown?: Grouping;
} = {}): WorkspaceRoster {
  return create(WorkspaceRosterSchema, {
    repository: { sections: init.repos ?? [] },
    task: { sections: init.tasks ?? [] },
    recentlyMerged: init.merged ?? mergedSection(),
    shown:
      init.shown === "task"
        ? { case: "shownTask", value: {} }
        : { case: "shownRepository", value: {} },
    ...(init.current === undefined
      ? {}
      : { current: { workspace: { id: init.current, dir: `/w/${init.current}` } } }),
  });
}
