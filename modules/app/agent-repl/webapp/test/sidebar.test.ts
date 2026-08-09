// @vitest-environment jsdom
//
// The pure builders (validation, recency, HTML) need no DOM; jsdom is here
// for WorkspaceSidebar itself, whose delegation resolves clicks through
// real `closest` walks over the rendered mount.
import { afterEach, describe, expect, it, vi } from "vitest";

import type {
  RosterRepoSection,
  RosterRow as FrameRosterRow,
  RosterTaskSection,
  WorkspaceRoster as RosterFrame,
} from "../src/frontend-proto.js";
import { HostGlobal } from "../src/host.js";
import {
  COMMAND_FAILED_NOTICE,
  RECENTLY_MERGED_KEY,
  COMMAND_FAILED_NOTICE_MS,
  EXPAND_HOOK,
  NO_TASK_KEY,
  RepoGroup,
  WorkspaceRoster,
  WorkspaceRow,
  WorkspaceSidebar,
  formatRecency,
  installWorkspaceExpandHook,
  mergeRowHtml,
  rosterFromFrame,
  sidebarHtml,
  statusDotHtml,
  taskSectionHtml,
  workspaceStatusFromRenderState,
} from "../src/sidebar.js";
import type { MergeStatus } from "../src/state-adapter.js";
import css from "../src/styles.css?raw";
import { ForwardingLogger, setLogger } from "../src/wslog.js";

const NOW_MS = 1_700_000_000_000;
const NOW_S = NOW_MS / 1000;

/** A roster row, defaulted to a quiet idle workspace with no family. */
function row(over: Partial<WorkspaceRow> = {}): WorkspaceRow {
  return {
    name: "ws",
    dir: "/tmp/ws",
    status: "ready",
    closed: false,
    current: false,
    lastViewedAt: null,
    mergedAt: null,
    branch: null,
    parentBranch: null,
    summary: null,
    children: [],
    ...over,
  };
}

function group(over: Partial<RepoGroup> = {}): RepoGroup {
  return { key: "doom", label: "doom", folded: false, done: false, rows: [row()], ...over };
}

function roster(over: Partial<WorkspaceRoster> = {}): WorkspaceRoster {
  return {
    view: "repository",
    repos: [group()],
    tasks: [],
    recentlyMerged: null,
    navDir: null,
    ...over,
  };
}

/** Render with no panels open, at the fixed clock, with no error note. */
function html(r: WorkspaceRoster, open: ReadonlySet<string> = new Set()): string {
  return sidebarHtml(r, open, NOW_MS, null);
}

// --- frame builders: the rail's ONLY ingress ---------------------------------
//
// The roster reaches WorkspaceSidebar solely as a decoded
// `frontend.v1.WorkspaceRoster`, so every sidebar-level test drives it through
// a frame. The wire spellings are deliberately literal here — 0 for "never",
// "" for "unknown" — so a test that cares about a stamp or a branch has to say
// so, and the defaults exercise the sentinel mapping on every other test.

/** A frame row, defaulted to a quiet idle workspace with no family. */
function wireRow(over: Partial<FrameRosterRow> = {}): FrameRosterRow {
  return {
    dir: "/tmp/ws",
    name: "ws",
    status: { case: "ready" },
    current: false,
    children: [],
    lastViewedAtMs: 0,
    mergedAtMs: 0,
    branch: "",
    parentBranch: "",
    summary: "",
    closed: false,
    ...over,
  };
}

/** A repository section, defaulted to the one "doom" repo carrying one row. */
function wireRepo(over: Partial<RosterRepoSection> = {}): RosterRepoSection {
  return { repoKey: "doom", label: "doom", folded: false, rows: [wireRow()], ...over };
}

/** A task section, defaulted to an open task carrying no rows. */
function wireTask(over: Partial<RosterTaskSection> = {}): RosterTaskSection {
  return { taskId: "t1", title: "T", done: false, rows: [], ...over };
}

/**
 * A decoded roster frame in the repository grouping.
 *
 * Revision 0 by default so successive pushes in one test adopt through the
 * gate's equal-revision rule rather than needing a hand-cranked counter; the
 * gate's own tests set revisions explicitly.
 */
function wireFrame(over: Partial<RosterFrame> = {}): RosterFrame {
  return {
    revision: 0,
    bootId: "boot-a",
    view: { case: "repository", value: { sections: [wireRepo()] } },
    recentlyMerged: { rows: [], folded: false, label: "" },
    currentDir: "",
    navDir: "",
    ...over,
  };
}

/** Adopt a frame through the sole ingress. */
function push(sidebar: WorkspaceSidebar, over: Partial<RosterFrame> = {}): void {
  sidebar.adoptRosterFrame(wireFrame(over));
}

/** Adopt a repository frame whose single "doom" section carries ROWS. */
function pushRows(sidebar: WorkspaceSidebar, rows: FrameRosterRow[]): void {
  push(sidebar, { view: { case: "repository", value: { sections: [wireRepo({ rows })] } } });
}

/** Adopt a task-grouping frame carrying SECTIONS. */
function pushTasks(sidebar: WorkspaceSidebar, sections: RosterTaskSection[]): void {
  push(sidebar, { view: { case: "task", value: { sections } } });
}

describe("the session monitoring overlay", () => {
  it("breathes only the current row's dot, leaving the roster's other rows alone", () => {
    // Arrange — two idle rows, one current.
    const r = roster({
      repos: [
        group({
          rows: [
            row({ name: "me", dir: "/tmp/me", current: true }),
            row({ name: "other", dir: "/tmp/other" }),
          ],
        }),
      ],
    });
    // Act
    const out = sidebarHtml(r, new Set(), NOW_MS, null, true);
    // Assert — exactly one overlay dot, on the current row.
    expect(out.match(/st-monitoring/g)?.length).toBe(1);
    expect(out).toContain(`data-row-dir="/tmp/me"`);
  });

  it("paints no overlay anywhere while the session is not monitoring", () => {
    // Arrange
    const r = roster({ repos: [group({ rows: [row({ current: true })] })] });
    // Act / Assert
    expect(sidebarHtml(r, new Set(), NOW_MS, null, false)).not.toContain("st-monitoring");
  });
});

describe("formatRecency", () => {
  it("renders nothing for a workspace never viewed", () => {
    // Arrange + Act + Assert
    expect(formatRecency(null, NOW_MS)).toBe("");
  });

  it("reads a stamp under a minute old as now", () => {
    // Arrange + Act + Assert
    expect(formatRecency(NOW_S - 59, NOW_MS)).toBe("now");
  });

  it("truncates a minutes-old stamp to whole minutes", () => {
    // Arrange + Act + Assert
    expect(formatRecency(NOW_S - 5 * 60 - 30, NOW_MS)).toBe("5m");
  });

  it("truncates an hours-old stamp to whole hours", () => {
    // Arrange + Act + Assert
    expect(formatRecency(NOW_S - 3 * 3600 - 1200, NOW_MS)).toBe("3h");
  });

  it("truncates a days-old stamp to whole days", () => {
    // Arrange + Act + Assert
    expect(formatRecency(NOW_S - 2 * 86400 - 3600, NOW_MS)).toBe("2d");
  });

  it("floors a stamp ahead of the reader's clock to now", () => {
    // Arrange + Act + Assert
    expect(formatRecency(NOW_S + 90, NOW_MS)).toBe("now");
  });
});

describe("statusDotHtml", () => {
  it("keys a thinking dot to its breathing-red class", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("thinking")).toContain(`class="st st-thinking"`);
  });

  it("keys an init dot to its own class (styled blue, unlike thinking)", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("init")).toContain(`class="st st-init"`);
  });

  it("keys a permission dot to its breathing-yellow class", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("permission")).toContain(`class="st st-permission"`);
  });

  it("keys a done dot to its solid-green class", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("done")).toContain(`class="st st-done"`);
  });

  it("keys a ready dot to its green class", () => {
    // Arrange + Act + Assert — a ready workspace is as available as a done one.
    expect(statusDotHtml("ready")).toContain(`class="st st-ready"`);
  });

  it("keys an idle-async dot to its yellow class", () => {
    // Arrange + Act + Assert — no foreground turn, live detached work.
    expect(statusDotHtml("idle-async")).toContain(`class="st st-idle-async"`);
  });

  it("keys a vendor-blocked dot to its purple class", () => {
    // Arrange + Act + Assert — blocked until a human or the vendor acts.
    expect(statusDotHtml("vendor-blocked")).toContain(`class="st st-vendor-blocked"`);
  });

  it("keys a start-failed dot to the compromised-route class", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("start-failed")).toContain(`class="st st-start-failed"`);
  });

  it("keys a degraded dot to the compromised-route class", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("degraded")).toContain(`class="st st-degraded"`);
  });

  it("keys a dead dot to its grey class", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("dead")).toContain(`class="st st-dead"`);
  });

  it("keys a none dot to the invisible placeholder class", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("none")).toContain(`class="st st-none"`);
  });

  it("keys an inactive dot to its own question-mark class", () => {
    // Arrange + Act + Assert — the sidebar-but-not-tab-bar marker.
    expect(statusDotHtml("inactive")).toContain(`class="st st-inactive"`);
  });

  it("carries the question-mark glyph on an inactive dot", () => {
    // Arrange + Act + Assert — a perspective-less row shows ❓, not a disc.
    expect(statusDotHtml("inactive")).toContain(">❓<");
  });

  it("carries the recycle glyph on a merge-enqueuing dot", () => {
    // Arrange + Act + Assert — the first mark of the pipeline the glyph denotes.
    expect(statusDotHtml("merge-enqueuing")).toContain(">⟳<");
  });

  it("carries the recycle glyph on a merging dot", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("merging")).toContain(">⟳<");
  });

  it("carries the recycle glyph on a merge-queued dot", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("merge-queued")).toContain(">⟳<");
  });

  it("carries the recycle glyph on a merge-conflict dot", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("merge-conflict")).toContain(">⟳<");
  });

  it("carries the recycle glyph on a merge-failed dot", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("merge-failed")).toContain(">⟳<");
  });

  it("drops the recycle glyph from a merged dot", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("merged")).not.toContain(">⟳<");
  });

  it("keeps the recycle glyph on a merge-queued dot", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("merge-queued")).toContain(">⟳<");
  });

  it("leaves a disc status empty of the glyph", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("done")).not.toContain("⟳");
  });

  it("overlays the breathing monitoring dot on a quiescent status", () => {
    // Arrange + Act + Assert — the session-local monitoring overlay.
    expect(statusDotHtml("ready", true)).toContain(`class="st st-monitoring"`);
  });

  it("never lets the monitoring overlay outrank an active status", () => {
    // Arrange + Act + Assert — thinking's red breath is more specific.
    expect(statusDotHtml("thinking", true)).toContain(`class="st st-thinking"`);
  });

  it("spins the in-flight merge", () => {
    // Arrange + Act — the motion lives in the stylesheet, so it is pinned there.
    const spinning = css.match(/#ws-sidebar \.st-merging,[^{]*\{[^}]*animation: ws-spin/);
    // Assert
    expect(spinning).not.toBeNull();
  });

  it("spins the conflicting merge like the in-flight one", () => {
    // Arrange + Act — the conflict is the active run, so it keeps turning.
    const spinning = css.match(/#ws-sidebar \.st-merge-conflict \{[^}]*animation: ws-spin/);
    // Assert
    expect(spinning).not.toBeNull();
  });

  it("colors the conflicting merge with the error hue", () => {
    // Arrange + Act — red is the ONLY thing separating a conflict from a merge.
    const colored = css.match(/#ws-sidebar \.st-merge-conflict,\n#ws-sidebar \.st-merge-failed \{[^}]*color: var\(--err\)/);
    // Assert
    expect(colored).not.toBeNull();
  });

  it("never spins the queued merge", () => {
    // Arrange + Act + Assert — nothing is in flight for a queued workspace.
    expect(css).not.toMatch(/\.st-merge-queued[^{]*\{[^}]*animation/);
  });

  it("never spins the enqueuing merge", () => {
    // Arrange + Act + Assert
    expect(css).not.toMatch(/\.st-merge-enqueuing[^{]*\{[^}]*animation/);
  });

  it("never spins the failed merge", () => {
    // Arrange + Act + Assert — a failed run is over, so its glyph rests.
    expect(css).not.toMatch(/\.st-merge-failed[^{]*\{[^}]*animation/);
  });
});

describe("sidebarHtml", () => {
  it("greys a closed row via the gone class", () => {
    // Arrange + Act
    const out = html(roster({ repos: [group({ rows: [row({ closed: true })] })] }));
    // Assert
    expect(out).toContain(`class="ws gone"`);
  });

  it("greys a merged row like a closed one", () => {
    // Arrange + Act
    const out = html(roster({ repos: [group({ rows: [row({ status: "merged" })] })] }));
    // Assert
    expect(out).toContain(`class="ws gone"`);
  });

  it("marks the current workspace's wrapper", () => {
    // Arrange + Act
    const out = html(roster({ repos: [group({ rows: [row({ current: true })] })] }));
    // Assert
    expect(out).toContain(`class="ws current"`);
  });

  it("rings the row the keyboard cursor points at", () => {
    // Arrange + Act
    const out = html(roster({ navDir: "/tmp/ws" }));
    // Assert
    expect(out).toContain(`class="ws navsel"`);
  });

  it("leaves rows off the keyboard cursor unringed", () => {
    // Arrange + Act
    const out = html(roster({ navDir: "/somewhere/else" }));
    // Assert
    expect(out).not.toContain("navsel");
  });

  it("marks a folded repo section", () => {
    // Arrange + Act
    const out = html(roster({ repos: [group({ folded: true })] }));
    // Assert
    expect(out).toContain(`class="repo folded"`);
  });

  it("hides a folded section's rows through the stylesheet", () => {
    // Arrange + Act + Assert — the rows stay in the DOM; CSS hides them.
    expect(css).toMatch(/#ws-sidebar \.repo\.folded \.rows \{ display: none; \}/);
  });

  it("opens the detail panel for a dir in the open set", () => {
    // Arrange + Act
    const out = html(roster(), new Set(["/tmp/ws"]));
    // Assert
    expect(out).toContain(`class="ws open"`);
  });

  it("counts nested children into the header total", () => {
    // Arrange — one root with two children is three workspaces.
    const family = row({
      children: [row({ dir: "/tmp/a" }), row({ dir: "/tmp/b" })],
    });
    // Act
    const out = html(roster({ repos: [group({ rows: [family] })] }));
    // Assert
    expect(out).toContain(`<span class="sb-count">3</span>`);
  });

  it("counts nested children into the repo label too", () => {
    // Arrange
    const family = row({ children: [row({ dir: "/tmp/a" })] });
    // Act
    const out = html(roster({ repos: [group({ rows: [family] })] }));
    // Assert
    expect(out).toContain(`<span class="n">(2)</span>`);
  });

  it("nests children inside the family guide-line wrapper", () => {
    // Arrange
    const family = row({ children: [row({ dir: "/tmp/kid", name: "kid" })] });
    // Act
    const out = html(roster({ repos: [group({ rows: [family] })] }));
    // Assert
    expect(out).toMatch(/<div class="kids">[\s\S]*data-row-dir="\/tmp\/kid"/);
  });

  it("stamps a row's recency from its lastViewedAt", () => {
    // Arrange + Act
    const out = html(roster({ repos: [group({ rows: [row({ lastViewedAt: NOW_S - 300 })] })] }));
    // Assert
    expect(out).toContain(`<span class="when">5m</span>`);
  });

  it("stamps a merged row's recency from its mergedAt, not its lastViewedAt", () => {
    // Arrange
    const merged = row({ status: "merged", mergedAt: NOW_S - 7200, lastViewedAt: NOW_S - 300 });
    // Act
    const out = html(roster({ repos: [group({ rows: [merged] })] }));
    // Assert
    expect(out).toContain(`<span class="when">2h</span>`);
  });

  it("renders the recently-merged group as its own marked section", () => {
    // Arrange
    const recent = group({ key: "__recently_merged__", label: "Recently Merged" });
    // Act
    const out = html(roster({ recentlyMerged: recent }));
    // Assert
    expect(out).toContain("merged-section");
  });

  it("orders the recently-merged section after the repo sections", () => {
    // Arrange
    const recent = group({ key: "__recently_merged__", label: "Recently Merged" });
    // Act
    const out = html(roster({ repos: [group({ label: "doom" })], recentlyMerged: recent }));
    // Assert
    expect(out.indexOf("merged-section")).toBeGreaterThan(out.indexOf(">doom<"));
  });

  it("omits the recently-merged section when nothing merged recently", () => {
    // Arrange + Act
    const out = html(roster({ recentlyMerged: null }));
    // Assert
    expect(out).not.toContain("merged-section");
  });

  it("details a row's branch, parent, and dir when present", () => {
    // Arrange
    const detailed = row({ branch: "DWC/x", parentBranch: "master", summary: "fix the thing" });
    // Act
    const out = html(roster({ repos: [group({ rows: [detailed] })] }));
    // Assert
    expect(out).toContain("<dt>branch</dt><dd><code>DWC/x</code></dd>");
    expect(out).toContain("<dt>parent</dt><dd><code>master</code></dd>");
    expect(out).toContain("<dt>dir</dt><dd><code>/tmp/ws</code></dd>");
    expect(out).toContain(`<p class="summary">fix the thing</p>`);
  });

  it("omits detail pairs the roster carries as null", () => {
    // Arrange + Act — the default row has neither branch nor summary.
    const out = html(roster());
    // Assert
    expect(out).not.toContain("<dt>branch</dt>");
    expect(out).not.toContain(`class="summary"`);
  });

  it("escapes markup in a workspace name", () => {
    // Arrange + Act
    const out = html(roster({ repos: [group({ rows: [row({ name: "<img src=x>" })] })] }));
    // Assert
    expect(out).not.toContain("<img");
  });

  it("shows the transient error note as the rail's footer", () => {
    // Arrange + Act
    const out = sidebarHtml(roster(), new Set(), NOW_MS, "workspace command failed");
    // Assert
    expect(out).toContain(`<div class="sb-err" role="alert">workspace command failed</div>`);
  });

  it("puts the error note after the scrolling sections, not in the header", () => {
    // Arrange + Act
    const out = sidebarHtml(roster(), new Set(), NOW_MS, "workspace command failed");
    // Assert
    expect(out.indexOf("sb-err")).toBeGreaterThan(out.indexOf(`class="sb-scroll"`));
  });

  it("keeps the error note out of the scrolling sections", () => {
    // Arrange + Act
    const out = sidebarHtml(roster(), new Set(), NOW_MS, "workspace command failed");
    // Assert — the note follows the .sb-scroll element's close, so scrolling
    // the roster can never carry it out of view.
    expect(out).toContain(`</div>\n    <div class="sb-err"`);
  });

  it("omits the error slot while no note is pending", () => {
    // Arrange + Act + Assert
    expect(html(roster())).not.toContain("sb-err");
  });

  it("marks the active view button in the selector", () => {
    // Arrange + Act
    const out = html(roster({ view: "repository" }));
    // Assert
    expect(out).toContain(`class="sb-view-btn active" data-set-view="repository"`);
    expect(out).toContain(`class="sb-view-btn" data-set-view="task"`);
  });

  it("titles the header Workspaces in the repository view", () => {
    expect(html(roster())).toContain(`<span class="sb-title">Workspaces</span>`);
  });

  it("titles the header Tasks and shows the add button in the task view", () => {
    // Arrange + Act
    const out = html(roster({ view: "task", tasks: [] }));
    // Assert
    expect(out).toContain(`<span class="sb-title">Tasks</span>`);
    expect(out).toContain(`data-add-task`);
  });

  it("hides the add-task button outside the task view", () => {
    expect(html(roster())).not.toContain("data-add-task");
  });

  it("renders task sections instead of repos in the task view", () => {
    // Arrange
    const r = roster({
      view: "task",
      repos: [],
      tasks: [group({ key: "t1", label: "Ship it", rows: [row({ name: "ws" })] })],
    });
    // Act
    const out = html(r);
    // Assert
    expect(out).toContain("Ship it");
    expect(out).toContain(`data-task-open data-task-id="t1"`);
    expect(out).toContain(`data-task-check data-task-id="t1"`);
    expect(out).toContain(`data-task-add data-task-id="t1"`);
  });

  it("strikes through and checks a done task section", () => {
    // Arrange + Act
    const out = taskSectionHtml(
      group({ key: "t1", label: "Done", done: true, rows: [] }),
      null,
      new Set(),
      NOW_MS,
    );
    // Assert
    expect(out).toContain("task-section done");
    expect(out).toContain(`class="task-check done"`);
    expect(out).toContain("✓");
  });

  it("gives the No task bucket no checkbox, open, or add controls", () => {
    // Arrange + Act
    const out = taskSectionHtml(
      group({ key: NO_TASK_KEY, label: "No task", rows: [row()] }),
      null,
      new Set(),
      NOW_MS,
    );
    // Assert
    expect(out).toContain("No task");
    expect(out).not.toContain("data-task-check");
    expect(out).not.toContain("data-task-open");
    expect(out).not.toContain("data-task-add");
  });
});

/** A mounted sidebar over a recording fetch, at the fixed clock.
 * `isPinned` seeds the feed-pin probe the reveal consults; `parkCalls`
 * counts every re-park the reveal fired. */
function harness(opts: { ok?: boolean; reject?: boolean; isPinned?: () => boolean } = {}): {
  mount: HTMLElement;
  sidebar: WorkspaceSidebar;
  calls: Array<{ url: string; body: unknown }>;
  parkCalls: () => number;
} {
  const calls: Array<{ url: string; body: unknown }> = [];
  const fetchFn = ((url: string, init?: RequestInit) => {
    calls.push({ url, body: JSON.parse(String(init?.body)) });
    if (opts.reject) return Promise.reject(new Error("network down"));
    return Promise.resolve({ ok: opts.ok !== false, status: opts.ok === false ? 502 : 200 });
  }) as unknown as typeof fetch;
  const mount = document.createElement("nav");
  mount.hidden = true;
  let parks = 0;
  const sidebar = new WorkspaceSidebar(mount, {
    httpBase: "http://daemon",
    fetchFn,
    now: () => NOW_MS,
    isPinned: opts.isPinned ?? ((): boolean => false),
    parkFeed: () => {
      parks += 1;
    },
  });
  return { mount, sidebar, calls, parkCalls: () => parks };
}

function click(el: Element): void {
  el.dispatchEvent(new MouseEvent("click", { bubbles: true }));
}

/**
 * A rail carrying a task control the render could never emit — one with no
 * task id — so a click on it trips the handler's contract check. jsdom turns
 * the rethrow into a window `error` event; the listener collects it (and
 * preventDefault-s it, so the deliberate throw is not reported as an
 * unhandled test-run error) for the assertions to read back.
 */
function breachHarness(): { mount: HTMLElement; reported: string[] } {
  vi.spyOn(console, "error").mockImplementation(() => undefined);
  const reported: string[] = [];
  const onError = (e: ErrorEvent): void => {
    reported.push(String(e.error));
    e.preventDefault();
  };
  window.addEventListener("error", onError);
  cleanups.push(() => window.removeEventListener("error", onError));
  const { mount, sidebar } = harness();
  push(sidebar);
  mount.querySelector(".sb-head")!.insertAdjacentHTML("beforeend", `<span data-task-check></span>`);
  return { mount, reported };
}

/** Teardown registered by a test's arrangement, drained after each test. */
const cleanups: Array<() => void> = [];

/** Settle the post's promise chain (real timers only). */
const flush = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));

afterEach(() => {
  while (cleanups.length > 0) cleanups.pop()!();
  vi.restoreAllMocks();
  vi.useRealTimers();
});

describe("WorkspaceSidebar", () => {
  it("uses the revisioned webapp authority for the current row only", () => {
    const { mount, sidebar } = harness();
    pushRows(sidebar, [
      wireRow({ name: "current", dir: "/tmp/current", current: true, status: { case: "thinking" } }),
      wireRow({ name: "other", dir: "/tmp/other", current: false, status: { case: "done" } }),
    ]);

    sidebar.setAuthoritativeCurrentStatus("interrupted");

    expect(mount.querySelector(".ws.current .st")?.getAttribute("title")).toBe("interrupted");
    expect(mount.innerHTML).toContain('title="done"');
  });

  it("keeps applying current status authority across later Emacs roster pushes", () => {
    const { mount, sidebar } = harness();
    sidebar.setAuthoritativeCurrentStatus("degraded");
    pushRows(sidebar, [wireRow({ current: true, status: { case: "thinking" } })]);
    expect(mount.querySelector(".ws.current .st")?.getAttribute("title")).toBe("degraded");
  });

  it("maps every underscore wire spelling onto the roster vocabulary", () => {
    expect(workspaceStatusFromRenderState("submitting")).toBe("submitting");
    expect(workspaceStatusFromRenderState("idle")).toBe("ready");
    expect(workspaceStatusFromRenderState("idle_async")).toBe("idle-async");
    expect(workspaceStatusFromRenderState("vendor_blocked")).toBe("vendor-blocked");
    expect(workspaceStatusFromRenderState("merge_conflict")).toBe("merge-conflict");
    expect(workspaceStatusFromRenderState("interrupted")).toBe("interrupted");
  });

  it("repaints the rail when the monitoring gate flips", () => {
    // Arrange — a pushed roster whose only row is the current workspace.
    const { mount, sidebar } = harness();
    pushRows(sidebar, [wireRow({ current: true })]);
    // Act
    sidebar.setMonitoring(true);
    // Assert — the current row's dot breathes amber without a fresh push.
    expect(mount.innerHTML).toContain("st-monitoring");
  });

  it("redraws the rail from its retained roster on a repaint (F5)", () => {
    // Arrange — a pushed roster, then the rail is blown away as a hidden
    // webview's stale frame would be.
    const { mount, sidebar } = harness();
    pushRows(sidebar, [wireRow({ name: "alpha" })]);
    mount.innerHTML = "";
    // Act — the repaint-on-show pass.
    sidebar.repaint();
    // Assert — the rail is back, from the roster it already held.
    expect(mount.innerHTML).toContain("alpha");
  });

  it("draws nothing on a repaint before the first roster push", () => {
    // Arrange — nothing has been pushed, so there is nothing to draw.
    const { mount, sidebar } = harness();
    // Act
    sidebar.repaint();
    // Assert
    expect(mount.innerHTML).toBe("");
  });

  it("stays hidden until the first roster push", () => {
    // Arrange + Act
    const { mount } = harness();
    // Assert
    expect(mount.hidden).toBe(true);
  });

  it("reveals the rail on the first roster push", () => {
    // Arrange
    const { mount, sidebar } = harness();
    // Act
    push(sidebar);
    // Assert
    expect(mount.hidden).toBe(false);
  });

  it("re-parks the feed on the reveal when it was pinned to its tail", () => {
    // Arrange — a feed sitting at its tail when the first push arrives.
    const { sidebar, parkCalls } = harness({ isPinned: () => true });
    // Act — the first push reveals the rail and reflows the feed.
    push(sidebar);
    // Assert — the reveal snapped the reflowed feed back to its tail.
    expect(parkCalls()).toBe(1);
  });

  it("leaves the feed alone on the reveal when the user had scrolled up", () => {
    // Arrange — a feed the user scrolled off its tail before the first push.
    const { sidebar, parkCalls } = harness({ isPinned: () => false });
    // Act
    push(sidebar);
    // Assert — no re-park, so the user's place survives the reveal.
    expect(parkCalls()).toBe(0);
  });

  it("does not re-park on a later push once the rail is already revealed", () => {
    // Arrange — a pinned feed whose rail is already revealed by a first push.
    const { sidebar, parkCalls } = harness({ isPinned: () => true });
    push(sidebar);
    // Act — a second push repaints the already-visible rail (no reflow).
    push(sidebar);
    // Assert — only the reveal re-parked; the later push left the feed alone.
    expect(parkCalls()).toBe(1);
  });

  it("reads the feed's pin before the reveal flips the rail visible", () => {
    // Arrange — a probe that records the rail's hidden state at read time.
    let hiddenWhenProbed: boolean | null = null;
    const mount = document.createElement("nav");
    mount.hidden = true;
    const sidebar = new WorkspaceSidebar(mount, {
      httpBase: "http://daemon",
      now: () => NOW_MS,
      isPinned: () => {
        hiddenWhenProbed = mount.hidden;
        return true;
      },
      parkFeed: () => {},
    });
    // Act
    push(sidebar);
    // Assert — the pin was read while the rail was still hidden (pre-reflow).
    expect(hiddenWhenProbed).toBe(true);
  });

  it("POSTs a switch command when a row body is clicked", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    push(sidebar);
    // Act
    click(mount.querySelector(".row")!);
    // Assert
    expect(calls).toEqual([
      { url: "http://daemon/workspace-command", body: [{ type: "switch", dir: "/tmp/ws" }] },
    ]);
  });

  it("resolves a click on a row's inner span to the row's switch", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    push(sidebar);
    // Act
    click(mount.querySelector(".row .name")!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "switch", dir: "/tmp/ws" }]);
  });

  it("POSTs a fold asking for the folded state an unfolded repo lacks", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    push(sidebar);
    // Act
    click(mount.querySelector(".repo-head")!);
    // Assert
    expect(calls).toEqual([
      { url: "http://daemon/workspace-command", body: [{ type: "fold", repo_key: "doom", folded: true }] },
    ]);
  });

  it("never hands the sidebar itself to the default fetch as its `this`", () => {
    // Arrange — no injected fetchFn, so the constructor takes its default off
    // the global. WKWebView's fetch is a real Window method and rejects any
    // `this` that is not the window, so the one thing the default must never
    // do is arrive carrying the sidebar (which `this.fetchFn(...)` would).
    const seen: unknown[] = [];
    const original = globalThis.fetch;
    globalThis.fetch = function (this: unknown): Promise<Response> {
      seen.push(this);
      return Promise.resolve({ ok: true, status: 200 } as Response);
    };
    cleanups.push(() => {
      globalThis.fetch = original;
    });
    const mount = document.createElement("nav");
    const sidebar = new WorkspaceSidebar(mount, { httpBase: "http://daemon" });
    push(sidebar);

    // Act
    click(mount.querySelector(".repo-head")!);

    // Assert
    expect(seen).toHaveLength(1);
    expect(seen[0]).not.toBe(sidebar);
  });

  it("POSTs a fold asking to unfold an already-folded repo", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    push(sidebar, {
      view: { case: "repository", value: { sections: [wireRepo({ folded: true })] } },
    });
    // Act
    click(mount.querySelector(".repo-head")!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "fold", repo_key: "doom", folded: false }]);
  });

  it("never folds locally — Emacs answers with the fresh roster", () => {
    // Arrange
    const { mount, sidebar } = harness();
    push(sidebar);
    // Act
    click(mount.querySelector(".repo-head")!);
    // Assert
    expect(mount.querySelector("section")!.className).toBe("repo");
  });

  it("POSTs a set-view command when a view button is clicked", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    push(sidebar);
    // Act
    click(mount.querySelector(`[data-set-view="task"]`)!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "set-view", view: "task" }]);
  });

  it("POSTs a task-create command when the add-task button is clicked", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    pushTasks(sidebar, []);
    // Act
    click(mount.querySelector("[data-add-task]")!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "task-create" }]);
  });

  it("POSTs a task-toggle-done command when a task checkbox is clicked", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    pushTasks(sidebar, [wireTask()]);
    // Act
    click(mount.querySelector("[data-task-check]")!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "task-toggle-done", id: "t1" }]);
  });

  it("POSTs a task-open command when a task label is clicked", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    pushTasks(sidebar, [wireTask()]);
    // Act
    click(mount.querySelector("[data-task-open]")!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "task-open", id: "t1" }]);
  });

  it("POSTs a task-add-workspace command when a task add button is clicked", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    pushTasks(sidebar, [wireTask()]);
    // Act
    click(mount.querySelector("[data-task-add]")!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "task-add-workspace", id: "t1" }]);
  });

  it("switches a workspace row inside a task section, not the task", () => {
    // Arrange — a task section whose row must still post a plain switch.
    const { mount, sidebar, calls } = harness();
    pushTasks(sidebar, [wireTask({ rows: [wireRow({ dir: "/tmp/inside" })] })]);
    // Act
    click(mount.querySelector(".row")!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "switch", dir: "/tmp/inside" }]);
  });

  it("opens a row's detail panel from its chevron", () => {
    // Arrange
    const { mount, sidebar } = harness();
    push(sidebar);
    // Act
    click(mount.querySelector("[data-chev]")!);
    // Assert
    expect(mount.querySelector(".ws")!.classList.contains("open")).toBe(true);
  });

  it("does not POST anything for a chevron click", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    push(sidebar);
    // Act
    click(mount.querySelector("[data-chev]")!);
    // Assert
    expect(calls).toHaveLength(0);
  });

  it("keeps an opened panel open across a roster re-push", () => {
    // Arrange
    const { mount, sidebar } = harness();
    push(sidebar);
    click(mount.querySelector("[data-chev]")!);
    // Act — Emacs pushes again; the rendered DOM is rebuilt wholesale.
    push(sidebar);
    // Assert
    expect(mount.querySelector(".ws")!.classList.contains("open")).toBe(true);
  });

  it("closes an opened panel on the chevron's second click", () => {
    // Arrange
    const { mount, sidebar } = harness();
    push(sidebar);
    click(mount.querySelector("[data-chev]")!);
    // Act
    click(mount.querySelector("[data-chev]")!);
    // Assert
    expect(mount.querySelector(".ws")!.classList.contains("open")).toBe(false);
  });

  it("opens a row's detail panel from toggleDetail (the keyboard path)", () => {
    // Arrange
    const { mount, sidebar } = harness();
    push(sidebar);
    // Act
    sidebar.toggleDetail("/tmp/ws");
    // Assert
    expect(mount.querySelector(".ws")!.classList.contains("open")).toBe(true);
  });

  it("closes the panel on a second toggleDetail", () => {
    // Arrange
    const { mount, sidebar } = harness();
    push(sidebar);
    sidebar.toggleDetail("/tmp/ws");
    // Act
    sidebar.toggleDetail("/tmp/ws");
    // Assert
    expect(mount.querySelector(".ws")!.classList.contains("open")).toBe(false);
  });

  it("does not POST anything for a toggleDetail", () => {
    // Arrange
    const { sidebar, calls } = harness();
    push(sidebar);
    // Act
    sidebar.toggleDetail("/tmp/ws");
    // Assert
    expect(calls).toHaveLength(0);
  });

  it("surfaces a non-2xx command response in the footer", async () => {
    // Arrange
    vi.spyOn(console, "error").mockImplementation(() => undefined);
    const { mount, sidebar } = harness({ ok: false });
    push(sidebar);
    // Act
    click(mount.querySelector(".row")!);
    await flush();
    // Assert
    expect(mount.querySelector(".sb-err")!.textContent).toBe(COMMAND_FAILED_NOTICE);
  });

  it("surfaces a network failure in the footer", async () => {
    // Arrange
    vi.spyOn(console, "error").mockImplementation(() => undefined);
    const { mount, sidebar } = harness({ reject: true });
    push(sidebar);
    // Act
    click(mount.querySelector(".row")!);
    await flush();
    // Assert
    expect(mount.querySelector(".sb-err")!.textContent).toBe(COMMAND_FAILED_NOTICE);
  });

  it("renders a click-handler contract breach in the same footer slot", () => {
    // Arrange — a task control the render could never emit: no task id.
    const { mount } = breachHarness();
    // Act
    click(mount.querySelector("[data-task-check]")!);
    // Assert
    expect(mount.querySelector(".sb-err")!.textContent).toContain("task control without an id");
  });

  it("still rethrows a click-handler contract breach after showing it", () => {
    // Arrange
    const { mount, reported } = breachHarness();
    // Act
    click(mount.querySelector("[data-task-check]")!);
    // Assert — the footer note is additive; the breach stays as loud as it was.
    expect(reported.some((r) => r.includes("task control without an id"))).toBe(true);
  });

  it("clears the failure note after its holding time", async () => {
    // Arrange
    vi.useFakeTimers();
    vi.spyOn(console, "error").mockImplementation(() => undefined);
    const { mount, sidebar } = harness({ reject: true });
    push(sidebar);
    click(mount.querySelector(".row")!);
    await vi.advanceTimersByTimeAsync(0);
    // Act
    await vi.advanceTimersByTimeAsync(COMMAND_FAILED_NOTICE_MS);
    // Assert
    expect(mount.querySelector(".sb-err")).toBeNull();
  });
});

/**
 * The roster FRAME path: the decoded `frontend.v1.WorkspaceRoster` the rail
 * now renders from, and the revision gate both publish paths share.
 */
describe("the roster frame", () => {
  /** A frame row carrying exactly one status arm (arms are payload-free). */
  function frameRow(over: Partial<FrameRosterRow> = {}): FrameRosterRow {
    return {
      dir: "/w/a",
      name: "a",
      status: { case: "ready" },
      current: false,
      children: [],
      lastViewedAtMs: 0,
      mergedAtMs: 0,
      branch: "",
      parentBranch: "",
      summary: "",
      closed: false,
      ...over,
    };
  }

  /** A decoded roster frame in the repository grouping. */
  function frame(over: Partial<RosterFrame> = {}): RosterFrame {
    return {
      revision: 3,
      bootId: "boot-a",
      view: {
        case: "repository",
        value: { sections: [{ repoKey: "doom", folded: false, label: "doom", rows: [frameRow()] }] },
      },
      recentlyMerged: { rows: [], folded: false, label: "" },
      currentDir: "",
      navDir: "",
      ...over,
    };
  }

  it("renders the rail from an adopted frame", () => {
    // Arrange
    const { mount, sidebar } = harness();
    // Act
    sidebar.adoptRosterFrame(frame());
    // Assert — the frame's row is on screen, addressed by its dir.
    expect(mount.querySelector('[data-row-dir="/w/a"]')).not.toBeNull();
  });

  it("reveals the rail on the first frame, as the boot snapshot delivers it", () => {
    // Arrange — a rail still hidden, exactly as it sits before the connect burst.
    const { mount, sidebar } = harness();
    // Act — the snapshot's roster frame arrives with the feed's own frames.
    sidebar.adoptRosterFrame(frame());
    // Assert
    expect(mount.hidden).toBe(false);
  });

  it("re-parks the feed on the frame reveal when it was pinned to its tail", () => {
    // Arrange — a feed sitting at its tail when the boot roster frame lands.
    const { sidebar, parkCalls } = harness({ isPinned: () => true });
    // Act
    sidebar.adoptRosterFrame(frame());
    // Assert — the reveal snapped the reflowed feed back to its tail.
    expect(parkCalls()).toBe(1);
  });

  it("maps the repository view arm onto repo sections", () => {
    // Arrange
    const { mount, sidebar } = harness();
    // Act
    sidebar.adoptRosterFrame(frame());
    // Assert — a fold-toggling repo header keyed by the section's repo key.
    expect(mount.querySelector('[data-repo-key="doom"]')).not.toBeNull();
  });

  it("maps the task view arm onto task sections", () => {
    // Arrange
    const { mount, sidebar } = harness();
    // Act
    sidebar.adoptRosterFrame(
      frame({
        view: {
          case: "task",
          value: { sections: [{ taskId: "t1", title: "Ship it", done: false, rows: [frameRow()] }] },
        },
      }),
    );
    // Assert — the task header carries the task's id and its display title.
    const head = mount.querySelector('[data-task-open][data-task-id="t1"]');
    expect(head?.textContent).toBe("Ship it");
  });

  it("maps a row's status arm onto the rail's status vocabulary", () => {
    // Arrange — an arm whose keyword spelling differs from the arm name.
    const { mount, sidebar } = harness();
    // Act
    sidebar.adoptRosterFrame(
      frame({
        view: {
          case: "repository",
          value: {
            sections: [
              {
                repoKey: "doom",
                folded: false,
                label: "doom",
                rows: [frameRow({ status: { case: "idleAsync" } })],
              },
            ],
          },
        },
      }),
    );
    // Assert
    expect(mount.querySelector(".st-idle-async")).not.toBeNull();
  });

  it("renders no Recently Merged section when the frame's section is empty", () => {
    // Arrange
    const { mount, sidebar } = harness();
    // Act
    sidebar.adoptRosterFrame(frame());
    // Assert — proto3 spells "no recent merges" as an empty row list.
    expect(mount.querySelector(".merged-section")).toBeNull();
  });

  it("keys the Recently Merged section the way the fold command addresses it", () => {
    // Arrange
    const { mount, sidebar } = harness();
    // Act
    sidebar.adoptRosterFrame(
      frame({
        recentlyMerged: {
          rows: [frameRow({ dir: "/w/m", status: { case: "merged" } })],
          folded: false,
          label: "",
        },
      }),
    );
    // Assert — the key must match `agent-repl--sidebar-merged-key`.
    expect(
      mount.querySelector(`.merged-section [data-repo-key="${RECENTLY_MERGED_KEY}"]`),
    ).not.toBeNull();
  });

  it("reads an empty navDir as no keyboard cursor", () => {
    // Arrange
    const { mount, sidebar } = harness();
    // Act
    sidebar.adoptRosterFrame(frame({ navDir: "" }));
    // Assert — no row wears the cursor's dashed ring.
    expect(mount.querySelector(".navsel")).toBeNull();
  });
});

/**
 * The display fields `frontend.v1.RosterRow` and the section messages carry:
 * the two timestamps behind the when-column, the detail panel's branch lines
 * and summary, `closed`, and the section labels. Every one of them travels the
 * frame — there is no second ingress to supply them.
 */
describe("the roster frame's display fields", () => {
  /** The one row of a default repository frame, mapped. */
  function mappedRow(over: Partial<FrameRosterRow>): WorkspaceRow {
    return rosterFromFrame(wireFrame({
      view: { case: "repository", value: { sections: [wireRepo({ rows: [wireRow(over)] })] } },
    })).repos[0]!.rows[0]!;
  }

  it("converts a millisecond viewed stamp to the seconds the rail counts in", () => {
    // Arrange — the wire's unit is ms, `formatRecency`'s is seconds, so this
    // pins the factor of 1000 a wrong seam would silently drop.
    const viewedMs = NOW_MS - 330_000;
    // Act
    const mapped = mappedRow({ lastViewedAtMs: viewedMs });
    // Assert
    expect(mapped.lastViewedAt).toBe(NOW_S - 330);
  });

  it("renders that converted stamp as its compact age in the when-column", () => {
    // Arrange — 5m30s ago, which truncates to 5m.
    const { mount, sidebar } = harness();
    // Act
    pushRows(sidebar, [wireRow({ lastViewedAtMs: NOW_MS - 330_000 })]);
    // Assert — a seam that forgot to divide would read "now" forever.
    expect(mount.querySelector(".when")!.textContent).toBe("5m");
  });

  it("reads a zero viewed stamp as a workspace never viewed", () => {
    // Arrange + Act — 0 is proto3's only spelling of "never".
    const mapped = mappedRow({ lastViewedAtMs: 0 });
    // Assert
    expect(mapped.lastViewedAt).toBeNull();
  });

  it("converts a millisecond merge stamp to seconds too", () => {
    // Arrange + Act
    const mapped = mappedRow({ mergedAtMs: NOW_MS - 7_200_000 });
    // Assert
    expect(mapped.mergedAt).toBe(NOW_S - 7200);
  });

  it("prefers the merge stamp over the viewed stamp in the when-column", () => {
    // Arrange — a merged row viewed a minute ago but merged two hours back;
    // the proto says the merge wins, since merge age is the fact worth showing.
    const { mount, sidebar } = harness();
    // Act
    pushRows(sidebar, [
      wireRow({ lastViewedAtMs: NOW_MS - 60_000, mergedAtMs: NOW_MS - 7_200_000 }),
    ]);
    // Assert
    expect(mount.querySelector(".when")!.textContent).toBe("2h");
  });

  it("falls back to the viewed stamp when the row never merged", () => {
    // Arrange + Act
    const { mount, sidebar } = harness();
    pushRows(sidebar, [wireRow({ lastViewedAtMs: NOW_MS - 7_200_000, mergedAtMs: 0 })]);
    // Assert
    expect(mount.querySelector(".when")!.textContent).toBe("2h");
  });

  it("recedes a closed row, the treatment a merged row gets", () => {
    // Arrange + Act — a pane dismissed but still switchable.
    const { mount, sidebar } = harness();
    pushRows(sidebar, [wireRow({ closed: true })]);
    // Assert
    expect(mount.querySelector(".ws")!.classList.contains("gone")).toBe(true);
  });

  it("leaves an open row un-receded", () => {
    // Arrange + Act
    const { mount, sidebar } = harness();
    pushRows(sidebar, [wireRow({ closed: false })]);
    // Assert
    expect(mount.querySelector(".ws")!.classList.contains("gone")).toBe(false);
  });

  it("renders the row's branch in its detail panel", () => {
    // Arrange + Act
    const { mount, sidebar } = harness();
    pushRows(sidebar, [wireRow({ branch: "wave2b-sidebar" })]);
    // Assert
    expect(mount.querySelector(".detail")!.textContent).toContain("wave2b-sidebar");
  });

  it("renders the row's parent branch in its detail panel", () => {
    // Arrange + Act
    const { mount, sidebar } = harness();
    pushRows(sidebar, [wireRow({ parentBranch: "master" })]);
    // Assert
    expect(mount.querySelector(".detail")!.textContent).toContain("master");
  });

  it("renders the row's summary in its detail panel", () => {
    // Arrange + Act
    const { mount, sidebar } = harness();
    pushRows(sidebar, [wireRow({ summary: "retire the hook ingress" })]);
    // Assert
    expect(mount.querySelector(".detail .summary")!.textContent).toBe("retire the hook ingress");
  });

  it("omits the summary entirely when the wire carries none", () => {
    // Arrange + Act — "" is the only "none" a proto3 string has.
    const { mount, sidebar } = harness();
    pushRows(sidebar, [wireRow({ summary: "" })]);
    // Assert — an empty paragraph would take space saying nothing.
    expect(mount.querySelector(".detail .summary")).toBeNull();
  });

  it("maps an empty branch to no branch line rather than a blank one", () => {
    // Arrange + Act
    const mapped = mappedRow({ branch: "" });
    // Assert
    expect(mapped.branch).toBeNull();
  });

  it("heads a repo section with the label the author supplied", () => {
    // Arrange + Act — the label is display only; repoKey stays the identity.
    const { mount, sidebar } = harness();
    push(sidebar, {
      view: {
        case: "repository",
        value: { sections: [wireRepo({ repoKey: "/src/doom", label: "doom" })] },
      },
    });
    // Assert
    expect(mount.querySelector(".repo-head")!.textContent).toContain("doom");
  });

  it("falls back to the repo key when the section carries no label", () => {
    // Arrange + Act — empty means the author has no label, per the field comment.
    const { mount, sidebar } = harness();
    push(sidebar, {
      view: {
        case: "repository",
        value: { sections: [wireRepo({ repoKey: "/src/doom", label: "" })] },
      },
    });
    // Assert
    expect(mount.querySelector(".repo-head")!.textContent).toContain("/src/doom");
  });

  it("keeps the repo key as the fold identity even when a label differs", () => {
    // Arrange + Act
    const { mount, sidebar } = harness();
    push(sidebar, {
      view: {
        case: "repository",
        value: { sections: [wireRepo({ repoKey: "/src/doom", label: "doom" })] },
      },
    });
    // Assert — the fold command POSTs this key back to Emacs.
    expect(mount.querySelector('[data-repo-key="/src/doom"]')).not.toBeNull();
  });

  it("labels the Recently Merged section from the frame", () => {
    // Arrange + Act — the author resolves the heading; nothing is hardcoded.
    const { mount, sidebar } = harness();
    push(sidebar, {
      recentlyMerged: {
        rows: [wireRow({ dir: "/w/m", status: { case: "merged" } })],
        folded: false,
        label: "Recently Merged",
      },
    });
    // Assert
    expect(mount.querySelector(".merged-section .repo-head")!.textContent).toContain(
      "Recently Merged",
    );
  });

  it("folds the Recently Merged section when the frame says it is folded", () => {
    // Arrange + Act
    const { mount, sidebar } = harness();
    push(sidebar, {
      recentlyMerged: {
        rows: [wireRow({ dir: "/w/m", status: { case: "merged" } })],
        folded: true,
        label: "Recently Merged",
      },
    });
    // Assert
    expect(mount.querySelector(".merged-section")!.classList.contains("folded")).toBe(true);
  });

  it("leaves the Recently Merged section unfolded when the frame says so", () => {
    // Arrange + Act — the fold is read off the frame, never defaulted.
    const { mount, sidebar } = harness();
    push(sidebar, {
      recentlyMerged: {
        rows: [wireRow({ dir: "/w/m", status: { case: "merged" } })],
        folded: false,
        label: "Recently Merged",
      },
    });
    // Assert
    expect(mount.querySelector(".merged-section")!.classList.contains("folded")).toBe(false);
  });
});

/**
 * The legacy `window.agentReplWorkspaceRoster` script injection is retired.
 * It could not carry the revision the gate ranks by, and a second ingress meant
 * the reveal and the diagnostics could differ by path.
 */
describe("the retired roster hook ingress", () => {
  it("exports no roster hook name", async () => {
    // Arrange + Act
    const mod = await import("../src/sidebar.js");
    // Assert
    expect(Object.keys(mod)).not.toContain("ROSTER_HOOK");
  });

  it("exports no roster hook installer", async () => {
    // Arrange + Act
    const mod = await import("../src/sidebar.js");
    // Assert
    expect(Object.keys(mod)).not.toContain("installWorkspaceRosterHook");
  });

  it("exports no hook-shaped roster validator", async () => {
    // Arrange + Act — the frame path validates in the decoder instead.
    const mod = await import("../src/sidebar.js");
    // Assert
    expect(Object.keys(mod)).not.toContain("validateWorkspaceRoster");
  });

  it("leaves the sidebar with no external update entry", () => {
    // Arrange
    const { sidebar } = harness();
    // Act + Assert — `adoptRosterFrame` is the single ingress.
    expect((sidebar as unknown as Record<string, unknown>).update).toBeUndefined();
  });

  it("still keeps the expand hook, which is a keyboard gesture and not an ingress", async () => {
    // Arrange + Act — retiring the roster hook must not take C-S-RET with it.
    const mod = await import("../src/sidebar.js");
    // Assert
    expect(Object.keys(mod)).toContain("installWorkspaceExpandHook");
  });
});

describe("the roster revision gate", () => {
  function frameAt(revision: number, dir: string): RosterFrame {
    return {
      revision,
      bootId: "boot-a",
      view: {
        case: "repository",
        value: {
          sections: [
            {
              repoKey: "doom",
              folded: false,
              label: "doom",
              rows: [
                {
                  dir,
                  name: "a",
                  status: { case: "ready" },
                  current: false,
                  children: [],
                  lastViewedAtMs: 0,
                  mergedAtMs: 0,
                  branch: "",
                  parentBranch: "",
                  summary: "",
                  closed: false,
                },
              ],
            },
          ],
        },
      },
      recentlyMerged: { rows: [], folded: false, label: "" },
      currentDir: "",
      navDir: "",
    };
  }

  it("drops a frame older than the held revision", () => {
    // Arrange — a reconnect replay reordering behind what the page holds.
    const { mount, sidebar } = harness();
    sidebar.adoptRosterFrame(frameAt(5, "/w/new"));
    // Act
    sidebar.adoptRosterFrame(frameAt(4, "/w/old"));
    // Assert — the newer picture still stands.
    expect(mount.querySelector('[data-row-dir="/w/old"]')).toBeNull();
  });

  it("logs the drop with both revisions rather than swallowing it", () => {
    // Arrange
    const lines: string[] = [];
    const { sidebar } = harness();
    sidebar.adoptRosterFrame(frameAt(5, "/w/new"));
    setLogger(new ForwardingLogger(() => true, (level, line) => lines.push(`${level}: ${line}`)));
    // Act
    sidebar.adoptRosterFrame(frameAt(4, "/w/old"));
    // Assert
    expect(
      lines.some((l) => l.includes("revision 4 is older than the held revision 5")),
    ).toBe(true);
  });

  it("adopts a frame at the held revision, since it asserts the same picture", () => {
    // Arrange
    const { mount, sidebar } = harness();
    sidebar.adoptRosterFrame(frameAt(5, "/w/first"));
    // Act
    sidebar.adoptRosterFrame(frameAt(5, "/w/second"));
    // Assert
    expect(mount.querySelector('[data-row-dir="/w/second"]')).not.toBeNull();
  });

  it("adopts the first frame whatever revision it claims", () => {
    // Arrange — nothing held yet, so there is no revision to be older than.
    const { mount, sidebar } = harness();
    // Act
    sidebar.adoptRosterFrame(frameAt(0, "/w/a"));
    // Assert
    expect(mount.hidden).toBe(false);
  });

  it("names the frame as the adopting path in the log", () => {
    // Arrange
    const lines: string[] = [];
    setLogger(new ForwardingLogger(() => true, (level, line) => lines.push(`${level}: ${line}`)));
    const { sidebar } = harness();
    // Act
    sidebar.adoptRosterFrame(frameAt(7, "/w/a"));
    // Assert
    expect(lines.some((l) => l.includes("workspace roster adopted path=frame revision=7"))).toBe(
      true,
    );
  });
});

describe("installWorkspaceExpandHook", () => {
  it("names the hook the way the Emacs side must call it", () => {
    // Arrange + Act + Assert
    expect(EXPAND_HOOK).toBe("agentReplWorkspaceExpand");
  });

  it("plants the hook under that name", () => {
    // Arrange
    const target: HostGlobal = {};
    const { sidebar } = harness();
    // Act
    installWorkspaceExpandHook(target, sidebar);
    // Assert
    expect(typeof target[EXPAND_HOOK]).toBe("function");
  });

  it("unfolds the addressed row's detail panel when fired", () => {
    // Arrange
    const target: HostGlobal = {};
    const { mount, sidebar } = harness();
    push(sidebar);
    installWorkspaceExpandHook(target, sidebar);
    // Act — the way an Emacs host script fires it, dir as a plain string.
    (target[EXPAND_HOOK] as (d: unknown) => void)("/tmp/ws");
    // Assert
    expect(mount.querySelector(".ws")!.classList.contains("open")).toBe(true);
  });

  it("folds the panel back when fired a second time", () => {
    // Arrange
    const target: HostGlobal = {};
    const { mount, sidebar } = harness();
    push(sidebar);
    installWorkspaceExpandHook(target, sidebar);
    (target[EXPAND_HOOK] as (d: unknown) => void)("/tmp/ws");
    // Act
    (target[EXPAND_HOOK] as (d: unknown) => void)("/tmp/ws");
    // Assert
    expect(mount.querySelector(".ws")!.classList.contains("open")).toBe(false);
  });

  it("throws on a non-string dir rather than coercing it", () => {
    // Arrange
    const target: HostGlobal = {};
    const { sidebar } = harness();
    installWorkspaceExpandHook(target, sidebar);
    // Act + Assert
    expect(() => (target[EXPAND_HOOK] as (d: unknown) => void)(7)).toThrow(/must be a string/);
  });
});

// --- the current row's structured merge status ------------------------------

describe("the rail's structured merge status", () => {
  /** A merge status carrying PHASE, with the envelope fixed. */
  const merge = (phase: MergeStatus["phase"]): MergeStatus => ({
    runId: "run-1",
    phaseStartedAtMs: NOW_MS - 5_000,
    updatedAtMs: NOW_MS,
    phase,
  });

  const PICKING = merge({
    case: "cherryPicking",
    value: { commitsTotal: 4, commitsLanded: 1, currentSha: "abc1234", currentSubject: "fix it" },
  });

  /** Adopt a frame whose single row is THIS session's, mid-merge. */
  const pushMerging = (sidebar: WorkspaceSidebar): void =>
    pushRows(sidebar, [wireRow({ current: true, status: { case: "merging" } })]);

  it("renders nothing when no merge touches the workspace", () => {
    // Arrange / Act
    const got = mergeRowHtml(null);
    // Assert
    expect(got).toBe("");
  });

  it("shows the queue place while enqueued", () => {
    // Arrange / Act
    const got = mergeRowHtml(merge({ case: "enqueued", value: { position: 2, depth: 3 } }));
    // Assert
    expect(got).toContain("merge queued · 2/3");
  });

  it("shows the commit in hand while cherry-picking", () => {
    // Arrange / Act
    const got = mergeRowHtml(PICKING);
    // Assert
    expect(got).toContain("merging · 1/4 · picking · abc1234 fix it");
  });

  it("shows the before-action prompt, which the recycle glyph never could", () => {
    // Arrange / Act
    const got = mergeRowHtml(merge({ case: "beforeAction", value: { prompt: "run the linter" } }));
    // Assert
    expect(got).toContain("merge before-action · before-action · run the linter");
  });

  it("shows the after-action prompt", () => {
    // Arrange / Act
    const got = mergeRowHtml(merge({ case: "afterAction", value: { prompt: "close it" } }));
    // Assert
    expect(got).toContain("merge after-action · after-action · close it");
  });

  it("names the conflicted commit", () => {
    // Arrange / Act
    const got = mergeRowHtml(
      merge({
        case: "conflict",
        value: {
          conflictedSha: "bad1234",
          conflictedSubject: "same file",
          commitsTotal: 4,
          commitsLanded: 2,
        },
      }),
    );
    // Assert
    expect(got).toContain("conflict · bad1234 same file");
  });

  it("paints a conflict loud: it is the one merge state waiting on the user", () => {
    // Arrange / Act
    const got = mergeRowHtml(
      merge({
        case: "conflict",
        value: { conflictedSha: "bad1234", conflictedSubject: "x", commitsTotal: 4, commitsLanded: 2 },
      }),
    );
    // Assert
    expect(got).toContain('class="ws-merge error"');
  });

  it("carries a failed run's cause as its own note line", () => {
    // Arrange / Act
    const got = mergeRowHtml(
      merge({
        case: "failed",
        value: {
          cause: "tests failed",
          commitsTotal: 4,
          commitsLanded: 3,
          failingSha: "fee1234",
          failingSubject: "break it",
          failedJson:
            '{"cause":"tests failed","commitsTotal":4,"commitsLanded":3,"failingSha":"fee1234","failingSubject":"break it"}',
        },
      }),
    );
    // Assert
    expect(got).toContain('<div class="ws-merge-note error">tests failed</div>');
  });

  it("notes a landed merge whose after-action failed", () => {
    // Arrange / Act
    const got = mergeRowHtml(
      merge({ case: "merged", value: { commitsTotal: 4, afterActionError: "timed out" } }),
    );
    // Assert
    expect(got).toContain("after-action failed: timed out");
  });

  it("escapes a commit subject, which is daemon text", () => {
    // Arrange / Act
    const got = mergeRowHtml(
      merge({
        case: "cherryPicking",
        value: {
          commitsTotal: 1,
          commitsLanded: 0,
          currentSha: "abc1234",
          currentSubject: "<img src=x>",
        },
      }),
    );
    // Assert
    expect(got).not.toContain("<img");
  });

  it("draws the merge on THIS session's row", () => {
    // Arrange
    const { mount, sidebar } = harness();
    pushMerging(sidebar);
    // Act
    sidebar.setMergeStatus(PICKING);
    // Assert
    expect(mount.querySelector(".ws-merge")?.textContent).toContain("1/4");
  });

  it("leaves every OTHER row exactly as Emacs asserted it", () => {
    // Arrange — the status describes this session only.
    const { mount, sidebar } = harness();
    pushRows(sidebar, [
      wireRow({ dir: "/tmp/other" }),
      wireRow({ current: true, status: { case: "merging" } }),
    ]);
    // Act
    sidebar.setMergeStatus(PICKING);
    // Assert
    expect(mount.querySelectorAll(".ws-merge").length).toBe(1);
  });

  it("repaints when a per-commit tick moves the run on", () => {
    // Arrange
    const { mount, sidebar } = harness();
    pushMerging(sidebar);
    sidebar.setMergeStatus(PICKING);
    // Act
    sidebar.setMergeStatus({
      ...PICKING,
      updatedAtMs: NOW_MS + 1,
      phase: {
        case: "cherryPicking",
        value: { commitsTotal: 4, commitsLanded: 2, currentSha: "def5678", currentSubject: "next" },
      },
    });
    // Assert
    expect(mount.querySelector(".ws-merge")?.textContent).toContain("2/4");
  });

  it("clears the lines when the merge status goes away", () => {
    // Arrange
    const { mount, sidebar } = harness();
    pushMerging(sidebar);
    sidebar.setMergeStatus(PICKING);
    // Act
    sidebar.setMergeStatus(null);
    // Assert
    expect(mount.querySelector(".ws-merge")).toBeNull();
  });

  it("styles both merge lines, so neither renders unthemed", () => {
    // Assert — the class list is the whole visual contract for these rows.
    expect(css).toContain("#ws-sidebar .ws-merge,");
    expect(css).toContain("#ws-sidebar .ws-merge-note {");
  });
});
