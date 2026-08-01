// @vitest-environment jsdom
//
// The pure builders (validation, recency, HTML) need no DOM; jsdom is here
// for WorkspaceSidebar itself, whose delegation resolves clicks through
// real `closest` walks over the rendered mount.
import { afterEach, describe, expect, it, vi } from "vitest";

import { HostGlobal } from "../src/host.js";
import {
  COMMAND_FAILED_NOTICE,
  COMMAND_FAILED_NOTICE_MS,
  EXPAND_HOOK,
  NO_TASK_KEY,
  ROSTER_HOOK,
  RepoGroup,
  WorkspaceRoster,
  WorkspaceRow,
  WorkspaceSidebar,
  formatRecency,
  installWorkspaceExpandHook,
  installWorkspaceRosterHook,
  sidebarHtml,
  statusDotHtml,
  taskSectionHtml,
  validateWorkspaceRoster,
  workspaceStatusFromRenderState,
} from "../src/sidebar.js";
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

describe("validateWorkspaceRoster", () => {
  it("passes a well-formed roster through intact", () => {
    // Arrange
    const r = roster({ navDir: "/tmp/ws" });
    // Act + Assert
    expect(validateWorkspaceRoster(r)).toEqual(r);
  });

  it("throws on an unknown status rather than defaulting it", () => {
    // Arrange
    const r = roster({ repos: [group({ rows: [row({ status: "exploded" as never })] })] });
    // Act + Assert
    expect(() => validateWorkspaceRoster(r)).toThrow(/unknown status "exploded"/);
  });

  it("accepts the inactive status a perspective-less row carries", () => {
    // Arrange
    const r = roster({ repos: [group({ rows: [row({ status: "inactive" })] })] });
    // Act + Assert
    expect(() => validateWorkspaceRoster(r)).not.toThrow();
  });

  it("throws on an unknown status buried in a child row", () => {
    // Arrange
    const child = row({ dir: "/tmp/kid", status: "exploded" as never });
    const r = roster({ repos: [group({ rows: [row({ children: [child] })] })] });
    // Act + Assert
    expect(() => validateWorkspaceRoster(r)).toThrow(/children\[0\]\.status/);
  });

  it("throws when repos is not an array", () => {
    // Arrange + Act + Assert
    expect(() => validateWorkspaceRoster({ repos: {}, navDir: null })).toThrow(/repos/);
  });

  it("throws when a row's lastViewedAt is a string", () => {
    // Arrange
    const r = roster({ repos: [group({ rows: [row({ lastViewedAt: "5m" as never })] })] });
    // Act + Assert
    expect(() => validateWorkspaceRoster(r)).toThrow(/lastViewedAt/);
  });

  it("logs the breach before throwing on an unknown status", () => {
    // Arrange — the throw surfaces only in Emacs's script eval, so the
    // breach must also reach the daemon log the operator reads.
    const lines: string[] = [];
    setLogger(new ForwardingLogger(() => true, (level, line) => lines.push(`${level}: ${line}`)));
    const r = roster({ repos: [group({ rows: [row({ status: "exploded" as never })] })] });
    // Act + Assert
    expect(() => validateWorkspaceRoster(r)).toThrow(/unknown status "exploded"/);
    expect(lines.some((l) => l.startsWith("error:") && l.includes("unknown status"))).toBe(true);
  });

  it("logs the breach before throwing on a malformed shape", () => {
    // Arrange
    const lines: string[] = [];
    setLogger(new ForwardingLogger(() => true, (level, line) => lines.push(`${level}: ${line}`)));
    // Act + Assert
    expect(() => validateWorkspaceRoster({ repos: {}, navDir: null })).toThrow(/repos/);
    expect(lines.some((l) => l.startsWith("error:") && l.includes("repos"))).toBe(true);
  });

  it("throws when navDir is neither string nor null", () => {
    // Arrange + Act + Assert
    expect(() => validateWorkspaceRoster(roster({ navDir: 7 as never }))).toThrow(/navDir/);
  });

  it("carries the view and task sections through intact", () => {
    // Arrange
    const r = roster({ view: "task", tasks: [group({ key: "t1", label: "T", done: true })] });
    // Act + Assert
    expect(validateWorkspaceRoster(r)).toEqual(r);
  });

  it("throws on an unknown view rather than defaulting it", () => {
    // Arrange
    const r = roster({ view: "kanban" as never });
    // Act + Assert
    expect(() => validateWorkspaceRoster(r)).toThrow(/unknown view "kanban"/);
  });

  it("throws when tasks is not an array", () => {
    // Arrange + Act + Assert
    expect(() => validateWorkspaceRoster(roster({ tasks: {} as never }))).toThrow(/tasks/);
  });

  it("throws when a group's done flag is missing", () => {
    // Arrange — a group object with no `done` key.
    const bad = { key: "doom", label: "doom", folded: false, rows: [] };
    const r = { ...roster(), repos: [bad] };
    // Act + Assert
    expect(() => validateWorkspaceRoster(r)).toThrow(/done/);
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

  it("only spins the in-flight merge, never the queued one", () => {
    // Arrange + Act — the motion lives in the stylesheet, so it is pinned there.
    const spinning = css.match(/#ws-sidebar \.st-merging \{[^}]*animation: ws-spin/);
    // Assert
    expect(spinning).not.toBeNull();
    expect(css).not.toMatch(/\.st-merge-queued[^{]*\{[^}]*animation/);
    expect(css).not.toMatch(/\.st-merge-enqueuing[^{]*\{[^}]*animation/);
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
  sidebar.update(roster());
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
    sidebar.update(roster({ repos: [group({ rows: [
      row({ name: "current", current: true, status: "thinking" }),
      row({ name: "other", current: false, status: "done" }),
    ] })] }));

    sidebar.setAuthoritativeCurrentStatus("interrupted");

    expect(mount.querySelector(".ws.current .st")?.getAttribute("title")).toBe("interrupted");
    expect(mount.innerHTML).toContain('title="done"');
  });

  it("keeps applying current status authority across later Emacs roster pushes", () => {
    const { mount, sidebar } = harness();
    sidebar.setAuthoritativeCurrentStatus("degraded");
    sidebar.update(roster({ repos: [group({ rows: [row({ current: true, status: "thinking" })] })] }));
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
    sidebar.update(roster({ repos: [group({ rows: [row({ current: true })] })] }));
    // Act
    sidebar.setMonitoring(true);
    // Assert — the current row's dot breathes amber without a fresh push.
    expect(mount.innerHTML).toContain("st-monitoring");
  });

  it("redraws the rail from its retained roster on a repaint (F5)", () => {
    // Arrange — a pushed roster, then the rail is blown away as a hidden
    // webview's stale frame would be.
    const { mount, sidebar } = harness();
    sidebar.update(roster({ repos: [group({ rows: [row({ name: "alpha" })] })] }));
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
    sidebar.update(roster());
    // Assert
    expect(mount.hidden).toBe(false);
  });

  it("re-parks the feed on the reveal when it was pinned to its tail", () => {
    // Arrange — a feed sitting at its tail when the first push arrives.
    const { sidebar, parkCalls } = harness({ isPinned: () => true });
    // Act — the first push reveals the rail and reflows the feed.
    sidebar.update(roster());
    // Assert — the reveal snapped the reflowed feed back to its tail.
    expect(parkCalls()).toBe(1);
  });

  it("leaves the feed alone on the reveal when the user had scrolled up", () => {
    // Arrange — a feed the user scrolled off its tail before the first push.
    const { sidebar, parkCalls } = harness({ isPinned: () => false });
    // Act
    sidebar.update(roster());
    // Assert — no re-park, so the user's place survives the reveal.
    expect(parkCalls()).toBe(0);
  });

  it("does not re-park on a later push once the rail is already revealed", () => {
    // Arrange — a pinned feed whose rail is already revealed by a first push.
    const { sidebar, parkCalls } = harness({ isPinned: () => true });
    sidebar.update(roster());
    // Act — a second push repaints the already-visible rail (no reflow).
    sidebar.update(roster());
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
    sidebar.update(roster());
    // Assert — the pin was read while the rail was still hidden (pre-reflow).
    expect(hiddenWhenProbed).toBe(true);
  });

  it("rejects a malformed push loudly instead of painting it", () => {
    // Arrange
    const { sidebar } = harness();
    // Act + Assert
    expect(() => sidebar.update({ repos: "nope" })).toThrow(/repos/);
  });

  it("POSTs a switch command when a row body is clicked", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    sidebar.update(roster());
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
    sidebar.update(roster());
    // Act
    click(mount.querySelector(".row .name")!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "switch", dir: "/tmp/ws" }]);
  });

  it("POSTs a fold asking for the folded state an unfolded repo lacks", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    sidebar.update(roster());
    // Act
    click(mount.querySelector(".repo-head")!);
    // Assert
    expect(calls).toEqual([
      { url: "http://daemon/workspace-command", body: [{ type: "fold", repo_key: "doom", folded: true }] },
    ]);
  });

  it("POSTs a fold asking to unfold an already-folded repo", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    sidebar.update(roster({ repos: [group({ folded: true })] }));
    // Act
    click(mount.querySelector(".repo-head")!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "fold", repo_key: "doom", folded: false }]);
  });

  it("never folds locally — Emacs answers with the fresh roster", () => {
    // Arrange
    const { mount, sidebar } = harness();
    sidebar.update(roster());
    // Act
    click(mount.querySelector(".repo-head")!);
    // Assert
    expect(mount.querySelector("section")!.className).toBe("repo");
  });

  it("POSTs a set-view command when a view button is clicked", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    sidebar.update(roster());
    // Act
    click(mount.querySelector(`[data-set-view="task"]`)!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "set-view", view: "task" }]);
  });

  it("POSTs a task-create command when the add-task button is clicked", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    sidebar.update(roster({ view: "task", repos: [], tasks: [] }));
    // Act
    click(mount.querySelector("[data-add-task]")!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "task-create" }]);
  });

  it("POSTs a task-toggle-done command when a task checkbox is clicked", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    sidebar.update(
      roster({ view: "task", repos: [], tasks: [group({ key: "t1", label: "T", rows: [] })] }),
    );
    // Act
    click(mount.querySelector("[data-task-check]")!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "task-toggle-done", id: "t1" }]);
  });

  it("POSTs a task-open command when a task label is clicked", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    sidebar.update(
      roster({ view: "task", repos: [], tasks: [group({ key: "t1", label: "T", rows: [] })] }),
    );
    // Act
    click(mount.querySelector("[data-task-open]")!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "task-open", id: "t1" }]);
  });

  it("POSTs a task-add-workspace command when a task add button is clicked", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    sidebar.update(
      roster({ view: "task", repos: [], tasks: [group({ key: "t1", label: "T", rows: [] })] }),
    );
    // Act
    click(mount.querySelector("[data-task-add]")!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "task-add-workspace", id: "t1" }]);
  });

  it("switches a workspace row inside a task section, not the task", () => {
    // Arrange — a task section whose row must still post a plain switch.
    const { mount, sidebar, calls } = harness();
    sidebar.update(
      roster({
        view: "task",
        repos: [],
        tasks: [group({ key: "t1", label: "T", rows: [row({ dir: "/tmp/inside" })] })],
      }),
    );
    // Act
    click(mount.querySelector(".row")!);
    // Assert
    expect(calls[0]?.body).toEqual([{ type: "switch", dir: "/tmp/inside" }]);
  });

  it("opens a row's detail panel from its chevron", () => {
    // Arrange
    const { mount, sidebar } = harness();
    sidebar.update(roster());
    // Act
    click(mount.querySelector("[data-chev]")!);
    // Assert
    expect(mount.querySelector(".ws")!.classList.contains("open")).toBe(true);
  });

  it("does not POST anything for a chevron click", () => {
    // Arrange
    const { mount, sidebar, calls } = harness();
    sidebar.update(roster());
    // Act
    click(mount.querySelector("[data-chev]")!);
    // Assert
    expect(calls).toHaveLength(0);
  });

  it("keeps an opened panel open across a roster re-push", () => {
    // Arrange
    const { mount, sidebar } = harness();
    sidebar.update(roster());
    click(mount.querySelector("[data-chev]")!);
    // Act — Emacs pushes again; the rendered DOM is rebuilt wholesale.
    sidebar.update(roster());
    // Assert
    expect(mount.querySelector(".ws")!.classList.contains("open")).toBe(true);
  });

  it("closes an opened panel on the chevron's second click", () => {
    // Arrange
    const { mount, sidebar } = harness();
    sidebar.update(roster());
    click(mount.querySelector("[data-chev]")!);
    // Act
    click(mount.querySelector("[data-chev]")!);
    // Assert
    expect(mount.querySelector(".ws")!.classList.contains("open")).toBe(false);
  });

  it("opens a row's detail panel from toggleDetail (the keyboard path)", () => {
    // Arrange
    const { mount, sidebar } = harness();
    sidebar.update(roster());
    // Act
    sidebar.toggleDetail("/tmp/ws");
    // Assert
    expect(mount.querySelector(".ws")!.classList.contains("open")).toBe(true);
  });

  it("closes the panel on a second toggleDetail", () => {
    // Arrange
    const { mount, sidebar } = harness();
    sidebar.update(roster());
    sidebar.toggleDetail("/tmp/ws");
    // Act
    sidebar.toggleDetail("/tmp/ws");
    // Assert
    expect(mount.querySelector(".ws")!.classList.contains("open")).toBe(false);
  });

  it("does not POST anything for a toggleDetail", () => {
    // Arrange
    const { sidebar, calls } = harness();
    sidebar.update(roster());
    // Act
    sidebar.toggleDetail("/tmp/ws");
    // Assert
    expect(calls).toHaveLength(0);
  });

  it("surfaces a non-2xx command response in the footer", async () => {
    // Arrange
    vi.spyOn(console, "error").mockImplementation(() => undefined);
    const { mount, sidebar } = harness({ ok: false });
    sidebar.update(roster());
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
    sidebar.update(roster());
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
    sidebar.update(roster());
    click(mount.querySelector(".row")!);
    await vi.advanceTimersByTimeAsync(0);
    // Act
    await vi.advanceTimersByTimeAsync(COMMAND_FAILED_NOTICE_MS);
    // Assert
    expect(mount.querySelector(".sb-err")).toBeNull();
  });
});

describe("installWorkspaceRosterHook", () => {
  it("names the hook the way the Emacs side must call it", () => {
    // Arrange + Act + Assert
    expect(ROSTER_HOOK).toBe("agentReplWorkspaceRoster");
  });

  it("plants the hook under that name", () => {
    // Arrange
    const target: HostGlobal = {};
    const { sidebar } = harness();
    // Act
    installWorkspaceRosterHook(target, sidebar);
    // Assert
    expect(typeof target[ROSTER_HOOK]).toBe("function");
  });

  it("drives the sidebar's update when fired", () => {
    // Arrange
    const target: HostGlobal = {};
    const { mount, sidebar } = harness();
    installWorkspaceRosterHook(target, sidebar);
    // Act — the way an Emacs host script fires it, roster pre-parsed.
    (target[ROSTER_HOOK] as (r: unknown) => void)(roster());
    // Assert
    expect(mount.hidden).toBe(false);
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
    sidebar.update(roster());
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
    sidebar.update(roster());
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
