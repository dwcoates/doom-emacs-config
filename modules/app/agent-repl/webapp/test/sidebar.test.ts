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
  ROSTER_HOOK,
  RepoGroup,
  WorkspaceRoster,
  WorkspaceRow,
  WorkspaceSidebar,
  formatRecency,
  installWorkspaceRosterHook,
  sidebarHtml,
  statusDotHtml,
  validateWorkspaceRoster,
} from "../src/sidebar.js";
import css from "../src/styles.css?raw";

const NOW_MS = 1_700_000_000_000;
const NOW_S = NOW_MS / 1000;

/** A roster row, defaulted to a quiet idle workspace with no family. */
function row(over: Partial<WorkspaceRow> = {}): WorkspaceRow {
  return {
    name: "ws",
    dir: "/tmp/ws",
    status: "idle",
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
  return { key: "doom", label: "doom", folded: false, rows: [row()], ...over };
}

function roster(over: Partial<WorkspaceRoster> = {}): WorkspaceRoster {
  return { repos: [group()], recentlyMerged: null, navDir: null, ...over };
}

/** Render with no panels open, at the fixed clock, with no error note. */
function html(r: WorkspaceRoster, open: ReadonlySet<string> = new Set()): string {
  return sidebarHtml(r, open, NOW_MS, null);
}

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

  it("throws when navDir is neither string nor null", () => {
    // Arrange + Act + Assert
    expect(() => validateWorkspaceRoster(roster({ navDir: 7 as never }))).toThrow(/navDir/);
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

  it("keys a done-viewed dot to its solid-orange class", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("done-viewed")).toContain(`class="st st-done-viewed"`);
  });

  it("keys an idle dot to its hollow-ring class", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("idle")).toContain(`class="st st-idle"`);
  });

  it("keys a dead dot to its grey class", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("dead")).toContain(`class="st st-dead"`);
  });

  it("keys a none dot to the invisible placeholder class", () => {
    // Arrange + Act + Assert
    expect(statusDotHtml("none")).toContain(`class="st st-none"`);
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

  it("only spins the in-flight merge, never the queued one", () => {
    // Arrange + Act — the motion lives in the stylesheet, so it is pinned there.
    const spinning = css.match(/#ws-sidebar \.st-merging \{[^}]*animation: ws-spin/);
    // Assert
    expect(spinning).not.toBeNull();
    expect(css).not.toMatch(/\.st-merge-queued[^{]*\{[^}]*animation/);
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

  it("shows the transient error note in the header", () => {
    // Arrange + Act
    const out = sidebarHtml(roster(), new Set(), NOW_MS, "workspace command failed");
    // Assert
    expect(out).toContain(`<span class="sb-err">workspace command failed</span>`);
  });

  it("omits the error slot while no note is pending", () => {
    // Arrange + Act + Assert
    expect(html(roster())).not.toContain("sb-err");
  });
});

/** A mounted sidebar over a recording fetch, at the fixed clock. */
function harness(opts: { ok?: boolean; reject?: boolean } = {}): {
  mount: HTMLElement;
  sidebar: WorkspaceSidebar;
  calls: Array<{ url: string; body: unknown }>;
} {
  const calls: Array<{ url: string; body: unknown }> = [];
  const fetchFn = ((url: string, init?: RequestInit) => {
    calls.push({ url, body: JSON.parse(String(init?.body)) });
    if (opts.reject) return Promise.reject(new Error("network down"));
    return Promise.resolve({ ok: opts.ok !== false, status: opts.ok === false ? 502 : 200 });
  }) as unknown as typeof fetch;
  const mount = document.createElement("nav");
  mount.hidden = true;
  const sidebar = new WorkspaceSidebar(mount, {
    httpBase: "http://daemon",
    fetchFn,
    now: () => NOW_MS,
  });
  return { mount, sidebar, calls };
}

function click(el: Element): void {
  el.dispatchEvent(new MouseEvent("click", { bubbles: true }));
}

/** Settle the post's promise chain (real timers only). */
const flush = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));

afterEach(() => {
  vi.restoreAllMocks();
  vi.useRealTimers();
});

describe("WorkspaceSidebar", () => {
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

  it("surfaces a non-2xx command response in the header", async () => {
    // Arrange
    const errors = vi.spyOn(console, "error").mockImplementation(() => undefined);
    const { mount, sidebar } = harness({ ok: false });
    sidebar.update(roster());
    // Act
    click(mount.querySelector(".row")!);
    await flush();
    // Assert
    expect(mount.querySelector(".sb-err")!.textContent).toBe(COMMAND_FAILED_NOTICE);
    expect(errors).toHaveBeenCalled();
  });

  it("surfaces a network failure in the header", async () => {
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
