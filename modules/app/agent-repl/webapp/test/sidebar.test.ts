import { describe, expect, it } from "vitest";

import type {
  Entry,
  Group,
  MergeCommitRow,
  RosterEntry,
  Section,
  SidebarActionName,
  Snapshot,
} from "../src/sidebar-types.js";
import {
  EMACS_ONLY_ACTIONS,
  OpContext,
  SIDEBAR_HOOK,
  STANDALONE_TOAST,
  actionTargets,
  dragPayload,
  entryMatches,
  filterSnapshot,
  findEntry,
  formatDuration,
  formatMergeAgo,
  formatMergeClock,
  hoverCardHtml,
  indexRoster,
  installSidebarHook,
  isStandalone,
  makeGatedPost,
  mergedTargets,
  navigableRefs,
  notificationEvents,
  rosterBadgesHtml,
  runSidebarOp,
  showCommitRequest,
  sidebarHeaderHtml,
  sidebarWidth,
  snapshotHtml,
  snapshotTicks,
  stepCursor,
  syncCursor,
  wsOf,
} from "../src/sidebar.js";

/** A fixed "now" (epoch seconds) so clock renders are deterministic. */
const NOW = 1_752_690_000;

function entry(over: Partial<Entry> & { ws: string }): Entry {
  return {
    depth: 0,
    section: "main",
    session_id: null,
    status: null,
    glyph: "·",
    name_color: null,
    priority: null,
    summary: "—",
    summary_pending: false,
    dirty: false,
    hidden: false,
    marked: false,
    expanded: false,
    current: false,
    help: `Workspace: ${over.ws}`,
    detail: null,
    ...over,
  };
}

function rosterEntry(over: Partial<RosterEntry> = {}): RosterEntry {
  return {
    session_id: "s_1",
    turn_active: false,
    pending_permissions: [],
    queue: [],
    turn_preview: "",
    total_cost_usd: 0,
    hibernated: false,
    ...over,
  };
}

function group(over: Partial<Group> & { key: string }): Group {
  return { label: over.key, folded: false, entries: [], ...over };
}

function section(over: Partial<Section> & { id: string }): Section {
  return { label: over.id.toUpperCase(), count: 0, groups: [], ...over };
}

function snapshot(over: Partial<Snapshot> = {}): Snapshot {
  return {
    type: "workspace-snapshot",
    sidebar_version: 1,
    generated_at: NOW,
    current_ws: null,
    sidebar_visible: true,
    merge_slow_threshold: 3,
    marks: [],
    last_action_result: null,
    sections: [],
    ...over,
  };
}

function commit(over: Partial<MergeCommitRow> & Pick<MergeCommitRow, "sha" | "state">): MergeCommitRow {
  return { kind: "commit", subject: "a subject", ws: "ws-a", ...over };
}

describe("formatMergeAgo", () => {
  it("compounds every unit from the largest non-zero one down to seconds", () => {
    // Arrange + Act + Assert — 1820s is 30 whole minutes plus 20 seconds.
    expect(formatMergeAgo(1820)).toBe("30m 20s ago");
  });

  it("keeps an intermediate zero unit so the readout stays contiguous", () => {
    // Arrange + Act + Assert — 3601s is 1h, no minutes, 1s: the 0m survives.
    expect(formatMergeAgo(3601)).toBe("1h 0m 1s ago");
  });

  it("drops leading zero units above the first non-zero one", () => {
    // Arrange + Act + Assert — no phantom "0d 0h 0m" in front.
    expect(formatMergeAgo(59)).toBe("59s ago");
  });

  it("always ends in a seconds field even when it is zero", () => {
    // Arrange + Act + Assert
    expect(formatMergeAgo(60)).toBe("1m 0s ago");
  });

  it("reads a zero elapsed as zero seconds ago", () => {
    // Arrange + Act + Assert
    expect(formatMergeAgo(0)).toBe("0s ago");
  });

  it("clamps a negative elapsed to zero rather than counting backward", () => {
    // Arrange + Act + Assert — a stamp fractionally ahead of this clock.
    expect(formatMergeAgo(-5)).toBe("0s ago");
  });

  it("carries a day-scale elapsed through all four units", () => {
    // Arrange + Act + Assert — 90061s = 1d 1h 1m 1s.
    expect(formatMergeAgo(90_061)).toBe("1d 1h 1m 1s ago");
  });
});

describe("formatDuration", () => {
  it("reports a sub-minute span in whole seconds", () => {
    // Arrange + Act + Assert
    expect(formatDuration(45)).toBe("45s ago");
  });

  it("switches to whole minutes exactly at one minute", () => {
    // Arrange + Act + Assert
    expect(formatDuration(60)).toBe("1m ago");
  });

  it("keeps a sub-hour span in whole minutes", () => {
    // Arrange + Act + Assert
    expect(formatDuration(300)).toBe("5m ago");
  });

  it("switches to one-decimal hours exactly at one hour", () => {
    // Arrange + Act + Assert
    expect(formatDuration(3600)).toBe("1.0h ago");
  });

  it("carries the fractional hour in the decimal", () => {
    // Arrange + Act + Assert — 5400s is an hour and a half.
    expect(formatDuration(5400)).toBe("1.5h ago");
  });

  it("switches to one-decimal days exactly at one day", () => {
    // Arrange + Act + Assert
    expect(formatDuration(86_400)).toBe("1.0d ago");
  });

  it("clamps a negative span to zero seconds", () => {
    // Arrange + Act + Assert
    expect(formatDuration(-3)).toBe("0s ago");
  });
});

describe("formatMergeClock", () => {
  it("renders minutes and zero-padded seconds", () => {
    // Arrange + Act + Assert
    expect(formatMergeClock(65)).toBe("1:05");
  });

  it("renders a sub-minute elapsed with a zero minute field", () => {
    // Arrange + Act + Assert
    expect(formatMergeClock(3)).toBe("0:03");
  });
});

describe("snapshotHtml sections", () => {
  it("renders sections in snapshot order with counted headers", () => {
    // Arrange
    const snap = snapshot({
      sections: [
        section({ id: "main", label: "MAIN", count: 2 }),
        section({ id: "merged", label: "MERGED", count: 1 }),
      ],
    });
    // Act
    const html = snapshotHtml(snap, null, NOW);
    // Assert
    expect(html.indexOf("MAIN (2)")).toBeGreaterThanOrEqual(0);
    expect(html.indexOf("MAIN (2)")).toBeLessThan(html.indexOf("MERGED (1)"));
  });

  it("shows the (none) placeholder under an empty section", () => {
    // Arrange
    const snap = snapshot({ sections: [section({ id: "hidden", label: "HIDDEN" })] });
    // Act + Assert
    expect(snapshotHtml(snap, null, NOW)).toContain(`<div class="sidebar-empty">(none)</div>`);
  });

  it("renders the merge queue ahead of the workspace sections", () => {
    // Arrange — drawer order: MERGE QUEUE first.
    const snap = snapshot({
      merge_queue: { count: 1, rows: [commit({ sha: "82e4583", state: "pending" })] },
      sections: [section({ id: "main", label: "MAIN", count: 0 })],
    });
    // Act
    const html = snapshotHtml(snap, null, NOW);
    // Assert
    expect(html.indexOf("MERGE QUEUE (1)")).toBeLessThan(html.indexOf("MAIN (0)"));
  });
});

describe("snapshotHtml groups", () => {
  it("renders a folded group as its header only", () => {
    // Arrange — even entries smuggled into a folded group must not render.
    const snap = snapshot({
      sections: [
        section({
          id: "main",
          label: "MAIN",
          count: 1,
          groups: [group({ key: "/r/.git", label: "doom", folded: true, entries: [entry({ ws: "alpha" })] })],
        }),
      ],
    });
    // Act
    const html = snapshotHtml(snap, null, NOW);
    // Assert
    expect(html).toContain("▸");
    expect(html).not.toContain("alpha");
  });

  it("renders an unfolded group with the expanded fold glyph", () => {
    // Arrange
    const snap = snapshot({
      sections: [
        section({
          id: "main",
          label: "MAIN",
          count: 1,
          groups: [group({ key: "/r/.git", label: "doom", entries: [entry({ ws: "alpha" })] })],
        }),
      ],
    });
    // Act + Assert
    expect(snapshotHtml(snap, null, NOW)).toContain("▾");
  });
});

describe("snapshotHtml entries", () => {
  const oneEntry = (e: Entry): Snapshot =>
    snapshot({
      sections: [
        section({ id: "main", label: "MAIN", count: 1, groups: [group({ key: "/r/.git", entries: [e] })] }),
      ],
    });

  it("shows the state glyph, the name, and the summary", () => {
    // Arrange
    const snap = oneEntry(entry({ ws: "alpha", glyph: "⌛", summary: "fixing the tests" }));
    // Act
    const html = snapshotHtml(snap, null, NOW);
    // Assert
    expect(html).toContain("⌛");
    expect(html).toContain("alpha");
    expect(html).toContain("fixing the tests");
  });

  it("addresses a workspace row as ws:<name> for the cursor and clicks", () => {
    // Arrange + Act + Assert
    const html = snapshotHtml(oneEntry(entry({ ws: "alpha" })), null, NOW);
    expect(html).toContain(`data-entry="ws:alpha"`);
  });

  it("puts the cursor arrow in the gutter of the row under the cursor", () => {
    // Arrange + Act
    const html = snapshotHtml(oneEntry(entry({ ws: "alpha" })), "ws:alpha", NOW);
    // Assert
    expect(html).toContain(`<span class="sidebar-cursor-arrow">▶</span>`);
    expect(html).toContain("sidebar-at-cursor");
  });

  it("gives the mark dot the gutter even over the cursor arrow", () => {
    // Arrange — mark precedence (drawer C15).
    const html = snapshotHtml(oneEntry(entry({ ws: "alpha", marked: true })), "ws:alpha", NOW);
    // Assert
    expect(html).toContain(`<span class="sidebar-marked">●</span>`);
    expect(html).not.toContain("▶");
  });

  it("renders the dirty dot only on a dirty workspace", () => {
    // Arrange + Act + Assert
    expect(snapshotHtml(oneEntry(entry({ ws: "alpha", dirty: true })), null, NOW)).toContain("sidebar-dirty");
    expect(snapshotHtml(oneEntry(entry({ ws: "alpha" })), null, NOW)).not.toContain("sidebar-dirty");
  });

  it("renders the priority chip only when a priority is set", () => {
    // Arrange + Act + Assert
    const html = snapshotHtml(oneEntry(entry({ ws: "alpha", priority: "p1" })), null, NOW);
    expect(html).toContain(`<span class="sidebar-priority">p1</span>`);
    expect(snapshotHtml(oneEntry(entry({ ws: "alpha" })), null, NOW)).not.toContain("sidebar-priority");
  });

  it("dims a HIDDEN-section entry", () => {
    // Arrange + Act + Assert
    expect(snapshotHtml(oneEntry(entry({ ws: "alpha", hidden: true })), null, NOW)).toContain("sidebar-dim");
  });

  it("ticks the merged-ago clock in an expanded entry's detail", () => {
    // Arrange — merged 1820s before NOW.
    const e = entry({ ws: "alpha", expanded: true, detail: { merge_completed_at: NOW - 1820 } });
    // Act + Assert
    expect(snapshotHtml(oneEntry(e), null, NOW)).toContain("30m 20s ago");
  });

  it("suppresses the ahead-source line when the source branch is the trunk", () => {
    // Arrange — the line would restate "ahead <trunk>" verbatim.
    const e = entry({
      ws: "alpha",
      expanded: true,
      detail: { trunk: "master", ahead_master: 3, source_branch: "master", ahead_source: 3 },
    });
    // Act + Assert
    expect(snapshotHtml(oneEntry(e), null, NOW)).not.toContain("sidebar-detail-ahead-source");
  });

  it("escapes a malicious workspace name instead of injecting markup", () => {
    // Arrange
    const evil = `<img src=x onerror="alert(1)">`;
    const html = snapshotHtml(oneEntry(entry({ ws: evil, help: `Workspace: ${evil}` })), null, NOW);
    // Act + Assert
    expect(html).not.toContain("<img");
    expect(html).toContain("&lt;img src=x onerror=&quot;alert(1)&quot;&gt;");
  });
});

describe("snapshotHtml merge queue", () => {
  const withRows = (rows: NonNullable<Snapshot["merge_queue"]>["rows"], count = rows.length): Snapshot =>
    snapshot({ merge_queue: { count, rows } });

  it("marks the applying commit with the ⟳ glyph and the current subject face", () => {
    // Arrange
    const html = snapshotHtml(
      withRows([commit({ sha: "82e4583", state: "current", subject: "feat: x" })]),
      null,
      NOW,
    );
    // Assert
    expect(html).toContain("⟳");
    expect(html).toContain("sidebar-mq-current");
  });

  it("mutes a queued commit's subject", () => {
    // Arrange + Act + Assert
    const html = snapshotHtml(withRows([commit({ sha: "82e4583", state: "pending" })]), null, NOW);
    expect(html).toContain("sidebar-mq-pending");
  });

  it("marks a halted commit with the ⛔ glyph", () => {
    // Arrange + Act + Assert
    expect(snapshotHtml(withRows([commit({ sha: "82e4583", state: "halted" })]), null, NOW)).toContain("⛔");
  });

  it("shows the elapsed clock once the current commit passes the slow threshold", () => {
    // Arrange — 10s elapsed against a 3s threshold.
    const html = snapshotHtml(
      withRows([commit({ sha: "82e4583", state: "current", started_at: NOW - 10 })]),
      null,
      NOW,
    );
    // Assert
    expect(html).toContain(`<span class="sidebar-mq-elapsed">0:10</span>`);
  });

  it("keeps a fast commit's clock quiet below the slow threshold", () => {
    // Arrange — 1s elapsed against a 3s threshold: the clock's presence is
    // itself the signal, so it must not appear.
    const html = snapshotHtml(
      withRows([commit({ sha: "82e4583", state: "current", started_at: NOW - 1 })]),
      null,
      NOW,
    );
    // Assert
    expect(html).not.toContain("sidebar-mq-elapsed");
  });

  it("renders a conflict row with the 💥 glyph and the conflict detail line", () => {
    // Arrange
    const html = snapshotHtml(
      withRows([
        commit({
          sha: "82e4583",
          state: "conflict",
          conflict_files: 2,
          resolver_phase: "analyzing",
          resolver_started_at: NOW - 5,
        }),
      ]),
      null,
      NOW,
    );
    // Assert
    expect(html).toContain("💥");
    expect(html).toContain("2 files unmerged · resolver: analyzing 0:05");
  });

  it("renders a project separator as a non-navigable group-style header", () => {
    // Arrange
    const html = snapshotHtml(withRows([{ kind: "separator", project: "doom" }], 1), null, NOW);
    // Assert
    expect(html).toContain("doom");
    expect(html).not.toContain("data-entry");
  });
});

describe("snapshotHtml chrome", () => {
  it("shows the stale badge once the snapshot outlives the heartbeat window", () => {
    // Arrange — 91s old against the 90s threshold.
    const snap = snapshot({ generated_at: NOW - 91 });
    // Act + Assert
    expect(snapshotHtml(snap, null, NOW)).toContain("sidebar-stale");
  });

  it("keeps the stale badge off a fresh snapshot", () => {
    // Arrange + Act + Assert
    expect(snapshotHtml(snapshot(), null, NOW)).not.toContain("sidebar-stale");
  });

  it("renders the toast strip with its dismiss control when given an error", () => {
    // Arrange + Act
    const html = snapshotHtml(snapshot(), null, NOW, "Cannot kill a MERGED workspace");
    // Assert
    expect(html).toContain("sidebar-toast");
    expect(html).toContain("Cannot kill a MERGED workspace");
    expect(html).toContain("data-toast-close");
  });
});

describe("navigableRefs", () => {
  it("lists group headers and workspace rows exactly as rendered", () => {
    // Arrange
    const snap = snapshot({
      sections: [
        section({
          id: "main",
          label: "MAIN",
          count: 2,
          groups: [group({ key: "/r/.git", entries: [entry({ ws: "a" }), entry({ ws: "b" })] })],
        }),
      ],
    });
    // Act + Assert
    expect(navigableRefs(snap)).toEqual(["repo:/r/.git", "ws:a", "ws:b"]);
  });

  it("contributes only the header for a folded group", () => {
    // Arrange
    const snap = snapshot({
      sections: [
        section({
          id: "main",
          label: "MAIN",
          count: 1,
          groups: [group({ key: "/r/.git", folded: true, entries: [entry({ ws: "a" })] })],
        }),
      ],
    });
    // Act + Assert
    expect(navigableRefs(snap)).toEqual(["repo:/r/.git"]);
  });
});

describe("stepCursor", () => {
  const refs = ["repo:/r/.git", "ws:a", "ws:b"];

  it("advances to the next entry", () => {
    // Arrange + Act + Assert
    expect(stepCursor(refs, "ws:a", +1)).toBe("ws:b");
  });

  it("retreats to the previous entry", () => {
    // Arrange + Act + Assert
    expect(stepCursor(refs, "ws:a", -1)).toBe("repo:/r/.git");
  });

  it("clamps at the end of the list rather than wrapping", () => {
    // Arrange + Act + Assert
    expect(stepCursor(refs, "ws:b", +1)).toBe("ws:b");
  });

  it("lands a null cursor on the first entry", () => {
    // Arrange + Act + Assert
    expect(stepCursor(refs, null, +1)).toBe("repo:/r/.git");
  });

  it("has no cursor over an empty list", () => {
    // Arrange + Act + Assert
    expect(stepCursor([], "ws:a", +1)).toBeNull();
  });
});

describe("syncCursor", () => {
  const refs = ["repo:/r/.git", "ws:a", "ws:b"];

  it("snaps to the active workspace when current_ws changed", () => {
    // Arrange + Act + Assert — the drawer's cursor-follows-switch rule.
    expect(syncCursor({ cursor: "ws:a", refs, prevCurrent: "a", current: "b" })).toBe("ws:b");
  });

  it("keeps the cursor put while current_ws is unchanged", () => {
    // Arrange + Act + Assert — merely re-rendering must not steal the cursor.
    expect(syncCursor({ cursor: "ws:a", refs, prevCurrent: "b", current: "b" })).toBe("ws:a");
  });

  it("falls back to the first entry when the cursored row vanished", () => {
    // Arrange + Act + Assert
    expect(syncCursor({ cursor: "ws:gone", refs, prevCurrent: "b", current: "b" })).toBe(
      "repo:/r/.git",
    );
  });

  it("has no cursor when the list is empty", () => {
    // Arrange + Act + Assert
    expect(syncCursor({ cursor: null, refs: [], prevCurrent: null, current: null })).toBeNull();
  });
});

describe("actionTargets", () => {
  it("prefers the marked set when marks exist", () => {
    // Arrange + Act + Assert — marks-or-point (drawer K21).
    expect(actionTargets(["m1", "m2"], "ws:a")).toEqual(["m1", "m2"]);
  });

  it("falls back to the workspace under the cursor when nothing is marked", () => {
    // Arrange + Act + Assert
    expect(actionTargets([], "ws:a")).toEqual(["a"]);
  });

  it("targets nothing from a group header with no marks", () => {
    // Arrange + Act + Assert — the caller no-ops on an empty target set.
    expect(actionTargets([], "repo:/r/.git")).toEqual([]);
  });
});

describe("mergedTargets", () => {
  it("keeps only the targets that sit in the MERGED section", () => {
    // Arrange
    const snap = snapshot({
      sections: [
        section({
          id: "main",
          label: "MAIN",
          count: 1,
          groups: [group({ key: "/r/.git", entries: [entry({ ws: "a" })] })],
        }),
        section({
          id: "merged",
          label: "MERGED",
          count: 1,
          groups: [group({ key: "/r/.git", entries: [entry({ ws: "b", section: "merged" })] })],
        }),
      ],
    });
    // Act + Assert
    expect(mergedTargets(snap, ["a", "b"])).toEqual(["b"]);
  });
});

describe("wsOf", () => {
  it("extracts the workspace name from a ws ref", () => {
    // Arrange + Act + Assert
    expect(wsOf("ws:alpha")).toBe("alpha");
  });

  it("yields no workspace for a repo ref", () => {
    // Arrange + Act + Assert
    expect(wsOf("repo:/r/.git")).toBeNull();
  });
});

describe("snapshotTicks", () => {
  it("ticks while a merge commit carries a running clock", () => {
    // Arrange
    const snap = snapshot({
      merge_queue: { count: 1, rows: [commit({ sha: "82e4583", state: "current", started_at: NOW })] },
    });
    // Act + Assert
    expect(snapshotTicks(snap)).toBe(true);
  });

  it("ticks while an expanded entry shows an ago clock", () => {
    // Arrange
    const snap = snapshot({
      sections: [
        section({
          id: "main",
          label: "MAIN",
          count: 1,
          groups: [
            group({ key: "/r/.git", entries: [entry({ ws: "a", detail: { last_prompt_at: NOW } })] }),
          ],
        }),
      ],
    });
    // Act + Assert
    expect(snapshotTicks(snap)).toBe(true);
  });

  it("stays quiet when nothing on screen moves with time", () => {
    // Arrange
    const snap = snapshot({
      sections: [
        section({
          id: "main",
          label: "MAIN",
          count: 1,
          groups: [group({ key: "/r/.git", entries: [entry({ ws: "a" })] })],
        }),
      ],
    });
    // Act + Assert
    expect(snapshotTicks(snap)).toBe(false);
  });
});

describe("sidebarWidth", () => {
  it("turns the snapshot's width_px into a flex-basis length", () => {
    // Arrange + Act + Assert
    expect(sidebarWidth(snapshot({ width_px: 420 }))).toBe("420px");
  });

  it("yields the empty string when the snapshot carries no width", () => {
    // Arrange + Act + Assert — "" hands the width back to the CSS var.
    expect(sidebarWidth(snapshot())).toBe("");
  });

  it("treats a non-positive width as absent", () => {
    // Arrange + Act + Assert — a 0px sidebar would be an invisible trap.
    expect(sidebarWidth(snapshot({ width_px: 0 }))).toBe("");
  });
});

describe("isStandalone", () => {
  it("reads a page without parent_ws as a standalone browser view", () => {
    // Arrange + Act + Assert
    expect(isStandalone(new URLSearchParams("sidebar=1"))).toBe(true);
  });

  it("reads a page with parent_ws as Emacs-hosted", () => {
    // Arrange + Act + Assert
    expect(isStandalone(new URLSearchParams("sidebar=1&parent_ws=doom"))).toBe(false);
  });

  it("counts a present-but-empty parent_ws as hosted", () => {
    // Arrange + Act + Assert — presence, not value, is the signal.
    expect(isStandalone(new URLSearchParams("parent_ws="))).toBe(false);
  });
});

describe("makeGatedPost", () => {
  type SentCall = [SidebarActionName, string[], Record<string, unknown>, boolean];
  const harness = (standalone: boolean) => {
    const sent: SentCall[] = [];
    const toasts: string[] = [];
    const post = makeGatedPost(
      standalone,
      (action, targets, args = {}, confirmed = false) => {
        sent.push([action, targets, args, confirmed]);
      },
      (toast) => toasts.push(toast),
    );
    return { post, sent, toasts };
  };

  it("refuses an Emacs-only action in a standalone view with the toast", () => {
    // Arrange
    const { post, sent, toasts } = harness(true);
    // Act
    post("visit", ["a"]);
    // Assert
    expect(sent).toEqual([]);
    expect(toasts).toEqual([STANDALONE_TOAST]);
  });

  it("lets an allowed action through in a standalone view", () => {
    // Arrange
    const { post, sent, toasts } = harness(true);
    // Act
    post("send-prompt", ["a"], { prompt: "hi" });
    // Assert
    expect(sent).toEqual([["send-prompt", ["a"], { prompt: "hi" }, false]]);
    expect(toasts).toEqual([]);
  });

  it("lets every action through when an Emacs host is attached", () => {
    // Arrange
    const { post, sent, toasts } = harness(false);
    // Act
    post("nuke", ["a"], {}, true);
    // Assert
    expect(sent).toEqual([["nuke", ["a"], {}, true]]);
    expect(toasts).toEqual([]);
  });
});

describe("EMACS_ONLY_ACTIONS", () => {
  it("blocks exactly the host-bound verbs", () => {
    // Arrange — the contract's standalone-blocked list, verbatim.
    const blocked: SidebarActionName[] = [
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
    ];
    // Act + Assert
    for (const action of blocked) expect(EMACS_ONLY_ACTIONS.has(action)).toBe(true);
  });

  it("keeps the still-useful verbs unblocked", () => {
    // Arrange — navigation-adjacent, marks, prompts, and interrupts work anywhere.
    const allowed: SidebarActionName[] = [
      "toggle-expand",
      "toggle-fold",
      "refresh",
      "toggle-mark",
      "clear-marks",
      "send-prompt",
      "interrupt",
    ];
    // Act + Assert
    for (const action of allowed) expect(EMACS_ONLY_ACTIONS.has(action)).toBe(false);
  });
});

describe("sidebarHeaderHtml", () => {
  it("shows the standalone chip in a standalone view", () => {
    // Arrange + Act + Assert
    expect(sidebarHeaderHtml(true)).toContain("sidebar-standalone-chip");
  });

  it("omits the chip when an Emacs host is attached", () => {
    // Arrange + Act + Assert
    expect(sidebarHeaderHtml(false)).not.toContain("sidebar-standalone-chip");
  });

  it("always carries the search input", () => {
    // Arrange + Act + Assert
    expect(sidebarHeaderHtml(false)).toContain(`class="sidebar-search"`);
    expect(sidebarHeaderHtml(true)).toContain(`class="sidebar-search"`);
  });
});

describe("priority badges", () => {
  const withPriority = (images?: Record<string, string>): Snapshot =>
    snapshot({
      priority_images: images,
      sections: [
        section({
          id: "main",
          label: "MAIN",
          count: 1,
          groups: [group({ key: "/r/.git", entries: [entry({ ws: "alpha", priority: "p1" })] })],
        }),
      ],
    });

  it("renders the image badge when priority_images carries the priority name", () => {
    // Arrange + Act
    const html = snapshotHtml(withPriority({ p1: "data:image/png;base64,AA==" }), null, NOW);
    // Assert
    expect(html).toContain(
      `<img class="sidebar-priority-img" src="data:image/png;base64,AA==" alt="p1">`,
    );
    expect(html).not.toContain(`<span class="sidebar-priority">`);
  });

  it("falls back to the text chip when the priority has no image", () => {
    // Arrange + Act — the map exists but names a different priority.
    const html = snapshotHtml(withPriority({ p2: "data:image/png;base64,AA==" }), null, NOW);
    // Assert
    expect(html).toContain(`<span class="sidebar-priority">p1</span>`);
    expect(html).not.toContain("sidebar-priority-img");
  });

  it("attribute-escapes the data URI without rejecting it", () => {
    // Arrange — a URI carrying quote/angle characters must not break out
    // of the src attribute, but Emacs owns it and it is never dropped.
    const html = snapshotHtml(withPriority({ p1: `data:image/svg+xml,<svg onload="x">` }), null, NOW);
    // Act + Assert
    expect(html).toContain(`src="data:image/svg+xml,&lt;svg onload=&quot;x&quot;&gt;"`);
    expect(html).not.toContain(`<svg`);
  });
});

describe("entryMatches", () => {
  it("matches the workspace name case-insensitively", () => {
    // Arrange + Act + Assert
    expect(entryMatches(entry({ ws: "Alpha-Fix" }), "alpha")).toBe(true);
  });

  it("matches the summary text", () => {
    // Arrange + Act + Assert
    expect(entryMatches(entry({ ws: "a", summary: "Fixing the tests" }), "TESTS")).toBe(true);
  });

  it("rejects a row matching neither field", () => {
    // Arrange + Act + Assert
    expect(entryMatches(entry({ ws: "a", summary: "docs" }), "tests")).toBe(false);
  });
});

describe("filterSnapshot", () => {
  const twoGroups = (): Snapshot =>
    snapshot({
      sections: [
        section({
          id: "main",
          label: "MAIN",
          count: 3,
          groups: [
            group({ key: "/r1/.git", label: "doom", entries: [entry({ ws: "alpha" }), entry({ ws: "beta" })] }),
            group({ key: "/r2/.git", label: "chess", entries: [entry({ ws: "gamma" })] }),
          ],
        }),
      ],
    });

  it("keeps only the rows matching the query", () => {
    // Arrange + Act
    const filtered = filterSnapshot(twoGroups(), "alp");
    // Assert
    expect(navigableRefs(filtered)).toEqual(["repo:/r1/.git", "ws:alpha"]);
  });

  it("hides a group header whose rows all filtered out", () => {
    // Arrange + Act — /r2's gamma does not match, so its header goes too.
    const filtered = filterSnapshot(twoGroups(), "alpha");
    // Assert
    expect(filtered.sections[0].groups.map((g) => g.key)).toEqual(["/r1/.git"]);
  });

  it("keeps the section count as the snapshot's total, not the filtered view's", () => {
    // Arrange + Act
    const filtered = filterSnapshot(twoGroups(), "alpha");
    // Assert
    expect(filtered.sections[0].count).toBe(3);
    expect(snapshotHtml(filtered, null, NOW)).toContain("MAIN (3)");
  });

  it("hides a folded group under any non-blank query", () => {
    // Arrange — a folded group ships entries: [], so nothing can survive.
    const snap = snapshot({
      sections: [
        section({
          id: "main",
          label: "MAIN",
          count: 1,
          groups: [group({ key: "/r/.git", folded: true, entries: [] })],
        }),
      ],
    });
    // Act + Assert
    expect(filterSnapshot(snap, "x").sections[0].groups).toEqual([]);
  });

  it("is the identity on a blank query", () => {
    // Arrange
    const snap = twoGroups();
    // Act + Assert — the very same object, not a rebuilt copy.
    expect(filterSnapshot(snap, "")).toBe(snap);
  });

  it("treats a whitespace-only query as blank", () => {
    // Arrange
    const snap = twoGroups();
    // Act + Assert
    expect(filterSnapshot(snap, "   ")).toBe(snap);
  });

  it("leaves the merge queue untouched", () => {
    // Arrange — the filter is over workspace rows only.
    const snap = snapshot({
      merge_queue: { count: 1, rows: [commit({ sha: "82e4583", state: "pending" })] },
      sections: [],
    });
    // Act + Assert
    expect(filterSnapshot(snap, "nomatch").merge_queue).toEqual(snap.merge_queue);
  });
});

describe("indexRoster", () => {
  it("indexes the listing by session id", () => {
    // Arrange
    const a = rosterEntry({ session_id: "s_a" });
    const b = rosterEntry({ session_id: "s_b" });
    // Act + Assert
    expect(indexRoster([a, b])).toEqual({ s_a: a, s_b: b });
  });
});

describe("rosterBadgesHtml", () => {
  it("shows the queue-depth badge when messages are queued", () => {
    // Arrange + Act + Assert
    expect(rosterBadgesHtml(rosterEntry({ queue: [1, 2] }))).toContain("⧉ 2");
  });

  it("keeps the queue badge off an empty queue", () => {
    // Arrange + Act + Assert
    expect(rosterBadgesHtml(rosterEntry())).not.toContain("sidebar-queue-badge");
  });

  it("renders one-click allow/deny buttons against the first pending permission", () => {
    // Arrange
    const html = rosterBadgesHtml(
      rosterEntry({ session_id: "s_x", pending_permissions: ["req-1", "req-2"] }),
    );
    // Act + Assert — both buttons target req-1; the shim gates the rest.
    expect(html).toContain(`data-perm="allow"`);
    expect(html).toContain(`data-perm="deny"`);
    expect(html).toContain(`data-perm-sid="s_x"`);
    expect(html).toContain(`data-perm-rid="req-1"`);
    expect(html).not.toContain("req-2");
  });

  it("shows the cost chip with exactly two decimals", () => {
    // Arrange + Act + Assert
    expect(rosterBadgesHtml(rosterEntry({ total_cost_usd: 1.239 }))).toContain("$1.24");
  });

  it("keeps the cost chip off a zero-cost session", () => {
    // Arrange + Act + Assert
    expect(rosterBadgesHtml(rosterEntry())).not.toContain("sidebar-cost-chip");
  });

  it("renders nothing without a roster entry to join", () => {
    // Arrange + Act + Assert
    expect(rosterBadgesHtml(null)).toBe("");
  });
});

describe("snapshotHtml roster join", () => {
  const oneJoined = (sessionId: string | null): Snapshot =>
    snapshot({
      sections: [
        section({
          id: "main",
          label: "MAIN",
          count: 1,
          groups: [group({ key: "/r/.git", entries: [entry({ ws: "alpha", session_id: sessionId })] })],
        }),
      ],
    });

  it("decorates a row whose session_id is in the roster", () => {
    // Arrange
    const roster = { s_1: rosterEntry({ session_id: "s_1", queue: [1] }) };
    // Act
    const html = snapshotHtml(oneJoined("s_1"), null, NOW, null, { roster });
    // Assert
    expect(html).toContain("sidebar-queue-badge");
  });

  it("leaves a row without a session id bare", () => {
    // Arrange
    const roster = { s_1: rosterEntry({ session_id: "s_1", queue: [1] }) };
    // Act
    const html = snapshotHtml(oneJoined(null), null, NOW, null, { roster });
    // Assert
    expect(html).not.toContain("sidebar-queue-badge");
  });
});

describe("hoverCardHtml", () => {
  const richEntry = (): Entry =>
    entry({
      ws: "alpha",
      status: "thinking",
      priority: "p1",
      summary: "fixing the tests",
      session_id: "s_abcdef123456",
      detail: { branch: "DWC/x", dirty_count: 4 },
    });

  it("carries name, status, priority, and summary", () => {
    // Arrange + Act
    const html = hoverCardHtml(richEntry());
    // Assert
    expect(html).toContain("alpha");
    expect(html).toContain("thinking");
    expect(html).toContain("p1");
    expect(html).toContain("fixing the tests");
  });

  it("shortens the session id", () => {
    // Arrange + Act
    const html = hoverCardHtml(richEntry());
    // Assert
    expect(html).toContain("s_abcdef12");
    expect(html).not.toContain("s_abcdef123456");
  });

  it("adds the roster's queue depth, cost, and turn preview", () => {
    // Arrange
    const roster = rosterEntry({ queue: [1, 2, 3], total_cost_usd: 0.5, turn_preview: "Reading store.ts…" });
    // Act
    const html = hoverCardHtml(richEntry(), roster);
    // Assert
    expect(html).toContain("3");
    expect(html).toContain("$0.50");
    expect(html).toContain("Reading store.ts…");
  });

  it("omits the turn-preview row while no turn streams", () => {
    // Arrange + Act
    const html = hoverCardHtml(richEntry(), rosterEntry());
    // Assert
    expect(html).not.toContain("turn:");
  });

  it("carries the cached detail fields the snapshot shipped", () => {
    // Arrange + Act
    const html = hoverCardHtml(richEntry());
    // Assert
    expect(html).toContain("DWC/x");
    expect(html).toContain("4 files");
  });

  it("escapes snapshot text instead of injecting markup", () => {
    // Arrange
    const evil = entry({ ws: "a", summary: `<img src=x onerror="alert(1)">` });
    // Act
    const html = hoverCardHtml(evil);
    // Assert
    expect(html).not.toContain("<img");
  });
});

describe("notificationEvents", () => {
  const inState = (over: Partial<Entry>): Snapshot =>
    snapshot({
      sections: [
        section({
          id: "main",
          label: "MAIN",
          count: 1,
          groups: [group({ key: "/r/.git", entries: [entry({ ws: "alpha", summary: "fixing tests", ...over })] })],
        }),
      ],
    });

  it("fires when an entry transitions INTO the permission status", () => {
    // Arrange
    const prev = inState({ status: "thinking" });
    const next = inState({ status: "permission" });
    // Act
    const events = notificationEvents(prev, next);
    // Assert
    expect(events).toHaveLength(1);
    expect(events[0].kind).toBe("permission");
    expect(events[0].body).toContain("alpha");
    expect(events[0].body).toContain("fixing tests");
  });

  it("stays quiet while the permission condition persists", () => {
    // Arrange — deduped per workspace+kind: only the edge fires.
    const prev = inState({ status: "permission" });
    const next = inState({ status: "permission" });
    // Act + Assert
    expect(notificationEvents(prev, next)).toEqual([]);
  });

  it("fires when an entry transitions INTO the merged section", () => {
    // Arrange
    const prev = inState({ section: "main" });
    const next = inState({ section: "merged" });
    // Act
    const events = notificationEvents(prev, next);
    // Assert
    expect(events).toHaveLength(1);
    expect(events[0].kind).toBe("merged");
  });

  it("stays quiet while the merged condition persists", () => {
    // Arrange
    const prev = inState({ section: "merged" });
    const next = inState({ section: "merged" });
    // Act + Assert
    expect(notificationEvents(prev, next)).toEqual([]);
  });

  it("counts a row appearing already in the condition as entering it", () => {
    // Arrange — a brand-new workspace waiting on a permission is news.
    const prev = snapshot({ sections: [] });
    const next = inState({ status: "permission" });
    // Act + Assert
    expect(notificationEvents(prev, next)).toHaveLength(1);
  });
});

describe("dragPayload", () => {
  const dragSnap = (): Snapshot =>
    snapshot({
      sections: [
        section({
          id: "main",
          label: "MAIN",
          count: 2,
          groups: [
            group({
              key: "/r/.git",
              entries: [entry({ ws: "a", priority: null }), entry({ ws: "b", priority: "p2" })],
            }),
          ],
        }),
        section({
          id: "merged",
          label: "MERGED",
          count: 1,
          groups: [group({ key: "/r/.git", entries: [entry({ ws: "m", section: "merged" })] })],
        }),
      ],
    });

  it("adopts the drop target's priority", () => {
    // Arrange + Act + Assert
    expect(dragPayload(dragSnap(), "a", "b")).toEqual({
      action: "set-priority",
      targets: ["a"],
      args: { priority: "p2" },
    });
  });

  it("clears the priority when the target row has none", () => {
    // Arrange + Act + Assert
    expect(dragPayload(dragSnap(), "b", "a")).toEqual({
      action: "set-priority",
      targets: ["b"],
      args: { priority: null },
    });
  });

  it("ignores a self-drop", () => {
    // Arrange + Act + Assert
    expect(dragPayload(dragSnap(), "a", "a")).toBeNull();
  });

  it("ignores a drop onto a row outside the MAIN section", () => {
    // Arrange + Act + Assert
    expect(dragPayload(dragSnap(), "a", "m")).toBeNull();
  });

  it("ignores an unknown source row", () => {
    // Arrange + Act + Assert
    expect(dragPayload(dragSnap(), "gone", "b")).toBeNull();
  });
});

describe("snapshotHtml drag affordances", () => {
  const oneRow = (over: Partial<Entry>): string =>
    snapshotHtml(
      snapshot({
        sections: [
          section({
            id: over.section === "merged" ? "merged" : "main",
            label: "S",
            count: 1,
            groups: [group({ key: "/r/.git", entries: [entry({ ws: "alpha", ...over })] })],
          }),
        ],
      }),
      null,
      NOW,
    );

  it("marks MAIN rows draggable", () => {
    // Arrange + Act + Assert
    expect(oneRow({ section: "main" })).toContain(`draggable="true"`);
  });

  it("keeps non-MAIN rows undraggable", () => {
    // Arrange + Act + Assert — reprioritizing is a MAIN-section concept.
    expect(oneRow({ section: "merged" })).not.toContain(`draggable="true"`);
  });
});

describe("showCommitRequest", () => {
  it("targets the commit's workspace with the sha argument", () => {
    // Arrange
    const row = commit({ sha: "82e4583", state: "pending", ws: "ws-a" });
    // Act + Assert
    expect(showCommitRequest(row)).toEqual({
      action: "show-commit",
      targets: ["ws-a"],
      args: { sha: "82e4583" },
    });
  });
});

describe("snapshotHtml merge-commit click affordance", () => {
  it("addresses a commit row by its sha for the show-commit click", () => {
    // Arrange
    const snap = snapshot({
      merge_queue: { count: 1, rows: [commit({ sha: "82e4583", state: "pending" })] },
    });
    // Act
    const html = snapshotHtml(snap, null, NOW);
    // Assert
    expect(html).toContain("data-mq-commit");
    expect(html).toContain(`data-sha="82e4583"`);
  });
});

describe("findEntry", () => {
  const snap = (): Snapshot =>
    snapshot({
      sections: [
        section({
          id: "main",
          label: "MAIN",
          count: 1,
          groups: [group({ key: "/r/.git", entries: [entry({ ws: "alpha" })] })],
        }),
      ],
    });

  it("finds a workspace row by name", () => {
    // Arrange + Act + Assert
    expect(findEntry(snap(), "alpha")?.ws).toBe("alpha");
  });

  it("yields null for an unknown name", () => {
    // Arrange + Act + Assert
    expect(findEntry(snap(), "gone")).toBeNull();
  });
});

describe("installSidebarHook", () => {
  it("plants the hook under the name frontend.el calls", () => {
    // Arrange
    const target: Record<string, unknown> = {};
    // Act
    installSidebarHook(target, () => {});
    // Assert
    expect(typeof target[SIDEBAR_HOOK]).toBe("function");
  });

  it("routes a fired op into the shared dispatcher", () => {
    // Arrange
    const target: Record<string, unknown> = {};
    const ops: string[] = [];
    installSidebarHook(target, (op) => ops.push(op));
    // Act — invoke exactly as an Emacs host script does.
    (target[SIDEBAR_HOOK] as (op: string) => void)("toggle-mark");
    // Assert
    expect(ops).toEqual(["toggle-mark"]);
  });
});

describe("runSidebarOp", () => {
  type PostCall = [SidebarActionName, string[], Record<string, unknown>?, boolean?];

  const opSnap = (over: Partial<Snapshot> = {}): Snapshot =>
    snapshot({
      sections: [
        section({
          id: "main",
          label: "MAIN",
          count: 2,
          groups: [group({ key: "/r/.git", entries: [entry({ ws: "a" }), entry({ ws: "b" })] })],
        }),
        section({
          id: "merged",
          label: "MERGED",
          count: 1,
          groups: [group({ key: "/r/.git", entries: [entry({ ws: "m", section: "merged" })] })],
        }),
      ],
      ...over,
    });

  const harness = (over: Partial<OpContext> = {}) => {
    const posts: PostCall[] = [];
    const moved: Array<string | null> = [];
    const snap = over.snap === undefined ? opSnap() : over.snap;
    const ctx: OpContext = {
      snap,
      refs: snap === null ? [] : navigableRefs(snap),
      cursor: "ws:a",
      post: (action, targets, args, confirmed) => {
        posts.push([action, targets, args, confirmed]);
      },
      moveCursor: (ref) => {
        moved.push(ref);
      },
      confirmFn: () => true,
      promptFn: () => "hello",
      blocked: () => false,
      ...over,
    };
    return { ctx, posts, moved };
  };

  it("visit posts the workspace under the cursor, exactly like Enter", () => {
    // Arrange
    const { ctx, posts } = harness();
    // Act
    runSidebarOp("visit", ctx);
    // Assert
    expect(posts).toEqual([["visit", ["a"], undefined, undefined]]);
  });

  it("visit no-ops from a group header", () => {
    // Arrange
    const { ctx, posts } = harness({ cursor: "repo:/r/.git" });
    // Act
    runSidebarOp("visit", ctx);
    // Assert
    expect(posts).toEqual([]);
  });

  it("nuke posts marks-or-point targets unconfirmed for non-merged rows", () => {
    // Arrange
    const { ctx, posts } = harness();
    // Act
    runSidebarOp("nuke", ctx);
    // Assert
    expect(posts).toEqual([["nuke", ["a"], {}, false]]);
  });

  it("nuke asks the merged-workspace confirm and posts confirmed on yes", () => {
    // Arrange
    const asked: string[] = [];
    const { ctx, posts } = harness({
      cursor: "ws:m",
      confirmFn: (msg) => {
        asked.push(msg);
        return true;
      },
    });
    // Act
    runSidebarOp("nuke", ctx);
    // Assert
    expect(asked).toHaveLength(1);
    expect(asked[0]).toContain("m");
    expect(posts).toEqual([["nuke", ["m"], {}, true]]);
  });

  it("nuke aborts entirely when the merged confirm is declined", () => {
    // Arrange
    const { ctx, posts } = harness({ cursor: "ws:m", confirmFn: () => false });
    // Act
    runSidebarOp("nuke", ctx);
    // Assert
    expect(posts).toEqual([]);
  });

  it("nuke skips the confirm when the gate would refuse anyway", () => {
    // Arrange — confirming an action that can never post would be a lie.
    const asked: string[] = [];
    const { ctx, posts } = harness({
      cursor: "ws:m",
      blocked: () => true,
      confirmFn: (msg) => {
        asked.push(msg);
        return true;
      },
    });
    // Act — post still runs, so the gate can raise its refusal toast.
    runSidebarOp("nuke", ctx);
    // Assert
    expect(asked).toEqual([]);
    expect(posts).toEqual([["nuke", ["m"], {}, false]]);
  });

  it("kill prefers the marked set, exactly like d", () => {
    // Arrange
    const { ctx, posts } = harness({ snap: opSnap({ marks: ["m1", "m2"] }) });
    // Act
    runSidebarOp("kill", ctx);
    // Assert
    expect(posts).toEqual([["kill", ["m1", "m2"], undefined, undefined]]);
  });

  it("send-prompt sends the prompted text, exactly like i", () => {
    // Arrange
    const { ctx, posts } = harness({ promptFn: () => "do the thing" });
    // Act
    runSidebarOp("send-prompt", ctx);
    // Assert
    expect(posts).toEqual([["send-prompt", ["a"], { prompt: "do the thing" }, undefined]]);
  });

  it("send-prompt aborts on a cancelled dialog", () => {
    // Arrange
    const { ctx, posts } = harness({ promptFn: () => null });
    // Act
    runSidebarOp("send-prompt", ctx);
    // Assert
    expect(posts).toEqual([]);
  });

  it("send-prompt aborts on a blank entry", () => {
    // Arrange
    const { ctx, posts } = harness({ promptFn: () => "   " });
    // Act
    runSidebarOp("send-prompt", ctx);
    // Assert
    expect(posts).toEqual([]);
  });

  it("merge-into-source posts marks-or-point targets, exactly like M", () => {
    // Arrange
    const { ctx, posts } = harness();
    // Act
    runSidebarOp("merge-into-source", ctx);
    // Assert
    expect(posts).toEqual([["merge-into-source", ["a"], undefined, undefined]]);
  });

  it("toggle-hidden posts the cursor workspace, exactly like H", () => {
    // Arrange
    const { ctx, posts } = harness();
    // Act
    runSidebarOp("toggle-hidden", ctx);
    // Assert
    expect(posts).toEqual([["toggle-hidden", ["a"], undefined, undefined]]);
  });

  it("toggle-mark posts and advances the cursor, exactly like t", () => {
    // Arrange
    const { ctx, posts, moved } = harness();
    // Act
    runSidebarOp("toggle-mark", ctx);
    // Assert
    expect(posts).toEqual([["toggle-mark", ["a"], undefined, undefined]]);
    expect(moved).toEqual(["ws:b"]);
  });

  it("clear-marks posts with no targets, exactly like u", () => {
    // Arrange
    const { ctx, posts } = harness();
    // Act
    runSidebarOp("clear-marks", ctx);
    // Assert
    expect(posts).toEqual([["clear-marks", [], undefined, undefined]]);
  });

  it("priority-up posts the cursor workspace, exactly like +", () => {
    // Arrange
    const { ctx, posts } = harness();
    // Act
    runSidebarOp("priority-up", ctx);
    // Assert
    expect(posts).toEqual([["priority-up", ["a"], undefined, undefined]]);
  });

  it("priority-down posts the cursor workspace, exactly like -", () => {
    // Arrange
    const { ctx, posts } = harness();
    // Act
    runSidebarOp("priority-down", ctx);
    // Assert
    expect(posts).toEqual([["priority-down", ["a"], undefined, undefined]]);
  });

  it("ignores an unknown op, like an unknown frame type", () => {
    // Arrange
    const { ctx, posts } = harness();
    // Act
    runSidebarOp("frobnicate", ctx);
    // Assert
    expect(posts).toEqual([]);
  });

  it("no-ops before the first snapshot arrives", () => {
    // Arrange
    const { ctx, posts } = harness({ snap: null });
    // Act
    runSidebarOp("visit", ctx);
    // Assert
    expect(posts).toEqual([]);
  });

  it("refuses a blocked op end to end: no wire send, one toast", () => {
    // Arrange — the full standalone path: op → gated post → toast.
    const sent: PostCall[] = [];
    const toasts: string[] = [];
    const gated = makeGatedPost(
      true,
      (action, targets, args = {}, confirmed = false) => {
        sent.push([action, targets, args, confirmed]);
      },
      (toast) => toasts.push(toast),
    );
    const { ctx } = harness({ post: gated, blocked: (a) => EMACS_ONLY_ACTIONS.has(a) });
    // Act
    runSidebarOp("visit", ctx);
    // Assert
    expect(sent).toEqual([]);
    expect(toasts).toEqual([STANDALONE_TOAST]);
  });
});
