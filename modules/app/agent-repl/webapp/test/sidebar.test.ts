import { describe, expect, it } from "vitest";

import type { Entry, Group, MergeCommitRow, Section, Snapshot } from "../src/sidebar-types.js";
import {
  actionTargets,
  formatDuration,
  formatMergeAgo,
  formatMergeClock,
  mergedTargets,
  navigableRefs,
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
