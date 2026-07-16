import { describe, expect, it } from "vitest";

import {
  CounterEntry,
  CounterSpec,
  RETENTION_TURNS,
  activeCount,
  countLabel,
  counterMenuHtml,
  counterOverlayHtml,
  isActive,
  isVisible,
  missingBubbleNotice,
  pruneByRecency,
  turnsAgoLabel,
  turnsSince,
} from "../src/counter-menu.js";

/** A spec standing in for a concrete counter. */
const SPEC: CounterSpec = {
  menu: "things",
  item: "thing",
  noun: "thing",
  busyNoun: "running",
  title: "session things",
  placeholder: "starting…",
};

/** A roster entry, defaulted to a settled (terminal) one aged 0 turns. */
function entry(over: Partial<CounterEntry> = {}): CounterEntry {
  return {
    id: "t1",
    summary: "do the thing",
    detail: "",
    status: "done",
    nested: false,
    deactivatedAtTurn: 0,
    ...over,
  };
}

/** An active roster entry (never deactivated). */
function active(over: Partial<CounterEntry> = {}): CounterEntry {
  return entry({ status: "running", deactivatedAtTurn: null, ...over });
}

describe("isActive", () => {
  it("reads a starting entry as active", () => {
    // Arrange + Act + Assert
    expect(isActive(entry({ status: "starting", deactivatedAtTurn: null }))).toBe(true);
  });

  it("reads a running entry as active", () => {
    // Arrange + Act + Assert
    expect(isActive(active())).toBe(true);
  });

  it("reads a done entry as inactive", () => {
    // Arrange + Act + Assert
    expect(isActive(entry({ status: "done" }))).toBe(false);
  });

  it("reads an errored entry as inactive", () => {
    // Arrange + Act + Assert
    expect(isActive(entry({ status: "error" }))).toBe(false);
  });
});

describe("activeCount", () => {
  it("counts only the active entries", () => {
    // Arrange
    const entries = [active(), entry({ id: "t2" }), active({ id: "t3" })];
    // Act + Assert
    expect(activeCount(entries)).toBe(2);
  });
});

describe("turnsSince", () => {
  it("is null while the entry is still active", () => {
    // Arrange + Act + Assert
    expect(turnsSince(active(), 9)).toBeNull();
  });

  it("counts the turns elapsed since the entry went terminal", () => {
    // Arrange + Act + Assert
    expect(turnsSince(entry({ deactivatedAtTurn: 4 }), 7)).toBe(3);
  });

  it("clamps a same-turn deactivation to zero", () => {
    // Arrange + Act + Assert
    expect(turnsSince(entry({ deactivatedAtTurn: 5 }), 5)).toBe(0);
  });
});

describe("isVisible", () => {
  it("keeps an active entry regardless of the clock", () => {
    // Arrange + Act + Assert
    expect(isVisible(active(), 999)).toBe(true);
  });

  it("keeps a terminal entry right at the retention boundary", () => {
    // Arrange
    const e = entry({ deactivatedAtTurn: 0 });
    // Act + Assert
    expect(isVisible(e, RETENTION_TURNS)).toBe(true);
  });

  it("drops a terminal entry one turn past the retention boundary", () => {
    // Arrange
    const e = entry({ deactivatedAtTurn: 0 });
    // Act + Assert
    expect(isVisible(e, RETENTION_TURNS + 1)).toBe(false);
  });
});

describe("pruneByRecency", () => {
  it("drops entries that have aged out but keeps active and recent ones", () => {
    // Arrange
    const entries = [
      active({ id: "a" }),
      entry({ id: "recent", deactivatedAtTurn: 8 }),
      entry({ id: "old", deactivatedAtTurn: 0 }),
    ];
    // Act
    const kept = pruneByRecency(entries, 10).map((e) => e.id);
    // Assert
    expect(kept).toEqual(["a", "recent"]);
  });
});

describe("turnsAgoLabel", () => {
  it("reads a same-turn deactivation as just now", () => {
    // Arrange + Act + Assert
    expect(turnsAgoLabel(0)).toBe("just now");
  });

  it("singularizes a one-turn-old deactivation", () => {
    // Arrange + Act + Assert
    expect(turnsAgoLabel(1)).toBe("1 turn ago");
  });

  it("pluralizes an older deactivation", () => {
    // Arrange + Act + Assert
    expect(turnsAgoLabel(3)).toBe("3 turns ago");
  });
});

describe("countLabel", () => {
  it("singularizes a lone entry", () => {
    // Arrange + Act + Assert
    expect(countLabel("agent", 1)).toBe("1 agent");
  });

  it("pluralizes several entries", () => {
    // Arrange + Act + Assert
    expect(countLabel("task", 3)).toBe("3 tasks");
  });
});

describe("missingBubbleNotice", () => {
  it("names the counter's noun so the notice says which roster missed", () => {
    // Arrange + Act + Assert
    expect(missingBubbleNotice(SPEC)).toBe(
      "thing has no bubble in the current feed (discarded by /clear)",
    );
  });
});

describe("counterMenuHtml", () => {
  it("renders nothing when the roster is empty", () => {
    // Arrange + Act + Assert
    expect(counterMenuHtml(SPEC, [], false, 0)).toBe("");
  });

  it("renders nothing when every entry has aged out of the window", () => {
    // Arrange
    const entries = [entry({ deactivatedAtTurn: 0 })];
    // Act + Assert
    expect(counterMenuHtml(SPEC, entries, false, RETENTION_TURNS + 1)).toBe("");
  });

  it("counts only the entries still within the window", () => {
    // Arrange
    const entries = [active(), entry({ id: "t2", deactivatedAtTurn: 0 })];
    // Act — at turn 0 both are visible.
    const html = counterMenuHtml(SPEC, entries, false, 0);
    // Assert
    expect(html).toContain("2 things");
  });

  it("carries a downward caret while closed", () => {
    // Arrange + Act
    const html = counterMenuHtml(SPEC, [active()], false, 0);
    // Assert
    expect(html).toContain(`<span class="things-caret" aria-hidden="true">▾</span>`);
  });

  it("flips the caret upward while open", () => {
    // Arrange + Act
    const html = counterMenuHtml(SPEC, [active()], true, 0);
    // Assert
    expect(html).toContain(`<span class="things-caret" aria-hidden="true">▴</span>`);
  });

  it("marks the toggle for the topbar's delegated click handler", () => {
    // Arrange + Act
    const html = counterMenuHtml(SPEC, [active()], false, 0);
    // Assert
    expect(html).toContain("data-things-toggle");
  });

  it("reports the closed roster to assistive tech", () => {
    // Arrange + Act
    const html = counterMenuHtml(SPEC, [active()], false, 0);
    // Assert
    expect(html).toContain(`aria-expanded="false"`);
  });

  it("withholds the overlay while the chip is closed", () => {
    // Arrange + Act
    const html = counterMenuHtml(SPEC, [active()], false, 0);
    // Assert
    expect(html).not.toContain("things-overlay");
  });

  it("drops the overlay while the chip is open", () => {
    // Arrange + Act
    const html = counterMenuHtml(SPEC, [active()], true, 0);
    // Assert
    expect(html).toContain("things-overlay");
  });

  it("badges how many entries are still working", () => {
    // Arrange + Act
    const html = counterMenuHtml(SPEC, [active(), entry({ id: "t2" })], false, 0);
    // Assert
    expect(html).toContain(`<span class="things-running">1 running</span>`);
  });

  it("omits the working badge once every entry has settled", () => {
    // Arrange + Act
    const html = counterMenuHtml(SPEC, [entry()], false, 0);
    // Assert
    expect(html).not.toContain("things-running");
  });
});

describe("counterOverlayHtml", () => {
  it("renders one row per entry", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active(), active({ id: "t2" })], 0);
    // Assert
    expect(html.match(/class="thing-row/g)).toHaveLength(2);
  });

  it("names each entry by its summary", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active({ summary: "hunt the flake" })], 0);
    // Assert
    expect(html).toContain("hunt the flake");
  });

  it("addresses each row by the entry's id so a click can act on it", () => {
    // Arrange + Act — the id is how the subagent roster jumps the feed to a card.
    const html = counterOverlayHtml(SPEC, [active({ id: "toolu_42" })], 0);
    // Assert
    expect(html).toContain(`data-thing-id="toolu_42"`);
  });

  it("escapes markup in the addressable id", () => {
    // Arrange + Act — the id is interpolated into an attribute value.
    const html = counterOverlayHtml(SPEC, [active({ id: `a"><b` })], 0);
    // Assert
    expect(html).not.toContain(`id="a"><b"`);
    expect(html).toContain("&quot;");
  });

  it("stands the placeholder in for a summary that has not streamed yet", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active({ summary: "", status: "starting" })], 0);
    // Assert
    expect(html).toContain("starting…");
  });

  it("chips the detail beside the summary", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active({ detail: "Explore" })], 0);
    // Assert
    expect(html).toContain(`<span class="thing-type">Explore</span>`);
  });

  it("omits the detail chip when the entry carries none", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active({ detail: "" })], 0);
    // Assert
    expect(html).not.toContain("thing-type");
  });

  it("colors the status dot by the entry's status", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [entry({ status: "error" })], 0);
    // Assert
    expect(html).toContain("thing-dot thing-error");
  });

  it("shows an active entry's bare status", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active()], 0);
    // Assert
    expect(html).toContain(`<span class="thing-status">running</span>`);
  });

  it("appends the age to a terminal entry's status", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [entry({ status: "done", deactivatedAtTurn: 2 })], 5);
    // Assert
    expect(html).toContain(`<span class="thing-status">done · 3 turns ago</span>`);
  });

  it("greys a terminal entry's row via the inactive class", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [entry({ deactivatedAtTurn: 0 })], 1);
    // Assert
    expect(html).toContain("thing-row inactive");
  });

  it("leaves an active entry's row ungreyed", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active()], 1);
    // Assert
    expect(html).not.toContain("inactive");
  });

  it("indents a nested entry's row", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active({ nested: true })], 0);
    // Assert
    expect(html).toContain("thing-row nested");
  });

  it("escapes markup in an entry's summary", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active({ summary: "<img src=x>" })], 0);
    // Assert
    expect(html).not.toContain("<img");
  });

  it("escapes markup in an entry's detail", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active({ detail: "<img src=x>" })], 0);
    // Assert
    expect(html).not.toContain("<img");
  });
});
