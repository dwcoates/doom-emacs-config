import { describe, expect, it } from "vitest";

import {
  CounterEntry,
  CounterSpec,
  countLabel,
  counterMenuHtml,
  counterOverlayHtml,
  dropdownChipHtml,
  isActive,
  missingBubbleNotice,
} from "../src/counter-menu.js";

/** A spec standing in for a concrete counter. */
const SPEC: CounterSpec = {
  menu: "things",
  item: "thing",
  noun: "thing",
  title: "session things",
  placeholder: "starting…",
};

/** A roster entry, defaulted to a settled (terminal) one. */
function entry(over: Partial<CounterEntry> = {}): CounterEntry {
  return {
    id: "t1",
    summary: "do the thing",
    detail: "",
    status: "done",
    nested: false,
    ...over,
  };
}

/** An active roster entry (still running). */
function active(over: Partial<CounterEntry> = {}): CounterEntry {
  return entry({ status: "running", ...over });
}

describe("isActive", () => {
  it("reads a starting entry as active", () => {
    // Arrange + Act + Assert
    expect(isActive(entry({ status: "starting" }))).toBe(true);
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
    expect(counterMenuHtml(SPEC, [], false)).toBe("");
  });

  it("renders nothing when every entry has settled", () => {
    // Arrange + Act + Assert — a done entry is no longer running, so the
    // chip disappears rather than logging what finished.
    expect(counterMenuHtml(SPEC, [entry({ status: "done" })], false)).toBe("");
  });

  it("counts only the entries still running", () => {
    // Arrange — one running, one settled.
    const entries = [active(), entry({ id: "t2", status: "done" })];
    // Act
    const html = counterMenuHtml(SPEC, entries, false);
    // Assert — the settled one is not counted.
    expect(html).toContain("1 thing");
  });

  it("hides a settled entry from an otherwise-running roster", () => {
    // Arrange — two running, one errored.
    const entries = [active(), active({ id: "t2" }), entry({ id: "t3", status: "error" })];
    // Act + Assert — the errored entry drops out of the count.
    expect(counterMenuHtml(SPEC, entries, false)).toContain("2 things");
  });

  it("carries a downward caret while closed", () => {
    // Arrange + Act
    const html = counterMenuHtml(SPEC, [active()], false);
    // Assert
    expect(html).toContain(`<span class="things-caret" aria-hidden="true">▾</span>`);
  });

  it("flips the caret upward while open", () => {
    // Arrange + Act
    const html = counterMenuHtml(SPEC, [active()], true);
    // Assert
    expect(html).toContain(`<span class="things-caret" aria-hidden="true">▴</span>`);
  });

  it("marks the toggle for the topbar's delegated click handler", () => {
    // Arrange + Act
    const html = counterMenuHtml(SPEC, [active()], false);
    // Assert
    expect(html).toContain("data-things-toggle");
  });

  it("reports the closed roster to assistive tech", () => {
    // Arrange + Act
    const html = counterMenuHtml(SPEC, [active()], false);
    // Assert
    expect(html).toContain(`aria-expanded="false"`);
  });

  it("withholds the overlay while the chip is closed", () => {
    // Arrange + Act
    const html = counterMenuHtml(SPEC, [active()], false);
    // Assert
    expect(html).not.toContain("things-overlay");
  });

  it("drops the overlay while the chip is open", () => {
    // Arrange + Act
    const html = counterMenuHtml(SPEC, [active()], true);
    // Assert
    expect(html).toContain("things-overlay");
  });

  it("carries no still-working badge, since the count is already the running count", () => {
    // Arrange + Act — every shown entry is running, so a separate
    // "N running" badge would only repeat the count.
    const html = counterMenuHtml(SPEC, [active(), active({ id: "t2" })], false);
    // Assert
    expect(html).not.toContain("things-running");
  });
});

describe("counterOverlayHtml", () => {
  it("renders one row per entry", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active(), active({ id: "t2" })]);
    // Assert
    expect(html.match(/class="thing-row/g)).toHaveLength(2);
  });

  it("names each entry by its summary", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active({ summary: "hunt the flake" })]);
    // Assert
    expect(html).toContain("hunt the flake");
  });

  it("addresses each row by the entry's id so a click can act on it", () => {
    // Arrange + Act — the id is how the subagent roster jumps the feed to a card.
    const html = counterOverlayHtml(SPEC, [active({ id: "toolu_42" })]);
    // Assert
    expect(html).toContain(`data-thing-id="toolu_42"`);
  });

  it("escapes markup in the addressable id", () => {
    // Arrange + Act — the id is interpolated into an attribute value.
    const html = counterOverlayHtml(SPEC, [active({ id: `a"><b` })]);
    // Assert
    expect(html).not.toContain(`id="a"><b"`);
    expect(html).toContain("&quot;");
  });

  it("stands the placeholder in for a summary that has not streamed yet", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active({ summary: "", status: "starting" })]);
    // Assert
    expect(html).toContain("starting…");
  });

  it("chips the detail beside the summary", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active({ detail: "Explore" })]);
    // Assert
    expect(html).toContain(`<span class="thing-type">Explore</span>`);
  });

  it("omits the detail chip when the entry carries none", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active({ detail: "" })]);
    // Assert
    expect(html).not.toContain("thing-type");
  });

  it("colors the status dot by the entry's status", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active({ status: "starting" })]);
    // Assert
    expect(html).toContain("thing-dot thing-starting");
  });

  it("shows the entry's bare status", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active()]);
    // Assert
    expect(html).toContain(`<span class="thing-status">running</span>`);
  });

  it("indents a nested entry's row", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active({ nested: true })]);
    // Assert
    expect(html).toContain("thing-row nested");
  });

  it("escapes markup in an entry's summary", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active({ summary: "<img src=x>" })]);
    // Assert
    expect(html).not.toContain("<img");
  });

  it("escapes markup in an entry's detail", () => {
    // Arrange + Act
    const html = counterOverlayHtml(SPEC, [active({ detail: "<img src=x>" })]);
    // Assert
    expect(html).not.toContain("<img");
  });
});

describe("dropdownChipHtml", () => {
  it("stamps the stem across the menu class, toggle attribute, and caret class", () => {
    // Arrange + Act — one stem names the whole DOM family.
    const html = dropdownChipHtml("things", "3 things", "t", false, () => "");
    // Assert
    expect(html).toContain(`class="things-menu"`);
    expect(html).toContain("data-things-toggle");
    expect(html).toContain(`class="things-caret"`);
  });

  it("escapes markup in the title", () => {
    // Arrange + Act
    const html = dropdownChipHtml("things", "3 things", `"><img src=x>`, false, () => "");
    // Assert
    expect(html).not.toContain("<img");
  });

  it("passes the label through as HTML so callers can embed badges", () => {
    // Arrange + Act — the counters' running badge is a span inside the label.
    const html = dropdownChipHtml("things", `x <span class="b">1</span>`, "t", false, () => "");
    // Assert
    expect(html).toContain(`x <span class="b">1</span>`);
  });

  it("builds the overlay only while open", () => {
    // Arrange — a closed chip must not pay for the list it is not showing.
    let built = 0;
    const overlay = () => {
      built++;
      return "<ul></ul>";
    };
    // Act
    dropdownChipHtml("things", "x", "t", false, overlay);
    dropdownChipHtml("things", "x", "t", true, overlay);
    // Assert
    expect(built).toBe(1);
  });

  it("flips the caret with the disclosure state", () => {
    // Arrange + Act + Assert
    expect(dropdownChipHtml("things", "x", "t", true, () => "")).toContain("▴");
    expect(dropdownChipHtml("things", "x", "t", false, () => "")).toContain("▾");
  });
});
