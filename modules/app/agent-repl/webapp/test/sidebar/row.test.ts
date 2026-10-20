// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { captureLogRecords, forwardedRecord, type LogCapture } from "../log-capture.js";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { OpenWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_workspace_pb";
import { SelectWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import { SetWorkspaceHiddenResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_hidden_pb";
import { UpdateSidebarViewResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import { RosterRowSchema, type RosterRowWhenSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { liveElapsedClock } from "../../src/elapsed-clock.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { heatColor } from "../../src/heat.js";
import { HOVER_CLOSE_GRACE_MS } from "../../src/sidebar/row-hover.js";
import {
  BAND_RETURNS,
  drawBandChoices,
  MERGED_TODAY_CLASS,
  drawRosterRow,
  returnFromBand,
  type BandReturn,
  drawRosterRowStandIn,
  appendRowKids,
  STAND_IN_ATTRIBUTE,
  drawRosterRowOneShot,
  drawRosterRowAsyncMerging,
  drawRosterRowMergePaused,
  mergedToday,
  drawRosterRowWhen,
  drawStatusMark,
  placeOpenRowDetails,
  placeRowDetail,
  toggleRowMenu,
  type RowPlace,
  drawRosterFamily,
  pathedFamilies,
} from "../../src/sidebar/row.js";
import { REVEAL_MARGIN_PX } from "../../src/topbar/clamp.js";
import { codeOf } from "../source-text.js";
import { GLYPH_IMAGES, NO_MARKERS, ROSTER_ARM_CLASS, ROSTER_STATUS_CASES, type RosterStatusCase } from "../../src/sidebar/tones.js";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import {
  NOW,
  appContext,
  fakeTicker,
  fakeTimers,
  row,
  sidebarContext,
  startOfToday,
  startOfTomorrow,
  openRow,
  hiddenRow,
} from "./harness.js";
import { createSidebarView } from "../../src/sidebar/view.js";
import { SELECTION_NUMBER_RISE, SELECTION_NUMBER_RISE_MS, createSelectionFade } from "../../src/sidebar/selection-fade.js";
import stylesheet from "../../src/styles.css?raw";
import { cascadedValue, inRail, rulesOf } from "../stylesheet.js";

/** The markup the DOM holds once glyph NAME's shared picture is drawn into a mark. */
function drawnPicture(name: string): string {
  const host = document.createElement("span");
  host.innerHTML = GLYPH_IMAGES[name] ?? "";
  return host.innerHTML;
}

const SELECT_OK = create(SelectWorkspaceResponseSchema, {
  result: { case: "success", value: {} },
});

/** The row line: what the pointer and the keyboard focus are read from. */
const lineOf = (drawn: HTMLElement): HTMLElement =>
  drawn.querySelector(":scope > .row") as HTMLElement;

/** This row's own detail panel. */
const panelOf = (drawn: HTMLElement): HTMLElement =>
  drawn.querySelector(":scope > .detail") as HTMLElement;

/** The row's name. */
const nameOf = (drawn: HTMLElement): HTMLElement => lineOf(drawn).querySelector(".name") as HTMLElement;

/** The status dot. */
const dotOf = (drawn: HTMLElement): HTMLElement =>
  lineOf(drawn).querySelector(".st") as HTMLElement;

/** The row's duration field: every row's pointer trigger for the panel (owner request, 2026-10-14). */
const durationOf = (drawn: HTMLElement): HTMLElement =>
  lineOf(drawn).querySelector(":scope > [data-when]") as HTMLElement;

function hoverIn(drawn: HTMLElement): void {
  durationOf(drawn).dispatchEvent(new MouseEvent("mouseenter"));
}

function hoverOut(drawn: HTMLElement): void {
  durationOf(drawn).dispatchEvent(new MouseEvent("mouseleave"));
}

async function click(control: Element): Promise<void> {
  (control as HTMLElement).dispatchEvent(new MouseEvent("click", { bubbles: true }));
  await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
}

describe("the row's identity and hooks", () => {
  it("is addressed by the workspace id the daemon minted", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.getAttribute("data-roster-row")).toBe("ws-1");
  });

  it("states its status arm", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", status: { case: "thinking", value: {} } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.getAttribute("data-arm")).toBe("thinking");
  });

  it("draws the display name", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", name: "the rail" }), sidebarContext(), "R");
    expect(drawn.querySelector(".name")?.textContent).toBe("the rail");
  });

  it("offers exactly one SelectWorkspace click target", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.querySelectorAll(":scope > [data-select]").length).toBe(1);
  });
});

describe("the status mark", () => {
  it.each([...ROSTER_STATUS_CASES])("paints %s the shared vocabulary's color", (arm) => {
    const mark = drawStatusMark(arm, "R.status", NO_MARKERS);
    expect(mark.classList.contains(ROSTER_ARM_CLASS[arm])).toBe(true);
  });

  it.each([...ROSTER_STATUS_CASES])("names %s's glyph", (arm) => {
    expect(drawStatusMark(arm, "R.status", NO_MARKERS).hasAttribute("data-glyph")).toBe(true);
  });

  it("draws a failed turn end as a turquoise dot", () => {
    const mark = drawStatusMark("turnFailed", "R.status", NO_MARKERS);
    expect(mark.classList.contains("tone-turquoise")).toBe(true);
    expect(mark.getAttribute("data-glyph")).toBe("dot");
  });

  it.each(["mergeQueued", "merging", "mergeFailed", "merged"] as const)(
    "draws %s as the one merge glyph in a glyph box",
    (arm) => {
      const mark = drawStatusMark(arm, "R.status", NO_MARKERS);
      expect([mark.getAttribute("data-glyph"), mark.classList.contains("st-glyph"), mark.innerHTML]).toEqual([
        "merge",
        true,
        drawnPicture("merge"),
      ]);
    },
  );

  it("inks a failed merge's glyph red on its green tone", () => {
    const mark = drawStatusMark("mergeFailed", "R.status", NO_MARKERS);
    expect([mark.classList.contains("tone-green"), mark.classList.contains("ink-red")]).toEqual([true, true]);
  });

  it("inks a compaction's dot orange on its red tone", () => {
    const mark = drawStatusMark("compacting", "R.status", NO_MARKERS);
    expect([mark.classList.contains("tone-red"), mark.classList.contains("ink-orange")]).toEqual([true, true]);
  });

  it("inks a thinking dot in its own tone", () => {
    const mark = drawStatusMark("thinking", "R.status", NO_MARKERS);
    expect([...mark.classList].filter((c) => c.startsWith("ink-"))).toEqual([]);
  });

  it("draws a grey dot for an incidentally inactive workspace", () => {
    const mark = drawStatusMark("inactive", "R.status", NO_MARKERS);
    expect([mark.getAttribute("data-glyph"), mark.classList.contains("tone-none"), mark.textContent]).toEqual([
      "dot",
      true,
      "",
    ]);
  });

  it("draws a snowflake for a cold gate", () => {
    expect(drawStatusMark("coldGate", "R.status", NO_MARKERS).innerHTML).toBe(drawnPicture("snowflake"));
  });

  it("draws no picture for a workspace with no session", () => {
    expect(drawStatusMark("none", "R.status", NO_MARKERS).textContent).toBe("");
  });

  // THE MARK STANDS STILL (owner request, 2026-10-14): the name breathes in
  // its place.
  it.each(["thinking", "merging", "init", "compacting"] as const)("holds a %s mark still", (arm) => {
    expect(drawStatusMark(arm, "R.status", NO_MARKERS).classList.contains("breathes")).toBe(false);
  });

  it("holds an async merge's glyph still", () => {
    expect(drawStatusMark("ready", "R.status", { oneShot: false, asyncMerging: true, mergePaused: false }).classList.contains("breathes")).toBe(false);
  });
});

describe("the breathing name", () => {
  const drawArm = (arm: RosterStatusCase, place: RowPlace = "grouping"): HTMLElement =>
    drawRosterRow(
      row({ id: "ws-1", status: { case: arm, value: {} } as MessageInitShape<typeof RosterRowSchema>["status"] }),
      sidebarContext(),
      "R",
      place,
    );

  it("breathes the name while a turn is in flight", () => {
    expect(nameOf(drawArm("thinking")).classList.contains("breathes")).toBe(true);
  });

  it("breathes the name while the merge queue is on this run", () => {
    expect(nameOf(drawArm("merging")).classList.contains("breathes")).toBe(true);
  });

  it.each(["permission", "question", "waiting", "mergeQueued", "coldGate"] as const)("holds a %s name still", (arm) => {
    expect(nameOf(drawArm(arm)).classList.contains("breathes")).toBe(false);
  });

  it("breathes the name of a working one-shot", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "thinking", value: {} }, oneShot: true }), sidebarContext(), "R");
    expect(nameOf(drawn).classList.contains("breathes")).toBe(true);
  });

  it("breathes the name of a row with an async merge", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "ready", value: {} }, asyncMerging: true }), sidebarContext(), "R");
    expect(nameOf(drawn).classList.contains("breathes")).toBe(true);
  });

  it.each(["merged", "hidden"] as const)("holds a %s band row's name still", (place) => {
    expect(nameOf(drawArm("thinking", place)).classList.contains("breathes")).toBe(false);
  });

  it("never breathes the mark of a breathing row", () => {
    expect(dotOf(drawArm("thinking")).classList.contains("breathes")).toBe(false);
  });
});


describe("the highlight and the receded styling", () => {
  it("highlights the row the RESOLVER called current", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", current: true }), sidebarContext(), "R");
    expect(drawn.getAttribute("data-current")).toBe("true");
  });

  it("does not highlight a row the resolver left uncurrent", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", current: false }), sidebarContext(), "R");
    expect(drawn.hasAttribute("data-current")).toBe(false);
  });

  it.each([
    { name: "highlights the shown row the resolver has not named current yet", id: "ws-2", current: false, want: true },
    { name: "does not highlight the resolver's current row while another is shown", id: "ws-1", current: true, want: false },
  ])("$name", ({ id, current, want }) => {
    // Arrange
    const sc = { ...sidebarContext(), shownAhead: () => "ws-2" };

    // Act
    const drawn = drawRosterRow(row({ id, current }), sc, "R");

    // Assert
    expect([drawn.hasAttribute("data-current"), drawn.classList.contains("current")]).toEqual([want, want]);
  });

  /** The declarations of every stylesheet rule that paints a current row. */
  const currentRowDeclarations = (): string[] =>
    rulesOf(stylesheet)
      .filter((rule) => rule.selectors.some((sel) => sel.includes(".ws.current")))
      .map((rule) => rule.declarations);

  it("marks the current row with the muted gutter bar", () => {
    // Arrange / Act
    const declarations = currentRowDeclarations();
    // Assert
    expect(declarations.some((d) => /border-left:\s*var\(--row-gutter-w\) solid var\(--row-current-gutter\)/.test(d))).toBe(true);
  });

  it("fills the current row's line and its gutter bar partway from --card and --muted toward the rail", () => {
    // Arrange / Act
    const declarations = currentRowDeclarations();
    const token = (name: string): string | undefined =>
      rulesOf(stylesheet)
        .filter((rule) => rule.selectors.includes("#ws-sidebar"))
        .map((rule) => new RegExp(`${name}:\\s*([^;]+);`).exec(rule.declarations)?.[1])
        .find((value) => value !== undefined);
    // Assert
    expect([
      declarations.some((d) => /background-color:\s*var\(--row-current-tint\)/.test(d)),
      token("--row-current-tint"),
      token("--row-current-gutter"),
    ]).toEqual([
      true,
      "color-mix(in srgb, var(--card) 38%, var(--sidebar-bg))",
      "color-mix(in srgb, var(--muted) 64%, var(--sidebar-bg))",
    ]);
  });

  it("paints the current row's tint through the cascade", () => {
    // Arrange
    const ws = drawRosterRow(row({ id: "ws-1", current: true }), sidebarContext(), "R");
    // Act
    const background = inRail(ws, () => cascadedValue(ws.querySelector(".row")!, "background-color"));
    // Assert
    expect(background).toBe("var(--row-current-tint)");
  });

  it("does not embolden the current row", () => {
    // Arrange / Act
    const declarations = currentRowDeclarations();
    // Assert
    expect(declarations.filter((d) => /\bfont-weight\s*:/.test(d))).toEqual([]);
  });

  it("recedes a closed workspace", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", closed: true }), sidebarContext(), "R");
    expect(drawn.getAttribute("data-closed")).toBe("true");
  });

  it("leaves an open workspace unreceded", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", closed: false }), sidebarContext(), "R");
    expect(drawn.hasAttribute("data-closed")).toBe(false);
  });
});

describe("the attention marker", () => {
  it("marks the row when the daemon set it", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", attention: true }), sidebarContext(), "R");
    expect(drawn.hasAttribute("data-attention")).toBe(true);
  });

  it("draws no marker when it is absent", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.querySelector(".sb-attn")).toBeNull();
  });

  it("starts the blink lit, on the cadence the message specifies", () => {
    const sc = sidebarContext();
    const drawn = drawRosterRow(row({ id: "ws-1", attention: true }), sc, "R");
    // The MARKER blinks, not the row: `data-blink` is painted on the element
    // the hook contract points at, `[data-roster-row] [data-attention]`.
    expect(drawn.querySelector(".sb-attn")?.getAttribute("data-blink")).toBe("on");
  });
});

describe("the priority badge", () => {
  it("draws the resolver's composed label verbatim", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", priority: "P0.5" }), sidebarContext(), "R");
    expect(drawn.querySelector(".sb-prio")?.textContent).toBe("P0.5");
  });

  it("addresses the badge by that label", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", priority: "P1" }), sidebarContext(), "R");
    expect(drawn.querySelector("[data-priority='P1']")).not.toBeNull();
  });

  it("draws no badge for an unprioritized workspace", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.querySelector(".sb-prio")).toBeNull();
  });
});

describe("the when-column", () => {
  it("reads 0s with no turn running", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect([drawn.querySelector(".when")?.getAttribute("data-when"), drawn.querySelector(".when")?.textContent]).toEqual([
      "noTurnRunning",
      "0s",
    ]);
  });

  it("never ticks with no turn running", () => {
    // Arrange
    const ticker = fakeTicker();
    const sc = sidebarContext(appContext({}, ticker));
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    // Act
    ticker.tick(NOW + 120_000);
    // Assert
    expect([drawn.querySelector(".when")?.textContent, sc.disposers.length]).toEqual(["0s", 0]);
  });

  describe.each(["turnRunning", "syncMerging"] as const)("the counting arm %s", (arm) => {
    const counting = (startedMs: number): MessageInitShape<typeof RosterRowWhenSchema>["shown"] => ({
      case: arm,
      value: { startedAtMs: BigInt(startedMs) },
    });

    it("counts its elapsed time", () => {
      const drawn = drawRosterRow(row({ id: "ws-1", when: counting(NOW - 65_000) }), sidebarContext(), "R");
      expect([drawn.querySelector(".when")?.getAttribute("data-when"), drawn.querySelector(".when")?.textContent]).toEqual([
        arm,
        "1m 5s",
      ]);
    });

    it("ticks off the SHARED clock", () => {
      // Arrange
      const ticker = fakeTicker();
      const sc = sidebarContext(appContext({}, ticker));
      const drawn = drawRosterRow(row({ id: "ws-1", when: counting(NOW - 60_000) }), sc, "R");
      // Act
      ticker.tick(NOW + 120_000);
      // Assert
      expect(drawn.querySelector(".when")?.textContent).toBe("3m");
    });

    it("registers its tick as a teardown the next push runs", () => {
      const sc = sidebarContext(appContext({}, fakeTicker()));
      drawRosterRow(row({ id: "ws-1", when: counting(NOW) }), sc, "R");
      expect(sc.disposers.length).toBe(1);
    });

    it.each([0, 4_920, 65_432, 3_725_000])("reads exactly what the footer's turn clock reads %sms in", (elapsed) => {
      // Arrange: the strip's clock is a live elapsed clock from the same start.
      const ticker = fakeTicker();
      const started = NOW - elapsed;
      const strip = liveElapsedClock(ticker, "info-time", started);
      // Act
      const drawn = drawRosterRow(row({ id: "ws-1", when: counting(started) }), sidebarContext(appContext({}, ticker)), "R");
      ticker.tick(NOW + 7_300);
      // Assert
      expect(drawn.querySelector(".when")?.textContent).toBe(strip.textContent);
    });

    it("refuses a start no JS instant holds as a malformed view", () => {
      const bad = row({ id: "ws-1", when: counting(0) });
      (bad.when!.shown.value as { startedAtMs: bigint }).startedAtMs = BigInt(Number.MAX_SAFE_INTEGER) + 1n;
      expect(() => drawRosterRow(bad, sidebarContext(), "R")).toThrow(MalformedView);
    });
  });

  it("refuses an unset when-column as a malformed view", () => {
    const bad = row({ id: "ws-1" });
    bad.when!.shown = { case: undefined };
    expect(() => drawRosterRow(bad, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("draws a settled merge as a bare relative age", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", when: { case: "merged", value: { atMs: BigInt(NOW - 3_600_000) } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".when")?.textContent).toBe("1h");
  });

  it("draws a hidden workspace's age from its hide", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", when: { case: "hidden", value: { atMs: BigInt(NOW - 120_000) } } }),
      sidebarContext(),
      "R",
      "hidden",
    );
    expect([drawn.querySelector(".when")?.getAttribute("data-when"), drawn.querySelector(".when")?.textContent]).toEqual([
      "hidden",
      "2m",
    ]);
  });

  it("counts a hidden workspace down to its cache lapse", () => {
    const drawn = drawRosterRow(
      row({
        id: "ws-1",
        when: { case: "hidden", value: { atMs: BigInt(NOW - 120_000), cache: { lapsesAtMs: BigInt(NOW + 754_000) } } },
      }),
      sidebarContext(),
      "R",
      "hidden",
    );
    expect(drawn.querySelector(".when")?.textContent).toBe("12m 34s");
  });

  it("colors the countdown on the heat gradient", () => {
    const drawn = drawRosterRow(
      row({
        id: "ws-1",
        when: { case: "hidden", value: { atMs: BigInt(NOW), cache: { lapsesAtMs: BigInt(NOW + 30 * 60_000) } } },
      }),
      sidebarContext(),
      "R",
      "hidden",
    );
    expect((drawn.querySelector(".when") as HTMLElement).style.color).toBe(heatColor(0.5, "p"));
  });

  it("ticks the countdown to a cold cache off the SHARED clock", () => {
    const ticker = fakeTicker();
    const sc = sidebarContext(appContext({}, ticker));
    const drawn = drawRosterRow(
      row({
        id: "ws-1",
        when: { case: "hidden", value: { atMs: BigInt(NOW), cache: { lapsesAtMs: BigInt(NOW + 60_000) } } },
      }),
      sc,
      "R",
      "hidden",
    );
    ticker.tick(NOW + 60_000);
    const when = drawn.querySelector(".when") as HTMLElement;
    expect([when.textContent, when.hasAttribute("data-cache-cold")]).toEqual(["cold cache", true]);
  });

  it("draws a hidden row's cold cache clear of the row's recession, at full opacity", () => {
    // Arrange
    const ticker = fakeTicker();
    const sc = sidebarContext(appContext({}, ticker));
    const drawn = drawRosterRow(
      row({
        id: "ws-1",
        when: { case: "hidden", value: { atMs: BigInt(NOW), cache: { lapsesAtMs: BigInt(NOW + 60_000) } } },
      }),
      sc,
      "R",
      "hidden",
    );
    ticker.tick(NOW + 60_000);
    // Act
    const opacities = inRail(drawn, () => [
      cascadedValue(drawn.querySelector(":scope > .row") as HTMLElement, "opacity") || "1",
      cascadedValue(drawn.querySelector(":scope > .row > .when") as HTMLElement, "opacity") || "1",
      cascadedValue(drawn.querySelector(":scope > .row > .name") as HTMLElement, "opacity"),
    ]);
    // Assert
    expect(opacities).toEqual(["1", "1", "var(--receded-opacity)"]);
  });

  it("marks a warm countdown as not yet cold", () => {
    const drawn = drawRosterRow(
      row({
        id: "ws-1",
        when: { case: "hidden", value: { atMs: BigInt(NOW), cache: { lapsesAtMs: BigInt(NOW + 60_000) } } },
      }),
      sidebarContext(),
      "R",
      "hidden",
    );
    const when = drawn.querySelector(".when") as HTMLElement;
    expect([when.hasAttribute("data-cache"), when.hasAttribute("data-cache-cold")]).toEqual([true, false]);
  });

});

describe("the detail panel", () => {
  it("draws the branch when the line is present", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", detail: { branch: { name: "feat/rail" } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".detail")?.textContent).toContain("feat/rail");
  });

  it("draws the parent branch when the line is present", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", detail: { parentBranch: { name: "master" } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".detail")?.textContent).toContain("master");
  });

  it("draws the summary when the line is present", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", detail: { summary: { text: "porting the rail" } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".detail .summary")?.textContent).toBe("porting the rail");
  });

  it.each([
    { name: "a count", count: 4, want: "4" },
    { name: "a zero count", count: 0, want: "0" },
  ])("draws the commit count when the line is present: $name", ({ count, want }) => {
    const drawn = drawRosterRow(row({ id: "ws-1", detail: { commits: { count } } }), sidebarContext(), "R");
    const terms = [...drawn.querySelectorAll(".detail dt")].map((dt) => dt.textContent);
    const values = [...drawn.querySelectorAll(".detail dd")].map((dd) => dd.textContent);
    expect([terms, values]).toEqual([["commits"], [want]]);
  });

  it("draws the commit count after the branch lines", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", detail: { branch: { name: "feat/rail" }, parentBranch: { name: "master" }, commits: { count: 2 } } }),
      sidebarContext(),
      "R",
    );
    expect([...drawn.querySelectorAll(".detail dt")].map((dt) => dt.textContent)).toEqual(["branch", "from", "commits"]);
  });

  it("omits the commit count when the line is absent", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", detail: { branch: { name: "feat/rail" } } }), sidebarContext(), "R");
    expect([...drawn.querySelectorAll(".detail dt")].map((dt) => dt.textContent)).not.toContain("commits");
  });

  it("omits an absent line rather than drawing it blank", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", detail: { branch: { name: "feat/rail" } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".detail .summary")).toBeNull();
  });

  it("draws no list at all when every line is absent", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", detail: {} }), sidebarContext(), "R");
    expect(drawn.querySelector(".detail dl")).toBeNull();
  });

  it("is closed until the row is expanded", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("opens on hover", () => {
    // ARRANGE
    vi.useFakeTimers();
    try {
      const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
      // ACT
      hoverIn(drawn);
      // ASSERT
      expect(drawn.classList.contains("open")).toBe(true);
    } finally {
      vi.useRealTimers();
    }
  });

  it("remembers the expansion on this page, keyed by the workspace id", () => {
    // ARRANGE
    vi.useFakeTimers();
    try {
      const sc = sidebarContext();
      const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
      // ACT
      hoverIn(drawn);
      // ASSERT
      expect([...sc.openDetails]).toEqual(["ws-1"]);
    } finally {
      vi.useRealTimers();
    }
  });

  it("draws an expansion this page remembers open on the next push", () => {
    const sc = sidebarContext(appContext(), fakeTimers(), createSidebarView(), new Set(["ws-1"]));
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    expect(drawn.classList.contains("open")).toBe(true);
  });

  it("never asks the daemon about a detail, which is this page's own", () => {
    // ARRANGE
    vi.useFakeTimers();
    const asked: string[] = [];
    try {
      const sc = sidebarContext(
        appContext({
          updateSidebarView: (request) => {
            asked.push(request.change.case ?? "unset");
            return create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } });
          },
        }),
      );
      const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
      // ACT
      hoverIn(drawn);
      // ASSERT
      expect(asked).toEqual([]);
    } finally {
      vi.useRealTimers();
    }
  });

  it("carries no chevron at all", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.querySelector(".chev")).toBeNull();
  });
});

describe("the family", () => {
  it("nests a child under its parent", () => {
    const drawn = drawRosterFamily(openRow(row({ id: "ws-1", children: [row({ id: "ws-2" })] })), sidebarContext(), "R");
    expect(drawn.querySelector(".kids [data-roster-row='ws-2']")).not.toBeNull();
  });

  it("nests a grandchild one generation further", () => {
    const drawn = drawRosterFamily(
      openRow(row({ id: "ws-1", children: [row({ id: "ws-2", children: [row({ id: "ws-3" })] })] })),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".kids .kids [data-roster-row='ws-3']")).not.toBeNull();
  });

  it("draws no family box for a childless row", () => {
    const drawn = drawRosterFamily(openRow(row({ id: "ws-1" })), sidebarContext(), "R");
    expect(drawn.querySelector(".kids")).toBeNull();
  });

  it("draws every child the wire nests, a receded one included", () => {
    // The daemon lists each state on its own, so a row it nests is drawn.
    const drawn = drawRosterFamily(openRow(row({ id: "ws-1", children: [row({ id: "ws-2", closed: true })] })), sidebarContext(), "R");
    expect(drawn.querySelector(".kids [data-roster-row='ws-2']")).not.toBeNull();
  });

  it("refuses a list row naming no row as a malformed view", () => {
    const bad = openRow(row({ id: "ws-1" }));
    bad.row = undefined;
    expect(() => drawRosterFamily(bad, sidebarContext(), "R")).toThrow(/R\.row/);
  });
});

describe("pathedFamilies", () => {
  it("gives each family its index under the region's path", () => {
    const got = pathedFamilies([openRow(row({ id: "a" })), openRow(row({ id: "b" }))], "R.rows");
    expect(got.map((e) => [e.family.row?.workspace?.workspace?.id, e.path])).toEqual([
      ["a", "R.rows[0]"],
      ["b", "R.rows[1]"],
    ]);
  });

  it.each([
    ["roster.ts", "drawRosterRows"],
    ["keyed-rows.ts", "visible"],
    ["row.ts", "appendRowKids"],
  ])("is how %s walks a list's rows", (file, fn) => {
    // Arrange
    const source = codeOf(readFileSync(join(__dirname, "..", "..", "src", "sidebar", file), "utf8"));
    const start = source.indexOf(`function ${fn}(`);

    // Act
    const body = source.slice(start, source.indexOf("\n}\n", start));

    // Assert
    expect(body.includes("pathedFamilies(")).toBe(true);
  });
});

describe("the row click in the hidden section", () => {
  // OWNER REQUEST, 2026-10-13: the click shows the workspace and then
  // selects it, so the user lands on what they clicked.
  it("shows the workspace again, then selects it", async () => {
    const calls: string[] = [];
    const sc = sidebarContext(
      appContext({
        setWorkspaceHidden: (request) => {
          calls.push(`hidden:${request.workspace?.id ?? ""}:${request.visibility.case ?? ""}`);
          return create(SetWorkspaceHiddenResponseSchema, { result: { case: "success", value: {} } });
        },
        selectWorkspace: (request) => {
          calls.push(`select:${request.workspace?.id ?? ""}`);
          return SELECT_OK;
        },
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R", "hidden");
    await click(drawn.querySelector("[data-select]") as Element);
    expect(calls).toEqual(["hidden:ws-1:show", "select:ws-1"]);
  });

  it("selects nothing when the show is refused", async () => {
    const selected: string[] = [];
    const sc = sidebarContext(
      appContext({
        setWorkspaceHidden: () =>
          create(SetWorkspaceHiddenResponseSchema, {
            result: { case: "error", value: { cause: { case: "unknownWorkspace", value: {} } } },
          }),
        selectWorkspace: (request) => {
          selected.push(request.workspace?.id ?? "");
          return SELECT_OK;
        },
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R", "hidden");
    await click(drawn.querySelector("[data-select]") as Element);
    expect(selected).toEqual([]);
  });

  it("shows a hidden child row by its own ref", async () => {
    const shown: string[] = [];
    const sc = sidebarContext(
      appContext({
        setWorkspaceHidden: (request) => {
          shown.push(request.workspace?.id ?? "");
          return create(SetWorkspaceHiddenResponseSchema, { result: { case: "success", value: {} } });
        },
        selectWorkspace: () => SELECT_OK,
      }),
    );
    const drawn = drawRosterFamily(hiddenRow(row({ id: "ws-1", children: [row({ id: "ws-2" })] })), sc, "R", "hidden");
    await click(drawn.querySelector("[data-roster-row='ws-2'] [data-select]") as Element);
    expect(shown).toEqual(["ws-2"]);
  });

  it("says a refusal at the row itself", async () => {
    const sc = sidebarContext(
      appContext({
        setWorkspaceHidden: () =>
          create(SetWorkspaceHiddenResponseSchema, {
            result: { case: "error", value: { cause: { case: "unknownWorkspace", value: {} } } },
          }),
      }),
    );
    const capture = captureLogRecords();
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R", "hidden");
    await click(drawn.querySelector("[data-select]") as Element);
    const record = await forwardedRecord(capture, "sidebar.verbs.refused");
    expect([drawn.querySelector(":scope > .sb-refusal")?.textContent, record.level.case]).toEqual([
      "the daemon does not know this workspace",
      "warn",
    ]);
  });
});

describe("returnFromBand", () => {
  const ws = create(WorkspaceRefSchema, { id: "ws-1" });
  function band(answer: boolean, calls: string[]): BandReturn {
    return {
      label: "Test",
      operation: "sidebar.row.test-band",
      message: "returning a workspace from a test band",
      bringBack: (_line, _sc, workspace) => {
        calls.push(`back:${workspace.id}`);
        return Promise.resolve(answer);
      },
    };
  }

  it("brings the workspace back before it selects it", async () => {
    const calls: string[] = [];
    const sc = sidebarContext(
      appContext({
        selectWorkspace: (request) => {
          calls.push(`select:${request.workspace?.id ?? ""}`);
          return SELECT_OK;
        },
      }),
    );
    await returnFromBand(band(true, calls), document.createElement("div"), sc, ws);
    expect(calls).toEqual(["back:ws-1", "select:ws-1"]);
  });

  it("selects nothing when the workspace was not brought back", async () => {
    const calls: string[] = [];
    const sc = sidebarContext(
      appContext({
        selectWorkspace: () => {
          calls.push("select");
          return SELECT_OK;
        },
      }),
    );
    await returnFromBand(band(false, calls), document.createElement("div"), sc, ws);
    expect(calls).toEqual(["back:ws-1"]);
  });

  it("records the click at info under the band's operation", async () => {
    const capture = captureLogRecords();
    await returnFromBand(band(false, []), document.createElement("div"), sidebarContext(), ws);
    const record = await forwardedRecord(capture, "sidebar.row.test-band");
    expect(record.level.case).toBe("info");
  });

  // EVERY BAND'S CLICK GOES THROUGH returnFromBand: a band the table lacks
  // would be a place whose click is spelled some other way.
  it("names a return for every band a row can be drawn in", () => {
    const bands: RowPlace[] = ["hidden", "merged"];
    expect(Object.keys(BAND_RETURNS).sort()).toEqual([...bands].sort());
  });
});

describe("the row click in the recently-merged band", () => {
  // OWNER REQUESTS, 2026-10-14 and 2026-10-15: the click offers reopening the
  // merged workspace, to ask it about what it landed, or reopening it and
  // reverting its merge; either one reopens it and then selects it.
  const OPEN_OK = create(OpenWorkspaceResponseSchema, { result: { case: "success", value: {} } });
  const OPEN_REFUSED = create(OpenWorkspaceResponseSchema, {
    result: { case: "error", value: { cause: { case: "unknownWorkspace", value: {} } } },
  });
  const REOPEN = '[data-band-return="sidebar.row.reopen"]';
  const REVERT = '[data-band-return="sidebar.row.revert-merge"]';

  /** A merged row drawn against SC, and its choice list. */
  function mergedRow(sc: ReturnType<typeof sidebarContext>): { drawn: HTMLElement; choices: HTMLElement } {
    const drawn = drawRosterRow(row({ id: "ws-1", closed: true }), sc, "R", "merged");
    return { drawn, choices: drawn.querySelector(":scope > .sb-band-choices") as HTMLElement };
  }

  /** An app context recording each open (and whether it asked the revert) and each select. */
  function recording(calls: string[], open = OPEN_OK) {
    return sidebarContext(
      appContext({
        openWorkspace: (request) => {
          calls.push(`${request.revertMerge === undefined ? "open" : "revert"}:${request.workspace?.id ?? ""}`);
          return open;
        },
        selectWorkspace: (request) => {
          calls.push(`select:${request.workspace?.id ?? ""}`);
          return SELECT_OK;
        },
      }),
    );
  }

  it("draws its choices hidden", () => {
    const { choices } = mergedRow(recording([]));
    expect(choices.hidden).toBe(true);
  });

  it("offers reopening and reopening to revert the merge, in that order", () => {
    const { choices } = mergedRow(recording([]));
    expect([...choices.querySelectorAll(".sb-menu-item")].map((e) => e.textContent)).toEqual([
      "Reopen",
      "Reopen and revert the merge",
    ]);
  });

  it("reveals its choices on the click and opens nothing", async () => {
    const calls: string[] = [];
    const { drawn, choices } = mergedRow(recording(calls));
    await click(drawn.querySelector("[data-select]") as Element);
    expect([choices.hidden, calls]).toEqual([false, []]);
  });

  it("hides its choices on a second click", async () => {
    const { drawn, choices } = mergedRow(recording([]));
    await click(drawn.querySelector("[data-select]") as Element);
    await click(drawn.querySelector("[data-select]") as Element);
    expect(choices.hidden).toBe(true);
  });

  it("reopens the workspace, then selects it, on Reopen", async () => {
    const calls: string[] = [];
    const { drawn } = mergedRow(recording(calls));
    await click(drawn.querySelector("[data-select]") as Element);
    await click(drawn.querySelector(REOPEN) as Element);
    expect(calls).toEqual(["open:ws-1", "select:ws-1"]);
  });

  it("reopens the workspace asking the revert, then selects it, on the revert", async () => {
    const calls: string[] = [];
    const { drawn } = mergedRow(recording(calls));
    await click(drawn.querySelector("[data-select]") as Element);
    await click(drawn.querySelector(REVERT) as Element);
    expect(calls).toEqual(["revert:ws-1", "select:ws-1"]);
  });

  it("hides its choices once one is picked", async () => {
    const { drawn, choices } = mergedRow(recording([]));
    await click(drawn.querySelector("[data-select]") as Element);
    await click(drawn.querySelector(REVERT) as Element);
    expect(choices.hidden).toBe(true);
  });

  it("selects nothing when the open is refused", async () => {
    const calls: string[] = [];
    const { drawn } = mergedRow(recording(calls, OPEN_REFUSED));
    await click(drawn.querySelector(REVERT) as Element);
    expect(calls).toEqual(["revert:ws-1"]);
  });

  it("says a refused open at the row itself", async () => {
    const capture = captureLogRecords();
    const { drawn } = mergedRow(recording([], OPEN_REFUSED));
    await click(drawn.querySelector(REOPEN) as Element);
    const record = await forwardedRecord(capture, "sidebar.verbs.refused");
    expect([drawn.querySelector(":scope > .sb-refusal")?.textContent, record.level.case]).toEqual([
      "the daemon does not know this workspace",
      "warn",
    ]);
  });

  it.each([
    ["the reopen", REOPEN, "sidebar.row.reopen"],
    ["the revert", REVERT, "sidebar.row.revert-merge"],
  ])("records %s at info", async (_name, choice, operation) => {
    const capture = captureLogRecords();
    const { drawn } = mergedRow(recording([]));
    await click(drawn.querySelector(choice) as Element);
    const record = await forwardedRecord(capture, operation);
    expect(record.level.case).toBe("info");
  });
});

describe("the band choice list", () => {
  it("is drawn only for a band with several returns", () => {
    const sc = sidebarContext(appContext({}));
    const hidden = drawRosterRow(row({ id: "ws-1" }), sc, "R", "hidden");
    const merged = drawRosterRow(row({ id: "ws-2", closed: true }), sc, "R", "merged");
    expect([
      hidden.querySelector(":scope > .sb-band-choices"),
      merged.querySelector(":scope > .sb-band-choices") !== null,
    ]).toEqual([null, true]);
  });

  // EVERY CHOICE IS A BAND RETURN: the list draws one entry per return the
  // band names, so an entry spelled some other way fails here.
  it("draws one entry per return, each tagged with its return's operation", () => {
    const sc = sidebarContext(appContext({}));
    const choices = drawBandChoices(BAND_RETURNS.merged, document.createElement("div"), sc, create(WorkspaceRefSchema, { id: "ws-1" }));
    expect([...choices.querySelectorAll("[data-band-return]")].map((e) => e.getAttribute("data-band-return"))).toEqual(
      BAND_RETURNS.merged.map((band) => band.operation),
    );
  });
});

describe("the row click", () => {
  it("calls SelectWorkspace and nothing else", async () => {
    const calls: string[] = [];
    const sc = sidebarContext(
      appContext({
        selectWorkspace: (request) => {
          calls.push(request.workspace?.id ?? "");
          return SELECT_OK;
        },
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    await click(drawn.querySelector("[data-select]") as Element);
    expect(calls).toEqual(["ws-1"]);
  });

  it("selects nothing while the workspace is still opening", async () => {
    // Arrange
    const calls: string[] = [];
    const sc = sidebarContext(
      appContext({
        selectWorkspace: (request) => {
          calls.push(request.workspace?.id ?? "");
          return SELECT_OK;
        },
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "inactive", value: {} }, opening: true }), sc, "R");

    // Act
    await click(drawn.querySelector(".row") as Element);

    // Assert
    expect(calls).toEqual([]);
  });

  it("offers no select target while the workspace is still opening", () => {
    // Arrange
    const sc = sidebarContext(appContext({}));

    // Act
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "inactive", value: {} }, opening: true }), sc, "R");

    // Assert
    expect([drawn.querySelector("[data-select]"), drawn.querySelector(".row")?.getAttribute("aria-disabled")]).toEqual([
      null,
      "true",
    ]);
  });

  it("selects an inactive workspace no open is in flight for", async () => {
    // Arrange
    const calls: string[] = [];
    const sc = sidebarContext(
      appContext({
        selectWorkspace: (request) => {
          calls.push(request.workspace?.id ?? "");
          return SELECT_OK;
        },
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "inactive", value: {} } }), sc, "R");

    // Act
    await click(drawn.querySelector(".row") as Element);

    // Assert
    expect(calls).toEqual(["ws-1"]);
  });

  it("echoes the served ref rather than rebuilding one", async () => {
    let dir = "";
    const sc = sidebarContext(
      appContext({
        selectWorkspace: (request) => {
          dir = request.workspace?.dir ?? "";
          return SELECT_OK;
        },
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1", dir: "/w/one" }), sc, "R");
    await click(drawn.querySelector("[data-select]") as Element);
    expect(dir).toBe("/w/one");
  });

  it("says a refusal at the row itself", async () => {
    const sc = sidebarContext(
      appContext({
        selectWorkspace: () =>
          create(SelectWorkspaceResponseSchema, {
            result: {
              case: "error",
              value: { cause: { case: "unknownWorkspace", value: {} } },
            },
          }),
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    await click(drawn.querySelector("[data-select]") as Element);
    expect(drawn.querySelector(":scope > .sb-refusal")?.textContent).toBe(
      "the daemon does not know this workspace",
    );
  });

  it("labels the row's refusal with the cause's own arm", async () => {
    const sc = sidebarContext(
      appContext({
        selectWorkspace: () =>
          create(SelectWorkspaceResponseSchema, {
            result: {
              case: "error",
              value: { cause: { case: "notYetAdopted", value: {} } },
            },
          }),
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    await click(drawn.querySelector("[data-select]") as Element);
    expect(drawn.querySelector(":scope > .sb-refusal")?.getAttribute("data-arm")).toBe(
      "notYetAdopted",
    );
  });
});

describe("the selection fade", () => {
  /** A rail context whose selection fade reads CLOCK, with every drawn row under the document. */
  const fadeContext = (clock: () => number) => {
    const sc = sidebarContext(appContext({ selectWorkspace: () => SELECT_OK }));
    return { ...sc, selectionFade: createSelectionFade(document, clock) };
  };
  const rising = (drawn: HTMLElement): boolean => drawn.classList.contains(SELECTION_NUMBER_RISE.className);

  afterEach(() => {
    document.body.replaceChildren();
  });

  it("rises the number of a row drawn current after another row was", () => {
    // Arrange
    const sc = fadeContext(() => NOW);
    drawRosterRow(row({ id: "ws-1", current: true }), sc, "R");

    // Act
    const drawn = drawRosterRow(row({ id: "ws-2", current: true }), sc, "R");

    // Assert
    expect(rising(drawn)).toBe(true);
  });

  it("does not fade a number on a click alone: the highlight has not moved", async () => {
    // Arrange
    const sc = fadeContext(() => NOW);
    document.body.appendChild(drawRosterRow(row({ id: "ws-1", current: true }), sc, "R"));
    const drawn = drawRosterRow(row({ id: "ws-2" }), sc, "R");
    document.body.appendChild(drawn);

    // Act
    await click(drawn.querySelector("[data-select]") as Element);

    // Assert
    expect(rising(drawn)).toBe(false);
  });

  it("draws a row redrawn mid-rise into the rise", () => {
    // Arrange
    let clock = NOW;
    const sc = fadeContext(() => clock);
    drawRosterRow(row({ id: "ws-1", current: true }), sc, "R");
    drawRosterRow(row({ id: "ws-2", current: true }), sc, "R");
    const elapsed = SELECTION_NUMBER_RISE_MS / 2;
    clock += elapsed;

    // Act
    const after = drawRosterRow(row({ id: "ws-2", current: true }), sc, "R");

    // Assert
    expect([rising(after), after.style.getPropertyValue(SELECTION_NUMBER_RISE.delayProperty)]).toEqual([
      true,
      `-${elapsed}ms`,
    ]);
  });
});

describe("the verb menu", () => {
  it("is drawn with the row, hidden until it is asked for", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect((drawn.querySelector(".sb-menu") as HTMLElement).hidden).toBe(true);
  });

  it("opens on the row's own control", async () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    await click(drawn.querySelector(".sb-more") as Element);
    expect((drawn.querySelector(".sb-menu") as HTMLElement).hidden).toBe(false);
  });

  it("opens DOWNWARD, under the row line, so it cannot clip off the top", async () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    await click(drawn.querySelector(".sb-more") as Element);
    expect(drawn.querySelector(".row")?.nextElementSibling?.classList.contains("sb-menu")).toBe(
      true,
    );
  });

  it("closes on a second click rather than stacking a second menu", async () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    await click(drawn.querySelector(".sb-more") as Element);
    await click(drawn.querySelector(".sb-more") as Element);
    expect((drawn.querySelector(".sb-menu") as HTMLElement).hidden).toBe(true);
    expect(drawn.querySelectorAll(".sb-menu")).toHaveLength(1);
  });

  it("opens on a right-click of the row", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    drawn
      .querySelector(".row")
      ?.dispatchEvent(new MouseEvent("contextmenu", { bubbles: true }));
    expect((drawn.querySelector(".sb-menu") as HTMLElement).hidden).toBe(false);
  });

  it("does not select the workspace when the control is clicked", async () => {
    let calls = 0;
    const sc = sidebarContext(
      appContext({
        selectWorkspace: () => {
          calls += 1;
          return SELECT_OK;
        },
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    await click(drawn.querySelector(".sb-more") as Element);
    expect(calls).toBe(0);
  });
});

describe("a malformed row", () => {
  it("refuses a row with no lifecycle at all", () => {
    const malformed = create(RosterRowSchema, {
      workspace: { workspace: { id: "ws-1", dir: "/w" } },
      name: { text: "one" },
      current: { current: false },
      closed: { closed: false },
      when: {},
      detail: {},
    });
    expect(() => drawRosterRow(malformed, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("refuses a row with no workspace", () => {
    const malformed = create(RosterRowSchema, {
      name: { text: "one" },
      status: { case: "ready", value: {} },
      current: { current: false },
      closed: { closed: false },
      when: {},
      detail: {},
    });
    expect(() => drawRosterRow(malformed, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("refuses a row with no name", () => {
    const malformed = create(RosterRowSchema, {
      workspace: { workspace: { id: "ws-1", dir: "/w" } },
      status: { case: "ready", value: {} },
      current: { current: false },
      closed: { closed: false },
      when: {},
      detail: {},
    });
    expect(() => drawRosterRow(malformed, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("refuses a row that does not state its highlight", () => {
    const malformed = create(RosterRowSchema, {
      workspace: { workspace: { id: "ws-1", dir: "/w" } },
      name: { text: "one" },
      status: { case: "ready", value: {} },
      closed: { closed: false },
      when: {},
      detail: {},
    });
    expect(() => drawRosterRow(malformed, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("refuses a row that does not state its receded styling", () => {
    const malformed = create(RosterRowSchema, {
      workspace: { workspace: { id: "ws-1", dir: "/w" } },
      name: { text: "one" },
      status: { case: "ready", value: {} },
      current: { current: false },
      when: {},
      detail: {},
    });
    expect(() => drawRosterRow(malformed, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("refuses a row with no when-column box", () => {
    const malformed = create(RosterRowSchema, {
      workspace: { workspace: { id: "ws-1", dir: "/w" } },
      name: { text: "one" },
      status: { case: "ready", value: {} },
      current: { current: false },
      closed: { closed: false },
      detail: {},
    });
    expect(() => drawRosterRow(malformed, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("refuses a row with no detail box", () => {
    const malformed = create(RosterRowSchema, {
      workspace: { workspace: { id: "ws-1", dir: "/w" } },
      name: { text: "one" },
      status: { case: "ready", value: {} },
      current: { current: false },
      closed: { closed: false },
      when: {},
    });
    expect(() => drawRosterRow(malformed, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("refuses a child that is itself malformed", () => {
    const malformed = create(RosterRowSchema, {
      workspace: { workspace: { id: "ws-2", dir: "/w" } },
      name: { text: "two" },
      current: { current: false },
      closed: { closed: false },
      when: {},
      detail: {},
    });
    const parent = row({ id: "ws-1", children: [malformed] });
    expect(() => drawRosterFamily(openRow(parent), sidebarContext(), "R")).toThrow(MalformedView);
  });
});

describe("a when-column arm this build has never heard of", () => {
  it("is a malformed view, never a blank column", () => {
    expect(() =>
      drawRosterRowWhen(
        { shown: { case: "aFutureArm", value: {} } } as never,
        sidebarContext(),
        "R.when",
      ),
    ).toThrow(new MalformedView("R.when.shown", "arm 'aFutureArm' is not one this build can draw"));
  });
});

describe("the row menu toggle", () => {
  const TARGET = {
    sc: sidebarContext(),
    workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w/one" }),
    name: "one",
  };

  it("reveals the menu the row owns", () => {
    const ws = document.createElement("div");
    const menu = document.createElement("div");
    menu.className = "sb-menu";
    menu.hidden = true;
    ws.appendChild(menu);
    toggleRowMenu(ws, TARGET);
    expect(menu.hidden).toBe(false);
  });

  it("leaves a nested row's menu alone, because the menu must be this row's own", () => {
    const ws = document.createElement("div");
    const child = document.createElement("div");
    const menu = document.createElement("div");
    menu.className = "sb-menu";
    menu.hidden = true;
    child.appendChild(menu);
    ws.appendChild(child);
    toggleRowMenu(ws, TARGET);
    expect(menu.hidden).toBe(true);
  });
});

describe("the detail panel opens on hover", () => {
  /**
   * Owner ruling, 2026-09-14: there is no chevron; hovering the row does what
   * opening the chevron did. NO REAL TIMERS — the two delays are advanced.
   */
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  it("opens nothing on a resting row", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("does not open when the pointer rests on the status dot", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    // ACT
    (lineOf(drawn).querySelector(".st") as HTMLElement).dispatchEvent(new MouseEvent("mouseenter"));
    vi.runAllTimers();
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("does not open when the pointer rests on the name", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    // ACT
    (lineOf(drawn).querySelector(".name") as HTMLElement).dispatchEvent(new MouseEvent("mouseenter"));
    vi.runAllTimers();
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("opens when the pointer reaches a running turn's duration", () => {
    // ARRANGE
    const drawn = drawRosterRow(
      row({ id: "ws-1", when: { case: "turnRunning", value: { startedAtMs: BigInt(NOW) } } }),
      sidebarContext(),
      "R",
    );
    // ACT
    hoverIn(drawn);
    // ASSERT
    expect([durationOf(drawn).getAttribute("data-when"), drawn.classList.contains("open")]).toEqual(["turnRunning", true]);
  });

  it("opens the moment the pointer reaches the duration, arming no timer", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    const timers = vi.getTimerCount();
    // ACT
    hoverIn(drawn);
    // ASSERT
    expect([drawn.classList.contains("open"), vi.getTimerCount()]).toEqual([true, timers]);
  });

  it("does not open when the pointer rests on the row line itself", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    // ACT
    lineOf(drawn).dispatchEvent(new MouseEvent("mouseenter"));
    vi.runAllTimers();
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("closes again after a pass-through once the grace has run out", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    // ACT: the pointer crosses the duration on its way somewhere else.
    hoverIn(drawn);
    hoverOut(drawn);
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("closes once the pointer has left the row and the grace has run out", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    hoverIn(drawn);
    // ACT
    hoverOut(drawn);
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("is still open inside the grace, so the pointer can travel to the panel", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    hoverIn(drawn);
    // ACT
    hoverOut(drawn);
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS - 1);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(true);
  });

  it("stays open while the pointer is inside the panel", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    hoverIn(drawn);
    hoverOut(drawn);
    // ACT: the pointer arrives in the panel before the grace runs out.
    panelOf(drawn).dispatchEvent(new MouseEvent("mouseenter"));
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS * 10);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(true);
  });

  it("closes when the pointer leaves the panel too", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    hoverIn(drawn);
    hoverOut(drawn);
    panelOf(drawn).dispatchEvent(new MouseEvent("mouseenter"));
    // ACT
    panelOf(drawn).dispatchEvent(new MouseEvent("mouseleave"));
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(false);
  });

  describe.each([
    {
      place: "merged" as const,
      make: () =>
        row({ id: "ws-1", closed: true, status: { case: "merged", value: {} }, when: { case: "merged", value: { atMs: BigInt(NOW) } } }),
    },
    { place: "hidden" as const, make: () => row({ id: "ws-1", when: { case: "hidden", value: { atMs: BigInt(NOW) } } }) },
  ])("on a $place row, whose duration is the trigger as everywhere", ({ place, make }) => {
    const bandRow = (): HTMLElement => drawRosterRow(make(), sidebarContext(), "R", place);

    it("opens the moment the pointer reaches the duration", () => {
      // ARRANGE
      const drawn = bandRow();
      // ACT
      hoverIn(drawn);
      // ASSERT
      expect(drawn.classList.contains("open")).toBe(true);
    });

    it("closes once the pointer has left the duration and the grace has run out", () => {
      // ARRANGE
      const drawn = bandRow();
      hoverIn(drawn);
      // ACT
      hoverOut(drawn);
      vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
      // ASSERT
      expect(drawn.classList.contains("open")).toBe(false);
    });

    it("does not open when the pointer rests on the row line itself", () => {
      // ARRANGE
      const drawn = bandRow();
      // ACT
      lineOf(drawn).dispatchEvent(new MouseEvent("mouseenter"));
      vi.runAllTimers();
      // ASSERT
      expect(drawn.classList.contains("open")).toBe(false);
    });
  });

  it("opens at once when the keyboard focus reaches the row", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    // ACT
    lineOf(drawn).dispatchEvent(new FocusEvent("focusin", { bubbles: true }));
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(true);
  });

  it("closes again when the focus leaves the row entirely", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    lineOf(drawn).dispatchEvent(new FocusEvent("focusin", { bubbles: true }));
    // ACT
    lineOf(drawn).dispatchEvent(
      new FocusEvent("focusout", { bubbles: true, relatedTarget: document.body }),
    );
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("keeps the panel open while the focus moves BETWEEN the row's own controls", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    const line = lineOf(drawn);
    line.dispatchEvent(new FocusEvent("focusin", { bubbles: true }));
    // ACT
    line.dispatchEvent(
      new FocusEvent("focusout", { bubbles: true, relatedTarget: line.querySelector(".sb-more") }),
    );
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(true);
  });

  it("still selects the workspace when the row itself is clicked", async () => {
    // ARRANGE
    vi.useRealTimers();
    let calls = 0;
    const sc = sidebarContext(
      appContext({
        selectWorkspace: () => {
          calls += 1;
          return SELECT_OK;
        },
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    // ACT
    await click(lineOf(drawn));
    // ASSERT
    expect(calls).toBe(1);
  });
});

describe("the detail panel leaves the rail and stays inside the window", () => {
  // jsdom reports every rect as zero, so the rects this placement reads are
  // staged on the prototype and the window's size is stubbed — the same
  // arrangement the topbar reveal suite uses for the same reason.
  const rects = new Map<Element, DOMRect>();
  let original: typeof Element.prototype.getBoundingClientRect;

  const rect = (init: { left: number; top: number; width: number; height: number }): DOMRect =>
    ({
      left: init.left,
      top: init.top,
      right: init.left + init.width,
      bottom: init.top + init.height,
      width: init.width,
      height: init.height,
      x: init.left,
      y: init.top,
      toJSON: () => ({}),
    });

  beforeEach(() => {
    rects.clear();
    // Captured to be ASSIGNED back in afterEach, never called off the reference.
    // eslint-disable-next-line @typescript-eslint/unbound-method -- see above
    original = Element.prototype.getBoundingClientRect;
    Element.prototype.getBoundingClientRect = function staged(this: Element): DOMRect {
      return rects.get(this) ?? rect({ left: 0, top: 0, width: 0, height: 0 });
    };
    vi.stubGlobal("innerWidth", 1000);
    vi.stubGlobal("innerHeight", 800);
  });

  afterEach(() => {
    Element.prototype.getBoundingClientRect = original;
    vi.unstubAllGlobals();
  });

  /** An expanded row whose line and panel have the staged rects given. */
  function expandedRow(line: DOMRect, panel: DOMRect): HTMLElement {
    const drawn = drawRosterRow(
      row({ id: "ws-1", detail: { branch: { name: "feat/rail" } } }),
      sidebarContext(appContext(), fakeTimers(), createSidebarView(), new Set(["ws-1"])),
      "R",
    );
    rects.set(drawn.querySelector(":scope > .row") as Element, line);
    rects.set(drawn.querySelector(":scope > .detail") as Element, panel);
    return drawn;
  }

  it("grows the panel out of the right edge of the row's own line", () => {
    // ARRANGE: a rail-width row at the window's left edge.
    const drawn = expandedRow(
      rect({ left: 8, top: 100, width: 190, height: 24 }),
      rect({ left: 0, top: 0, width: 320, height: 90 }),
    );
    // ACT
    placeRowDetail(drawn);
    // ASSERT: the line's own right, and its top.
    expect([panelOf(drawn).style.left, panelOf(drawn).style.top]).toEqual(["198px", "100px"]);
  });

  it("slides a panel that would run off the right edge back inside the window", () => {
    // ARRANGE: a row near the right edge, with a panel wider than what is left.
    const drawn = expandedRow(
      rect({ left: 880, top: 100, width: 110, height: 24 }),
      rect({ left: 0, top: 0, width: 320, height: 90 }),
    );
    // ACT
    placeRowDetail(drawn);
    // ASSERT: held one margin clear of the right edge, never cut off by it.
    expect(panelOf(drawn).style.left).toBe(`${1000 - REVEAL_MARGIN_PX - 320}px`);
  });

  it("slides a panel near the bottom edge up until it fits the window", () => {
    // ARRANGE: a row low in the window, with a tall panel.
    const drawn = expandedRow(
      rect({ left: 8, top: 700, width: 190, height: 24 }),
      rect({ left: 0, top: 0, width: 320, height: 400 }),
    );
    // ACT
    placeRowDetail(drawn);
    // ASSERT: held one margin clear of the bottom edge.
    expect(panelOf(drawn).style.top).toBe(`${800 - REVEAL_MARGIN_PX - 400}px`);
  });

  it("caps a panel taller than the window at the height the window leaves it", () => {
    // ARRANGE
    const drawn = expandedRow(
      rect({ left: 8, top: 300, width: 190, height: 24 }),
      rect({ left: 0, top: 0, width: 320, height: 2000 }),
    );
    // ACT
    placeRowDetail(drawn);
    // ASSERT: it scrolls inside itself rather than running past the bottom.
    expect(panelOf(drawn).style.maxHeight).toBe(`${800 - 2 * REVEAL_MARGIN_PX}px`);
  });

  it("measures the panel free of the cap the last placement applied", () => {
    // ARRANGE: a panel whose last placement capped it, measured as tall as
    // its natural height only once that cap is lifted.
    const drawn = expandedRow(
      rect({ left: 8, top: 700, width: 190, height: 24 }),
      rect({ left: 0, top: 0, width: 320, height: 90 }),
    );
    const panel = panelOf(drawn);
    panel.style.maxHeight = "50px";
    panel.getBoundingClientRect = (): DOMRect =>
      rect({ left: 0, top: 0, width: 320, height: panel.style.maxHeight === "" ? 400 : 90 });
    // ACT
    placeRowDetail(drawn);
    // ASSERT: placed by its natural 400px, not the capped 90px.
    expect(panel.style.top).toBe(`${800 - REVEAL_MARGIN_PX - 400}px`);
  });

  it("places a row that was drawn already expanded, once it is on the page", () => {
    // ARRANGE
    const drawn = expandedRow(
      rect({ left: 8, top: 40, width: 190, height: 24 }),
      rect({ left: 0, top: 0, width: 320, height: 90 }),
    );
    const host = document.createElement("div");
    host.appendChild(drawn);
    // ACT
    placeOpenRowDetails(host);
    // ASSERT
    expect(panelOf(drawn).style.top).toBe("40px");
  });

  it("leaves a closed row's panel unplaced", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    const host = document.createElement("div");
    host.appendChild(drawn);
    // ACT
    placeOpenRowDetails(host);
    // ASSERT
    expect(panelOf(drawn).style.top).toBe("");
  });

  it("places the panel the moment the hover opens it", () => {
    // ARRANGE: a resting row, expanded by the hover rather than by the draw.
    vi.useFakeTimers();
    try {
      const drawn = drawRosterRow(
        row({ id: "ws-1", detail: { branch: { name: "feat/rail" } } }),
        sidebarContext(),
        "R",
      );
      rects.set(
        drawn.querySelector(":scope > .row") as Element,
        rect({ left: 8, top: 200, width: 190, height: 24 }),
      );
      rects.set(
        drawn.querySelector(":scope > .detail") as Element,
        rect({ left: 0, top: 0, width: 320, height: 90 }),
      );
      // ACT
      hoverIn(drawn);
      // ASSERT
      expect(panelOf(drawn).style.top).toBe("200px");
    } finally {
      vi.useRealTimers();
    }
  });
});

describe("the unread turn end under an idle_async row", () => {
  const nameOf = (drawn: HTMLElement): HTMLElement => drawn.querySelector(":scope > .row > .name") as HTMLElement;
  const paintOf = (drawn: HTMLElement, selector: string): string =>
    inRail(drawn, () => cascadedValue(drawn.querySelector(selector) as HTMLElement, "color"));
  const idle = { case: "idleAsync", value: {} } as const;

  it.each([
    { name: "an unread done inks the name green", end: "done", want: "var(--ok)" },
    { name: "an unread interrupted inks the name green", end: "interrupted", want: "var(--ok)" },
    { name: "an unread failed turn inks the name turquoise", end: "turnFailed", want: "var(--turquoise)" },
  ] as const)("$name", ({ end, want }) => {
    // Arrange, Act.
    const drawn = drawRosterRow(
      row({ id: "ws-1", status: idle, idleAsyncUnread: { turnEnd: { case: end, value: {} } } }),
      sidebarContext(),
      "R",
    );

    // Assert.
    expect(paintOf(drawn, ":scope > .row > .name")).toBe(want);
    expect(drawn.getAttribute("data-unread-turn-end")).toBe(end);
  });

  it("keeps the status dot on the background's yellow", () => {
    // Arrange, Act.
    const drawn = drawRosterRow(
      row({ id: "ws-1", status: idle, idleAsyncUnread: { turnEnd: { case: "done", value: {} } } }),
      sidebarContext(),
      "R",
    );

    // Assert.
    expect(paintOf(drawn, ":scope > .row > .st")).toBe("var(--async)");
  });

  it("leaves a read idle_async row's name on the background's yellow", () => {
    // Arrange, Act.
    const drawn = drawRosterRow(row({ id: "ws-1", status: idle }), sidebarContext(), "R");

    // Assert.
    expect(paintOf(drawn, ":scope > .row > .name")).toBe("var(--async)");
    expect(drawn.hasAttribute("data-unread-turn-end")).toBe(false);
  });

  it("draws no turn-end color on a hidden row", () => {
    // Arrange, Act.
    const drawn = drawRosterRow(
      row({ id: "ws-1", status: idle, idleAsyncUnread: { turnEnd: { case: "done", value: {} } } }),
      sidebarContext(),
      "R",
      "hidden",
    );

    // Assert.
    expect(nameOf(drawn).classList.contains("tone-green")).toBe(false);
  });

  it("refuses the marker on any arm but idleAsync", () => {
    // Arrange.
    const bad = row({ id: "ws-1", status: { case: "done", value: {} }, idleAsyncUnread: { turnEnd: { case: "done", value: {} } } });

    // Act, Assert.
    expect(() => drawRosterRow(bad, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("refuses a marker that names no turn end", () => {
    // Arrange.
    const bad = row({ id: "ws-1", status: idle, idleAsyncUnread: {} });

    // Act, Assert.
    expect(() => drawRosterRow(bad, sidebarContext(), "R")).toThrow(MalformedView);
  });
});

describe("the row's display mode", () => {
  it("greys the NAME when the daemon carries the viewed marker", () => {
    // Arrange, Act.
    const drawn = drawRosterRow(row({ id: "ws-1", viewed: true }), sidebarContext(), "R");

    // Assert.
    const name = drawn.querySelector(":scope > .row > .name") as HTMLElement;
    expect(name.classList.contains("viewed")).toBe(true);
  });

  it("leaves the NAME alone when it carries none", () => {
    // Arrange, Act.
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");

    // Assert.
    const name = drawn.querySelector(":scope > .row > .name") as HTMLElement;
    expect(name.classList.contains("viewed")).toBe(false);
  });

  it("marks the whole row PARTIAL for the hook contract", () => {
    // Arrange, Act.
    const drawn = drawRosterRow(row({ id: "ws-1", viewed: true }), sidebarContext(), "R");

    // Assert.
    expect(drawn.getAttribute("data-viewed")).toBe("true");
  });

  it("keeps the status dot's tone class in partial mode", () => {
    // Arrange: the mode says what the user has SEEN, never what the workspace
    // is doing -- the dot is what says that.
    const sc = sidebarContext();

    // Act.
    const full = drawRosterRow(row({ id: "ws-1", status: { case: "done", value: {} } }), sc, "R");
    const partial = drawRosterRow(
      row({ id: "ws-2", status: { case: "done", value: {} }, viewed: true }),
      sc,
      "R",
    );

    // Assert.
    const dot = (el: HTMLElement): string =>
      (el.querySelector(":scope > .row > .st") as HTMLElement).className;
    expect(dot(partial)).toBe(dot(full));
  });

  describe("the status dot's paint", () => {
    const paintOf = (drawn: HTMLElement, selector: string): string =>
      inRail(drawn, () => cascadedValue(drawn.querySelector(selector) as HTMLElement, "color"));

    it.each([
      { name: "a viewed done row's dot is green", status: "done", viewed: true, want: "var(--ok)" },
      { name: "an unviewed done row's dot is green", status: "done", viewed: false, want: "var(--ok)" },
    ] as const)("$name", ({ status, viewed, want }) => {
      // Arrange, Act.
      const drawn = drawRosterRow(row({ id: "ws-1", status: { case: status, value: {} }, viewed }), sidebarContext(), "R");

      // Assert.
      expect(paintOf(drawn, ":scope > .row > .st")).toBe(want);
    });

    it("paints a viewed interrupted row's dot as an unviewed one", () => {
      // Arrange, Act.
      const at = (viewed: boolean): HTMLElement =>
        drawRosterRow(row({ id: "ws-1", status: { case: "interrupted", value: {} }, viewed }), sidebarContext(), "R");

      // Assert.
      expect(paintOf(at(true), ":scope > .row > .st")).toBe(paintOf(at(false), ":scope > .row > .st"));
    });

    it("paints a viewed row's name grey while its dot keeps its tone", () => {
      // Arrange, Act.
      const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "done", value: {} }, viewed: true }), sidebarContext(), "R");

      // Assert.
      expect(paintOf(drawn, ":scope > .row > .name")).toBe("var(--muted)");
    });

    it("leaves an unviewed child's dot green under a viewed parent", () => {
      // Arrange, Act.
      const drawn = drawRosterFamily(
        openRow(
          row({
            id: "parent",
            status: { case: "done", value: {} },
            viewed: true,
            children: [row({ id: "child", status: { case: "done", value: {} } })],
          }),
        ),
        sidebarContext(),
        "R",
      );

      // Assert.
      expect(paintOf(drawn, ".kids .st")).toBe("var(--ok)");
    });
  });

  it("draws PARTIAL on the push that changes the status when the marker is set", () => {
    // Arrange: the row was last drawn FULL at idle_async.
    const sc = sidebarContext();
    drawRosterRow(row({ id: "ws-1", status: { case: "idleAsync", value: {} } }), sc, "R");

    // Act: the detached work ended and the daemon says the result is read.
    const drawn = drawRosterRow(
      row({ id: "ws-1", status: { case: "done", value: {} }, viewed: true }),
      sc,
      "R",
    );

    // Assert: the daemon resolves the marker with the status, so the page
    // draws what the wire says.
    const name = drawn.querySelector(":scope > .row > .name") as HTMLElement;
    expect(name.classList.contains("viewed")).toBe(true);
  });
});

describe("the reviving shimmer", () => {
  it("shimmers the NAME while the daemon carries the reviving marker", () => {
    // Arrange, Act.
    const drawn = drawRosterRow(row({ id: "ws-1", reviving: true }), sidebarContext(), "R");

    // Assert.
    const name = drawn.querySelector(":scope > .row > .name") as HTMLElement;
    expect(name.classList.contains("reviving")).toBe(true);
  });

  it("leaves the NAME still once the marker is gone", () => {
    // Arrange, Act: the daemon lowered the marker when the revival ended.
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");

    // Assert.
    const name = drawn.querySelector(":scope > .row > .name") as HTMLElement;
    expect(name.classList.contains("reviving")).toBe(false);
  });

  it("marks the whole row reviving for the hook contract", () => {
    // Arrange, Act.
    const drawn = drawRosterRow(row({ id: "ws-1", reviving: true }), sidebarContext(), "R");

    // Assert.
    expect(drawn.getAttribute("data-reviving")).toBe("true");
  });

  it("leaves the status dot's tone untouched while reviving", () => {
    // Arrange: the marker is not a status arm.
    const sc = sidebarContext();

    // Act.
    const still = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    const reviving = drawRosterRow(row({ id: "ws-2", reviving: true }), sc, "R");

    // Assert.
    const dot = (el: HTMLElement): string =>
      (el.querySelector(":scope > .row > .st") as HTMLElement).className;
    expect(dot(reviving)).toBe(dot(still));
  });

  it("keeps the viewed mode alongside the shimmer", () => {
    // Arrange, Act.
    const drawn = drawRosterRow(
      row({ id: "ws-1", viewed: true, reviving: true }),
      sidebarContext(),
      "R",
    );

    // Assert: the two markers are independent.
    const name = drawn.querySelector(":scope > .row > .name") as HTMLElement;
    expect(name.classList.contains("viewed")).toBe(true);
  });
});

describe("the durable last-selected instant", () => {
  it("draws a row carrying it exactly as the same row without it", () => {
    // Arrange: the instant is an ordering fact for clients, never drawn.
    const plain = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");

    // Act.
    const stamped = drawRosterRow(
      row({ id: "ws-1", lastSelectedAtMs: 1756400000000n }),
      sidebarContext(),
      "R",
    );

    // Assert.
    expect(stamped.outerHTML).toBe(plain.outerHTML);
  });
});

describe("the row's drawn status is recorded", () => {
  /** The sidebar.drawn-status records a capture holds, in order. */
  async function drawnStatuses(capture: LogCapture) {
    capture.logger.flush();
    await Promise.resolve();
    return capture.sent
      .filter((record) => record.operation === "sidebar.drawn-status")
      .map((record) => record.context as Record<string, unknown>);
  }

  it("records the arm a row draws, keyed by its workspace", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    drawRosterRow(row({ id: "ws-1", status: { case: "thinking", value: {} } }), sidebarContext(), "R");
    // Assert
    expect(await drawnStatuses(capture)).toEqual([
      expect.objectContaining({ key: "ws-1", arm: "thinking", source: "daemon" }),
    ]);
  });

  it("records nothing when a redraw draws the row's same arm", async () => {
    // Arrange
    const capture = captureLogRecords();
    const sc = sidebarContext();
    drawRosterRow(row({ id: "ws-1", status: { case: "thinking", value: {} } }), sc, "R");
    // Act
    drawRosterRow(row({ id: "ws-1", status: { case: "thinking", value: {} } }), sc, "R");
    // Assert
    expect(await drawnStatuses(capture)).toHaveLength(1);
  });

  it("records a row whose arm changed with the arm it drew before", async () => {
    // Arrange
    const capture = captureLogRecords();
    const sc = sidebarContext();
    drawRosterRow(row({ id: "ws-1", status: { case: "thinking", value: {} } }), sc, "R");
    // Act
    drawRosterRow(row({ id: "ws-1", status: { case: "severed", value: {} } }), sc, "R");
    // Assert
    expect((await drawnStatuses(capture)).at(-1)).toEqual(
      expect.objectContaining({ arm: "severed", previous_arm: "thinking" }),
    );
  });
});

describe("a merge that landed today", () => {
  const mergedRow = (atMs: number, sc = sidebarContext(), merged = true): HTMLElement =>
    drawRosterRow(
      row({ id: "ws-1", status: { case: "merged", value: {} }, when: { case: "merged", value: { atMs: BigInt(atMs) } } }),
      sc,
      "R",
      merged ? "merged" : "grouping",
    );
  const hasGutter = (drawn: HTMLElement): boolean | undefined =>
    drawn.querySelector(".row")?.classList.contains(MERGED_TODAY_CLASS);

  it.each([
    { name: "the same instant", at: () => NOW, now: () => NOW, want: true },
    { name: "local midnight that began today", at: startOfToday, now: () => NOW, want: true },
    { name: "the last millisecond of yesterday", at: () => startOfToday() - 1, now: () => NOW, want: false },
    { name: "a merge seen again after midnight", at: () => NOW, now: startOfTomorrow, want: false },
  ])("decides $name by the local calendar date", ({ at, now, want }) => {
    // Act + Assert
    expect(mergedToday(at(), now())).toBe(want);
  });

  it("marks the row with the gutter", () => {
    expect(hasGutter(mergedRow(NOW - 60_000))).toBe(true);
  });

  it("leaves a merge from yesterday without the gutter", () => {
    expect(hasGutter(mergedRow(startOfToday() - 1))).toBe(false);
  });

  it("drops the gutter once the shared ticker crosses midnight", () => {
    // Arrange
    const ticker = fakeTicker();
    const drawn = mergedRow(NOW, sidebarContext(appContext({}, ticker)));
    // Act
    ticker.tick(startOfTomorrow());
    // Assert
    expect(hasGutter(drawn)).toBe(false);
  });

  it("registers the gutter's tick as a teardown beside the age's", () => {
    const sc = sidebarContext();
    mergedRow(NOW, sc);
    expect(sc.disposers.length).toBe(2);
  });

  it("is not marked on a row drawn outside the merged band", () => {
    expect(hasGutter(mergedRow(NOW, sidebarContext(), false))).toBe(false);
  });

  it("leaves the name's own colour class untouched", () => {
    expect(mergedRow(NOW).querySelector(".name")?.classList.contains(MERGED_TODAY_CLASS)).toBe(false);
  });
});

describe("the one-shot mark", () => {
  it.each(["submitting", "thinking", "clearing"] as const)("draws a breathing target in place of the %s mark", (arm) => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: arm, value: {} }, oneShot: true }), sidebarContext(), "R");
    expect([dotOf(drawn).getAttribute("data-glyph"), dotOf(drawn).innerHTML]).toEqual(["target", drawnPicture("target")]);
  });

  it.each(["compacting", "done", "idleAsync", "permission", "question", "coldGate", "mergeQueued", "merging", "mergeFailed"] as const)(
    "draws a one-shot's own %s mark",
    (arm) => {
      const drawn = drawRosterRow(row({ id: "ws-1", status: { case: arm, value: {} }, oneShot: true }), sidebarContext(), "R");
      expect(dotOf(drawn).outerHTML).toBe(drawStatusMark(arm, "R.status", NO_MARKERS).outerHTML);
    },
  );

  it("draws the plain dot for a thinking row that is no one-shot", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "thinking", value: {} } }), sidebarContext(), "R");
    expect(dotOf(drawn).getAttribute("data-glyph")).toBe("dot");
  });

  it("answers true for a one-shot mark that is there", () => {
    const mark = row({ id: "ws-1", oneShot: true }).oneShot;
    expect(mark).toBeDefined();
    expect(drawRosterRowOneShot(mark!, "R.oneShot")).toBe(true);
  });
});

// AN ASYNC MERGE OF THE WORKSPACE'S OWN BRANCH (owner request, 2026-10-13):
// the NAME keeps the workspace's own status color and only a plain dot gives
// way to the purple merge glyph; every other mark outranks it.
describe("the async-merging marker", () => {
  it.each([
    ["ready", "tone-green"],
    ["done", "tone-green"],
    ["thinking", "tone-red"],
    ["idleAsync", "tone-yellow"],
  ] as const)("keeps a %s row's own %s and draws the purple merge glyph, its name breathing", (arm, tone) => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: arm, value: {} }, asyncMerging: true }), sidebarContext(), "R");
    expect([
      drawn.classList.contains(tone),
      drawn.classList.contains("tone-purple"),
      dotOf(drawn).classList.contains(tone),
      dotOf(drawn).getAttribute("data-glyph"),
      dotOf(drawn).classList.contains("st-glyph"),
      dotOf(drawn).classList.contains("ink-purple"),
      lineOf(drawn).querySelector(".name")?.classList.contains("breathes"),
    ]).toEqual([true, false, true, "merge", true, true, true]);
  });

  it("draws the async merge's glyph with a running merge's picture", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "ready", value: {} }, asyncMerging: true }), sidebarContext(), "R");
    expect(dotOf(drawn).innerHTML).toBe(drawStatusMark("merging", "R.status", NO_MARKERS).innerHTML);
  });

  it.each(["compacting", "permission", "question", "coldGate", "mergeFailed"] as const)(
    "draws the %s row's own mark over the async merge",
    (arm) => {
      const drawn = drawRosterRow(row({ id: "ws-1", status: { case: arm, value: {} }, asyncMerging: true }), sidebarContext(), "R");
      expect(dotOf(drawn).outerHTML).toBe(drawStatusMark(arm, "R.status", NO_MARKERS).outerHTML);
    },
  );

  it("draws a working one-shot's target over the async merge", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", status: { case: "thinking", value: {} }, oneShot: true, asyncMerging: true }),
      sidebarContext(),
      "R",
    );
    expect(dotOf(drawn).getAttribute("data-glyph")).toBe("target");
  });

  it("draws no purple ink on a row with no async merge", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "ready", value: {} } }), sidebarContext(), "R");
    expect(dotOf(drawn).classList.contains("ink-purple")).toBe(false);
  });

  it("answers true for an async-merging marker that is there", () => {
    const mark = row({ id: "ws-1", asyncMerging: true }).asyncMerging;
    expect(mark).toBeDefined();
    expect(drawRosterRowAsyncMerging(mark!, "R.asyncMerging")).toBe(true);
  });
});

// A PAUSED QUEUE (owner ruling, 2026-10-14): the merge indicator greys and
// the name stops breathing for the merge, keeping its color.
describe("the merge-paused marker", () => {

  it("greys a paused merging row's glyph", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "merging", value: {} }, mergePaused: true }), sidebarContext(), "R");
    expect(dotOf(drawn).classList.contains("ink-grey")).toBe(true);
  });

  it("stops a paused merging row's name breathing", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "merging", value: {} }, mergePaused: true }), sidebarContext(), "R");
    expect(nameOf(drawn).classList.contains("breathes")).toBe(false);
  });

  it("keeps a paused merging row's purple tone", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "merging", value: {} }, mergePaused: true }), sidebarContext(), "R");
    expect([drawn.classList.contains("tone-purple"), dotOf(drawn).classList.contains("tone-purple")]).toEqual([true, true]);
  });

  it("greys a paused merge_queued row's glyph", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "mergeQueued", value: {} }, mergePaused: true }), sidebarContext(), "R");
    expect([dotOf(drawn).classList.contains("ink-grey"), dotOf(drawn).classList.contains("ink-purple")]).toEqual([true, false]);
  });

  it("greys a paused async merge's mark and stops the name breathing", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", status: { case: "ready", value: {} }, asyncMerging: true, mergePaused: true }),
      sidebarContext(),
      "R",
    );
    expect([
      dotOf(drawn).getAttribute("data-glyph"),
      dotOf(drawn).classList.contains("ink-grey"),
      dotOf(drawn).classList.contains("ink-purple"),
      nameOf(drawn).classList.contains("breathes"),
      drawn.classList.contains("tone-green"),
    ]).toEqual(["merge", true, false, false, true]);
  });

  it("keeps a thinking row breathing under a paused async merge", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", status: { case: "thinking", value: {} }, asyncMerging: true, mergePaused: true }),
      sidebarContext(),
      "R",
    );
    expect(nameOf(drawn).classList.contains("breathes")).toBe(true);
  });

  it("answers true for a merge-paused marker that is there", () => {
    const mark = row({ id: "ws-1", mergePaused: true }).mergePaused;
    expect(mark).toBeDefined();
    expect(drawRosterRowMergePaused(mark!, "R.mergePaused")).toBe(true);
  });
});

describe("the bands draw no status mark", () => {
  it.each(["merged", "hidden"] as const)("draws no mark for a row in the %s band", (place) => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "thinking", value: {} } }), sidebarContext(), "R", place);
    expect(drawn.querySelector(".st")).toBeNull();
  });

  it("draws the mark for a row in a grouping", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "thinking", value: {} } }), sidebarContext(), "R", "grouping");
    expect(drawn.querySelector(".st")).not.toBeNull();
  });

  it.each(["grouping", "merged", "hidden"] as const)("keeps a cold gate's arm on the %s row for its strike", (place) => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "coldGate", value: {} } }), sidebarContext(), "R", place);
    expect(drawn.getAttribute("data-arm")).toBe("coldGate");
  });
});

// A HIDDEN ROW WEARS NO STATE (owner request, 2026-10-13): only a cold gate's
// strike, which rides on `data-arm`.
describe("the hidden section's rows", () => {
  it.each(["severed", "dead", "thinking", "done", "idleAsync", "turnFailed", "merging"] as const)(
    "draws a %s row in plain text, with no status color",
    (arm) => {
      const drawn = drawRosterRow(row({ id: "ws-1", status: { case: arm, value: {} } }), sidebarContext(), "R", "hidden");
      expect([...drawn.classList].filter((c) => c.startsWith("tone-"))).toEqual(["tone-none"]);
    },
  );

  it.each(["merged", "hidden"] as const)("draws a %s row's name in the bands' one name grey", (place) => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "thinking", value: {} } }), sidebarContext(), "R", place);
    expect(drawn.querySelector(":scope > .row > .name")?.classList.contains("band-name")).toBe(true);
  });

  it("leaves a grouping row's name out of the bands' grey", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "thinking", value: {} } }), sidebarContext(), "R", "grouping");
    expect(drawn.querySelector(":scope > .row > .name")?.classList.contains("band-name")).toBe(false);
  });

  it("paints a hidden row's name in the repository names' grey", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "done", value: {} } }), sidebarContext(), "R", "hidden");
    expect(inRail(drawn, () => cascadedValue(drawn.querySelector(":scope > .row > .name") as HTMLElement, "color"))).toBe(
      "var(--repo-head-expanded)",
    );
  });

  it.each([
    { name: "a hidden row recedes", place: "hidden", closed: false, want: true },
    { name: "a closed row recedes", place: "grouping", closed: true, want: true },
    { name: "an open grouping row does not recede", place: "grouping", closed: false, want: false },
  ] as const)("$name", ({ place, closed, want }) => {
    const drawn = drawRosterRow(row({ id: "ws-1", closed }), sidebarContext(), "R", place);
    expect(drawn.classList.contains("receded")).toBe(want);
  });

  it("draws a hidden row at the receded opacity a merged row wears", () => {
    const opacityOf = (place: "hidden" | "merged", closed: boolean): string => {
      const drawn = drawRosterRow(row({ id: "ws-1", closed }), sidebarContext(), "R", place);
      return inRail(drawn, () => cascadedValue(drawn.querySelector(":scope > .row > .name") as HTMLElement, "opacity"));
    };
    expect([opacityOf("hidden", false), opacityOf("merged", true)]).toEqual(["var(--receded-opacity)", "var(--receded-opacity)"]);
  });

  it("recedes at 0.55", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(inRail(drawn, (rail) => cascadedValue(rail, "--receded-opacity"))).toBe("0.55");
  });

  it.each([
    { name: "an open grouping row", place: "grouping", closed: false, row: "1", when: "var(--receded-opacity)" },
    { name: "a hidden row", place: "hidden", closed: false, row: "1", when: "var(--receded-opacity)" },
    { name: "a merged row", place: "merged", closed: true, row: "1", when: "var(--receded-opacity)" },
  ] as const)("draws $name's when-column at the one receded grey", ({ place, closed, row: rowOpacity, when }) => {
    // Arrange
    const drawn = drawRosterRow(row({ id: "ws-1", closed }), sidebarContext(), "R", place);
    // Act
    const opacities = inRail(drawn, () => [
      cascadedValue(drawn.querySelector(":scope > .row") as HTMLElement, "opacity") || "1",
      cascadedValue(drawn.querySelector(":scope > .row > .when") as HTMLElement, "opacity") || "1",
    ]);
    // Assert: the row's and the column's opacities compound to the receded one exactly once.
    expect(opacities).toEqual([rowOpacity, when]);
  });

  it.each<{ name: string; when: MessageInitShape<typeof RosterRowWhenSchema>["shown"]; want: string }>([
    { name: "a turn running", when: { case: "turnRunning", value: { startedAtMs: BigInt(NOW - 65_000) } }, want: "var(--when-counting-opacity)" },
    { name: "a sync merge", when: { case: "syncMerging", value: { startedAtMs: BigInt(NOW - 65_000) } }, want: "var(--when-counting-opacity)" },
    { name: "no turn running", when: { case: "noTurnRunning", value: {} }, want: "var(--receded-opacity)" },
  ])("draws an open row's when-column with $name at $want", ({ when, want }) => {
    // Arrange
    const drawn = drawRosterRow(row({ id: "ws-1", when }), sidebarContext(), "R", "grouping");
    // Act
    const opacity = inRail(drawn, () => cascadedValue(drawn.querySelector(":scope > .row > .when") as HTMLElement, "opacity"));
    // Assert
    expect(opacity).toBe(want);
  });

  it("lifts a counting when-column to 0.75", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(inRail(drawn, (rail) => cascadedValue(rail, "--when-counting-opacity"))).toBe("0.75");
  });

  it("strikes a hidden row's name through at a cold gate", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "coldGate", value: {} } }), sidebarContext(), "R", "hidden");
    expect(inRail(drawn, () => cascadedValue(drawn.querySelector(":scope > .row > .name") as HTMLElement, "text-decoration"))).toBe(
      "line-through",
    );
  });

  it("keeps the status color on the same row in a grouping", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", status: { case: "severed", value: {} } }), sidebarContext(), "R", "grouping");
    expect(drawn.classList.contains("tone-blue")).toBe(true);
  });

  it("does not grey a read row's name", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", status: { case: "done", value: {} }, viewed: true }),
      sidebarContext(),
      "R",
      "hidden",
    );
    expect([drawn.querySelector(".name")?.classList.contains("viewed"), drawn.hasAttribute("data-viewed")]).toEqual([false, false]);
  });

  it("draws no attention marker", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", attention: true }), sidebarContext(), "R", "hidden");
    expect([drawn.querySelector(".sb-attn"), drawn.hasAttribute("data-attention")]).toEqual([null, false]);
  });

  it("draws no reviving shimmer", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", reviving: true }), sidebarContext(), "R", "hidden");
    expect(drawn.querySelector(".name")?.classList.contains("reviving")).toBe(false);
  });

  it("draws a hidden child row in plain text too", () => {
    const drawn = drawRosterFamily(
      hiddenRow(row({ id: "ws-1", children: [row({ id: "ws-2", status: { case: "dead", value: {} } })] })),
      sidebarContext(),
      "R",
      "hidden",
    );
    expect(drawn.querySelector('[data-roster-row="ws-2"]')?.classList.contains("tone-blue")).toBe(false);
  });
});

describe("drawRosterRowStandIn", () => {
  it("is the row's box, line and name, marked as a stand-in", () => {
    // Act
    const standIn = drawRosterRowStandIn(openRow(row({ id: "ws-1", name: "alpha" })), "row");

    // Assert
    expect([
      standIn.getAttribute("data-roster-row"),
      standIn.hasAttribute(STAND_IN_ATTRIBUTE),
      standIn.querySelector(":scope > .row > .name")?.textContent,
    ]).toEqual(["ws-1", true, "alpha"]);
  });

  it("draws nothing of the row but its line", () => {
    // Act
    const standIn = drawRosterRowStandIn(openRow(row({ id: "ws-1" })), "row");

    // Assert
    expect(standIn.querySelectorAll("*").length).toBe(2);
  });

  it("carries a stand-in for each visible child, where the search looks for one", () => {
    // Act
    const standIn = drawRosterRowStandIn(openRow(row({ id: "ws-1", children: [row({ id: "ws-2", name: "beta" })] })), "row");

    // Assert
    expect(standIn.querySelector(":scope > .kids > [data-roster-row='ws-2'] > .row > .name")?.textContent).toBe("beta");
  });

  it("refuses a row naming no workspace as a malformed view", () => {
    // Arrange
    const bad = row({ id: "ws-1" });
    bad.workspace = undefined;

    // Act / Assert
    expect(() => drawRosterRowStandIn(openRow(bad), "row")).toThrow(MalformedView);
  });

  it("refuses a row with no name as a malformed view", () => {
    // Arrange
    const bad = row({ id: "ws-1" });
    bad.name = undefined;

    // Act / Assert
    expect(() => drawRosterRowStandIn(openRow(bad), "row")).toThrow(MalformedView);
  });
});

describe("appendRowKids", () => {
  it("draws each visible child into one kids box, in order", () => {
    // Arrange
    const ws = document.createElement("div");
    const parent = row({ id: "ws-1", children: [row({ id: "ws-2" }), row({ id: "ws-3" })] });

    // Act
    appendRowKids(ws, openRow(parent).children, "row", (child) => drawRosterRowStandIn(child, "child"));

    // Assert
    expect([...ws.querySelectorAll(":scope > .kids > [data-roster-row]")].map((el) => el.getAttribute("data-roster-row"))).toEqual([
      "ws-2",
      "ws-3",
    ]);
  });

  it("hands each child its path under the parent's", () => {
    // Arrange
    const paths: string[] = [];
    const parent = row({ id: "ws-1", children: [row({ id: "ws-2" })] });

    // Act
    appendRowKids(document.createElement("div"), openRow(parent).children, "row", (child, childPath) => {
      paths.push(childPath);
      return document.createElement("div");
    });

    // Assert
    expect(paths).toEqual(["row.children[0]"]);
  });

  it("draws no kids box for a row with no visible child", () => {
    // Arrange
    const ws = document.createElement("div");

    // Act
    appendRowKids(ws, [], "row", () => document.createElement("div"));

    // Assert
    expect(ws.querySelector(".kids")).toBeNull();
  });

  it("is how both a drawn row and its stand-in nest their children", () => {
    // Arrange
    const source = readFileSync(join(__dirname, "../../src/sidebar/row.ts"), "utf8");

    // Act
    const handRolled = source.match(/kids\.className = "kids"/g) ?? [];

    // Assert
    expect(handRolled).toHaveLength(1);
  });
});


describe("the tab number slot", () => {
  it("leads an open row's line with an empty number slot before the status mark", () => {
    // Act
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");

    // Assert
    const line = drawn.querySelector(":scope > .row")!;
    expect([line.children[0].className, line.children[0].textContent, line.children[1].classList.contains("st")]).toEqual([
      "sb-num",
      "",
      true,
    ]);
  });

  it.each<RowPlace>(["merged", "hidden"])("draws no number slot on a %s band row", (place) => {
    // Act
    const drawn = drawRosterRow(row({ id: "ws-1", closed: place === "merged" }), sidebarContext(), "R", place);

    // Assert
    expect(drawn.querySelector(".sb-num")).toBeNull();
  });
});
