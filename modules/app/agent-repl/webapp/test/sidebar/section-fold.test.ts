// @vitest-environment jsdom
// The one fold every daemon-held sidebar fold draws. See src/sidebar/section-fold.ts.
import { create } from "@bufbuild/protobuf";
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  UpdateSidebarViewResponseSchema,
  type UpdateSidebarViewRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import {
  CURRENT_FLASH_CLASS,
  FOCUS_FOLDED_SECTIONS,
  HOLDS_CURRENT_ATTRIBUTE,
  installFocusFold,
  foldOnClick,
  drawRosterHoldsCurrent,
  drawRosterSectionFold,
  paintHoldsCurrent,
  sectionFold,
  trackDaemonFold,
} from "../../src/sidebar/section-fold.js";
import { log } from "../../src/log.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { codeOf } from "../source-text.js";
import { createFocusLeft } from "../../src/focus-left.js";
import { createPresence } from "../../src/presence.js";
import { drawWorkspaceRoster } from "../../src/sidebar/roster.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import {
  NOW,
  appContext,
  hiddenSection,
  mergeQueueBand,
  mergedSection,
  mergingHead,
  queueGroup,
  roster,
  row,
  sidebarContext,
} from "./harness.js";

/** A sidebar whose daemon records every UpdateSidebarView and answers success. */
function recording(): { asked: UpdateSidebarViewRequest[]; sc: ReturnType<typeof sidebarContext> } {
  const asked: UpdateSidebarViewRequest[] = [];
  const sc = sidebarContext(
    appContext({
      updateSidebarView: (request) => {
        asked.push(request);
        return create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } });
      },
    }),
  );
  return { asked, sc };
}

describe("foldOnClick", () => {
  it.each([true, false])("marks the header as the fold control of a section folded %s", (folded) => {
    // Arrange
    const section = document.createElement("div");
    section.classList.toggle("folded", folded);
    const header = document.createElement("div");

    // Act
    foldOnClick(header, section, () => {});

    // Assert
    expect([header.hasAttribute("data-section-fold"), header.getAttribute("data-folded"), header.textContent]).toEqual([
      true,
      String(folded),
      "",
    ]);
  });

  it("hands a click anywhere on the header to the gesture, the header as its control", () => {
    // Arrange
    const section = document.createElement("div");
    const header = document.createElement("div");
    const label = document.createElement("span");
    header.appendChild(label);
    const calls: Array<[HTMLElement, HTMLElement]> = [];
    foldOnClick(header, section, (s, control) => calls.push([s, control]));

    // Act
    label.click();

    // Assert
    expect(calls).toEqual([[section, header]]);
  });

  it("asks nothing for a click a control inside the header stops", () => {
    // Arrange
    const section = document.createElement("div");
    const header = document.createElement("div");
    const control = document.createElement("button");
    control.addEventListener("click", (event) => event.stopPropagation());
    header.appendChild(control);
    let calls = 0;
    foldOnClick(header, section, () => (calls += 1));

    // Act
    control.click();

    // Assert
    expect(calls).toBe(0);
  });
});

describe("trackDaemonFold", () => {
  it.each([true, false])("folds the section as the wire says (%s)", (wireFolded) => {
    // Arrange
    const section = document.createElement("div");

    // Act
    trackDaemonFold(section, "sub:1", wireFolded, sidebarContext());

    // Assert
    expect(section.classList.contains("folded")).toBe(wireFolded);
  });
});

describe("sectionFold", () => {
  it.each([
    { folded: false, fold: "collapse" },
    { folded: true, fold: "expand" },
  ])("asks for the other fold of a section folded $folded", async ({ folded, fold }) => {
    // Arrange
    const { asked, sc } = recording();
    const section = document.createElement("div");
    section.classList.toggle("folded", folded);
    const control = document.createElement("span");
    section.appendChild(control);
    document.body.appendChild(section);

    // Act
    sectionFold(sc, "sub:1", { case: "subsection", value: { id: "sub-1" } })(section, control);
    await new Promise((resolve) => setTimeout(resolve, 0));

    // Assert
    const change = asked[0]?.change;
    expect(change?.case === "foldSection" ? [change.value.section.case, change.value.fold.case] : change?.case).toEqual([
      "subsection",
      fold,
    ]);
  });
});

describe("sectionFold's record of a fold it asks", () => {
  afterEach(() => {
    vi.restoreAllMocks();
    document.body.replaceChildren();
  });

  it.each([
    { folded: false, collapse: true, message: "the section's fold gesture asks the daemon to collapse it" },
    { folded: true, collapse: false, message: "the section's fold gesture asks the daemon to expand it" },
  ])("records at info a gesture asking collapse $collapse", ({ folded, collapse, message }) => {
    // Arrange
    const { sc } = recording();
    const section = document.createElement("div");
    section.classList.toggle("folded", folded);
    const control = document.createElement("span");
    section.appendChild(control);
    document.body.appendChild(section);
    const info = vi.spyOn(log, "info");

    // Act
    sectionFold(sc, "repo:r1", { case: "repository", value: { id: "r1", dir: "/r1" } })(section, control);

    // Assert
    expect(info).toHaveBeenCalledWith(message, { operation: "sidebar.fold.asked", context: { section: "repo:r1", collapse } });
  });
});

describe("sectionFold on a section holding the current workspace", () => {
  afterEach(() => {
    vi.restoreAllMocks();
    document.body.replaceChildren();
  });

  /** An expanded or FOLDED section holding the current workspace, and its control. */
  function holding(folded: boolean): { section: HTMLElement; control: HTMLElement } {
    const section = document.createElement("div");
    section.classList.toggle("folded", folded);
    paintHoldsCurrent(section, true);
    const control = document.createElement("span");
    section.appendChild(control);
    document.body.appendChild(section);
    return { section, control };
  }

  it("asks no collapse and stays expanded", async () => {
    // Arrange
    const { asked, sc } = recording();
    const { section, control } = holding(false);

    // Act
    sectionFold(sc, "repo:r1", { case: "repository", value: { id: "r1", dir: "/r1" } })(section, control);
    await new Promise((resolve) => setTimeout(resolve, 0));

    // Assert
    expect([asked.length, section.classList.contains("folded")]).toEqual([0, false]);
  });

  it("logs that the gesture asked nothing", () => {
    // Arrange
    const { sc } = recording();
    const { section, control } = holding(false);
    const debug = vi.spyOn(log, "debug");

    // Act
    sectionFold(sc, "repo:r1", { case: "repository", value: { id: "r1", dir: "/r1" } })(section, control);

    // Assert
    expect(debug).toHaveBeenCalledWith("the section holds the current workspace; its fold gesture asks nothing", {
      operation: "sidebar.fold.holds-current",
      context: { section: "repo:r1" },
    });
  });

  it("flashes the selected row inside the section", () => {
    // Arrange
    const { sc } = recording();
    const { section, control } = holding(false);
    const row = document.createElement("div");
    row.className = "ws current";
    section.appendChild(row);

    // Act
    sectionFold(sc, "repo:r1", { case: "repository", value: { id: "r1", dir: "/r1" } })(section, control);

    // Assert
    expect(row.classList.contains(CURRENT_FLASH_CLASS)).toBe(true);
  });

  it("stops flashing when the animation ends", () => {
    // Arrange
    const { sc } = recording();
    const { section, control } = holding(false);
    const row = document.createElement("div");
    row.className = "ws current";
    section.appendChild(row);
    sectionFold(sc, "repo:r1", { case: "repository", value: { id: "r1", dir: "/r1" } })(section, control);

    // Act
    row.dispatchEvent(new Event("animationend"));

    // Assert
    expect(row.classList.contains(CURRENT_FLASH_CLASS)).toBe(false);
  });

  it("still asks to expand a folded one", async () => {
    // Arrange
    const { asked, sc } = recording();
    const { section, control } = holding(true);

    // Act
    sectionFold(sc, "repo:r1", { case: "repository", value: { id: "r1", dir: "/r1" } })(section, control);
    await new Promise((resolve) => setTimeout(resolve, 0));

    // Assert
    const change = asked[0]?.change;
    expect(change?.case === "foldSection" ? change.value.fold.case : change?.case).toBe("expand");
  });
});

describe("paintHoldsCurrent", () => {
  it.each([true, false])("says on the box whether it holds the current workspace (%s)", (holds) => {
    // Arrange
    const section = document.createElement("div");
    paintHoldsCurrent(section, !holds);

    // Act
    paintHoldsCurrent(section, holds);

    // Assert
    expect(section.hasAttribute(HOLDS_CURRENT_ATTRIBUTE)).toBe(holds);
  });
});

describe("drawRosterHoldsCurrent", () => {
  it.each([
    ["a collapsed fold", { case: "collapsed", value: {} }, false],
    ["a collapsible expanded fold", { case: "expanded", value: { collapse: { case: "collapsible", value: {} } } }, false],
    ["an expanded fold holding the current workspace", { case: "expanded", value: { collapse: { case: "holdsCurrent", value: {} } } }, true],
  ] as const)("reads %s", (_name, fold, holds) => {
    expect(drawRosterHoldsCurrent(fold, "fold")).toBe(holds);
  });

  it("refuses an unset fold as a malformed view", () => {
    expect(() => drawRosterHoldsCurrent({ case: undefined }, "fold")).toThrow(MalformedView);
  });

  it("refuses an expanded fold with no collapse arm as a malformed view", () => {
    expect(() => drawRosterHoldsCurrent({ case: "expanded", value: { collapse: { case: undefined } } }, "fold")).toThrow(
      MalformedView,
    );
  });
});

describe("the sidebar's folds", () => {
  it("make their fold control and ask their fold only through section-fold", () => {
    // A fold control or a fold ask spelled by hand could fold one way here and another there.
    const dir = join(__dirname, "..", "..", "src", "sidebar");
    const offenders = readdirSync(dir)
      .filter((name) => name.endsWith(".ts") && name !== "section-fold.ts")
      .filter((name) => {
        const code = codeOf(readFileSync(join(dir, name), "utf8"));
        return code.includes(`"data-section-fold", ""`) || code.includes(`case: "foldSection"`);
      });
    expect(offenders).toEqual([]);
  });
});

describe("drawRosterSectionFold", () => {
  it.each([
    ["expanded", false],
    ["collapsed", true],
  ] as const)("reads the %s arm as folded=%s", (arm, folded) => {
    expect(drawRosterSectionFold({ case: arm, value: {} }, "fold")).toBe(folded);
  });

  it("refuses an unset fold as a malformed view", () => {
    expect(() => drawRosterSectionFold({ case: undefined }, "fold")).toThrow(MalformedView);
  });
});

describe("installFocusFold", () => {
  afterEach(() => {
    document.body.replaceChildren();
  });

  /** A rail drawn with Recently Merged, Hidden, Merge Queue and Open all expanded, and a focus-left fired by hand. */
  function rail(init: { mergedCollapsed?: boolean } = {}) {
    const { asked, sc } = recording();
    const merged = row({ id: "ws-m", closed: true, status: { case: "merged", value: {} }, when: { case: "merged", value: { atMs: BigInt(NOW) } } });
    const host = document.createElement("div");
    host.appendChild(
      drawWorkspaceRoster(
        roster({
          merged: mergedSection([merged], init.mergedCollapsed ?? false),
          hidden: hiddenSection([row({ id: "ws-h" })]),
          mergeQueue: mergeQueueBand([queueGroup({ id: "repo-1", queue: { head: mergingHead("w1") } })]),
        }),
        sc,
      ),
    );
    document.body.appendChild(host);
    const focus = createFocusLeft(window, createPresence(document, true));
    return { asked, sc, host, focus };
  }

  /** The sections each recorded ask folds, and which way. */
  const folds = (asked: UpdateSidebarViewRequest[]) =>
    asked.map((request) =>
      request.change.case === "foldSection" ? [request.change.value.section.case, request.change.value.fold.case] : request.change.case,
    );

  it("collapses an open Recently Merged and Hidden as the focus leaves, and no other section", async () => {
    // Arrange
    const { asked, sc, host, focus } = rail();
    installFocusFold(host, sc, focus);
    // Act
    focus.left("the page lost focus");
    await new Promise((resolve) => setTimeout(resolve, 0));
    // Assert
    expect(folds(asked)).toEqual([
      ["recentlyMerged", "collapse"],
      ["hidden", "collapse"],
    ]);
  });

  it("paints each collapse at once", () => {
    const { sc, host, focus } = rail();
    installFocusFold(host, sc, focus);
    focus.left("the page lost focus");
    expect([".merged-section", ".hidden-section"].map((sel) => host.querySelector(sel)?.classList.contains("folded"))).toEqual([true, true]);
  });

  it("asks nothing of a section folded already", async () => {
    const { asked, sc, host, focus } = rail({ mergedCollapsed: true });
    installFocusFold(host, sc, focus);
    focus.left("the page lost focus");
    await new Promise((resolve) => setTimeout(resolve, 0));
    expect(folds(asked)).toEqual([["hidden", "collapse"]]);
  });

  it("hears nothing once uninstalled", async () => {
    const { asked, sc, host, focus } = rail();
    installFocusFold(host, sc, focus)();
    focus.left("the page lost focus");
    await new Promise((resolve) => setTimeout(resolve, 0));
    expect(asked).toEqual([]);
  });

  it("records each fold with its section and why the focus left, at debug", async () => {
    const { sc, host, focus } = rail();
    installFocusFold(host, sc, focus);
    const capture = captureLogRecords("debug");
    focus.left("the page lost focus");
    const record = await forwardedRecord(capture, "sidebar.fold.focus-left");
    expect([record.level.case, record.context]).toMatchObject(["debug", { section: "merged", why: "the page lost focus" }]);
  });

  it("refuses and logs a drawn section without its fold control, asking nothing", async () => {
    // Arrange
    const { asked, sc, host, focus } = rail();
    host.querySelector(".merged-section > [data-section-fold]")!.removeAttribute("data-section-fold");
    installFocusFold(host, sc, focus);
    const capture = captureLogRecords("error");
    // Act
    expect(() => focus.left("the page lost focus")).toThrow(/the merged section has no fold control/);
    // Assert
    const record = await forwardedRecord(capture, "sidebar.fold.focus-left");
    expect([record.context, asked]).toMatchObject([{ section: "merged", why: "the page lost focus" }, []]);
  });

  it("folds exactly the Recently Merged and Hidden sections", () => {
    expect(FOCUS_FOLDED_SECTIONS.map((shape) => [shape.className, shape.foldKey, shape.target?.case])).toEqual([
      ["merged-section", "merged", "recentlyMerged"],
      ["hidden-section", "hidden", "hidden"],
    ]);
  });
});
