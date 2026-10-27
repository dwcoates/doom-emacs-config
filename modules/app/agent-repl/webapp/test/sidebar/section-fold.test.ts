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
  HOLDS_CURRENT_ATTRIBUTE,
  drawFoldToggle,
  drawRosterHoldsCurrent,
  drawRosterSectionFold,
  paintHoldsCurrent,
  sectionFold,
  trackDaemonFold,
} from "../../src/sidebar/section-fold.js";
import { log } from "../../src/log.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { codeOf } from "../source-text.js";
import { appContext, sidebarContext } from "./harness.js";

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

describe("drawFoldToggle", () => {
  it.each([
    { folded: true, glyph: "▸" },
    { folded: false, glyph: "▾" },
  ])("draws a section folded $folded as $glyph", ({ folded, glyph }) => {
    // Arrange
    const section = document.createElement("div");
    section.classList.toggle("folded", folded);

    // Act
    const triangle = drawFoldToggle(section, () => {});

    // Assert
    expect([triangle.textContent, triangle.getAttribute("data-folded")]).toEqual([glyph, String(folded)]);
  });

  it("hands a click to the gesture without letting it reach the header", () => {
    // Arrange
    const section = document.createElement("div");
    const header = document.createElement("div");
    const calls: HTMLElement[] = [];
    let headerClicks = 0;
    header.addEventListener("click", () => headerClicks++);
    const triangle = drawFoldToggle(section, (_section, control) => calls.push(control));
    header.appendChild(triangle);

    // Act
    triangle.click();

    // Assert
    expect([calls, headerClicks]).toEqual([[triangle], 0]);
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
  it("draw their triangle and ask their fold only through section-fold", () => {
    // A triangle or a fold ask spelled by hand could fold one way here and another there.
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
