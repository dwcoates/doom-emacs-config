// @vitest-environment jsdom
// The one fold every daemon-held sidebar fold draws. See src/sidebar/section-fold.ts.
import { create } from "@bufbuild/protobuf";
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import {
  UpdateSidebarViewResponseSchema,
  type UpdateSidebarViewRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import { drawFoldToggle, drawRosterSectionFold, sectionFold, trackDaemonFold } from "../../src/sidebar/section-fold.js";
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
