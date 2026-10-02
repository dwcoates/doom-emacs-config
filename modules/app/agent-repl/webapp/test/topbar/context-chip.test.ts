// @vitest-environment jsdom
import { createControl } from "../../src/control.js";
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  TokenBreakdownRowSchema,
  TokenBreakdownViewSchema,
  TopbarContextChipSchema,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  drawTokenBreakdownRow,
  drawTokenBreakdownView,
  drawTopbarContextChip,
  formatSharePermille,
  formatTokenCount,
} from "../../src/topbar/context-chip.js";
import { cascadedValue, installStylesheet } from "../stylesheet.js";
import { openPanel, topbarContext } from "./fixtures.js";

const row = (init: Partial<{ label: string; tokens: number; sharePermille: number; emphasized: boolean; depth: number }>) =>
  create(TokenBreakdownRowSchema, {
    label: init.label ?? "uncached input",
    tokens: BigInt(init.tokens ?? 0),
    sharePermille: init.sharePermille,
    emphasized: init.emphasized ?? false,
    depth: init.depth ?? 0,
  });

const chip = (text: string, sections: unknown[] = []) =>
  create(TopbarContextChipSchema, {
    text,
    breakdown: create(TokenBreakdownViewSchema, { sections: sections as never }),
  });

describe("drawTopbarContextChip", () => {
  it("draws the daemon's figure verbatim, never one it formatted itself", () => {
    const { tc } = topbarContext();
    expect(drawTopbarContextChip(chip("142.3k"), tc).textContent).toBe("142.3k");
  });

  it("paints the figure yellow, the context register", () => {
    const { tc } = topbarContext();
    const drawn = drawTopbarContextChip(chip("142.3k"), tc);
    expect(drawn.querySelector(".topbar-context-figure")?.classList.contains("tone-yellow")).toBe(
      true,
    );
  });

  it("lets the yellow WIN the cascade, the class alone having been a lie once", () => {
    // The class assertion above passed for as long as the stylesheet's shared
    // topbar-button rule carried `color: var(--muted)`: same specificity,
    // declared later, so the one colored number in the strip was drawn grey in
    // every running page and only a screenshot of the real topbar saw it.
    // ARRANGE
    const remove = installStylesheet();
    const { host, tc } = topbarContext();
    host.append(drawTopbarContextChip(chip("142.3k"), tc));
    // ACT
    const figure = host.querySelector(".topbar-context-figure");
    // ASSERT
    expect(cascadedValue(figure as Element, "color")).toBe("var(--async)");
    remove();
  });

  it("leaves the quiet controls beside it quiet", () => {
    // The negative of the row above: the shared muted color was not deleted,
    // it was narrowed to the controls it was written for.
    // ARRANGE
    const remove = installStylesheet();
    const button = createControl();
    button.className = "topbar-model-button";
    document.body.append(button);
    // ACT / ASSERT
    expect(cascadedValue(button, "color")).toBe("var(--muted)");
    button.remove();
    remove();
  });

  it("opens the breakdown on a click", () => {
    // ARRANGE
    const { host, tc } = topbarContext();
    host.append(
      drawTopbarContextChip(
        chip("1k", [{ heading: { text: "session" }, rows: [row({ label: "fresh input" })] }]),
        tc,
      ),
    );
    // ACT
    host
      .querySelector(".topbar-context-figure")
      ?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    // ASSERT
    expect(openPanel(host)?.textContent).toContain("fresh input");
  });

  it("refuses a chip carrying no breakdown, which the proto says is always set", () => {
    const { tc } = topbarContext();
    expect(() => drawTopbarContextChip(create(TopbarContextChipSchema, { text: "1k" }), tc)).toThrow(
      MalformedView,
    );
  });
});

describe("drawTokenBreakdownView", () => {
  it("draws the section heading verbatim", () => {
    const view = create(TokenBreakdownViewSchema, {
      sections: [{ heading: { text: "session" }, rows: [] }],
    });
    expect(drawTokenBreakdownView(view).querySelector(".token-breakdown-heading")?.textContent).toBe(
      "session",
    );
  });

  it("refuses a section carrying no heading", () => {
    const view = create(TokenBreakdownViewSchema, { sections: [{ rows: [] }] });
    expect(() => drawTokenBreakdownView(view)).toThrow(MalformedView);
  });

  it("draws the sections in the served order", () => {
    const view = create(TokenBreakdownViewSchema, {
      sections: [
        { heading: { text: "session" }, rows: [] },
        { heading: { text: "per model" }, rows: [] },
      ],
    });
    const headings = Array.from(
      drawTokenBreakdownView(view).querySelectorAll(".token-breakdown-heading"),
    ).map((el) => el.textContent);
    expect(headings).toEqual(["session", "per model"]);
  });
});

describe("drawTokenBreakdownRow", () => {
  const path = "TokenBreakdownView.sections[0].rows[0]";

  it("draws the label verbatim", () => {
    expect(
      drawTokenBreakdownRow(row({ label: "cache read" }), path).querySelector(
        ".token-breakdown-label",
      )?.textContent,
    ).toBe("cache read");
  });

  it("groups the count rather than abbreviating it", () => {
    expect(
      drawTokenBreakdownRow(row({ tokens: 142_300 }), path).querySelector(
        ".token-breakdown-tokens",
      )?.textContent,
    ).toBe(formatTokenCount(142_300));
  });

  it("draws the share when the producer computed one", () => {
    expect(
      drawTokenBreakdownRow(row({ sharePermille: 431 }), path).querySelector(
        ".token-breakdown-share",
      )?.textContent,
    ).toBe("43.1%");
  });

  it("draws a real zero share rather than omitting it", () => {
    expect(
      drawTokenBreakdownRow(row({ sharePermille: 0 }), path).querySelector(
        ".token-breakdown-share",
      )?.textContent,
    ).toBe("0.0%");
  });

  it("omits the share entirely when it is unset, never as a misleading zero", () => {
    expect(
      drawTokenBreakdownRow(row({}), path).querySelector(".token-breakdown-share"),
    ).toBeNull();
  });

  it("marks an emphasized row, which is a layout fact resolved daemon-side", () => {
    expect(drawTokenBreakdownRow(row({ emphasized: true }), path).hasAttribute("data-emphasized")).toBe(
      true,
    );
  });

  it("carries the indent depth", () => {
    expect(drawTokenBreakdownRow(row({ depth: 2 }), path).getAttribute("data-depth")).toBe("2");
  });
});

describe("formatSharePermille", () => {
  it("reads permille at one decimal, the resolution the producer rounded to", () => {
    expect(formatSharePermille(1000)).toBe("100.0%");
  });
});
