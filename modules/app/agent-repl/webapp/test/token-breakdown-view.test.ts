/**
 * token-breakdown-view — the breakdown menu drawn from the RESOLVED
 * `TokenBreakdownView`.
 *
 * The subject is that this end performs NO arithmetic on a row: not a sum, not
 * a total, not a percentage, not a re-rounding. One edge per test.
 */
import { describe, expect, it } from "vitest";
import {
  NO_SHARE_PERMILLE,
  shareLabel,
  tokenBreakdownRowHtml,
  tokenBreakdownSectionHtml,
  tokenBreakdownViewHtml,
} from "../src/token-breakdown-view.js";
import type { TokenBreakdownRow, TokenBreakdownView } from "../src/frontend-proto.js";

function row(over: Partial<TokenBreakdownRow> = {}): TokenBreakdownRow {
  return { label: "cache read", tokens: 12000, sharePermille: 812, emphasized: false, depth: 0, ...over };
}

function view(over: Partial<TokenBreakdownView> = {}): TokenBreakdownView {
  return {
    workspace: "/ws",
    fence: "f1",
    sections: [{ label: "this turn", rows: [row(), row({ label: "fresh input", tokens: 2800, sharePermille: 188 })] }],
    ...over,
  };
}

describe("the share", () => {
  it("prints the daemon's already-rounded permille as a percentage", () => {
    // Arrange / Act — dividing for display cannot change WHICH number is shown.
    // Assert
    expect(shareLabel(812)).toBe("81.2%");
  });

  it("omits the percentage entirely when no share applies", () => {
    // Arrange / Act — -1 is "no share applies", not a small number.
    // Assert
    expect(shareLabel(NO_SHARE_PERMILLE)).toBe("");
  });

  it("prints a real zero percent rather than omitting it", () => {
    // Arrange / Act — the distinction is why the field is signed.
    // Assert
    expect(shareLabel(0)).toBe("0.0%");
  });

  it("renders no share cell for a row that carries none", () => {
    // Arrange / Act
    const html = tokenBreakdownRowHtml(row({ sharePermille: NO_SHARE_PERMILLE }));
    // Assert
    expect(html).not.toContain("tb-share");
  });
});

describe("a row", () => {
  it("prints the daemon's count verbatim", () => {
    // Arrange / Act — no thousands grouping, no rescaling, no rounding.
    const html = tokenBreakdownRowHtml(row({ tokens: 12000 }));
    // Assert
    expect(html).toContain(">12000<");
  });

  it("prints the label verbatim", () => {
    // Arrange / Act
    const html = tokenBreakdownRowHtml(row({ label: "uncached input" }));
    // Assert
    expect(html).toContain("uncached input");
  });

  it("marks an emphasized row as a headline", () => {
    // Arrange / Act — a layout fact the daemon resolved.
    const html = tokenBreakdownRowHtml(row({ emphasized: true }));
    // Assert
    expect(html).toContain("tb-emphasized");
  });

  it("leaves a detail row unemphasized", () => {
    // Arrange / Act
    const html = tokenBreakdownRowHtml(row({ emphasized: false }));
    // Assert
    expect(html).not.toContain("tb-emphasized");
  });

  it("carries the daemon's indent depth rather than inferring one", () => {
    // Arrange / Act
    const html = tokenBreakdownRowHtml(row({ depth: 2 }));
    // Assert
    expect(html).toContain('data-tb-depth="2"');
  });

  it("escapes the label", () => {
    // Arrange / Act
    const html = tokenBreakdownRowHtml(row({ label: "<img src=x>" }));
    // Assert
    expect(html).not.toContain("<img");
  });
});

describe("a section", () => {
  it("prints its heading verbatim", () => {
    // Arrange / Act
    const html = tokenBreakdownSectionHtml({ label: "session", rows: [] });
    // Assert
    expect(html).toContain("session");
  });

  it("keeps the daemon's row order", () => {
    // Arrange / Act
    const html = tokenBreakdownSectionHtml(view().sections[0]);
    // Assert
    expect(html.indexOf("cache read")).toBeLessThan(html.indexOf("fresh input"));
  });

  it("adds no total row of its own", () => {
    // Arrange / Act — a renderer-computed total is a second answer to a
    // question that already has one.
    const html = tokenBreakdownSectionHtml(view().sections[0]);
    // Assert
    expect(html.match(/tb-row/g)).toHaveLength(2);
  });
});

describe("the menu", () => {
  it("renders NOTHING for a workspace with no published breakdown", () => {
    // Arrange / Act — never a zeroed table.
    // Assert
    expect(tokenBreakdownViewHtml(null)).toBe("");
  });

  it("renders an empty menu for a published view with no sections", () => {
    // Arrange / Act — "nothing to break down yet" is a different fact from the
    // daemon not having said anything.
    const html = tokenBreakdownViewHtml(view({ sections: [] }));
    // Assert
    expect(html).toBe('<div class="tb-menu"></div>');
  });

  it("keeps the daemon's section order", () => {
    // Arrange / Act
    const html = tokenBreakdownViewHtml(
      view({
        sections: [
          { label: "this turn", rows: [] },
          { label: "session", rows: [] },
        ],
      }),
    );
    // Assert
    expect(html.indexOf("this turn")).toBeLessThan(html.indexOf("session"));
  });
});
