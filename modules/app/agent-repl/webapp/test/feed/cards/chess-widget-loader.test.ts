// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { ensureChessWidgetStylesheet, loadChessWidget } from "../../../src/feed/cards/chess-widget-loader.js";

describe("ensureChessWidgetStylesheet", () => {
  it("adds the stylesheet once per URL", () => {
    // Arrange.
    document.head.replaceChildren();

    // Act.
    ensureChessWidgetStylesheet(document, "/chess-widget/a/cee-web-widget.css");
    ensureChessWidgetStylesheet(document, "/chess-widget/a/cee-web-widget.css");

    // Assert.
    expect(document.head.querySelectorAll("link[data-chess-widget]")).toHaveLength(1);
  });

  it("adds a rebuilt widget's stylesheet beside the old one", () => {
    // Arrange.
    document.head.replaceChildren();
    ensureChessWidgetStylesheet(document, "/chess-widget/a/cee-web-widget.css");

    // Act.
    ensureChessWidgetStylesheet(document, "/chess-widget/b/cee-web-widget.css");

    // Assert.
    const hrefs = [...document.head.querySelectorAll("link[data-chess-widget]")].map((l) => l.getAttribute("href"));
    expect(hrefs).toEqual(["/chess-widget/a/cee-web-widget.css", "/chess-widget/b/cee-web-widget.css"]);
  });
});

describe("loadChessWidget", () => {
  it("answers a module that exports mountCeeWebWidget", async () => {
    // Arrange.
    const url = `data:text/javascript,${encodeURIComponent("export function mountCeeWebWidget() { return {}; }")}`;

    // Act.
    const loaded = await loadChessWidget(url);

    // Assert.
    expect(typeof loaded.mountCeeWebWidget).toBe("function");
  });

  it("refuses a module that exports no mount", async () => {
    // Arrange.
    const url = `data:text/javascript,${encodeURIComponent("export const other = 1;")}`;

    // Act, Assert.
    await expect(loadChessWidget(url)).rejects.toThrow("exports no mountCeeWebWidget");
  });
});
