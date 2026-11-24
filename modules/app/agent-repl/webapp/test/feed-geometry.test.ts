// @vitest-environment jsdom
// Where the root feed's scroll box stands against its rows. See src/feed-geometry.ts.
import { beforeEach, describe, expect, it } from "vitest";

import { boxRect, settableScrollTop } from "./dom-rect.js";
import { blankView, feedGeometry, GEOMETRY_TAIL_ROWS, type FeedGeometry } from "../src/feed-geometry.js";
import { drawRootFeedShell } from "./root-feed-shell.js";

/** Give EL a laid-out box at TOP (viewport coordinates) of HEIGHT. */
function place(el: Element, top: number, height: number): void {
  el.getBoundingClientRect = () => boxRect({ top, width: 100, height });
}

/** The app's feed shell: a scroll box at viewport top 100, CLIENT tall, scrolled to SCROLL_TOP of SCROLL_HEIGHT. */
function shell(scrollTop: number, scrollHeight: number, client: number): { box: HTMLElement; feed: HTMLElement; rows: HTMLElement } {
  const { box, feed, rows } = drawRootFeedShell(`<section id="hold-tray"></section>`);
  place(box, 100, client);
  settableScrollTop(box, scrollTop);
  Object.defineProperty(box, "scrollHeight", { value: scrollHeight });
  Object.defineProperty(box, "clientHeight", { value: client });
  place(feed, 100 - scrollTop, scrollHeight);
  place(document.getElementById("hold-tray")!, 100 - scrollTop + scrollHeight, 0);
  return { box, feed, rows };
}

/** A root row drawn in the row list ROWS at content offset TOP, HEIGHT tall, with the box scrolled to SCROLL_TOP. */
function row(rows: HTMLElement, scrollTop: number, top: number, height: number): HTMLElement {
  const el = document.createElement("article");
  el.className = "feed-item";
  el.setAttribute("data-feed-row", `r${top}`);
  rows.append(el);
  place(el, 100 - scrollTop + top, height);
  return el;
}

function geometry(): FeedGeometry {
  const g = feedGeometry(document);
  expect(g).not.toBeNull();
  return g!;
}

beforeEach(() => {
  document.body.replaceChildren();
});

describe("feedGeometry", () => {
  it("is null on a page that draws no root feed", () => {
    // Arrange
    document.body.innerHTML = "<div></div>";

    // Act
    const g = feedGeometry(document);

    // Assert
    expect(g).toBeNull();
  });

  it("states the scroll box's position and size", () => {
    // Arrange
    shell(500, 2000, 400);

    // Act
    const g = geometry();

    // Assert
    expect(g).toMatchObject({ scroll_top: 500, scroll_height: 2000, client_height: 400 });
  });

  it("counts the root rows that meet the viewport", () => {
    // Arrange
    const { rows } = shell(500, 2000, 400);
    row(rows, 500, 0, 300);
    row(rows, 500, 450, 100);
    row(rows, 500, 850, 100);

    // Act
    const g = geometry();

    // Assert
    expect(g).toMatchObject({ root_rows: 3, rows_in_view: 2 });
  });

  it("counts the rows drawn in the feed's row list, inside its body mount", () => {
    // Arrange
    const { rows } = shell(0, 2000, 400);
    row(rows, 0, 0, 100);
    row(rows, 0, 100, 100);

    // Act
    const g = geometry();

    // Assert
    expect(g).toMatchObject({ root_rows: 2, rows_in_view: 2 });
  });

  it("does not count a row drawn outside the feed's row list", () => {
    // Arrange
    const { feed } = shell(0, 2000, 400);
    row(feed, 0, 0, 100);

    // Act
    const g = geometry();

    // Assert
    expect(g).toMatchObject({ root_rows: 0, rows_in_view: 0 });
  });

  it("counts no rows while the feed's row list is not drawn", () => {
    // Arrange
    const { feed } = shell(0, 2000, 400);
    feed.replaceChildren();

    // Act
    const g = geometry();

    // Assert
    expect(g).toMatchObject({ root_rows: 0, rows_in_view: 0 });
  });

  it("does not count a row that only touches the viewport's edge", () => {
    // Arrange
    const { rows } = shell(500, 2000, 400);
    row(rows, 500, 400, 100);
    row(rows, 500, 900, 100);

    // Act
    const g = geometry();

    // Assert
    expect(g.rows_in_view).toBe(0);
  });

  it("describes the last rows in content coordinates", () => {
    // Arrange
    const { rows } = shell(500, 2000, 400);
    for (let i = 0; i < GEOMETRY_TAIL_ROWS + 1; i++) row(rows, 500, i * 100, 50);

    // Act
    const g = geometry();

    // Assert
    expect(g.tail.map((b) => [b.what, b.top, b.height])).toEqual([
      ["article.feed-item", 100, 50],
      ["article.feed-item", 200, 50],
      ["article.feed-item", 300, 50],
    ]);
  });

  it("describes every child of the scroll box", () => {
    // Arrange
    shell(500, 2000, 400);

    // Act
    const g = geometry();

    // Assert
    expect(g.children.map((b) => [b.what, b.top, b.height])).toEqual([
      ["main#feed", 0, 2000],
      ["section#hold-tray", 2000, 0],
    ]);
  });
});

describe("blankView", () => {
  it("is a feed holding rows with none in view", () => {
    // Arrange
    const { rows } = shell(1500, 2000, 400);
    row(rows, 1500, 0, 300);

    // Act
    const blank = blankView(geometry());

    // Assert
    expect(blank).toBe(true);
  });

  it("is no blank view while a row is in view", () => {
    // Arrange
    const { rows } = shell(0, 2000, 400);
    row(rows, 0, 0, 300);

    // Act
    const blank = blankView(geometry());

    // Assert
    expect(blank).toBe(false);
  });

  it("is no blank view for an empty feed", () => {
    // Arrange
    shell(0, 400, 400);

    // Act
    const blank = blankView(geometry());

    // Assert
    expect(blank).toBe(false);
  });

  it("is no blank view for a box with no height", () => {
    // Arrange
    const { rows } = shell(0, 0, 0);
    row(rows, 0, 0, 300);

    // Act
    const blank = blankView(geometry());

    // Assert
    expect(blank).toBe(false);
  });
});
