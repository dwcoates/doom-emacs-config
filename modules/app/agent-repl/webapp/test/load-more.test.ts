// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { loadMoreView, paintLoadMore, type LoadMoreState } from "../src/load-more.js";

/**
 * THE CONTROL IS A PURE FUNCTION OF FOUR FACTS.
 *
 * Three separate conditions spread across a renderer is how a button ends up
 * pressable while a request is already out, so the decision is taken once and
 * asserted here.
 */

function state(over: Partial<LoadMoreState> = {}): LoadMoreState {
  return { cursor: "c1", reachedStart: false, loading: false, givenUp: false, ...over };
}

describe("loadMoreView", () => {
  it("a cursor and nothing in the way is READY", () => {
    // Arrange / Act
    const view = loadMoreView(state());
    // Assert
    expect(view.mode).toBe("ready");
    expect(view.enabled).toBe(true);
  });

  it("the conversation's beginning RETIRES the control entirely", () => {
    // Arrange — a FACT the daemon established by reading to the floor, never
    // an inference from an empty page.
    // Act
    const view = loadMoreView(state({ reachedStart: true, cursor: null }));
    // Assert
    expect(view.mode).toBe("hidden");
  });

  it("a retired history wins even if a cursor somehow survived it", () => {
    // Arrange — the strongest fact must win, or a stale cursor keeps a button
    // alive that can only ever return nothing.
    // Act
    const view = loadMoreView(state({ reachedStart: true, cursor: "c1" }));
    // Assert
    expect(view.mode).toBe("hidden");
  });

  it("a request in flight leaves the control shown but not pressable", () => {
    // Arrange — a second click would only mint a request the pager drops.
    // Act
    const view = loadMoreView(state({ loading: true }));
    // Assert
    expect(view.mode).toBe("loading");
    expect(view.enabled).toBe(false);
  });

  it("a spent failure ceiling SAYS SO and offers the retry", () => {
    // Arrange — a load-more that silently stops working is worse than one that
    // says it did.
    // Act
    const view = loadMoreView(state({ givenUp: true }));
    // Assert
    expect(view.mode).toBe("stopped");
    expect(view.enabled).toBe(true);
    expect(view.label).toMatch(/retry/i);
  });
});

describe("paintLoadMore", () => {
  it("a hidden control takes no layout at all", () => {
    // Arrange — an empty but present bar would leave a gap above the first
    // bubble for the rest of the session.
    const host = document.createElement("div");
    // Act
    paintLoadMore(host, loadMoreView(state({ reachedStart: true })), () => {});
    // Assert
    expect(host.hidden).toBe(true);
    expect(host.children).toHaveLength(0);
  });

  it("a ready control dispatches exactly one request per click", () => {
    // Arrange
    const host = document.createElement("div");
    let clicks = 0;
    paintLoadMore(host, loadMoreView(state()), () => {
      clicks += 1;
    });
    // Act
    host.querySelector("button")?.click();
    // Assert
    expect(clicks).toBe(1);
  });

  it("a loading control cannot dispatch anything", () => {
    // Arrange
    const host = document.createElement("div");
    let clicks = 0;
    paintLoadMore(host, loadMoreView(state({ loading: true })), () => {
      clicks += 1;
    });
    // Act
    host.querySelector("button")?.click();
    // Assert
    expect(clicks).toBe(0);
    expect(host.querySelector("button")?.disabled).toBe(true);
  });

  it("the painted button names WHICH state produced it", () => {
    // Arrange — so a reader does not have to infer the state from the copy.
    const host = document.createElement("div");
    // Act
    paintLoadMore(host, loadMoreView(state({ givenUp: true })), () => {});
    // Assert
    expect(host.querySelector("button")?.dataset.mode).toBe("stopped");
  });

  it("a repaint replaces the control rather than stacking a second one", () => {
    // Arrange
    const host = document.createElement("div");
    paintLoadMore(host, loadMoreView(state()), () => {});
    // Act
    paintLoadMore(host, loadMoreView(state({ loading: true })), () => {});
    // Assert
    expect(host.querySelectorAll("button")).toHaveLength(1);
  });
});
