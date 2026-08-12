// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import {
  historyContinuation,
  loadMoreView,
  paintLoadMore,
  type LoadMoreState,
} from "../src/load-more.js";

/**
 * THE CONTROL IS DRIVEN BY THE CONTINUATION ONEOF AND BY NOTHING ELSE.
 *
 * Availability is the daemon's answer; `loading`/`givenUp` decide only whether
 * the offered control is pressable. Three separate conditions spread across a
 * renderer is how a button ends up pressable while a request is already out,
 * so the decision is taken once and asserted here.
 */

function state(over: Partial<LoadMoreState> = {}): LoadMoreState {
  return { continuation: { case: "more" }, loading: false, givenUp: false, ...over };
}

describe("loadMoreView", () => {
  it("HistoryHasMore with nothing in the way OFFERS the affordance", () => {
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
    const view = loadMoreView(state({ continuation: { case: "start" } }));
    // Assert
    expect(view.mode).toBe("hidden");
  });

  it("no page adopted yet is SILENCE, not an offer", () => {
    // Arrange — a client told nothing about whether history remains must not
    // offer an affordance on that silence; that is the decorative button.
    // Act
    const view = loadMoreView(state({ continuation: null }));
    // Assert
    expect(view.mode).toBe("hidden");
  });

  it("HistoryAtStart retires the affordance even mid-request", () => {
    // Arrange — the strongest fact wins, so no in-flight request can keep
    // chrome on screen above a conversation with no more of itself to give.
    // Act
    const view = loadMoreView(state({ continuation: { case: "start" }, loading: true }));
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
    paintLoadMore(host, loadMoreView(state({ continuation: { case: "start" } })), () => {});
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

describe("historyContinuation", () => {
  it("HistoryHasMore resolves to the arm that offers the affordance", () => {
    // Arrange — the arm is EMPTY on purpose: a fact, not a handle.
    // Act
    const continuation = historyContinuation({ more: {} });
    // Assert
    expect(continuation).toEqual({ case: "more" });
  });

  it("HistoryAtStart resolves to the arm that retires the affordance", () => {
    // Arrange / Act
    const continuation = historyContinuation({ start: {} });
    // Assert
    expect(continuation).toEqual({ case: "start" });
  });

  it("NEITHER arm set is a LOUD error rather than either default", () => {
    // Arrange — defaulting to `more` gives an affordance that can never
    // succeed; defaulting to `start` silently hides the rest of the
    // conversation. So neither is chosen.
    // Act / Assert
    expect(() => historyContinuation({})).toThrow(/NEITHER/);
  });

  it("BOTH arms set is a LOUD error, since they are one oneof", () => {
    // Arrange / Act / Assert
    expect(() => historyContinuation({ more: {}, start: {} })).toThrow(/both/i);
  });
});
