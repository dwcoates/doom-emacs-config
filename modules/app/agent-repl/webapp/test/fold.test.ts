/**
 * The shared fold skeleton — both of its shapes.
 *
 * The click-to-open shape is what every grey card's activity and stream
 * sections wear. The FIXED shape is what a teal card's do (see
 * `ASYNC_TEAL_TOOLS` in render.ts): no toggle, no caret, no open state, and a
 * body that is always in the HTML for the stylesheet to cap and scroll.
 */
import { describe, expect, it } from "vitest";

import { FIXED_FOLD_CLASS, Fold, capLabel } from "../src/fold.js";

/** A fold with the parts every case here shares, overridden per case. */
function fold(over: Partial<Parameters<typeof Fold>[0]> = {}): string {
  return Fold({
    id: "t1",
    foldClass: "agent-activity",
    tickerClass: "agent-ticker",
    ticker: "3 steps",
    body: () => "<i>BODY</i>",
    open: false,
    ...over,
  });
}

describe("capLabel", () => {
  it("leaves a label that fits alone", () => {
    // Arrange / Act / Assert
    expect(capLabel("short", 10)).toBe("short");
  });

  it("marks the truncation rather than silently shortening", () => {
    // Arrange / Act / Assert
    expect(capLabel("abcdefghij", 5)).toBe("abcd…");
  });
});

describe("Fold — the click-to-open shape", () => {
  it("renders the ticker without the body while closed", () => {
    // Arrange / Act
    const html = fold();
    // Assert
    expect(html).toContain("3 steps");
    expect(html).not.toContain("BODY");
  });

  it("carries the toggle the delegated click handler flips", () => {
    // Arrange / Act
    const html = fold();
    // Assert
    expect(html).toContain(`data-panel-toggle="t1"`);
  });

  it("mounts the panel body once open", () => {
    // Arrange / Act
    const html = fold({ open: true });
    // Assert
    expect(html).toContain(`<div class="agent-panel"><i>BODY</i></div>`);
  });

  it("turns the caret over to announce the open state", () => {
    // Arrange / Act
    const html = fold({ open: true });
    // Assert
    expect(html).toContain("▴");
  });

  it("escapes an id carrying markup, so no fold id can break out of the attribute", () => {
    // Arrange / Act
    const html = fold({ id: `a"><script>` });
    // Assert
    expect(html).not.toContain("<script>");
  });
});

describe("Fold — the fixed shape", () => {
  it("renders the body with `open` false, since a fixed panel has no open state", () => {
    // Arrange / Act
    const html = fold({ fixed: true });
    // Assert
    expect(html).toContain(`<div class="agent-panel"><i>BODY</i></div>`);
  });

  it("marks the wrapper so the stylesheet can cap and scroll the panel", () => {
    // Arrange / Act
    const html = fold({ fixed: true });
    // Assert
    expect(html).toContain(`class="agent-activity ${FIXED_FOLD_CLASS}"`);
  });

  it("drops the toggle, so no click of the reader's can collapse it", () => {
    // Arrange / Act
    const html = fold({ fixed: true, open: true });
    // Assert
    expect(html).not.toContain("data-panel-toggle");
  });

  it("drops the caret, having no state for one to announce", () => {
    // Arrange / Act
    const html = fold({ fixed: true });
    // Assert
    expect(html).not.toContain("agent-caret");
    expect(html).not.toContain("▾");
  });

  it("never carries the open class, which the fixed rules do not key off", () => {
    // Arrange / Act — `open: true` must not leak a second state in.
    const html = fold({ fixed: true, open: true });
    // Assert
    expect(html).not.toContain(" open\"");
  });

  it("keeps the ticker as the panel's heading", () => {
    // Arrange / Act — the step count is as useful above the panel as it was
    // in place of a closed one.
    const html = fold({ fixed: true });
    // Assert
    expect(html).toContain(`<div class="agent-ticker">3 steps</div>`);
  });

  it("renders the body exactly once", () => {
    // Arrange — the thunk is called unconditionally now, so a second call
    // would double the whole child feed.
    let calls = 0;
    // Act
    fold({
      fixed: true,
      body: () => {
        calls++;
        return "";
      },
    });
    // Assert
    expect(calls).toBe(1);
  });

  it("leaves the body unrendered on a non-fixed closed fold, as before", () => {
    // Arrange
    let calls = 0;
    // Act
    fold({
      body: () => {
        calls++;
        return "";
      },
    });
    // Assert
    expect(calls).toBe(0);
  });
});
