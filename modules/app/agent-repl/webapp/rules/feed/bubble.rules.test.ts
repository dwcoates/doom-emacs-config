// @vitest-environment jsdom
/**
 * THE BUBBLE'S RULES OVER THE STYLESHEET'S TEXT: every display the sheet sets
 * on a mounted bubble's sub-feed panel or overlay is guarded by a `[hidden]`
 * rule, and the flush sub-feed drops the inner box's border. The class is read
 * off a mounted production bubble, the declarations off src/styles.css.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import STYLESHEET from "../../src/styles.css?raw";
import { rulesOf } from "../stylesheet-text.js";
import { mount, panelOf } from "../../test/feed/bubble-mount.js";
import { subagentRow } from "../../test/feed/harness.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

/** Every rule block in the sheet, in source order, as selector + body. */
function rules(): { selector: string; body: string }[] {
  return rulesOf(STYLESHEET).map((rule) => ({ selector: rule.selectors.join(", "), body: rule.declarations }));
}

describe("mountBubble: the fold actually hides the sub-feed", () => {
  // THE CASCADE IS THE CLAIM, and it cannot be asked of jsdom: `[hidden]` is a
  // USER-AGENT rule an author rule setting `display` outranks by source order,
  // while jsdom answers `none` for a hidden element whatever the sheet says.
  // What is asserted HERE is the sheet the browser will read
  // (test/feed/bubble.test.ts holds the rest of this account).

  it("guards every display the sheet sets on the panel with a [hidden] rule", () => {
    // Arrange: the class production actually puts on the sub-feed panel.
    const { bubble } = mount(subagentRow("b1"));
    const classes = [...panelOf(bubble).classList];
    expect(classes.length).toBeGreaterThan(0);
    // Act: the sheet's rules that set `display` on any of those classes, and
    // the ones that set it on the same class WHEN HIDDEN.
    const sets = rules().filter(
      (rule) =>
        classes.some((cls) => rule.selector.includes(`.${cls}`)) &&
        /(^|[;\s])display\s*:/.test(rule.body),
    );
    const guards = sets.filter(
      (rule) => rule.selector.includes("[hidden]") && /display\s*:\s*none/.test(rule.body),
    );
    // Assert: the LAST word on display for a hidden panel is `none`.
    expect(sets.length).toBeGreaterThan(0);
    expect(guards.length).toBeGreaterThan(0);
    expect(sets.indexOf(guards[guards.length - 1])).toBe(sets.length - 1);
  });

  // FLUSH INTERIOR (owner ruling, 2026-09-14): the recursive sub-feed sits
  // DIRECTLY in the tool card, not inside a second bordered `.agent-panel` box.
  // The fill and the corner radius are read from jsdom's resolved cascade (a
  // `background` shorthand and `border-radius` it parses). The BORDER is not
  // measurable here -- the base `.agent-panel { border: 1px solid var(--…) }`
  // shorthand is one jsdom cannot parse into longhands (see the outer-card note
  // above), so its computed border-style stays `none` whether or not the flush
  // override lands -- so the border removal is asserted from the sheet's own
  // source order, scoped to the rules that ACTUALLY match the panel element
  // (`.matches`, so `.fold-fixed > .agent-panel` and the scrollbar
  // pseudo-element rules that merely mention `.agent-panel` are excluded).

  /** The panel-matching rules that set PROP, in the sheet's source order. */
  function panelSetters(bubble: { element: HTMLElement }, prop: string): { selector: string; body: string }[] {
    const panel = panelOf(bubble);
    return rules().filter(
      (rule) =>
        new RegExp(`(^|[;\\s])${prop}\\s*:`).test(rule.body) &&
        rule.selector
          .split(",")
          .some((group) => {
            const bare = group.trim().replace(/::[a-z-]+$/i, "");
            if (bare === "") return false;
            try {
              return panel.matches(bare);
            } catch {
              return false;
            }
          }),
    );
  }

  it("drops the inner box border so the sub-feed is not a card-within-a-card", () => {
    // Arrange
    const { bubble } = mount(subagentRow("b1"));
    // Act: the last word the cascade gives the panel's border.
    const setters = panelSetters(bubble, "border");
    const last = setters[setters.length - 1];
    // Assert: the flush `.bubble-subfeed` rule wins, removing the border.
    expect(setters.length).toBeGreaterThan(0);
    expect(last.selector).toContain(".bubble-subfeed");
    expect(last.body).toMatch(/(^|[;\s])border\s*:\s*none/);
  });
});

describe("mountBubble: an overlay question in place of the sub-feed", () => {
  it("guards the overlay's display with a [hidden] rule", () => {
    const sets = rulesOf(STYLESHEET).filter(
      (rule) => rule.selectors.join(", ").includes(".bubble-overlay") && /(^|[;\s])display\s*:/.test(rule.declarations),
    );
    const last = sets[sets.length - 1];
    expect(sets.length === 0 || (last.selectors.join(", ").includes("[hidden]") && /display\s*:\s*none/.test(last.declarations))).toBe(true);
  });
});
