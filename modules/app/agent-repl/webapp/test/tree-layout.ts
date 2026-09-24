/**
 * A LAYOUT FOR THE METAPROMPT TREE'S COLUMN BUDGET, IN JSDOM.
 *
 * jsdom performs no layout, so the response bubble's `measureTreeCols` (which
 * reads a character probe's width, the containing block's width, the bubble's
 * `max-width` and the chrome's computed lengths) has nothing to read there.
 * This stages exactly those reads, and it stages them the way a REAL engine
 * answers them — which is the whole point: a DETACHED element has no box and no
 * computed style (zero rects, empty strings), so a measurement taken before the
 * row joins the document fails here exactly as it does in the webview. The
 * unit suite once staged geometry regardless of attachment, and that is how a
 * first paint that always fell back to 105 columns in the webview passed.
 *
 * What it stages, for CONNECTED elements only:
 *   - `getClientRects()` answers one box for an element with no `[hidden]`
 *     ancestor-or-self, and none for one inside a hidden subtree — the way an
 *     engine answers a `display: none` element — which is what the body's
 *     "laid out" test reads;
 *   - a `.mp-tree` element's rect is `CHARPX` per character of its text (the
 *     measure's probe is one);
 *   - an element holding a `.bubble` child reports `CONTAININGPX` as its
 *     `clientWidth` (the bubble's containing block);
 *   - `getComputedStyle` answers the bubble's `max-width` as MAXWIDTH, the
 *     body's horizontal padding as BODYPADDINGPX a side, every other
 *     horizontal padding, border and margin as `0px`, and the
 *     `--scrollbar-gutter-width` token as SCROLLBARPX (in px), or as
 *     SCROLLBARTOKEN verbatim when a test stages an unreadable one.
 * Every other read passes through to jsdom untouched.
 */
import { afterEach, beforeEach } from "vitest";
import { visibleWidth } from "../src/metaprompt-tree.js";
import { SCROLLBAR_GUTTER_TOKEN } from "../src/bubble/body.js";

/** The geometry a staged tree measurement reads. */
export interface TreeLayout {
  /** One column of the tree's monospace font, in px. */
  charPx: number;
  /** The content width of the bubble's containing block, in px. */
  containingPx: number;
  /** The bubble's computed `max-width` (a percentage, or px). */
  maxWidth: string;
  /** The bubble body's padding on each side, in px. */
  bodyPaddingPx: number;
  /** The scrollbar gutter token's width, in px (the stylesheet's 8px). */
  scrollbarPx: number;
  /** The gutter token's raw value, when a test stages one that is not px. */
  scrollbarToken?: string;
}

/** A layout with the stylesheet's real 77% cap and round numbers. */
export const DEFAULT_TEST_LAYOUT: TreeLayout = {
  charPx: 8,
  containingPx: 1000,
  maxWidth: "77%",
  bodyPaddingPx: 10,
  scrollbarPx: 8,
};

/** The horizontal lengths the measure reads off computed styles. */
const LENGTHS = new Set([
  "paddingLeft",
  "paddingRight",
  "borderLeftWidth",
  "borderRightWidth",
  "marginLeft",
  "marginRight",
]);

function rect(width: number): DOMRect {
  return { width, height: 0, top: 0, left: 0, right: width, bottom: 0, x: 0, y: 0, toJSON: () => ({}) };
}

/**
 * Install LAYOUT (merged over `DEFAULT_TEST_LAYOUT`), and answer a teardown
 * that puts jsdom back. The layout object itself is returned live, so a test
 * can move the column (`layout.containingPx = …`) to stage a resize.
 */
export function installTreeLayout(overrides: Partial<TreeLayout> = {}): {
  layout: TreeLayout;
  uninstall: () => void;
} {
  const layout: TreeLayout = { ...DEFAULT_TEST_LAYOUT, ...overrides };

  // Captured to be ASSIGNED back on teardown, never called off the reference.
  // eslint-disable-next-line @typescript-eslint/unbound-method -- see above
  const originalRect = Element.prototype.getBoundingClientRect;
  Element.prototype.getBoundingClientRect = function staged(this: Element): DOMRect {
    if (!this.isConnected) return rect(0);
    if (this.classList.contains("mp-tree")) return rect((this.textContent ?? "").length * layout.charPx);
    return originalRect.call(this);
  };

  // Captured to be ASSIGNED back on teardown, never called off the reference.
  // eslint-disable-next-line @typescript-eslint/unbound-method -- see above
  const originalRects = Element.prototype.getClientRects;
  Element.prototype.getClientRects = function staged(this: Element): DOMRectList {
    const boxes = this.isConnected && this.closest("[hidden]") === null ? [rect(1)] : [];
    return Object.assign(boxes, { item: (i: number) => boxes[i] ?? null });
  };

  const clientWidth = Object.getOwnPropertyDescriptor(Element.prototype, "clientWidth");
  Object.defineProperty(HTMLElement.prototype, "clientWidth", {
    configurable: true,
    get(this: HTMLElement): number {
      if (this.isConnected && this.querySelector(":scope > .bubble") !== null) return layout.containingPx;
      return (clientWidth?.get?.call(this) as number | undefined) ?? 0;
    },
  });

  // Captured to be ASSIGNED back on teardown; the stub calls it bound.
  // eslint-disable-next-line @typescript-eslint/unbound-method -- see above
  const originalStyle = window.getComputedStyle;
  window.getComputedStyle = (el: Element, pseudo?: string | null): CSSStyleDeclaration => {
    const real = originalStyle.call(window, el, pseudo);
    return new Proxy(real, {
      get(target, property): unknown {
        if (property === "maxWidth" && el.classList.contains("bubble")) {
          return el.isConnected ? layout.maxWidth : "";
        }
        if (property === "getPropertyValue") {
          return (name: string): string => {
            if (name !== SCROLLBAR_GUTTER_TOKEN) return real.getPropertyValue(name);
            if (!el.isConnected) return "";
            return layout.scrollbarToken ?? `${String(layout.scrollbarPx)}px`;
          };
        }
        if (typeof property === "string" && LENGTHS.has(property)) {
          if (!el.isConnected) return "";
          const padded = el.classList.contains("bubble-body") && property.startsWith("padding");
          return padded ? `${String(layout.bodyPaddingPx)}px` : "0px";
        }
        const value: unknown = Reflect.get(target, property);
        return typeof value === "function" ? (value as (...args: unknown[]) => unknown).bind(target) : value;
      },
    });
  };

  return {
    layout,
    uninstall: () => {
      Element.prototype.getBoundingClientRect = originalRect;
      Element.prototype.getClientRects = originalRects;
      delete (HTMLElement.prototype as { clientWidth?: number }).clientWidth;
      window.getComputedStyle = originalStyle;
    },
  };
}

/**
 * The column budget a staged LAYOUT yields, computed the way the measure is
 * specified to: the cap less the body's padding and the expanded scrollbar's
 * gutter, in whole columns.
 */
export function stagedCols(layout: TreeLayout): number {
  const pct = /^([\d.]+)%$/.exec(layout.maxWidth);
  const capPx =
    pct === null ? Number.parseFloat(layout.maxWidth) : (Number.parseFloat(pct[1]) / 100) * layout.containingPx;
  return Math.floor((capPx - 2 * layout.bodyPaddingPx - layout.scrollbarPx) / layout.charPx);
}

/**
 * Install a fresh default layout before every test of the calling `describe`
 * and remove it after, answering a handle whose `layout` is the live one the
 * current test may move.
 */
export function useTreeLayout(): { readonly layout: TreeLayout } {
  let installed: ReturnType<typeof installTreeLayout> | null = null;
  beforeEach(() => {
    installed = installTreeLayout();
  });
  afterEach(() => {
    installed?.uninstall();
    installed = null;
  });
  return {
    get layout(): TreeLayout {
      if (installed === null) throw new Error("useTreeLayout: no layout is installed outside a test");
      return installed.layout;
    },
  };
}

/**
 * A metaprompt tree with branches far wider than the default layout's
 * 92-column budget, so a bubble at its cap must wrap them.
 */
export const WIDE_TREE = [
  "1 🌳 A tree drawn in a bubble of any kind, wrapped at that bubble's own cap",
  "├── 1.1 This branch is deliberately much longer than the default layout's ninety-three column budget, so it must wrap",
  "└── 1.2 The last branch, also long enough to run well past the budget and wrap onto a continuation line of its own",
].join("\n");

/** A tree whose every branch fits comfortably under the default layout's budget. */
export const FITTING_TREE = [
  "1 🌳 A tree whose longest branch is comfortably under the cap.",
  "├── 1.1 A branch of about seventy rendered columns, well inside the cap here.",
  "└── 1.2 Another branch, also short enough to stand on one line.",
].join("\n");

/** Every drawn tree line's width in columns, under EL. */
export function treeLineWidths(el: Element): number[] {
  return [...el.querySelectorAll(".mp-tree .mp-line:not(.mp-blank)")].map((line) =>
    visibleWidth(line.textContent ?? ""),
  );
}
