// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import { readFileSync } from "node:fs";
import { join, relative } from "node:path";
import {
  SVG_NS,
  ancestorMatching,
  computedPx,
  nodeLabel,
  pageIntersectionObserver,
  pageResizeObserver,
  placeChildren,
  placeOptional,
  repaintMark,
  scrollbarWidthPx,
} from "../src/dom.js";
import { codeOf } from "./source-text.js";
import { filesUnder } from "./files-under.js";

describe("scrollbarWidthPx", () => {
  it("answers a classic bar's width", () => {
    expect(scrollbarWidthPx({ offsetWidth: 200, clientWidth: 185 }, 0)).toBe(15);
  });

  it("answers 0 for an overlay bar, which takes no layout width", () => {
    expect(scrollbarWidthPx({ offsetWidth: 200, clientWidth: 200 }, 0)).toBe(0);
  });

  it("takes the side borders off, which are not the bar", () => {
    expect(scrollbarWidthPx({ offsetWidth: 202, clientWidth: 185 }, 2)).toBe(15);
  });

  it("is the one formula: every scrollbar measurement calls it, and none rolls its own", () => {
    // Arrange — every source under src/, comments stripped.
    const src = join(process.cwd(), "src");
    const code = new Map(
      filesUnder(src).map((path) => [
        relative(src, path),
        codeOf(readFileSync(path, "utf8")),
      ]),
    );
    // Act
    const callers = [...code].filter(([, text]) => /\bscrollbarWidthPx\(/.test(text)).map(([p]) => p).sort();
    const rolled = [...code].filter(([, text]) => /offsetWidth\s*-[^;]*clientWidth/.test(text)).map(([p]) => p);
    // Assert
    expect([callers, rolled]).toEqual([["bubble/body.ts", "dom.ts"], ["dom.ts"]]);
  });
});

describe("computedPx", () => {
  it.each([
    ["a whole px length", "12px", 12],
    ["a fractional px length", "5.6px", 5.6],
    ["a negative px length", "-3.5px", -3.5],
    ["zero in px", "0px", 0],
  ])("answers %s", (_label, value, want) => {
    expect(computedPx(value)).toBe(want);
  });

  it.each([
    ["a keyword", "medium"],
    ["a unitless zero", "0"],
    ["a percentage", "50%"],
    ["an empty value", ""],
    ["a calc", "calc(10px + 2%)"],
    ["a malformed number", "1.2.3px"],
  ])("answers null for %s", (_label, value) => {
    expect(computedPx(value)).toBeNull();
  });

  it("is the one parse of a computed px length in the webapp", () => {
    // Arrange
    const src = join(process.cwd(), "src");
    const code = new Map(filesUnder(src).map((path) => [relative(src, path), readFileSync(path, "utf8")]));
    // Act
    const callers = [...code].filter(([, text]) => /\bcomputedPx\(/.test(text)).map(([p]) => p).sort();
    const rolled = [...code].filter(([, text]) => /px\$\//.test(text)).map(([p]) => p);
    // Assert
    expect([callers, rolled]).toEqual([["bubble/body.ts", "dom.ts", "feed/collapsed-width.ts", "feed/ellipsis-trim.ts", "sidebar/band-fit.ts"], ["dom.ts"]]);
  });
});

/** Fake ancestor-chain node: the shape ancestorMatching walks. */
interface FakeNode {
  name: string;
  parentElement: FakeNode | null;
  match: boolean;
}

function node(name: string, over: Partial<FakeNode> = {}): FakeNode {
  return { name, parentElement: null, match: false, ...over };
}

const matches = (n: FakeNode): boolean => n.match;

describe("ancestorMatching", () => {
  it("returns the start node itself when it matches", () => {
    // Arrange
    const stop = node("stop");
    const start = node("start", { parentElement: stop, match: true });
    // Act + Assert
    expect(ancestorMatching(start, stop, matches)?.name).toBe("start");
  });

  it("climbs to the nearest matching ancestor", () => {
    // Arrange
    const stop = node("stop");
    const hit = node("hit", { parentElement: stop, match: true });
    const start = node("start", { parentElement: hit });
    // Act + Assert
    expect(ancestorMatching(start, stop, matches)?.name).toBe("hit");
  });

  it("returns the innermost of two matching ancestors", () => {
    // Arrange
    const stop = node("stop");
    const outer = node("outer", { parentElement: stop, match: true });
    const inner = node("inner", { parentElement: outer, match: true });
    const start = node("start", { parentElement: inner });
    // Act + Assert
    expect(ancestorMatching(start, stop, matches)?.name).toBe("inner");
  });

  it("returns null when nothing below the stop node matches", () => {
    // Arrange
    const stop = node("stop", { match: true });
    const start = node("start", { parentElement: stop });
    // Act + Assert
    expect(ancestorMatching(start, stop, matches)).toBeNull();
  });

  it("never returns the stop node, even when it matches", () => {
    // Arrange
    const stop = node("stop", { match: true });
    // Act + Assert
    expect(ancestorMatching(stop, stop, matches)).toBeNull();
  });

  it("returns null for a walk with no start node", () => {
    // Arrange
    const stop = node("stop", { match: true });
    // Act + Assert
    expect(ancestorMatching(null, stop, matches)).toBeNull();
  });

  it("stops the walk at the stop node rather than running off the chain", () => {
    // Arrange — a matching node ABOVE the stop node is out of bounds.
    const above = node("above", { match: true });
    const stop = node("stop", { parentElement: above });
    const start = node("start", { parentElement: stop });
    // Act + Assert
    expect(ancestorMatching(start, stop, matches)).toBeNull();
  });
});

describe("placeChildren", () => {
  /** A parent holding elements named by ID, in order. */
  function parentOf(...ids: string[]): { parent: HTMLElement; el: Record<string, HTMLElement> } {
    const parent = document.createElement("div");
    const el: Record<string, HTMLElement> = {};
    for (const id of ids) {
      el[id] = document.createElement("article");
      el[id].id = id;
      parent.append(el[id]);
    }
    return { parent, el };
  }

  /** The ids of the nodes a mutation batch REMOVED from PARENT. */
  function removedIds(observer: MutationObserver): string[] {
    return observer
      .takeRecords()
      .flatMap((record) => [...record.removedNodes])
      .map((node) => (node as HTMLElement).id);
  }

  /** Watch PARENT's child list, synchronously readable through takeRecords. */
  function watch(parent: HTMLElement): MutationObserver {
    const observer = new MutationObserver(() => undefined);
    observer.observe(parent, { childList: true });
    return observer;
  }

  it("removes nothing when the order is unchanged", () => {
    // Arrange -- the redraw a row repaint inside its chrome triggers.
    const { parent, el } = parentOf("a", "b", "c");
    const observer = watch(parent);
    // Act
    placeChildren(parent, [el.a, el.b, el.c]);
    // Assert -- no element left the document, so no scroll box inside reset.
    expect(removedIds(observer)).toEqual([]);
  });

  it("inserts an appended element without removing any other", () => {
    // Arrange -- the redraw a live append triggers.
    const { parent, el } = parentOf("a", "b");
    const fresh = document.createElement("article");
    fresh.id = "c";
    const observer = watch(parent);
    // Act
    placeChildren(parent, [el.a, el.b, fresh]);
    // Assert
    expect(removedIds(observer)).toEqual([]);
  });

  it("lands the children in the desired order", () => {
    // Arrange
    const { parent, el } = parentOf("a", "b", "c");
    // Act
    placeChildren(parent, [el.c, el.a, el.b]);
    // Assert
    expect([...parent.children].map((child) => child.id)).toEqual(["c", "a", "b"]);
  });

  it("drops a child the desired list no longer holds", () => {
    // Arrange
    const { parent, el } = parentOf("a", "b", "c");
    // Act
    placeChildren(parent, [el.a, el.c]);
    // Assert
    expect([...parent.children].map((child) => child.id)).toEqual(["a", "c"]);
  });

  it("drops a stray non-element node", () => {
    // Arrange
    const { parent, el } = parentOf("a");
    parent.append(document.createTextNode("stray"));
    // Act
    placeChildren(parent, [el.a]);
    // Assert
    expect(parent.childNodes.length).toBe(1);
  });

  it("answers how many elements it inserted or moved", () => {
    // Arrange -- one new element in the middle.
    const { parent, el } = parentOf("a", "b");
    const fresh = document.createElement("article");
    // Act + Assert
    expect(placeChildren(parent, [el.a, fresh, el.b])).toBe(1);
  });
});

describe("repaintMark", () => {
  it("paints the class and the text it is handed", () => {
    // Arrange
    const el = document.createElement("span");
    // Act
    repaintMark(el, "glyph is-live", "●");
    // Assert
    expect([el.className, el.textContent]).toEqual(["glyph is-live", "●"]);
  });

  it("writes neither when the element already holds both", () => {
    // Arrange — a recording element whose class and text writes are counted.
    const el = document.createElement("span");
    el.className = "glyph is-live";
    el.textContent = "●";
    const observer = new MutationObserver(() => undefined);
    observer.observe(el, { attributes: true, childList: true, characterData: true, subtree: true });
    // Act
    repaintMark(el, "glyph is-live", "●");
    // Assert
    expect(observer.takeRecords()).toHaveLength(0);
    observer.disconnect();
  });

  it("is how every in-place glyph repaint is written", () => {
    // Arrange — the modules that repaint a breathing state glyph in place.
    const sites = ["feed/merge/tab-strip.ts", "feed/merge/tests-tab.ts"].map((file) =>
      readFileSync(join(__dirname, "../src", file), "utf8"),
    );
    // Act
    const handRolled = sites.filter((source) => /\.className !== /.test(source));
    const shared = sites.filter((source) => source.includes("repaintMark("));
    // Assert
    expect([handRolled.length, shared.length]).toEqual([0, sites.length]);
  });
});

describe("placeOptional", () => {
  /** A host holding a name, and a value element of TEXT drawn as "value". */
  const value = (text: string): HTMLElement => {
    const el = document.createElement("span");
    el.className = "value";
    el.textContent = text;
    return el;
  };
  const host = (): HTMLElement => {
    const el = document.createElement("div");
    el.append(Object.assign(document.createElement("span"), { className: "name" }));
    return el;
  };

  it("puts a first-known value in place through INSERT", () => {
    // Arrange
    const el = host();
    // Act
    placeOptional(el, ":scope > .value", value("3"), (next) => el.prepend(next));
    // Assert
    expect([...el.children].map((c) => c.className)).toEqual(["value", "name"]);
  });

  it("drops the value once it is unknown", () => {
    // Arrange
    const el = host();
    el.append(value("3"));
    // Act
    placeOptional(el, ":scope > .value", null, () => undefined);
    // Assert
    expect(el.querySelector(".value")).toBeNull();
  });

  it("replaces the value whose text changed", () => {
    // Arrange
    const el = host();
    el.append(value("3"));
    // Act
    placeOptional(el, ":scope > .value", value("4"), () => undefined);
    // Assert
    expect(el.querySelector(".value")?.textContent).toBe("4");
  });

  it("keeps the element of a value whose text did not change", () => {
    // Arrange
    const el = host();
    const drawn = value("3");
    el.append(drawn);
    // Act
    placeOptional(el, ":scope > .value", value("3"), () => undefined);
    // Assert
    expect(el.querySelector(".value")).toBe(drawn);
  });

  it("is how the merge tests tab places its optional elements", () => {
    // Arrange — the module whose suite rows draw counts and a reused source.
    const source = readFileSync(join(__dirname, "../src/feed/merge/tests-tab.ts"), "utf8");
    // Act
    const handRolled = /drawn\?\.remove\(\)/.test(source);
    const shared = source.match(/placeOptional\(/g)?.length ?? 0;
    // Assert
    expect([handRolled, shared]).toEqual([false, 2]);
  });
});

describe("SVG_NS", () => {
  it("is the namespace an svg element is created in", () => {
    // Arrange / Act
    const svg = document.createElementNS(SVG_NS, "svg");
    // Assert
    expect(svg.namespaceURI).toBe("http://www.w3.org/2000/svg");
  });

  it("is the one spelling: no source but dom.ts writes the namespace out", () => {
    // Arrange — every source under src/, comments stripped.
    const src = join(process.cwd(), "src");
    // Act
    const spelled = filesUnder(src)
      .filter((path) => codeOf(readFileSync(path, "utf8")).includes("www.w3.org/2000/svg"))
      .map((path) => relative(src, path));
    // Assert
    expect(spelled).toEqual(["dom.ts"]);
  });
});

describe("pageResizeObserver", () => {
  afterEach(() => {
    vi.unstubAllGlobals();
  });

  it("answers the observer of the page the element lives in", () => {
    // Arrange
    const el = document.createElement("div");
    // Act / Assert
    expect(pageResizeObserver(el)).toBe(window.ResizeObserver);
  });

  it("answers null for an element of a page with no observer", () => {
    // Arrange
    vi.stubGlobal("ResizeObserver", undefined);
    const el = document.createElement("div");
    // Act / Assert
    expect(pageResizeObserver(el)).toBeNull();
  });

  it("answers null for an element of a document with no window", () => {
    // Arrange
    const el = document.implementation.createHTMLDocument("detached").createElement("div");
    // Act / Assert
    expect(pageResizeObserver(el)).toBeNull();
  });

  it("is the one availability check every measurer asks", () => {
    // Arrange: every source file under src.
    const src = join(__dirname, "../src");
    const files = filesUnder(src);
    // Act: a hand-rolled check reads `typeof <x>.ResizeObserver`.
    const rolled = files
      .filter((f) => /typeof [\w.?]+\.ResizeObserver/.test(readFileSync(f, "utf8")))
      .map((f) => relative(src, f));
    // Assert: only the helper's own module.
    expect(rolled).toEqual(["dom.ts"]);
  });
});

describe("pageIntersectionObserver", () => {
  afterEach(() => {
    vi.unstubAllGlobals();
  });

  it("answers the page's observer", () => {
    // Act / Assert
    expect(pageIntersectionObserver()).toBe(globalThis.IntersectionObserver);
  });

  it("answers null for a page with no observer", () => {
    // Arrange
    vi.stubGlobal("IntersectionObserver", undefined);
    // Act / Assert
    expect(pageIntersectionObserver()).toBeNull();
  });

  it("is the one availability check every viewport watch asks", () => {
    // Arrange: every source file under src.
    const src = join(__dirname, "../src");
    const files = filesUnder(src);
    // Act: a hand-rolled check reads `IntersectionObserver` off the global.
    const rolled = files
      .filter((f) => /IntersectionObserver\?:/.test(readFileSync(f, "utf8")))
      .map((f) => relative(src, f));
    // Assert: only the helper's own module.
    expect(rolled).toEqual(["dom.ts"]);
  });
});


describe("nodeLabel", () => {
  it.each([
    { name: "an element with classes", node: () => Object.assign(document.createElement("div"), { className: "bubble md" }), want: "div.bubble.md" },
    { name: "an element with none", node: () => document.createElement("section"), want: "section" },
    {
      name: "a text node, by the element it sits in",
      node: () => Object.assign(document.createElement("p"), { className: "prose" }).appendChild(document.createTextNode("hi")),
      want: "p.prose",
    },
    { name: "a detached text node", node: () => document.createTextNode("hi"), want: "#text" },
  ])("names $name", ({ node, want }) => {
    // Act
    const label = nodeLabel(node());

    // Assert
    expect(label).toBe(want);
  });

  it("is the one place a node is spelled tag.class", () => {
    // Arrange: the spelling's own template, as a hand-rolled copy would write it.
    const spelling = /tagName\.toLowerCase\(\)\}\.\$\{/;

    // Act
    const src = join(process.cwd(), "src");
    const copies = filesUnder(src)
      .filter((file) => relative(src, file) !== "dom.ts")
      .filter((file) => spelling.test(codeOf(readFileSync(file, "utf8"))))
      .map((file) => relative(src, file));

    // Assert
    expect(copies).toEqual([]);
  });
});
