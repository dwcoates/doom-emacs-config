// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import { readFileSync } from "node:fs";
import { join, relative } from "node:path";
import {
  SVG_NS,
  ancestorMatching,
  childrenMatching,
  computedPx,
  nodeLabel,
  pageIntersectionObserver,
  pageMutationObserver,
  pageResizeObserver,
  placeChildren,
  placeOptional,
  queryAll,
  repaintMark,
  scrollbarWidthPx,
  windowOf,
} from "../src/dom.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";
import { codeUnder, filesUnder } from "./files-under.js";
import { dispatchPageTransition } from "./page-transition.js";

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
    const code = codeUnder(src);
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
    expect([callers, rolled]).toEqual([["bubble/body.ts", "dom.ts", "feed/collapsed-width.ts", "feed/ellipsis-trim.ts", "sidebar/band-fit.ts", "sidebar/selection-scroll.ts"], ["dom.ts"]]);
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
    const spelled = [...codeUnder(src)].filter(([, code]) => code.includes("www.w3.org/2000/svg")).map(([path]) => path);
    // Assert
    expect(spelled).toEqual(["dom.ts"]);
  });
});

describe("windowOf", () => {
  it("answers the window the element lives in", () => {
    // Arrange
    const el = document.createElement("div");
    // Act / Assert
    expect(windowOf(el)).toBe(window);
  });

  it("throws for an element of a document with no window", () => {
    // Arrange
    const el = document.implementation.createHTMLDocument("windowless").createElement("div");
    // Act / Assert
    expect(() => windowOf(el)).toThrow(/lives in a window/);
  });

  it("is the one window lookup every sidebar module asks", () => {
    // Arrange: every sidebar source file.
    const src = join(__dirname, "../src");
    const files = filesUnder(join(src, "sidebar"));
    // Act: a hand-rolled lookup defines its own windowOf.
    const rolled = files.filter((f) => /function windowOf\(/.test(readFileSync(f, "utf8"))).map((f) => relative(src, f));
    // Assert: none, so every caller shares dom.ts's.
    expect(rolled).toEqual([]);
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
    const copies = [...codeUnder(src)]
      .filter(([file, code]) => file !== "dom.ts" && spelling.test(code))
      .map(([file]) => file);

    // Assert
    expect(copies).toEqual([]);
  });
});

/** A tree to query: an outer box around ROOT, which holds rows, a nest and a list. */
function queryTree(): { outer: HTMLElement; root: HTMLElement } {
  document.body.replaceChildren();
  const outer = document.createElement("section");
  outer.className = "outer";
  outer.innerHTML = `
    <div class="root row" id="root">
      <div class="row" id="a"><span class="row" id="a1"></span></div>
      <div class="nest" id="b"><div class="row" id="b1"></div></div>
      <ul id="c"><li class="row" id="c1"></li><li id="c2"></li></ul>
    </div>`;
  document.body.append(outer);
  return { outer, root: outer.querySelector<HTMLElement>("#root")! };
}

const ids = (els: readonly Element[]): string[] => els.map((el) => el.id);

describe("queryAll", () => {
  afterEach(() => {
    document.body.replaceChildren();
  });

  it.each([
    ["a class", ".row"],
    ["every element", "*"],
    ["a descendant combinator", ".nest .row"],
    ["a child combinator", "ul > li"],
    ["an ancestor outside the root", ".outer .row"],
    ["a selector list", "#c2, .nest"],
    ["an attribute", "[id^=c]"],
    ["nothing", ".absent"],
  ])("answers what querySelectorAll answers for %s", (_label, selector) => {
    // Arrange
    const { root } = queryTree();
    // Act
    const found = queryAll(root, selector);
    // Assert
    expect(ids(found)).toEqual(ids([...root.querySelectorAll(selector)]));
  });

  it("answers in document order, nested matches after their ancestor", () => {
    const { root } = queryTree();
    expect(ids(queryAll(root, ".row"))).toEqual(["a", "a1", "b1", "c1"]);
  });

  it("never answers the root itself, even when it matches", () => {
    const { root } = queryTree();
    expect(ids(queryAll(root, ".root"))).toEqual([]);
  });

  it("answers the whole document, its root element included, from a Document", () => {
    queryTree();
    expect(queryAll(document, "html")).toEqual([document.documentElement]);
  });

  it("answers under a DocumentFragment root", () => {
    // Arrange
    const fragment = document.createDocumentFragment();
    const row = document.createElement("p");
    row.className = "row";
    fragment.append(row);
    // Act / Assert
    expect(queryAll(fragment, ".row")).toEqual([row]);
  });

  it("throws the SyntaxError querySelectorAll throws for a malformed selector, even under an empty root", () => {
    // Arrange
    const empty = document.createElement("div");
    let thrown: unknown = null;
    // Act
    try {
      queryAll(empty, "[[");
    } catch (err) {
      thrown = err;
    }
    // Assert
    expect(thrown).toBeInstanceOf(DOMException);
    expect((thrown as DOMException).name).toBe("SyntaxError");
  });

  it("refuses a :scope selector, which matches() would read as the tested element", () => {
    const { root } = queryTree();
    expect(() => queryAll(root, ":scope > .row")).toThrow(/":scope" selectors are refused/);
  });

  it("records the refused :scope selector through the canonical logger", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { root } = queryTree();
    // Act
    expect(() => queryAll(root, ":scope .row")).toThrow();
    // Assert
    const record = await forwardedRecord(capture, "dom.query-scope-refused");
    expect([record.level.case, record.context]).toEqual([
      "error",
      expect.objectContaining({ helper: "queryAll", selector: ":scope .row" }),
    ]);
  });
});

describe("childrenMatching", () => {
  afterEach(() => {
    document.body.replaceChildren();
  });

  it("answers the parent's own matching children, in order, and no deeper element", () => {
    const { root } = queryTree();
    expect(ids(childrenMatching(root, ".row, ul"))).toEqual(["a", "c"]);
  });

  it("answers what querySelectorAll answers for the same :scope > selector", () => {
    const { root } = queryTree();
    expect(ids(childrenMatching(root, "div"))).toEqual(ids([...root.querySelectorAll(":scope > div")]));
  });

  it("answers no children when none match", () => {
    const { root } = queryTree();
    expect(childrenMatching(root, ".absent")).toEqual([]);
  });

  it("refuses a :scope selector and records it through the canonical logger", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { root } = queryTree();
    // Act
    expect(() => childrenMatching(root, ":scope > .row")).toThrow(/":scope" selectors are refused/);
    // Assert
    const record = await forwardedRecord(capture, "dom.query-scope-refused");
    expect(record.context).toEqual(expect.objectContaining({ helper: "childrenMatching", selector: ":scope > .row" }));
  });

});

describe("querySelectorAll", () => {
  it("is the one way the webapp queries: no source calls querySelectorAll, whose answer WebKit caches", () => {
    // Arrange — every source under src/, comments stripped.
    const src = join(process.cwd(), "src");
    const code = [...codeUnder(src)];
    // Act
    const direct = code.filter(([, text]) => /\bquerySelectorAll\b/.test(text)).map(([path]) => path);
    // Assert
    expect(direct).toEqual([]);
  });
});

/** Let queued mutation records reach their observers. */
const records = (): Promise<void> => new Promise((resolve) => setTimeout(resolve, 0));


describe("pageMutationObserver", () => {
  afterEach(() => {
    // Every test ends with the page shown, as the next one expects it.
    dispatchPageTransition("pageshow", true);
    document.body.replaceChildren();
  });

  it("delivers the mutations of what it observes, as MutationObserver does", async () => {
    // Arrange
    const box = document.createElement("div");
    document.body.append(box);
    const seen: Node[] = [];
    const watch = pageMutationObserver(document, (list) => seen.push(...list.map((r) => r.target)));
    watch.observe(box, { childList: true });
    // Act
    box.append(document.createElement("span"));
    await records();
    // Assert
    expect(seen).toEqual([box]);
    watch.disconnect();
  });

  it("watches every target it is asked to", async () => {
    // Arrange
    const a = document.createElement("div");
    const b = document.createElement("div");
    document.body.append(a, b);
    const seen: Node[] = [];
    const watch = pageMutationObserver(document, (list) => seen.push(...list.map((r) => r.target)));
    watch.observe(a, { childList: true });
    watch.observe(b, { childList: true });
    // Act
    a.append(document.createElement("span"));
    b.append(document.createElement("span"));
    await records();
    // Assert
    expect(seen).toEqual([a, b]);
    watch.disconnect();
  });

  it("is disconnected when the page hides, so a mutation during teardown queues nothing", async () => {
    // Arrange
    const box = document.createElement("div");
    document.body.append(box);
    const callback = vi.fn();
    pageMutationObserver(document, callback).observe(box, { childList: true, subtree: true });
    pageMutationObserver(document, callback).observe(document.documentElement, { attributes: true, subtree: true });
    // Act
    dispatchPageTransition("pagehide", false);
    box.append(document.createElement("span"));
    document.documentElement.setAttribute("data-teardown", "");
    await records();
    // Assert
    expect(callback).not.toHaveBeenCalled();
    document.documentElement.removeAttribute("data-teardown");
  });

  it("watches again when the page is restored from the back/forward cache", async () => {
    // Arrange
    const box = document.createElement("div");
    document.body.append(box);
    const callback = vi.fn();
    const watch = pageMutationObserver(document, callback);
    watch.observe(box, { childList: true });
    dispatchPageTransition("pagehide", true);
    // Act
    dispatchPageTransition("pageshow", true);
    box.append(document.createElement("span"));
    await records();
    // Assert
    expect(callback).toHaveBeenCalledTimes(1);
    watch.disconnect();
  });

  it("does not watch again on a pageshow that is not a restore", async () => {
    // Arrange
    const box = document.createElement("div");
    document.body.append(box);
    const callback = vi.fn();
    const watch = pageMutationObserver(document, callback);
    watch.observe(box, { childList: true });
    dispatchPageTransition("pagehide", false);
    // Act
    dispatchPageTransition("pageshow", false);
    box.append(document.createElement("span"));
    await records();
    // Assert
    expect(callback).not.toHaveBeenCalled();
    watch.disconnect();
  });

  it("holds a target asked for while the page is hidden until the page is restored", async () => {
    // Arrange
    const box = document.createElement("div");
    document.body.append(box);
    const callback = vi.fn();
    const watch = pageMutationObserver(document, callback);
    dispatchPageTransition("pagehide", true);
    watch.observe(box, { childList: true });
    box.append(document.createElement("span"));
    await records();
    expect(callback).not.toHaveBeenCalled();
    // Act
    dispatchPageTransition("pageshow", true);
    box.append(document.createElement("span"));
    await records();
    // Assert
    expect(callback).toHaveBeenCalledTimes(1);
    watch.disconnect();
  });

  it("stays disconnected across a restore once its own disconnect ran", async () => {
    // Arrange
    const box = document.createElement("div");
    document.body.append(box);
    const callback = vi.fn();
    const watch = pageMutationObserver(document, callback);
    watch.observe(box, { childList: true });
    watch.disconnect();
    // Act
    dispatchPageTransition("pagehide", true);
    dispatchPageTransition("pageshow", true);
    box.append(document.createElement("span"));
    await records();
    // Assert
    expect(callback).not.toHaveBeenCalled();
  });

  it("builds its observer with the page's own MutationObserver, so a wrapper of it sees every observer", () => {
    // Arrange
    const win = document.defaultView!;
    const original = win.MutationObserver;
    let built = 0;
    win.MutationObserver = class extends original {
      constructor(callback: MutationCallback) {
        super(callback);
        built += 1;
      }
    };
    try {
      // Act
      pageMutationObserver(document, () => undefined).disconnect();
    } finally {
      win.MutationObserver = original;
    }
    // Assert
    expect(built).toBe(1);
  });

  it("refuses a document with no window, and records it through the canonical logger", async () => {
    // Arrange
    const capture = captureLogRecords();
    const windowless = document.implementation.createHTMLDocument("detached");
    // Act
    expect(() => pageMutationObserver(windowless, () => undefined)).toThrow(/has no window/);
    // Assert
    const record = await forwardedRecord(capture, "dom.page-observer-windowless");
    expect([record.level.case, record.context]).toEqual(["error", expect.objectContaining({ document: windowless.URL })]);
  });
});

describe("MutationObserver", () => {
  it("is built only by pageMutationObserver: no source constructs one a page would carry into teardown", () => {
    // Arrange — every source under src/, comments stripped.
    const src = join(process.cwd(), "src");
    const code = [...codeUnder(src)];
    // Act
    const builders = code.filter(([, text]) => /\bnew\s+(?:[\w$]+\.)*MutationObserver\b/.test(text)).map(([path]) => path);
    // Assert
    expect(builders).toEqual(["dom.ts"]);
  });
});
