// @vitest-environment node
/**
 * The DOM helpers of src/dom.ts are the only ones: no source rolls its own
 * scrollbar width, px parse, glyph repaint, optional placement, svg namespace,
 * decorative element, window lookup, observer availability check, node
 * spelling, querySelectorAll or MutationObserver.
 */
import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import { join, relative } from "node:path";
import { codeUnder, filesUnder } from "./files-under.js";

describe("scrollbarWidthPx", () => {
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

describe("repaintMark", () => {
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
  it("is the one spelling: no source but dom.ts writes the namespace out", () => {
    // Arrange — every source under src/, comments stripped.
    const src = join(process.cwd(), "src");
    // Act
    const spelled = [...codeUnder(src)].filter(([, code]) => code.includes("www.w3.org/2000/svg")).map(([path]) => path);
    // Assert
    expect(spelled).toEqual(["dom.ts"]);
  });
});

describe("decorativeElement", () => {
  /** A hand-rolled decorative element: a classed span or div set aria-hidden by hand. */
  const HAND_ROLLED = /const (\w+) = document\.createElement\("(?:span|div)"\);\s*\1\.className = [^;]+;(?:\s*\1\.setAttribute\([^;]+\);)*?\s*\1\.setAttribute\("aria-hidden", "true"\)/;

  it("is the one maker: no source hand-rolls a classed element it hides", () => {
    // Arrange — every source under src/, comments stripped.
    const src = join(process.cwd(), "src");
    // Act
    const code = [...codeUnder(src)];
    const rolled = code.filter(([, text]) => HAND_ROLLED.test(text)).map(([path]) => path);
    const callers = code.filter(([, text]) => /\bdecorativeElement\(/.test(text)).map(([path]) => path);
    // Assert: none rolls its own, and the sidebar's rules are among its callers.
    expect([rolled, ["sidebar/merge-queue-view.ts", "sidebar/roster.ts"].every((path) => callers.includes(path))]).toEqual([[], true]);
  });

  it("recognizes a hand-rolled copy", () => {
    const copy = 'const g = document.createElement("span");\n  g.className = "x";\n  g.setAttribute("data-glyph", n);\n  g.setAttribute("aria-hidden", "true");';
    expect(HAND_ROLLED.test(copy)).toBe(true);
  });
});

describe("windowOf", () => {
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
