/**
 * THE SCROLL RULES: scroll.ts owns every scroll write and every centering
 * jump, and the scroll arithmetic it exports is the one its callers share.
 */
import { describe, expect, it } from "vitest";
import scrollSource from "../src/scroll.ts?raw";
import selectionScrollSource from "../src/sidebar/selection-scroll.ts?raw";
import scrollKeepSource from "../src/sidebar/scroll-keep.ts?raw";
import bandFitSource from "../src/sidebar/band-fit.ts?raw";
import sidebarPopupsSource from "../src/host/sidebar-popups.ts?raw";
import nodeRevealSource from "../src/feed/node-reveal.ts?raw";
import marginStampSource from "../src/feed/margin-stamp.ts?raw";

describe("tailGap", () => {
  it("is the one gap arithmetic: no other source module computes the gap below the view by hand", () => {
    // Arrange
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
    const sources = import.meta.glob(["../src/**/*.ts"], { query: "?raw", import: "default", eager: true }) as Record<string, string>;
    const gap = /scrollHeight\s*-\s*\w+\.(scrollTop\s*-\s*\w+\.clientHeight|clientHeight\s*-\s*\w+\.scrollTop)\b/;
    // Act
    const handRolled = Object.entries(sources)
      .filter(([, text]) => gap.test(text.replace(/export function tailGap[\s\S]*?\n}\n/, "")))
      .map(([path]) => path);
    // Assert
    expect(handRolled).toEqual([]);
  });
});

/**
 * THE USER OWNS THE SCROLL, MECHANIZED (owner rule, 2026-09-23).
 *
 * scroll.ts is the ONE module that writes a scroll position. Every other
 * module is scanned for a scroll write -- a `scrollTop`/`scrollLeft`
 * assignment, a `scrollIntoView`/`scrollTo`/`scrollBy` call, a park, place or
 * shift -- and a hit fails here, so an implicit mover cannot come back quietly.
 * Inside scroll.ts, the writes are held to the named list: the tail owner's
 * two primitives, the reader's own wheel redirect, the reader's own
 * collapse click, a response node's reveal inside its capped bubble, the
 * reader's own drag at the rail's edge, and the reader's own switch to a
 * workspace whose rail row is out of view. A bubble's scroll box has no other
 * writer.
 */
describe("no scroll write outside scroll.ts", () => {
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
  const sources = import.meta.glob("../src/**/*.ts", {
    query: "?raw",
    import: "default",
    eager: true,
  }) as Record<string, string>;

  /** Every src module's text except scroll.ts, which IS the owner. */
  const others = Object.entries(sources).filter(([path]) => !path.endsWith("/scroll.ts"));
  const own = Object.entries(sources).find(([path]) => path.endsWith("/scroll.ts"))?.[1] ?? "";

  /** The modules whose text matches PATTERN, by path. */
  const offending = (pattern: RegExp): string[] =>
    others.filter(([, src]) => pattern.test(src)).map(([path]) => path);

  it("scans a real set of sibling modules, so an empty glob cannot pass it", () => {
    // Arrange + Act + Assert
    expect(others.length).toBeGreaterThan(5);
  });

  it("finds no scrollTop or scrollLeft assignment in any other module", () => {
    // Arrange + Act
    const found = offending(/\.scroll(?:Top|Left)\s*(?:=(?!=)|\+=|-=)/);
    // Assert
    expect(found).toEqual([]);
  });

  it("finds no scrollIntoView, scrollTo or scrollBy call in any other module", () => {
    // Arrange + Act
    const found = offending(/\b(?:scrollIntoView|scrollTo|scrollBy)\s*\(/);
    // Assert
    expect(found).toEqual([]);
  });

  it("finds no park or place call in any other module", () => {
    // Arrange + Act
    const found = offending(/\.(?:park|place)\s*\(/);
    // Assert
    expect(found).toEqual([]);
  });

  it("finds no shift-by-an-amount call in any other module", () => {
    // Arrange + Act — an argument-less `shift()` is Array.prototype.shift.
    const found = offending(/\.shift\s*\(\s*[^)\s]/);
    // Assert
    expect(found).toEqual([]);
  });

  it("holds scroll.ts's own writes to the named list", () => {
    // Arrange + Act
    const writes = own
      .split("\n")
      .filter((line) => /\.scroll(?:Top|Left)\s*(?:=(?!=)|\+=|-=)/.test(line))
      .map((line) => line.trim());
    // Assert
    expect(writes).toEqual([
      "this.box.scrollTop = this.box.scrollHeight;",
      "if (delta !== 0) this.box.scrollTop = to;",
      "redirect.to.scrollTop += redirect.delta;",
      "section.scrollTop = 0;",
      "box.scrollTop += delta;",
      "scroller.scrollTop += step;",
      "region.scrollTop += delta;",
      "region.scrollTop = top;",
      "box.scrollTop += delta;",
      "box.scrollTop = chatEndTop(box);",
      "box.scrollTop = to;",
    ]);
  });

  it("holds scroll.ts to no scrollIntoView, scrollTo or scrollBy call", () => {
    // Arrange + Act + Assert
    expect(/\b(?:scrollIntoView|scrollTo|scrollBy)\s*\(/.test(own)).toBe(false);
  });
});

describe("every centering jump goes through the one centering jump", () => {
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- as above: tsc reads import.meta.glob's raw result as `unknown` without it.
  const sources = import.meta.glob("../src/**/*.ts", {
    query: "?raw",
    import: "default",
    eager: true,
  }) as Record<string, string>;
  const own = Object.entries(sources).find(([path]) => path.endsWith("/scroll.ts"))?.[1] ?? "";
  const others = Object.entries(sources).filter(([path]) => !path.endsWith("/scroll.ts"));

  /** The body of TailFollow's method NAME, from its signature to its closing brace. */
  function methodBody(name: string): string {
    const start = own.indexOf(`\n  ${name}(`);
    if (start < 0) throw new Error(`no method ${name}`);
    return own.slice(start, own.indexOf("\n  }\n", start));
  }

  it.each(["selectionMoved", "detachedWorkSelected", "entryJumped"])("routes %s through centerOn", (name) => {
    // Arrange, Act
    const body = methodBody(name);
    // Assert
    expect(/this\.(?:centerOn|revealCentered)\(/.test(body)).toBe(true);
  });

  it("computes a centering move in one place only, the hold's", () => {
    // Arrange, Act
    const calls = own.match(/revealCenterDelta\(/g)?.length;
    // Assert: its definition and the one call in `recenter`.
    expect([calls, methodBody("private recenter").includes("revealCenterDelta(")]).toEqual([2, true]);
  });

  it("hands every jump a target made by centerTarget, never a measured geometry", () => {
    // Arrange
    const jumpCalls = others.flatMap(([path, src]) =>
      [...src.matchAll(/\.(selectionMoved|detachedWorkSelected|entryJumped)\(([^;]*)\);/g)].map((m) => [path, m[2]]),
    );
    // Act
    const strays = jumpCalls.filter(([, arg]) => arg !== "null" && arg !== "target" && !arg.startsWith("centerTarget("));
    // Assert
    expect([jumpCalls.length > 0, strays]).toEqual([true, []]);
  });
});

describe("spanIntersection", () => {
  it("is the one clip node-reveal and the margin stamp share", () => {
    // Arrange, Act, Assert: neither hand-rolls a max-of-tops, min-of-bottoms clip.
    expect([
      nodeRevealSource.match(/spanIntersection\(/g)?.length,
      nodeRevealSource.includes("Math.max(visible.top"),
      marginStampSource.match(/spanIntersection\(/g)?.length,
      marginStampSource.includes("Math.max(row.top"),
    ]).toEqual([1, false, 1, false]);
  });
});

describe("nearestEdgeOffset", () => {
  it("is the one arithmetic the sidebar's selection scroll and the feed's composer reveal share", () => {
    // Arrange, Act, Assert: neither hand-rolls an edge comparison of its own;
    // scroll.ts holds the definition and the feed's one use.
    expect([
      selectionScrollSource.match(/nearestEdgeOffset\(/g)?.length,
      selectionScrollSource.includes("viewBottom"),
      scrollSource.match(/nearestEdgeOffset\(/g)?.length,
    ]).toEqual([1, false, 2]);
  });
});

describe("TailFollow: the scroll floor under a click-held view", () => {
  it("hand-rolls no click supersede followed by a park outside parkSuperseding", () => {
    // Arrange + Act
    const pairs = scrollSource.match(/this\.supersedeClick\((\w+|"\w+")\);\s*this\.park\(\1\)/g) ?? [];
    // Assert -- the one pair left is parkSuperseding's own body.
    expect(pairs).toEqual(["this.supersedeClick(cause);\n    this.park(cause)"]);
  });
});

describe("heldRowName", () => {
  it("is the one way TailFollow names its held row", () => {
    expect([scrollSource.includes('this.clickedRow === null ? "none"'), scrollSource.match(/heldRowName\(this\.clickedRow\)/g)?.length]).toEqual([false, 2]);
  });
});

describe("scrollKept", () => {
  it.each([
    ["sidebar/scroll-keep.ts", scrollKeepSource],
    ["sidebar/band-fit.ts", bandFitSource],
    ["host/sidebar-popups.ts", sidebarPopupsSource],
  ])("is how %s puts a redrawn scroller back", (_file, source) => {
    // Act
    const keeps = source.includes("scrollKept(");
    // Assert
    expect(keeps).toBe(true);
  });
});
