// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { readFileSync, readdirSync, statSync } from "node:fs";
import { join, relative } from "node:path";
import { ancestorMatching, placeChildren, scrollbarWidthPx } from "../src/dom.js";
import { codeOf } from "./source-text.js";

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
    const walk = (dir: string): string[] =>
      readdirSync(dir).flatMap((name) => {
        const path = join(dir, name);
        if (statSync(path).isDirectory()) return walk(path);
        return path.endsWith(".ts") ? [path] : [];
      });
    const code = new Map(
      walk(src).map((path) => [
        relative(src, path),
        codeOf(readFileSync(path, "utf8")),
      ]),
    );
    // Act
    const callers = [...code].filter(([, text]) => /\bscrollbarWidthPx\(/.test(text)).map(([p]) => p).sort();
    const rolled = [...code].filter(([, text]) => /offsetWidth\s*-[^;]*clientWidth/.test(text)).map(([p]) => p);
    // Assert
    expect([callers, rolled]).toEqual([["bubble/body.ts", "dom.ts", "expand.ts"], ["dom.ts"]]);
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
