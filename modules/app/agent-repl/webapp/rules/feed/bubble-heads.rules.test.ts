/**
 * EVERY BUBBLE WEARS THE ONE BUBBLE CHROME (bubble.ts). A bubble's head is
 * drawn by a `*Head` row renderer, the chrome is the only place such a
 * renderer is called, and a head's drawing function is reached only through
 * the registry that fills that renderer, so a head can only ever be drawn
 * inside the chrome's frame, head row, fold and sub-feed. The merge head's look (`merge-head`) is
 * drawn only by the modules that draw a bubble head or the merge frame, so no
 * row draws that look on its own outside a frame (a child's row once did,
 * full-width and in the feed's own fonts, 2026-11-26).
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { codeUnder, srcDir } from "../files-under.js";
import { codeOf } from "../../test/source-text.js";

/** The text of the function NAME declared in CODE at any depth, through its closing brace at the declaration's own indent. */
function nestedFunction(code: string, name: string): string {
  const start = code.indexOf(`function ${name}(`);
  if (start < 0) throw new Error(`the code declares no function ${name}`);
  const lineStart = code.lastIndexOf("\n", start) + 1;
  const indent = code.slice(lineStart, start).match(/^\s*/)?.[0] ?? "";
  const end = code.indexOf(`\n${indent}}\n`, start);
  if (end < 0) throw new Error(`the function ${name} has no closing brace at its own indent`);
  return code.slice(start, end);
}

/** The bubble chrome's head: feed.ts's bubbleHead, the one caller of a head renderer. */
function chromeHead(): string {
  return nestedFunction(codeOf(readFileSync(path.join(srcDir, "feed", "feed.ts"), "utf8")), "bubbleHead");
}

/** The row renderers that draw a bubble's head: RowRenderers' `*Head` members. */
function headRenderers(): string[] {
  const code = codeOf(readFileSync(path.join(srcDir, "feed", "renderers.ts"), "utf8"));
  const seam = code.slice(code.indexOf("export interface RowRenderers"));
  return [...seam.slice(0, seam.indexOf("\n}")).matchAll(/^\s+(\w+Head)\(/gm)].map((m) => m[1]);
}

describe("every bubble head is drawn inside the bubble chrome", () => {
  it("finds the head renderers it holds to the rule", () => {
    expect(headRenderers()).toEqual(
      expect.arrayContaining(["shellHead", "mergeHead", "subagentHead", "detachedSubagentHead", "childHead"]),
    );
  });

  it("calls a head renderer nowhere but the chrome's head (feed.ts bubbleHead)", () => {
    const heads = headRenderers();
    const call = new RegExp(`\\.(${heads.join("|")})\\(`);
    const offenders = [...codeUnder(srcDir)].flatMap(([file, code]) => {
      const scanned = file === path.join("feed", "feed.ts") ? code.replace(nestedFunction(code, "bubbleHead"), "") : code;
      return call.test(scanned) ? [file] : [];
    });
    expect(offenders).toEqual([]);
  });

  it("calls every head renderer from the chrome's head", () => {
    const body = chromeHead();
    expect(headRenderers().filter((head) => !body.includes(`renderers.${head}(`))).toEqual([]);
  });
});

/** Each head renderer's drawing function, as the registry (row-renderers.ts) fills the seam: head key to function name. */
function registeredHeads(): Map<string, string> {
  const code = codeOf(readFileSync(path.join(srcDir, "feed", "row-renderers.ts"), "utf8"));
  return new Map([...code.matchAll(/^\s+(\w+Head):\s*(?:withCardQuestion\()?(\w+)/gm)].map((m) => [m[1], m[2]]));
}

describe("a head's drawing function is drawn only as a head", () => {
  it("registers a drawing function for every head renderer", () => {
    expect([...registeredHeads().keys()].sort()).toEqual([...headRenderers()].sort());
  });

  it("names a head's drawing function nowhere but its own module and the registry, once", () => {
    // A row path that imported a head's drawing function would draw the head
    // with no bubble around it, as a child's row once was (feed-view.ts).
    const registry = path.join("feed", "row-renderers.ts");
    const code = codeUnder(srcDir);
    const offenders = [...registeredHeads().values()].flatMap((fn) => {
      const named = new RegExp(`\\b${fn}\\b`, "g");
      return [...code].flatMap(([file, text]) => {
        if (text.includes(`export function ${fn}(`)) return [];
        const uses = text.match(named)?.length ?? 0;
        // The registry imports it once and registers it once.
        if (file === registry) return uses === 2 ? [] : [`${file} names ${fn} ${uses} times`];
        return uses === 0 ? [] : [`${file} names ${fn}`];
      });
    });
    expect(offenders).toEqual([]);
  });
});

describe("the merge head's look is drawn only in a frame", () => {
  it("names the merge-head class only in the bubble heads and the merge frame", () => {
    const drawers = new Set([
      path.join("feed", "merge", "merge.ts"),
      path.join("feed", "child.ts"),
      path.join("feed", "cards", "merge-frame.ts"),
    ]);
    const look = /["'`][^"'`]*\bmerge-head\b(?!-)/;
    const offenders = [...codeUnder(srcDir)]
      .filter(([file, code]) => look.test(code) && !drawers.has(file))
      .map(([file]) => file);
    expect(offenders).toEqual([]);
  });
});
