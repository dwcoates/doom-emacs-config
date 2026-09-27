/**
 * THE ONE-BUBBLE GUARD (owner rulings, 2026-09-23). Every blue and purple
 * bubble is built by `drawBubble` (src/bubble/draw.ts) over the one body
 * pipeline (src/bubble/body.ts), hung in the one scroll box, measured by the
 * one has-more measurer and opened by the one toggle (expand.ts). This reads
 * the whole `src` tree and fails on any source that builds a bubble, a scroll
 * box or a body of its own, or carries its own copy of the wrap, paint, scroll,
 * expand or has-more logic — and holds every bubble kind to the shared shape:
 * it builds a spec, calls `drawBubble`, and hands its prose to the body as a
 * markdown slot rather than rendering markdown itself.
 */
import { readdirSync, readFileSync, statSync } from "node:fs";
import { join, relative } from "node:path";
import { describe, expect, it } from "vitest";

const SRC = join(process.cwd(), "src");

/** Every `.ts` source under DIR, recursively. */
function sourcesUnder(dir: string): string[] {
  return readdirSync(dir).flatMap((name) => {
    const path = join(dir, name);
    if (statSync(path).isDirectory()) return sourcesUnder(path);
    return path.endsWith(".ts") ? [path] : [];
  });
}

/** Every source, keyed by its path under src/, comments stripped. */
const SOURCES: ReadonlyMap<string, string> = new Map(
  sourcesUnder(SRC).map((path) => [
    relative(SRC, path),
    readFileSync(path, "utf8")
      .replace(/\/\*[\s\S]*?\*\//g, "")
      .replace(/^\s*\/\/.*$/gm, ""),
  ]),
);

/** The sources whose code matches PATTERN. */
function sourcesMatching(pattern: RegExp): string[] {
  return [...SOURCES].filter(([, code]) => pattern.test(code)).map(([path]) => path).sort();
}

/** Every bubble kind's module, and the spec-building shape each must share. */
const KINDS = [
  "feed/cards/response.ts",
  "feed/cards/controls.ts",
  "feed/rows/user-prompt.ts",
  "feed/rows/agent-prompt.ts",
  "feed/rows/peer-message.ts",
  "feed/rows/separation.ts",
  "feed/rows/turn-ended.ts",
  "tray/held-prompt.ts",
] as const;

describe("every blue and purple bubble is built by drawBubble", () => {
  it("builds a `.bubble` element nowhere else", () => {
    expect(
      sourcesMatching(/className\s*=\s*[`"']bubble(?![-\w])|classList\.add\(\s*["']bubble["']/),
    ).toEqual([]);
  });

  it("names the bubble class only in the module that draws it", () => {
    expect(sourcesMatching(/\bBUBBLE_CLASS\s*=|["']bubble["']/)).toEqual(["bubble/draw.ts"]);
  });

  it("hangs a scroll box only there", () => {
    expect(sourcesMatching(/\bbubbleScroll\(/)).toEqual(["bubble/draw.ts", "feed/bubble-scroll.ts"]);
  });

  it("makes a bubble body only there", () => {
    expect(sourcesMatching(/\bcreateBubbleBody\(/)).toEqual(["bubble/body.ts", "bubble/draw.ts"]);
  });

  it.each(KINDS)("has %s build a spec and hand it to drawBubble", (kind) => {
    expect(SOURCES.get(kind)).toMatch(/\bdrawBubble\(/);
  });

  it("is called by the bubble kinds and by nothing else", () => {
    expect(sourcesMatching(/\bdrawBubble\(/)).toEqual(["bubble/draw.ts", ...KINDS].sort());
  });
});

describe("no bubble kind carries its own copy of the shared logic", () => {
  it("paints and wraps a body only in the body pipeline", () => {
    expect(sourcesMatching(/\b(?:measureTreeCols|proseHtml|reconcileChildren)\(/)).toEqual(["bubble/body.ts"]);
  });

  it("paints a body only through drawBubble, and repaints a slot only for the type-out", () => {
    expect([sourcesMatching(/\bpaintBody\(/), sourcesMatching(/\brepaintSlot\(/)]).toEqual([
      ["bubble/body.ts", "bubble/draw.ts"],
      ["bubble/body.ts", "feed/cards/response.ts"],
    ]);
  });

  it.each(KINDS)("has %s hand its prose to the body as a slot, never render markdown itself", (kind) => {
    expect(SOURCES.get(kind)).not.toMatch(/\brenderMarkdown\(|\.innerHTML\s*=/);
  });

  it("measures has-more only in the one measurer and the two places it is armed", () => {
    expect(sourcesMatching(/\binstallHasMore\(/)).toEqual([
      "feed/bubble-more.ts",
      "feed/bubble-scroll.ts",
      "feed/title-fold.ts",
    ]);
  });

  it("toggles a fold open only in expand.ts", () => {
    expect(sourcesMatching(/\btoggleSection\(|classList\.(?:add|toggle)\(\s*EXPANDED_CLASS/)).toEqual([
      "expand.ts",
    ]);
  });

  it("collapses a fold only in expand.ts", () => {
    expect(sourcesMatching(/classList\.remove\(\s*EXPANDED_CLASS/)).toEqual(["expand.ts"]);
  });

  it("collapses a fold at exactly one site, the one collapse every trigger shares", () => {
    expect(SOURCES.get("expand.ts")?.match(/classList\.remove\(\s*EXPANDED_CLASS/g)).toHaveLength(1);
  });

  it.each(KINDS)("has %s arm no click toggle of its own", (kind) => {
    expect(SOURCES.get(kind)).not.toMatch(/\bEXPANDED_CLASS\b|["']expanded["']|aria-expanded/);
  });

  it.each(KINDS)("has %s write no scroll position", (kind) => {
    expect(SOURCES.get(kind)).not.toMatch(/\bscroll(?:Top|Left)\s*=|scrollIntoView|scrollTo\(/);
  });
});
