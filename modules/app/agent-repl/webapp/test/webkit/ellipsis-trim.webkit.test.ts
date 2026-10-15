/**
 * A COLLAPSED ONE-LINE CLAMP'S `…` FOLLOWS A WORD CHARACTER, IN WEBKIT (owner
 * request, 2026-10-15: "hello world…", never "hello world …", "hello world.…",
 * "hello world,…" or "hello world-…").
 *
 * The engine writes the `…` and an ellipsis has no client rect, so what a clamp
 * shows is read from its paint: the text is a solid sentinel red over solid
 * blue, and the clamp's row of inked pixel columns is matched against the same
 * row of an unclamped line reading each prefix of the text followed by `…`.
 * Each case is drawn twice, untrimmed (the engine's own cut) and trimmed by the
 * REAL `refreshHasMore` (bundled), and the trimmed one must show exactly the
 * untrimmed one's prefix with its trailing spaces, periods, commas and hyphens
 * dropped: never a trimmable character before the `…`, and never a word
 * character more dropped than that.
 */
import path from "node:path";
import { fileURLToPath } from "node:url";
import type { Browser, Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { ELLIPSIS_CAP_LINES } from "../../src/bubble/draw.js";
import { BUBBLE_MORE_ELLIPSIS } from "../../src/feed/bubble-more.js";
import { isTrimmable } from "../../src/feed/ellipsis-trim.js";
import { TITLE_FOLD_CLASS } from "../../src/feed/title-fold.js";
import { bundlePage } from "./bundle";
import { screenshotPixels } from "./pixels";
import { openFeedPage } from "./feed-page";

const here = path.dirname(fileURLToPath(import.meta.url));

/** Solid red text over solid blue boxes with no shadow or motion: a red pixel is ink. */
const SENTINEL = `
  .tool-card, .bubble { background: rgb(0, 0, 255) !important; box-shadow: none !important; animation: none !important;
    margin: 0 !important; }
  .tool-card *, .bubble * { color: rgb(255, 0, 0) !important; }
  .row { height: 40px; overflow: hidden; }`;

/** One run of trimmable characters per text, each between two words. */
const TEXTS = {
  space: "hello world and then more words follow here",
  period: "hello world. and then more words follow here",
  comma: "hello world, and then more words follow here",
  hyphen: "hello world - and then more words follow here",
  run: "hello world.,- and then more words follow here",
} as const;

type TextKey = keyof typeof TEXTS;

/** The card widths each text is clamped at, 1px apart so every cut is met. */
const WIDTHS = Array.from({ length: 50 }, (_, i) => 80 + i);

/** The bubble widths each text is clamped at. */
const BUBBLE_WIDTHS = Array.from({ length: 50 }, (_, i) => 95 + i);

/** A clamp kind: its markup at a width, and the clamp element's selector. */
interface Kind {
  readonly clamp: string;
  readonly box: string;
  card(text: string, width: number, clamped: boolean): string;
}

const TITLE: Kind = {
  clamp: ".tool-input",
  box: `.${TITLE_FOLD_CLASS}`,
  card: (text, width, clamped) =>
    `<div class="tool-card tool-fold" style="width: ${width}px">` +
    (clamped
      ? `<div class="tool-input ${TITLE_FOLD_CLASS}">${text}</div></div>`
      : `<div class="tool-input" style="white-space: nowrap">${text}</div></div>`),
};

const BUBBLE: Kind = {
  clamp: ".bubble-body",
  box: ".bubble-scroll",
  card: (text, width, clamped) =>
    `<div class="bubble md" data-role="prompt" data-variant="held" data-cap-lines="${ELLIPSIS_CAP_LINES}"` +
    ` data-more="${clamped ? BUBBLE_MORE_ELLIPSIS : "fade"}" style="width: ${width}px; max-width: none">` +
    `<div class="bubble-box bubble-scroll"><div class="bubble-body"><p${clamped ? "" : ' style="white-space: nowrap"'}>${text}</p></div></div></div>`,
};

/** One character of a row's first line, as the trim's own walk reads it, in CSS px. */
interface Laid {
  ch: string;
  right: number;
}

/** What a page draw answers per row: its clamp's box and its first line's characters. */
interface Drawn {
  top: number;
  bottom: number;
  left: number;
  line: Laid[];
}

/** One row of the page: its id and its markup. */
interface Row {
  id: string;
  html: string;
  /** Whether the real measurer runs over the row's box. */
  trim: boolean;
}

/** Draw ROWS one under another, run the measurer over the trimmed ones, and answer each row. */
async function draw(page: Page, rows: Row[], kind: Kind): Promise<Record<string, Drawn>> {
  return page.evaluate(
    ({ rows, clamp, box }) => {
      const feed = document.getElementById("feed");
      if (feed === null) throw new Error("the page's shell did not mount");
      feed.innerHTML = rows.map((r) => `<div class="row" id="${r.id}">${r.html}</div>`).join("");
      const api = (
        window as unknown as {
          EllipsisTrim: {
            refreshHasMore(box: HTMLElement): void;
            domClampGeometry: { firstLine(clamp: HTMLElement): { ch: string; right: number }[] };
          };
        }
      ).EllipsisTrim;
      for (const r of rows) {
        if (r.trim) api.refreshHasMore(document.querySelector<HTMLElement>(`#${r.id} ${box}`)!);
      }
      const out: Record<string, { top: number; bottom: number; left: number; line: { ch: string; right: number }[] }> =
        {};
      for (const r of rows) {
        const el = document.querySelector<HTMLElement>(`#${r.id} ${clamp}`)!;
        const rect = el.getBoundingClientRect();
        out[r.id] = { top: rect.top, bottom: rect.bottom, left: rect.left, line: api.domClampGeometry.firstLine(el) };
      }
      return out;
    },
    { rows, clamp: kind.clamp, box: kind.box },
  );
}

/** Each row's rightmost inked pixel column's right edge, in CSS px. */
async function inkEnds(page: Page, rows: Record<string, Drawn>): Promise<Record<string, number>> {
  const bottom = Math.ceil(Math.max(...Object.values(rows).map((r) => r.bottom)));
  const px = await screenshotPixels(page, { x: 0, y: 0, width: 800, height: bottom });
  const out: Record<string, number> = {};
  for (const [id, r] of Object.entries(rows)) {
    let end = -Infinity;
    for (let y = Math.floor(r.top); y < Math.ceil(r.bottom); y++) {
      for (let x = Math.floor(r.left); x < 800; x++) {
        const [red, , blue] = px.rgba(x, y);
        if (red > 100 && blue < 200) end = Math.max(end, x + 1);
      }
    }
    if (end === -Infinity) throw new Error(`row ${id} painted no text`);
    out[id] = end;
  }
  return out;
}

/**
 * How far right of the character before it the `…` inks, in px: read off an
 * unclamped line reading "hello…", whose last character before the `…` is known.
 */
function inkOffset(ref: Drawn, end: number): number {
  const o = ref.line.filter((c) => c.ch === "o").at(-1);
  if (o === undefined) throw new Error("the reference line laid out no 'o'");
  return end - o.right;
}

/**
 * The text a clamp's `…` follows: its first line up to the character whose
 * right edge sits the reference offset left of the `…`'s ink, or undefined
 * when no character sits within a pixel and a half of it (each trimmable mark
 * is wider than that, so a match names one character).
 */
function before(row: Drawn, end: number, offset: number): string | undefined {
  const at = end - offset;
  let best = -1;
  for (let i = 0; i < row.line.length; i++) {
    if (best < 0 || Math.abs(row.line[i].right - at) < Math.abs(row.line[best].right - at)) best = i;
  }
  if (best < 0 || Math.abs(row.line[best].right - at) > 1.5) return undefined;
  return row.line
    .slice(0, best + 1)
    .map((c) => c.ch)
    .join("");
}

/** PREFIX with its trailing trimmable characters dropped. */
function trimmed(prefix: string): string {
  let end = prefix.length;
  while (end > 0 && isTrimmable(prefix[end - 1])) end--;
  return prefix.slice(0, end);
}

/** One case of one text in one kind: what the untrimmed and the trimmed clamps' `…` followed. */
interface Shown {
  width: number;
  untrimmed: string | undefined;
  trimmed: string | undefined;
}

async function shownAcross(page: Page, kind: Kind, text: string, widths: number[]): Promise<Shown[]> {
  const rows: Row[] = [{ id: "ref", html: kind.card("hello…", 700, false), trim: false }];
  for (const w of widths) {
    rows.push({ id: `raw-${w}`, html: kind.card(text, w, true), trim: false });
    rows.push({ id: `cut-${w}`, html: kind.card(text, w, true), trim: true });
  }
  const drawn = await draw(page, rows, kind);
  const ends = await inkEnds(page, drawn);
  const offset = inkOffset(drawn.ref, ends.ref);
  return widths.map((w) => ({
    width: w,
    untrimmed: before(drawn[`raw-${w}`], ends[`raw-${w}`], offset),
    trimmed: before(drawn[`cut-${w}`], ends[`cut-${w}`], offset),
  }));
}

/** The cases whose trimmed clamp's `…` did not follow the untrimmed one's text, trimmed. */
function misplaced(shown: Shown[]): Shown[] {
  return shown.filter(
    (s) => s.untrimmed === undefined || trimmed(s.untrimmed) === "" || s.trimmed !== trimmed(s.untrimmed),
  );
}

describe("a collapsed one-line clamp's ellipsis in WebKit", () => {
  let browser: Browser;
  // THE PAINTS ARE THE beforeAll'S, AND THE TESTS ONLY READ THEM, as every
  // webkit suite does (vitest.webkit.config.ts).
  const titles = {} as Record<TextKey, Shown[]>;
  const bubbles = {} as Record<TextKey, Shown[]>;

  beforeAll(async () => {
    let page: Page;
    ({ browser, page } = await openFeedPage({ width: 800, height: 9000, deviceScaleFactor: 1, styles: [SENTINEL] }));
    await page.addScriptTag({ content: await bundlePage(path.join(here, "ellipsis-trim-page.ts"), "EllipsisTrim") });
    for (const key of Object.keys(TEXTS) as TextKey[]) {
      titles[key] = await shownAcross(page, TITLE, TEXTS[key], WIDTHS);
      bubbles[key] = await shownAcross(page, BUBBLE, TEXTS[key], BUBBLE_WIDTHS);
    }
  });

  afterAll(async () => {
    await browser.close();
  });

  it("meets every trimmable character at the untrimmed cut across the widths", () => {
    // Arrange / Act
    const cut = Object.values(titles)
      .flat()
      .map((s) => s.untrimmed?.at(-1))
      .filter((ch) => ch !== undefined && isTrimmable(ch));

    // Assert: the sweep reaches the bug it guards against, for each mark.
    expect(new Set(cut)).toEqual(new Set([" ", ".", ",", "-"]));
  });

  for (const key of Object.keys(TEXTS) as TextKey[]) {
    it(`ends a tool card title's ellipsis after a word character (${key})`, () => {
      // Arrange / Act
      const bad = misplaced(titles[key]);

      // Assert
      expect(bad).toEqual([]);
    });

    it(`ends an ellipsis bubble's ellipsis after a word character (${key})`, () => {
      // Arrange / Act
      const bad = misplaced(bubbles[key]);

      // Assert
      expect(bad).toEqual([]);
    });
  }
});
