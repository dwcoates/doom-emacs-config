/**
 * A CONTROL TAKES THE BOX A BUTTON TOOK, IN WEBKIT (owner ruling, 2026-10-02:
 * every `<button>` became an `<ar-button>`, src/control.ts).
 *
 * The user agent styled a button and styles nothing for an undefined element,
 * so styles.css restates the button's base look under `:where(ar-button)`, and
 * draws the controls macOS used to paint as native push buttons to that
 * bezel's geometry. This lays a real `<button>` (the reference, which no src
 * module builds any more) beside a control in headless WebKit over the real
 * stylesheet, and holds their boxes equal: unstyled, author-flattened, and
 * native-drawn, enabled and disabled.
 */
import path from "node:path";
import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

const here = path.dirname(fileURLToPath(import.meta.url));

/**
 * Each case: its name, the class both elements wear, the inline style both
 * wear, the style only the reference button wears, and whether disabled. An
 * unstyled button is drawn by the native theme, which no other element can
 * request, so the base look's reference is a button with that theme off.
 */
const CASES = [
  ["styled by no author rule", "", "", "appearance: none", false],
  ["styled by no author rule, disabled", "", "", "appearance: none", true],
  ["flattened by an author rule", "", "background: none; border: none; padding: 0", "", false],
  ["drawn as a native push button", "composer-send", "", "", false],
  ["drawn as a native push button, disabled", "composer-send", "", "", true],
] as const;

/** A box, rounded to a hundredth of a pixel. */
type Box = [number, number];

describe("a control's box, in WebKit", () => {
  let browser: Browser;
  const boxes = new Map<string, { native: Box; control: Box }>();

  beforeAll(async () => {
    const css = await readFile(path.join(here, "../../src/styles.css"), "utf8");
    browser = await webkit.launch();
    const page = await browser.newPage();
    const rows = CASES.map(([, cls, style, nativeOnly, disabled], i) => {
      const nativeOff = disabled ? " disabled" : "";
      const controlOff = disabled ? ' aria-disabled="true"' : "";
      return `<div style="font-size: 16px; padding: 4px">
        <button id="n${i}" class="${cls}" style="${style}; ${nativeOnly}"${nativeOff}>Send now</button>
        <ar-button id="c${i}" role="button" class="${cls}" style="${style}"${controlOff}>Send now</ar-button>
      </div>`;
    }).join("");
    await page.setContent(`<!doctype html><html><head><style>${css}</style></head><body>${rows}</body></html>`);
    for (const [i, [name]] of CASES.entries()) {
      const read = (id: string): Promise<Box> =>
        page.evaluate((target) => {
          const r = document.getElementById(target)?.getBoundingClientRect();
          if (r === undefined) throw new Error(`${target} did not mount`);
          return [Math.round(r.width * 100) / 100, Math.round(r.height * 100) / 100] as [number, number];
        }, id);
      boxes.set(name, { native: await read(`n${i}`), control: await read(`c${i}`) });
    }
  });

  afterAll(async () => {
    await browser.close();
  });

  it.each(CASES.map(([name]) => [name]))("is the size a button was when %s", (name) => {
    const box = boxes.get(name);
    expect(box?.control).toEqual(box?.native);
  });
});
