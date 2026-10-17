/**
 * THE ONE WEBKIT PAGE BOOT (boot-page.ts), in real headless WebKit.
 *
 * Its browser is Playwright's WebKit build pinned by `playwright-core`
 * (package.json); `npx playwright-core install webkit` fetches it once.
 */
import path from "node:path";
import { readFile, readdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { webkit, type Browser } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { bootPage, pageAssets } from "./boot-page";

const here = path.dirname(fileURLToPath(import.meta.url));

let browser: Browser;

// THE BROWSER'S COLD START IS THE HOOK'S WORK, under the hook's bound: the
// first page a freshly launched WebKit opens pays for its web process, which
// alone outruns the 850ms a test may take, so the hook opens and closes one
// and every test's boot is a warm one.
beforeAll(async () => {
  browser = await webkit.launch();
  await (await browser.newPage()).close();
});

afterAll(async () => {
  await browser.close();
});

/** A script that publishes `window.sample` once it has run. */
const PUBLISHES = "window.sample = { ready: true };";
/** A stylesheet with nothing in it to apply. */
const NO_RULES = "/* no rules */";

describe("bootPage", () => {
  it("answers once the script has published its API on the named global", async () => {
    // Arrange + Act
    const { page } = await bootPage(browser, {
      script: PUBLISHES,
      css: NO_RULES,
      global: "sample",
      viewport: { width: 300, height: 200 },
    });
    // Assert
    expect(await page.evaluate(() => (window as unknown as { sample: { ready: boolean } }).sample.ready)).toBe(true);
    await page.close();
  });

  it("lays the stylesheet under the page", async () => {
    // Arrange + Act
    const { page } = await bootPage(browser, {
      script: PUBLISHES,
      css: "body { margin: 7px; }",
      global: "sample",
      viewport: { width: 300, height: 200 },
    });
    // Assert
    expect(await page.evaluate(() => getComputedStyle(document.body).marginTop)).toBe("7px");
    await page.close();
  });

  it("opens the page at the viewport and pixel ratio asked for, with the body's style", async () => {
    // Arrange + Act
    const { page } = await bootPage(browser, {
      script: PUBLISHES,
      css: NO_RULES,
      global: "sample",
      viewport: { width: 321, height: 123 },
      deviceScaleFactor: 2,
      bodyStyle: "margin:0;background:#fff",
    });
    // Assert
    const seen = await page.evaluate(() => [
      window.innerWidth,
      window.innerHeight,
      window.devicePixelRatio,
      document.body.getAttribute("style"),
    ]);
    expect(seen).toEqual([321, 123, 2, "margin:0;background:#fff"]);
    await page.close();
  });

  it("fails the boot, naming the error, when the page throws while booting", async () => {
    // Arrange
    const script = `${PUBLISHES} throw new Error("the mount broke");`;
    // Act + Assert
    await expect(
      bootPage(browser, { script, css: NO_RULES, global: "sample", viewport: { width: 300, height: 200 } }),
    ).rejects.toThrow(/the page failed to boot: .*the mount broke/);
  });

  it("answers the list later page errors land in", async () => {
    // Arrange
    const { page, errors } = await bootPage(browser, {
      script: PUBLISHES,
      css: NO_RULES,
      global: "sample",
      viewport: { width: 300, height: 200 },
    });
    const thrown = page.waitForEvent("pageerror");
    // Act
    await page.evaluate(() => {
      setTimeout(() => {
        throw new Error("after the boot");
      }, 0);
    });
    await thrown;
    // Assert
    expect(errors.join("; ")).toMatch(/after the boot/);
    await page.close();
  });
});

describe("pageAssets", () => {
  // THE BUNDLE IS THE HOOK'S WORK, under the hook's bound: a cold bundle
  // (the first after a source change) outruns the 850ms a test may take, so
  // the test only reads what the hook produced.
  let assets: { script: string; css: string };
  beforeAll(async () => {
    assets = await pageAssets("page-log.ts", "pageLog");
  });

  it("bundles the page module beside it and reads the real stylesheet", () => {
    // Assert
    expect([assets.script.includes("installPageLogger"), assets.css.includes("#feed-scroll")]).toEqual([true, true]);
  });
});

describe("the one boot", () => {
  // READING EVERY SUITE IS THE HOOK'S WORK, under the hook's bound, for the
  // same reason as the bundle above; the test reads what the hook found.
  // This suite names the boot's own failure, so it is not one of them.
  let suites: string[];
  const own: string[] = [];
  beforeAll(async () => {
    suites = (await readdir(here)).filter(
      (name) => name.endsWith(".webkit.test.ts") && name !== "boot-page.webkit.test.ts",
    );
    for (const name of suites) {
      const src = await readFile(path.join(here, name), "utf8");
      if (src.includes("the page failed to boot")) own.push(name);
    }
  });

  it("is the only place a WebKit suite boots its page by hand", () => {
    // Assert
    expect([suites.length > 5, own]).toEqual([true, []]);
  });
});
