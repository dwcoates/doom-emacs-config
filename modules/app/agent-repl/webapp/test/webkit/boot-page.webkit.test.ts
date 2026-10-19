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
import { rejectionOf } from "./rejection";

const here = path.dirname(fileURLToPath(import.meta.url));

let browser: Browser;

/*
 * EVERY BOOT, BUNDLE AND READ IS PAID IN A HOOK. The webkit config gives a test
 * body 850ms and a hook 90s (webapp/AGENTS.md "Wait/timeout bounds"), on the
 * contract that a test only reads what its hook produced: a page boot measured
 * ~0.5s healthy and crossed 850ms under a loaded full run. Each describe's
 * `beforeAll` does the work and records what the tests read.
 */
beforeAll(async () => {
  browser = await webkit.launch();
});

afterAll(async () => {
  await browser.close();
});

/** A script that publishes `window.sample` once it has run. */
const PUBLISHES = "window.sample = { ready: true };";
/** A stylesheet with nothing in it to apply. */
const NO_RULES = "/* no rules */";

describe("bootPage", () => {
  /** What each boot the tests read answered. */
  const seen: {
    published?: boolean;
    margin?: string;
    viewport?: unknown[];
    bootFailure?: unknown;
    laterErrors?: string;
  } = {};

  beforeAll(async () => {
    const published = await bootPage(browser, {
      script: PUBLISHES,
      css: NO_RULES,
      global: "sample",
      viewport: { width: 300, height: 200 },
    });
    seen.published = await published.page.evaluate(
      () => (window as unknown as { sample: { ready: boolean } }).sample.ready,
    );
    await published.page.close();

    const styled = await bootPage(browser, {
      script: PUBLISHES,
      css: "body { margin: 7px; }",
      global: "sample",
      viewport: { width: 300, height: 200 },
    });
    seen.margin = await styled.page.evaluate(() => getComputedStyle(document.body).marginTop);
    await styled.page.close();

    const sized = await bootPage(browser, {
      script: PUBLISHES,
      css: NO_RULES,
      global: "sample",
      viewport: { width: 321, height: 123 },
      deviceScaleFactor: 2,
      bodyStyle: "margin:0;background:#fff",
    });
    seen.viewport = await sized.page.evaluate(() => [
      window.innerWidth,
      window.innerHeight,
      window.devicePixelRatio,
      document.body.getAttribute("style"),
    ]);
    await sized.page.close();

    const script = `${PUBLISHES} throw new Error("the mount broke");`;
    seen.bootFailure = await rejectionOf(
      bootPage(browser, { script, css: NO_RULES, global: "sample", viewport: { width: 300, height: 200 } }),
    );

    const { page, errors } = await bootPage(browser, {
      script: PUBLISHES,
      css: NO_RULES,
      global: "sample",
      viewport: { width: 300, height: 200 },
    });
    const thrown = page.waitForEvent("pageerror");
    await page.evaluate(() => {
      setTimeout(() => {
        throw new Error("after the boot");
      }, 0);
    });
    await thrown;
    seen.laterErrors = errors.join("; ");
    await page.close();
  });

  it("answers once the script has published its API on the named global", () => {
    // Arrange + Act: the hook's first boot. Assert
    expect(seen.published).toBe(true);
  });

  it("lays the stylesheet under the page", () => {
    // Arrange + Act: the hook's styled boot. Assert
    expect(seen.margin).toBe("7px");
  });

  it("opens the page at the viewport and pixel ratio asked for, with the body's style", () => {
    // Arrange + Act: the hook's sized boot. Assert
    expect(seen.viewport).toEqual([321, 123, 2, "margin:0;background:#fff"]);
  });

  it("fails the boot, naming the error, when the page throws while booting", () => {
    // Arrange + Act: the hook's throwing boot. Assert
    expect(seen.bootFailure).toBeInstanceOf(Error);
    expect(String(seen.bootFailure)).toMatch(/the page failed to boot: .*the mount broke/);
  });

  it("answers the list later page errors land in", () => {
    // Arrange + Act: the hook's boot that throws after booting. Assert
    expect(seen.laterErrors).toMatch(/after the boot/);
  });
});

describe("pageAssets", () => {
  let assets: { script: string; css: string };

  beforeAll(async () => {
    assets = await pageAssets("page-log.ts", "pageLog");
  });

  it("bundles the page module beside it and reads the real stylesheet", () => {
    // Arrange + Act: the hook's bundle. Assert
    expect([assets.script.includes("installPageLogger"), assets.css.includes("#feed-scroll")]).toEqual([true, true]);
  });
});

describe("the one boot", () => {
  let suites: string[];
  const own: string[] = [];

  beforeAll(async () => {
    // This suite names the boot's own failure, so it is not one of them.
    suites = (await readdir(here)).filter(
      (name) => name.endsWith(".webkit.test.ts") && name !== "boot-page.webkit.test.ts",
    );
    for (const name of suites) {
      const src = await readFile(path.join(here, name), "utf8");
      if (src.includes("the page failed to boot")) own.push(name);
    }
  });

  it("is the only place a WebKit suite boots its page by hand", () => {
    // Arrange + Act: the hook's read of every suite. Assert
    expect([suites.length > 5, own]).toEqual([true, []]);
  });
});
