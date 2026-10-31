/**
 * A FOLDED SECTION'S HEADER SPANS RULE TO RULE, IN WEBKIT (owner request,
 * 2026-10-31).
 *
 * A header's hover wash is its own box, so a folded Open, Deploys or Hidden
 * header must reach from the rule above it down to the rule below it, leaving
 * no bare strip at either edge. jsdom lays nothing out, so this draws the
 * rail's sections in the shape roster.ts draws them, in real headless WebKit
 * over the real stylesheet, and measures each folded header against its
 * rules.
 */
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { pageStylesheet } from "./stylesheets";

/** Which of the three sections are folded; Deploys is not drawn at all when `merged` is null. */
interface Folds {
  open: boolean;
  merged: boolean | null;
  hidden: boolean;
}

/** One section box around one row, as roster.ts draws it. */
function section(cls: string, label: string, folded: boolean): string {
  return `<div class="repo ${cls}${folded ? " folded" : ""}"><div class="repo-head"><span class="sb-label">${label}</span><span class="sb-count">(1)</span></div><div class="rows"><div class="ws"><div class="row"><span class="name">ws</span></div></div></div></div>`;
}

/** The rail under its head, its sections folded as FOLDS says. */
function rail(folds: Folds): string {
  const open = `<div class="repo open-section${folds.open ? " folded" : ""}"><div class="repo-head"><span class="sb-label">Open</span><span class="sb-count">(1)</span></div><div class="sb-sections">${section("repo-section", "repo", false)}</div></div>`;
  return `<div class="sb-head"><span class="sb-title">Workspaces</span></div><div class="sb-scroll"><div class="sb-roster"><div class="sb-pane">${open}${folds.merged === null ? "" : section("merged-section", "Deploys", folds.merged)}${section("hidden-section", "Hidden", folds.hidden)}</div></div></div>`;
}

/** How far a folded header's box falls short of the rule above and the rule below, in px. */
interface Shortfall {
  above: number;
  below: number;
}

/** Draw FOLDS and measure the header of the section SELECTOR names against its rules. */
async function shortfall(page: Page, folds: Folds, selector: string): Promise<Shortfall> {
  return page.evaluate(
    ({ markup, selector }) => {
      const host = document.getElementById("ws-sidebar");
      if (host === null) throw new Error("the page's rail did not mount");
      host.innerHTML = markup;
      const sectionEl = host.querySelector(selector) as HTMLElement;
      const head = (sectionEl.querySelector(":scope > .repo-head") as HTMLElement).getBoundingClientRect();
      // The rule above: the rail head's bottom rule over the Open section, a
      // band's own top border otherwise.
      const previous = sectionEl.previousElementSibling as HTMLElement | null;
      const ruleAbove =
        previous === null
          ? (host.querySelector(".sb-head") as HTMLElement).getBoundingClientRect().bottom
          : sectionEl.getBoundingClientRect().top + parseFloat(getComputedStyle(sectionEl).borderTopWidth);
      const next = sectionEl.nextElementSibling as HTMLElement;
      const ruleBelow = next.getBoundingClientRect().top;
      return { above: head.top - ruleAbove, below: ruleBelow - head.bottom };
    },
    { markup: rail(folds), selector },
  );
}

describe("a folded section's header in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await pageStylesheet();
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 400, height: 900 } });
    await page.setContent(`<!doctype html><html><head></head><body><nav id="ws-sidebar"></nav></body></html>`);
    await page.addStyleTag({ content: css });
  });

  afterAll(async () => {
    await browser?.close();
  });

  it("reaches the rule above a folded Open section", async () => {
    // ARRANGE / ACT
    const measured = await shortfall(page, { open: true, merged: false, hidden: false }, ".open-section");
    // ASSERT
    expect(measured.above).toBeCloseTo(0, 1);
  });

  it("reaches the rule below a folded Open section", async () => {
    // ARRANGE / ACT
    const measured = await shortfall(page, { open: true, merged: false, hidden: false }, ".open-section");
    // ASSERT
    expect(measured.below).toBeCloseTo(0, 1);
  });

  it("reaches the rule above a folded Deploys section", async () => {
    // ARRANGE / ACT
    const measured = await shortfall(page, { open: false, merged: true, hidden: false }, ".merged-section");
    // ASSERT
    expect(measured.above).toBeCloseTo(0, 1);
  });

  it("reaches the rule below a folded Deploys section", async () => {
    // ARRANGE / ACT
    const measured = await shortfall(page, { open: false, merged: true, hidden: false }, ".merged-section");
    // ASSERT
    expect(measured.below).toBeCloseTo(0, 1);
  });

  it("reaches the rule below a folded Open section over a folded Deploys", async () => {
    // ARRANGE / ACT
    const measured = await shortfall(page, { open: true, merged: true, hidden: true }, ".open-section");
    // ASSERT
    expect(measured.below).toBeCloseTo(0, 1);
  });

  it("reaches the rule below a folded Open section straight over Hidden", async () => {
    // ARRANGE / ACT
    const measured = await shortfall(page, { open: true, merged: null, hidden: false }, ".open-section");
    // ASSERT
    expect(measured.below).toBeCloseTo(0, 1);
  });
});
