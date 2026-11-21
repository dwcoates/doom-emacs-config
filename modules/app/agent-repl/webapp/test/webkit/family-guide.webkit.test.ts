/**
 * THE FAMILY GUIDE RUNS DOWN THE MIDDLE OF THE PARENT'S STATUS MARK, IN WEBKIT
 * (owner request, 2026-11-21).
 *
 * A parent's children stand one generation step in beside a vertical guide
 * drawn straight down from the middle of the parent's status mark, and a
 * subsection's top-level rows stand under its header exactly as children
 * stand under their parent, beside the same guide, drawn where a top-level
 * row's mark stands. jsdom lays nothing out, so this draws rail rows as the
 * roster draws them (src/sidebar/row.ts: the recency gutter's slot, the tab
 * number, the status mark, the name) in real headless WebKit over the real
 * stylesheet, and measures them.
 */
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { pageStylesheet } from "./stylesheets";

/** One rail row as the roster draws it, holding KIDS when it has any. */
function ws(id: string, kids = ""): string {
  const box = kids === "" ? "" : `<div class="kids">${kids}</div>`;
  return (
    `<div class="ws" data-roster-row="${id}"><div class="row">` +
    `<span class="sb-gutter"></span><span class="sb-num">1</span>` +
    `<span class="st tone-green" data-glyph="dot"></span><span class="name">${id}</span>` +
    `</div>${box}</div>`
  );
}

/** A repository's rows: a top-level family, then a subsection of two rows. */
const RAIL =
  `<div class="rows">${ws("parent", ws("child", ws("grandchild")))}` +
  `<div class="sb-subsection"><div class="sb-subsection-head"><span class="sb-subsection-label">Other</span></div>` +
  `<div class="rows" data-subsection="s">${ws("member", ws("member-child"))}${ws("member-2")}</div></div></div>`;

/** Where each row's mark centers and where each guide line centers. */
interface Measured {
  marks: Record<string, number>;
  guides: Record<string, number>;
}

/** Draw the rail and measure every mark's center and every guide's center. */
async function measure(page: Page): Promise<Measured> {
  return page.evaluate((markup) => {
    const host = document.getElementById("ws-sidebar");
    if (host === null) throw new Error("the page's rail did not mount");
    host.innerHTML = markup;
    const marks: Record<string, number> = {};
    for (const row of host.querySelectorAll<HTMLElement>("[data-roster-row]")) {
      const st = row.querySelector(":scope > .row > .st") as HTMLElement;
      const box = st.getBoundingClientRect();
      marks[row.dataset.rosterRow ?? ""] = box.left + box.width / 2;
    }
    const guideOf = (el: HTMLElement): number => {
      const line = getComputedStyle(el, "::before");
      return el.getBoundingClientRect().left + parseFloat(line.left) + parseFloat(line.borderLeftWidth) / 2;
    };
    const guides: Record<string, number> = {};
    for (const kids of host.querySelectorAll<HTMLElement>(".kids")) {
      guides[(kids.parentElement as HTMLElement).dataset.rosterRow ?? ""] = guideOf(kids);
    }
    guides["subsection"] = guideOf(host.querySelector(".sb-subsection > .rows") as HTMLElement);
    return { marks, guides };
  }, RAIL);
}

describe("the family guide in WebKit", () => {
  let browser: Browser;
  let page: Page;
  let measured: Measured;

  beforeAll(async () => {
    const css = await pageStylesheet();
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 400, height: 400 } });
    await page.setContent(`<!doctype html><html><head></head><body><div id="ws-sidebar"></div></body></html>`);
    await page.addStyleTag({ content: css });
    measured = await measure(page);
  });

  afterAll(async () => {
    await browser.close();
  });

  it.each(["parent", "child", "member"])("runs the guide under %s's children down the middle of its mark", (parent) => {
    expect(Math.abs((measured.guides[parent] ?? NaN) - (measured.marks[parent] ?? NaN))).toBeLessThanOrEqual(0.5);
  });

  it("runs a subsection's guide where a top-level row's mark stands", () => {
    expect(Math.abs((measured.guides["subsection"] ?? NaN) - (measured.marks["parent"] ?? NaN))).toBeLessThanOrEqual(0.5);
  });

  it("indents a subsection's top-level rows as a parent's children", () => {
    expect([measured.marks["member"], measured.marks["member-2"]]).toEqual([measured.marks["child"], measured.marks["child"]]);
  });

  it("indents a subsection's row's children as a grandchild", () => {
    expect(measured.marks["member-child"]).toBe(measured.marks["grandchild"]);
  });
});
