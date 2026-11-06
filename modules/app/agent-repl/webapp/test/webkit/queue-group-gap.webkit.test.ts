/**
 * A QUEUE GROUP STANDS OFF THE NEXT ONE BY --inside-rule-gap, IN WEBKIT
 * (owner request, 2026-11-06).
 *
 * Under the Merge Queue header, the gap from an open group's last line to the
 * next group's name equals the gap from a group's name to the first line of
 * its queue, whatever the queue ends with: the head card's step line, its
 * stage strip, its name line alone, a remediating card's line, or a waiting
 * merge's line. A gap is measured text to text, as the rules between the
 * sections are: the upper line's baseline to the lower line's capital top.
 * jsdom lays nothing out, so this draws the section in the shape
 * `drawRosterMergeQueueRepoGroup` draws it (its classes pinned in
 * test/sidebar/roster.test.ts) in real headless WebKit over the real
 * stylesheet, and measures the lines.
 */
import { webkit, type Browser, type Page } from "playwright-core";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { pageStylesheet } from "./stylesheets";

/** The head card's merge glyph, as the sidebar draws it. */
const GLYPH = `<span class="sb-mq-glyph" aria-hidden="true">⑂</span>`;

/** A head-line: the glyph, a merge's NAME and its clock. */
function headLine(name: string): string {
  return `<div class="sb-mq-head-line">${GLYPH}<span class="sb-mq-name">${name}</span><span class="sb-mq-clock">4m</span></div>`;
}

/** The head card, with a stage strip when STAGES and a step line when DETAIL, as `drawRosterMergeQueueHead` draws it. */
function head(name: string, stages: boolean, detail: string | null): string {
  const strip = stages
    ? `<div class="sb-mq-stages"><span class="sb-mq-stage" data-state="succeeded"><span class="sb-mq-stage-glyph">✓</span><span class="sb-mq-stage-label">tests</span></span><span class="sb-mq-stage" data-state="live"><span class="sb-mq-stage-glyph">●</span><span class="sb-mq-stage-label">deploy</span></span></div>`
    : "";
  const step = detail === null ? "" : `<div class="sb-mq-detail">${detail}</div>`;
  return `<div class="sb-mq-head" data-standing="merging">${headLine(name)}${strip}${step}</div>`;
}

/** A remediating card, as `drawRosterMergeQueueRemediating` draws it. */
function remediating(name: string): string {
  return `<div class="sb-mq-remediating">${headLine(name)}<div class="sb-mq-detail">remediating tests</div></div>`;
}

/** A waiting merge's line, as `drawRosterMergeQueueWaiting` draws it. */
function waiting(name: string): string {
  return `<div class="sb-mq-waiting"><span class="sb-mq-name">${name}</span><span class="sb-mq-clock">1m</span></div>`;
}

/** One queue group named NAME around its queue body BODY. */
function group(name: string, body: string): string {
  return `<div class="merged-group" data-group-section="merge-queue" data-repository="${name}"><div class="merged-group-head"><span class="sb-label">${name}</span></div><div class="rows sb-merge-queue">${body}</div></div>`;
}

/** The Merge Queue section holding GROUPS, in the rail's shape. */
function rail(groups: string): string {
  return `<nav id="ws-sidebar" style="width:260px;height:600px"><div class="sb-scroll"><div class="sb-roster"><div class="sb-pane"><div class="repo merge-queue-section"><div class="repo-head"><span class="sb-label">Merge Queue</span></div><div class="rows">${groups}</div></div></div></div></div></nav>`;
}

/** What one draw measured, in px, text to text. */
interface Gaps {
  /** From the first group's name to the first line of its queue. */
  under: number;
  /** From the first group's last line to the second group's name. */
  between: number;
}

/**
 * Draw two groups, the first's queue BODY, and measure. A line's baseline is
 * where a zero-height inline block set in its text stands; its capital top
 * is the baseline less its font's capital height (canvas `measureText`).
 */
async function measure(page: Page, body: string): Promise<Gaps> {
  const html = rail(group("explanation-engine", body) + group("doom", head("next-up", false, "next")));
  return page.evaluate((markup) => {
    document.body.innerHTML = markup;
    const ctx = document.createElement("canvas").getContext("2d");
    if (ctx === null) throw new Error("the page drew no canvas context");
    const baseline = (el: Element): number => {
      const probe = document.createElement("span");
      probe.style.cssText = "display:inline-block;width:0;height:0;vertical-align:baseline";
      el.appendChild(probe);
      const at = probe.getBoundingClientRect().top;
      probe.remove();
      return at;
    };
    const capTop = (el: Element): number => {
      const c = getComputedStyle(el);
      ctx.font = `${c.fontStyle} ${c.fontWeight} ${c.fontSize} ${c.fontFamily}`;
      return baseline(el) - ctx.measureText("H").actualBoundingBoxAscent;
    };
    // A line's text: a name or a step, or a stage's label on a strip.
    const text = (line: Element): Element => {
      const own = line.matches(".sb-mq-detail, .sb-mq-stages") ? line : line.querySelector(".sb-mq-name");
      const found = line.matches(".sb-mq-stages") ? line.querySelector(".sb-mq-stage-label") : own;
      if (found === null) throw new Error(`a queue line drew no text: ${line.className}`);
      return found;
    };
    const [first, second] = [...document.querySelectorAll(".merged-group")];
    const queue = first.querySelector(".sb-merge-queue") as HTMLElement;
    const lines = [...queue.querySelectorAll(".sb-mq-head-line, .sb-mq-stages, .sb-mq-detail, .sb-mq-waiting")];
    const label = (g: Element): Element => g.querySelector(".merged-group-head > .sb-label") as Element;
    return {
      under: capTop(text(lines[0])) - baseline(label(first)),
      between: capTop(label(second)) - baseline(text(lines[lines.length - 1])),
    };
  }, html);
}

/** Every line a queue group can end with, each as the last line of a queue. */
const ENDINGS = [
  { ends: "the head card's step line", body: head("cee-head", true, "deploying") },
  { ends: "the head card's stage strip", body: head("cee-head", true, null) },
  { ends: "the head card's name line", body: head("cee-head", false, null) },
  { ends: "a remediating card's line", body: head("cee-head", true, "deploying") + remediating("fixing") },
  { ends: "a waiting merge's line", body: head("cee-head", true, "deploying") + waiting("behind") },
];

describe("a queue group's gap to the next group in WebKit", () => {
  let browser: Browser;
  let page: Page;

  beforeAll(async () => {
    const css = await pageStylesheet();
    browser = await webkit.launch();
    page = await browser.newPage({ viewport: { width: 400, height: 700 } });
    await page.setContent(`<!doctype html><html><head></head><body></body></html>`);
    await page.addStyleTag({ content: css });
  });

  afterAll(async () => {
    await browser.close();
  });

  it.each(ENDINGS)("stands the next group's name off $ends by the gap under a group's name", async ({ body }) => {
    const m = await measure(page, body);
    expect(m.between).toBeCloseTo(m.under, 0);
  });

  it("leaves the section's last group's last card at its own reach", async () => {
    await measure(page, head("cee-head", true, "deploying"));
    const margin = await page.evaluate(() => {
      const last = document.querySelector(".merged-group:last-child > .sb-merge-queue > :last-child") as Element;
      return getComputedStyle(last).marginBottom;
    });
    expect(margin).toBe("0px");
  });
});
