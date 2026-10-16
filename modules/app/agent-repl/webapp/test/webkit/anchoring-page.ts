/**
 * THE PAGE THE WEBKIT ANCHORING TEST DRIVES (anchoring.webkit.test.ts).
 *
 * Bundled by that test into one script and run in headless WebKit, over the
 * REAL stylesheet (src/styles.css, injected by the test) and the REAL feed
 * scroll wiring: the tail owner and the box observer, built exactly as
 * `mountFeed` builds them (src/feed/feed.ts). Only the rows are synthetic:
 * `.feed-item` wrappers holding one block of a fixed, varied height, which a
 * test can grow to stand in for a row above the reader changing height.
 *
 * WHAT IT MEASURES IS WHAT IS PAINTED: every sample is a painted frame's
 * (`paintedFrames`, painted-frames.ts).
 */
import { installPageLogger } from "./page-log";
import { observeScrollBox, type TailFollow } from "../../src/scroll.js";
import { createFeedTail } from "../../src/feed/feed.js";
import { paintedFrames } from "./painted-frames";

/** The rows the page draws, and the real height of row I (60px to 759px). */
const ROWS = 400;
const rowHeight = (i: number): number => 60 + ((i * 97) % 700);

/** What one reader step saw across the frames it painted. */
export interface StepResult {
  /** The largest painted deviation of the tracked row from where the reader put it. */
  worst: number;
  /** The box's scrollTop once the step settled. */
  top: number;
}

/** What the test calls, on `window.anchoring`. */
export interface AnchoringPage {
  /** Park the feed at its tail, as a first paint does, and let it settle. */
  park(): Promise<void>;
  /** One reader gesture of PX upward, then FRAMES painted frames of measurement. */
  stepUp(px: number, frames: number): Promise<StepResult>;
  /** Grow the row just above the viewport by PX, then FRAMES painted frames of measurement. */
  growAbove(px: number, frames: number): Promise<StepResult>;
  /** Every record the page's logger has written so far, as `level operation`. */
  records(): string[];
}

declare global {
  interface Window {
    anchoring: AnchoringPage;
  }
}

function mount(): { box: HTMLElement; host: HTMLElement; tail: TailFollow } {
  // The real shell's column (index.html), minus the components this page does
  // not mount: the scroll zone takes the column's whole height.
  document.body.innerHTML = `
    <div id="main-col">
      <div id="feed-scroll" class="scroll-zone">
        <main id="feed" data-feed="root"></main>
        <section id="hold-tray" data-component="hold-tray"></section>
      </div>
    </div>`;
  const box = document.getElementById("feed-scroll");
  const host = document.getElementById("feed");
  if (box === null || host === null) throw new Error("the page's shell did not mount");
  for (let i = 0; i < ROWS; i++) {
    const item = document.createElement("div");
    item.className = "feed-item";
    item.dataset.feedRow = `r${i.toString()}`;
    const card = document.createElement("div");
    card.style.height = `${rowHeight(i).toString()}px`;
    card.textContent = `row ${i.toString()}`;
    item.append(card);
    host.append(item);
  }
  // As mountFeed builds it: the feed's own tail owner, and the box observer.
  const tail = createFeedTail(box, host);
  observeScrollBox(box, tail);
  return { box, host, tail };
}

/** The row under the viewport's top plus OFFSET, or the first one below it. */
function rowAt(box: HTMLElement, host: HTMLElement, offset: number): HTMLElement {
  const y = box.getBoundingClientRect().top + offset;
  let below: HTMLElement | null = null;
  for (const row of host.children) {
    const r = row.getBoundingClientRect();
    if (r.top <= y && r.bottom >= y) return row as HTMLElement;
    if (below === null && r.top > y) below = row as HTMLElement;
  }
  if (below === null) throw new Error(`no row at or below ${y.toString()}px`);
  return below;
}

/** The last row whose bottom sits above the viewport's top. */
function rowAbove(box: HTMLElement, host: HTMLElement): HTMLElement {
  const top = box.getBoundingClientRect().top;
  let above: HTMLElement | null = null;
  for (const row of host.children) {
    if (row.getBoundingClientRect().bottom > top) break;
    above = row as HTMLElement;
  }
  if (above === null) throw new Error("no row sits wholly above the viewport");
  return above;
}

const records = installPageLogger("anchoring");

const { box, host, tail } = mount();
const frame = paintedFrames();

window.anchoring = {
  async park() {
    tail.initialPlacement();
    for (let i = 0; i < 8; i++) await frame(() => null);
  },
  async stepUp(px, frames) {
    // Where the tracked row is PAINTED, and the gesture, in that same frame.
    const { tracked, before, want } = await frame(() => {
      const row = rowAt(box, host, 400);
      const from = box.scrollTop;
      const at = row.getBoundingClientRect().top;
      // The reader's own input precedes the movement it causes, as a real
      // wheel does; the tail owner reads it as the reader's (READER_INPUTS).
      box.dispatchEvent(new WheelEvent("wheel", { deltaY: -px, bubbles: true }));
      box.scrollTop = from - px;
      return { tracked: row, before: at, want: Math.min(px, from) };
    });
    let worst = 0;
    for (let i = 0; i < frames; i++) {
      const at = await frame(() => tracked.getBoundingClientRect().top);
      worst = Math.max(worst, Math.abs(at - before - want));
    }
    return { worst, top: box.scrollTop };
  },
  async growAbove(px, frames) {
    // Where the tracked row is PAINTED, and the growth, in that same frame.
    const { tracked, before } = await frame(() => {
      const row = rowAt(box, host, 400);
      const at = row.getBoundingClientRect().top;
      const card = rowAbove(box, host).firstElementChild as HTMLElement;
      card.style.height = `${(card.offsetHeight + px).toString()}px`;
      return { tracked: row, before: at };
    });
    let worst = 0;
    for (let i = 0; i < frames; i++) {
      const at = await frame(() => tracked.getBoundingClientRect().top);
      worst = Math.max(worst, Math.abs(at - before));
    }
    return { worst, top: box.scrollTop };
  },
  records,
};
