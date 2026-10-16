/**
 * THE PAGE THE WEBKIT PINNED-TAIL TEST DRIVES (pinned-tail.webkit.test.ts).
 *
 * Bundled by that test into one script and run in headless WebKit, over the
 * REAL stylesheet and the REAL feed scroll wiring — the tail owner and the box
 * observer, built exactly as `mountFeed` builds them —
 * with the page's shell column: the scroll zone holding the feed and its
 * (empty) held section, and the footer below it. The rows are synthetic blocks
 * of known height, the last of them a merge bubble's row, as the row pinned to
 * the feed's bottom (FeedRowOrder.pinned) is drawn.
 *
 * WHAT IT MEASURES IS WHAT IS PAINTED: every sample is a painted frame's
 * (`paintedFrames`, painted-frames.ts), the frame the reader sees.
 */
import { ForwardingLogger, bindLogContext, setLogger } from "../../src/log.js";
import { observeScrollBox } from "../../src/scroll.js";
import { createFeedTail, latestEntry } from "../../src/feed/feed.js";
import { paintedFrames } from "./painted-frames";
import { MERGE_BUBBLE_ATTRIBUTE } from "../../src/feed/merge-bubble.js";

/** What one growth saw across the frames it painted. */
export interface GrowthResult {
  /** The gap between the latest entry's bottom and the box's, before the growth. */
  before: number;
  /** That gap in every frame painted after it. */
  after: number[];
}

/** What the test calls, on `window.pinnedTail`. */
export interface PinnedTailPage {
  /** Park the feed at its tail, as a first paint does, and let it settle. */
  park(): Promise<void>;
  /** Grow the pinned merge bubble by PX in a task, then sample FRAMES painted frames. */
  growMerge(px: number, frames: number): Promise<GrowthResult>;
  /** Grow the footer by PX in a task, then sample FRAMES painted frames. */
  growFooter(px: number, frames: number): Promise<GrowthResult>;
}

declare global {
  interface Window {
    pinnedTail: PinnedTailPage;
  }
}

setLogger(new ForwardingLogger(() => Promise.resolve("accepted"), () => undefined, {}, "info"));
bindLogContext({ connection_id: "webkit-pinned-tail-page" });

document.body.innerHTML = `
  <div id="main-col" style="height:100vh">
    <div id="feed-scroll" class="scroll-zone">
      <main id="feed" data-feed="root"><div class="feed-body"><div class="feed-rows"></div></div></main>
      <section id="hold-tray" data-component="hold-tray"></section>
    </div>
    <footer id="footer"><div id="foot" style="height:40px">footer</div></footer>
  </div>`;
const box = document.getElementById("feed-scroll") as HTMLElement;
const rows = document.querySelector(".feed-rows") as HTMLElement;
const foot = document.getElementById("foot") as HTMLElement;
for (let i = 0; i < 40; i++) {
  const row = document.createElement("article");
  row.className = "feed-item";
  row.dataset.feedRow = `r${i.toString()}`;
  const card = document.createElement("div");
  card.style.height = `${(80 + ((i * 97) % 300)).toString()}px`;
  card.textContent = `row ${i.toString()}`;
  row.append(card);
  rows.append(row);
}
// The merge bubble's row, pinned below every other row.
const pinned = document.createElement("article");
pinned.className = "feed-item";
pinned.dataset.feedRow = "m1";
const merge = document.createElement("div");
merge.setAttribute(MERGE_BUBBLE_ATTRIBUTE, "");
merge.style.height = "300px";
merge.textContent = "merge";
pinned.append(merge);
rows.append(pinned);
const tail = createFeedTail(box, rows);
observeScrollBox(box, tail);

const frame = paintedFrames();

/** The gap between the latest entry's bottom and the box's bottom. */
const gap = (): number => {
  const entry = latestEntry(box);
  if (entry === null) throw new Error("nothing is drawn");
  return Math.round(box.getBoundingClientRect().bottom - entry.getBoundingClientRect().bottom);
};

async function growth(change: () => void, frames: number): Promise<GrowthResult> {
  const before = await frame(gap);
  await new Promise<void>((resolve) =>
    setTimeout(() => {
      change();
      resolve();
    }, 0),
  );
  const after: number[] = [];
  for (let i = 0; i < frames; i++) after.push(await frame(gap));
  return { before, after };
}

window.pinnedTail = {
  async park() {
    tail.initialPlacement();
    for (let i = 0; i < 8; i++) await frame(() => null);
  },
  growMerge: (px, frames) =>
    growth(() => {
      merge.style.height = `${(merge.offsetHeight + px).toString()}px`;
    }, frames),
  growFooter: (px, frames) =>
    growth(() => {
      foot.style.height = `${(foot.offsetHeight + px).toString()}px`;
    }, frames),
};
