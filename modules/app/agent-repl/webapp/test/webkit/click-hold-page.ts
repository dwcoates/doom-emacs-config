/**
 * THE PAGE THE WEBKIT CLICK-HOLD TEST DRIVES (click-hold.webkit.test.ts).
 *
 * Bundled by that test into one script and run in headless WebKit, over the
 * REAL stylesheet and the REAL feed scroll wiring: the feed's tail owner
 * (`createFeedTail`), the box observer, and the click hold on the gate dock
 * (`holdViewOnClicks`), as `mountFeed` wires them. The page's shell is the
 * body grid's: the main column holding the scroll zone, and the gate dock in
 * the row under it at the input window's height. The rows are synthetic
 * blocks of known height, the last of them a card holding an option button,
 * as a question or permission card at the feed's tail is drawn.
 *
 * WHAT IT MEASURES IS WHAT IS PAINTED: every sample is a painted frame's
 * (`paintedFrames`, painted-frames.ts), the frame the reader sees.
 */
import { installPageLogger } from "./page-log";
import { holdViewOnClicks, observeScrollBox } from "../../src/scroll.js";
import { createFeedTail } from "../../src/feed/feed.js";
import { paintedFrames } from "./painted-frames";

/** The rows the page draws above the tail card, each this tall. */
const ROWS = 30;
const ROW_PX = 120;
/** The tail card's height before an answer shrinks it. */
const CARD_PX = 400;
/** The gate dock's height: the input window's, as Emacs tells it. */
export const DOCK_PX = 210;

/** What the test calls, on `window.clickHold`. */
export interface ClickHoldPage {
  /** Park the feed at its tail, as a first paint does, and let it settle. */
  park(): Promise<void>;
  /**
   * The reader answers the tail card (CLICK) or does not, and its redraw
   * lands a task later, PX shorter. Answers the largest painted move of the
   * card's top across FRAMES frames.
   */
  answerTailCard(click: boolean, px: number, frames: number): Promise<number>;
  /**
   * The reader answers the docked gate: the dock hides a task later, and the
   * page then shrinks by the dock's height as Emacs shows the input window
   * again. Answers the largest painted move of the tail card's top across
   * FRAMES frames after each.
   */
  answerDockedGate(frames: number): Promise<number>;
  /** Every record the page's logger has written so far, as `level operation`. */
  records(): string[];
}

declare global {
  interface Window {
    clickHold: ClickHoldPage;
  }
}

const records = installPageLogger("click-hold");

document.documentElement.style.setProperty("--gate-dock-height", `${DOCK_PX.toString()}px`);
document.body.innerHTML = `
  <div id="main-col">
    <div id="feed-scroll" class="scroll-zone">
      <main id="feed" data-feed="root"></main>
      <section id="hold-tray" data-component="hold-tray"></section>
    </div>
  </div>
  <div id="gate-dock" data-component="gate-dock"><button id="gate-choice">pay</button></div>`;
const box = document.getElementById("feed-scroll");
const host = document.getElementById("feed");
const dock = document.getElementById("gate-dock");
const choice = document.getElementById("gate-choice");
if (box === null || host === null || dock === null || choice === null) {
  throw new Error("the page's shell did not mount");
}
for (let i = 0; i < ROWS; i++) {
  const item = document.createElement("div");
  item.className = "feed-item";
  item.dataset.feedRow = `r${i.toString()}`;
  const block = document.createElement("div");
  block.style.height = `${ROW_PX.toString()}px`;
  block.textContent = `row ${i.toString()}`;
  item.append(block);
  host.append(item);
}
const tailRow = document.createElement("div");
tailRow.className = "feed-item";
tailRow.dataset.feedRow = "ask";
const card = document.createElement("div");
card.style.height = `${CARD_PX.toString()}px`;
const option = document.createElement("button");
option.textContent = "1. yes";
card.append(option);
tailRow.append(card);
host.append(tailRow);

const tail = createFeedTail(box, host);
observeScrollBox(box, tail);
holdViewOnClicks(dock, tail);
const frame = paintedFrames();

/** Run CHANGE in a task of its own, as a daemon push lands after the click. */
const later = (change: () => void): Promise<void> =>
  new Promise<void>((resolve) =>
    setTimeout(() => {
      change();
      resolve();
    }, 0),
  );

/** The largest painted move of the tail card's top from BEFORE across FRAMES frames. */
async function worstMove(before: number, frames: number): Promise<number> {
  let worst = 0;
  for (let i = 0; i < frames; i++) {
    const at = await frame(() => tailRow.getBoundingClientRect().top);
    worst = Math.max(worst, Math.abs(at - before));
  }
  return worst;
}

window.clickHold = {
  async park() {
    tail.initialPlacement();
    for (let i = 0; i < 8; i++) await frame(() => null);
  },
  async answerTailCard(click, px, frames) {
    const before = await frame(() => {
      const at = tailRow.getBoundingClientRect().top;
      if (click) option.click();
      return at;
    });
    await later(() => {
      card.style.height = `${(CARD_PX - px).toString()}px`;
    });
    return worstMove(before, frames);
  },
  async answerDockedGate(frames) {
    const before = await frame(() => {
      const at = tailRow.getBoundingClientRect().top;
      choice.click();
      return at;
    });
    await later(() => {
      dock.hidden = true;
    });
    const hidden = await worstMove(before, frames);
    await later(() => {
      document.body.style.height = `calc(100vh - ${DOCK_PX.toString()}px)`;
    });
    return Math.max(hidden, await worstMove(before, frames));
  },
  records,
};
