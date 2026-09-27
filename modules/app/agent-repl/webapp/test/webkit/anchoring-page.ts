/**
 * THE PAGE THE WEBKIT ANCHORING TEST DRIVES (anchoring.webkit.test.ts).
 *
 * Bundled by that test into one script and run in headless WebKit, over the
 * REAL stylesheet (src/styles.css, injected by the test) and the REAL feed
 * scroll wiring: the tail owner, the box observer and the overscan band, built
 * exactly as `mountFeed` builds them (src/feed/feed.ts). Only the rows are
 * synthetic: `.feed-item` wrappers holding one block of a fixed, varied height,
 * so a row's first layout changes its height from the stylesheet's
 * `contain-intrinsic-size` guess to a known real one.
 *
 * WHAT IT MEASURES IS WHAT IS PAINTED. A frame's sample is taken in a
 * ResizeObserver created AFTER the feed's own, whose target is resized on
 * every animation frame: the rendering update runs the scroll events, the
 * animation frames, layout, then the resize observers in creation order, and
 * only then paints. So the sample sees the layout the feed's own corrections
 * left, which is the frame the reader sees. Reading layout anywhere else -- a
 * task between two frames -- sees growth an IntersectionObserver callback
 * has just caused but no rendering update has corrected yet, a state that is
 * never painted.
 */
import { createOverscan } from "../../src/feed/overscan.js";
import { ForwardingLogger, bindLogContext, setLogger } from "../../src/log.js";
import { TailFollow, feedAnchorRows, observeScrollBox, revealGeometry } from "../../src/scroll.js";

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
  // As mountFeed builds it: the tail owner reading the latest entry and
  // anchoring on the feed's rows, the box observer, and the overscan band
  // watching every row.
  const tail = new TailFollow(
    box,
    () => {
      const last = host.lastElementChild;
      return last === null ? null : revealGeometry(box, last);
    },
    feedAnchorRows(box, host),
  );
  observeScrollBox(box, tail);
  const overscan = createOverscan(box);
  if (overscan === null) throw new Error("this engine ships no IntersectionObserver");
  for (const row of host.children) overscan.observe(row as HTMLElement);
  return { box, host, tail };
}

/**
 * A painted-frame clock: each call resolves with READ's answer, taken in the
 * resize-observer phase of the next rendering update, after the feed's own.
 */
function paintedFrames(): <T>(read: () => T) => Promise<T> {
  const sentinel = document.createElement("div");
  sentinel.style.cssText = "position:fixed;left:0;top:0;height:1px;width:1px;visibility:hidden";
  document.body.append(sentinel);
  let waiting: (() => void) | null = null;
  const observer = new ResizeObserver(() => {
    const next = waiting;
    waiting = null;
    next?.();
  });
  observer.observe(sentinel);
  let wide = false;
  return <T>(read: () => T) =>
    new Promise<T>((resolve) => {
      requestAnimationFrame(() => {
        wide = !wide;
        sentinel.style.width = wide ? "2px" : "1px";
        waiting = () => resolve(read());
      });
    });
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

/**
 * The webapp's own logger, at DEBUG. Its console line -- written for every
 * record, before the forwarding throttle -- is what this page keeps for the
 * test to read, as `level operation`; the forwarding sink accepts and drops.
 */
const written: string[] = [];
setLogger(
  new ForwardingLogger(
    () => Promise.resolve("accepted"),
    (level, line) => {
      const { operation } = JSON.parse(line) as { operation: string };
      written.push(`${level} ${operation}`);
    },
    {},
    "debug",
  ),
);
bindLogContext({ connection_id: "webkit-anchoring-page" });

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
  records: () => [...written],
};
