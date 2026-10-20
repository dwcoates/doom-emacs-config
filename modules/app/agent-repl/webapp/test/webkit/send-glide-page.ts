/**
 * THE PAGE THE WEBKIT SEND-GLIDE TEST DRIVES (send-glide.webkit.test.ts): the
 * real glide over a real sent prompt parked at the tail of the feed shell.
 */
import { glideSentPrompt } from "../../src/feed/send-glide.js";
import { installPageLogger } from "./page-log";

installPageLogger("send-glide");

/** What the scroll box and the bubble measured at one moment of the glide. */
export interface GlideSample {
  /** The scroll box's `scrollHeight`. */
  scrollHeight: number;
  /** How far the bubble's top sits below its row's top, px. */
  bubbleBelowRow: number;
}

/** What the test calls, on `window.sendGlide`. */
export interface SendGlidePage {
  /** The scroll box's `scrollHeight` with the prompt parked and nothing gliding. */
  restingScrollHeight(): number;
  /** Glide the parked prompt, and sample it at each of AT_MS into the glide. */
  sampleDuring(atMs: readonly number[]): GlideSample[];
}

declare global {
  interface Window {
    sendGlide: SendGlidePage;
  }
}

/** The feed shell's scroll box, parked at its tail. */
function parkedBox(): HTMLElement {
  const box = document.getElementById("feed-scroll");
  if (box === null) throw new Error("send-glide page: the feed shell has no #feed-scroll");
  box.scrollTop = box.scrollHeight;
  return box;
}

/** The sent prompt's row and its bubble. */
function prompt(): { row: HTMLElement; bubble: HTMLElement } {
  const row = document.querySelector<HTMLElement>('[data-feed-row="sent"]');
  const bubble = row?.firstElementChild;
  if (!(bubble instanceof HTMLElement)) throw new Error("send-glide page: the sent prompt is not drawn");
  return { row: row as HTMLElement, bubble };
}

// THE GLIDE IS SAMPLED AT ITS OWN TIMES, never after a real wait: it is paused
// once it starts and seeked to each time in turn (collapse-motion-page.ts).
window.sendGlide = {
  restingScrollHeight() {
    return parkedBox().scrollHeight;
  },
  sampleDuring(atMs) {
    const box = parkedBox();
    const { row, bubble } = prompt();
    glideSentPrompt(row, bubble);
    const glide = bubble.getAnimations()[0];
    if (glide === undefined) throw new Error("send-glide page: the glide never started");
    glide.pause();
    return atMs.map((at) => {
      glide.currentTime = at;
      return {
        scrollHeight: box.scrollHeight,
        bubbleBelowRow: Math.round(bubble.getBoundingClientRect().top - row.getBoundingClientRect().top),
      };
    });
  },
};
