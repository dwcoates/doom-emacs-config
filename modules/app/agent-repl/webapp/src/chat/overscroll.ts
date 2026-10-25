/**
 * overscroll — the chat's pull past its end (owner request, 2026-10-25).
 *
 * The chat cannot scroll past its end plus its leeway (scroll.ts
 * `CHAT_LEEWAY_FRACTION`): the box's extent stops there. A wheel that keeps
 * pushing down at that limit PULLS the drawn content up a little, with
 * mounting resistance, and lets it spring back once the wheel goes quiet:
 * the elastic edge the platform draws for a scroll view. The pull moves
 * paint only (a transform), so the scroll position never leaves its bounds.
 */
import { log } from "../log.js";

/** How long the wheel must be quiet before the pull springs back. */
export const OVERSCROLL_RELEASE_MS = 120;

/** The spring back: a quick ease out, as the platform's. */
export const OVERSCROLL_SPRING = "transform 380ms cubic-bezier(0.22, 1, 0.36, 1)";

/** The rubber band's stiffness: the platform's own constant. */
const RUBBER_BAND_C = 0.55;

/**
 * How far a pull of PUSHED wheel pixels draws, against a box HEIGHT tall:
 * the rubber band curve, which follows the push at first and never reaches
 * the box's height however far it is pushed.
 */
export function rubberBand(pushed: number, height: number): number {
  if (pushed <= 0 || height <= 0) return 0;
  return (1 - 1 / ((pushed * RUBBER_BAND_C) / height + 1)) * height;
}

/** The timer a pull's release waits on (a seam for tests). */
export interface OverscrollTimers {
  setTimeout(fn: () => void, ms: number): number;
  clearTimeout(id: number): void;
}

/**
 * Pull CONTENT when a wheel pushes SCROLL past the end it can reach. Answers
 * the listener's teardown.
 */
export function installChatOverscroll(scroll: HTMLElement, content: HTMLElement, timers: OverscrollTimers = window): () => void {
  let pushed = 0;
  let release = 0;
  const draw = (): void => {
    const pull = rubberBand(pushed, scroll.clientHeight);
    content.style.transform = pull === 0 ? "" : `translateY(${-pull}px)`;
  };
  const springBack = (): void => {
    release = 0;
    if (pushed === 0) return;
    log.debug("the chat's pull past its end springs back", { operation: "chat.overscroll.release", context: { pushed: Math.round(pushed) } });
    pushed = 0;
    content.style.transition = OVERSCROLL_SPRING;
    draw();
  };
  const onWheel = (event: WheelEvent): void => {
    const atLimit = scroll.scrollTop >= scroll.scrollHeight - scroll.clientHeight - 1;
    if (pushed === 0 && !(atLimit && event.deltaY > 0)) return;
    pushed = Math.max(0, pushed + event.deltaY);
    content.style.transition = "";
    draw();
    if (release !== 0) timers.clearTimeout(release);
    release = timers.setTimeout(springBack, OVERSCROLL_RELEASE_MS);
  };
  scroll.addEventListener("wheel", onWheel, { passive: true });
  return () => {
    scroll.removeEventListener("wheel", onWheel);
    if (release !== 0) timers.clearTimeout(release);
    content.style.transform = "";
    content.style.transition = "";
  };
}
