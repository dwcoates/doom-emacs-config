/**
 * THE ONE FAKE OF A TAIL OWNER'S THREE EVENT SOURCES. A test builds a
 * `TailFollow` over a plain box object, which dispatches no events, so it
 * subscribes the owner here and fires each source by hand.
 *
 * `gesture` is the READER moving the box: their input reaches it first and
 * the scroll event follows, which is the order the browser delivers them in
 * and the order the owner's attribution rule depends on. `scroll` alone is
 * therefore a movement with no reader behind it -- the box's own clamp.
 */
import type { ReaderInput, TailFollow } from "../src/scroll.js";

export interface TailTriggers {
  /** The box's scroll event. */
  scroll: () => void;
  /** The box's resize pass. */
  resize: () => void;
  /** A reader input reached the box (a wheel unless KIND says otherwise). */
  input: (kind?: ReaderInput) => void;
  /** The reader's own movement: an input, then the scroll event. */
  gesture: () => void;
}

/** Subscribe TAIL to hand-fired event sources and answer them. */
export function tailTriggers(tail: TailFollow): TailTriggers {
  let onScroll = (): void => {};
  let onResize = (): void => {};
  let onInput = (_kind: ReaderInput): void => {};
  tail.observe(
    (cb) => {
      onScroll = cb;
    },
    (cb) => {
      onResize = cb;
    },
    (cb) => {
      onInput = cb;
    },
  );
  return {
    scroll: () => onScroll(),
    resize: () => onResize(),
    input: (kind = "wheel") => onInput(kind),
    gesture: () => {
      onInput("wheel");
      onScroll();
    },
  };
}
