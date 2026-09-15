// @vitest-environment jsdom
// The driver that keeps the prompt bubble's thinking wave running while the
// webview reports itself hidden — WebKit suspends the compositor animation
// then, so a JS timer hand-paints `background-position-x` instead. See
// src/prompt-wave-driver.ts for the whole account.
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  PROMPT_WAVE_ATTRIBUTE,
  PROMPT_WAVE_WORKING,
  bubbleWave,
} from "../src/breathing.js";
import {
  HIDDEN_TICK_MS,
  PAGE_HIDDEN_ATTRIBUTE,
  createPromptWaveDriver,
  type PromptWaveDriver,
} from "../src/prompt-wave-driver.js";

/** The container this test's bubbles live in, torn down after each case. */
let root: HTMLElement;
let driver: PromptWaveDriver | null;

/** Override the shared document's visibility, restored in `afterEach`. */
function setVisibility(state: "visible" | "hidden"): void {
  Object.defineProperty(document, "visibilityState", {
    configurable: true,
    get: () => state,
  });
  document.dispatchEvent(new Event("visibilitychange"));
}

/** A prompt bubble carrying the in-flight mark the driver paints. */
function workingBubble(): HTMLElement {
  const bubble = document.createElement("div");
  bubble.className = "bubble user";
  bubble.setAttribute(PROMPT_WAVE_ATTRIBUTE, PROMPT_WAVE_WORKING);
  root.append(bubble);
  return bubble;
}

/** A settled prompt bubble — on screen, but its turn is over, so it is unmarked. */
function settledBubble(): HTMLElement {
  const bubble = document.createElement("div");
  bubble.className = "bubble user";
  root.append(bubble);
  return bubble;
}

beforeEach(() => {
  vi.useFakeTimers();
  root = document.createElement("div");
  document.body.append(root);
  driver = null;
});

afterEach(() => {
  driver?.stop();
  vi.useRealTimers();
  root.remove();
  document.documentElement.removeAttribute(PAGE_HIDDEN_ATTRIBUTE);
  // Own accessor deleted, so the prototype getter ("prerender") is back.
  delete (document as { visibilityState?: unknown }).visibilityState;
});

describe("the hidden-page prompt-wave driver", () => {
  it("advances the band across ticks while the page is hidden", () => {
    // Arrange — a waving bubble on a page that is already hidden, and a clock
    // the driver reads so the phase is ours to move.
    let clock = Date.now();
    const bubble = workingBubble();
    setVisibility("hidden");
    driver = createPromptWaveDriver({ root, now: () => clock, reducedMotion: () => false });
    driver.start();
    const first = bubble.style.backgroundPositionX;

    // Act — a later tick fires against a moved clock (half a period on, so the
    // linear pass is at a different point no matter where the epoch stands).
    clock += 1600;
    vi.advanceTimersByTime(HIDDEN_TICK_MS);
    const second = bubble.style.backgroundPositionX;

    // Assert — the band moved, and each paint is exactly the epoch's position.
    expect(second).not.toBe(first);
    expect(second).toBe(`${bubbleWave.positionX(clock)}%`);
  });

  it("paints the band the instant it goes hidden, not a tick later", () => {
    // Arrange — a waving bubble, visible.
    const clock = Date.now();
    const bubble = workingBubble();
    setVisibility("visible");
    driver = createPromptWaveDriver({ root, now: () => clock, reducedMotion: () => false });
    driver.start();

    // Act — the page hides.
    setVisibility("hidden");

    // Assert — painted at once, so the compositor's last frozen frame is
    // replaced with no gap.
    expect(bubble.style.backgroundPositionX).toBe(`${bubbleWave.positionX(clock)}%`);
  });

  it("paints nothing under reduced motion, which has no wave to keep alive", () => {
    // Arrange — reduced motion asked for; the stylesheet drops the fill.
    let clock = Date.now();
    const bubble = workingBubble();
    setVisibility("hidden");
    driver = createPromptWaveDriver({ root, now: () => clock, reducedMotion: () => true });
    driver.start();

    // Act — ticks pass.
    clock += 1600;
    vi.advanceTimersByTime(HIDDEN_TICK_MS);

    // Assert — never painted a position.
    expect(bubble.style.backgroundPositionX).toBe("");
  });

  it("leaves a settled prompt alone, painting only the in-flight ones", () => {
    // Arrange — a settled bubble (no in-flight mark) beside a working one.
    const clock = Date.now();
    const settled = settledBubble();
    workingBubble();
    setVisibility("hidden");
    driver = createPromptWaveDriver({ root, now: () => clock, reducedMotion: () => false });

    // Act
    driver.start();

    // Assert — the settled prompt never waved.
    expect(settled.style.backgroundPositionX).toBe("");
  });

  it("flags the root while hidden so the stylesheet drops the dead animation", () => {
    // Arrange
    const clock = Date.now();
    workingBubble();
    setVisibility("hidden");
    driver = createPromptWaveDriver({ root, now: () => clock, reducedMotion: () => false });

    // Act
    driver.start();

    // Assert
    expect(document.documentElement.hasAttribute(PAGE_HIDDEN_ATTRIBUTE)).toBe(true);
  });

  it("hands back to the compositor on return, re-seeked to the same phase", () => {
    // Arrange — waving while hidden, its position hand-painted.
    let clock = Date.now();
    const bubble = workingBubble();
    setVisibility("hidden");
    driver = createPromptWaveDriver({ root, now: () => clock, reducedMotion: () => false });
    driver.start();
    expect(bubble.style.backgroundPositionX).not.toBe("");

    // Act — the page returns to visible at a later phase.
    clock += 900;
    setVisibility("visible");

    // Assert — the inline position is cleared so the animation owns it again,
    // the flag is gone so the animation restarts, and the delay is re-seeked to
    // the epoch so it resumes where the band already is.
    expect({
      position: bubble.style.backgroundPositionX,
      flagged: document.documentElement.hasAttribute(PAGE_HIDDEN_ATTRIBUTE),
      delay: bubble.style.animationDelay,
    }).toEqual({
      position: "",
      flagged: false,
      delay: `-${Math.round(bubbleWave.delayMs(clock))}ms`,
    });
  });

  it("stops ticking after stop(), leaving no timer painting a torn-down page", () => {
    // Arrange — running while hidden.
    let clock = Date.now();
    const bubble = workingBubble();
    setVisibility("hidden");
    driver = createPromptWaveDriver({ root, now: () => clock, reducedMotion: () => false });
    driver.start();
    driver.stop();
    const afterStop = bubble.style.backgroundPositionX;

    // Act — time passes with no driver.
    clock += 1600;
    vi.advanceTimersByTime(HIDDEN_TICK_MS * 4);

    // Assert — nothing repainted, and the root flag is cleared.
    expect(bubble.style.backgroundPositionX).toBe(afterStop);
    expect(document.documentElement.hasAttribute(PAGE_HIDDEN_ATTRIBUTE)).toBe(false);
  });
});
