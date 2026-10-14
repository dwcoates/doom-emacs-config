/**
 * wall-phase — every looping animation in the rail runs on the WALL CLOCK, so
 * no redraw and no workspace switch ever restarts one.
 *
 * WHY. The rail is drawn whole on every roster push (`sidebar.ts`), and a
 * fresh element starts its CSS animation at 0%, so a breathing dot or a
 * spinning merge glyph snapped back to its start on every push — and a
 * workspace switch is a push. Worse, every workspace has its OWN webview, so a
 * switch also swaps which page is on screen: a page whose own clock started
 * at a different moment, and whose animations WebKit suspended while it was
 * hidden. Switching therefore showed every animation jumping to another phase.
 *
 * THE FIX IS ONE CLOCK EVERY PAGE ALREADY SHARES. Each looping animation is
 * seeked to `now mod period` of the wall clock (`Date.now()`), which needs no
 * stamped epoch and no coordination: every page, every redraw and every
 * reveal of a hidden page computes the same phase for the same instant, so
 * the dot on screen after a switch is exactly where the one before it was.
 *
 * THE PERIOD IS READ, NEVER RESTATED. The seek walks the rail's running Web
 * Animations (`Element.getAnimations`) and takes each one's duration from its
 * own timing, so a keyframe rule's duration lives only in `styles.css`; there
 * is no TypeScript copy to drift from it. One-shot animations and transitions
 * (finite iterations) are not loops and are left alone.
 *
 * WHEN IT SEEKS. After every draw, once the drawn rail is in the document
 * (`seek`), and on the first animation frame after the page becomes visible
 * again — the first frame is the first moment WebKit is running the
 * animations it suspended, so a seek there cannot be undone by the resume.
 */
import { log } from "../log.js";
import { createPresence, pagePresence, type Presence } from "../presence.js";
import { nextFrame } from "../frame.js";

/** The slice of a Web Animation the seek reads and moves. */
export interface PhasedAnimation {
  currentTime: CSSNumberish | null;
  readonly effect: { getTiming(): EffectTiming } | null;
}

/** Where a rail's running animations are read from. */
export type AnimationSource = (host: Element) => Iterable<PhasedAnimation>;

/** The page's own animations under HOST, which is what production reads. */
export const PAGE_ANIMATIONS: AnimationSource = (host) => host.getAnimations({ subtree: true });

/** What the seek needs from its environment; every field defaults to the page. */
export interface WallPhaseEnv {
  animations?: AnimationSource;
  /** The wall clock, in epoch milliseconds. */
  now?: () => number;
  /** The document whose visibility is watched when no presence is given. */
  doc?: Document;
  /** Whether a reader can see the page; the page's own presence when absent. */
  presence?: Presence;
  /** Schedules a callback for the next animation frame. */
  frame?: (callback: () => void) => void;
}

/** An installed seek: run it after a draw, dispose it with the rail. */
export interface WallPhase {
  /** Seek every looping animation under the host to the wall clock. */
  seek(): void;
  dispose(): void;
}

/** How far into a PERIOD-long cycle the wall clock is at NOW, in `[0, period)`. */
export function wallPhaseMs(nowMs: number, periodMs: number): number {
  return ((nowMs % periodMs) + periodMs) % periodMs;
}

/**
 * Seek every LOOPING animation in ANIMATIONS to the wall clock's phase at NOW,
 * and answer how many were sought.
 *
 * An alternating loop repeats every TWO iterations, so that is its period. The
 * animation's own delay is kept: the seek lands its ACTIVE time on the phase.
 *
 * An infinite animation without a positive finite duration, or a CSS
 * animation with no effect, cannot exist in a rail drawn from `styles.css`;
 * either is a defect, and is thrown rather than skipped — before any animation
 * is moved, so a refused seek leaves every phase as it was.
 */
export function seekToWallClock(animations: Iterable<PhasedAnimation>, nowMs: number): number {
  const seeks: Array<{ animation: PhasedAnimation; to: number }> = [];
  for (const animation of animations) {
    const timing = requireTiming(animation);
    if (timing.iterations !== Infinity) continue;
    const duration = requireLoopDuration(timing);
    const alternates = timing.direction === "alternate" || timing.direction === "alternate-reverse";
    const period = alternates ? duration * 2 : duration;
    seeks.push({ animation, to: (timing.delay ?? 0) + wallPhaseMs(nowMs, period) });
  }
  for (const { animation, to } of seeks) animation.currentTime = to;
  return seeks.length;
}

/** ANIMATION's timing, which every running animation has. */
function requireTiming(animation: PhasedAnimation): EffectTiming {
  if (animation.effect === null) {
    log.error("a running rail animation has no effect to time", {
      operation: "sidebar.wall-phase.no-effect",
      context: { current_time: animation.currentTime },
    });
    throw new Error("sidebar wall phase: a running rail animation has no effect");
  }
  return animation.effect.getTiming();
}

/** A loop's duration, which must be a positive finite number of milliseconds. */
function requireLoopDuration(timing: EffectTiming): number {
  const duration = timing.duration;
  if (typeof duration !== "number" || !Number.isFinite(duration) || duration <= 0) {
    log.error("an endless rail animation has no positive duration to loop on", {
      operation: "sidebar.wall-phase.bad-duration",
      context: { duration: String(duration), iterations: String(timing.iterations) },
    });
    throw new Error(`sidebar wall phase: an endless rail animation has duration ${String(duration)}`);
  }
  return duration;
}

/**
 * Keep HOST's looping animations on the wall clock: `seek` after each draw,
 * and on the first frame after the page is shown again.
 */
export function installWallPhase(host: Element, env: WallPhaseEnv = {}): WallPhase {
  const animations = env.animations ?? PAGE_ANIMATIONS;
  const now = env.now ?? ((): number => Date.now());
  const doc = env.doc ?? document;
  const frame = env.frame ?? nextFrame;
  const owned = env.presence === undefined && doc !== document ? createPresence(doc, false) : null;
  const presence = env.presence ?? owned ?? pagePresence();

  let disposed = false;
  const seek = (): void => {
    const sought = seekToWallClock(animations(host), now());
    log.debug("seeked the rail's looping animations to the wall clock", {
      operation: "sidebar.wall-phase.seek",
      context: { animations: sought },
    });
  };
  const unsubscribe = presence.onChange((seen) => {
    if (!seen) return;
    frame(() => {
      if (!disposed) seek();
    });
  });

  return {
    seek,
    dispose(): void {
      disposed = true;
      unsubscribe();
      owned?.dispose();
    },
  };
}
