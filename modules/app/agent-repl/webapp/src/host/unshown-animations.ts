/**
 * unshown-animations — A PAGE THE WEBHOST IS NOT SHOWING RUNS NO ANIMATIONS,
 * AND A PAGE IT SHOWS AGAIN HAS EVERY ANIMATION WHERE IT WOULD HAVE BEEN.
 *
 * WHY. A page not shown is `visibility: hidden` in the webhost (host.css), and
 * WebKit keeps running its CSS animations: every working workspace's spinner,
 * breathing footer word and prompt wave forced a style pass on its page every
 * frame, nobody seeing any of it. Most of the webhost's CPU is that rendering
 * update, and most of the update is style recalculation
 * (docs/investigations/2026-10-13-page-cost-by-component.md, addendum).
 *
 * HOW IT PAUSES. While the page is not shown the root wears
 * {@link PAGE_UNSHOWN_ATTRIBUTE}, and the stylesheet pauses every animation
 * under it (`animation-play-state: paused`). A paused animation holds its
 * current time and costs nothing per frame.
 *
 * HOW IT RESUMES WITHOUT A JUMP. The loops follow a fixed start time
 * (breathing.ts), so resuming one from where it paused would put it behind,
 * and the next redraw would snap it forward. So every animation's current time
 * is moved on by exactly how long it was frozen: since the hide for one running
 * then, since its own creation for one made while hidden (a paused animation is
 * created at time 0). A one-shot that would have ended is left ended. The
 * creation moment is taken from the page's mutations, coalesced to one scan a
 * frame, so it is exact to within a frame.
 *
 * THE ORDER ON SHOW IS ENGINE-MEASURED (WebKit, 2026-10-13). The targets are
 * read while paused, the pause is lifted, and only then are the times set: a
 * one-shot seeked past its end while paused is REWOUND when the play state
 * changes (play() restarts a finished animation), and one seeked after keeps
 * its end. All of it runs in one task, so no frame shows the frozen phase.
 */
import { log } from "../log.js";
import type { Presence } from "../presence.js";
import { nextFrame } from "../frame.js";
import { pageMutationObserver } from "../dom.js";

/** The root attribute the stylesheet pauses every animation under. */
export const PAGE_UNSHOWN_ATTRIBUTE = "data-page-unshown";

/** The slice of a Web Animation the pause reads and moves. */
export interface FrozenAnimation {
  currentTime: CSSNumberish | null;
}

/** What the pause needs from the page; every field is the live page's in production. */
export interface UnshownAnimationsEnv {
  readonly doc: Document;
  readonly presence: Presence;
  /** The document timeline's time, in ms: the clock animations run on. */
  readonly now: () => number;
  /** Every animation in the page now (flushing style, so a pause or resume takes effect). */
  readonly animations: () => Iterable<FrozenAnimation>;
  /** Call FN after each batch of the page's mutations; answers the stop. */
  readonly observe: (fn: () => void) => () => void;
  /** Run FN on the next animation frame. */
  readonly frame: (fn: () => void) => void;
}

/** The live page's environment. */
export function liveUnshownAnimationsEnv(doc: Document, presence: Presence): UnshownAnimationsEnv {
  if (doc.defaultView === null) throw new Error("unshown-animations: the document has no window");
  return {
    doc,
    presence,
    now: () => Number(doc.timeline.currentTime),
    animations: () => doc.getAnimations(),
    observe: (fn) => {
      const observer = pageMutationObserver(doc, fn);
      observer.observe(doc.documentElement, { subtree: true, childList: true, attributes: true, characterData: true });
      return () => observer.disconnect();
    },
    frame: nextFrame,
  };
}

/** A held current time, which a paused animation always has. */
function heldTime(animation: FrozenAnimation): number {
  const time = animation.currentTime;
  if (typeof time !== "number") {
    const held = String(time);
    log.error(`a paused animation holds no numeric time (${held}); the page's animations cannot resume`, {
      operation: "page.animations-resume-failed",
      context: { held },
    });
    throw new Error(`unshown-animations: a paused animation holds no numeric time (${held})`);
  }
  return time;
}

/** Pause the page's animations while ENV's page is not shown; answers the stop. */
export function pauseAnimationsWhileUnshown(env: UnshownAnimationsEnv): () => void {
  const { doc, presence } = env;
  const root = doc.documentElement;
  /**
   * When each animation in the page froze: the hide, or its creation while
   * hidden. Rebuilt from the page's animations on every scan, so a hidden
   * page whose footer redraws for hours holds only the animations it has.
   */
  let frozenAt = new Map<FrozenAnimation, number>();
  let stopObserving: (() => void) | null = null;
  /** The first mutation not yet scanned, when one is pending. */
  let pendingSince: number | null = null;

  /** Date every animation in the page not yet dated to MOMENT, forgetting those gone. */
  const date = (moment: number): number => {
    const next = new Map<FrozenAnimation, number>();
    for (const animation of env.animations()) next.set(animation, frozenAt.get(animation) ?? moment);
    frozenAt = next;
    return next.size;
  };

  const scan = (): void => {
    if (pendingSince === null) return;
    const since = pendingSince;
    pendingSince = null;
    date(since);
  };

  /** Pause the page; answers when it was paused. */
  const hide = (): number => {
    root.setAttribute(PAGE_UNSHOWN_ATTRIBUTE, "");
    const at = env.now();
    const paused = date(at);
    stopObserving = env.observe(() => {
      if (pendingSince !== null) return;
      pendingSince = env.now();
      env.frame(scan);
    });
    log.debug("the page is not shown; its animations are paused", {
      operation: "page.animations-paused",
      context: { animations: paused },
    });
    return at;
  };

  /** Resume the page paused at HIDDEN_AT. */
  const show = (hiddenAt: number): void => {
    const began = performance.now();
    stopObserving?.();
    stopObserving = null;
    scan();
    const at = env.now();
    // The targets are read WHILE PAUSED: every held time, moved on by how
    // long its animation was frozen.
    const targets: [FrozenAnimation, number][] = [];
    let unrecorded = 0;
    for (const animation of env.animations()) {
      const since = frozenAt.get(animation);
      if (since === undefined) {
        unrecorded += 1;
        continue;
      }
      targets.push([animation, heldTime(animation) + (at - since)]);
    }
    root.removeAttribute(PAGE_UNSHOWN_ATTRIBUTE);
    // Flush the lifted pause before seeking, or a one-shot past its end
    // rewinds (see the header).
    env.animations();
    for (const [animation, time] of targets) animation.currentTime = time;
    const hiddenMs = at - hiddenAt;
    frozenAt.clear();
    log.info("the page is shown again; its animations resumed where they would have been", {
      operation: "page.animations-resumed",
      context: {
        animations: targets.length,
        // An animation made by no mutation the page saw (a style rule
        // changing on its own) resumes from where it paused.
        unrecorded,
        hidden_ms: Math.round(hiddenMs),
        took_ms: Math.round((performance.now() - began) * 10) / 10,
      },
    });
  };

  /** When the page was paused, while it is not shown. */
  let hiddenAt: number | null = presence.shown() ? null : hide();
  const unsubscribe = presence.onChange(() => {
    const shown = presence.shown();
    if (shown && hiddenAt !== null) {
      const since = hiddenAt;
      hiddenAt = null;
      show(since);
    } else if (!shown && hiddenAt === null) {
      hiddenAt = hide();
    }
  });
  return () => {
    unsubscribe();
    stopObserving?.();
    stopObserving = null;
    root.removeAttribute(PAGE_UNSHOWN_ATTRIBUTE);
  };
}
