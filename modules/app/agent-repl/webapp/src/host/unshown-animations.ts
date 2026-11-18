/**
 * unshown-animations — A PAGE THE WEBHOST IS NOT SHOWING RUNS NO ANIMATIONS,
 * AND A PAGE IT SHOWS AGAIN HAS EVERY ANIMATION WHERE IT WOULD HAVE BEEN.
 *
 * WHY. A page not shown is `visibility: hidden` in the webhost (host.css), and
 * WebKit keeps running its CSS animations: every working workspace's spinner,
 * breathing footer word and status wave forced a style pass on its page every
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
 * and the next redraw would snap it forward. So every animated element's
 * animations are moved on by exactly how long they were frozen: since the hide
 * for an element there then, since its insertion for one the page added while
 * hidden (a paused animation is made at time 0). The move is a delay: each of
 * the element's `animation-delay`s goes that many ms more negative, which
 * advances the animation as far as seeking it would. A one-shot whose moment
 * passed while hidden is thereby past its end, and is left ended.
 *
 * NO ANIMATION OBJECT IS EVER ASKED FOR. On the system WebKit an animation
 * that script has asked for (`getAnimations()`) is never freed, nor the
 * element it animated, and a hidden working page's footer is rebuilt on every
 * push, so asking leaked every fresh letter for good (2026-11-18; the webapp's
 * AGENTS.md, Performance lessons). Elements are read by their computed style
 * only, and dated by the page's mutations, which name each inserted node.
 *
 * WHAT IS DATED. An element is dated by its insertion alone. An element there
 * at the hide that starts a new animation by a class or style change while
 * hidden is moved on from the hide, as its other animations are.
 *
 * THE ORDER ON SHOW IS ENGINE-MEASURED (WebKit, 2026-10-13). The animated
 * elements are read while paused, the pause is lifted and flushed, and only
 * then are the delays moved: a one-shot moved past its end while paused is
 * REWOUND when the play state changes (play() restarts a finished animation),
 * and one moved after keeps its end. All of it runs in one task, so no frame
 * shows the frozen phase.
 */
import { log } from "../log.js";
import type { Presence } from "../presence.js";
import { pageMutationObserver } from "../dom.js";

/** The root attribute the stylesheet pauses every animation under. */
export const PAGE_UNSHOWN_ATTRIBUTE = "data-page-unshown";

/** An element running at least one animation, which the show moves on. */
export interface AnimatedElement {
  readonly element: Element;
  /** Move every one of the element's animations on by BY_MS. */
  advance(byMs: number): void;
}

/** What the pause needs from the page; every field is the live page's in production. */
export interface UnshownAnimationsEnv {
  readonly doc: Document;
  readonly presence: Presence;
  /** The page's clock, in ms. */
  readonly now: () => number;
  /** Every element of the page running an animation now (flushing style). */
  readonly animated: () => Iterable<AnimatedElement>;
  /** Flush the page's style, so a lifted pause takes effect. */
  readonly flush: () => void;
  /** Call FN with the elements each batch of the page's mutations inserted; answers the stop. */
  readonly observe: (fn: (inserted: Iterable<Element>) => void) => () => void;
}

/** The live page's environment. */
export function liveUnshownAnimationsEnv(doc: Document, presence: Presence): UnshownAnimationsEnv {
  const view = doc.defaultView;
  if (view === null) throw new Error("unshown-animations: the document has no window");
  return {
    doc,
    presence,
    now: () => performance.now(),
    animated: () => animatedElements(doc, view),
    flush: () => void view.getComputedStyle(doc.documentElement).animationPlayState,
    observe: (fn) => {
      const observer = pageMutationObserver(doc, (records) => fn(insertedElements(records)));
      observer.observe(doc.documentElement, { subtree: true, childList: true });
      return () => observer.disconnect();
    },
  };
}

/** Every element under DOC whose computed style runs an animation. */
export function animatedElements(doc: Document, view: Window & typeof globalThis): AnimatedElement[] {
  const found: AnimatedElement[] = [];
  const walker = doc.createTreeWalker(doc.documentElement, NodeFilter.SHOW_ELEMENT);
  for (let node: Node | null = walker.currentNode; node !== null; node = walker.nextNode()) {
    const element = node as Element;
    const computed = view.getComputedStyle(element);
    const names = cssList(computed.animationName);
    if (names.every((name) => name === "none")) continue;
    if (!(element instanceof view.HTMLElement) && !(element instanceof view.SVGElement)) continue;
    const styled = element;
    found.push({
      element,
      advance: (byMs) => {
        // The computed list may be shorter than the names, and repeats as
        // CSS repeats it: every animation gets its own moved delay.
        const delays = cssList(view.getComputedStyle(styled).animationDelay).map(cssTimeMs);
        styled.style.animationDelay = names.map((_, i) => `${delays[i % delays.length]! - byMs}ms`).join(", ");
      },
    });
  }
  return found;
}

/** Every element RECORDS inserted, with everything under it. */
export function insertedElements(records: Iterable<MutationRecord>): Element[] {
  const found: Element[] = [];
  for (const record of records) {
    for (const node of record.addedNodes) {
      if (!(node instanceof Element)) continue;
      found.push(node, ...node.querySelectorAll("*"));
    }
  }
  return found;
}

/** A computed comma list's parts. */
function cssList(value: string): string[] {
  return value.split(",").map((part) => part.trim());
}

/** A computed CSS time (`1.3s`, `200ms`) in milliseconds; anything else is a broken style read. */
export function cssTimeMs(value: string): number {
  const ms = value.endsWith("ms") ? Number(value.slice(0, -2)) : value.endsWith("s") ? Number(value.slice(0, -1)) * 1000 : NaN;
  if (!Number.isFinite(ms)) {
    log.error(`an animation's computed delay is no CSS time (${value}); the page's animations cannot resume`, {
      operation: "page.animations-resume-failed",
      context: { delay: value },
    });
    throw new Error(`unshown-animations: an animation's computed delay is no CSS time (${value})`);
  }
  return ms;
}

/** Pause the page's animations while ENV's page is not shown; answers the stop. */
export function pauseAnimationsWhileUnshown(env: UnshownAnimationsEnv): () => void {
  const { doc, presence } = env;
  const root = doc.documentElement;
  /**
   * When each element the page inserted while hidden was inserted. An element
   * not here was there at the hide. Weak, so a hidden page whose footer
   * redraws for hours holds none of the letters it dropped.
   */
  let insertedAt = new WeakMap<Element, number>();
  let stopObserving: (() => void) | null = null;

  /** Pause the page; answers when it was paused. */
  const hide = (): number => {
    root.setAttribute(PAGE_UNSHOWN_ATTRIBUTE, "");
    const at = env.now();
    insertedAt = new WeakMap();
    stopObserving = env.observe((inserted) => {
      const moment = env.now();
      // A reinserted element restarts its animations, so its latest
      // insertion is the one it is frozen since.
      for (const element of inserted) insertedAt.set(element, moment);
    });
    log.debug("the page is not shown; its animations are paused", { operation: "page.animations-paused" });
    return at;
  };

  /** Resume the page paused at HIDDEN_AT. */
  const show = (hiddenAt: number): void => {
    const began = performance.now();
    stopObserving?.();
    stopObserving = null;
    const at = env.now();
    // The elements are read WHILE PAUSED, each with how long it was frozen.
    const targets: [AnimatedElement, number][] = [];
    for (const animated of env.animated()) {
      targets.push([animated, at - (insertedAt.get(animated.element) ?? hiddenAt)]);
    }
    root.removeAttribute(PAGE_UNSHOWN_ATTRIBUTE);
    // Flush the lifted pause before moving, or a one-shot past its end
    // rewinds (see the header).
    env.flush();
    for (const [animated, by] of targets) animated.advance(by);
    insertedAt = new WeakMap();
    log.info("the page is shown again; its animations resumed where they would have been", {
      operation: "page.animations-resumed",
      context: {
        animated_elements: targets.length,
        hidden_ms: Math.round(at - hiddenAt),
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
