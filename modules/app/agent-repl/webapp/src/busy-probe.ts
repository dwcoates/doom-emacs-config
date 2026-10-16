/**
 * busy-probe — where one page's main-thread time goes between censuses, by
 * the kind of callback that spent it.
 *
 * DIAGNOSIS, 2026-10-16. The webhost's one WebContent process sits at 15-20%
 * of a core with nobody typing. A native sample of it puts that time in
 * WebCore's shared timer: JS timers whose DOM writes wake MutationObserver
 * callbacks that measure text (`Range.getClientRects`) and read `scrollTop`,
 * and the rendering updates after them. Every workspace page lives in that
 * one process, so the sample cannot say WHICH page spends it. Each page
 * counts its own callbacks here, and the census (page-census.ts) states the
 * tally once a minute on the page's own workspace log.
 *
 * WHAT IT SEES: every timer, animation frame, MutationObserver and
 * ResizeObserver callback the page registers after `installBusyProbe`. What it
 * does not see is the browser's own work outside a callback (the style,
 * layout and paint of a rendering update) and stream reads, which resolve as
 * promises; a layout a callback forces is inside the callback, and counted.
 *
 * WHAT IT COSTS: two `performance.now()` reads and a map update per callback.
 * The page runs a handful of callbacks a second at rest, so the probe is
 * well under the noise of what it measures. It is a diagnosis, and it is
 * removed once the attribution is done.
 */

/** The kinds of callback the probe tells apart. */
export type BusyKind = "timer" | "frame" | "mutation" | "resize";

/** One kind's or one label's tally since the last take. */
export interface BusyTally {
  count: number;
  ms: number;
}

/** One label's tally, in a report's top list. */
export interface BusyTop extends BusyTally {
  readonly kind: BusyKind;
  readonly label: string;
}

/** What a take answers: the totals by kind, and the costliest labels. */
export interface BusyReport {
  readonly busy_ms: number;
  readonly kinds: Readonly<Record<BusyKind, BusyTally>>;
  readonly top: readonly BusyTop[];
}

/** The installed probe. */
export interface BusyProbe {
  /** The tally since the last take, which starts the next one. */
  take(): BusyReport;
  /** Put every wrapped global back. */
  stop(): void;
}

/** The globals the probe wraps. */
export interface BusyWindow {
  setTimeout: Window["setTimeout"];
  setInterval: Window["setInterval"];
  requestAnimationFrame: Window["requestAnimationFrame"];
  MutationObserver: typeof MutationObserver;
  ResizeObserver: typeof ResizeObserver;
}

/** How many labels a report names, costliest first. */
export const BUSY_TOP = 8;

/** NODE as `tag.class`, the way the scroll witness names an element. */
function targetLabel(node: Node): string {
  const el = node instanceof Element ? node : node.parentElement;
  if (el === null) return node.nodeName.toLowerCase();
  const classes = [...el.classList].join(".");
  return classes === "" ? el.tagName.toLowerCase() : `${el.tagName.toLowerCase()}.${classes}`;
}

/** A callback's own name, or `anonymous`. */
function fnLabel(fn: { readonly name: string }): string {
  return fn.name === "" ? "anonymous" : fn.name;
}

/** Wrap WIN's callback registrations so each callback's time is tallied. */
export function installBusyProbe(win: BusyWindow, now: () => number = () => performance.now()): BusyProbe {
  const original = {
    setTimeout: win.setTimeout,
    setInterval: win.setInterval,
    requestAnimationFrame: win.requestAnimationFrame,
    MutationObserver: win.MutationObserver,
    ResizeObserver: win.ResizeObserver,
  };
  const emptyKinds = (): Record<BusyKind, BusyTally> => ({
    timer: { count: 0, ms: 0 },
    frame: { count: 0, ms: 0 },
    mutation: { count: 0, ms: 0 },
    resize: { count: 0, ms: 0 },
  });
  let kinds = emptyKinds();
  let labels = new Map<string, BusyTop>();

  const record = (kind: BusyKind, label: string, ms: number): void => {
    const total = kinds[kind];
    total.count += 1;
    total.ms += ms;
    const key = `${kind}\u0000${label}`;
    const tally = labels.get(key) ?? { kind, label, count: 0, ms: 0 };
    tally.count += 1;
    tally.ms += ms;
    labels.set(key, tally);
  };

  /** FN, timed under KIND and the label LABEL answers when it runs. */
  const timed = <A extends unknown[], R>(kind: BusyKind, label: () => string, fn: (...args: A) => R) =>
    function (this: unknown, ...args: A): R {
      const began = now();
      try {
        return fn.apply(this, args);
      } finally {
        record(kind, label(), now() - began);
      }
    };

  /** A timer's handler, timed when it is a function; a string runs as it is. */
  const timedHandler = (handler: TimerHandler): TimerHandler => {
    if (typeof handler !== "function") return handler;
    const fn = handler as (...args: unknown[]) => unknown;
    return timed("timer", () => fnLabel(fn), fn);
  };
  win.setTimeout = ((handler: TimerHandler, ms?: number, ...args: unknown[]) =>
    original.setTimeout.call(win, timedHandler(handler), ms, ...args)) as Window["setTimeout"];
  win.setInterval = ((handler: TimerHandler, ms?: number, ...args: unknown[]) =>
    original.setInterval.call(win, timedHandler(handler), ms, ...args)) as Window["setInterval"];
  win.requestAnimationFrame = (callback: FrameRequestCallback): number =>
    original.requestAnimationFrame.call(win, timed("frame", () => fnLabel(callback), callback));

  // AN OBSERVER IS NAMED BY WHAT IT FIRST OBSERVES: its callback is usually an
  // arrow with no name, and its target says which component it serves.
  const BaseMutation = original.MutationObserver;
  win.MutationObserver = class extends BaseMutation {
    constructor(callback: MutationCallback) {
      const named: { label: string } = { label: "unobserved" };
      super(timed("mutation", () => named.label, callback));
      this.named = named;
    }
    private readonly named: { label: string };
    override observe(target: Node, options?: MutationObserverInit): void {
      if (this.named.label === "unobserved") this.named.label = targetLabel(target);
      super.observe(target, options);
    }
  };
  const BaseResize = original.ResizeObserver;
  win.ResizeObserver = class extends BaseResize {
    constructor(callback: ResizeObserverCallback) {
      const named: { label: string } = { label: "unobserved" };
      super(timed("resize", () => named.label, callback));
      this.named = named;
    }
    private readonly named: { label: string };
    override observe(target: Element, options?: ResizeObserverOptions): void {
      if (this.named.label === "unobserved") this.named.label = targetLabel(target);
      super.observe(target, options);
    }
  };

  return {
    take(): BusyReport {
      const taken = kinds;
      const top = [...labels.values()].sort((a, b) => b.ms - a.ms).slice(0, BUSY_TOP);
      kinds = emptyKinds();
      labels = new Map();
      const busy = Object.values(taken).reduce((sum, tally) => sum + tally.ms, 0);
      const round = (tally: BusyTally): BusyTally => ({ count: tally.count, ms: Math.round(tally.ms) });
      return {
        busy_ms: Math.round(busy),
        kinds: {
          timer: round(taken.timer),
          frame: round(taken.frame),
          mutation: round(taken.mutation),
          resize: round(taken.resize),
        },
        top: top.map((tally) => ({ ...tally, ms: Math.round(tally.ms) })),
      };
    },
    stop(): void {
      Object.assign(win, original);
    },
  };
}
