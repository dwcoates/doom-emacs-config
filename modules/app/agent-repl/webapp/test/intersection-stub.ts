import { vi } from "vitest";

/**
 * A driveable `IntersectionObserver` for the feed's lazy rendering.
 *
 * jsdom implements none, which is itself the environment `canDeferItems`
 * answers false for — so a test that wants to see a placeholder upgrade has to
 * supply one, and has to be able to say WHEN an item came near rather than
 * waiting for a layout jsdom never performs.
 */
export class StubIntersectionObserver implements IntersectionObserver {
  /** Every observer built while the stub was installed, in construction order. */
  static instances: StubIntersectionObserver[] = [];

  readonly targets = new Set<Element>();
  readonly options: IntersectionObserverInit | undefined;
  readonly root: Element | Document | null = null;
  readonly rootMargin: string = "";
  readonly thresholds: readonly number[] = [];
  private callback: IntersectionObserverCallback;

  constructor(callback: IntersectionObserverCallback, options?: IntersectionObserverInit) {
    this.callback = callback;
    this.options = options;
    this.root = (options?.root as Element | Document | null) ?? null;
    this.rootMargin = options?.rootMargin ?? "";
    StubIntersectionObserver.instances.push(this);
  }

  observe(el: Element): void {
    this.targets.add(el);
  }

  unobserve(el: Element): void {
    this.targets.delete(el);
  }

  disconnect(): void {
    this.targets.clear();
  }

  takeRecords(): IntersectionObserverEntry[] {
    return [];
  }

  /** Fire the callback as if every element in ELS had come near. */
  fire(els: readonly Element[]): void {
    this.fireWith(els.map((target) => ({ target, isIntersecting: true })));
  }

  /** Fire the callback with explicit intersection verdicts. */
  fireWith(entries: ReadonlyArray<{ target: Element; isIntersecting: boolean }>): void {
    this.callback(entries as unknown as IntersectionObserverEntry[], this);
  }
}

/**
 * Run BUILD with the stub installed as the global `IntersectionObserver`,
 * answering whatever it built. The stub stays installed afterwards (the
 * renderer keeps its observer), so the caller unstubs in its own teardown.
 */
export function withIntersectionObserver<T>(build: () => T): T {
  vi.stubGlobal("IntersectionObserver", StubIntersectionObserver);
  return build();
}
