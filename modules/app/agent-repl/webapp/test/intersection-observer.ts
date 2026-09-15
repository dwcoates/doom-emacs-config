/**
 * An `IntersectionObserver` FOR JSDOM — the same environment substitution as
 * the harness's `ResizeObserver` and `scrollIntoView` stubs, and for the same
 * reason.
 *
 * jsdom performs no layout, so it ships no `IntersectionObserver`: nothing ever
 * enters or leaves a viewport there, and nothing would ever be delivered even
 * if the class existed. The feed's overscan buffer (overscan.ts) subscribes one
 * to the scroll box, so without this every mount of the feed under jsdom would
 * take the "no IntersectionObserver" no-op path and the overscan wiring would
 * never run in a test.
 *
 * The substitution is deliberately NOT a no-op class. It keeps the registry of
 * what is observed and the options each observer was built with, so a test can
 * assert the root and margin the app asked for, and `fireIntersection` is the
 * test's way of saying "this element crossed the band" — and it THROWS when
 * nothing observes the element handed to it, so a mount that forgot to observe
 * a row fails the fire rather than passing a silent no-op.
 */

/** The options one observer was constructed with, plus what it watches. */
export interface IntersectionRegistration {
  callback: IntersectionObserverCallback;
  targets: Set<Element>;
  root: Element | null;
  rootMargin: string;
  observer: IntersectionObserver;
}

const registrations = new Set<IntersectionRegistration>();

/** Installed once per environment; a second call is a no-op, not a reset. */
let installed = false;

/**
 * Install the stub as `globalThis.IntersectionObserver`. Idempotent, so a setup
 * file loaded once per worker and a file installing it for itself cannot fight.
 */
export function installIntersectionObserver(): void {
  if (installed) return;
  installed = true;
  class StubIntersectionObserver {
    readonly registration: IntersectionRegistration;

    constructor(callback: IntersectionObserverCallback, options?: IntersectionObserverInit) {
      const root = options?.root ?? null;
      this.registration = {
        callback,
        targets: new Set(),
        root: root instanceof Element ? root : null,
        rootMargin: options?.rootMargin ?? "0px 0px 0px 0px",
        observer: this as unknown as IntersectionObserver,
      };
      registrations.add(this.registration);
    }

    observe(target: Element): void {
      this.registration.targets.add(target);
    }

    unobserve(target: Element): void {
      this.registration.targets.delete(target);
    }

    disconnect(): void {
      this.registration.targets.clear();
      registrations.delete(this.registration);
    }

    takeRecords(): IntersectionObserverEntry[] {
      return [];
    }
  }
  (globalThis as { IntersectionObserver?: unknown }).IntersectionObserver =
    StubIntersectionObserver;
}

/** The observers currently registered, for asserting root/margin and reach. */
export function intersectionObservers(): readonly IntersectionRegistration[] {
  return [...registrations];
}

/**
 * Report that TARGET crossed the band — into it when INTERSECTING is true, out
 * of it when false — to every observer watching it.
 *
 * Throws when nothing is watching it. A test that fires at a feed row is
 * asserting the mount observed that very element, so a silent no-op would turn
 * the missing subscription — the whole defect this exists for — into a green
 * run.
 */
export function fireIntersection(target: Element, intersecting: boolean): void {
  const watching = [...registrations].filter((r) => r.targets.has(target));
  if (watching.length === 0) {
    throw new Error("fireIntersection: no IntersectionObserver is watching that element");
  }
  for (const registration of watching) {
    const entry = { target, isIntersecting: intersecting } as IntersectionObserverEntry;
    registration.callback([entry], registration.observer);
  }
}
