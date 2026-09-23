/**
 * A `ResizeObserver` FOR JSDOM — an environment substitution, because jsdom
 * lacks the capability rather than because the app has a seam here.
 *
 * jsdom performs no layout, so it ships no `ResizeObserver`: an element's box
 * never changes size there, and nothing would ever be delivered even if the
 * class existed. The app subscribes one to the feed's scroll box
 * (`observeScrollBox`), so without this every mount of the feed under jsdom
 * throws on a missing global before it has drawn a row.
 *
 * The substitution is deliberately NOT a no-op class. It keeps the registry of
 * what is observed, so `fireResize` is the test's way of saying "this element's
 * box changed" — and it THROWS when nothing observes the element handed to it,
 * which is what makes a wiring test a real check: a mount that forgot to
 * subscribe fails the fire rather than passing a silent no-op.
 *
 * Entries are keyed by element, so a mount that disconnects (feed disposal)
 * leaves nothing behind for the next test to fire into.
 */

/** What each observer is: its callback, and the elements it watches. */
interface Registration {
  callback: () => void;
  targets: Set<Element>;
}

const registrations = new Set<Registration>();

/** Installed once per environment; a second call is a no-op, not a reset. */
let installed = false;

/**
 * Install the stub as `globalThis.ResizeObserver`. Idempotent, so a setup file
 * loaded once per worker and a file installing it for itself cannot fight.
 */
export function installResizeObserver(): void {
  if (installed) return;
  installed = true;
  class StubResizeObserver {
    private readonly registration: Registration;

    constructor(callback: () => void) {
      this.registration = { callback, targets: new Set() };
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
  }
  (globalThis as { ResizeObserver?: unknown }).ResizeObserver = StubResizeObserver;
}

/**
 * Report that TARGET's box changed size, to every observer watching it.
 *
 * Throws when nothing is watching it. A test that fires at the feed's scroll
 * box is asserting the mount subscribed to that very element, so a silent
 * no-op would turn the missing subscription — the whole defect this exists for
 * — into a green run.
 */
export function fireResize(target: Element): void {
  const watching = [...registrations].filter((r) => r.targets.has(target));
  if (watching.length === 0) {
    throw new Error("fireResize: no ResizeObserver is watching that element");
  }
  for (const registration of watching) registration.callback();
}
