/**
 * page-teardown — the end of a page: the stops of everything the boot installs
 * for the page's whole lifetime (its timers), and the abort of every request
 * it has in flight.
 *
 * A page in production ends when its document is unloaded. A TEST that boots
 * the page is torn down too, and the unit suites share one environment per
 * worker (vitest.config.ts, `isolate: false`). So a boot's page-lifetime
 * install would outlive its test file: the page census's one-minute timer kept
 * firing into later files against a document that was no longer the
 * environment's (2026-10-17, a merge gate's run long enough for the first
 * tick). The boot registers each stop here, and a boot test tears the page
 * down.
 *
 * NOTHING RUNS IN A PAGE AFTER ITS DOCUMENT IS UNLOADED (2026-10-17). An
 * unloaded document's script is not stopped at once: a request in flight when
 * the page's frame was navigated away (a handover re-points every page) still
 * answered, and its draw ran in a document with no browsing context, where a
 * custom element is never upgraded (`d.onConnect is not a function`). So the
 * page's `pagehide` (`endPageAtPagehide`) aborts every request the page's
 * transport made (`pageEnding`), and a call the abort ended never settles
 * (`abandonedAtPageEnd`): no continuation of it runs in the dead page.
 */

const stops: (() => void)[] = [];
let ending = new AbortController();

/** Begin a page: its requests are aborted when it ends. The boot calls it first. */
export function beginPage(): void {
  ending = new AbortController();
}

/** The signal every request of the current page carries; aborted when the page ends. */
export function pageEnding(): AbortSignal {
  return ending.signal;
}

/** Whether the current page has ended. */
export function pageEnded(): boolean {
  return ending.signal.aborted;
}

/**
 * What a call the page's end cut off answers: a promise that never settles,
 * so nothing awaiting it runs in a page that is gone.
 */
export function abandonedAtPageEnd<T>(): Promise<T> {
  return new Promise<T>(() => undefined);
}

/** End the page: abort every request it made, then run its stops. */
export function endPage(): void {
  ending.abort(new DOMException("the page was unloaded", "AbortError"));
  tearDownPage();
}

/**
 * End the page at WIN's `pagehide`, unless it is kept in the back/forward
 * cache (`persisted`), from which it can come back. The listener goes with the
 * page's stops, so a test's teardown takes it off a window its suites share.
 */
export function endPageAtPagehide(win: Window): void {
  const end = (event: PageTransitionEvent): void => {
    if (!event.persisted) endPage();
  };
  win.addEventListener("pagehide", end);
  stopAtPageTeardown(() => win.removeEventListener("pagehide", end));
}

/** Register STOP, run when the page is torn down. */
export function stopAtPageTeardown(stop: () => void): void {
  stops.push(stop);
}

/** Run every registered stop, the latest first, and forget them. */
export function tearDownPage(): void {
  for (const stop of stops.splice(0).reverse()) stop();
}
