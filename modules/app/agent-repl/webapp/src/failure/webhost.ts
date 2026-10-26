/**
 * webhost failures — the webhost's own failures, listed in a page's warning chip.
 *
 * The webhost draws the sidebar for every workspace page, so a failure of its
 * machinery (the sidebar's stream, a verb it could not send) is the webhost's.
 * The topbar's warning chip is the webapp's ONE error surface and the webhost
 * has no topbar, so the webhost hands every page the failures standing in it
 * (host/protocol.ts `webhostFailures`), and each page's chip lists them beside
 * its own.
 *
 * A PAGE'S OWN ROW WINS. When the page and the webhost both stand a failure of
 * the same arm (the daemon went away, and both their links say so), the chip
 * lists the page's row once, the way a repeat report of one arm replaces its
 * row (failure/local.ts).
 */
import { log } from "../log.js";
import { createListeners } from "../listeners.js";
import type { WebhostFailure } from "../host/protocol.js";
import type { LocalFailure, LocalFailureView } from "./local.js";

/** The webhost's failures as the page last heard them. */
export interface WebhostFailureView extends LocalFailureView {
  /** Replace the whole set with FAILURES, as the webhost last stated it. */
  set(failures: readonly WebhostFailure[]): void;
}

/** A view of the webhost's failures, empty until the webhost states them. */
export function createWebhostFailureView(): WebhostFailureView {
  let standing: readonly LocalFailure[] = [];
  const listeners = createListeners();
  return {
    standing: () => standing,
    subscribe(listener) {
      return listeners.add(listener);
    },
    set(failures) {
      log.info("the webhost's standing failures changed", {
        operation: "warning-chip.webhost-failures",
        context: { arms: failures.map((f) => f.arm) },
      });
      standing = failures;
      listeners.notify();
    },
  };
}

/** OWN's failures, then OTHER's whose arm OWN does not stand. */
export function mergeFailureViews(own: LocalFailureView, other: LocalFailureView): LocalFailureView {
  return {
    standing() {
      const mine = own.standing();
      const arms = new Set(mine.map((f) => f.arm));
      return [...mine, ...other.standing().filter((f) => !arms.has(f.arm))];
    },
    subscribe(listener) {
      const offOwn = own.subscribe(listener);
      const offOther = other.subscribe(listener);
      return () => {
        offOwn();
        offOther();
      };
    },
  };
}
