/**
 * engine/in-flight.ts — the record of one piece of work in flight, which later
 * callers wait out.
 *
 * The session engine has work a later call must not run beside: a parked
 * bridge's opening, a session's start. Each is held as the work in flight from
 * the moment it begins until it settles, and a later call that must not
 * overlap it waits for that settlement first. This is the one spelling of
 * that record:
 *
 * - THE RECORD IS SET BEFORE THE WORK RUNS, so even the work's own
 *   synchronous first steps already see it in flight.
 * - THE RECORD IS CLEARED BEFORE ANY WAITER RESUMES, so a waiter that resumes
 *   finds nothing in flight unless a newer piece of work has begun since.
 * - THE WAIT NEVER REJECTS. A waiter hears HOW the work settled, summarized by
 *   the caller's `settledAs`; the work's own failure reaches the one caller
 *   that ran it, through the promise `run` returns.
 */
import { settleable } from "./settleable.js";

/** How a piece of work in flight ended: its value, or what it threw. */
export type InFlightOutcome<T> =
  | { readonly kind: "resolved"; readonly value: T }
  | { readonly kind: "rejected"; readonly error: unknown };

/** The one piece of work in flight, if any, settling as an `S`. */
export class InFlight<S> {
  private current: Promise<S> | undefined;

  /**
   * How the work in flight settles, or undefined when none is in flight.
   * Never rejects; the record is already cleared when it resolves.
   */
  get settling(): Promise<S> | undefined {
    return this.current;
  }

  /**
   * Run WORK as the work in flight, and answer its own promise, which the
   * caller holds: its rejection is the caller's to hear. SETTLED_AS summarizes
   * the outcome for every waiter, and must not throw.
   *
   * A record a newer `run` replaced is left to that newer work: only the
   * record this call set is cleared when its work settles.
   */
  run<T>(work: () => Promise<T>, settledAs: (outcome: InFlightOutcome<T>) => S): Promise<T> {
    const done = settleable<S>();
    this.current = done.promise;
    const settle = (outcome: InFlightOutcome<T>): void => {
      if (this.current === done.promise) this.current = undefined;
      done.resolve(settledAs(outcome));
    };
    // A WORK THAT THROWS BEFORE ITS FIRST AWAIT still settles the record: the
    // async wrapper turns that throw into the same rejection, value and all.
    const running = (async (): Promise<T> => work())();
    running.then(
      (value) => {
        settle({ kind: "resolved", value });
      },
      (error: unknown) => {
        settle({ kind: "rejected", error });
      },
    );
    return running;
  }
}
