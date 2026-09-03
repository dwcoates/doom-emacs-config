/**
 * DRIVING THE REAL CHAIN FROM THE PAGE.
 *
 * One place for the three things every area file does: mount the app against
 * the real daemon, submit a prompt through the app's OWN composer, and wait
 * until the thing the scenario produces is DRAWN.
 *
 * SCENARIOS ARE NEVER MINTED HERE. Every `!name` this layer submits is a
 * fake-SDK scenario a Go area test already drives (project-lead ruling); the
 * mapping from a drawn family to its scenario was established empirically
 * against the real chain and is recorded in `e2e/WEBAPP-LAYER-SPEC.md` §F2.
 */
import { expect } from "vitest";

import type { MountedApp } from "../integration/harness";
import { startAgainstRealDaemon } from "./real-daemon";

/**
 * How long one real turn is given to come back drawn.
 *
 * REUSED, NOT MINTED: 5s is `harness.DefaultTimeout` in the Go suite, sized
 * off a 443-test daemon/integration run (median 0.2s, observed max 1.7s) and
 * independently corroborated by the old `daemon/e2e`'s ~0.65s-per-await
 * baseline for exactly this shape — a real node-shim spawn plus a turn
 * through a real store. This layer's turn is that shape with jsdom drawing on
 * the end, so it inherits the budget rather than inventing a third number.
 *
 * It bounds a HANG. `awaitDrawn` returns on the first round the predicate
 * holds, and nothing in it sleeps.
 */
export const TURN_BUDGET_MS = 5_000;

/**
 * How long the page is given to boot against the real daemon.
 *
 * One real `AdoptWebWorkspace` round trip plus the accept of every `Watch*`
 * stream the six mounts open, against a daemon on loopback. Same grounding as
 * TURN_BUDGET_MS, minus the shim spawn, which boot does not pay.
 */
export const BOOT_BUDGET_MS = 5_000;

/** A test's own timeout when it drives one real turn end to end. */
export const TURN_TEST_MS = TURN_BUDGET_MS + BOOT_BUDGET_MS;

/**
 * How long the page is given to observe a whole DAEMON HANDOVER (§F9 #39).
 *
 * ITS OWN CONSTANT, because the chain it bounds is not a turn: a merge
 * landing, the rollout trigger, the incumbent re-execing itself, a SECOND real
 * claude-repld's full boot, the adoption rendezvous, and only then the
 * `transferred` push this end waits on. That is structurally two real process
 * lifecycles, so a turn's 5s would bound the wrong thing.
 *
 * REUSED RATHER THAN MINTED: 15s is `harness.HandoverChainTimeout`
 * (`daemon/integration/harness/daemon.go:47-57` — 3x DefaultTimeout, sized off
 * that measured chain), the bound the GO side of this very scenario runs on.
 * The page waits on the tail of that chain, so it inherits the driver's budget
 * rather than inventing a third number; a page bound tighter than the driver's
 * would fail the run for the driver still being mid-handover.
 *
 * It bounds a HANG: `awaitDrawn` returns on the first round the push has been
 * applied, and nothing in it sleeps.
 */
export const HANDOVER_BUDGET_MS = 15_000;

/**
 * The handover test's own timeout: this page's boot, the handover, and the
 * fresh page's boot at the successor's address.
 */
export const HANDOVER_TEST_MS = BOOT_BUDGET_MS + HANDOVER_BUDGET_MS + BOOT_BUDGET_MS;

/** Mount the real app, with the dev composer, against the real daemon. */
export async function bootLayer(): Promise<MountedApp> {
  return startAgainstRealDaemon({ composer: true });
}

/**
 * Settle until `predicate` holds, or fail naming what never appeared.
 *
 * `settle()` alone answers "the DOM stopped changing", which for a turn still
 * running in another process is true and useless. Each round hands the event
 * loop back so the real socket can deliver; the loop ends the instant the
 * predicate holds. Never a sleep, never a fixed interval.
 */
export async function awaitDrawn(
  app: MountedApp,
  what: string,
  predicate: () => boolean,
  budgetMs = TURN_BUDGET_MS,
): Promise<void> {
  // Date.now() advances with real time here (`shouldAdvanceTime`), so this
  // measures the real budget rather than the page's own fake clock.
  const deadline = Date.now() + budgetMs;
  for (;;) {
    await app.settle();
    if (predicate()) return;
    if (Date.now() >= deadline) {
      throw new Error(
        `${what} was never drawn within ${budgetMs}ms; ` +
          `row kinds drawn: [${drawnKinds(app).join(", ")}]; ` +
          `failure arms: [${app.failureArms().join(", ")}]; ` +
          `refusal arms: [${app.refusalArms().join(", ")}]`,
      );
    }
  }
}

/** Every drawn row's kind (and activity unit), for a diagnostic. */
export function drawnKinds(app: MountedApp): string[] {
  return app.$$("[data-feed-row]").map((row) => {
    const kind = row.dataset.rowKind ?? "?";
    const unit = row.dataset.unit;
    return unit === undefined ? kind : `${kind}.${unit}`;
  });
}

/** Type text into a composer as a user would, then settle. */
export async function type(
  app: MountedApp,
  text: string,
  host = '[data-component="composer"]',
): Promise<void> {
  const input = app.$(`${host} textarea`) as HTMLTextAreaElement | null;
  if (!input) throw new Error(`no composer input at ${host}`);
  input.value = text;
  input.dispatchEvent(new Event("input", { bubbles: true }));
  await app.settle();
}

/** Submit whatever is typed into the given composer. */
export async function send(
  app: MountedApp,
  host = '[data-component="composer"]',
): Promise<void> {
  await app.click(`${host} [data-composer-send]`);
}

/** Type and send one prompt through the app's own composer. */
export async function submit(app: MountedApp, text: string): Promise<void> {
  await type(app, text);
  await send(app);
}

/** The selector for one drawn row kind, optionally one activity unit. */
export function rowSelector(kind: string, unit?: string): string {
  const base = `[data-feed-row][data-row-kind="${kind}"]`;
  return unit === undefined ? base : `${base}[data-unit="${unit}"]`;
}

/** Every drawn element for a row kind (and activity unit). */
export function rows(app: MountedApp, kind: string, unit?: string): HTMLElement[] {
  return app.$$(rowSelector(kind, unit));
}

/**
 * Drive one real turn to completion and answer the newest row of a family.
 *
 * THE TURN COUNT IS READ OFF THE DOM, never held in a module counter: a
 * counter that a failed test left one ahead makes every LATER test in the
 * file fail for a reason that is not its own, which is how one broken
 * assertion became "21 failed" the first time this layer ran.
 */
export async function driveTurn(
  app: MountedApp,
  scenario: string,
  kind: string,
  unit?: string,
): Promise<HTMLElement> {
  // BOTH COUNTS ARE SNAPSHOTTED FIRST. The page accumulates rows across a
  // file's tests, so "a row of this family exists" is already true from an
  // earlier turn: only a row MORE than were standing is this turn's, and
  // waiting on the count is what stops a test asserting on its predecessor's
  // row when its own scenario drew none.
  const beforeFamily = rows(app, kind, unit).length;
  const beforeTurns = rows(app, "turnEnded").length;
  const what = `${kind}${unit === undefined ? "" : "." + unit}`;
  await submit(app, `!${scenario}`);
  await awaitDrawn(
    app,
    `a NEW ${what} row for !${scenario} (${beforeFamily} stood before it)`,
    () => rows(app, kind, unit).length > beforeFamily,
  );
  await awaitDrawn(app, `the turn for !${scenario} to end`, () =>
    rows(app, "turnEnded").length > beforeTurns,
  );
  const drawn = rows(app, kind, unit);
  const last = drawn[drawn.length - 1];
  expect(last, `!${scenario} drew no ${what} row`).toBeDefined();
  return last as HTMLElement;
}

/**
 * Submit `!scenario` and wait until at least one row of the named kind is
 * drawn, answering those rows.
 *
 * The scenario prefix is the fake SDK's own selector (`fake/registry.ts`), the
 * same one every Go area test uses.
 */
export async function driveScenario(
  app: MountedApp,
  scenario: string,
  kind: string,
  unit?: string,
): Promise<HTMLElement[]> {
  await submit(app, `!${scenario}`);
  await awaitDrawn(app, `a ${kind}${unit === undefined ? "" : "." + unit} row for !${scenario}`, () =>
    rows(app, kind, unit).length > 0,
  );
  return rows(app, kind, unit);
}

/** Drive a scenario and answer the LAST row of the named kind it drew. */
export async function driveScenarioRow(
  app: MountedApp,
  scenario: string,
  kind: string,
  unit?: string,
): Promise<HTMLElement> {
  const drawn = await driveScenario(app, scenario, kind, unit);
  const last = drawn[drawn.length - 1];
  expect(last, `!${scenario} drew no ${kind} row`).toBeDefined();
  return last as HTMLElement;
}

/** Wait for the turn to be terminal — its `turn_ended` row is drawn. */
export async function awaitTurnEnded(app: MountedApp, expected = 1): Promise<void> {
  await awaitDrawn(app, `${expected} turn_ended row(s)`, () => rows(app, "turnEnded").length >= expected);
}

/** The trimmed text of an element, for a verbatim assertion. */
export function textOf(element: Element | null | undefined): string {
  return element?.textContent?.trim() ?? "";
}
