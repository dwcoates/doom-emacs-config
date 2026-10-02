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
import { type Control } from "../../src/control.js";
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

/** What a file may ask of its own mount, beyond the layer's defaults. */
export interface BootOptions {
  /**
   * Mount with PRODUCTION'S OWN `ClientLog` sink, so every record this page
   * emits is forwarded to the real daemon and lands in the workspace's
   * `webapp.log`.
   *
   * OFF BY DEFAULT AND OPT-IN PER FILE, on the measurement recorded in
   * `setup.ts`: forwarding costs every file real time (+50% across the eleven
   * of them, +0.9s on the heaviest) and only the file that asserts about the
   * forwarding needs it.
   */
  clientLog?: boolean;
}

/** Mount the real app, with the dev composer, against the real daemon. */
export async function bootLayer(options: BootOptions = {}): Promise<MountedApp> {
  return startAgainstRealDaemon({ composer: true, clientLog: options.clientLog === true });
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
  await awaitSettled(app, `${what} was never drawn`, predicate, budgetMs);
}

/**
 * The settle loop `awaitDrawn` is made of, with the caller supplying the whole
 * phrase rather than a row's name.
 *
 * SPLIT OUT rather than copied: `send` waits on the composer, which is not a
 * row and does not read as "was never drawn", and a second hand-rolled loop is
 * a second place for a sleep to appear later. The diagnostic is the same one
 * either way — the round count and the drawn rows are what tell a starved page
 * from an unanswered one, whichever wait timed out.
 */
async function awaitSettled(
  app: MountedApp,
  phrase: string,
  predicate: () => boolean,
  budgetMs: number,
): Promise<void> {
  // Date.now() advances with real time here (`shouldAdvanceTime`), so this
  // measures the real budget rather than the page's own fake clock.
  const started = Date.now();
  const deadline = started + budgetMs;
  // HOW MANY TIMES THE LOOP ACTUALLY RAN, which is what separates a chain that
  // did not answer from a PAGE THAT NEVER GOT THE CPU TO ASK. This suite runs
  // `-parallel 8` worlds beside up to three vitest children, and a red run has
  // been observed where the daemon, the shim and the store were all silent for
  // 5.23 wall-clock seconds because this node process was descheduled for the
  // whole budget: the submission was never sent, so nothing downstream had
  // anything to answer. A wall-clock budget cannot tell those apart on its own,
  // and the round count can: an ordinary 5s budget completes hundreds.
  let rounds = 0;
  for (;;) {
    await app.settle();
    rounds++;
    if (predicate()) return;
    if (Date.now() >= deadline) {
      throw new Error(
        `${phrase} within ${budgetMs}ms ` +
          `(${rounds} settle rounds in ${Date.now() - started}ms of wall clock; ` +
          `a low count for the budget means this page was starved of CPU rather than left unanswered); ` +
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

/**
 * How long the composer is given to be ready to take the next press.
 *
 * REUSED, NOT MINTED: what this waits out is the TAIL OF THE PREVIOUS
 * SUBMISSION — `composer.ts` disables the button for the whole of its
 * `SubmitPrompt` unary and re-enables it in that call's `finally` — so the
 * thing being bounded is one real unary against a loopback daemon, which is
 * strictly less than the turn TURN_BUDGET_MS already bounds. It takes that
 * budget rather than inventing a third number.
 *
 * ON A HEALTHY CHAIN THIS WAIT IS ZERO ROUNDS: `driveTurn` only returns once
 * the turn's own rows are drawn, and the unary answers long before them. It
 * bounds a composer that is CLOSED (the footer reading merging, closing or
 * disconnected), which is a fault to name rather than a press to drop.
 */
export const COMPOSER_READY_BUDGET_MS = TURN_BUDGET_MS;

/**
 * Submit whatever is typed into the given composer.
 *
 * THE PRESS IS WAITED FOR AND THEN PROVEN, because `composer.ts` DROPS a press
 * it cannot take: `submit()` returns silently when the button is disabled,
 * when a submission is still in flight, or when the box is empty. A dropped
 * press sent nothing, so the turn it was for never started and the test that
 * waited for its row failed five seconds later naming the ROW — a chain that
 * was never asked anything read as a chain that never answered.
 *
 * MEASURED, on the run that produced this fix (`feed-families.layer.test.ts`,
 * `!rotate`, 2026-09-10): the page completed 11514 settle rounds inside its 5s
 * budget with NOTHING in flight, and the shim recorded no `StartTurn` at all
 * between the previous turn's (16:32:39.258) and the NEXT test's
 * (16:32:44.446). The press had been dropped: the harness's in-flight set
 * clears when a response HEAD lands, while the composer stays `inFlight` until
 * the whole unary resolves, so `settle()` could report the page quiet with the
 * button still disabled.
 *
 * Two things close it, and both are here rather than in the app — a composer
 * that ignores a press while one submission is in flight is what production
 * WANTS:
 *
 *   * the press WAITS for a pressable button, so the window cannot be entered;
 *   * the press is CHECKED, synchronously, before anything settles. `submit()`
 *     disables the button in the click handler itself, so a button still
 *     enabled on the next line means the press was dropped — and it is named
 *     here, at the press, instead of surfacing as a missing row later.
 */
export async function send(
  app: MountedApp,
  host = '[data-component="composer"]',
): Promise<void> {
  const typed = typedText(app, host);
  if (await press(app, host)) return;
  throw new Error(
    `the composer at ${host} DROPPED the press and submitted nothing ` +
      `(composer.ts returns silently when the box is empty, when the gate is ` +
      `closed, or while a submission is in flight); ` +
      `the box held ${JSON.stringify(typed)}; ` +
      `refusal arms: [${app.refusalArms().join(", ")}]`,
  );
}

/**
 * Press Send and answer whether the composer TOOK the press, for the one
 * caller that presses expecting nothing to happen.
 *
 * `send` is this plus "and it must have been taken", which is what every
 * scenario that drives a turn wants. §F8 #33 — the empty box — wants the other
 * half: the control pressed, and honestly nothing submitted.
 */
export async function press(
  app: MountedApp,
  host = '[data-component="composer"]',
): Promise<boolean> {
  const selector = `${host} [data-composer-send]`;
  const button = (): Control | null =>
    app.$(selector) as Control | null;
  if (button() === null) throw new Error(`no composer send button at ${selector}`);

  await awaitSettled(
    app,
    `the composer send button at ${selector} never became pressable`,
    () => button()?.disabled === false,
    COMPOSER_READY_BUDGET_MS,
  );

  const pressed = button();
  if (pressed === null) throw new Error(`the composer send button at ${selector} went away`);
  // NOT `app.click`, which settles: the answer below must be read BEFORE the
  // event loop turns, or the unary this press starts could already have
  // answered and re-enabled the button.
  pressed.click();
  const taken = pressed.disabled;
  await app.settle();
  return taken;
}

/** What is typed in a composer right now, for a diagnostic. */
function typedText(app: MountedApp, host: string): string {
  const input = app.$(`${host} textarea`) as HTMLTextAreaElement | null;
  return input?.value ?? "";
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
  // THE ROWS ARE IDENTIFIED, NOT COUNTED, AND NOT POSITIONED. The page
  // accumulates rows across a file's tests, so "a row of this family exists"
  // is already true from an earlier turn; and the feed UPSERTS and reorders,
  // so "the last row of this family" is not reliably the one this turn drew
  // either. Counting alone was enough to see a row appear, and then the last
  // row read back as a PREDECESSOR'S: `!rotate` asserted its `cleared`
  // separation and read the previous test's `compacted` one, on the e2e host
  // run. Every drawn row carries the daemon's own FeedRow id on
  // `data-feed-row` and an upsert keeps it, so the ids that were not standing
  // before are exactly this turn's rows.
  const standing = new Set(rows(app, kind, unit).map(rowID));
  // THE TURN'S END IS IDENTIFIED TOO, for the same reason the family's row is
  // and for one more: a scenario that CUTS CONTEXT — `!compact`, `!rotate` —
  // makes the page drop every row above its divider, so the terminal row count
  // after the turn can be LOWER than the count before it. A "one more than
  // before" wait then never comes true and the turn reads as never having
  // ended, which is how this helper first met the feed's own start bound.
  const standingTurns = new Set(rows(app, "turnEnded").map(rowID));
  const what = `${kind}${unit === undefined ? "" : "." + unit}`;
  await submit(app, `!${scenario}`);
  await awaitDrawn(
    app,
    `a NEW ${what} row for !${scenario} (${standing.size} stood before it)`,
    () => rows(app, kind, unit).some((row) => !standing.has(rowID(row))),
  );
  await awaitDrawn(app, `the turn for !${scenario} to end`, () =>
    rows(app, "turnEnded").some((row) => !standingTurns.has(rowID(row))),
  );
  const drawn = rows(app, kind, unit).filter((row) => !standing.has(rowID(row)));
  const last = drawn[drawn.length - 1];
  expect(last, `!${scenario} drew no ${what} row`).toBeDefined();
  return last;
}

/**
 * A drawn row's own identity: the FeedRow id the daemon minted for it, which
 * `rowSelector` already requires every matched element to carry.
 *
 * EXPORTED because identification is not `driveTurn`'s alone: any helper that
 * asks "which row did MY submission draw" needs it, and the ones that counted
 * and then read the last row by position have all been wrong in the same way.
 */
export function rowID(row: HTMLElement): string {
  const id = row.dataset.feedRow;
  expect(id, "a drawn feed row carries no FeedRow id").toBeTruthy();
  return id as string;
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
  return last;
}

/** Wait for the turn to be terminal — its `turn_ended` row is drawn. */
export async function awaitTurnEnded(app: MountedApp, expected = 1): Promise<void> {
  await awaitDrawn(app, `${expected} turn_ended row(s)`, () => rows(app, "turnEnded").length >= expected);
}

/** The trimmed text of an element, for a verbatim assertion. */
export function textOf(element: Element | null | undefined): string {
  return element?.textContent?.trim() ?? "";
}
