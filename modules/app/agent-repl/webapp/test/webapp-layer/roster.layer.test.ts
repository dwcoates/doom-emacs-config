/**
 * §F9 — TRAY, SIDEBAR AND LIFECYCLE, against the real chain.
 *
 * Three surfaces that are not the feed and not the strips:
 *
 *  - the DAEMON-HOLD TRAY, its own region at the feed's tail, whole-list
 *    replaced. A held prompt IS a `conversation.v1.UserSaid` the daemon has
 *    not yet delivered — never a feed row — so the tray is the only place it
 *    can be seen, and `UpdateHeldPrompt` is issued from it.
 *  - the SIDEBAR roster, the ONE global stream (no workspace scoping), with
 *    both groupings arriving resolved as siblings and the client's local
 *    preference picking which to render.
 *  - the LIFECYCLE banner: a drain-scheduled `WatchDaemon` push draws the
 *    standing page-wide restart banner.
 *
 * A HELD PROMPT IS MADE THE WAY THE DAEMON MAKES ONE: a turn that stays open
 * (`hold`, lifecycle.ts — the same scenario permission_e2e_test.go uses to
 * prove a second submission is HELD rather than merely fast), then a second
 * submission behind it.
 */
import { afterAll, beforeAll, expect, it } from "vitest";

import type { MountedApp } from "../integration/harness";
import {
  BOOT_BUDGET_MS,
  TURN_TEST_MS,
  awaitDrawn,
  bootLayer,
  rows,
  submit,
  textOf,
} from "./drive";

let app: MountedApp;

// THE PAGE'S OWN RECORDS ARE FORWARDED FOR THIS AREA, and it is the one area
// that needs them: its subject is a banner drawn from a DAEMON PUSH, so
// "the banner was never drawn" is answerable only by whether
// `lifecycle.shutdown-announced` was written on the page side. Without it a red
// run leaves the daemon's log saying the announcement was published to one
// client and nothing at all saying whether that client acted on it. The cost is
// the one measured in setup.ts (~0.9s on the heaviest file), paid by this file
// alone.
beforeAll(async () => {
  app = await bootLayer({ clientLog: true });
}, BOOT_BUDGET_MS);

afterAll(async () => {
  await app?.stop();
});

/** Release a parked turn through the footer's own interrupt, if one stands. */
async function releaseParkedTurn(): Promise<void> {
  const interrupt = app.$(".footer-clock [data-interrupt]");
  if (!interrupt) return;
  const before = rows(app, "turnEnded").length;
  await app.clickElement(interrupt);
  const confirm = app.$("[data-interrupt-confirm]");
  if (confirm) await app.clickElement(confirm);
  await awaitDrawn(app, "the parked turn to end", () => rows(app, "turnEnded").length > before);
}

// §F9 #36 — the tray draws a real held prompt, and the hold is NOT a feed row.
it(
  "draws a really-held prompt in the tray, and never as a feed row",
  async () => {
    // Arrange — a turn that parks, so the next submission cannot be delivered.
    await submit(app, "!hold");
    await awaitDrawn(app, "the parked turn's own rows", () => rows(app, "userPrompt").length > 0);
    const promptRows = rows(app, "userPrompt").length;

    // Act — a second submission, behind the running one.
    await submit(app, "this one has to wait");

    // Assert — it is in the TRAY, addressed by the turn the daemon minted for
    // it, and the feed grew no row for it.
    await awaitDrawn(app, "the held prompt in the tray", () => app.$$("[data-held-turn]").length > 0);
    const held = app.$$("[data-held-turn]");
    expect(held.length).toBeGreaterThan(0);
    expect(textOf(held[0])).not.toBe("");
    expect(rows(app, "userPrompt").length).toBe(promptRows);
  },
  TURN_TEST_MS,
);

// §F9 #36 (the tray's own verb) — `UpdateHeldPrompt` is issued from the card.
it(
  "discards a held prompt from the tray, and the card leaves the whole-list replacement",
  async () => {
    // Arrange — a standing hold from the previous act, or a fresh one.
    if (app.$$("[data-held-turn]").length === 0) {
      await submit(app, "!hold");
      await awaitDrawn(app, "the parked turn's rows", () => rows(app, "userPrompt").length > 0);
      await submit(app, "another one to wait");
      await awaitDrawn(app, "the held prompt", () => app.$$("[data-held-turn]").length > 0);
    }
    const card = app.$$("[data-held-turn]")[0];
    const turn = card.dataset.heldTurn;
    const action = card.querySelector<HTMLElement>("[data-held-action]");
    expect(action, "the held card drew no action to take").not.toBeNull();

    // Act — the card's own control, which calls UpdateHeldPrompt.
    await app.clickElement(action as HTMLElement);

    // Assert — the daemon replaced the whole list, and this hold is not in it.
    await awaitDrawn(
      app,
      `the hold for turn ${turn} to leave the tray`,
      () => app.$(`[data-held-turn="${turn}"]`) === null || app.$$("[data-held-turn]").length === 0,
    );
    expect(app.refusalArms()).toHaveLength(0);

    // Leave the workspace idle for the tests below.
    await releaseParkedTurn();
  },
  TURN_TEST_MS,
);

// §F9 #37 — the roster is entirely daemon-resolved, over the ONE global
// stream, with both groupings arriving as siblings.
it(
  "draws the workspace roster from the daemon's own global stream",
  async () => {
    // Arrange / Act — boot alone opens the roster stream; this world has one
    // registered workspace, so the roster has exactly one row to resolve.
    await awaitDrawn(app, "the roster", () => app.$("[data-grouping]") !== null);

    // Assert — the rendered grouping is named on the element (the client's
    // local preference picks which resolved sibling to draw).
    const roster = app.$("[data-grouping]");
    expect(roster).not.toBeNull();
    expect(roster?.getAttribute("data-grouping")).not.toBe("");
    expect(textOf(roster)).not.toBe("");
  },
  TURN_TEST_MS,
);

it(
  "offers both resolved groupings and renders the one the picker names",
  async () => {
    // Arrange — both groupings arrive RESOLVED as siblings on the same push,
    // so the picker can offer both without another round trip.
    await awaitDrawn(app, "the roster", () => app.$("[data-grouping]") !== null);
    const picks = app.$$("[data-grouping-pick]").map((pick) => pick.getAttribute("data-grouping-pick"));

    // Assert — the picker names each grouping the daemon resolved, and the
    // rendered one is among them (grouping mode is webview-local, so the
    // rendered value is always one the client could pick).
    expect(picks.length).toBeGreaterThan(1);
    expect(picks).toContain(app.$("[data-grouping]")?.getAttribute("data-grouping"));
    expect(app.failureArms()).toHaveLength(0);
  },
  TURN_TEST_MS,
);

// §F9 #38 — the drain banner is DAEMON-PUSHED, never a client heuristic.
//
// The schedule is set through the app's OWN `UpdateShutdownSchedule` verb (the
// operator surface the contract lists under admin/diagnostics), so the push
// that draws the banner is the real daemon's.
it(
  "draws the page-wide restart banner from the daemon's own drain push",
  async () => {
    // Arrange — the banner host is empty before any schedule exists.
    expect(app.$('[data-component="drain-banner"] [data-restarting]')).toBeNull();

    // Act — schedule a drain through the app's own client.
    await app.ctx.client.updateShutdownSchedule({
      action: {
        case: "schedule",
        value: {
          // Far enough out that nothing in this run is drained; the banner is
          // about the SCHEDULE existing, not about it firing.
          // ON THE DAEMON'S CLOCK, deliberately.
          //
          // Two clocks are in play and they are not the same: the page's
          // ticker starts at HARNESS_EPOCH_MS (ten seconds past the epoch) so
          // the fixtures' timestamps read sensibly, while the daemon runs on
          // the real wall clock. An instant on the PAGE's clock is decades in
          // the DAEMON's past, so it fires the drain immediately — observed as
          // a real `daemon.drain.fire` and a draining daemon, not as a
          // scheduled one. The schedule therefore has to be real-clock, and
          // the consequence is that the banner's COUNTDOWN reads absurdly
          // ("expected back in 496795h") on the page's clock — a harness
          // artifact, which is exactly why the assertions below read the
          // banner's cause and its note rather than its countdown.
          atMs: BigInt(Date.now() + 60 * 60 * 1000),
          // The reason is typed, never a bare string, and the banner names it.
          reason: { kind: { case: "operator", value: { note: "the webapp layer's drain banner" } } },
        },
      },
    });

    // Assert — the banner is drawn from the pushed schedule, names the cause
    // the push carried, and repeats the operator's own note.
    // WAIT ON THE CAUSE, NOT ON THE HOST BEING NON-EMPTY. Every notice this
    // host draws wears `data-restarting` (lifecycle.ts's `notice()` sets it on
    // all three), and only the RESTARTING notice carries `data-shutdown-cause`
    // — the drain-scheduled notice wears `data-drain-scheduled` instead, and
    // the moved notice `data-moved`. Since the daemon fires this schedule
    // immediately (see the two-clocks note above), the notice this test is
    // about is the restarting one, and a wait on `[data-restarting]` would
    // stop at whichever notice happened to be standing first and then read a
    // null cause. Waiting on the cause attribute makes the assertion
    // order-independent.
    // THE FAILURE HAS TO SPLIT THE TWO CAUSES ITSELF. "never drawn" leaves a
    // reader unable to tell a push that never arrived from one that arrived and
    // drew the wrong notice, and this area's own artifacts carry no page-side
    // record to settle it -- which is exactly where one red run's diagnosis
    // stopped. The banner host's own markup is the fact that separates them: an
    // EMPTY host means no lifecycle push reached this page at all, and a host
    // holding `data-drain-scheduled` means the schedule arrived and the
    // announcement did not (or did not take precedence, which lifecycle.ts's
    // `redraw` says it must).
    const bannerHost = (): string =>
      app.$('[data-component="drain-banner"]')?.innerHTML ?? "(no drain-banner host)";
    try {
      await awaitDrawn(
        app,
        "the drain banner",
        () => app.$('[data-component="drain-banner"] [data-shutdown-cause]') !== null,
      );
    } catch (error) {
      throw new Error(`${String(error)}; drain-banner host: ${bannerHost()}`, { cause: error });
    }
    const banner = app.$('[data-component="drain-banner"] [data-shutdown-cause]');
    expect(banner).not.toBeNull();
    expect(banner?.getAttribute("data-shutdown-cause")).toBe("scheduledDrain");
    // The operator's own note travelled the whole way — app -> daemon ->
    // WatchDaemon push -> banner — and is drawn verbatim, which is the fact
    // that proves this banner came from the daemon rather than from a client
    // heuristic.
    expect(textOf(banner)).toContain("the webapp layer's drain banner");
  },
  TURN_TEST_MS,
);
