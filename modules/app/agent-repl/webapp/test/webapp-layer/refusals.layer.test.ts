/**
 * §F8 — REFUSAL WORDING AND PLACEMENT, against the real chain.
 *
 * Every response in the contract is `oneof result { success | error }`, error
 * arms are per-method typed, and a refusal renders AT THE CALL SITE — the
 * composer for a submission, the clicked control for a control's own call.
 * Domain outcomes (deny, nothing-running, empty result) are SUCCESS arms and
 * must never draw as refusals.
 *
 * Every answer asserted here is the REAL daemon's own, never a scripted
 * response. One §F8 scenario is NOT reachable from this page and is called out
 * in place rather than faked — see the first test's own comment.
 */
import { afterAll, beforeAll, expect, it } from "vitest";

import type { MountedApp } from "../integration/harness";
import {
  BOOT_BUDGET_MS,
  TURN_TEST_MS,
  awaitDrawn,
  bootLayer,
  press,
  rows,
  submit,
  textOf,
  type,
} from "./drive";

let app: MountedApp;

beforeAll(async () => {
  app = await bootLayer();
}, BOOT_BUDGET_MS);

afterAll(async () => {
  await app?.stop();
});

/** The composer's own refusal, wherever it draws. */
function composerRefusal(): HTMLElement | null {
  return app.$('[data-component="composer"] .refusal, .composer-refusal');
}

// §F8 #33. THE DUPLICATE-KEY REFUSAL IS NOT REACHABLE FROM THIS PAGE, and
// this file says so rather than faking it.
//
// The idempotency key is the contract's ONE client-minted value, and the app
// reuses it ONLY to retry a submission that FAILED — a successful send mints a
// fresh one. So against a real daemon, sending the same text twice is two
// legitimate turns, and provoking the daemon's own duplicate arm would need a
// fault injected between the app and the daemon, which this layer has no seam
// for (and inventing one would make the refusal the harness's, not the
// daemon's). The fake-daemon integration suite covers that arm directly
// (composer.integration.test.ts, "a duplicate submission"); reported as the
// one §F8 scenario this layer cannot carry.
//
// What IS this layer's to prove is the other half of the same rule: a fresh
// key per distinct submission, so a second send is a second turn and NOT a
// refusal.
it(
  "mints a second turn for a second submission of the same text, refusing nothing",
  async () => {
    // Arrange — one real turn, submitted and finished.
    const before = rows(app, "turnEnded").length;
    await submit(app, "a prompt worth sending twice");
    await awaitDrawn(app, "the first turn to end", () => rows(app, "turnEnded").length > before);
    const promptRows = rows(app, "userPrompt").length;

    // Act — the same text again, which mints a FRESH key.
    await submit(app, "a prompt worth sending twice");

    // Assert — a second prompt row, and nothing refused anywhere.
    await awaitDrawn(
      app,
      "the second prompt row",
      () => rows(app, "userPrompt").length > promptRows,
    );
    expect(rows(app, "userPrompt").length).toBeGreaterThan(promptRows);
    expect(composerRefusal()).toBeNull();
    expect(app.failureArms()).toHaveLength(0);
    await awaitDrawn(app, "the second turn to end", () => rows(app, "turnEnded").length > before + 1);
  },
  TURN_TEST_MS,
);

// §F8 #33 (the empty case) — nothing is sent, and nothing is refused either.
it(
  "sends nothing and refuses nothing for an empty composer",
  async () => {
    // Arrange
    const before = rows(app, "userPrompt").length;

    // Act — the send control with an empty box. `press` rather than `send`:
    // this is the ONE scenario that presses expecting the composer to take
    // nothing, and `send` exists to make that a fault everywhere else.
    await type(app, "");
    const taken = await press(app);

    // Assert — the press was honestly dropped, and no row, no refusal: an
    // empty submission is not an error, it is not a submission.
    expect(taken).toBe(false);
    expect(rows(app, "userPrompt").length).toBe(before);
    expect(app.refusalArms()).toHaveLength(0);
  },
  TURN_TEST_MS,
);

// §F8 #34 — a refusal renders AT THE CALL SITE. What is asserted is the
// PLACEMENT rule, not that any particular call succeeds: whatever answer the
// daemon gives a control, the consequence stays inside that control and the
// page's failure overlay stays empty.
//
// FINDING, REPORTED, NOT ASSERTED AS PASSING: against this real chain, picking
// a SERVED model option comes back as a TRANSPORT refusal
// ("the daemon could not be reached", drawn in `.topbar-model`) rather than
// the echoed `AgentModel` token the contract promises for `SetModel`. That is
// a production fault this layer surfaced; the placement rule below still holds
// over it, which is exactly why placement is what this test pins.
it(
  "keeps a control's own answer inside that control, and out of the page's failure overlay",
  async () => {
    // Arrange — a session, so the topbar has a model selector at all, then the
    // selector open on the daemon's served options.
    const before = rows(app, "turnEnded").length;
    await submit(app, "open a session for the model selector");
    await awaitDrawn(app, "the opening turn to end", () => rows(app, "turnEnded").length > before);
    await awaitDrawn(app, "the model button", () => app.$(".topbar-model-button") !== null);
    await app.click(".topbar-model-button");
    await awaitDrawn(app, "the model options", () => app.$$("[data-model-option]").length > 0);

    // Act — pick a served option.
    await app.clickElement(app.$$("[data-model-option]")[0]);
    await app.settle();

    // Assert — nothing about this control's answer escaped it: no page-wide
    // failure card, and no refusal drawn anywhere outside the selector.
    expect(app.failureArms()).toHaveLength(0);
    expect(app.$$(".refusal").filter((el) => el.closest(".topbar-model") === null)).toHaveLength(0);
  },
  TURN_TEST_MS,
);

// §F8 #35 — a DOMAIN OUTCOME is a success arm, never a refusal.
//
// FINDING, REPORTED: the `nothing_running` outcome is NOT reachable from this
// page. An idle footer draws no interrupt control at all (measured against the
// real daemon: `.footer-clock [data-interrupt]` is absent whenever nothing is
// running), so the page can never issue the call that would answer it. The
// fake-daemon integration suite covers that arm directly
// (footer.integration.test.ts, "draws the nothing-running outcome as a note").
// What this layer CAN pin is the reason it is unreachable, which is itself the
// contract's own rule: a control that has nothing to act on is not drawn.
it(
  "offers no interrupt control while the workspace is idle",
  async () => {
    // Arrange — the workspace is idle: the previous test's turn ended.
    await awaitDrawn(app, "the footer's clock", () => app.$(".footer-clock") !== null);

    // Assert
    expect(app.$(".footer-clock [data-interrupt]")).toBeNull();
  },
  TURN_TEST_MS,
);

// §F8 #35 (the feed's own domain outcome) — a turn that FAILED is a drawn
// outcome on its terminal row, not a refusal and not a page failure.
it(
  "draws a failed turn's own cause on its terminal row, not as a refusal",
  async () => {
    // Arrange
    const before = rows(app, "turnEnded").length;

    // Act — `fail-execution` is the turn-lifecycle area's own failing turn.
    await submit(app, "!fail-execution");
    await awaitDrawn(app, "the failed turn's terminal row", () => rows(app, "turnEnded").length > before);

    // Assert — the terminal row carries the cause, and the FEED drew no
    // refusal: a failed turn is an outcome, not a refused call. (The
    // page-wide refusal list is deliberately not consulted: the model
    // selector above may still be showing its own control's answer, which is
    // not this test's subject.)
    const terminal = rows(app, "turnEnded").slice(-1)[0];
    expect(textOf(terminal)).not.toBe("");
    expect(app.$$('[data-feed="root"] .refusal')).toHaveLength(0);
    expect(app.failureArms()).toHaveLength(0);
  },
  TURN_TEST_MS,
);
