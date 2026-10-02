/**
 * §F4 — PERMISSION AND QUESTION CARDS, against the real chain.
 *
 * These are the two families where the PAGE is a participant rather than a
 * viewer: the real shim asks, the card draws with its controls, the user
 * clicks, the app calls `AnswerPermission`/`AnswerQuestion`, and the card's
 * new state arrives on the FEED PUSH rather than in the rpc's response. No
 * layer below the webapp can cover that round trip.
 *
 * A card scenario deliberately does NOT end its turn: the turn is blocked on
 * the ask, which is why every wait here is on the card (or its answered
 * state), never on `turn_ended`.
 *
 * SCENARIOS ARE THE GO SUITE'S OWN: `perm-hold`, `perm-allow-standing-mode`,
 * `perm-deny-policy`, `ask-single`, `ask-multi`, `ask-free` and
 * `ask-unanswered` are all driven by `permission_e2e_test.go` /
 * `questions_e2e_test.go`.
 */
import { afterAll, afterEach, beforeAll, expect, it } from "vitest";

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

beforeAll(async () => {
  app = await bootLayer();
}, BOOT_BUDGET_MS);

afterAll(async () => {
  await app?.stop();
});

/**
 * LEAVE THE WORKSPACE IDLE, or the next test's prompt is never delivered.
 *
 * A card scenario blocks its turn on the ask, and the daemon HOLDS every
 * prompt submitted while a turn runs. So a test that opens a card and walks
 * away starves every test after it — which is exactly what happened the first
 * time this file ran: one standing `perm-hold` card, eight failures that were
 * not about their own subjects.
 *
 * TWO STEPS, BOTH THROUGH THE PAGE'S OWN CONTROLS:
 *
 *  1. Answer whatever is still open the way a user would — deny a permission,
 *     submit a question's free-text escape.
 *  2. If the turn STILL has not ended, interrupt it from the footer's own
 *     clock control. This is not belt-and-braces: `perm-hold` is documented
 *     (permissions.ts, and permission_e2e_test.go's own header) as a turn that
 *     PARKS however the ask resolves — "the only terminal it can reach is an
 *     interrupt's" — so for that scenario the interrupt IS the release, and an
 *     answer alone would hang the file.
 */
async function releaseWorkspace(): Promise<void> {
  const openPermissions = app.$$(
    '[data-feed-row][data-row-kind="permission"] [data-permission="deny"]',
  );
  const openQuestions = app.$$(
    '[data-feed-row][data-row-kind="question"] [data-question-submit]',
  );
  for (const deny of openPermissions) await app.clickElement(deny);
  for (const submitButton of openQuestions) {
    const card = submitButton.closest("[data-feed-row]");
    const other = card?.querySelector<HTMLInputElement>(
      "[data-question-other]",
    );
    if (other) {
      other.value = "teardown answer";
      other.dispatchEvent(new Event("input", { bubbles: true }));
      await app.settle();
    }
    await app.clickElement(submitButton);
  }
  if (everyTurnEnded()) return;

  // THE FOOTER'S OWN INTERRUPT, once it says the turn runs: a parked turn
  // offers it, and a turn that the answer above is ending lets the wait end
  // without it.
  await awaitDrawn(
    app,
    "the running turn's interrupt control, or its end",
    () => everyTurnEnded() || app.$(".footer-clock [data-interrupt]") !== null,
  );
  if (!everyTurnEnded()) {
    const interrupt = app.$(".footer-clock [data-interrupt]");
    if (interrupt) await app.clickElement(interrupt);
    const confirm = app.$("[data-interrupt-confirm]");
    if (confirm) await app.clickElement(confirm);
  }
  await awaitDrawn(
    app,
    "the parked turn to end after the page interrupted it",
    everyTurnEnded,
  );
}

/**
 * Whether every turn this file started has ended, read off the FEED alone:
 * one prompt row and one turn-ended row per turn. It is never read off the
 * footer, a separate stream that may still be catching up with a turn end
 * the feed has already drawn -- which once sent this teardown to interrupt a
 * turn that had already ended, and to wait 5s for an end that had come.
 */
function everyTurnEnded(): boolean {
  return rows(app, "turnEnded").length >= rows(app, "userPrompt").length;
}

afterEach(async () => {
  await releaseWorkspace();
}, TURN_TEST_MS);

/**
 * Drive an asking scenario and answer the card row it added.
 *
 * The wait is on the card COUNT rising, so a card standing from an earlier
 * test in this file is never mistaken for this scenario's.
 */
async function ask(
  scenario: string,
  kind: "permission" | "question",
): Promise<HTMLElement> {
  const before = rows(app, kind).length;
  await submit(app, `!${scenario}`);
  await awaitDrawn(
    app,
    `a new ${kind} card for !${scenario}`,
    () => rows(app, kind).length > before,
  );
  const drawn = rows(app, kind);
  return drawn[drawn.length - 1];
}

// §F4 #19.
it(
  "draws a real permission ask with its own action buttons",
  async () => {
    // Arrange / Act — a real ask from the real shim.
    const card = await ask("perm-hold", "permission");

    // Assert — the card's controls are drawn, and it is waiting.
    expect(card.querySelectorAll("[data-permission]").length).toBeGreaterThan(
      0,
    );
    expect(card.querySelector("[data-permission-reason]")).not.toBeNull();
    expect(card.querySelector(".perm-waiting")).not.toBeNull();
  },
  TURN_TEST_MS,
);

// §F4 #19 (second half) — the standing-allow TOKEN never reaches the client;
// only the PRESENCE of an offer may make the standing button drawable.
it(
  "draws the standing-allow button only for an ask that offered a standing form",
  async () => {
    // Arrange / Act
    const offered = await ask("perm-allow-standing-mode", "permission");

    // Assert — the button's existence is the whole visible consequence of the
    // offer; no token is anywhere in the card's markup.
    expect(
      offered.querySelector('[data-permission="allowStanding"]'),
    ).not.toBeNull();
  },
  TURN_TEST_MS,
);

// §F4 #20.
it(
  "answers a permission from the card, and draws the answered state pushed back onto the same row",
  async () => {
    // Arrange
    const card = await ask("perm-hold", "permission");
    const id = card.dataset.feedRow;
    expect(id).toBeDefined();
    const allow = card.querySelector<HTMLElement>(
      '[data-permission="allowOnce"]',
    );
    expect(allow, "the open card drew no allow-once button").not.toBeNull();

    // Act — the app's own AnswerPermission call.
    await app.clickElement(allow as HTMLElement);

    // Assert — the ANSWERED state arrives on the feed push and is drawn on the
    // same row (a cold repaint from the row alone renders it), with the
    // verdict naming its own arm.
    await awaitDrawn(
      app,
      `the answered state on permission row ${id}`,
      () => app.$(`[data-feed-row="${id}"] .perm-verdict`) !== null,
    );
    const verdict = app.$(`[data-feed-row="${id}"] .perm-verdict`);
    expect(verdict).not.toBeNull();
    expect(verdict?.getAttribute("data-arm")).not.toBe("");
    // The controls are gone: an answered card is not answerable twice.
    expect(app.$$(`[data-feed-row="${id}"] [data-permission]`)).toHaveLength(0);
  },
  TURN_TEST_MS,
);

// §F4 #20 (the deny path) — a DENY is a domain outcome, so it draws as a
// verdict on the card and never as a refusal.
it(
  "draws a policy denial as the card's own verdict, not as a refusal",
  async () => {
    // Arrange — `perm-deny-policy` is denied by policy inside the shim, so
    // the daemon resolves the call itself and the card never opens.
    const before = rows(app, "activity", "simpleToolCall").length;

    // Act
    await submit(app, "!perm-deny-policy");

    // Assert — the tool call carries the outcome; nothing draws a refusal.
    await awaitDrawn(
      app,
      "the denied tool call",
      () => rows(app, "activity", "simpleToolCall").length > before,
    );
    expect(app.refusalArms()).toHaveLength(0);
  },
  TURN_TEST_MS,
);

// §F4 #21.
it(
  "always draws a question's free-text escape alongside its options",
  async () => {
    // Arrange / Act — a single-choice ask, which still gets the escape.
    const card = await ask("ask-single", "question");

    // Assert
    expect(
      card.querySelectorAll("[data-question-option]").length,
    ).toBeGreaterThan(0);
    expect(card.querySelector("[data-question-other]")).not.toBeNull();
    expect(card.querySelector("[data-question-submit]")).not.toBeNull();
  },
  TURN_TEST_MS,
);

it(
  "draws a multi-choice question in its own mode",
  async () => {
    // Arrange / Act
    const card = await ask("ask-multi", "question");

    // Assert — the mode rides the block, so the client draws it rather than
    // inferring it from the options.
    const block = card.querySelector<HTMLElement>("[data-question-mode]");
    expect(block).not.toBeNull();
    expect(block?.dataset.questionMode).not.toBe("");
  },
  TURN_TEST_MS,
);

it(
  "answers a question through the free-text escape rather than an option",
  async () => {
    // Arrange — `ask-free` still draws whatever options the ask carried (the
    // escape is ALWAYS drawn, it is not an option-less mode), so this test is
    // about answering THROUGH the escape.
    const card = await ask("ask-free", "question");
    const id = card.dataset.feedRow;
    const other = card.querySelector<HTMLInputElement>("[data-question-other]");
    expect(other, "the free-text escape is always drawn").not.toBeNull();

    // Act — type into the escape and submit, picking no option at all.
    (other as HTMLInputElement).value = "something else entirely";
    (other as HTMLInputElement).dispatchEvent(
      new Event("input", { bubbles: true }),
    );
    await app.settle();
    await app.click(`[data-feed-row="${id}"] [data-question-submit]`);

    // Assert — the card closes on the answer the page sent.
    await awaitDrawn(
      app,
      `the answered state on question row ${id}`,
      () => app.$(`[data-feed-row="${id}"] [data-question-submit]`) === null,
    );
    expect(app.$(`[data-feed-row="${id}"] [data-state]`)).not.toBeNull();
  },
  TURN_TEST_MS,
);

// §F4 #20 for questions — answering from the card, with the new state pushed
// back onto the same row.
it(
  "answers a question from the card, and draws the answered state on the same row",
  async () => {
    // Arrange
    const card = await ask("ask-single", "question");
    const id = card.dataset.feedRow;
    const option = card.querySelector<HTMLElement>("[data-question-option]");
    expect(option, "the open card drew no option to pick").not.toBeNull();

    // Act — pick an option, then submit through the card's own control.
    await app.clickElement(option as HTMLElement);
    await app.click(`[data-feed-row="${id}"] [data-question-submit]`);

    // Assert — the card states its new state, and the submit control is gone.
    await awaitDrawn(
      app,
      `the answered state on question row ${id}`,
      () => app.$(`[data-feed-row="${id}"] [data-question-submit]`) === null,
    );
    const answered = app.$(`[data-feed-row="${id}"] [data-state]`);
    expect(answered).not.toBeNull();
    expect(textOf(app.$(`[data-feed-row="${id}"]`))).not.toBe("");
  },
  TURN_TEST_MS,
);

// §F4 #22 — an EXPIRED ask draws as expired, never as pending.
it(
  "draws an unanswered ask's own state rather than leaving it pending",
  async () => {
    // Arrange / Act — `ask-unanswered` is the ask the shim never gets an
    // answer for; the daemon resolves its state, the client only draws it.
    const card = await ask("ask-unanswered", "question");

    // Assert — the card states a state of its own, drawn from the row alone.
    const stated = card.querySelector<HTMLElement>("[data-state]");
    expect(stated).not.toBeNull();
    expect(stated?.dataset.state).not.toBe("");
  },
  TURN_TEST_MS,
);

// §F4 — landing 10: the undecidable denial is its OWN verdict, not the policy
// one. `!perm-undecidable` is a gated call in auto mode whose classifier
// reaches no verdict, so nobody — no rule, no user — refused it.
it(
  "draws an undecidable denial under its own verdict value",
  async () => {
    // Arrange
    const before = rows(app, "permission").length;

    // Act
    await submit(app, "!perm-undecidable");

    // Assert
    await awaitDrawn(
      app,
      "the undecidable permission verdict",
      () => rows(app, "permission").length > before,
    );
    const drawn = rows(app, "permission");
    const card = drawn[drawn.length - 1];
    expect(
      card
        .querySelector(".perm-verdict")
        ?.getAttribute("data-permission-verdict"),
    ).toBe("deniedUndecidable");
  },
  TURN_TEST_MS,
);
