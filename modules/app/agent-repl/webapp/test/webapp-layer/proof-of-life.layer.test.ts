/**
 * PROOF OF LIFE (§F1) — the whole chain, end to end, through the real webapp.
 *
 *   fake SDK -> real shim -> real store + real sidecar -> real claude-repld
 *            -> real webapp (this file, in jsdom)
 *
 * One scenario: a prompt typed into the webapp's OWN composer and sent, and
 * the response row drawn in the real webapp DOM. The submission is the app's
 * own — it mints the idempotency key, sets the webapp origin and carries the
 * workspace — and the row is the daemon's own push, so nothing here is
 * arranged by the test.
 *
 * THE PROMPT IS BARE PROSE, deliberately. With no `!scenario` prefix the fake
 * SDK runs its default PROSE turn (`src/fake/scenarios/prose.ts`), whose
 * conclusion is `echo: <prompt> [mode=...] [model=...]` — a string derived
 * from what THIS page sent, so a drawn row carrying it cannot have come from
 * anywhere but this submission travelling the whole chain and back.
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

beforeAll(async () => {
  app = await bootLayer();
}, BOOT_BUDGET_MS);

afterAll(async () => {
  await app?.stop();
});

it(
  "draws the real daemon's response to a prompt sent from its own composer",
  async () => {
    // Arrange — the app is mounted against the real daemon (beforeAll);
    // nothing about the daemon's state is scripted.
    const prompt = "webapp layer proof of life";

    // Act — the app's own send path: SubmitPrompt over the real transport.
    await submit(app, prompt);

    // Assert — the prompt row the daemon pushed back, then the response row
    // the real shim's answer became.
    await awaitDrawn(app, "the user prompt row", () =>
      rows(app, "userPrompt").some((row) => textOf(row).includes(prompt)),
    );
    await awaitDrawn(app, "the response row", () =>
      rows(app, "activity", "response").some((row) => textOf(row).includes(`echo: ${prompt}`)),
    );
    expect(rows(app, "activity", "response").map(textOf).join("\n")).toContain(`echo: ${prompt}`);
  },
  TURN_TEST_MS,
);
