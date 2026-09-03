/**
 * PROOF OF LIFE — the whole chain, end to end, through the real webapp.
 *
 *   fake SDK -> real shim -> real store + real sidecar -> real claude-repld
 *            -> real webapp (this file, in jsdom)
 *
 * One scenario (`WEBAPP-LAYER-SPEC.md` §F1): a prompt typed into the webapp's
 * OWN composer and sent, and the response row drawn in the real webapp DOM.
 * The submission is the app's own — the app mints the idempotency key, sets
 * the webapp origin and carries the workspace — and the row is the daemon's
 * own push, so nothing here is arranged by the test.
 *
 * THE PROMPT IS BARE PROSE, deliberately. With no `!scenario` prefix the fake
 * SDK runs its default PROSE turn (`src/fake/scenarios/prose.ts`), whose
 * conclusion is `echo: <prompt> [mode=...] [model=...]` — a string derived
 * from what THIS page sent, so a drawn row carrying it cannot have come from
 * anywhere but this submission travelling the whole chain and back.
 */
import { afterAll, beforeAll, expect, it } from "vitest";

import type { MountedApp } from "../integration/harness";
import { startAgainstRealDaemon } from "./real-daemon";

/**
 * How long one real turn is given to come back drawn.
 *
 * REUSED, NOT MINTED: 5s is `harness.DefaultTimeout` in the Go suite, sized
 * off a 443-test daemon/integration run (median 0.2s, observed max 1.7s) and
 * independently corroborated by the old `daemon/e2e`'s own ~0.65s-per-await
 * baseline for exactly this shape — a real node-shim spawn plus a turn
 * through a real store. This layer's turn is that same shape with jsdom
 * drawing on the end, so it inherits that budget rather than inventing a
 * third number. It bounds a HANG: nothing sleeps, the loop below is
 * `settle()`, and it returns the instant the row is drawn.
 */
const TURN_BUDGET_MS = 5_000;

/**
 * How long the page is given to boot against the real daemon.
 *
 * Boot is one real `AdoptWebWorkspace` round trip plus the accept of every
 * `Watch*` stream the six mounts open, against a daemon on loopback. Same
 * grounding as TURN_BUDGET_MS (the Go suite's measured per-rpc budget), minus
 * the shim spawn, which boot does not pay.
 */
const BOOT_BUDGET_MS = 5_000;

let app: MountedApp;

beforeAll(async () => {
  app = await startAgainstRealDaemon({ composer: true });
}, BOOT_BUDGET_MS);

afterAll(async () => {
  await app?.stop();
});

/**
 * Settle until `predicate` holds, or fail naming what never appeared.
 *
 * `settle()` alone answers "the DOM stopped changing", which for a turn still
 * running in another process is true and useless. This re-settles until the
 * thing the test is about is DRAWN — never a sleep, never a fixed interval:
 * each round hands the event loop back so the real socket can deliver, and
 * the loop ends on the first round the predicate holds.
 */
async function awaitDrawn(what: string, predicate: () => boolean): Promise<void> {
  // Date.now() advances with real time here (`shouldAdvanceTime`), so this
  // measures the real budget rather than the page's own fake clock.
  const deadline = Date.now() + TURN_BUDGET_MS;
  for (;;) {
    await app.settle();
    if (predicate()) return;
    if (Date.now() >= deadline) {
      throw new Error(
        `${what} was never drawn within ${TURN_BUDGET_MS}ms; ` +
          `rows drawn: [${app.rowIds().join(", ")}]; ` +
          `failure arms: [${app.failureArms().join(", ")}]`,
      );
    }
  }
}

/** The drawn text of every response row currently in the root feed. */
function responseTexts(): string[] {
  return app
    .$$('[data-row-kind="activity"]')
    .map((row) => row.textContent?.trim() ?? "")
    .filter((text) => text !== "");
}

it("draws the real daemon's response to a prompt sent from its own composer", async () => {
  // Arrange — the app is mounted against the real daemon (beforeAll); nothing
  // about the daemon's state is scripted.
  const prompt = "webapp layer proof of life";
  const input = app.$('[data-component="composer"] textarea') as HTMLTextAreaElement | null;
  if (!input) throw new Error("the dev composer drew no input to type into");
  input.value = prompt;
  input.dispatchEvent(new Event("input", { bubbles: true }));
  await app.settle();

  // Act — the app's own send path: SubmitPrompt over the real transport.
  await app.click('[data-component="composer"] [data-composer-send]');

  // Assert — the prompt row the daemon pushed back, then the response row the
  // real shim's answer became.
  await awaitDrawn("the user prompt row", () =>
    app.$$('[data-row-kind="userPrompt"]').some((row) => (row.textContent ?? "").includes(prompt)),
  );
  await awaitDrawn("the response row", () =>
    responseTexts().some((text) => text.includes(`echo: ${prompt}`)),
  );
  expect(responseTexts().join("\n")).toContain(`echo: ${prompt}`);
}, TURN_BUDGET_MS + BOOT_BUDGET_MS);
