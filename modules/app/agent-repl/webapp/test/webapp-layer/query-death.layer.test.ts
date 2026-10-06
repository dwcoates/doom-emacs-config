/**
 * §F2 — THE SECOND WAY A VENDOR QUERY DIES, against the real chain.
 *
 * ITS OWN AREA FILE, AND THE REASON IS STRUCTURAL. A dead vendor query ends
 * its session: `onQueryLost` drops the query and the shim then refuses every
 * later `StartTurn` with `StartTurnFailure.query_dead` — the designed arm,
 * held by `shim/test/integration/turn.test.ts` as "`!query-eof` then
 * StartTurn". A webapp-layer file mounts ONE page against ONE workspace and
 * therefore drives ONE session, so a file can exercise at most ONE query
 * death; a second one submits a prompt the vendor never receives and times
 * out on a terminal row nobody was ever going to draw.
 *
 * `!query-eof` (`SessionQueryDied.unexpected_eof`) is `feed-families.layer.
 * test.ts`'s last test, and this file holds the other cause arm. The Go
 * driver gives each area file its own world, which is what makes the two
 * sessions separate.
 *
 * SCENARIOS ARE THE GO SUITE'S OWN: `!query-fail` is driven by
 * `e2e/producerfaults_e2e_test.go`'s `TestQueryFailEndsTheTurnAsQueryDied`.
 */
import { afterAll, beforeAll, expect, it } from "vitest";

import type { MountedApp } from "../integration/harness";
import { BOOT_BUDGET_MS, TURN_TEST_MS, awaitDrawn, bootLayer, rows, submit } from "./drive";

let app: MountedApp;

beforeAll(async () => {
  app = await bootLayer();
}, BOOT_BUDGET_MS);

afterAll(async () => {
  await app?.stop();
});

it(
  "names an iterator failure on the turn's agent-repl outcome marker",
  async () => {
    // Arrange
    const before = rows(app, "turnEnded").length;

    // Act — `!query-fail` throws out of the sdk's iterator.
    await submit(app, "!query-fail");
    await awaitDrawn(
      app,
      "the terminal row for !query-fail",
      () => rows(app, "turnEnded").length > before,
    );

    // Assert
    const drawn = rows(app, "turnEnded");
    const row = drawn[drawn.length - 1];
    expect(row.querySelector("[data-turn-error]")?.getAttribute("data-turn-error")).toBe(
      "queryDied",
    );
    // The ending is agent-repl's outcome marker (owner ruling, 2026-10-06),
    // its expansion naming the query's death by the iterator and what it threw.
    const marker = row.querySelector(".outcome-marker");
    expect(marker?.getAttribute("data-family")).toBe("agentReplFault");
    expect(marker?.querySelector('[data-line="what-died"] .outcome-marker-value')?.textContent).toContain(
      "the SDK's iterator threw",
    );
    expect(marker?.querySelector('[data-line="thrown"]')).not.toBeNull();
  },
  TURN_TEST_MS,
);
