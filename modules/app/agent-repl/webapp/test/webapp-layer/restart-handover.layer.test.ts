/**
 * §F9 #39 — THE RESTART HANDOVER, against the real chain.
 *
 * The one scenario in this layer where the DAEMON UNDER THE PAGE is replaced
 * while the page is mounted. It is driven from the Go side
 * (`TestWebappLayer/WebappLayerRestartHandover`), which owns the incumbent's re-exec, the
 * successor's boot and the adoption rendezvous; this file is the page half.
 *
 * THE RENDEZVOUS, AND WHY IT IS NOT A SLEEP. The Go side must not replace the
 * daemon before this page is mounted and its streams are standing, or the
 * `transferred` push has no subscriber to reach. So the first thing this file
 * does after mounting is log a MARKER through the daemon's own `ClientLog`
 * rpc, which the daemon persists into the workspace's `webapp` log sink; the
 * Go side awaits that record with `AwaitLogRecord` and only then lands the
 * commit that fires the rollout. One structured-log record, read by the party
 * that needs it, in place of any timing assumption on either side.
 *
 * WHAT THE PAGE IS HELD TO (`src/lifecycle/lifecycle.ts`, "THE PAGE MOVES
 * ITSELF", owner ruling 2026-11-22): no handover reloads a page. On the
 * `transferred` push the mounted page adopts its workspace IN PLACE, through
 * the one shared address every daemon serves (the incumbent forwards the call
 * to the successor), and on its own webapp build re-homes every stream there
 * (`AppContext.rehome`). So the contract's promises to THIS end are:
 *
 *  1. the page re-homes: every standing stream is told to reopen;
 *  2. it never goes quiet and never draws the moved notice;
 *  3. its views are drawn again, from the successor, with no failure filed.
 *
 * Its in-place adoption is the rendezvous's web participant, which the Go side
 * pairs with the host adopt Emacs would make.
 */
import { afterAll, beforeAll, expect, it } from "vitest";

import { type MountedApp } from "../integration/harness";
import {
  BOOT_BUDGET_MS,
  HANDOVER_BUDGET_MS,
  HANDOVER_TEST_MS,
  awaitDrawn,
  bootLayer,
  textOf,
} from "./drive";

/**
 * The operation the mounted marker carries. THE GO SIDE MATCHES ON IT
 * VERBATIM (`wlPageMountedOperation` in `e2e/webapplayer_e2e_test.go`); the
 * two constants are documented on each other and move together.
 */
const MOUNTED_MARKER = "webapp-layer.handover.page-mounted";

/** The one page, carried across the handover. */
let app: MountedApp;

beforeAll(async () => {
  app = await bootLayer();
}, BOOT_BUDGET_MS);

afterAll(async () => {
  await app?.stop();
});

it(
  "adopts its workspace in place and re-homes every stream when its daemon hands the workspace over, never going quiet",
  async () => {
    // Arrange — THE RENDEZVOUS. The page is mounted and every stream the six
    // mounts opened is standing (that is what `bootLayer` returning means),
    // so the marker below is the honest signal that a handover fired now will
    // be observed rather than missed.
    await awaitDrawn(app, "the footer, so the page's streams are standing", () =>
      textOf(app.$(".footer-status")) !== "",
    );
    let rehomed = 0;
    app.ctx.onDaemonMoved(() => {
      rehomed += 1;
    });
    await app.ctx.client.clientLog({
      source: { case: "workspace", value: app.ctx.workspace },
      record: {
        level: { case: "info", value: {} },
        operation: MOUNTED_MARKER,
        message: "the webapp layer's handover page is mounted and its streams are standing",
      },
    });

    // Act — the Go side, woken by that record, lands a commit on the daemon's
    // own checkout, which fires a real blue-green self-rollout, and adopts the
    // host half on the successor. The page's own `transferred` handling is the
    // web half. Nothing here drives it.

    // Assert (1) — the page re-homed. The budget is the handover's, not a
    // turn's: two real process lifecycles.
    await awaitDrawn(app, "the page re-homing its streams", () => rehomed > 0, HANDOVER_BUDGET_MS);

    // Assert (2) — never quiet, never the moved notice.
    expect(app.ctx.isQuiesced()).toBe(false);
    expect(app.$('[data-component="drain-banner"] [data-moved]')).toBeNull();

    // Assert (3) — its views drawn again from the successor, nothing filed.
    await awaitDrawn(
      app,
      "the re-homed page's footer status",
      () => textOf(app.$(".footer-status")) !== "",
      HANDOVER_BUDGET_MS,
    );
    expect(app.feedContainer(), "the re-homed page drew no root feed").not.toBeNull();
    expect(app.failureArms()).toHaveLength(0);
  },
  HANDOVER_TEST_MS,
);
