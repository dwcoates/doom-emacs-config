/**
 * §F9 #39 — THE RESTART HANDOVER, against the real chain.
 *
 * The one scenario in this layer where the DAEMON UNDER THE PAGE is replaced
 * while the page is mounted. It is driven from the Go side
 * (`TestWebappLayerRestartHandover`), which owns the incumbent's re-exec, the
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
 * WHAT THE PAGE IS HELD TO (`src/lifecycle/lifecycle.ts:12-21`):
 *
 *   "THE WEBAPP DOES NOT REDIAL (project lead, final). A successor on a
 *    different loopback port is a DIFFERENT ORIGIN ... re-pointing the webview
 *    is Emacs's job, and it does it by reloading the view at the new address."
 *
 * So the contract's three promises to THIS end are:
 *
 *  1. the `transferred{address}` push on `WatchWebWorkspace` draws the
 *     terminal "workspace moved to <address>" notice, naming the successor
 *     (`lifecycle.ts:130-137` and `drawMovedNotice`, `lifecycle.ts:344-353`);
 *  2. the page then GOES QUIET — `ctx.quiesce()` — so every later verb is
 *     refused locally rather than spent on a daemon that has released the
 *     workspace (`src/rpc/unary.ts:36-66`, and `src/rpc/context.ts:18-23`
 *     "made structural rather than hoped for");
 *  3. RECOVERY IS A FRESH PAGE AT THE NEW ADDRESS, which is exactly what
 *     Emacs does with the address this banner names — and a fresh page adopts
 *     the workspace on the successor itself, at boot, before any view stream
 *     opens (`lifecycle.ts:23-28`, `adoptAtBoot`). This file performs that
 *     reload with the address it read off its OWN banner, so the successor's
 *     `AdoptWebWorkspace` is issued by the real app rather than by the driver.
 *
 * WHAT IT IS NOT HELD TO: the `transferring_away` refusal ARM. It is the same
 * fact arriving as an answer (`src/rpc/moved.ts:5-13`), and the old daemon
 * only starts answering it once `recordTransfer` has run — one line before the
 * push (`daemon/internal/rollout/handover.go:129-130`). By the time this page
 * could issue a verb against the old daemon it has already been quiesced BY
 * that push, so the arm is unreachable from here without racing the two; the
 * Go suite pins it directly (`e2e/adoption_e2e_test.go`,
 * TestRefusalOrderingDuringHandover). What this file pins instead is the
 * refusal the page really does draw afterwards: the local one.
 */
import { afterAll, beforeAll, expect, it } from "vitest";

import { type MountedApp, startAppAgainst } from "../integration/harness";
import {
  BOOT_BUDGET_MS,
  HANDOVER_BUDGET_MS,
  HANDOVER_TEST_MS,
  awaitDrawn,
  bootLayer,
  submit,
  textOf,
} from "./drive";
import { realDaemon } from "./real-daemon";

/**
 * The operation the mounted marker carries. THE GO SIDE MATCHES ON IT
 * VERBATIM (`wlPageMountedOperation` in `e2e/webapplayer_e2e_test.go`); the
 * two constants are documented on each other and move together.
 */
const MOUNTED_MARKER = "webapp-layer.handover.page-mounted";

/** The page before the handover, and the fresh page after it. */
let app: MountedApp;
let recovered: MountedApp | undefined;

beforeAll(async () => {
  app = await bootLayer();
}, BOOT_BUDGET_MS);

afterAll(async () => {
  await recovered?.stop();
  await app?.stop();
});

it(
  "draws the successor's address and goes quiet when its daemon hands the workspace over, and a fresh page recovers on the successor",
  async () => {
    // Arrange — THE RENDEZVOUS. The page is mounted and every stream the six
    // mounts opened is standing (that is what `bootLayer` returning means),
    // so the marker below is the honest signal that a handover fired now will
    // be observed rather than missed.
    await awaitDrawn(app, "the footer, so the page's streams are standing", () =>
      textOf(app.$(".footer-status")) !== "",
    );
    await app.ctx.client.clientLog({
      workspace: app.ctx.workspace,
      record: {
        level: { case: "info", value: {} },
        operation: MOUNTED_MARKER,
        message: "the webapp layer's handover page is mounted and its streams are standing",
      },
    });

    // Act — the Go side, woken by that record, lands a commit on the daemon's
    // own checkout, which fires a real blue-green self-rollout, and completes
    // the adoption rendezvous on the successor. Nothing here drives it.

    // Assert (1) — the terminal notice, naming the successor's address. The
    // budget is the handover's, not a turn's: two real process lifecycles.
    await awaitDrawn(
      app,
      "the moved notice naming the successor",
      () => app.$('[data-component="drain-banner"] [data-moved]') !== null,
      HANDOVER_BUDGET_MS,
    );
    const banner = app.$('[data-component="drain-banner"] [data-moved]');
    expect(banner).not.toBeNull();
    const address = banner?.getAttribute("data-moved") ?? "";
    // The address is the ONE fact the reader may need, so it is drawn as well
    // as carried (`lifecycle.ts:344-350`).
    expect(address).not.toBe("");
    expect(textOf(banner)).toBe(`workspace moved to ${address}`);
    // A DIFFERENT DAEMON, not this page's own: the successor listens on its
    // own loopback port, which is the whole reason this page cannot follow it.
    expect(`http://${address}`).not.toBe(realDaemon().baseUrl);

    // Assert (2) — the page went quiet, and the refusal it now gets is the
    // LOCAL one: `callUnary` refuses before the wire, and the composer draws
    // that at the control that made the call, as its `transport` pseudo-arm.
    expect(app.ctx.isQuiesced()).toBe(true);
    await submit(app, "this cannot be sent any more");
    await awaitDrawn(app, "the composer's refusal after the move", () =>
      app.refusalArms().includes("transport"),
    );
    expect(textOf(app.$(".composer-refusal"))).toBe(
      "the daemon could not be reached — your text is kept; try again",
    );

    // Act — THE RELOAD EMACS WOULD PERFORM, at the address this page named.
    // The old page is disposed first: one document, one shell, one move
    // handler, and the released workspace's page has nothing left to do.
    await app.stop();
    const daemon = realDaemon();
    recovered = await startAppAgainst(`http://${address}`, {
      composer: true,
      workspaceId: daemon.workspaceId,
      workspaceDir: daemon.workspaceDir,
    });

    // Assert (3) — the fresh page adopted the workspace on the SUCCESSOR
    // (`adoptAtBoot` runs before any view mounts, so a mount that returned at
    // all is a completed adoption), and its own views recovered: the feed is
    // open at the root address and the FOOTER resolved a status, which only
    // the successor's own `WatchFooter` push can put there.
    //
    // The status itself is not asserted to be any particular value: the host
    // participant this run holds lives on the OLD daemon's stream, which the
    // handover took down, so the recovered page's connectivity correctly reads
    // as disconnected. That a status was RESOLVED AT ALL is the recovery.
    await awaitDrawn(
      recovered,
      "the recovered page's footer status",
      () => textOf(recovered?.$(".footer-status")) !== "",
      HANDOVER_BUDGET_MS,
    );
    expect(textOf(recovered.$(".footer-status"))).not.toBe("");
    expect(recovered.feedContainer(), "the recovered page drew no root feed").not.toBeNull();
    expect(recovered.ctx.isQuiesced()).toBe(false);
    expect(recovered.failureArms()).toHaveLength(0);
  },
  HANDOVER_TEST_MS,
);
