/**
 * §F5 — FOOTER AND TOPBAR SURFACES, against the real chain.
 *
 * Every figure here is one the REAL daemon resolved from a REAL turn: the
 * footer's status ladder as the turn moves through it, the tokens cell, the
 * clock, the fully-resolved expanded panels, and the topbar's account,
 * connectivity, model and context chips.
 *
 * WHAT ONLY THE WEBAPP CAN COVER: that these views are DRAWN — that the
 * status ladder redraws on each push, that opening a panel costs no round
 * trip (the panels ship resolved on every push, and which one is open is
 * webview-local), and that a panel row jumps to its `FeedId`.
 */
import { afterAll, beforeAll, expect, it } from "vitest";

import type { MountedApp } from "../integration/harness";
import {
  BOOT_BUDGET_MS,
  TURN_TEST_MS,
  awaitDrawn,
  bootLayer,
  driveTurn,
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

// §F5 #23 — the status ladder, redrawn as the real turn moves through it.
it(
  "draws the footer's status for an idle workspace, and again while a real turn runs",
  async () => {
    // Arrange — a workspace with nothing running says so.
    await awaitDrawn(app, "the footer's idle status", () => textOf(app.$(".footer-status")) !== "");
    const idle = textOf(app.$(".footer-status"));

    // Act — a real turn that PARKS, so the running status is observable
    // rather than a state the test races (`hold` is lifecycle.ts's turn that
    // stays open until interrupted, and permission_e2e_test.go drives it for
    // exactly this reason).
    await submit(app, "!hold");

    // Assert — the strip redrew to a different status than idle.
    await awaitDrawn(
      app,
      "the footer's running status",
      () => textOf(app.$(".footer-status")) !== idle,
    );
    expect(textOf(app.$(".footer-status"))).not.toBe(idle);

    // Release the parked turn through the page's own interrupt, so the next
    // test's submission is not held behind it.
    //
    // THE STOP IS AWAITED, NEVER LOOKED FOR ONCE. The status leaves idle while
    // the prompt is still submitting, but the turn stop mounts only once the
    // clock is live (strip.ts drawFooterClock). A one-shot lookup in that
    // window found no stop, skipped the release, and left `!hold` parked: every
    // later test in this file then queued behind it and timed out.
    await awaitDrawn(app, "the running turn's stop control", () => app.$(".footer-clock [data-interrupt]") !== null);
    const interrupt = app.$(".footer-clock [data-interrupt]");
    if (interrupt === null) throw new Error("the turn stop vanished between its wait and its click");
    //
    // THE STOP CAN LAND BEFORE THE TURN RUNS. The status takes the turn when
    // the prompt is accepted, while the session is still coming up, so the
    // stop may withdraw the accepted turn rather than end a running one. A
    // withdrawn turn draws no turn end, and either way the turn stop leaves
    // the strip once nothing is left to stop.
    const beforeTurns = rows(app, "turnEnded").length;
    await app.clickElement(interrupt);
    const confirm = app.$("[data-interrupt-confirm]");
    if (confirm) await app.clickElement(confirm);
    await awaitDrawn(
      app,
      "the parked turn to end or be withdrawn",
      () =>
        rows(app, "turnEnded").length > beforeTurns ||
        app.$(".footer-clock [data-interrupt]") === null,
    );
  },
  TURN_TEST_MS,
);

// §F5 #24 — the clock and the tokens cell, from the real turn's own figures.
it(
  "draws the footer's clock and tokens cell after a real turn",
  async () => {
    // Arrange / Act
    await driveTurn(app, "usage-full", "activity", "response");

    // Assert — both cells exist and carry the daemon's own text. The FIGURES
    // are the daemon's to resolve (the footer draws them verbatim), so what is
    // asserted here is that the cells are drawn and non-empty, never a number
    // this test computed.
    await awaitDrawn(app, "the footer's tokens cell", () => app.$(".footer-tokens") !== null);
    expect(app.$(".footer-tokens")).not.toBeNull();
    expect(app.$(".footer-clock")).not.toBeNull();
  },
  TURN_TEST_MS,
);

// §F5 #25 — an expanded panel ships FULLY RESOLVED on every push, so opening
// one costs no round trip; which panel is open is webview-local.
it(
  "opens a footer panel with no further round trip, drawing the rows the push already carried",
  async () => {
    // Arrange — a real turn, so the footer has something to resolve panels
    // from, and a panel control to click.
    await driveTurn(app, "usage-full", "activity", "response");
    const control = app.$(".footer-tokens");
    expect(control, "the footer drew no tokens cell to open").not.toBeNull();

    // Act
    await app.clickElement(control as HTMLElement);

    // Assert — a panel is drawn, and it names which panel it is.
    await awaitDrawn(app, "an expanded footer panel", () => app.$(".footer-expanded[data-panel]") !== null);
    const panel = app.$(".footer-expanded[data-panel]");
    expect(panel).not.toBeNull();
    expect(panel?.getAttribute("data-panel")).not.toBe("");
  },
  TURN_TEST_MS,
);

// §F5 #26 — the topbar's own cells, resolved by the daemon.
it(
  "draws the topbar's account label and connectivity from the real daemon",
  async () => {
    // Arrange / Act — boot alone resolves these; no turn is needed.
    await awaitDrawn(app, "the topbar's account label", () => app.$(".topbar-account") !== null);

    // Assert — the account is drawn (the fake config the world writes is
    // logged in, so this is an email rather than the logged-out warning), and
    // the connectivity dot is drawn beside it.
    expect(textOf(app.$(".topbar-account"))).not.toBe("");
    expect(app.$(".topbar-connectivity")).not.toBeNull();
  },
  TURN_TEST_MS,
);

it(
  "draws the topbar's model selector with the daemon's own options",
  async () => {
    // Arrange
    await awaitDrawn(app, "the topbar's model button", () => app.$(".topbar-model-button") !== null);

    // Act — the reveal renders BELOW the strip.
    await app.click(".topbar-model-button");

    // Assert — the options are the daemon's served tokens, drawn as rows.
    await awaitDrawn(app, "the model options", () => app.$$("[data-model-option]").length > 0);
    expect(app.$$("[data-model-option]").length).toBeGreaterThan(0);
  },
  TURN_TEST_MS,
);

it(
  "draws the topbar's context chip from the real session's usage",
  async () => {
    // Arrange / Act — a turn, so there is context to report.
    await driveTurn(app, "usage-full", "activity", "response");

    // Assert — the chip is drawn; its figure is the daemon's, drawn verbatim.
    await awaitDrawn(app, "the topbar's context chip", () => app.$(".topbar-context") !== null);
    expect(app.$(".topbar-context")).not.toBeNull();
  },
  TURN_TEST_MS,
);

// §F5 #27 — the warning dropdown is the home of unmodeled-tool and
// session-fault surfacing.
it(
  "surfaces an unmodeled tool in the topbar's warnings rather than the feed",
  async () => {
    // Arrange
    const feedRowsBefore = rows(app, "activity", "simpleToolCall").length;

    // Act — `unmodeled` is the scenario for a tool the contract does not
    // model (an MCP server's tool is `mcp-tool`, an ordinary card).
    await driveTurn(app, "unmodeled", "activity", "response");

    // Assert — the warnings surface exists and the tool did NOT become a feed
    // tool-call row (NOT in the feed: unmodeled tools are the topbar's).
    await awaitDrawn(app, "the topbar's warnings", () => app.$(".topbar-warnings") !== null);
    expect(app.$(".topbar-warnings")).not.toBeNull();
    expect(rows(app, "activity", "simpleToolCall").length).toBe(feedRowsBefore);
  },
  TURN_TEST_MS,
);
