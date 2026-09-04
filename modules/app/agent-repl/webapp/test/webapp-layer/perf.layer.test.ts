/**
 * THE PERF AREA — `e2e/PERF-SPEC.md` §F, the rows the WEBAPP layer hosts.
 *
 * Five assertions, all against the real chain (real store, real sidecar, real
 * claude-repld, real shim over the fake SDK, the real webapp in jsdom):
 *
 *   perf-prompt-bubble    row 1b  — send click -> the prompt bubble is drawn
 *   perf-response-bubble  row 3a  — response frame -> the response bubble
 *   perf-interrupt-footer row 5   — interrupt confirm -> the footer arm flips
 *   perf-question-card    row 8a  — question frame -> the card is drawn
 *   perf-sidebar-selected row 11c — roster frame -> the sidebar's current row
 *
 * WHAT IS MEASURED, AND BY WHOSE CLOCK: every number here is a page-side
 * DURATION on the real clock captured before the harness faked timers (§A4),
 * closed by `ApplyProbe` on the frame itself and never by `settle()` (§A4.4).
 * Nothing here is correlated to a daemon instant, because nothing can be
 * (§A4.1). The percentiles are shipped to the Go driver as ONE `ClientLog`
 * record, which is where the budget and the baseline are enforced (§D1).
 *
 * SCENARIOS ARE NEVER MINTED HERE, per the layer's own standing rule: `!hold`
 * and `!ask-single` are both driven by Go area tests already
 * (`interrupt_e2e_test.go`, `questions_e2e_test.go`), and the bare prompt is
 * the fake SDK's default prose turn.
 *
 * NOTHING SLEEPS. Each sample waits on the event it times.
 */
import { afterAll, beforeAll, expect, it } from "vitest";

import type { MountedApp } from "../integration/harness";
import { BOOT_BUDGET_MS, TURN_BUDGET_MS, awaitDrawn, bootLayer, rows } from "./drive";
import {
  ApplyProbe,
  PERF_SAMPLES,
  PerfRecorder,
  awaitSample,
  realNow,
  shipSamples,
} from "./perf";

/** The second workspace row 11c alternates the selection between. */
const WORKSPACE_ID_B = "AGENT_REPL_E2E_WORKSPACE_ID_B";
const WORKSPACE_DIR_B = "AGENT_REPL_E2E_WORKSPACE_DIR_B";

/**
 * One sample's own budget. It bounds a HANG, not a measurement: the probe
 * closes on the frame, and this only decides how long a sample that never
 * arrives is waited for. Reused from the layer's own TURN_BUDGET_MS rather
 * than minted.
 */
const SAMPLE_BUDGET_MS = TURN_BUDGET_MS;

/**
 * A whole assertion's timeout: twenty real turns, plus the boot.
 *
 * MEASURED, then set at ~3x, the same way every other bound in this suite was
 * derived — see `WebappLayerPerfTimeout` in `e2e/perf_webapp_test.go`, which
 * bounds the child this file runs in and carries the measurement.
 */
const ASSERTION_TEST_MS = PERF_SAMPLES * TURN_BUDGET_MS + BOOT_BUDGET_MS;

let app: MountedApp;
let probe: ApplyProbe;
const recorders: PerfRecorder[] = [];

beforeAll(async () => {
  app = await bootLayer();
  probe = ApplyProbe.install(app);

  // ONE WARM-UP TURN, AS ARRANGEMENT — never a discarded sample (§D1 forbids
  // discarding one). The first prompt a workspace ever receives pays for the
  // real shim session's own start, a node process spawn, and that cost belongs
  // to cold start (§C row 17a) and not to any row here. Measured without it,
  // perf-prompt-bubble's max was ~370ms on sample 1 and under 10ms on the
  // other nineteen: one cost, filed under the wrong row.
  const send = typeInto("perf warm-up");
  await app.settle();
  send.click();
  await awaitTurnEnded(0);
}, BOOT_BUDGET_MS + TURN_BUDGET_MS);

afterAll(async () => {
  // THE NUMBERS SHIP BEFORE THE PAGE STOPS. A record sent after `stop()` has
  // no transport to travel on, and the Go driver would then wait out its own
  // bound for a record that was never sent.
  if (app !== undefined && recorders.length > 0) await shipSamples(app, recorders);
  probe?.uninstall();
  await app?.stop();
});

/** Register a recorder so `afterAll` ships it. */
function recorder(name: string): PerfRecorder {
  const rec = new PerfRecorder(name);
  recorders.push(rec);
  return rec;
}

/** Type into the app's own composer without settling on the way out. */
function typeInto(text: string): HTMLElement {
  const input = app.$('[data-component="composer"] textarea') as HTMLTextAreaElement | null;
  if (!input) throw new Error("no composer input");
  input.value = text;
  input.dispatchEvent(new Event("input", { bubbles: true }));
  const send = app.$('[data-component="composer"] [data-composer-send]');
  if (!send) throw new Error("no composer send control");
  return send;
}

/** Leave the workspace idle, so the next sample's prompt is not held. */
async function awaitTurnEnded(before: number): Promise<void> {
  await awaitDrawn(
    app,
    "the turn to end before the next sample",
    () => rows(app, "turnEnded").length > before,
  );
}

// ---------------------------------------------------------------------------
// Rows 1b and 3a, measured in the SAME twenty turns.
//
// They are two different intervals over one chain — 1b is a full round trip
// from the send click, 3a is one frame's apply — so driving them together
// costs twenty turns instead of forty and changes neither number: each has its
// own probe arming, and 3a's origin is its own frame's arrival, not 1b's
// click.
// ---------------------------------------------------------------------------
it(
  "measures the prompt bubble round trip and the response bubble's apply",
  async () => {
    const prompt = recorder("perf-prompt-bubble");
    const response = recorder("perf-response-bubble");

    for (let i = 0; i < PERF_SAMPLES; i += 1) {
      // Arrange. BOTH counts are read, and BOTH probes armed, BEFORE the send:
      // the response row can arrive in the same task as the prompt bubble, and
      // a count read after the prompt sample would already include it — the
      // response waiter would then be waiting for a SECOND response row this
      // turn never produces.
      const endedBefore = rows(app, "turnEnded").length;
      const promptsBefore = rows(app, "userPrompt").length;
      const responsesBefore = responseRows().length;
      const send = typeInto(`perf prompt bubble ${i}`);
      await app.settle();

      // Row 1b: the origin is the click, and the terminal is the frame after
      // which the page's OWN prompt bubble is drawn. §C row 1 establishes that
      // this bubble is NOT optimistic — `composer.ts:send1` only remembers the
      // TurnId — so this really is a server round trip.
      const armedPrompt = probe.armFrom(
        realNow(),
        () => rows(app, "userPrompt").length > promptsBefore,
      );
      // Row 3a: the origin is THE FRAME'S OWN ARRIVAL. §C row 3a restates
      // "visible" as the first response element entering the DOM, because
      // `smooth.ts` paces the character reveal at 200 cps on a 0.3 s constant
      // by design — a type-out, not a latency.
      const armedResponse = probe.armFromFrame(() => responseRows().length > responsesBefore);

      // Act.
      send.click();

      // Assert.
      prompt.record(await awaitSample(app, probe, "the prompt bubble", armedPrompt, SAMPLE_BUDGET_MS));
      response.record(
        await awaitSample(app, probe, "the response bubble", armedResponse, SAMPLE_BUDGET_MS),
      );

      await awaitTurnEnded(endedBefore);
    }

    expect(prompt.n).toBe(PERF_SAMPLES);
    expect(response.n).toBe(PERF_SAMPLES);
  },
  ASSERTION_TEST_MS,
);

// ---------------------------------------------------------------------------
// Row 5 — the interrupt control, and the footer arm it flips.
// ---------------------------------------------------------------------------
it(
  "measures the interrupt click to the footer arm the frame carries",
  async () => {
    const rec = recorder("perf-interrupt-footer");

    for (let i = 0; i < PERF_SAMPLES; i += 1) {
      // Arrange: a turn that PARKS until an interrupt lands. `!hold` is
      // documented as ending only on an interrupt (lifecycle.ts), which is
      // what gives the footer a live arm to flip out of.
      const endedBefore = rows(app, "turnEnded").length;
      const send = typeInto("!hold");
      await app.settle();
      send.click();
      await awaitDrawn(app, "the footer to report a running turn", () => footerArm() === "thinking");

      // The footer's own interrupt, as a user reaches it.
      const interrupt = app.$(".footer-clock [data-interrupt]");
      if (!interrupt) throw new Error("the footer drew no interrupt control while a turn was running");

      // Act + Assert: the origin is THE INTERRUPT CLICK, which is the one that
      // calls `Interrupt` (`footer/stop.ts:interruptControl`'s own listener ->
      // `rpc/unary.ts:callUnary`). The control grows a SECOND, confirming
      // button only when the daemon refuses with `confirmRequired` — live
      // agents the stop would also end (`footer/stop.ts:drawConfirm`) — which
      // `!hold` has none of. A confirm appearing here would mean this sample
      // measured a refusal round trip rather than the interrupt, so it is
      // reported as a fault rather than clicked through.
      //
      // THE ARM IS CAUGHT ON ITS FRAME, NEVER BY SETTLING TO IT. §C row 5 is
      // explicit: the footer's momentary arms are retired by
      // `resolve/footer/resolver.go`'s `clock.AfterFunc` at
      // DefaultMomentaryDwell = 1500ms, so an assertion that settles its way
      // to the arm has a 1.5 s window and passes by luck. The probe closes on
      // the frame that carries it.
      const armBefore = footerArm();
      const armed = probe.armFrom(realNow(), () => footerArm() !== armBefore);
      interrupt.click();
      rec.record(await awaitSample(app, probe, "the footer arm to flip", armed, SAMPLE_BUDGET_MS));
      if (app.$("[data-interrupt-confirm]")) {
        throw new Error(
          "the interrupt was refused with confirmRequired, so this sample timed a refusal and not the stop",
        );
      }

      await awaitTurnEnded(endedBefore);
    }

    expect(rec.n).toBe(PERF_SAMPLES);
  },
  ASSERTION_TEST_MS,
);

// ---------------------------------------------------------------------------
// Row 8a — an ask raised, and the card the frame draws.
// ---------------------------------------------------------------------------
it(
  "measures the question frame to the question card in the DOM",
  async () => {
    const rec = recorder("perf-question-card");

    for (let i = 0; i < PERF_SAMPLES; i += 1) {
      // Arrange: `!ask-single` blocks its turn on the ask, so the wait is on
      // the card and never on `turn_ended`.
      const cardsBefore = rows(app, "question").length;
      const send = typeInto("!ask-single");
      await app.settle();

      // Act + Assert: the origin is THE QUESTION FRAME'S OWN ARRIVAL. §C row
      // 8a is explicit that the daemon-side origin
      // (`daemon.feed.question`) is in daemon time and cannot be subtracted
      // from a page instant, so the page measures `consume` -> DOM only.
      const armed = probe.armFromFrame(() => rows(app, "question").length > cardsBefore);
      send.click();
      rec.record(await awaitSample(app, probe, "the question card", armed, SAMPLE_BUDGET_MS));

      // Leave the workspace idle: an unanswered ask holds every later prompt.
      await answerStandingQuestions();
    }

    expect(rec.n).toBe(PERF_SAMPLES);
  },
  ASSERTION_TEST_MS,
);

// ---------------------------------------------------------------------------
// Row 11c — SelectWorkspace, and the sidebar's current row.
//
// THE OWNER'S FRAMING, and what this actually covers: "switching workspaces
// (Emacs tabs) is reflected in the webapp sidebar more-or-less instantly."
// The Emacs-originated half — RET on a tab -> `agent-repl-host-select` ->
// `SelectWorkspace` sent (`lisp/host.el:273`) — is phase 2 with the other
// Emacs rows. What this measures is everything after that: the roster frame
// the daemon publishes (`workspace/verbs.go:(*verbs).republishRegistry` ->
// `resolve/sidebar/resolver.go` SetRegistry then SetSelected — the WHOLE
// roster is republished) arriving at the page, and the sidebar's selected
// marker moving.
//
// THE RPC IS ISSUED FROM THE PAGE'S OWN CLIENT, and it is deliberately
// OUTSIDE the measured interval: the origin stamp is the frame's arrival
// (`armFromFrame`), so who called `SelectWorkspace` cannot affect the number.
// It is the same rpc Emacs's tab switch sends, on the same real daemon, and
// issuing it here rather than from the Go driver costs no fidelity in the hop
// being timed while sparing this file a twenty-round cross-process rendezvous.
//
// THE SIDEBAR IS O(N) BY CONSTRUCTION (§C row 21): `sidebar.ts:mountSidebar`'s
// onPush calls `drawWorkspaceRoster` and then `body.replaceChildren(drawn)`,
// rebuilding the whole rail per push. This row's number therefore includes a
// full rail rebuild, which is the design and not a defect.
// ---------------------------------------------------------------------------
it(
  "measures the roster frame to the sidebar's selected row moving",
  async () => {
    const rec = recorder("perf-sidebar-selected");
    const a = app.ctx.workspace;
    const b = { id: required(WORKSPACE_ID_B), dir: required(WORKSPACE_DIR_B) };

    for (let i = 0; i < PERF_SAMPLES; i += 1) {
      // Arrange: alternate, so every sample is a real MOVE of the marker.
      const target = i % 2 === 0 ? b : a;
      await awaitDrawn(app, `a sidebar row for ${target.id}`, () => rosterRow(target.id) !== null);
      if (isCurrent(target.id)) {
        // Already selected — put the marker on the other one first, without
        // measuring that hop.
        const other = target === b ? a : b;
        await app.ctx.client.selectWorkspace({ workspace: other });
        await awaitDrawn(app, `the marker to leave ${target.id}`, () => !isCurrent(target.id));
      }

      // Act + Assert.
      const armed = probe.armFromFrame(() => isCurrent(target.id));
      await app.ctx.client.selectWorkspace({ workspace: target });
      rec.record(
        await awaitSample(app, probe, "the sidebar's current row", armed, SAMPLE_BUDGET_MS),
      );
    }

    expect(rec.n).toBe(PERF_SAMPLES);
  },
  ASSERTION_TEST_MS,
);

// ---------------------------------------------------------------------------
// Shared readbacks, all on the DOM hooks contract.
// ---------------------------------------------------------------------------

/** Every drawn response activity row. */
function responseRows(): HTMLElement[] {
  return app.$$('[data-feed-row][data-row-kind="activity"][data-unit="response"]');
}

/** The footer's status arm, or "" when the footer drew no status. */
function footerArm(): string {
  return app.$(".footer-status")?.getAttribute("data-arm") ?? "";
}

/** One sidebar roster row by workspace id. */
function rosterRow(id: string): HTMLElement | null {
  return app.$(`[data-roster-row="${id}"]`);
}

/** Whether the sidebar draws a workspace as the current one. */
function isCurrent(id: string): boolean {
  return rosterRow(id)?.getAttribute("data-current") === "true";
}

/** Answer every standing question card through its own free-text escape. */
async function answerStandingQuestions(): Promise<void> {
  const submits = app.$$('[data-feed-row][data-row-kind="question"] [data-question-submit]');
  for (const submitButton of submits) {
    const card = submitButton.closest("[data-feed-row]");
    const other = card?.querySelector<HTMLInputElement>("[data-question-other]");
    if (other) {
      other.value = "perf answer";
      other.dispatchEvent(new Event("input", { bubbles: true }));
      await app.settle();
    }
    await app.clickElement(submitButton);
  }
  await awaitDrawn(
    app,
    "every question card to leave its standing state",
    () => app.$$('[data-row-kind="question"][data-state="standing"]').length === 0,
  );
}

/** An environment value the Go driver owes this child. */
function required(name: string): string {
  const value = process.env[name];
  if (value === undefined || value === "") {
    throw new Error(
      `${name} is unset: the perf area is driven by e2e/perf_webapp_test.go, which registers a ` +
        "second workspace for row 11c and passes it in the environment",
    );
  }
  return value;
}
