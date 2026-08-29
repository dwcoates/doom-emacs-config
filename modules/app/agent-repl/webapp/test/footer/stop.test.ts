// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { InterruptResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  STOP_GLYPH,
  buildInterruptRequest,
  drawAgentsPanelStopAll,
  drawInterruptSuccess,
  drawTurnStopControl,
} from "../../src/footer/stop.js";
import {
  WORKSPACE,
  confirmRequired,
  harness,
  interruptSuccess,
  type Harness,
} from "./harness.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

/** The scripted router answers on timers; let every promise settle. */
async function settle(): Promise<void> {
  for (let i = 0; i < 40; i += 1) await vi.advanceTimersByTimeAsync(0);
}

/** Click the control's primary button. */
function press(control: HTMLElement, selector = "[data-interrupt]"): void {
  control.querySelector<HTMLElement>(selector)?.dispatchEvent(
    new MouseEvent("click", { bubbles: true }),
  );
}

describe("buildInterruptRequest", () => {
  it("echoes this page's workspace", () => {
    expect(buildInterruptRequest(harness().ctx, "turn", false).workspace).toEqual(WORKSPACE);
  });

  it("sets the TURN target arm", () => {
    expect(buildInterruptRequest(harness().ctx, "turn", false).target.case).toBe("turn");
  });

  it("sets the ALL AGENTS target arm", () => {
    expect(buildInterruptRequest(harness().ctx, "allAgents", false).target.case).toBe("allAgents");
  });

  it("leaves confirm_agents false on a first ask", () => {
    expect(buildInterruptRequest(harness().ctx, "turn", false).confirmAgents).toBe(false);
  });

  it("sets confirm_agents on the answer to the challenge", () => {
    expect(buildInterruptRequest(harness().ctx, "turn", true).confirmAgents).toBe(true);
  });
});

describe("drawTurnStopControl", () => {
  it("draws the stop hook the integration suite targets", () => {
    const control = drawTurnStopControl(harness().ctx);
    expect(control.querySelector("[data-interrupt]")).not.toBeNull();
  });

  it("draws a stop GLYPH rather than a word alone", () => {
    const control = drawTurnStopControl(harness().ctx);
    expect(control.textContent).toContain(STOP_GLYPH);
  });

  it("interrupts the TURN when pressed", async () => {
    const h = harness();
    press(drawTurnStopControl(h.ctx));
    await settle();
    expect(h.calls.interrupt[0]?.target.case).toBe("turn");
  });

  it("does not confirm agents on the first press", async () => {
    const h = harness();
    press(drawTurnStopControl(h.ctx));
    await settle();
    expect(h.calls.interrupt[0]?.confirmAgents).toBe(false);
  });

  it("notes an interrupted turn at the control", async () => {
    const h = harness({ interrupt: () => interruptSuccess("interruptedTurn") });
    const control = drawTurnStopControl(h.ctx);
    press(control);
    await settle();
    expect(control.querySelector("[data-stop-outcome]")?.getAttribute("data-stop-outcome")).toBe(
      "interruptedTurn",
    );
  });

  it("notes a stop that found nothing running — an ANSWER, not a refusal", async () => {
    const h = harness({ interrupt: () => interruptSuccess("nothingRunning") });
    const control = drawTurnStopControl(h.ctx);
    press(control);
    await settle();
    expect(control.querySelector(".refusal")).toBeNull();
    expect(control.querySelector("[data-stop-outcome]")?.textContent).toBe("nothing running");
  });

  it("draws the confirm step naming the live agents the stop would also end", async () => {
    const h = harness({ interrupt: () => confirmRequired(3n) });
    const control = drawTurnStopControl(h.ctx);
    press(control);
    await settle();
    expect(control.querySelector("[data-interrupt-confirm]")?.textContent).toBe(
      "also stop 3 live agents?",
    );
  });

  it("singularizes the challenge for one live agent", async () => {
    const h = harness({ interrupt: () => confirmRequired(1n) });
    const control = drawTurnStopControl(h.ctx);
    press(control);
    await settle();
    expect(control.querySelector("[data-interrupt-confirm]")?.textContent).toBe(
      "also stop 1 live agent?",
    );
  });

  it("resends with confirm_agents when the challenge is answered", async () => {
    let answered = false;
    const h = harness({
      interrupt: () => {
        if (answered) return interruptSuccess("interruptedTurn");
        answered = true;
        return confirmRequired(2n);
      },
    });
    const control = drawTurnStopControl(h.ctx);
    press(control);
    await settle();
    press(control, "[data-interrupt-confirm]");
    await settle();
    expect(h.calls.interrupt[1]?.confirmAgents).toBe(true);
  });

  it("clears the challenge once the resend answers", async () => {
    let answered = false;
    const h = harness({
      interrupt: () => {
        if (answered) return interruptSuccess("interruptedTurn");
        answered = true;
        return confirmRequired(2n);
      },
    });
    const control = drawTurnStopControl(h.ctx);
    press(control);
    await settle();
    press(control, "[data-interrupt-confirm]");
    await settle();
    expect(control.querySelector("[data-interrupt-confirm]")).toBeNull();
  });

  it("draws the refusal at the control when the stop never reached the daemon", async () => {
    const h = harness({
      interrupt: () => {
        throw new Error("no route to the daemon");
      },
    });
    const control = drawTurnStopControl(h.ctx);
    press(control);
    await settle();
    expect(control.querySelector(".refusal")?.getAttribute("data-arm")).toBe("transport");
  });

  it("draws a refusal for an error response whose kind oneof sets no arm", async () => {
    const h = harness({
      interrupt: () => create(InterruptResponseSchema, { result: { case: "error", value: {} } }),
    });
    const control = drawTurnStopControl(h.ctx);
    press(control);
    await settle();
    expect(control.querySelector(".refusal")?.getAttribute("data-arm")).toBe("malformed");
  });

  it("files an unreadable answer as frame_undecodable", async () => {
    const h = harness({
      interrupt: () => create(InterruptResponseSchema, { result: { case: "error", value: {} } }),
    });
    press(drawTurnStopControl(h.ctx));
    await settle();
    expect(h.sink.reported).toContain("frameUndecodable");
  });

  it("refuses a response whose result oneof sets no arm", async () => {
    const h = harness({ interrupt: () => create(InterruptResponseSchema, {}) });
    const control = drawTurnStopControl(h.ctx);
    press(control);
    await settle();
    expect(control.querySelector(".refusal")?.getAttribute("data-arm")).toBe("malformed");
  });
});

describe("drawAgentsPanelStopAll", () => {
  it("interrupts EVERY live agent when pressed", async () => {
    const h: Harness = harness({ interrupt: () => interruptSuccess("interruptedDetached", 3n) });
    press(drawAgentsPanelStopAll(h.ctx));
    await settle();
    expect(h.calls.interrupt[0]?.target.case).toBe("allAgents");
  });

  it("reports how many agents the stop reached", async () => {
    const h = harness({ interrupt: () => interruptSuccess("interruptedDetached", 3n) });
    const control = drawAgentsPanelStopAll(h.ctx);
    press(control);
    await settle();
    expect(control.querySelector("[data-stop-outcome]")?.textContent).toBe("stopped 3 agents");
  });

  it("singularizes a stop that reached one agent", async () => {
    const h = harness({ interrupt: () => interruptSuccess("interruptedDetached", 1n) });
    const control = drawAgentsPanelStopAll(h.ctx);
    press(control);
    await settle();
    expect(control.querySelector("[data-stop-outcome]")?.textContent).toBe("stopped 1 agent");
  });

  it("notes a fan-wide stop that found nothing running", async () => {
    const h = harness({ interrupt: () => interruptSuccess("nothingRunning") });
    const control = drawAgentsPanelStopAll(h.ctx);
    press(control);
    await settle();
    expect(control.querySelector("[data-stop-outcome]")?.textContent).toBe("nothing running");
  });
});

describe("drawInterruptSuccess: the arm is what the stop DID", () => {
  it("refuses a success whose outcome oneof sets no arm", () => {
    expect(() =>
      drawInterruptSuccess(
        create(InterruptResponseSchema, { result: { case: "success", value: {} } }).result
          .value as never,
        "InterruptSuccess",
      ),
    ).toThrow(MalformedView);
  });
});
