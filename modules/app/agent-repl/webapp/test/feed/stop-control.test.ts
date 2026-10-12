// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  InterruptRequestSchema,
  InterruptResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_interrupt_pb";
import { FeedIdSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawStopControl, INTERRUPT_OUTCOME_MS, type StopSite } from "../../src/feed/stop-control.js";
import { harness, rowContext, subagentRow, type Harness } from "./harness.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1_000_000);
});
afterEach(() => {
  vi.useRealTimers();
});

/** A stop site of the test's own, so the helper is read apart from any caller. */
const SITE: StopSite = {
  prefix: "test-stop",
  operation: "feed.test-stop",
  refusedOperation: "feed.test-stop-refused",
  describe: "stopping the test's work",
  request: (workspace, target) =>
    create(InterruptRequestSchema, { workspace, target: { case: "detached", value: target } }),
};

/** The control for row ROW, drawn with harness H. */
function draw(row = "r1", h: Harness = harness()): { el: HTMLElement; h: Harness } {
  const target = create(FeedIdSchema, { value: row });
  const el = drawStopControl(rowContext(h.ctx, subagentRow(row)), SITE, target);
  document.body.append(el);
  return { el, h };
}

/** Let the scripted answer land. */
async function settle(): Promise<void> {
  for (let i = 0; i < 30; i += 1) await vi.advanceTimersByTimeAsync(0);
}

/** A harness whose Interrupt answers with the error ARM. */
function refusing(arm: string): Harness {
  return harness({
    interrupt: () =>
      create(InterruptResponseSchema, {
        result: { case: "error", value: { kind: { case: arm as never, value: {} as never } } },
      }),
  });
}

describe("drawStopControl", () => {
  it("names its parts by the site's prefix", () => {
    const { el } = draw();
    expect([el.className, el.querySelector("[data-interrupt]")?.className]).toEqual(["test-stop", "test-stop-button"]);
  });

  it("sends the site's request for the target row", async () => {
    // Arrange
    const { el, h } = draw("r7");
    // Act
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    // Assert
    expect(h.calls.interrupt[0]?.target).toEqual({
      case: "detached",
      value: { $typeName: "frontend.v1.FeedId", value: "r7" },
    });
  });

  it("records the click under the site's operation", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { el } = draw();
    // Act
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    // Assert
    expect((await forwardedRecord(capture, "feed.test-stop")).level.case).toBe("info");
  });

  it("draws a success under the site's outcome class", async () => {
    const { el } = draw();
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".test-stop-outcome")?.getAttribute("data-outcome")).toBe("interruptedDetached");
  });

  it("clears a success once it has been on screen long enough to read", async () => {
    const { el } = draw();
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    await vi.advanceTimersByTimeAsync(INTERRUPT_OUTCOME_MS + 1_000);
    expect(el.querySelector(".test-stop-outcome")).toBeNull();
  });

  it("draws a refusal at the control by its arm", async () => {
    const { el } = draw("r1", refusing("notDetachedWork"));
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")?.getAttribute("data-arm")).toBe("notDetachedWork");
  });

  it("records a refusal at WARN under the site's refused operation", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { el } = draw("r1", refusing("notDetachedWork"));
    // Act
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    // Assert
    expect((await forwardedRecord(capture, "feed.test-stop-refused")).level.case).toBe("warn");
  });

  it("tells the reader when the click never reached the daemon", async () => {
    const h = harness({
      interrupt: () => {
        throw new Error("gone");
      },
    });
    const { el } = draw("r1", h);
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    expect(el.querySelector(".refusal")?.getAttribute("data-arm")).toBe("transport");
  });

  it("drops the previous click's answer when clicked again", async () => {
    // Arrange
    const { el } = draw("r1", refusing("notDetachedWork"));
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    // Act
    el.querySelector<HTMLElement>("[data-interrupt]")?.click();
    await settle();
    // Assert
    expect(el.querySelectorAll(".refusal")).toHaveLength(1);
  });
});

describe("the stop control's call sites", () => {
  it.each(["src/feed/rows/subagent.ts"])(
    "%s draws its stop through drawStopControl rather than a control of its own",
    (file) => {
      const source = readFileSync(join(process.cwd(), file), "utf8");
      expect([source.includes("drawStopControl("), source.includes('client.interrupt(')]).toEqual([true, false]);
    },
  );
});
