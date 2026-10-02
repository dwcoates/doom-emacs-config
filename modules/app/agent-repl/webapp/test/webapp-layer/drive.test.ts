// @vitest-environment jsdom
/**
 * `send()` — the one place a layer test presses the app's own Send button.
 *
 * The behaviour under test is not the composer's; it is the DRIVER'S promise
 * that a press it made was TAKEN. `src/composer/composer.ts` drops a press it
 * cannot take (empty box, closed gate, a submission still in flight) and says
 * nothing about it, which is right for production and ruinous for a driver:
 * the turn never starts, and the test that waits for the turn's row fails a
 * budget later naming the row instead of the press.
 *
 * This file runs in the FAST UNIT SUITE and needs no daemon: `send()` reads
 * and presses a DOM button and settles, so a stub page is the whole world it
 * requires. It sits beside the layer files it serves and is named
 * `drive.test.ts` rather than `drive.layer.test.ts`, which is the suffix both
 * vitest configs and `e2e/scenariomatrix_test.go` use to mean "the Go world
 * drives this one".
 */
import { createControl, type Control } from "../../src/control.js";
import { beforeEach, describe, expect, it, vi } from "vitest";

import type { MountedApp } from "../integration/harness";
import { COMPOSER_READY_BUDGET_MS, press, send } from "./drive";

const HOST = '[data-component="composer"]';

/** How far the stub's `settle()` moves the clock, so a budget can expire. */
const SETTLE_STEP_MS = 250;

interface StubPage {
  readonly app: MountedApp;
  readonly button: Control;
  readonly input: HTMLTextAreaElement;
  /** How many times `settle()` was awaited. */
  settles(): number;
}

/**
 * A page with one composer, one Send button and a clock that only moves when
 * the driver settles.
 *
 * THE CLOCK IS FAKE AND MOVED BY `settle()`, never by a sleep: `send()`'s
 * budget is read off `Date.now()`, so a test that must see the budget expire
 * advances it the way the real loop does — one settle at a time.
 */
function stubPage(options: { text?: string; disabled?: boolean } = {}): StubPage {
  const root = document.createElement("div");
  root.setAttribute("data-component", "composer");
  const input = document.createElement("textarea");
  input.value = options.text ?? "!scenario";
  const button = createControl();
  button.setAttribute("data-composer-send", "");
  button.disabled = options.disabled ?? false;
  root.append(input, button);
  document.body.append(root);

  let settles = 0;
  const app = {
    $: (selector: string) => document.querySelector(selector),
    $$: (selector: string) => [...document.querySelectorAll(selector)],
    settle: async () => {
      settles += 1;
      vi.advanceTimersByTime(SETTLE_STEP_MS);
    },
    failureArms: () => [],
    refusalArms: () => [],
  } as unknown as MountedApp;

  return { app, button, input, settles: () => settles };
}

beforeEach(() => {
  vi.useFakeTimers();
});

describe("send", () => {
  it("presses a button that is already pressable", async () => {
    // Arrange — the composer is open and nothing is in flight.
    const page = stubPage();
    // A real `submit()` disables the button inside the click handler.
    page.button.addEventListener("click", () => {
      page.button.disabled = true;
    });

    // Act
    await send(page.app);

    // Assert — the press was taken.
    expect(page.button.disabled).toBe(true);
  });

  it("waits for a button still disabled by the previous submission", async () => {
    // Arrange — the button frees up two settles in, as the previous
    // submission's unary answering would free it.
    const page = stubPage({ disabled: true });
    page.button.addEventListener("click", () => {
      page.button.disabled = true;
    });
    let pressable = 0;
    const original = page.app.settle.bind(page.app);
    vi.spyOn(page.app, "settle").mockImplementation(async () => {
      await original();
      pressable += 1;
      if (pressable >= 2) page.button.disabled = false;
    });

    // Act
    await send(page.app);

    // Assert — it waited rather than dropping the press on a disabled button.
    expect(page.settles()).toBeGreaterThanOrEqual(2);
  });

  it("names the composer when the button never becomes pressable", async () => {
    // Arrange — a closed gate keeps the button disabled for good.
    const page = stubPage({ disabled: true });

    // Act / Assert
    await expect(send(page.app)).rejects.toThrow(/never became pressable/);
  });

  it("spends no more than the composer budget waiting for a pressable button", async () => {
    // Arrange
    const page = stubPage({ disabled: true });
    const started = Date.now();

    // Act
    await send(page.app).catch(() => undefined);

    // Assert — the wait is bounded, and by THIS site's stated budget.
    expect(Date.now() - started).toBeLessThanOrEqual(COMPOSER_READY_BUDGET_MS + SETTLE_STEP_MS);
  });

  it("names a dropped press when the button stays enabled through the click", async () => {
    // Arrange — a composer that ignores the press, which is what an empty box,
    // a closed gate or an in-flight submission all look like from out here.
    const page = stubPage();

    // Act / Assert
    await expect(send(page.app)).rejects.toThrow(/DROPPED the press/);
  });

  it("quotes what was typed when a press is dropped", async () => {
    // Arrange
    const page = stubPage({ text: "!rotate" });

    // Act / Assert — the box's contents are the first thing that explains a
    // silently refused press.
    await expect(send(page.app)).rejects.toThrow(/"!rotate"/);
  });

  it("names the selector when there is no send button at all", async () => {
    // Arrange
    const page = stubPage();
    page.button.remove();

    // Act / Assert
    await expect(send(page.app)).rejects.toThrow(/no composer send button at/);
  });

  it("presses the button of the addressed host and no other", async () => {
    // Arrange — a bubble composer beside the root one.
    const page = stubPage();
    page.button.addEventListener("click", () => {
      page.button.disabled = true;
    });
    const bubble = document.createElement("div");
    bubble.setAttribute("data-component", "bubble-composer");
    const bubbleInput = document.createElement("textarea");
    bubbleInput.value = "!other";
    const bubbleSend = createControl();
    bubbleSend.setAttribute("data-composer-send", "");
    bubbleSend.addEventListener("click", () => {
      bubbleSend.disabled = true;
    });
    bubble.append(bubbleInput, bubbleSend);
    document.body.append(bubble);

    // Act
    await send(page.app, '[data-component="bubble-composer"]');

    // Assert
    expect(bubbleSend.disabled).toBe(true);
    expect(page.button.disabled).toBe(false);
  });
});

describe("HOST", () => {
  it("is the selector send addresses by default", () => {
    // Arrange / Act / Assert — the default host is the root composer, which is
    // what every layer file relies on when it calls `send(app)`.
    const page = stubPage();
    expect(document.querySelector(`${HOST} [data-composer-send]`)).toBe(page.button);
  });
});

describe("press", () => {
  it("answers true when the composer took the press", async () => {
    // Arrange
    const page = stubPage();
    page.button.addEventListener("click", () => {
      page.button.disabled = true;
    });

    // Act / Assert
    await expect(press(page.app)).resolves.toBe(true);
  });

  it("answers false when the composer dropped the press", async () => {
    // Arrange — an empty box, which `composer.ts` returns on before it
    // disables anything.
    const page = stubPage({ text: "" });

    // Act / Assert — dropped, and NOT a fault: §F8 #33 presses expecting this.
    await expect(press(page.app)).resolves.toBe(false);
  });
});
