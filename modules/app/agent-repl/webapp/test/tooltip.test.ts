// @vitest-environment jsdom
/**
 * The delayed hover tooltip (src/tooltip.ts): shown after the pointer rests
 * for `TOOLTIP_DELAY_MS`, owned by the host so a redraw under a resting
 * pointer keeps it, and gone on leave, press, a vanished anchor or teardown.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import {
  TOOLTIP_DELAY_MS,
  TOOLTIP_KEY_ATTR,
  TOOLTIP_TEXT_ATTR,
  armTooltip,
  mountTooltips,
  type Tooltips,
} from "../src/tooltip.js";

describe("armTooltip", () => {
  it("stamps the key and the text", () => {
    // Arrange
    const el = document.createElement("span");
    // Act
    armTooltip(el, "chip:agents", "Background subagents");
    // Assert
    expect([el.getAttribute(TOOLTIP_KEY_ATTR), el.getAttribute(TOOLTIP_TEXT_ATTR)]).toEqual([
      "chip:agents",
      "Background subagents",
    ]);
  });
});

describe("mountTooltips", () => {
  let host: HTMLElement;
  let tooltips: Tooltips;

  /** Draw one armed element per [key, text] into the host, replacing what was there. */
  function draw(...armed: [string, string][]): HTMLElement[] {
    const els = armed.map(([key, text]) => {
      const el = document.createElement("span");
      armTooltip(el, key, text);
      return el;
    });
    host.replaceChildren(...els);
    return els;
  }

  const over = (el: Element): void => {
    el.dispatchEvent(new MouseEvent("mouseover", { bubbles: true }));
  };
  const shown = (): HTMLElement[] => [...document.body.querySelectorAll<HTMLElement>(".app-tooltip")];

  beforeEach(() => {
    vi.useFakeTimers();
    host = document.createElement("div");
    document.body.append(host);
    tooltips = mountTooltips(host);
  });

  afterEach(() => {
    tooltips.dispose();
    host.remove();
    vi.useRealTimers();
  });

  it("shows nothing before the delay has passed", () => {
    // Arrange
    const [chip] = draw(["chip:agents", "Background subagents"]);
    // Act
    over(chip);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS - 1);
    // Assert
    expect(shown()).toHaveLength(0);
  });

  it("shows the element's text once the pointer has rested for the delay", () => {
    // Arrange
    const [chip] = draw(["chip:agents", "Background subagents"]);
    // Act
    over(chip);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Assert
    expect(shown().map((tip) => tip.textContent)).toEqual(["Background subagents"]);
  });

  it("waits one second", () => {
    expect(TOOLTIP_DELAY_MS).toBe(1000);
  });

  it("shows the tooltip as the tooltip role", () => {
    // Arrange
    const [chip] = draw(["chip:agents", "Background subagents"]);
    // Act
    over(chip);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Assert
    expect(shown()[0]?.getAttribute("role")).toBe("tooltip");
  });

  it("answers a hover from inside the armed element", () => {
    // Arrange
    const [chip] = draw(["chip:agents", "Background subagents"]);
    const glyph = document.createElement("span");
    chip.append(glyph);
    // Act
    over(glyph);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Assert
    expect(shown()).toHaveLength(1);
  });

  it("ignores a hover on an element with no tooltip", () => {
    // Arrange
    const plain = document.createElement("span");
    host.replaceChildren(plain);
    // Act
    over(plain);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Assert
    expect(shown()).toHaveLength(0);
  });

  it("restarts the delay when the pointer moves to another element", () => {
    // Arrange
    const [agents, shells] = draw(["chip:agents", "Background subagents"], ["chip:shells", "Background shells"]);
    over(agents);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS - 1);
    // Act
    over(shells);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS - 1);
    // Assert
    expect(shown()).toHaveLength(0);
  });

  it("moves the shown tooltip to the next element only after its own delay", () => {
    // Arrange
    const [agents, shells] = draw(["chip:agents", "Background subagents"], ["chip:shells", "Background shells"]);
    over(agents);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Act
    over(shells);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Assert
    expect(shown().map((tip) => tip.textContent)).toEqual(["Background shells"]);
  });

  it("keeps the pending delay when the element under the pointer is redrawn", () => {
    // Arrange
    const [chip] = draw(["chip:agents", "Background subagents"]);
    over(chip);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS / 2);
    // Act
    const [redrawn] = draw(["chip:agents", "Background subagents"]);
    tooltips.refresh();
    over(redrawn);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS / 2);
    // Assert
    expect(shown()).toHaveLength(1);
  });

  it("keeps a shown tooltip, with the redrawn element's text, across a redraw", () => {
    // Arrange
    const [chip] = draw(["chip:waitingOnUser", "1 waiting on you"]);
    over(chip);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Act
    draw(["chip:waitingOnUser", "2 waiting on you"]);
    tooltips.refresh();
    // Assert
    expect(shown().map((tip) => tip.textContent)).toEqual(["2 waiting on you"]);
  });

  it("hides a shown tooltip whose element a redraw removed", () => {
    // Arrange
    const [chip] = draw(["chip:agents", "Background subagents"]);
    over(chip);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Act
    draw();
    tooltips.refresh();
    // Assert
    expect(shown()).toHaveLength(0);
  });

  it("shows nothing when the element vanished before the delay passed", () => {
    // Arrange
    const [chip] = draw(["chip:agents", "Background subagents"]);
    over(chip);
    // Act
    draw();
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Assert
    expect(shown()).toHaveLength(0);
  });

  it("hides the tooltip when the pointer leaves the host", () => {
    // Arrange
    const [chip] = draw(["chip:agents", "Background subagents"]);
    over(chip);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Act
    host.dispatchEvent(new MouseEvent("mouseleave"));
    // Assert
    expect(shown()).toHaveLength(0);
  });

  it("cancels the pending delay when the pointer leaves the host", () => {
    // Arrange
    const [chip] = draw(["chip:agents", "Background subagents"]);
    over(chip);
    // Act
    host.dispatchEvent(new MouseEvent("mouseleave"));
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Assert
    expect(shown()).toHaveLength(0);
  });

  it("hides the tooltip on a press, and does not bring it back while the pointer stays", () => {
    // Arrange
    const [chip] = draw(["chip:agents", "Background subagents"]);
    over(chip);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Act
    chip.dispatchEvent(new MouseEvent("mousedown", { bubbles: true }));
    over(chip);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Assert
    expect(shown()).toHaveLength(0);
  });

  it("removes the tooltip and stops listening when disposed", () => {
    // Arrange
    const [chip] = draw(["chip:agents", "Background subagents"]);
    over(chip);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Act
    tooltips.dispose();
    over(chip);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Assert
    expect(shown()).toHaveLength(0);
  });

  it("anchors the tooltip above the element's center", () => {
    // Arrange
    const [chip] = draw(["chip:agents", "Background subagents"]);
    chip.getBoundingClientRect = () => new DOMRect(100, 50, 40, 20);
    // Act
    over(chip);
    vi.advanceTimersByTime(TOOLTIP_DELAY_MS);
    // Assert
    expect([shown()[0]?.style.left, shown()[0]?.style.top]).toEqual(["120px", "44px"]);
  });
});
