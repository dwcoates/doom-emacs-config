// @vitest-environment jsdom
// Reading and moving a page's CSS animations by their delays, never by asking
// WebKit for one. See src/animation-delays.ts.
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it } from "vitest";

import {
  ANIMATION_RESTARTING_ATTRIBUTE,
  PSEUDO_DELAY,
  animatedParts,
  cssDurationMs,
  cssTimeMs,
  nth,
  partAnimations,
  placeOneShot,
  restartAnimations,
  writeDelays,
} from "../src/animation-delays.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";
import { codeUnder } from "./files-under.js";
import { readFileSync } from "node:fs";
import { codeOf } from "./source-text.js";

const here = path.dirname(fileURLToPath(import.meta.url));

/** A computed style listing these animation properties. */
function computed(fields: Partial<Record<"animationName" | "animationDuration" | "animationIterationCount" | "animationDirection" | "animationDelay", string>>): CSSStyleDeclaration {
  return {
    animationName: "none",
    animationDuration: "0s",
    animationIterationCount: "1",
    animationDirection: "normal",
    animationDelay: "0s",
    ...fields,
  } as unknown as CSSStyleDeclaration;
}

afterEach(() => {
  document.body.replaceChildren();
});

describe("reading a part's animations", () => {
  it("reads every list off the computed style", () => {
    // Arrange
    const el = document.createElement("div");

    // Act
    const read = partAnimations(
      el,
      "::before",
      computed({ animationName: "a, b", animationDuration: "1s, 200ms", animationIterationCount: "infinite, 2", animationDirection: "alternate, normal", animationDelay: "-50ms" }),
    );

    // Assert
    expect(read).toEqual({
      element: el,
      part: "::before",
      names: ["a", "b"],
      durations: [1000, 200],
      iterations: [Infinity, 2],
      directions: ["alternate", "normal"],
      delays: [-50],
    });
  });

  it("reads nothing off a part running no animation", () => {
    // Arrange / Act
    const read = partAnimations(document.createElement("div"), "", computed({ animationName: "none" }));

    // Assert
    expect(read).toBeNull();
  });

  it("reads nothing off a part whose style declares no animation", () => {
    // Arrange / Act
    const read = partAnimations(document.createElement("div"), "", computed({ animationName: "" }));

    // Assert
    expect(read).toBeNull();
  });

  it("finds the element's own animation and passes over its pseudo-elements that run none", () => {
    // Arrange
    const el = document.createElement("div");
    el.style.animationName = "spin";
    document.body.append(el);

    // Act
    const parts = animatedParts(el, window);

    // Assert
    expect(parts.map((p) => p.part)).toEqual([""]);
  });

  it("finds nothing on an element that is neither HTML nor SVG", () => {
    // Arrange
    const el = document.createElementNS("http://www.w3.org/1998/Math/MathML", "math");
    document.body.append(el);

    // Act
    const parts = animatedParts(el, window);

    // Assert
    expect(parts).toEqual([]);
  });

  it("repeats a list shorter than the names, as CSS does", () => {
    // Arrange / Act / Assert
    expect([nth([1, 2], 0), nth([1, 2], 1), nth([1, 2], 2)]).toEqual([1, 2, 1]);
  });
});

describe("writing a part's delays", () => {
  it("writes the element's own delays inline", () => {
    // Arrange
    const el = document.createElement("div");

    // Act
    writeDelays(el, "", [-300, 50]);

    // Assert
    expect(el.style.animationDelay).toBe("-300ms, 50ms");
  });

  it.each(["::before", "::after"] as const)("writes a %s's delays to its element's property, marking the element", (part) => {
    // Arrange
    const el = document.createElement("div");

    // Act
    writeDelays(el, part, [-300]);

    // Assert
    expect([el.style.getPropertyValue(PSEUDO_DELAY[part].property), el.hasAttribute(PSEUDO_DELAY[part].attribute), el.style.animationDelay]).toEqual(["-300ms", true, ""]);
  });
});

describe("restarting animations", () => {
  it("marks every element for the one flush, and clears the mark after", () => {
    // Arrange
    const a = document.createElement("div");
    const b = document.createElement("div");
    const during: boolean[] = [];

    // Act
    restartAnimations([a, b], () => during.push(a.hasAttribute(ANIMATION_RESTARTING_ATTRIBUTE), b.hasAttribute(ANIMATION_RESTARTING_ATTRIBUTE)));

    // Assert
    expect([during, a.hasAttribute(ANIMATION_RESTARTING_ATTRIBUTE), b.hasAttribute(ANIMATION_RESTARTING_ATTRIBUTE)]).toEqual([[true, true], false, false]);
  });

  it("flushes nothing when there is nothing to restart", () => {
    // Arrange
    let flushes = 0;

    // Act
    restartAnimations([], () => {
      flushes += 1;
    });

    // Assert
    expect(flushes).toBe(0);
  });
});

describe("reading a computed time", () => {
  it.each([
    { value: "1.5s", ms: 1500 },
    { value: "-200ms", ms: -200 },
    { value: "0s", ms: 0 },
  ])("reads $value as $ms ms", ({ value, ms }) => {
    // Arrange / Act / Assert
    expect(cssTimeMs(value)).toBe(ms);
  });

  it("throws on a value that is no CSS time", () => {
    // Arrange / Act / Assert
    expect(() => cssTimeMs("auto")).toThrow(/no CSS time/);
  });

  it("records the broken time at error with the value read", async () => {
    // Arrange
    const capture = captureLogRecords();

    // Act
    expect(() => cssTimeMs("auto")).toThrow();

    // Assert
    const record = await forwardedRecord(capture, "animation-delays.bad-time");
    expect([record.level.case, record.context?.["value"]]).toEqual(["error", "auto"]);
  });
});

describe("reading a computed duration", () => {
  it("reads auto, the initial duration, as 0", () => {
    // Arrange / Act / Assert
    expect(cssDurationMs("auto")).toBe(0);
  });

  it("reads a CSS time as that time", () => {
    // Arrange / Act / Assert
    expect(cssDurationMs("1.3s")).toBe(1300);
  });
});

describe("the call sites", () => {
  const code = codeUnder(path.join(here, "../src"));

  it("ask WebKit for no animation anywhere in the webapp", () => {
    // Arrange / Act
    const asking = [...code].filter(([, text]) => text.includes("getAnimations(")).map(([file]) => file);

    // Assert
    expect(asking).toEqual([]);
  });

  it.each(["sidebar/wall-phase.ts", "host/unshown-animations.ts", "page-census.ts"])(
    "%s reads animations through the shared helpers, with no time parsing of its own",
    (file) => {
      // Arrange
      const text = code.get(file) ?? "";

      // Act
      const shape = [text.includes('from "../animation-delays.js"') || text.includes('from "./animation-delays.js"'), /function cssTimeMs|function cssList/.test(text)];

      // Assert
      expect(shape).toEqual([true, false]);
    },
  );
});

describe("placeOneShot", () => {
  it.each([
    { name: "hands the element the length and minus the time already run", elapsed: 1_250, want: ["5000ms", "-1250ms"] },
    { name: "reads a run not yet begun as no delay at all", elapsed: 0, want: ["5000ms", "0ms"] },
    { name: "places the animation to the whole millisecond", elapsed: 1_249.6, want: ["5000ms", "-1250ms"] },
  ])("$name", ({ elapsed, want }) => {
    // Arrange
    const el = document.createElement("span");

    // Act
    placeOneShot(el, "--length", 5_000, "--delay", elapsed);

    // Assert
    expect([el.style.getPropertyValue("--length"), el.style.getPropertyValue("--delay")]).toEqual(want);
  });

  // EVERY ONE-SHOT IS PLACED BY THE ONE HELPER: a site that spells its own
  // length and negative delay would drift from the others' format.
  it.each(["sidebar/fold-fade.ts", "sidebar/selection-fade.ts"])(
    "places %s's one-shot through placeOneShot",
    (file) => {
      // Arrange
      const code = codeOf(readFileSync(path.join(here, "../src", file), "utf8"));

      // Act / Assert
      expect([code.includes("placeOneShot("), /setProperty\([^)]*`\$\{-|setProperty\([^)]*`-\$\{/.test(code)]).toEqual([true, false]);
    },
  );
});
