// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { applyPlacement, viewportOf } from "../src/placement.js";
import { filesUnder } from "./files-under.js";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "../src");

/** Every TypeScript source under SRC, relative to it. */
const sources = (): string[] => filesUnder(SRC).map((file) => path.relative(SRC, file));

describe("viewportOf", () => {
  it("reads the window's inner size", () => {
    const win = { innerWidth: 1200, innerHeight: 700 } as Window;
    expect(viewportOf(win)).toEqual({ width: 1200, height: 700 });
  });

  it("reads this page's own window when given none", () => {
    expect(viewportOf()).toEqual({ width: window.innerWidth, height: window.innerHeight });
  });
});

describe("applyPlacement", () => {
  it("stands the box at the placement's left, top and height cap, in pixels", () => {
    // ARRANGE
    const el = document.createElement("div");
    // ACT
    applyPlacement(el, { left: 12, top: 40, maxHeight: 300 });
    // ASSERT
    expect([el.style.left, el.style.top, el.style.maxHeight]).toEqual(["12px", "40px", "300px"]);
  });
});

describe("the placement sites share the helpers", () => {
  it("reads the window's size nowhere but viewportOf", () => {
    const readers = sources().filter((file) => file !== "placement.ts" && /\.innerWidth\b/.test(readFileSync(path.join(SRC, file), "utf8")));
    expect(readers).toEqual([]);
  });

  it("applies a viewport placement's height cap nowhere but applyPlacement, the host-relative topbar reveal aside", () => {
    // The topbar's reveal is placed against its host's box, not the window
    // (topbar/reveal.ts), so its left is translated and it applies its own.
    const appliers = sources().filter(
      (file) => !["placement.ts", path.join("topbar", "reveal.ts")].includes(file) && /\.maxHeight\}px`/.test(readFileSync(path.join(SRC, file), "utf8")),
    );
    expect(appliers).toEqual([]);
  });
});
