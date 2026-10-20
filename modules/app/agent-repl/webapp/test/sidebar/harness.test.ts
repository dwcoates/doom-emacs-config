// @vitest-environment jsdom
// The sidebar tests' shared harness. See test/sidebar/harness.ts.
import { readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it, vi } from "vitest";
import { swallowTheNextClick } from "../../src/sidebar/drag.js";
import { codeOf } from "../source-text.js";
import { drawnEntries, primaryPointer, settleOwedDragClick } from "./harness.js";

describe("primaryPointer", () => {
  it("dispatches a bubbling primary left-button pointer event at the height asked", () => {
    // Arrange
    const parent = document.createElement("div");
    const target = document.createElement("span");
    parent.appendChild(target);
    const seen: PointerEvent[] = [];
    parent.addEventListener("pointerdown", (event) => seen.push(event));

    // Act
    primaryPointer("pointerdown", target, 42);

    // Assert
    expect(seen.map((e) => [e.type, e.isPrimary, e.button, e.pointerId, e.clientY])).toEqual([["pointerdown", true, 0, 1, 42]]);
  });

  it("is the one primary pointer the sidebar tests build", () => {
    // Arrange
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
    const sources = import.meta.glob("./*.ts", { query: "?raw", import: "default", eager: true }) as Record<string, string>;

    // Act
    const handRolled = Object.entries(sources)
      .filter(([path]) => path !== "./harness.ts" && path !== "./harness.test.ts")
      .filter(([, src]) => /new PointerEvent\([^)]*isPrimary: true/.test(src))
      .map(([path]) => path);

    // Assert
    expect(handRolled).toEqual([]);
  });
});

describe("settleOwedDragClick", () => {
  it("takes down a guard a drag left owed, so the next test's click goes through", () => {
    // Arrange
    swallowTheNextClick(window);

    // Act
    settleOwedDragClick();

    // Assert
    const reached = vi.fn();
    document.body.addEventListener("click", reached);
    document.body.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    document.body.removeEventListener("click", reached);
    expect(reached).toHaveBeenCalledTimes(1);
  });

  it("is called after each test by every sidebar test file that drags", () => {
    // A file that drags without it hands its owed click to whatever test runs next.
    const dir = __dirname;
    const offenders = readdirSync(dir)
      .filter((name) => name.endsWith(".test.ts"))
      .filter((name) => {
        const code = codeOf(readFileSync(join(dir, name), "utf8"));
        return /"pointermove"/.test(code) && !code.includes("settleOwedDragClick");
      });
    expect(offenders).toEqual([]);
  });
});

describe("drawnEntries", () => {
  it("spells a section list's children, a row by its id and a subsection as [id]", () => {
    // Arrange
    const list = document.createElement("div");
    const rowEl = document.createElement("div");
    rowEl.setAttribute("data-roster-row", "a");
    const sub = document.createElement("div");
    sub.className = "sb-subsection";
    sub.setAttribute("data-subsection", "foo");
    list.append(rowEl, sub);

    // Act / Assert
    expect(drawnEntries(list)).toEqual(["a", "[foo]"]);
  });

  it("is the one spelling the sidebar tests read a section's list by", () => {
    // Arrange
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- see "is the one primary pointer the sidebar tests build".
    const sources = import.meta.glob("./*.test.ts", { query: "?raw", import: "default", eager: true }) as Record<string, string>;

    // Act
    const handRolled = Object.entries(sources)
      .filter(([, source]) => /getAttribute\("data-roster-row"\) \?\? `\[\$\{el\.getAttribute\("data-subsection"\)/.test(source))
      .map(([file]) => file);

    // Assert
    expect(handRolled).toEqual([]);
  });
});
