// @vitest-environment jsdom
// Placing a box's children without disturbing those in place. See src/sidebar/children.ts.
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { placeChildren } from "../../src/sidebar/children.js";
import { codeOf } from "../source-text.js";

/** The child-list mutations PARENT sees while ACT runs. */
async function mutationsDuring(parent: Element, act: () => void): Promise<number> {
  const seen: MutationRecord[] = [];
  const observer = new MutationObserver((records) => seen.push(...records));
  observer.observe(parent, { childList: true });
  act();
  await Promise.resolve();
  seen.push(...observer.takeRecords());
  observer.disconnect();
  return seen.length;
}

describe("placeChildren", () => {
  it("touches nothing when the children are already in place", async () => {
    // Arrange
    const parent = document.createElement("div");
    const kids = [document.createElement("p"), document.createElement("p")];
    parent.append(...kids);

    // Act
    const mutations = await mutationsDuring(parent, () => placeChildren(parent, kids));

    // Assert
    expect(mutations).toBe(0);
  });

  it.each([
    ["reordered", (a: Element, b: Element) => [b, a]],
    ["one fewer", (a: Element) => [a]],
    ["one more", (a: Element, b: Element) => [a, b, document.createElement("p")]],
  ])("places children %s", async (_name, next) => {
    // Arrange
    const parent = document.createElement("div");
    const a = document.createElement("p");
    const b = document.createElement("p");
    parent.append(a, b);
    const want = next(a, b);

    // Act
    const mutations = await mutationsDuring(parent, () => placeChildren(parent, want));

    // Assert
    expect([mutations > 0, [...parent.childNodes]]).toEqual([true, want]);
  });

  it("is how the rail places its roster and its sections", () => {
    // Arrange
    const roster = codeOf(readFileSync(join(__dirname, "..", "..", "src", "sidebar", "roster.ts"), "utf8"));
    const sidebar = codeOf(readFileSync(join(__dirname, "..", "..", "src", "sidebar", "sidebar.ts"), "utf8"));
    const draw = roster.slice(roster.indexOf("draw(u: WorkspaceRoster): HTMLElement {"), roster.indexOf("dispose(): void {", roster.indexOf("draw(u: WorkspaceRoster)")));
    const drawNow = sidebar.slice(sidebar.indexOf("function drawRosterNow("), sidebar.indexOf("return {", sidebar.indexOf("function drawRosterNow(")));

    // Act / Assert
    expect([draw.includes("replaceChildren("), drawNow.includes("replaceChildren(")]).toEqual([false, false]);
    expect([draw.includes("placeChildren("), drawNow.includes("placeChildren(")]).toEqual([true, true]);
  });
});
