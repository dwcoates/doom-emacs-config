// @vitest-environment jsdom
// One drawn section's registrations, torn down together. See src/sidebar/scope.ts.
import { describe, expect, it } from "vitest";
import type { Dropdown } from "../../src/sidebar/dropdowns.js";
import { openScope } from "../../src/sidebar/scope.js";
import { sidebarContext } from "./harness.js";

/** A dropdown boxed by an element under ROOT, counting its closes. */
function openDropdownUnder(root: HTMLElement): Dropdown & { closes: number } {
  const element = document.createElement("div");
  root.appendChild(element);
  const opener = document.createElement("button");
  const d = {
    kind: "test",
    key: "test",
    element,
    opener,
    openers: [opener],
    closes: 0,
    close: () => {
      d.closes += 1;
    },
  };
  return d;
}

describe("openScope", () => {
  it("runs every teardown registered through its context when disposed", () => {
    // Arrange
    const scope = openScope(sidebarContext());
    let ran = 0;
    scope.context.onDispose(() => (ran += 1));
    scope.context.onDispose(() => (ran += 1));

    // Act
    scope.dispose(document.createElement("div"));

    // Assert
    expect(ran).toBe(2);
  });

  it("runs a teardown once however often it is disposed", () => {
    // Arrange
    const scope = openScope(sidebarContext());
    let ran = 0;
    scope.context.onDispose(() => (ran += 1));
    scope.dispose(document.createElement("div"));

    // Act
    scope.dispose(document.createElement("div"));

    // Assert
    expect(ran).toBe(1);
  });

  it("stops painting the view copies tracked through its context when disposed", () => {
    // Arrange
    const rail = sidebarContext();
    const scope = openScope(rail);
    const painted: boolean[] = [];
    scope.context.view.track("k", false, (value) => painted.push(value));

    // Act
    scope.dispose(document.createElement("div"));
    rail.view.ask("k", true);

    // Assert
    expect(painted).toEqual([]);
  });

  it("asks the rail's view for a change made through its context", () => {
    // Arrange
    const rail = sidebarContext();
    const scope = openScope(rail);
    const kept = openScope(rail);
    const painted: boolean[] = [];
    kept.context.view.track("k", false, (value) => painted.push(value));

    // Act
    scope.context.view.ask("k", true);

    // Assert
    expect(painted).toEqual([true]);
  });

  it("marks attention on the rail's registry", () => {
    // Arrange
    const rail = sidebarContext();
    const scope = openScope(rail);

    // Act
    scope.context.attention.mark("ws-1", document.createElement("div"));

    // Assert
    expect(rail.attention.stateOf("ws-1")).not.toBeNull();
  });

  it("marks its markers again on a new pass that keeps its section", () => {
    // Arrange
    const rail = sidebarContext();
    const scope = openScope(rail);
    rail.attention.beginPass();
    scope.context.attention.mark("ws-1", document.createElement("div"));
    rail.attention.endPass();

    // Act
    rail.attention.beginPass();
    scope.remark(rail.attention);
    rail.attention.endPass();

    // Assert
    expect(rail.attention.stateOf("ws-1")).not.toBeNull();
  });

  it("forgets a dropdown opened under its section's drawing when disposed", () => {
    // Arrange
    const rail = sidebarContext();
    const scope = openScope(rail);
    const root = document.createElement("div");
    const d = openDropdownUnder(root);
    rail.dropdowns.opened(d);

    // Act
    scope.dispose(root);
    rail.dropdowns.dismissOutside(null);

    // Assert
    expect(d.closes).toBe(0);
  });

  it("leaves a dropdown opened under another section's drawing", () => {
    // Arrange
    const rail = sidebarContext();
    const scope = openScope(rail);
    const d = openDropdownUnder(document.createElement("div"));
    rail.dropdowns.opened(d);

    // Act
    scope.dispose(document.createElement("div"));
    rail.dropdowns.dismissOutside(null);

    // Assert
    expect(d.closes).toBe(1);
  });

  it("forgets the row hover copies drawn under its section's drawing when disposed", () => {
    // Arrange
    const rail = sidebarContext();
    const scope = openScope(rail);
    const ws = document.createElement("div");
    const line = document.createElement("div");
    const trigger = document.createElement("span");
    const detail = document.createElement("div");
    line.appendChild(trigger);
    ws.append(line, detail);
    rail.rowHover.drawn("ws-1", { ws, line, trigger, detail });

    // Act
    scope.dispose(ws);
    rail.rowHover.enter("ws-1", "trigger");

    // Assert
    expect(ws.classList.contains("open")).toBe(false);
  });
});
