// @vitest-environment jsdom
/**
 * test/setup-shared-worker.ts's hygiene, read from inside the un-isolated
 * unit run it serves. The cases run in declaration order: each arranges in
 * one test what the setup must have undone by the next.
 */
import { describe, expect, it, vi } from "vitest";

describe("the shared worker's clock", () => {
  it("leaves a test free to install a fake clock it never uninstalls", () => {
    // Arrange / Act — a fake clock this file deliberately leaves behind.
    vi.useFakeTimers();

    // Assert
    expect(vi.isFakeTimers()).toBe(true);
  });

  it("hands the next test the real clock", () => {
    // Assert
    expect(vi.isFakeTimers()).toBe(false);
  });

  it("hands the next test a working setTimeout", async () => {
    // Act
    const fired = await new Promise<boolean>((resolve) => setTimeout(() => resolve(true), 0));

    // Assert
    expect(fired).toBe(true);
  });
});

describe("the shared worker's page", () => {
  it("leaves a test free to append to the page", () => {
    // Arrange / Act
    document.body.append(document.createElement("aside"));

    // Assert
    expect(document.body.children).toHaveLength(1);
  });

  it("hands the next test an empty page", () => {
    // Assert
    expect(document.body.children).toHaveLength(0);
  });

  it("leaves a test free to mark the root element", () => {
    // Arrange / Act
    document.documentElement.setAttribute("data-conversation-drawn", "page");
    document.documentElement.classList.add("dragging");

    // Assert
    expect(document.documentElement.getAttributeNames()).toHaveLength(2);
  });

  it("hands the next test a bare root element", () => {
    // Assert
    expect(document.documentElement.getAttributeNames()).toEqual([]);
  });

  let environmentView: (Window & typeof globalThis) | null = null;

  it("leaves a test free to define and redefine properties on the document", () => {
    // Arrange
    environmentView = document.defaultView;

    // Act
    Object.defineProperty(document, "visibilityState", { configurable: true, get: () => "hidden" });
    Object.defineProperty(document, "defaultView", { configurable: true, value: null });

    // Assert
    expect([document.visibilityState, document.defaultView]).toEqual(["hidden", null]);
  });

  it("hands the next test the document as the environment made it", () => {
    // Assert
    expect(Object.getOwnPropertyDescriptor(document, "visibilityState")).toBeUndefined();
    expect(document.visibilityState).toBe("visible");
    expect(document.defaultView).toBe(environmentView);
  });
});
