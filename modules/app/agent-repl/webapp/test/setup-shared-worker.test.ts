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
});
