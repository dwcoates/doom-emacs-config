// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { stampedAge } from "../../../src/feed/asks/stamped-age.js";
import { TICKING_ATTRIBUTE, stopClocks, stopTicking } from "../../../src/feed/ticking.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { askHarness } from "./harness.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
  document.body.replaceChildren();
});

describe("stampedAge", () => {
  it("reads the instant as a relative age", () => {
    // Arrange
    vi.setSystemTime(60_000);
    // Act
    const el = stampedAge(0n, "x.at_ms", askHarness().rc, "when");
    // Assert
    expect(el.textContent).toBe("1m ago");
  });

  it("wears the class it was given", () => {
    // Arrange, Act
    const el = stampedAge(0n, "x.at_ms", askHarness().rc, "perm-when");
    // Assert
    expect(el.className).toBe("perm-when");
  });

  it("advances on a later tick", async () => {
    // Arrange
    vi.setSystemTime(0);
    const el = stampedAge(0n, "x.at_ms", askHarness().rc, "when");
    document.body.append(el);
    // Act
    await vi.advanceTimersByTimeAsync(5_000);
    // Assert
    expect(el.textContent).toBe("5s ago");
  });

  it("keeps counting through a clock stop, since an age stays true after the turn", async () => {
    // Arrange
    vi.setSystemTime(0);
    const el = stampedAge(0n, "x.at_ms", askHarness().rc, "when");
    document.body.append(el);
    // Act
    stopClocks(el);
    await vi.advanceTimersByTimeAsync(5_000);
    // Assert
    expect(el.textContent).toBe("5s ago");
  });

  it("stops on a discard", () => {
    // Arrange
    const el = stampedAge(0n, "x.at_ms", askHarness().rc, "when");
    // Act
    stopTicking(el);
    // Assert
    expect(el.hasAttribute(TICKING_ATTRIBUTE)).toBe(false);
  });

  it("refuses an instant past the safe range as malformed", () => {
    // Arrange
    const tooBig = BigInt(Number.MAX_SAFE_INTEGER) + 1n;
    // Act, Assert
    expect(() => stampedAge(tooBig, "x.at_ms", askHarness().rc, "when")).toThrow(MalformedView);
  });
});

describe("stampedAge: the asks share it", () => {
  it.each(["permission.ts", "question.ts", "cold-gate.ts"])(
    "%s draws its settled instant through the shared helper, not a hand-rolled age",
    (file) => {
      // Arrange
      const source = readFileSync(join(__dirname, "../../../src/feed/asks", file), "utf8");
      // Act
      const handRolled = /`\$\{formatTickedAge\([^)]*\)\} ago`/.test(source);
      const shared = /stampedAge\(/.test(source);
      // Assert
      expect([handRolled, shared]).toEqual([false, true]);
    },
  );
});
