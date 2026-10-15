// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it, vi } from "vitest";
import { deferredAnimation } from "./deferred-animation.js";
import { filesUnder } from "./files-under.js";

describe("deferredAnimation", () => {
  it("leaves finished pending until the test settles it", async () => {
    // Arrange
    const { animation } = deferredAnimation();
    const done = vi.fn();
    void animation.finished.then(done, done);
    // Act
    await Promise.resolve();
    // Assert
    expect(done).not.toHaveBeenCalled();
  });

  it("resolves finished on finish, its handlers run by the time finish returns", async () => {
    // Arrange
    const fade = deferredAnimation();
    const done = vi.fn();
    void fade.animation.finished.then(done);
    // Act
    await fade.finish();
    // Assert
    expect(done).toHaveBeenCalledTimes(1);
  });

  it("rejects finished with an AbortError on cancel, its handlers run by the time cancel returns", async () => {
    // Arrange
    const fade = deferredAnimation();
    const failed = vi.fn();
    fade.animation.finished.catch(failed);
    // Act
    await fade.cancel();
    // Assert
    expect(String(failed.mock.calls[0]?.[0])).toBe("AbortError: cancelled");
  });
});

describe("deferredAnimation's call sites", () => {
  it("is the one deferred animation: no suite hand-rolls a pending finished", () => {
    // Arrange
    const root = join(__dirname);
    const helper = join(root, "deferred-animation.ts");
    // Act
    const handRolled = filesUnder(root).filter(
      (path) => path !== helper && /\{ finished \} as unknown as Animation/.test(readFileSync(path, "utf8")),
    );
    // Assert
    expect(handRolled).toEqual([]);
  });
});
