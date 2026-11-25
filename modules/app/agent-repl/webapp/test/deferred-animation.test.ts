// @vitest-environment jsdom
import { describe, expect, it, vi } from "vitest";
import { deferredAnimation } from "./deferred-animation.js";

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
