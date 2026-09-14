import { afterEach, describe, expect, it } from "vitest";
import {
  compactionProgress,
  onCompactionProgress,
  publishCompactionProgress,
  resetCompactionProgress,
} from "../../src/footer/progress.js";

afterEach(() => {
  resetCompactionProgress();
});

describe("the standing compaction line", () => {
  it("is null before any footer push", () => {
    // Arrange / Act / Assert
    expect(compactionProgress()).toBeNull();
  });

  it("is the line the last push carried", () => {
    // Arrange / Act
    publishCompactionProgress("compacting · reading the transcript");
    // Assert
    expect(compactionProgress()).toBe("compacting · reading the transcript");
  });

  it("is null again once a push carries no compaction", () => {
    // Arrange
    publishCompactionProgress("compacting · reading the transcript");
    // Act
    publishCompactionProgress(null);
    // Assert
    expect(compactionProgress()).toBeNull();
  });
});

describe("subscribing", () => {
  it("tells a late subscriber the line already standing", () => {
    // Arrange
    publishCompactionProgress("compacting · summarizing");
    const seen: (string | null)[] = [];
    // Act
    onCompactionProgress((text) => seen.push(text));
    // Assert
    expect(seen).toEqual(["compacting · summarizing"]);
  });

  it("tells a subscriber joining a quiet page there is no line", () => {
    // Arrange
    const seen: (string | null)[] = [];
    // Act
    onCompactionProgress((text) => seen.push(text));
    // Assert
    expect(seen).toEqual([null]);
  });

  it("publishes each new line to its subscribers", () => {
    // Arrange
    const seen: (string | null)[] = [];
    onCompactionProgress((text) => seen.push(text));
    // Act
    publishCompactionProgress("compacting · reading");
    publishCompactionProgress("compacting · summarizing");
    // Assert
    expect(seen).toEqual([null, "compacting · reading", "compacting · summarizing"]);
  });

  it("publishes nothing when the line is unchanged", () => {
    // Arrange
    const seen: (string | null)[] = [];
    onCompactionProgress((text) => seen.push(text));
    // Act
    publishCompactionProgress("compacting · reading");
    publishCompactionProgress("compacting · reading");
    // Assert
    expect(seen).toEqual([null, "compacting · reading"]);
  });

  it("stops publishing to an unsubscribed listener", () => {
    // Arrange
    const seen: (string | null)[] = [];
    const off = onCompactionProgress((text) => seen.push(text));
    // Act
    off();
    publishCompactionProgress("compacting · reading");
    // Assert
    expect(seen).toEqual([null]);
  });

  it("keeps publishing to the others when one unsubscribes as it runs", () => {
    // Arrange
    const seen: (string | null)[] = [];
    // The immediate call runs before `off` is bound, so the self-drop happens
    // on the first PUBLISHED line rather than on the subscribe.
    let off: () => void = () => {};
    off = onCompactionProgress((text) => {
      if (text !== null) off();
    });
    onCompactionProgress((text) => seen.push(text));
    // Act
    publishCompactionProgress("compacting · reading");
    // Assert
    expect(seen).toEqual([null, "compacting · reading"]);
  });
});
