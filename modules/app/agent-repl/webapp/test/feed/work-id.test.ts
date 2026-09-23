// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedDetachedWorkIdSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawDetachedWorkId, WORK_ID_ATTRIBUTE, WORK_ID_CLASS } from "../../src/feed/work-id.js";

describe("drawDetachedWorkId", () => {
  it("draws nothing when the daemon named no work", () => {
    // Arrange, Act
    const el = drawDetachedWorkId(undefined);

    // Assert
    expect(el).toBeNull();
  });

  it("draws the id verbatim, never shortened", () => {
    // Arrange
    const id = "toolu_01Ah5CB9uvhaLVdtsffFm69E";

    // Act
    const el = drawDetachedWorkId(create(FeedDetachedWorkIdSchema, { text: id }));

    // Assert
    expect(el?.textContent).toBe(id);
  });

  it("states the id on its hook attribute and wears the one class", () => {
    // Arrange, Act
    const el = drawDetachedWorkId(create(FeedDetachedWorkIdSchema, { text: "work-1" }));

    // Assert
    expect(el?.getAttribute(WORK_ID_ATTRIBUTE)).toBe("work-1");
    expect(el?.classList.contains(WORK_ID_CLASS)).toBe(true);
  });
});
