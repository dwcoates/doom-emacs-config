/** The shared `selectable` stamp. The proof both harnesses use it is rules/selectable.rules.test.ts. */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { FeedRowSchema } from "../../proto/gen/ts/frontend/v1/feed_pb";
import { selectable } from "./selectable.js";

describe("selectable", () => {
  it("stamps the row selectable", () => {
    // Arrange
    const row = create(FeedRowSchema, {});
    // Act
    const stamped = selectable(row);
    // Assert
    expect(stamped.selectable).toBeDefined();
  });

  it("answers the row it was given", () => {
    // Arrange
    const row = create(FeedRowSchema, {});
    // Act / Assert
    expect(selectable(row)).toBe(row);
  });
});
