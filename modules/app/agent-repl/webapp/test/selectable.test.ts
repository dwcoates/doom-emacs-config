/** The shared `selectable` stamp, and the proof both harnesses use it. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { FeedRowSchema } from "../../proto/gen/ts/frontend/v1/feed_pb";
import { selectable } from "./selectable.js";

const here = path.dirname(fileURLToPath(import.meta.url));

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

  it("is the one stamp every selection test uses", () => {
    // Arrange
    const users = ["feed/feed-view.test.ts", "integration/bubble-selection.integration.test.ts"];
    // Act
    const handRolled = users.filter((file) =>
      readFileSync(path.join(here, file), "utf8").includes("FeedRowSelectableSchema"),
    );
    // Assert
    expect(handRolled).toEqual([]);
  });
});
