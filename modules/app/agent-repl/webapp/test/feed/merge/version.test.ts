// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedMergeVersionSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { drawFeedMergeVersion } from "../../../src/feed/merge/version.js";
import { drawFeedMerge } from "../../../src/feed/merge/merge.js";
import { harness, mergeRow, rowContext } from "../harness.js";
import { mergeHead } from "./fixtures.js";

describe("drawFeedMergeVersion", () => {
  type Runs = { leading: string; incremented: string; trailing?: string };
  it.each<[string, Runs, string]>([
    ["a minor", { leading: "0.", incremented: "5", trailing: ".0" }, "0.5.0"],
    ["a patch", { leading: "0.4.", incremented: "8" }, "0.4.8"],
  ])("draws %s as the version's tag, its runs verbatim", (_name, version, tag) => {
    // Arrange
    const msg = create(FeedMergeVersionSchema, {
      leading: { text: version.leading },
      incremented: { text: version.incremented },
      trailing: version.trailing === undefined ? undefined : { text: version.trailing },
    });

    // Act
    const el = drawFeedMergeVersion(msg);

    // Assert
    expect(el.textContent).toBe(tag);
    expect(el.querySelector(".merge-version-incremented")?.textContent).toBe(version.incremented);
    expect(el.querySelector(".merge-version-trailing") !== null).toBe(version.trailing !== undefined);
  });

  it("refuses a version with no incremented number", () => {
    // Arrange
    const msg = create(FeedMergeVersionSchema, { leading: { text: "0." } });

    // Act / Assert
    expect(() => drawFeedMergeVersion(msg)).toThrow(MalformedView);
  });
});

describe("drawFeedMerge: the landed version", () => {
  it("draws the version on a landed head that carries one", () => {
    // Arrange
    const h = harness();
    const msg = mergeHead(
      { case: "success", endedAtMs: 61_000n, commit: "a1b2c3d" },
      { version: { leading: "0.", incremented: "5", trailing: ".0" } },
    );

    // Act
    const el = drawFeedMerge(msg, rowContext(h.ctx, mergeRow("m1")));

    // Assert
    expect(el.querySelector(".merge-version")?.textContent).toBe("0.5.0");
  });

  it("draws no version on a landed head that carries none", () => {
    // Arrange
    const h = harness();
    const msg = mergeHead({ case: "success", endedAtMs: 61_000n, commit: "a1b2c3d" });

    // Act
    const el = drawFeedMerge(msg, rowContext(h.ctx, mergeRow("m1")));

    // Assert
    expect(el.querySelector(".merge-version")).toBeNull();
  });
});
