import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedChildHeadSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { FEED_MERGE_HEAD_GLYPH } from "../../src/vocab.js";
import { childHeadInit } from "./child-init.js";

describe("childHeadInit", () => {
  it("spells an open child whose name selects it, under the merge head's glyph", () => {
    const head = create(FeedChildHeadSchema, childHeadInit(7n));
    expect([
      head.glyph?.icon,
      head.name?.text,
      head.name?.select?.target?.id,
      head.target?.text,
      head.standing?.standing.case,
      head.runtime?.startedAtMs,
    ]).toEqual([FEED_MERGE_HEAD_GLYPH, "fix-flaky", "ws-child", "~/.config/doom", "open", 7n]);
  });
});
