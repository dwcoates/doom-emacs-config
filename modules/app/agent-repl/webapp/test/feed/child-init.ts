/**
 * A child bubble's head as every fixture spells one: an open child named
 * `fix-flaky`, whose name selects `ws-child`, landing on `~/.config/doom`,
 * opened at STARTEDATMS. A fixture edits the init it is handed for any other
 * standing or target.
 */
import type { MessageInitShape } from "@bufbuild/protobuf";
import type { FeedChildHeadSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { FEED_MERGE_HEAD_GLYPH } from "../../src/vocab.js";

export function childHeadInit(startedAtMs: bigint): MessageInitShape<typeof FeedChildHeadSchema> {
  return {
    glyph: { icon: FEED_MERGE_HEAD_GLYPH },
    name: { text: "fix-flaky", select: { target: { id: "ws-child", dir: "/w/child" } } },
    target: { text: "~/.config/doom" },
    standing: { standing: { case: "open", value: {} } },
    runtime: { startedAtMs },
  };
}
