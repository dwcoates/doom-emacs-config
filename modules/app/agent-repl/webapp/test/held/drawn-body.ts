/**
 * drawnBodyOf — the body the daemon draws a held card's `said` as, for a test
 * fixture: each text block its words, each image by path a source named for
 * its file (as the image origin would serve it), each quote its parts. The
 * daemon's own resolution is `feed.DrawUserBlocks`; a fixture only needs a
 * body the card can draw that is recognizably the said's.
 */
import { create } from "@bufbuild/protobuf";
import {
  FeedUserPromptBodySchema,
  type FeedUserPromptBlock,
  type FeedUserPromptBody,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { UserSaid } from "../../../proto/gen/ts/conversation/v1/user_pb";

/** The source a fixture's image by PATH is served under. */
export function servedSrcOf(path: string): string {
  return `/feed-images/${path.split("/").pop() ?? ""}`;
}

export function drawnBodyOf(said: UserSaid | undefined): FeedUserPromptBody {
  const blocks: FeedUserPromptBlock["block"][] = [];
  for (const block of said?.content?.blocks ?? []) {
    switch (block.block.case) {
      case "text":
        blocks.push({ case: "text", value: { text: block.block.value.text } } as FeedUserPromptBlock["block"]);
        break;
      case "image": {
        const location = block.block.value.location;
        const src = location.case === "path" ? servedSrcOf(location.value.path) : location.case === "url" ? location.value.url : "";
        const alt = location.case === "path" ? (location.value.path.split("/").pop() ?? "") : "";
        blocks.push({ case: "image", value: { src, alt } } as FeedUserPromptBlock["block"]);
        break;
      }
      case "quote":
        blocks.push({
          case: "quote",
          value: { heading: "⟢ Replying to an earlier response", quoted: block.block.value.text, closing: "⟢ My message" },
        } as FeedUserPromptBlock["block"]);
        break;
      default:
        break;
    }
  }
  return create(FeedUserPromptBodySchema, { blocks: blocks.map((block) => ({ block })) });
}
