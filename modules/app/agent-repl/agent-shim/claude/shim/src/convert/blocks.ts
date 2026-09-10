/**
 * convert/blocks.ts — the vendor's content blocks, in this contract's terms.
 *
 * THE CONTENT MODEL IS NEUTRAL BY DESIGN. The vendor names seventeen block
 * kinds for its own API surface; this contract models the two facts underneath
 * them and lets the tool's own name carry the rest. So the mapping here is
 * small and deliberately narrow per site: what a TOOL returned and what a PERSON
 * said are different unions, because a tool result carrying a tool call would be
 * representable and meaningless.
 *
 * `UnsupportedBlock` IS NOT A FALLBACK. It is for a block whose kind is
 * genuinely unknowable — one the vendor introduced that nothing here has seen —
 * and never for one whose modelling was inconvenient. A reader who finds a
 * recognizable kind in it is looking at a producer defect.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";
import { rawStruct } from "./residue.js";

const LOGGER = bindLog({ component: "shim-convert-blocks", operation: "shim.convert.blocks" });

/** A vendor content block, as loosely as it must be read before it is typed. */
interface VendorBlock {
  readonly type?: string;
  readonly text?: string;
  readonly source?: { readonly type?: string; readonly media_type?: string; readonly url?: string };
  readonly [key: string]: unknown;
}

/** A block whose kind this schema does not model, kept whole. */
export function unsupportedBlock(block: VendorBlock): conversationv1.UnsupportedBlock {
  return create(conversationv1.UnsupportedBlockSchema, {
    kind: block.type ?? "unknown",
    raw: rawStruct(block),
  });
}

/**
 * An image, by REFERENCE.
 *
 * The vendor inlines base64 bytes; a conversation record is replayed many times
 * and inlining megabytes into something replayed is how a feed becomes slow. A
 * `url` source is carried as a url; a base64 source has no host-local path to
 * name — the shim spills nothing to disk — so it is carried as the data URL the
 * bytes already are, with the vendor's own media type stated.
 */
export function imageBlock(block: VendorBlock): conversationv1.ImageBlock | undefined {
  const source = block.source;
  if (source === undefined) return undefined;
  const mediaType = source.media_type ?? "";
  if (source.type === "url" && typeof source.url === "string") {
    return create(conversationv1.ImageBlockSchema, {
      location: {
        case: "url",
        value: create(conversationv1.ImageBlockUrlSchema, { url: source.url }),
      },
      mediaType,
    });
  }
  const data = (source as { data?: unknown }).data;
  if (source.type === "base64" && typeof data === "string" && mediaType !== "") {
    return create(conversationv1.ImageBlockSchema, {
      location: {
        case: "url",
        value: create(conversationv1.ImageBlockUrlSchema, {
          url: `data:${mediaType};base64,${data}`,
        }),
      },
      mediaType,
    });
  }
  LOGGER.log(
    { level: "warn", source_type: source.type },
    "an image block names no location this contract can carry",
  );
  return undefined;
}

/** One block of a tool's output. */
export function toolResultBlock(block: VendorBlock): conversationv1.ToolResultContentBlock {
  if (block.type === "text" && typeof block.text === "string") {
    return create(conversationv1.ToolResultContentBlockSchema, {
      block: { case: "text", value: create(conversationv1.TextBlockSchema, { text: block.text }) },
    });
  }
  if (block.type === "image") {
    const image = imageBlock(block);
    if (image !== undefined) {
      return create(conversationv1.ToolResultContentBlockSchema, {
        block: { case: "image", value: image },
      });
    }
  }
  LOGGER.log(
    { level: "warn", block_type: block.type },
    "a tool result block's kind is not modelled; it is kept whole as unsupported",
  );
  return create(conversationv1.ToolResultContentBlockSchema, {
    block: { case: "unsupported", value: unsupportedBlock(block) },
  });
}

/**
 * What a tool returned, by block.
 *
 * The vendor states a tool result's content as EITHER a plain string OR a block
 * array; a string is one text block, which is what it means.
 */
export function toolResultContent(content: unknown): conversationv1.ToolResultContent | undefined {
  if (content === undefined || content === null) return undefined;
  if (typeof content === "string") {
    return create(conversationv1.ToolResultContentSchema, {
      blocks: [
        create(conversationv1.ToolResultContentBlockSchema, {
          block: { case: "text", value: create(conversationv1.TextBlockSchema, { text: content }) },
        }),
      ],
    });
  }
  if (!Array.isArray(content)) {
    LOGGER.log(
      { level: "warn", content_type: typeof content },
      "a tool result's content is neither a string nor a block array",
    );
    return undefined;
  }
  return create(conversationv1.ToolResultContentSchema, {
    blocks: content.map((block) => toolResultBlock(block as VendorBlock)),
  });
}

/** One block of what a PERSON said. */
export function userContentBlock(block: VendorBlock): conversationv1.UserContentBlock {
  if (block.type === "text" && typeof block.text === "string") {
    return create(conversationv1.UserContentBlockSchema, {
      block: { case: "text", value: create(conversationv1.TextBlockSchema, { text: block.text }) },
    });
  }
  if (block.type === "image") {
    const image = imageBlock(block);
    if (image !== undefined) {
      return create(conversationv1.UserContentBlockSchema, {
        block: { case: "image", value: image },
      });
    }
  }
  LOGGER.log(
    { level: "warn", block_type: block.type },
    "a user content block's kind is not modelled; it is kept whole as unsupported",
  );
  return create(conversationv1.UserContentBlockSchema, {
    block: { case: "unsupported", value: unsupportedBlock(block) },
  });
}

/** What a person said, as an ordered sequence of blocks. */
export function userSaid(content: unknown): conversationv1.UserSaid {
  const blocks =
    typeof content === "string"
      ? [{ type: "text", text: content } satisfies VendorBlock]
      : Array.isArray(content)
        ? (content as VendorBlock[])
        : [];
  return create(conversationv1.UserSaidSchema, {
    content: create(conversationv1.UserContentSchema, {
      blocks: blocks.map(userContentBlock),
    }),
  });
}
