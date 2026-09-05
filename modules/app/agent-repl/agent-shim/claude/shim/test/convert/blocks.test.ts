/**
 * THE VENDOR'S CONTENT BLOCKS, in this contract's terms.
 *
 * The contract models the two facts under the vendor's seventeen block kinds, so
 * what is pinned here is which fact each kind maps to — and, load-bearing, that
 * `UnsupportedBlock` is reached ONLY for a kind nothing here can know. A
 * recognizable kind in that arm is a producer defect, so the suite forbids it.
 */
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../src/proto.js";
import {
  imageBlock,
  toolResultBlock,
  toolResultContent,
  unsupportedBlock,
  userContentBlock,
  userSaid,
} from "../../src/convert/blocks.js";
import { corpusLine } from "./fold-harness.js";

describe("text", () => {
  it("is carried verbatim as a tool result block", () => {
    const block = toolResultBlock({ type: "text", text: "ok" });

    expect(block.block.case).toBe("text");
    expect((block.block.value as conversationv1.TextBlock).text).toBe("ok");
  });

  it("is carried verbatim as something a person said", () => {
    const block = userContentBlock({ type: "text", text: "hello" });

    expect(block.block.case).toBe("text");
  });
});

describe("images", () => {
  it("carries a url source as a url", () => {
    const image = imageBlock({
      type: "image",
      source: { type: "url", url: "https://x/y.png", media_type: "image/png" },
    });

    expect(image?.location.case).toBe("url");
    expect(image?.mediaType).toBe("image/png");
  });

  it("carries base64 bytes BY REFERENCE, as the data URL they already are", () => {
    // A conversation record is replayed many times; inlining megabytes into
    // something replayed is how a feed becomes slow, and the shim spills nothing
    // to disk, so there is no host-local path to name.
    const image = imageBlock({
      type: "image",
      source: { type: "base64", data: "AAAA", media_type: "image/png" } as never,
    });

    const url = image?.location.value as conversationv1.ImageBlockUrl;
    expect(url.url).toBe("data:image/png;base64,AAAA");
  });

  it("answers nothing for a source naming no location this contract can carry", () => {
    expect(imageBlock({ type: "image", source: { type: "file" } })).toBeUndefined();
  });

  it("reads the real corpus image block", () => {
    // The vendor nests a tool's image inside the tool_result block's own
    // content, which is exactly the accident this contract erases.
    const line = corpusLine("content-blocks/image.jsonl");
    const outer = (line.message as { content?: unknown }).content;
    const result = (Array.isArray(outer) ? outer : []).find(
      (block) => (block as { type?: string }).type === "tool_result",
    ) as { content?: unknown } | undefined;
    const inner: unknown[] = Array.isArray(result?.content) ? (result.content as unknown[]) : [];
    const image = inner.find((block) => (block as { type?: string }).type === "image");

    expect(imageBlock(image as never)?.mediaType).toBe("image/png");
  });
});

describe("the unsupported arm", () => {
  it("keeps the vendor's own kind name, so a later schema knows what to model", () => {
    expect(unsupportedBlock({ type: "fallback" }).kind).toBe("fallback");
  });

  it("keeps the block whole, so not understanding it loses nothing", () => {
    const kept = unsupportedBlock({ type: "fallback", extra: 1 });

    expect(kept.raw).toEqual({ type: "fallback", extra: 1 });
  });

  it("is NOT reached for text, which is modelled", () => {
    expect(toolResultBlock({ type: "text", text: "" }).block.case).not.toBe("unsupported");
  });

  it("is NOT reached for an image, which is modelled", () => {
    const block = toolResultBlock({
      type: "image",
      source: { type: "url", url: "u", media_type: "image/png" },
    });

    expect(block.block.case).not.toBe("unsupported");
  });

  it("catches the corpus's genuinely unknown block kind", () => {
    const line = corpusLine("content-blocks/fallback.jsonl");
    const content = (line.message as { content?: unknown }).content;
    const blocks = Array.isArray(content) ? content : [];
    const unknown = blocks.find(
      (block) => (block as { type?: string }).type !== "text",
    ) as Record<string, unknown> | undefined;

    expect(toolResultBlock((unknown ?? { type: "fallback" }) as never).block.case).toBe(
      "unsupported",
    );
  });
});

describe("toolResultContent", () => {
  it("reads a plain string as the one text block it means", () => {
    const content = toolResultContent("done");

    expect(content?.blocks).toHaveLength(1);
  });

  it("reads a block array in the tool's own order", () => {
    const content = toolResultContent([
      { type: "text", text: "a" },
      { type: "text", text: "b" },
    ]);

    expect(
      content?.blocks.map((block) => (block.block.value as conversationv1.TextBlock).text),
    ).toEqual(["a", "b"]);
  });

  it("answers UNSET for a call that returned nothing at all", () => {
    expect(toolResultContent(undefined)).toBeUndefined();
  });

  it("answers UNSET for content that is neither a string nor a block array", () => {
    expect(toolResultContent(7)).toBeUndefined();
  });
});

describe("userSaid", () => {
  it("carries the blocks in the order the person composed them", () => {
    const said = userSaid([
      { type: "text", text: "one" },
      { type: "text", text: "two" },
    ]);

    expect(said.content?.blocks).toHaveLength(2);
  });

  it("reads a bare string as one text block", () => {
    expect(userSaid("hi").content?.blocks).toHaveLength(1);
  });

  it("answers an empty content for something that is neither", () => {
    expect(userSaid(undefined).content?.blocks).toHaveLength(0);
  });
});

describe("what a PERSON said, block by block", () => {
  it("carries an image a person attached as an image", () => {
    const block = userContentBlock({
      type: "image",
      source: { type: "url", url: "https://x/y.png", media_type: "image/png" },
    });

    expect(block.block.case).toBe("image");
  });

  it("keeps an image whose source names no location this contract can carry as unsupported", () => {
    // Losing the block entirely would lose what the person actually attached.
    const block = userContentBlock({ type: "image", source: { type: "file" } });

    expect(block.block.case).toBe("unsupported");
  });

  it("keeps a kind nothing here models whole, rather than dropping it", () => {
    const block = userContentBlock({ type: "document", title: "spec.pdf" });

    expect(block.block.case).toBe("unsupported");
  });

  it("keeps a text block with no text whole, since the contract models only real text", () => {
    const block = userContentBlock({ type: "text" });

    expect(block.block.case).toBe("unsupported");
  });
});

describe("an image block with no source at all", () => {
  it("answers nothing, because there is no location to name", () => {
    expect(imageBlock({ type: "image" })).toBeUndefined();
  });
});

describe("an unsupported block the vendor did not even name", () => {
  it("is kept under `unknown` rather than under the empty string", () => {
    expect(unsupportedBlock({}).kind).toBe("unknown");
  });
});
