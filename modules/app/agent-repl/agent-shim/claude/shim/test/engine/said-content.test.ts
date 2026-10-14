import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../src/proto.js";
import { imagePathNote, saidContent, saidContentShape, type ReadImage } from "../../src/engine/said-content.js";

type Block = conversationv1.UserContentBlock;

const text = (value: string): Block =>
  create(conversationv1.UserContentBlockSchema, {
    block: { case: "text", value: create(conversationv1.TextBlockSchema, { text: value }) },
  });

const quote = (value: string): Block =>
  create(conversationv1.UserContentBlockSchema, {
    block: { case: "quote", value: create(conversationv1.UserQuoteBlockSchema, { text: value }) },
  });

const image = (location: conversationv1.ImageBlock["location"], mediaType = "image/png"): Block =>
  create(conversationv1.UserContentBlockSchema, {
    block: { case: "image", value: create(conversationv1.ImageBlockSchema, { mediaType, location }) },
  });

const pathImage = (path: string, mediaType = "image/png"): Block =>
  image({ case: "path", value: create(conversationv1.ImageBlockPathSchema, { path }) }, mediaType);

const said = (...blocks: Block[]): conversationv1.UserSaid =>
  create(conversationv1.UserSaidSchema, { content: create(conversationv1.UserContentSchema, { blocks }) });

/** A filesystem holding exactly these files. */
const files =
  (contents: Record<string, string>): ReadImage =>
  (path) => {
    const content = contents[path];
    if (content === undefined) throw new Error(`ENOENT: no such file or directory, open '${path}'`);
    return Buffer.from(content);
  };

const b64 = (value: string): string => Buffer.from(value).toString("base64");

describe("what the vendor is sent for what the user said", () => {
  it.each([
    { name: "words alone are the words", blocks: [text("hello")], want: "hello" },
    {
      name: "a reply's quote is delivered verbatim ahead of the words, joined by a newline",
      blocks: [quote("⟢ Replying:\n```\nParis.\n```\n⟢ My message:\n"), text("And its population?")],
      want: "⟢ Replying:\n```\nParis.\n```\n⟢ My message:\n\nAnd its population?",
    },
    { name: "a said with no content is the empty string", blocks: [], want: "" },
  ])("$name", ({ blocks, want }) => {
    // Arrange
    const input = said(...blocks);

    // Act
    const content = saidContent(input, files({}));

    // Assert
    expect(content).toBe(want);
  });

  it("is the empty string for a prompt carrying no content message", () => {
    expect(saidContent(create(conversationv1.UserSaidSchema, {}), files({}))).toBe("");
  });

  it.each([
    {
      name: "an image by path is sent as its bytes, in base64",
      blocks: [pathImage("/ws/a.png")],
      want: [
        { type: "image", source: { type: "base64", media_type: "image/png", data: b64("PNG-A") } },
        { type: "text", text: imagePathNote("/ws/a.png") },
      ],
    },
    {
      name: "images keep their place between the words",
      blocks: [text("before"), pathImage("/ws/a.png"), text("between"), pathImage("/ws/b.png"), text("after")],
      want: [
        { type: "text", text: "before" },
        { type: "image", source: { type: "base64", media_type: "image/png", data: b64("PNG-A") } },
        { type: "text", text: imagePathNote("/ws/a.png") },
        { type: "text", text: "between" },
        { type: "image", source: { type: "base64", media_type: "image/png", data: b64("PNG-B") } },
        { type: "text", text: imagePathNote("/ws/b.png") },
        { type: "text", text: "after" },
      ],
    },
    {
      name: "adjacent words between two images are one text block, joined by a newline",
      blocks: [quote("the quote"), text("my words"), pathImage("/ws/a.png")],
      want: [
        { type: "text", text: "the quote\nmy words" },
        { type: "image", source: { type: "base64", media_type: "image/png", data: b64("PNG-A") } },
        { type: "text", text: imagePathNote("/ws/a.png") },
      ],
    },
    {
      name: "a run of only whitespace between two images sends no text block",
      blocks: [pathImage("/ws/a.png"), text(" \n "), pathImage("/ws/b.png")],
      want: [
        { type: "image", source: { type: "base64", media_type: "image/png", data: b64("PNG-A") } },
        { type: "text", text: imagePathNote("/ws/a.png") },
        { type: "image", source: { type: "base64", media_type: "image/png", data: b64("PNG-B") } },
        { type: "text", text: imagePathNote("/ws/b.png") },
      ],
    },
    {
      name: "an image by url is sent as that url",
      blocks: [image({ case: "url", value: create(conversationv1.ImageBlockUrlSchema, { url: "https://example.invalid/a.png" }) })],
      want: [{ type: "image", source: { type: "url", url: "https://example.invalid/a.png" } }],
    },
    {
      name: "a jpeg keeps its own media type",
      blocks: [pathImage("/ws/c.jpg", "image/jpeg")],
      want: [
        { type: "image", source: { type: "base64", media_type: "image/jpeg", data: b64("JPEG-C") } },
        { type: "text", text: imagePathNote("/ws/c.jpg") },
      ],
    },
  ])("$name", ({ blocks, want }) => {
    // Arrange
    const input = said(...blocks);
    const read = files({ "/ws/a.png": "PNG-A", "/ws/b.png": "PNG-B", "/ws/c.jpg": "JPEG-C" });

    // Act
    const content = saidContent(input, read);

    // Assert
    expect(content).toEqual(want);
  });

  it("names the file an image came from, so the agent can act on it", () => {
    expect(imagePathNote("/ws/a.png")).toBe("[the image above is the file /ws/a.png]");
  });

  it.each([
    {
      name: "an image file that cannot be read, naming the path and the cause",
      blocks: [text("look"), pathImage("/ws/gone.png")],
      error: /the prompt's image \/ws\/gone\.png could not be read: ENOENT/,
    },
    {
      name: "an empty image file",
      blocks: [pathImage("/ws/empty.png")],
      error: /the prompt's image \/ws\/empty\.png is an empty file/,
    },
    {
      name: "an image path that is not absolute",
      blocks: [pathImage("a.png")],
      error: /names the path "a\.png", which is not absolute/,
    },
    {
      name: "an image the vendor does not accept, naming its media type",
      blocks: [pathImage("/ws/a.tiff", "image/tiff")],
      error: /\/ws\/a\.tiff is "image\/tiff", which the vendor does not accept/,
    },
    {
      name: "an image naming no location",
      blocks: [image({ case: undefined })],
      error: /a prompt's image names no location/,
    },
    {
      name: "an UnsupportedBlock, rather than silently dropping what the user sent",
      blocks: [
        create(conversationv1.UserContentBlockSchema, {
          block: { case: "unsupported", value: create(conversationv1.UnsupportedBlockSchema, { kind: "video" }) },
        }),
      ],
      error: /UnsupportedBlock is not a fallback/,
    },
    {
      name: "a content block carrying no arm, rather than delivering a prompt short one block",
      blocks: [create(conversationv1.UserContentBlockSchema, {})],
      error: /carries no arm/,
    },
  ])("RAISES on $name", ({ blocks, error }) => {
    // Arrange
    const input = said(...blocks);
    const read = files({ "/ws/empty.png": "", "/ws/a.tiff": "TIFF" });

    // Act
    const act = (): unknown => saidContent(input, read);

    // Assert
    expect(act).toThrow(error);
  });
});

describe("the send's log shape", () => {
  it.each([
    { name: "words alone count their characters", content: "hello", want: { characters: 5, images: 0 } },
    {
      name: "a block list counts its words and its images",
      content: [
        { type: "text" as const, text: "abc" },
        { type: "image" as const, source: { type: "url" as const, url: "https://example.invalid/a.png" } },
        { type: "text" as const, text: "de" },
      ],
      want: { characters: 5, images: 1 },
    },
  ])("$name", ({ content, want }) => {
    expect(saidContentShape(content)).toEqual(want);
  });
});
