/**
 * engine/said-content.ts — what the vendor is sent for what the user said.
 *
 * A `conversation.v1.UserSaid` is an ORDERED list of blocks, and a person may
 * put an image between two sentences. The vendor's user message takes the same
 * shape (a list of content blocks), so the order the person composed is the
 * order the model reads, and an image arrives AS AN IMAGE: its bytes in a
 * `base64` source, read from the host path the composer wrote it to. A path
 * spelled into the words would have left the model reading a filename and
 * choosing whether to open it.
 *
 * TEXT ALONE STAYS A STRING. A prompt with no image is sent exactly as it
 * always was: its text and quote blocks joined by newlines. Only a prompt that
 * carries an image becomes a block list, where each run of words between two
 * images is one text block, joined the same way.
 *
 * THE BYTES ARE READ HERE AND NOWHERE ELSE. The record keeps the reference
 * (`ImageBlockPath`), which is what is replayed; only the vendor needs the
 * bytes, so only the send reads them.
 */

import { readFileSync } from "node:fs";
import { isAbsolute } from "node:path";
import type { conversationv1 } from "../proto.js";
import type { SdkUserMessage } from "../sdk/types.js";

/** A user message's content, in the vendor's own type. */
export type SaidContent = SdkUserMessage["message"]["content"];

/** One block of a user message's content, in the vendor's own type. */
type SaidContentBlock = Exclude<SaidContent, string>[number];

/**
 * The image media types the vendor's API accepts. An image outside them is
 * refused at the send rather than sent for the API to refuse: the refusal then
 * names the file and its type, where the API's would name neither.
 */
const VENDOR_IMAGE_MEDIA_TYPES = ["image/jpeg", "image/png", "image/gif", "image/webp"] as const;
type VendorImageMediaType = (typeof VENDOR_IMAGE_MEDIA_TYPES)[number];

function isVendorImageMediaType(mediaType: string): mediaType is VendorImageMediaType {
  return (VENDOR_IMAGE_MEDIA_TYPES as readonly string[]).includes(mediaType);
}

/** Reads an image file's bytes; the default reads the host filesystem. */
export type ReadImage = (path: string) => Buffer;

/**
 * What the vendor is sent for `said`.
 *
 * TEXT blocks are their text, and a QUOTE block (a reply's quote of an earlier
 * bubble, composed by the daemon) is its text, delivered as composed. An IMAGE
 * block becomes an image content block in its place. An `UnsupportedBlock`
 * RAISES: the contract says it is "not a fallback", so one arriving in a
 * prompt is a producer defect, and delivering the prompt without it would
 * silently drop what the user sent.
 */
export function saidContent(said: conversationv1.UserSaid, readImage: ReadImage = readFileSync): SaidContent {
  const blocks: SaidContentBlock[] = [];
  let run: string[] = [];
  let images = 0;
  const flushRun = (): void => {
    const text = run.join("\n");
    run = [];
    // A run of only whitespace (the space between two images) carries no
    // words, and the vendor's API refuses a text block with none.
    if (text.trim() !== "") blocks.push({ type: "text", text });
  };
  for (const block of said.content?.blocks ?? []) {
    switch (block.block.case) {
      case "text":
        run.push(block.block.value.text);
        break;
      case "quote":
        run.push(block.block.value.text);
        break;
      case "image":
        flushRun();
        blocks.push(vendorImage(block.block.value, readImage));
        images += 1;
        break;
      case "unsupported":
        throw new Error(
          `shim turn: a prompt carries an UnsupportedBlock (${block.block.value.kind}); ` +
            "UnsupportedBlock is not a fallback and the shim will not deliver a prompt with one silently dropped",
        );
      default:
        throw new Error("shim turn: a prompt content block carries no arm");
    }
  }
  if (images === 0) return run.join("\n");
  flushRun();
  return blocks;
}

/** The vendor image block for one of the user's images. */
function vendorImage(image: conversationv1.ImageBlock, readImage: ReadImage): SaidContentBlock {
  const location = image.location;
  switch (location.case) {
    case "path": {
      const path = location.value.path;
      if (!isAbsolute(path)) {
        throw new Error(`shim turn: a prompt's image names the path ${JSON.stringify(path)}, which is not absolute`);
      }
      if (!isVendorImageMediaType(image.mediaType)) {
        throw new Error(
          `shim turn: the prompt's image ${path} is ${JSON.stringify(image.mediaType)}, ` +
            `which the vendor does not accept (it takes ${VENDOR_IMAGE_MEDIA_TYPES.join(", ")})`,
        );
      }
      let bytes: Buffer;
      try {
        bytes = readImage(path);
      } catch (err) {
        throw new Error(
          `shim turn: the prompt's image ${path} could not be read: ${err instanceof Error ? err.message : String(err)}`,
          { cause: err },
        );
      }
      if (bytes.length === 0) throw new Error(`shim turn: the prompt's image ${path} is an empty file`);
      return { type: "image", source: { type: "base64", media_type: image.mediaType, data: bytes.toString("base64") } };
    }
    case "url":
      return { type: "image", source: { type: "url", url: location.value.url } };
    default:
      throw new Error("shim turn: a prompt's image names no location");
  }
}

/** The words and images a content carries, for the send's log line. */
export function saidContentShape(content: SaidContent): { characters: number; images: number } {
  if (typeof content === "string") return { characters: content.length, images: 0 };
  let characters = 0;
  let images = 0;
  for (const block of content) {
    if (block.type === "text") characters += block.text.length;
    if (block.type === "image") images += 1;
  }
  return { characters, images };
}
