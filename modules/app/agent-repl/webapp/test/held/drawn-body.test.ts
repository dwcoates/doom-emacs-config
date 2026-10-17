import { describe, expect, it } from "vitest";
import type { UserSaid } from "../../../proto/gen/ts/conversation/v1/user_pb";
import { userSaid } from "../integration/fixtures";
import { drawnBodyOf, servedSrcOf } from "./drawn-body";
import { plainProse } from "../prose";

const saidOf = (blocks: object[]): UserSaid => ({ content: { blocks: blocks.map((block) => ({ block })) } }) as unknown as UserSaid;

describe("drawnBodyOf", () => {
  it.each([
    { name: "no said draws no blocks", said: undefined, want: [] },
    { name: "a text block draws its words", said: userSaid("fix it"), want: [{ case: "text", value: { prose: plainProse("fix it") } }] },
    {
      name: "an image by path draws the served source and the file's name",
      said: saidOf([{ case: "image", value: { location: { case: "path", value: { path: "/w/.agent-repl/clip-ab.png" } } } }]),
      want: [{ case: "image", value: { src: "/feed-images/clip-ab.png", alt: "clip-ab.png" } }],
    },
    {
      name: "an image by url draws the url with no alt",
      said: saidOf([{ case: "image", value: { location: { case: "url", value: { url: "https://x/y.png" } } } }]),
      want: [{ case: "image", value: { src: "https://x/y.png", alt: "" } }],
    },
    {
      name: "a quote draws its heading, its text and its closing",
      said: saidOf([{ case: "quote", value: { text: "earlier" } }]),
      want: [{ case: "quote", value: { heading: "⟢ Replying to an earlier response", quoted: "earlier", closing: "⟢ My message" } }],
    },
  ])("$name", ({ said, want }) => {
    // Act
    const body = drawnBodyOf(said);

    // Assert
    expect(body.blocks.map(({ block }) => ({ case: block.case, value: { ...block.value } }))).toEqual(
      want.map((block) => ({ case: block.case, value: expect.objectContaining(block.value) as Record<string, unknown> })),
    );
  });

  it("the integration held fixtures draw their body through drawnBodyOf", async () => {
    // Arrange
    const { heldPromptItem, heldIngressItem } = await import("../integration/fixtures");

    // Act
    const prompt = heldPromptItem({ text: "p" }).item;
    const ingress = heldIngressItem({ text: "i" }).item;

    // Assert
    expect(prompt?.value?.body).toEqual(drawnBodyOf(prompt?.value?.said as UserSaid));
    expect(ingress?.value?.body).toEqual(drawnBodyOf(ingress?.value?.said as UserSaid));
  });
});

describe("servedSrcOf", () => {
  it("serves a path under its file name", () => {
    expect(servedSrcOf("/a/b/clip-1.png")).toBe("/feed-images/clip-1.png");
  });
});
