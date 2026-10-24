// @vitest-environment jsdom
// A prompt waiting in the held-prompt ingress, drawn as a parked card.
// See src/held/ingress-prompt.ts.
import { describe, expect, it } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { UserSaidSchema } from "../../../proto/gen/ts/conversation/v1/user_pb";
import { BUBBLE_QUOTE_CLASS } from "../../src/bubble/quote.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  HeldIngressPromptBadgeSchema,
  HeldIngressPromptSchema,
  HeldPromptSchema,
  type HeldIngressPrompt,
} from "../../../proto/gen/ts/frontend/v1/feed_held_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { Ticker } from "../../src/clock.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { HELD_STATUS_ATTRIBUTE, drawHeldPrompt } from "../../src/held/held-prompt.js";
import {
  DAEMON_UNREACHABLE_LABEL,
  INGRESS_SOURCE_ATTRIBUTE,
  INGRESS_STATUS_BADGES,
  drawEditorIngressEntry,
  drawHeldIngressPrompt,
} from "../../src/held/ingress-prompt.js";
import { oneofArms } from "../arms.js";
import { editorIngressEntry } from "../feed/harness.js";
import { testAppContext } from "../rpc/app-context.js";
import { testHeldContext } from "./held-context.js";
import { plainProse } from "../prose.js";

const NOW = 1_700_000_060_000;
const ticker: Ticker = { now: () => NOW, subscribe: () => () => undefined };

const heldContext = () =>
  testHeldContext(
    testAppContext({
      client: createAgentReplClient(createRouterTransport(({ service }) => service(AgentRepl, {}))),
      workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
      ticker,
      failures: { report: () => undefined, retract: () => undefined },
      composerEnabled: false,
    }),
  );

/** A daemon ingress item waiting on ARM, its badge LABEL and DETAIL. */
function ingress(arm: string, label = "waiting", detail?: string): HeldIngressPrompt {
  return create(HeldIngressPromptSchema, {
    said: { content: { blocks: [{ block: { case: "text", value: { text: "what's dmitry's comment" } } }] } },
    body: { blocks: [{ block: { case: "text", value: { prose: plainProse("what's dmitry's comment") } } }] },
    queuedAt: { atMs: BigInt(NOW - 60_000) },
    badge: create(HeldIngressPromptBadgeSchema, {
      label,
      ...(detail === undefined ? {} : { detail }),
      standsFor: { case: arm as "noSession", value: {} },
    }),
  });
}

describe("INGRESS_STATUS_BADGES", () => {
  it("colors every fact the schema's badge can stand for, and the page's own", () => {
    // Arrange / Act
    const arms = [...oneofArms(HeldIngressPromptBadgeSchema, "stands_for"), "daemonUnreachable"].sort();
    // Assert
    expect(Object.keys(INGRESS_STATUS_BADGES).sort()).toEqual(arms);
  });
});

describe("drawHeldIngressPrompt", () => {
  it.each([...oneofArms(HeldIngressPromptBadgeSchema, "stands_for")])("draws the %s badge in its color", (arm) => {
    // Act
    const card = drawHeldIngressPrompt(ingress(arm, "the words"), heldContext());
    // Assert
    const pill = card.querySelector(`[${HELD_STATUS_ATTRIBUTE}]`);
    expect([pill?.textContent, pill?.getAttribute(HELD_STATUS_ATTRIBUTE), pill?.classList.contains(INGRESS_STATUS_BADGES[arm as keyof typeof INGRESS_STATUS_BADGES])]).toEqual([
      "the words",
      arm,
      true,
    ]);
  });

  it("draws what was said and its queued age", () => {
    // Act
    const card = drawHeldIngressPrompt(ingress("noSession"), heldContext());
    // Assert
    expect([card.textContent?.includes("what's dmitry's comment"), card.querySelector("[data-queued]")?.textContent]).toEqual([
      true,
      "queued 1m ago",
    ]);
  });

  it("titles the badge with its detail", () => {
    // Act
    const card = drawHeldIngressPrompt(ingress("refused", "not yet queued", "keeps trying"), heldContext());
    // Assert
    expect((card.querySelector(`[${HELD_STATUS_ATTRIBUTE}]`) as HTMLElement).title).toBe("keeps trying");
  });

  it("marks the card as the daemon's and draws no controls", () => {
    // Act
    const card = drawHeldIngressPrompt(ingress("noSession"), heldContext());
    // Assert
    expect([card.getAttribute(INGRESS_SOURCE_ATTRIBUTE), card.querySelector("button")]).toEqual(["daemon", null]);
  });

  it.each([
    ["no badge", create(HeldIngressPromptSchema, { said: {}, body: {}, queuedAt: { atMs: 0n } })],
    ["no drawn body", create(HeldIngressPromptSchema, { said: {}, queuedAt: { atMs: 0n }, badge: { label: "x", standsFor: { case: "merge", value: {} } } })],
    ["no queued instant", create(HeldIngressPromptSchema, { said: {}, body: {}, badge: { label: "x", standsFor: { case: "merge", value: {} } } })],
    ["a badge standing for nothing", create(HeldIngressPromptSchema, { said: {}, body: {}, queuedAt: { atMs: 0n }, badge: { label: "x" } })],
    ["an empty label", ingress("merge", "")],
  ])("refuses an item with %s", (_name, item) => {
    // Act / Assert
    expect(() => drawHeldIngressPrompt(item, heldContext())).toThrow(MalformedView);
  });
});

describe("the daemon's ingress card draws the daemon's drawing", () => {
  it("draws its body rather than its said", () => {
    // Arrange — the body is an image, the said only words.
    const card = create(HeldIngressPromptSchema, {
      said: { content: { blocks: [{ block: { case: "text", value: { text: "the words" } } }] } },
      body: { blocks: [{ block: { case: "image", value: { src: "https://x/a.png", alt: "" } } }] },
      queuedAt: { atMs: 0n },
      badge: { label: "waiting", standsFor: { case: "noSession", value: {} } },
    });
    // Act
    const el = drawHeldIngressPrompt(card, heldContext());
    // Assert
    expect([el.querySelector<HTMLImageElement>("img.prompt-block-image")?.src, el.querySelector(".prompt-block-text")]).toEqual([
      "https://x/a.png",
      null,
    ]);
  });
});

describe("drawEditorIngressEntry", () => {
  it("draws the editor's entry with the page's own badge", () => {
    // Act
    const card = drawEditorIngressEntry(editorIngressEntry("k-1", "sent offline"), heldContext());
    // Assert
    const pill = card.querySelector(`[${HELD_STATUS_ATTRIBUTE}]`);
    expect([
      card.getAttribute(INGRESS_SOURCE_ATTRIBUTE),
      pill?.textContent,
      pill?.getAttribute(HELD_STATUS_ATTRIBUTE),
      card.textContent?.includes("sent offline"),
    ]).toEqual(["editor", DAEMON_UNREACHABLE_LABEL, "daemonUnreachable", true]);
  });
});

describe("an ingress entry's words, drawn unparsed", () => {
  /** An editor entry saying BLOCKS. */
  const saying = (blocks: MessageInitShape<typeof UserSaidSchema>["content"]): HTMLElement =>
    drawEditorIngressEntry({ key: "k", queuedAtMs: 0, said: create(UserSaidSchema, { content: blocks }) }, heldContext());

  it("draws a text block as a markdown slot", () => {
    // Act
    const card = saying({ blocks: [{ block: { case: "text", value: { text: "**bold**" } } }] });
    // Assert
    expect(card.querySelector(".queued-text")).not.toBeNull();
  });

  it("draws a tree typed in it as the markdown it was typed as, never as a tree", () => {
    // Arrange — no daemon has parsed it.
    const tree = "1 🔧 Fixed it\n└── 1.1 x";
    // Act
    const card = saying({ blocks: [{ block: { case: "text", value: { text: tree } } }] });
    // Assert
    expect(card.querySelector(".mp-tree")).toBeNull();
  });

  it("draws a reply's quote as an expand-only slot", () => {
    // Act
    const card = saying({ blocks: [{ block: { case: "quote", value: { text: "the quote" } } }] });
    // Assert
    expect(card.querySelector(`.queued-quote.${BUBBLE_QUOTE_CLASS}`)).not.toBeNull();
  });

  it("draws a url image as the picture", () => {
    // Act
    const card = saying({
      blocks: [{ block: { case: "image", value: { mediaType: "image/png", location: { case: "url", value: { url: "https://x/a.png" } } } } }],
    });
    // Assert
    expect(card.querySelector<HTMLImageElement>("img.queued-image")?.src).toBe("https://x/a.png");
  });

  it("names a host-path image rather than drawing a broken picture", () => {
    // Act
    const card = saying({
      blocks: [{ block: { case: "image", value: { mediaType: "image/png", location: { case: "path", value: { path: "/h/a.png" } } } } }],
    });
    // Assert
    expect(card.querySelector(".queued-image-path")?.textContent).toBe("/h/a.png");
  });

  it("draws nothing for an unmodeled block, recording its kind through the canonical logger", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    const card = saying({ blocks: [{ block: { case: "unsupported", value: { kind: "video" } } }] });
    // Assert
    const record = await forwardedRecord(capture, "held.held-prompt.unsupported-block");
    expect([card.querySelector(".queued-text, img, .queued-image-path"), record.level.case, record.context]).toEqual([
      null,
      "warn",
      expect.objectContaining({ kind: "video" }),
    ]);
  });

  it("refuses a block whose oneof is unset", () => {
    // Act + Assert
    expect(() => saying({ blocks: [{}] })).toThrow(MalformedView);
  });

  it("refuses an image whose location is unset", () => {
    // Act + Assert
    expect(() => saying({ blocks: [{ block: { case: "image", value: { mediaType: "image/png" } } }] })).toThrow(MalformedView);
  });
});

describe("the queued age every held card shares", () => {
  it("draws a held prompt's age and an ingress card's as the one element", () => {
    // Arrange
    const held = drawHeldPrompt(
      create(HeldPromptSchema, {
        turn: { value: "t1" },
        said: { content: { blocks: [{ block: { case: "text", value: { text: "x" } } }] } },
        body: { blocks: [{ block: { case: "text", value: { prose: plainProse("x") } } }] },
        queuedAt: { atMs: BigInt(NOW - 60_000) },
        classification: { case: "classifying", value: {} },
        badge: { label: "classifying", standsFor: { case: "classifying", value: {} } },
      }),
      heldContext(),
    );
    // Act
    const card = drawHeldIngressPrompt(ingress("noSession"), heldContext());
    // Assert
    const shape = (el: Element | null) => [el?.className, el?.hasAttribute("data-queued"), el?.textContent];
    expect(shape(card.querySelector("[data-queued]"))).toEqual(shape(held.querySelector("[data-queued]")));
  });
});

describe("the badge every held card shares", () => {
  it("draws a held prompt's badge and an ingress card's as the one element", () => {
    // Arrange
    const held = drawHeldPrompt(
      create(HeldPromptSchema, {
        turn: { value: "t1" },
        said: { content: { blocks: [{ block: { case: "text", value: { text: "x" } } }] } },
        body: { blocks: [{ block: { case: "text", value: { prose: plainProse("x") } } }] },
        queuedAt: { atMs: 0n },
        classification: { case: "classifying", value: {} },
        badge: { label: "classifying", standsFor: { case: "classifying", value: {} } },
      }),
      heldContext(),
    );
    // Act
    const card = drawHeldIngressPrompt(ingress("noSession", "waiting for the session"), heldContext());
    // Assert
    const shape = (el: Element | null) => [el?.tagName, [...(el?.classList ?? [])].slice(0, 2)];
    expect(shape(card.querySelector(`[${HELD_STATUS_ATTRIBUTE}]`))).toEqual(shape(held.querySelector(`[${HELD_STATUS_ATTRIBUTE}]`)));
  });
});
