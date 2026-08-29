// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  FeedSessionSeparationSchema,
  type FeedSessionSeparation,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  SEPARATION_ARMS,
  drawFeedSessionSeparation,
} from "../../../src/feed/rows/separation.js";
import { harness, rowContext, userPromptRow } from "../harness.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

/** A separation with the given arm. */
type SeparationInit = MessageInitShape<typeof FeedSessionSeparationSchema>;

function separation(
  kind: SeparationInit["kind"],
  opts: { label?: string; tokens?: { beforeText: string; afterText: string } } = {},
): FeedSessionSeparation {
  return create(FeedSessionSeparationSchema, {
    label: { text: opts.label ?? "context compacted" },
    kind,
    tokens: opts.tokens,
  });
}

function ctxFor(previous?: HTMLElement) {
  const { ctx } = harness();
  return rowContext(ctx, userPromptRow("x", "y"), { previous });
}

/** Every arm, with a payload legal for it. */
const ARMS: ReadonlyArray<[string, SeparationInit["kind"]]> = [
  ["cleared", { case: "cleared", value: {} }],
  [
    "compacted",
    {
      case: "compacted",
      value: { summary: { markdown: "we did things" }, fold: { folded: true } },
    },
  ],
  ["worktreeEntered", { case: "worktreeEntered", value: { path: { text: "/w/tree" } } }],
  [
    "worktreeLeft",
    { case: "worktreeLeft", value: { outcome: { case: "removed", value: {} } } },
  ],
];

describe("drawFeedSessionSeparation: ONE renderer for every arm", () => {
  it("words every arm the schema declares", () => {
    expect([...SEPARATION_ARMS].sort()).toEqual(ARMS.map(([name]) => name).sort());
  });

  it.each(ARMS)("gives %s the same rule-and-label structure", (_name, kind) => {
    const el = drawFeedSessionSeparation(separation(kind), ctxFor());
    expect([el.querySelector(".sep-rule") !== null, el.querySelector(".sep-label") !== null]).toEqual(
      [true, true],
    );
  });

  it.each(ARMS)("says which arm %s is, for the suite and the stylesheet", (name, kind) => {
    expect(drawFeedSessionSeparation(separation(kind), ctxFor()).getAttribute("data-arm")).toBe(
      name,
    );
  });

  it("gives the context arms their existing accents", () => {
    const cleared = drawFeedSessionSeparation(separation(ARMS[0][1]), ctxFor());
    expect(cleared.querySelector(".sep-rule")?.className).toContain("sep-accent-cleared");
  });

  it("gives the worktree arms the BLUE accent, as the schema requires", () => {
    const entered = drawFeedSessionSeparation(separation(ARMS[2][1]), ctxFor());
    expect(entered.querySelector(".sep-rule")?.className).toContain("sep-accent-worktree");
  });

  it("refuses an unset arm", () => {
    const msg = create(FeedSessionSeparationSchema, { label: { text: "x" } });
    expect(() => drawFeedSessionSeparation(msg, ctxFor())).toThrow(MalformedView);
  });

  it("refuses a divider with no label", () => {
    const msg = create(FeedSessionSeparationSchema, { kind: { case: "cleared", value: {} } });
    expect(() => drawFeedSessionSeparation(msg, ctxFor())).toThrow(MalformedView);
  });
});

describe("drawFeedSessionSeparation: the size change", () => {
  it("draws both already-formatted sides, doing no arithmetic", () => {
    const el = drawFeedSessionSeparation(
      separation(ARMS[0][1], { tokens: { beforeText: "180k", afterText: "12k" } }),
      ctxFor(),
    );
    expect(el.querySelector(".sep-tokens")?.textContent).toBe(" 180k → 12k");
  });

  it("draws no figure on the worktree arms, which change no context", () => {
    const el = drawFeedSessionSeparation(separation(ARMS[2][1]), ctxFor());
    expect(el.querySelector(".sep-tokens")).toBeNull();
  });
});

describe("drawFeedSessionSeparation: the compaction", () => {
  it("starts folded when the wire says folded", () => {
    const el = drawFeedSessionSeparation(separation(ARMS[1][1]), ctxFor());
    expect(el.querySelector<HTMLElement>(".sep-summary")?.hidden).toBe(true);
  });

  it("starts open when the wire says unfolded", () => {
    const el = drawFeedSessionSeparation(
      separation({
        case: "compacted",
        value: { summary: { markdown: "s" }, fold: { folded: false } },
      }),
      ctxFor(),
    );
    expect(el.querySelector<HTMLElement>(".sep-summary")?.hidden).toBe(false);
  });

  it("opens on the reader's click", () => {
    const el = drawFeedSessionSeparation(separation(ARMS[1][1]), ctxFor());
    el.querySelector<HTMLElement>(".sep-fold-toggle")?.click();
    expect(el.querySelector<HTMLElement>(".sep-summary")?.hidden).toBe(false);
  });

  it("keeps the reader's toggle across a re-push (R2)", () => {
    const first = drawFeedSessionSeparation(separation(ARMS[1][1]), ctxFor());
    first.querySelector<HTMLElement>(".sep-fold-toggle")?.click();
    const second = drawFeedSessionSeparation(separation(ARMS[1][1]), ctxFor(first));
    expect(second.querySelector<HTMLElement>(".sep-summary")?.hidden).toBe(false);
  });

  it("renders the summary as prose, not as raw markdown", () => {
    const el = drawFeedSessionSeparation(
      separation({
        case: "compacted",
        value: { summary: { markdown: "**done**" }, fold: { folded: false } },
      }),
      ctxFor(),
    );
    expect(el.querySelector(".sep-summary strong")?.textContent).toBe("done");
  });

  it("states what a cold read cost, when the compaction paid it", () => {
    const el = drawFeedSessionSeparation(
      separation({
        case: "compacted",
        value: {
          summary: { markdown: "s" },
          fold: { folded: true },
          coldRead: { evidence: { uncachedInputTokens: 180000n } },
        },
      }),
      ctxFor(),
    );
    expect(el.querySelector(".sep-cold-read")?.textContent).toContain("180000");
  });

  it("draws no cold-read notice on an ordinary compaction", () => {
    const el = drawFeedSessionSeparation(separation(ARMS[1][1]), ctxFor());
    expect(el.querySelector(".sep-cold-read")).toBeNull();
  });

  it("refuses a compaction with no fold field", () => {
    const msg = separation({ case: "compacted", value: { summary: { markdown: "s" } } });
    expect(() => drawFeedSessionSeparation(msg, ctxFor())).toThrow(MalformedView);
  });
});

describe("drawFeedSessionSeparation: the worktree arms", () => {
  it("draws the entered path through the ONE shared editor link", () => {
    const el = drawFeedSessionSeparation(separation(ARMS[2][1]), ctxFor());
    expect(el.querySelector("[data-editor-link]")?.getAttribute("data-host-path")).toBe("/w/tree");
  });

  it("draws the branch when the vendor named one", () => {
    const el = drawFeedSessionSeparation(
      separation({
        case: "worktreeEntered",
        value: { path: { text: "/w" }, branch: { text: "fix" } },
      }),
      ctxFor(),
    );
    expect(el.querySelector(".sep-branch")?.textContent).toBe(" on fix");
  });

  it("draws no branch when the vendor named none", () => {
    const el = drawFeedSessionSeparation(separation(ARMS[2][1]), ctxFor());
    expect(el.querySelector(".sep-branch")).toBeNull();
  });

  it("draws a kept tree's path as the same jump target", () => {
    const el = drawFeedSessionSeparation(
      separation({
        case: "worktreeLeft",
        value: { outcome: { case: "kept", value: { path: { text: "/w/kept" } } } },
      }),
      ctxFor(),
    );
    expect(el.querySelector("[data-editor-link]")?.getAttribute("data-host-path")).toBe("/w/kept");
  });

  it("draws a removed tree's discard line LOUDLY when there was one", () => {
    const el = drawFeedSessionSeparation(
      separation({
        case: "worktreeLeft",
        value: {
          outcome: { case: "removed", value: { discarded: { text: "3 files discarded" } } },
        },
      }),
      ctxFor(),
    );
    expect(el.querySelector(".sep-discarded")?.textContent).toBe("3 files discarded");
  });

  it("draws the label alone when a removal discarded nothing", () => {
    const el = drawFeedSessionSeparation(separation(ARMS[3][1]), ctxFor());
    expect(el.querySelector(".sep-discarded")).toBeNull();
  });

  it("refuses a left divider whose outcome arm is unset", () => {
    const msg = separation({ case: "worktreeLeft", value: {} });
    expect(() => drawFeedSessionSeparation(msg, ctxFor())).toThrow(MalformedView);
  });
});
