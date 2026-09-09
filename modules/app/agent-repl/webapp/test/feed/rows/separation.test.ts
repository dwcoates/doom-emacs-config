// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  FeedSessionSeparationSchema,
  type FeedSessionSeparation,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import stylesheet from "../../../src/styles.css?raw";
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
  [
    "compactionFailed",
    { case: "compactionFailed", value: { error: "the summarizing request was refused" } },
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

describe("drawFeedSessionSeparation: the compaction that did not happen", () => {
  const failed: SeparationInit["kind"] = {
    case: "compactionFailed",
    value: { error: "the summarizing request was refused" },
  };

  it("takes the FAILURE accent, not the compacted one it stands in for", () => {
    const el = drawFeedSessionSeparation(separation(failed), ctxFor());
    expect(el.querySelector(".sep-rule")?.className).toContain("sep-accent-compaction-failed");
  });

  it("states the producer's own account of the failure verbatim", () => {
    const el = drawFeedSessionSeparation(separation(failed), ctxFor());
    expect(el.querySelector(".sep-compaction-failed")?.textContent).toBe(
      "the summarizing request was refused",
    );
  });

  it("draws no size change, nothing having been cut", () => {
    const el = drawFeedSessionSeparation(separation(failed), ctxFor());
    expect(el.querySelector(".sep-tokens")).toBeNull();
  });

  it("draws no summary to fold, no compaction having happened", () => {
    const el = drawFeedSessionSeparation(separation(failed), ctxFor());
    expect(el.querySelector(".sep-fold-toggle")).toBeNull();
  });

  it("refuses a failed compaction with no label", () => {
    const msg = create(FeedSessionSeparationSchema, { kind: failed });
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

  // A FOLDED SUMMARY MUST ACTUALLY BE INVISIBLE, which the assertion above
  // does NOT establish: it reads the DOM property, and jsdom applies no
  // stylesheet, so `hidden` reads true while the real page drew the summary
  // anyway. That is exactly what happened — the D30 playtest photographed a
  // divider whose toggle said FOLDED with the summary sitting open beneath it.
  //
  // THE CAUSE IS SPECIFICITY, so this is pinned against the stylesheet rather
  // than against the DOM: the fold hides an element carrying `.bubble`,
  // `.bubble` sets `display`, and a class selector outranks the user-agent
  // `[hidden]` rule — so without an explicit guard `hidden` is inert here.
  it("hides the folded summary in the stylesheet too, not only as a DOM property", () => {
    // Arrange: the element the fold actually toggles.
    const el = drawFeedSessionSeparation(separation(ARMS[1][1]), ctxFor());
    const summary = el.querySelector<HTMLElement>(".sep-summary");

    // Act / Assert: it is a bubble, and `.bubble` sets display...
    expect(summary?.classList.contains("bubble")).toBe(true);
    expect(/\.bubble\s*\{[^}]*\bdisplay\s*:/.test(stylesheet)).toBe(true);

    // ...so the sheet must carry the guard that makes `hidden` bite.
    expect(/\.bubble\[hidden\]\s*\{[^}]*\bdisplay\s*:\s*none/.test(stylesheet)).toBe(true);
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

describe("drawFeedSessionSeparation: arms this build has no case for", () => {
  it("refuses a divider kind a NEWER daemon set, quoting the arm it could not draw", () => {
    // Arrange
    const msg = separation({ case: "cleared", value: {} });
    (msg as unknown as { kind: unknown }).kind = { case: "modelSwapped", value: {} };

    // Act
    let thrown: unknown;
    try {
      drawFeedSessionSeparation(msg, ctxFor());
    } catch (err) {
      thrown = err;
    }

    // Assert
    expect(thrown).toBeInstanceOf(MalformedView);
    expect((thrown as MalformedView).path).toBe("FeedSessionSeparation.kind");
    expect((thrown as MalformedView).detail).toBe(
      "arm 'modelSwapped' is not one this build can draw",
    );
  });

  it("refuses a left tree's outcome arm a NEWER daemon set, quoting that arm", () => {
    // Arrange
    const msg = separation({
      case: "worktreeLeft",
      value: { outcome: { case: "removed", value: {} } },
    });
    const left = (msg.kind as { value: { outcome: unknown } }).value;
    left.outcome = { case: "archived", value: {} };

    // Act
    let thrown: unknown;
    try {
      drawFeedSessionSeparation(msg, ctxFor());
    } catch (err) {
      thrown = err;
    }

    // Assert
    expect(thrown).toBeInstanceOf(MalformedView);
    expect((thrown as MalformedView).path).toBe("FeedSessionSeparation.worktree_left.outcome");
    expect((thrown as MalformedView).detail).toBe(
      "arm 'archived' is not one this build can draw",
    );
  });
});
