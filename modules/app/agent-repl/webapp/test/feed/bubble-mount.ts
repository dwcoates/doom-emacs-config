/**
 * A BUBBLE MOUNTED FOR A TEST: `mountBubble` over a row, with a marked stub
 * head and the default body, placed on the page. Shared by the bubble's
 * behavior tests (test/feed/bubble.test.ts) and its rules over the stylesheet
 * (rules/feed/bubble.rules.test.ts).
 */
import type { FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { mountBubble, type BubbleOverlay, type WireFold } from "../../src/feed/bubble.js";
import { defaultBubbleBody, type Handle } from "../../src/feed/renderers.js";
import type { RowContext } from "../../src/feed/cards/context.js";
import type { TailFollow } from "../../src/scroll.js";
import { harness, rowContext, stubRenderers, type Harness } from "./harness.js";

/** Mount a bubble for ROW with a marked head and the default body. */
export function mount(
  row: FeedRow,
  h: Harness = harness(),
  opts: {
    folded?: boolean;
    composerFactory?: (host: HTMLElement) => Handle;
    /** What the head states about itself, per draw. */
    states?: (string | null)[];
    /** A head of the suite's own, in place of the marked stub. */
    head?: (rc: RowContext) => HTMLElement;
    /** The page's scroll box and tail owner. */
    scroll?: { box: Element; tail: TailFollow };
    /** The daemon's fold, read off each push (a merge row ships one). */
    foldOf?: (row: FeedRow) => WireFold;
    merge?: boolean;
    /** Told each fold the reader makes with the head's toggle. */
    readerFold?: (folded: boolean) => void;
    /** Whether the reader may fold the bubble as its row stands. */
    collapsible?: (row: FeedRow) => boolean;
    /** The question drawn in place of the sub-feed. */
    overlay?: BubbleOverlay;
    /** Leave the bubble off the page, as the feed does before it places a row. */
    detached?: boolean;
    /** The feed the row sits in: the root feed unless a sub-feed is named. */
    feed?: RowContext["feed"];
  } = {},
) {
  const heads: number[] = [];
  const bubble = mountBubble({
    ctx: h.ctx,
    row,
    rc: rowContext(h.ctx, row, opts.feed === undefined ? {} : { feed: opts.feed }),
    head: (_row, rc) => {
      if (opts.head !== undefined) return opts.head(rc);
      const el = document.createElement("span");
      el.className = "stub-head";
      const state = opts.states?.[heads.length];
      if (state !== undefined && state !== null) el.setAttribute("data-state", state);
      heads.push(1);
      return el;
    },
    body: defaultBubbleBody,
    renderers: stubRenderers(),
    revealRow: async () => false,
    bubble: () => {
      throw new Error("no nested bubble in this fixture");
    },
    composerFactory:
      opts.composerFactory === undefined
        ? undefined
        : (host) => opts.composerFactory!(host),
    initialFolded: opts.folded ?? true,
    foldOf: opts.foldOf,
    merge: opts.merge,
    readerFold: opts.readerFold,
    collapsible: opts.collapsible,
    overlay: opts.overlay,
    scroll: opts.scroll,
  });
  if (opts.detached !== true) document.body.replaceChildren(bubble.element);
  return { bubble, h, heads };
}

/** The sub-feed panel of a mounted bubble. */
export function panelOf(bubble: { element: HTMLElement }): HTMLElement {
  const panel = bubble.element.querySelector<HTMLElement>("[data-subfeed]");
  if (panel === null) throw new Error("the bubble mounted no sub-feed panel");
  return panel;
}
