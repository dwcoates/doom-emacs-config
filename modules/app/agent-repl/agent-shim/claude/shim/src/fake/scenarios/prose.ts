/**
 * fake/scenarios/prose.ts — the turns that produce only reasoning and text.
 *
 * The default scenario lives here: any prompt that names no `!scenario` falls
 * through to `PROSE`, which is why an ordinary offline session behaves like an
 * ordinary session instead of refusing.
 */
import { conclude, scenario, visibleThinking, withheldThinking } from "./support.js";

/** Canned reply for `!md` — exercises every markdown construct the webapp renders. */
export const MARKDOWN_SHOWCASE = [
  "# Markdown showcase",
  "",
  "Rendered by the webapp's **markdown engine** — *streamed* over the wire like any other turn.",
  "",
  "## What works",
  "",
  "- **Bold**, *italic*, and `inline code`",
  "- [Links](https://example.com) with safe schemes only",
  "- Ordered lists too:",
  "",
  "1. first",
  "2. second",
  "",
  "> Blockquotes for the philosophical bits.",
  "",
  "```go",
  'func main() { fmt.Println("fenced code, escaped & highlighted-ish") }',
  "```",
  "",
  "---",
  "",
  "## A numbered tree",
  "",
  // DOTLESS LABELS, because that is the metaprompt's own tree shape: `1 🌳 …`,
  // `├── 1.1 …`. A trailing dot leaves the root line no longer an emoji root
  // to the webapp's own detector — `dottedLabelEnd` in
  // webapp/src/metaprompt-tree.ts stops at a dot no digit follows — and the
  // showcase then draws as an ordinary markdown list rather than the tree it
  // exists to demonstrate.
  "1 🌳 A bare Unicode tree, the shape the metaprompt answers in.",
  "├── 1.1 This branch is deliberately longer than the bubble's live width, so the webapp wraps it " +
    "before it is drawn, and every continuation line must still carry the rails of the branches around it.",
  "│   └── 1.1.1 A child beneath the wrapped branch, so the rail through the wrap is load-bearing.",
  "└── 1.2 The last branch, whose continuation carries no rail because nothing follows it, once it too " +
    "runs past the bubble's width and wraps onto a second line.",
  "",
  "That is the whole demo.",
].join("\n");

/**
 * A column width at least as wide as any bubble the webapp's test layouts
 * measure a metaprompt tree's budget in (webapp/test/tree-layout.ts). The
 * showcase carries branches WIDER than this so that the webapp's wrap is
 * exercised by every consumer that draws `!md`; the test beside this scenario
 * pins that the showcase still does.
 */
export const WEBAPP_TREE_WRAP_COLUMNS = 105;

/**
 * The default turn: several blocks, both thinking arms, one settled answer.
 *
 * FOUR blocks in one API response, not one. A single-block response cannot
 * exercise the block index at all — `<message.id>:<block_index>` is only
 * interesting when the indices differ — and it cannot show that the WITHHELD
 * and TEXT thinking arms are two shapes of the same block type. The last text
 * block is the turn's conclusion and the result repeats it verbatim.
 */
export const PROSE = scenario({
  name: "",
  prompt: "(any text with no `!scenario` prefix)",
  emits:
    "one API response of four blocks — withheld thinking, visible thinking, an opening text block, " +
    "and the concluding text block — then a success `result` whose `result` is the conclusion verbatim",
  writes: "four assistant lines sharing one `message.id`, then the user prompt line and the turn record",
  arms: "AgentThinking (withheld + text), AgentResponse.from_model, AgentSuccess.completed",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "prose" }, "fake prose turn");
    const conclusion = `echo: ${ctx.prompt} [mode=${ctx.permissionMode}] [model=${ctx.model}]`;
    ctx.assistant(
      [
        withheldThinking(),
        visibleThinking("The reasoning the model chose to surface."),
        { type: "text", text: "Here is what I found." },
        { type: "text", text: conclusion },
      ],
      { stopReason: "end_turn", effort: "high" },
    );
    ctx.result({ subtype: "success", result: conclusion });
  },
});

/** A long markdown reply, for the webapp's renderer. */
const MARKDOWN = scenario({
  name: "md",
  prompt: "!md",
  emits: "one text block carrying the markdown showcase, then a success `result`",
  writes: "one assistant line, the prompt line and the turn record",
  arms: "AgentResponse.from_model, AgentSuccess.completed",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "markdown" }, "fake markdown-showcase turn");
    conclude(ctx, MARKDOWN_SHOWCASE);
  },
});

export const PROSE_SCENARIOS = [PROSE, MARKDOWN];
