/**
 * user-prompt — the prompt bubble: what a person typed, or (on a subagent's
 * feed) the commission the bubble was given.
 *
 * THE LOOK IS THE EXISTING ONE, unchanged: the right-flushed `.bubble.user`
 * with its body in `.bubble-body`, and the page-global shadow wave crossing its
 * background. It folds with no line cap (`USER_PROMPT_CAP_LINES`): folded, it
 * shows the whole prompt and hides only a reply's quote.
 *
 * ONE ARM ON PURPOSE. `FeedUserPrompt.result` has a single `success` arm
 * because a user row is never in flight and never fails — but it is still a
 * oneof, so an unset one is a malformed view here rather than a bubble drawn
 * with no body.
 */
import { log } from "../../log.js";
import { USER_PROMPT_CAP_LINES, drawBubble } from "../../bubble/draw.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type {
  FeedUserPrompt,
  FeedUserPromptBlock,
  FeedUserPromptBody,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import type { SlotMark } from "../../bubble/body.js";
import { drawPromptBlockArm, drawPromptCopyControl } from "./blocks.js";
import { reserveCopyRoom } from "../../bubble/copy-room.js";
import { markKnownCommands } from "./known-commands.js";

/** The path prefix every refusal from this file is reported under. */
const PATH = "FeedUserPrompt";

/**
 * The prompt bubble. It is still: no animation runs on it, in flight or not
 * (owner ruling, 2026-10-26).
 */
export function drawFeedUserPrompt(msg: FeedUserPrompt, previous?: HTMLElement): HTMLElement {
  log.info("drawing a user prompt row", {
    operation: "feed.draw-user-prompt",
    context: { arm: msg.result.case ?? "unset", commands: msg.commands.length },
  });
  // The author is still a required field on the wire (a message with none is
  // malformed, not merely unattributed) — validated but no longer drawn: the
  // bubble carries no "You" label or other attribution (owner ruling,
  // 2026-09-14).
  requireMessage(msg.author, `${PATH}.author`);

  const result = requireCase(msg.result, `${PATH}.result`);
  switch (result.case) {
    case "success": {
      const body = requireMessage(result.value.body, `${PATH}.success.body`);
      // PREVIOUS, the row's last draw, is updated in place (drawBubble), so a
      // re-push never replaces the box a reader is scrolled inside.
      return drawBubble(
        {
          role: "prompt",
          variant: "user",
          hooks: ["user"],
          ...userPromptLook(body, `${PATH}.success.body`, knownCommandsMark(msg)),
        },
        previous,
      ).bubble;
    }
    default:
      return unreachableArm(`${PATH}.result`, result.case);
  }
}

/** The parts of a bubble that draw what a person typed as a user prompt does. */
export interface UserPromptLook {
  /** The copy control, at the bubble's top right on its first line of text. */
  corner: HTMLElement[];
  /** The blocks the person composed, through the one body pipeline. */
  content: HTMLElement[];
  /** The unbounded fold: folded, the whole prompt shows. */
  capLines: typeof USER_PROMPT_CAP_LINES;
}

/**
 * The look of a held card, which draws a person's words as `userPromptLook`
 * does, or something with no drawn blocks to copy (a held session act, an
 * editor's own ingress entry) with no copy control.
 */
export type HeldPromptLook = Omit<UserPromptLook, "corner"> & Partial<Pick<UserPromptLook, "corner">>;

/**
 * THE USER PROMPT'S LOOK, the one source of it (owner request, 2026-10-25):
 * every bubble showing words a person typed — the feed's user prompt, a held
 * prompt and an ingress prompt (src/held) — draws BODY's blocks, the copy
 * control over them, and the unbounded fold from here, so the kinds differ
 * only in their fill, their header strip and their footer.
 */
export function userPromptLook(body: FeedUserPromptBody, path: string, mark?: SlotMark): UserPromptLook {
  const content = drawFeedUserPromptBody(body, path, mark);
  reserveCopyRoom(content);
  return {
    corner: [drawPromptCopyControl(body.blocks, `${path}.blocks`)],
    content,
    capLines: USER_PROMPT_CAP_LINES,
  };
}

/**
 * The mark a prompt's text blocks take: every slash word naming one of the
 * known commands the row lists (`msg.commands`, the daemon's), marked as the
 * composer marks it. A row listing none takes no mark.
 */
export function knownCommandsMark(msg: FeedUserPrompt): SlotMark | undefined {
  if (msg.commands.length === 0) return undefined;
  const names = new Set(msg.commands.map((command) => command.name));
  return (rendered) => markKnownCommands(rendered, names);
}

/** The blocks the person composed, in order: the bubble's content. */
export function drawFeedUserPromptBody(
  body: FeedUserPromptBody,
  path: string,
  mark?: SlotMark,
): HTMLElement[] {
  return body.blocks.map((block, index) =>
    drawFeedUserPromptBlock(block, `${path}.blocks[${index}]`, mark),
  );
}

/** One block of a user prompt — the shared drawn block vocabulary. */
export function drawFeedUserPromptBlock(
  block: FeedUserPromptBlock,
  path: string,
  mark?: SlotMark,
): HTMLElement {
  return drawPromptBlockArm(block.block, `${path}.block`, mark);
}
