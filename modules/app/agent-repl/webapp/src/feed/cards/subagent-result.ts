/**
 * subagent-result — A SUBAGENT'S RETURNED RESULT, drawn once inside its own
 * card (the subagent's feed): the report it handed back.
 *
 * ITS SPEC (owner rulings, 2026-09-27): the report is the response bubble —
 * the same `.bubble.assistant` element and the same markdown pipeline a
 * response's prose goes through — and it is UNCAPPED like a final answer, so
 * the reader sees the whole result with no fold. The state rides `data-state`
 * as every card's does. An UNDELIVERED report adds one quiet line under it: the
 * vendor's reason verbatim when it stated one. No new look: the bubble is the
 * response's, and the reason line is the muted token.
 */
import { markdownSlot } from "../../bubble/body.js";
import { BUBBLE_UNCAPPED, drawBubble } from "../../bubble/draw.js";
import { log } from "../../log.js";
import { requireCase, requireMessage, unreachableArm } from "../../rpc/strict.js";
import type { RowContext } from "../renderers.js";
import type { FeedSubagentResult } from "../../../../proto/gen/ts/frontend/v1/feed_pb";

const PATH = "FeedSubagentResult";

/** The class the result bubble's hooks know it by. */
export const SUBAGENT_RESULT_CLASS = "subagent-result";

/** The class of the markdown slot the report is painted into. */
export const SUBAGENT_RESULT_REPORT_CLASS = "subagent-result-report";

/** The class of the undelivered state's quiet reason line. */
export const SUBAGENT_RESULT_REASON_CLASS = "subagent-result-reason";

/** Every state this build draws, for the suite to hold to the schema. */
export const SUBAGENT_RESULT_STATE_ARMS: readonly string[] = ["delivering", "delivered", "undelivered"];

/** The result bubble. */
export function drawFeedSubagentResult(u: FeedSubagentResult, rc: RowContext): HTMLElement {
  const state = requireCase(u.state, `${PATH}.state`);
  // THE ARM IS CHECKED BEFORE ANYTHING IS DRAWN, so an arm a newer daemon set
  // reaches the refusal that names it rather than half a bubble.
  const footer: HTMLElement[] = [];
  switch (state.case) {
    case "delivering":
    case "delivered":
      break;
    case "undelivered":
      if (state.value.reason !== undefined) {
        const reason = document.createElement("div");
        reason.className = SUBAGENT_RESULT_REASON_CLASS;
        // The vendor's refusal prose, drawn verbatim.
        reason.textContent = state.value.reason.text;
        footer.push(reason);
      }
      break;
    default: {
      const other: { case: string } = state;
      return unreachableArm(`${PATH}.state`, other.case);
    }
  }
  const report = requireMessage(u.report, `${PATH}.report`).text;
  log.info("drawing a subagent's returned result", {
    operation: "feed.cards.subagent-result",
    context: { row: rc.row.id?.value ?? "unset", state: state.case, characters: report.length },
  });
  return drawBubble(
    {
      role: "response",
      variant: "response",
      state: state.case,
      hooks: ["assistant", SUBAGENT_RESULT_CLASS],
      content: [markdownSlot(SUBAGENT_RESULT_REPORT_CLASS, report)],
      footer,
      capLines: BUBBLE_UNCAPPED,
    },
    rc.previous,
  ).bubble;
}
