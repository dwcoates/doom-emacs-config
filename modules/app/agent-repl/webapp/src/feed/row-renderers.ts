/**
 * row-renderers — THE REGISTRY: the one place the feed's row-renderer seam
 * (renderers.ts) is filled in with every card this build draws.
 *
 * IT IS ITS OWN MODULE so the seam stays below every card in the import graph:
 * each card imports the seam's types and helpers from renderers.ts, and only
 * this module imports the cards, so no card and the seam import each other in
 * a cycle (test/import-cycles.test.ts).
 */
import type { AppContext } from "../rpc/context.js";
import { drawFeedCommandPanel } from "../panels/panels.js";
import { drawFeedCommandRefused } from "../panels/refused.js";
import { drawFeedArtifact } from "./cards/artifact.js";
import { drawFeedChessBoard } from "./cards/chess-board.js";
import { drawFeedFindings } from "./cards/findings.js";
import { drawFeedFixGate } from "./cards/fix-gate.js";
import { drawFeedHotReload } from "./cards/hot-reload.js";
import { drawFeedHook } from "./cards/hook.js";
import { drawFeedPlan } from "./cards/plan.js";
import { drawFeedResponse } from "./cards/response.js";
import { drawFeedShellBody, drawFeedShellHead } from "./cards/shell.js";
import { drawFeedSkill } from "./cards/skill.js";
import { drawFeedSubagentResult } from "./cards/subagent-result.js";
import { drawFeedSimpleToolCall } from "./cards/tool-call.js";
import { drawFeedColdGate } from "./asks/cold-gate.js";
import { drawFeedPermission } from "./asks/permission.js";
import { drawFeedQuestion } from "./asks/question.js";
import { drawFeedMerge } from "./merge/merge.js";
import { mergeBubbleBody } from "./merge/merge-body.js";
import { drawFeedDetachedSubagent, drawFeedSubagent } from "./rows/subagent.js";
import { withCardQuestion } from "./response-question.js";
import { createLogger } from "../log.js";
import type { RowRenderers } from "./renderers.js";

const log = createLogger("webapp.feed.row-renderers");

/**
 * THE REGISTRY: every row renderer this build has, assembled once.
 *
 * WHY IT IS ASSEMBLED HERE rather than imported by the feed. The feed core is
 * the mechanism (pages, the tail, upserts, bubbles, reveal) and the cards are
 * the drawings; the feed importing all fifteen would put the mechanism above
 * every card in the import graph and make the seam unusable — a card could not
 * be tested, or replaced, without the whole feed behind it. So the seam is a
 * plain record, this function is the ONE place it is filled in, and both
 * `main.ts` and the integration harness call exactly this. A key added to
 * `RowRenderers` and not here does not compile.
 *
 * CTX is taken (and not yet read) because the seam belongs to the page, not to
 * a row: every renderer is handed its own `RowContext` at draw time. Taking it
 * keeps the call site honest — the registry is built once per page, after the
 * context exists — and leaves room for a renderer that needs a page-level fact
 * without changing every caller.
 */
export function createRowRenderers(_ctx: AppContext): RowRenderers {
  log.debug("assembling the feed's row renderers", {
    operation: "feed.renderers.assemble",
  });
  return {
    response: withCardQuestion(drawFeedResponse),
    simpleToolCall: withCardQuestion(drawFeedSimpleToolCall),
    hook: withCardQuestion(drawFeedHook),
    skill: withCardQuestion(drawFeedSkill),
    artifact: withCardQuestion(drawFeedArtifact),
    plan: withCardQuestion(drawFeedPlan),
    findings: withCardQuestion(drawFeedFindings),
    chessBoard: withCardQuestion(drawFeedChessBoard),
    subagentResult: withCardQuestion(drawFeedSubagentResult),
    fixGate: withCardQuestion(drawFeedFixGate),
    hotReload: withCardQuestion(drawFeedHotReload),
    shell: withCardQuestion(drawFeedShellBody),
    shellHead: withCardQuestion(drawFeedShellHead),
    permission: withCardQuestion(drawFeedPermission),
    question: withCardQuestion(drawFeedQuestion),
    coldGate: withCardQuestion(drawFeedColdGate),
    mergeHead: withCardQuestion(drawFeedMerge),
    subagentHead: withCardQuestion(drawFeedSubagent),
    detachedSubagentHead: withCardQuestion(drawFeedDetachedSubagent),
    commandPanel: withCardQuestion(drawFeedCommandPanel),
    commandRefused: withCardQuestion(drawFeedCommandRefused),
    mergeBody: mergeBubbleBody,
  };
}
