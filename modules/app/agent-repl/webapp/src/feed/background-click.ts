/**
 * background-click — a click on the feed OUTSIDE ANY BUBBLE ends a reply
 * selection (owner ruling, 2026-09-23).
 *
 * While a reply selection is active the latest-visible follow is held off
 * (`TailFollow.selectionMoved`), so streaming rows cannot pull the reader off
 * the response they picked. The hold-off lasts until the reader clicks the
 * feed's background: that click asks the DAEMON to clear the selection
 * (`SelectResponse` with the CLEAR direction, the arm double-escape sends),
 * because the daemon owns the selection. Nothing is cleared here. The daemon's
 * cleared-selection push then reaches `applySelection`, which parks at the tail
 * through the existing named cause (`selectionCleared`), and the follow resumes.
 *
 * A REFUSED OR FAILED CLEAR is an error the reader must see: it is filed on the
 * topbar's warning chip (`control_plane_failed`), the webapp's one error
 * surface, and logged.
 */
import { create } from "@bufbuild/protobuf";
import { ConnectError } from "@connectrpc/connect";
import {
  SelectResponseDirection,
  SelectResponseRequestSchema,
  SelectResponseResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_response_pb";
import { controlPlaneFailed } from "../failure/sink.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { guardMalformed } from "../rpc/guard.js";
import { callFailure, refusalOf } from "../rpc/refuse.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";

/** The class every root row's full-width wrapper wears (feed-view.ts). */
const ROW_WRAPPER_CLASS = "feed-item";

/** What the chip's `request` evidence names this call as. */
export const CLEAR_REQUEST = "clear the reply selection";

/**
 * WHETHER TARGET IS THE FEED'S BACKGROUND inside the scroll BOX. The one hit
 * test.
 *
 * The background is the layout that holds the rows, never anything drawn in
 * them:
 * - the scroll box itself;
 * - one of its direct children (the feed host and the hold tray host), which
 *   is where a click between rows lands, in the flex gap;
 * - a ROOT row's full-width wrapper (`.feed-item` in the feed host), which is
 *   where a click beside a narrower bubble or card lands.
 *
 * Everything else is not: a bubble or a card and anything inside it (each has
 * its own click semantics, expand among them), an interactive control, and a
 * row nested in a bubble's sub-feed, which is part of its bubble.
 */
export function isFeedBackground(target: EventTarget | null, box: Element): boolean {
  if (!(target instanceof Element) || !box.contains(target)) return false;
  if (target === box) return true;
  const parent = target.parentElement;
  if (parent === box) return true;
  return target.classList.contains(ROW_WRAPPER_CLASS) && parent?.parentElement === box;
}

/**
 * Arm the click-to-clear on the scroll BOX. SELECTIONACTIVE answers whether the
 * daemon's last pushed selection is active; a background click sends the clear
 * only then. Answers the uninstall.
 */
export function installBackgroundClear(
  box: HTMLElement,
  ctx: AppContext,
  selectionActive: () => boolean,
): () => void {
  const onClick = (event: MouseEvent): void => {
    if (!isFeedBackground(event.target, box)) return;
    if (!selectionActive()) {
      log.debug("a click on the feed background, with no reply selection to clear", {
        operation: "feed.background-click-idle",
        verbosity: "verbose",
      });
      return;
    }
    if (document.getSelection()?.isCollapsed === false) {
      // A drag that selected text ends in a click on the rows' common ancestor,
      // which is the background; it is a text selection, not a dismissal.
      log.debug("a text drag ended on the feed background; the reply selection stands", {
        operation: "feed.background-click-text-drag",
      });
      return;
    }
    log.info("a click on the feed background clears the reply selection", {
      operation: "feed.background-click-clear",
    });
    void guardMalformed(ctx, "feed.selection-clear", sendClear(ctx));
  };
  box.addEventListener("click", onClick);
  return () => {
    box.removeEventListener("click", onClick);
  };
}

/**
 * Ask the daemon to clear the reply selection. Its answer moves nothing: the
 * daemon's cleared-selection push is what returns the feed to its tail.
 */
async function sendClear(ctx: AppContext): Promise<void> {
  let response;
  try {
    response = await callUnary(
      ctx,
      "SelectResponse",
      (client) =>
        client.selectResponse(
          create(SelectResponseRequestSchema, {
            workspace: ctx.workspace,
            direction: SelectResponseDirection.CLEAR,
          }),
        ),
      SelectResponseResponseSchema,
    );
  } catch (err) {
    // `callUnary` already logged the transport failure; the chip is told here.
    if (!(err instanceof ConnectError)) throw err;
    ctx.failures.report(controlPlaneFailed(CLEAR_REQUEST, callFailure(err).text));
    return;
  }
  const result = requireCase(response.result, "SelectResponseResponse.result");
  switch (result.case) {
    case "success":
      log.debug("the daemon cleared the reply selection; its push returns the feed to the tail", {
        operation: "feed.selection-clear-answered",
      });
      return;
    case "error": {
      const said = refusalOf(result.value.cause, {}, "SelectResponseError.cause");
      log.error(`the daemon refused to clear the reply selection: ${said.text}`, {
        operation: "feed.selection-clear-refused",
        context: { arm: said.arm },
      });
      ctx.failures.report(controlPlaneFailed(CLEAR_REQUEST, said.text));
      return;
    }
    default: {
      const other: { case: string } = result;
      return unreachableArm("SelectResponseResponse.result", other.case);
    }
  }
}
