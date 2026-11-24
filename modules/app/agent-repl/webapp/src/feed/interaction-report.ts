/**
 * interaction-report — THE READER'S OWN HAND ON THE FEED IS THEIR INTERACTION
 * WITH THE WORKSPACE (owner request, 2026-11-23).
 *
 * A workspace is viewed exactly when the user has interacted with it since its
 * status last changed, and the editor's recency walk orders workspaces by when
 * the user last did (agentrepl.v1.ReportUserInteraction). Scrolling the feed is
 * one of those interactions, and only the page sees it, so the page reports it.
 *
 * ONLY THE READER'S OWN INPUT COUNTS. The report rides the very events the
 * feed's scroll owner reads as the reader's input (`READER_INPUTS`: a wheel, a
 * touch, a grab of the scroll bar, a key while the feed has focus), which
 * precede the movement they cause. A scroll the page makes itself (following
 * new output, revealing a row, a jump) raises none of them, so it is never
 * reported.
 *
 * A GESTURE IS ONE REPORT. One report is in flight at a time, and an input
 * within `INTERACTION_REPORT_SPACING_MS` of the last report sent is the same
 * gesture, so it is dropped rather than held. The spacing is a rate limit, not
 * a bound on anything: the daemon re-stamps and publishes nothing for a repeat
 * that changes nothing a client draws, so a dropped repeat loses nothing.
 *
 * A REFUSAL IS RECORDED and changes nothing here: the reader's next gesture
 * reports again. A transport failure is recorded by `callUnary`.
 */
import { create } from "@bufbuild/protobuf";
import {
  ReportUserInteractionRequestSchema,
  ReportUserInteractionResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_report_user_interaction_pb";
import { createLogger } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { callUnary } from "../rpc/unary.js";
import { READER_INPUTS } from "../scroll.js";

const log = createLogger("webapp.feed.interaction-report");

/**
 * How long after a report the reader's further input is the same gesture: a
 * wheel or trackpad stream, its momentum tail included, lasts about a second
 * (scroll.ts's quiet-frame count reads the same stream), so two seconds holds
 * one gesture and lets the next one report.
 */
export const INTERACTION_REPORT_SPACING_MS = 2000;

/** The slice of the page the reporter needs. */
export type InteractionReportContext = Pick<AppContext, "client" | "failures" | "isQuiesced" | "workspace" | "ticker">;

/**
 * Report the reader's input on BOX, the workspace's feed scroll box, as their
 * interaction with CTX's workspace. Returns the uninstaller.
 */
export function installFeedInteractionReport(ctx: InteractionReportContext, box: HTMLElement): () => void {
  let inFlight = false;
  let lastSentMs: number | null = null;

  const report = async (kind: string): Promise<void> => {
    try {
      const response = await callUnary(
        ctx,
        "ReportUserInteraction",
        (client) =>
          client.reportUserInteraction(
            create(ReportUserInteractionRequestSchema, {
              workspace: ctx.workspace,
              interaction: { kind: { case: "feedScrolled", value: {} } },
            }),
          ),
        ReportUserInteractionResponseSchema,
      );
      const result = response.result;
      if (result.case === "success") {
        log.debug("told the daemon the reader scrolled the feed", {
          operation: "feed.interaction-reported",
          context: { input: kind },
        });
        return;
      }
      const refusal = result.case === "error" ? result.value.cause.case : undefined;
      log.error("the daemon refused the reader's interaction: {refusal}", {
        operation: "feed.interaction-refused",
        context: { input: kind },
      }, { refusal: String(refusal) });
    } catch (err) {
      // callUnary owns the failure's record; this notes that the next
      // gesture reports again.
      log.info("the reader's interaction did not reach the daemon; the next gesture reports again: {err}", {
        operation: "feed.interaction-unsent",
        context: { input: kind },
      }, { err: String(err) });
    } finally {
      inFlight = false;
    }
  };

  const onInput = (event: Event): void => {
    const now = ctx.ticker.now();
    if (inFlight) return;
    if (lastSentMs !== null && now - lastSentMs < INTERACTION_REPORT_SPACING_MS) return;
    inFlight = true;
    lastSentMs = now;
    void report(event.type);
  };

  for (const kind of READER_INPUTS) {
    box.addEventListener(kind, onInput, { passive: true, capture: true });
  }
  return () => {
    for (const kind of READER_INPUTS) {
      box.removeEventListener(kind, onInput, { capture: true });
    }
  };
}
