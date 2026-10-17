/**
 * page-drawn-report — THE PAGE TELLS THE DAEMON ITS CONVERSATION IS DRAWN.
 *
 * A workspace is open, and the editor shows its tab, only once the daemon
 * knows its session is served AND this page has drawn its conversation
 * (OpenTabs, endpoint_open_tabs.proto). Only the page knows when it has drawn,
 * so it reports it (ReportPageDrawn) the moment its root carries the drawn
 * mark (feed/conversation-drawn.ts), and AGAIN on every re-attach of its page
 * stream while drawn: a daemon that restarted after the page drew has no other
 * way to learn it, and an open waiting on this page would wait forever.
 *
 * A refusal naming an unknown page is the race between a report and its own
 * stream's end; the next attach reports again. Any other refusal, and any
 * transport failure, is recorded loudly.
 */
import { create } from "@bufbuild/protobuf";
import {
  ReportPageDrawnRequestSchema,
  ReportPageDrawnResponseSchema,
} from "../../proto/gen/ts/agentrepl/v1/endpoint_report_page_drawn_pb";
import { CONVERSATION_DRAWN_ATTRIBUTE } from "./feed/conversation-drawn.js";
import { log } from "./log.js";
import type { AppContext } from "./rpc/context.js";
import { callUnary } from "./rpc/unary.js";
import { pageMutationObserver } from "./dom.js";

/** The installed report; `dispose` stops watching. */
export interface PageDrawnReport {
  dispose(): void;
}

/** Whether the page in DOC has drawn its conversation. */
function drawn(doc: Document): boolean {
  return doc.documentElement.hasAttribute(CONVERSATION_DRAWN_ATTRIBUTE);
}

/** Report the drawn conversation of the page in DOC to CTX's daemon. */
export function installPageDrawnReport(ctx: AppContext, doc: Document): PageDrawnReport {
  let reportedThisAttach = false;

  const report = async (cause: "drawn" | "attached"): Promise<void> => {
    if (reportedThisAttach) return;
    reportedThisAttach = true;
    try {
      const response = await callUnary(
        ctx,
        "ReportPageDrawn",
        (client) =>
          client.reportPageDrawn(create(ReportPageDrawnRequestSchema, { workspace: ctx.workspace, page: ctx.streams.page })),
        ReportPageDrawnResponseSchema,
      );
      const result = response.result;
      if (result.case === "success") {
        log.info("told the daemon this page's conversation is drawn", {
          operation: "page.drawn-reported",
          context: { cause },
        });
        return;
      }
      const refusal = result.case === "error" ? result.value.cause.case : undefined;
      if (refusal === "unknownPage") {
        // The report raced this page's stream ending; the next attach reports.
        reportedThisAttach = false;
        log.info("the daemon had no attached stream for this page yet; the next attach reports again", {
          operation: "page.drawn-report-raced",
          context: { cause },
        });
        return;
      }
      log.error(`the daemon refused this page's drawn report: ${String(refusal)}`, {
        operation: "page.drawn-report-refused",
        context: { cause, refusal: String(refusal) },
      });
    } catch (err) {
      // callUnary has already recorded the transport failure; the next attach
      // reports again.
      reportedThisAttach = false;
      log.warn(`the drawn report did not reach the daemon: ${String(err)}`, {
        operation: "page.drawn-report-unsent",
        context: { cause },
      });
    }
  };

  const unsubscribe = ctx.streams.onAttached(() => {
    reportedThisAttach = false;
    if (drawn(doc)) void report("attached");
  });
  const watch = pageMutationObserver(doc, () => {
    if (drawn(doc)) void report("drawn");
  });
  watch.observe(doc.documentElement, { attributes: true, attributeFilter: [CONVERSATION_DRAWN_ATTRIBUTE] });

  return {
    dispose() {
      unsubscribe();
      watch.disconnect();
    },
  };
}
