// @vitest-environment jsdom
// The page reports its drawn conversation to the daemon, on the mark and on
// every re-attach while drawn. See src/page-drawn-report.ts.
import { create } from "@bufbuild/protobuf";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  ReportPageDrawnResponseSchema,
  type ReportPageDrawnRequest,
  type ReportPageDrawnResponse,
} from "../../proto/gen/ts/agentrepl/v1/endpoint_report_page_drawn_pb";
import { CONVERSATION_DRAWN_ATTRIBUTE } from "../src/feed/conversation-drawn.js";
import { installPageDrawnReport, type PageDrawnReport } from "../src/page-drawn-report.js";
import type { AppContext } from "../src/rpc/context.js";
import { createTicker } from "../src/clock.js";
import { createAppContext } from "../src/rpc/context.js";
import { captureLogRecords, forwardedRecord, type LogCapture } from "./log-capture.js";
import { workspaceRef } from "../src/rpc/workspace-ref.js";
import { createLocalFailures } from "../src/failure/local.js";
import type { PageStreams } from "../src/rpc/page-streams.js";
import type { AgentReplClient } from "../src/rpc/client.js";


let sent: ReportPageDrawnRequest[];
let answer: ReportPageDrawnResponse;
let attachedListeners: (() => void)[];
let report: PageDrawnReport;
let capture: LogCapture;

function success(): ReportPageDrawnResponse {
  return create(ReportPageDrawnResponseSchema, { result: { case: "success", value: {} } });
}

function refused(cause: "unknownPage" | "unknownWorkspace"): ReportPageDrawnResponse {
  return create(ReportPageDrawnResponseSchema, {
    result: { case: "error", value: { cause: { case: cause, value: {} } } },
  });
}

/** A context whose client records ReportPageDrawn and whose stream attaches on demand. */
function context(): AppContext {
  const client = {
    reportPageDrawn: async (request: ReportPageDrawnRequest) => {
      sent.push(request);
      return answer;
    },
  } as unknown as AgentReplClient;
  const streams = {
    page: "page-7",
    watch: () => {
      throw new Error("no subscriptions in this test");
    },
    onAttached: (fn: () => void) => {
      attachedListeners.push(fn);
      return () => {
        attachedListeners = attachedListeners.filter((f) => f !== fn);
      };
    },
  } as unknown as PageStreams;
  return createAppContext({
    client,
    workspace: workspaceRef("ws-1", "/w"),
    ticker: createTicker(1000),
    failures: createLocalFailures(),
    composerEnabled: false,
    page: "page-7",
    streams,
  });
}

async function flush(): Promise<void> {
  for (let i = 0; i < 5; i += 1) await Promise.resolve();
}

function attach(): void {
  for (const fn of [...attachedListeners]) fn();
}

beforeEach(() => {
  sent = [];
  answer = success();
  attachedListeners = [];
  capture = captureLogRecords("debug");
});

afterEach(() => {
  report?.dispose();
  document.documentElement.removeAttribute(CONVERSATION_DRAWN_ATTRIBUTE);
});

describe("installPageDrawnReport", () => {
  it("reports when the conversation is drawn", async () => {
    // Arrange
    report = installPageDrawnReport(context(), document);

    // Act
    document.documentElement.setAttribute(CONVERSATION_DRAWN_ATTRIBUTE, "page");
    await flush();

    // Assert
    expect(sent.map((r) => r.page)).toEqual(["page-7"]);
  });

  it("reports nothing on an attach before the conversation is drawn", async () => {
    // Arrange
    report = installPageDrawnReport(context(), document);

    // Act
    attach();
    await flush();

    // Assert
    expect(sent).toEqual([]);
  });

  it("reports again on a re-attach while drawn", async () => {
    // Arrange
    report = installPageDrawnReport(context(), document);
    document.documentElement.setAttribute(CONVERSATION_DRAWN_ATTRIBUTE, "page");
    await flush();

    // Act
    attach();
    await flush();

    // Assert
    expect(sent).toHaveLength(2);
  });

  it("reports once per attach however often the mark is seen", async () => {
    // Arrange
    report = installPageDrawnReport(context(), document);
    document.documentElement.setAttribute(CONVERSATION_DRAWN_ATTRIBUTE, "page");
    await flush();

    // Act
    document.documentElement.setAttribute(CONVERSATION_DRAWN_ATTRIBUTE, "refused");
    await flush();

    // Assert
    expect(sent).toHaveLength(1);
  });

  it("reports again on the next attach after the daemon knew no such page", async () => {
    // Arrange
    answer = refused("unknownPage");
    report = installPageDrawnReport(context(), document);
    document.documentElement.setAttribute(CONVERSATION_DRAWN_ATTRIBUTE, "page");
    await flush();
    answer = success();

    // Act
    attach();
    await flush();

    // Assert
    expect(sent).toHaveLength(2);
  });

  it("records a refusal other than an unknown page at error", async () => {
    // Arrange
    answer = refused("unknownWorkspace");
    report = installPageDrawnReport(context(), document);

    // Act
    document.documentElement.setAttribute(CONVERSATION_DRAWN_ATTRIBUTE, "page");
    await flush();

    // Assert
    expect((await forwardedRecord(capture, "page.drawn-report-refused")).level.case).toBe("error");
  });

  it("records a report the daemon took at info", async () => {
    // Arrange
    report = installPageDrawnReport(context(), document);

    // Act
    document.documentElement.setAttribute(CONVERSATION_DRAWN_ATTRIBUTE, "page");
    await flush();

    // Assert
    expect((await forwardedRecord(capture, "page.drawn-reported")).level.case).toBe("info");
  });
});
