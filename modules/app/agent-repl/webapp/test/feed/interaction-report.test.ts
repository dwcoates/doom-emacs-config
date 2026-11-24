// @vitest-environment jsdom
// The reader's own input on the feed is reported as their interaction with
// the workspace, once per gesture. See src/feed/interaction-report.ts.
import { create } from "@bufbuild/protobuf";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  ReportUserInteractionResponseSchema,
  type ReportUserInteractionRequest,
  type ReportUserInteractionResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_report_user_interaction_pb";
import {
  INTERACTION_REPORT_SPACING_MS,
  installFeedInteractionReport,
  type InteractionReportContext,
} from "../../src/feed/interaction-report.js";
import { createLocalFailures } from "../../src/failure/local.js";
import type { AgentReplClient } from "../../src/rpc/client.js";
import { workspaceRef } from "../../src/rpc/workspace-ref.js";
import { READER_INPUTS } from "../../src/scroll.js";
import { captureLogRecords, forwardedRecord, type LogCapture } from "../log-capture.js";

let sent: ReportUserInteractionRequest[];
let answer: () => Promise<ReportUserInteractionResponse>;
let nowMs: number;
let quiesced: boolean;
let box: HTMLElement;
let uninstall: () => void;
let capture: LogCapture;

function success(): ReportUserInteractionResponse {
  return create(ReportUserInteractionResponseSchema, { result: { case: "success", value: {} } });
}

/** A context whose client records ReportUserInteraction and whose clock is `nowMs`. */
function context(): InteractionReportContext {
  const client = {
    reportUserInteraction: async (request: ReportUserInteractionRequest) => {
      sent.push(request);
      return answer();
    },
  } as unknown as AgentReplClient;
  return {
    client,
    failures: createLocalFailures(),
    isQuiesced: () => quiesced,
    workspace: workspaceRef("ws-1", "/w"),
    ticker: { now: () => nowMs, subscribe: () => () => {} },
  };
}

async function flush(): Promise<void> {
  for (let i = 0; i < 5; i += 1) await Promise.resolve();
}

function input(kind: string = "wheel"): void {
  // A key is a KeyboardEvent, as the page's own key handlers expect of one.
  box.dispatchEvent(kind === "keydown" ? new KeyboardEvent(kind, { bubbles: true, key: "PageDown" }) : new Event(kind, { bubbles: true }));
}

beforeEach(() => {
  sent = [];
  answer = async () => success();
  nowMs = 100_000;
  quiesced = false;
  box = document.createElement("div");
  document.body.appendChild(box);
  capture = captureLogRecords("debug");
  uninstall = installFeedInteractionReport(context(), box);
});

afterEach(() => {
  uninstall();
  box.remove();
});

describe("installFeedInteractionReport", () => {
  it.each([...READER_INPUTS])("reports a %s on the feed as the reader scrolling it", async (kind) => {
    // Act
    input(kind);
    await flush();

    // Assert
    expect(sent.map((r) => [r.workspace?.id, r.interaction?.kind.case])).toEqual([["ws-1", "feedScrolled"]]);
  });

  it("reports nothing for a scroll the page makes itself", async () => {
    // Act: a programmatic move raises a scroll event and no reader input.
    box.dispatchEvent(new Event("scroll"));
    await flush();

    // Assert
    expect(sent).toEqual([]);
  });

  it("reports a gesture once, however many inputs it carries", async () => {
    // Act
    input();
    await flush();
    nowMs += INTERACTION_REPORT_SPACING_MS - 1;
    input();
    await flush();

    // Assert
    expect(sent).toHaveLength(1);
  });

  it("reports the next gesture once the spacing has passed", async () => {
    // Arrange
    input();
    await flush();

    // Act
    nowMs += INTERACTION_REPORT_SPACING_MS;
    input();
    await flush();

    // Assert
    expect(sent).toHaveLength(2);
  });

  it("drops an input while a report is in flight", async () => {
    // Arrange: the first report's answer is held.
    let release!: () => void;
    answer = () => new Promise((resolve) => (release = () => resolve(success())));
    input();
    await flush();

    // Act
    nowMs += INTERACTION_REPORT_SPACING_MS;
    input();
    await flush();
    release();
    await flush();

    // Assert
    expect(sent).toHaveLength(1);
  });

  it("records a refusal at error, and the next gesture reports again", async () => {
    // Arrange
    answer = async () =>
      create(ReportUserInteractionResponseSchema, {
        result: { case: "error", value: { cause: { case: "unknownWorkspace", value: {} } } },
      });

    // Act
    input();
    await flush();
    nowMs += INTERACTION_REPORT_SPACING_MS;
    input();
    await flush();

    // Assert
    expect((await forwardedRecord(capture, "feed.interaction-refused")).level.case).toBe("error");
    expect(sent).toHaveLength(2);
  });

  it("records a report a quiesced page could not send at info", async () => {
    // Arrange
    quiesced = true;

    // Act
    input();
    await flush();

    // Assert
    expect(sent).toEqual([]);
    expect((await forwardedRecord(capture, "feed.interaction-unsent")).level.case).toBe("info");
  });

  it("stops reporting once uninstalled", async () => {
    // Act
    uninstall();
    input();
    await flush();

    // Assert
    expect(sent).toEqual([]);
  });
});
