// @vitest-environment jsdom
/**
 * response-question — a final response's question-mark control: a click asks
 * the daemon to toggle the question, and the pushed state draws every control.
 */
import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import {
  AskAboutResponseResponseSchema,
  type AskAboutResponseRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_ask_about_response_pb";
import { FeedResponseQuestionSchema, type FeedResponseQuestion } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import {
  ASKING_ATTRIBUTE,
  BUBBLE_QUESTION_CLASS,
  QUESTION_CONTROL_CLASS,
  QUESTION_ROW_ATTRIBUTE,
  applyResponseQuestion,
  installResponseQuestion,
  questionControl,
} from "../../src/feed/response-question.js";
import { scriptedAppContext } from "../rpc/app-context.js";
import { recordingFailures } from "../failure-sink.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

let teardown: (() => void) | undefined;

afterEach(() => {
  teardown?.();
  teardown = undefined;
  applyResponseQuestion(document, none());
  document.body.replaceChildren();
});

/** The pushed state naming no question. */
function none(): FeedResponseQuestion {
  return create(FeedResponseQuestionSchema, { state: { case: "none", value: {} } });
}

/** The pushed state naming a question about ROW. */
function asking(row: string): FeedResponseQuestion {
  return create(FeedResponseQuestionSchema, { state: { case: "asking", value: { row: { value: row } } } });
}

/** A feed host holding one question control per row in ROWS. */
function feedWith(rows: string[]): HTMLElement {
  const host = document.createElement("div");
  for (const row of rows) host.append(questionControl(row));
  document.body.append(host);
  return host;
}

/** The control for ROW inside HOST. */
function controlOf(host: HTMLElement, row: string): HTMLElement {
  const control = host.querySelector<HTMLElement>(`[${QUESTION_ROW_ATTRIBUTE}="${row}"]`);
  if (control === null) throw new Error(`no question control for ${row}`);
  return control;
}

/** Let the RPCs the clicks sent answer. */
async function answered(): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, 0));
}

/** A page whose daemon answers every toggle with ANSWER (success by default), recording each request. */
function scripted(answer?: () => unknown): {
  calls: AskAboutResponseRequest[];
  reported: FailureKind[];
  install: (host: HTMLElement) => void;
} {
  const calls: AskAboutResponseRequest[] = [];
  const sink = recordingFailures();
  const ctx = scriptedAppContext(
    {
      askAboutResponse: (req) => {
        calls.push(req);
        if (answer !== undefined) return answer() as never;
        return create(AskAboutResponseResponseSchema, {
          result: { case: "success", value: { state: { case: "withdrawn", value: {} } } },
        });
      },
    },
    { workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }), failures: sink },
  );
  return {
    calls,
    reported: sink.reports,
    install: (host) => {
      teardown = installResponseQuestion(host, ctx);
    },
  };
}

describe("questionControl", () => {
  it("is an ar-button naming its row, placed as a bubble's question control", () => {
    // Act
    const control = questionControl("r1");
    // Assert
    expect([
      control.tagName.toLowerCase(),
      control.classList.contains(QUESTION_CONTROL_CLASS),
      control.classList.contains(BUBBLE_QUESTION_CLASS),
      control.getAttribute(QUESTION_ROW_ATTRIBUTE),
    ]).toEqual(["ar-button", true, true, "r1"]);
  });

  it("is drawn at rest while no question stands", () => {
    // Act
    const control = questionControl("r1");
    // Assert
    expect([control.hasAttribute(ASKING_ATTRIBUTE), control.getAttribute("aria-pressed")]).toEqual([false, "false"]);
  });

  it("is drawn pressed when drawn while the question about its row stands", () => {
    // Arrange
    applyResponseQuestion(document, asking("r1"));
    // Act
    const control = questionControl("r1");
    // Assert
    expect([control.hasAttribute(ASKING_ATTRIBUTE), control.getAttribute("aria-pressed")]).toEqual([true, "true"]);
  });

  it("is drawn at rest while a question about another row stands", () => {
    // Arrange
    applyResponseQuestion(document, asking("r2"));
    // Act
    const control = questionControl("r1");
    // Assert
    expect(control.hasAttribute(ASKING_ATTRIBUTE)).toBe(false);
  });

  it("refuses a control naming no row", () => {
    // Act / Assert
    expect(() => questionControl("")).toThrow(/names no response row/);
  });
});

describe("applyResponseQuestion", () => {
  it("presses only the asked row's control", () => {
    // Arrange
    const host = feedWith(["r1", "r2"]);
    // Act
    applyResponseQuestion(document, asking("r2"));
    // Assert
    expect([controlOf(host, "r1").hasAttribute(ASKING_ATTRIBUTE), controlOf(host, "r2").hasAttribute(ASKING_ATTRIBUTE)]).toEqual([
      false,
      true,
    ]);
  });

  it("releases every control when the question ends", () => {
    // Arrange
    const host = feedWith(["r1"]);
    applyResponseQuestion(document, asking("r1"));
    // Act
    applyResponseQuestion(document, none());
    // Assert
    expect(controlOf(host, "r1").hasAttribute(ASKING_ATTRIBUTE)).toBe(false);
  });

  it("relabels a pressed control as withdrawing the question", () => {
    // Arrange
    const host = feedWith(["r1"]);
    // Act
    applyResponseQuestion(document, asking("r1"));
    // Assert
    expect(controlOf(host, "r1").getAttribute("title")).toBe("Withdraw the question about this response");
  });

  it("refuses a push with no state", () => {
    // Act / Assert
    expect(() => applyResponseQuestion(document, create(FeedResponseQuestionSchema, {}))).toThrow();
  });

  it("refuses an ask naming no row", () => {
    // Act / Assert
    expect(() =>
      applyResponseQuestion(document, create(FeedResponseQuestionSchema, { state: { case: "asking", value: {} } })),
    ).toThrow();
  });
});

describe("installResponseQuestion", () => {
  it("asks the daemon to toggle the clicked control's row", async () => {
    // Arrange
    const host = feedWith(["r1", "r2"]);
    const page = scripted();
    page.install(host);
    // Act
    controlOf(host, "r2").click();
    await answered();
    // Assert
    expect(page.calls.map((call) => [call.workspace?.id, call.row?.value])).toEqual([["ws-1", "r2"]]);
  });

  it("presses nothing itself: only the daemon's push draws the control", async () => {
    // Arrange
    const host = feedWith(["r1"]);
    const page = scripted();
    page.install(host);
    // Act
    controlOf(host, "r1").click();
    await answered();
    // Assert
    expect(controlOf(host, "r1").hasAttribute(ASKING_ATTRIBUTE)).toBe(false);
  });

  it("files the daemon's refusal on the chip and in the log", async () => {
    // Arrange
    const host = feedWith(["r1"]);
    const page = scripted(() =>
      create(AskAboutResponseResponseSchema, { result: { case: "error", value: { cause: { case: "notAFinalResponse", value: {} } } } }),
    );
    page.install(host);
    const records = captureLogRecords();
    // Act
    controlOf(host, "r1").click();
    await answered();
    // Assert
    expect(page.reported).toHaveLength(1);
    const record = await forwardedRecord(records, "feed.response-question.refused");
    expect([record.level.case, record.context?.arm, record.context?.row]).toEqual(["error", "notAFinalResponse", "r1"]);
  });

  it("files a daemon it cannot reach on the chip", async () => {
    // Arrange
    const host = feedWith(["r1"]);
    const page = scripted(() => {
      throw new ConnectError("connection refused", Code.Unavailable);
    });
    page.install(host);
    // Act
    controlOf(host, "r1").click();
    await answered();
    // Assert
    expect(page.reported).toHaveLength(1);
  });

  it("ignores a click outside every question control", async () => {
    // Arrange
    const host = feedWith(["r1"]);
    const page = scripted();
    page.install(host);
    // Act
    host.click();
    await answered();
    // Assert
    expect(page.calls).toEqual([]);
  });

  it("stops asking once uninstalled", async () => {
    // Arrange
    const host = feedWith(["r1"]);
    const page = scripted();
    page.install(host);
    teardown?.();
    teardown = undefined;
    // Act
    controlOf(host, "r1").click();
    await answered();
    // Assert
    expect(page.calls).toEqual([]);
  });
});
