/**
 * The read-notifications converter. The call takes no input and the vendor
 * answers in prose, so what matters is that the returned text is carried
 * verbatim — an empty queue is a successful answer — and that a failed read is
 * the failure arm.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { toolResultText } from "../../../src/convert/entries.js";
import { readNotificationsConverter } from "../../../src/convert/tools/read-notifications.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { conversationv1 } from "../../../src/proto.js";

const AGENT_ID = create(conversationv1.AgentIdSchema, { value: "session-1" });

const CALL: PendingCall = {
  toolUseId: "toolu_notes",
  toolName: "ReadNotifications",
  input: {},
  startedAtMs: 1_700_000_000_000,
  agentId: AGENT_ID,
};

function outcomeWith(
  content: conversationv1.ToolResultContent | undefined,
  isError = false,
): ToolOutcome {
  return { content, isError, structured: undefined, settledAtMs: 1_700_000_002_000 };
}

function readOf(
  item: conversationv1.AgentActivity["item"] | undefined,
): conversationv1.AgentReadNotifications {
  expect(item?.case).toBe("readNotifications");
  return item?.value as conversationv1.AgentReadNotifications;
}

function successOf(outcome: ToolOutcome): conversationv1.AgentReadNotificationsSuccess {
  const read = readOf(readNotificationsConverter.settle(CALL, outcome));
  expect(read.result.case).toBe("success");
  return read.result.value as conversationv1.AgentReadNotificationsSuccess;
}

describe("readNotificationsConverter kind and arms", () => {
  it("declares the read_notifications kind", () => {
    // Arrange, Act, Assert.
    expect(readNotificationsConverter.kind).toBe("read_notifications");
  });

  it("carries NO progress, because AgentReadNotifications declares no such arm", () => {
    // Arrange, Act, Assert.
    expect(readNotificationsConverter.carriesProgress).toBe(false);
  });
});

describe("readNotificationsConverter.start", () => {
  it("stamps the instant the call was issued", () => {
    // Arrange, Act.
    const read = readOf(readNotificationsConverter.start(CALL));

    // Assert.
    expect(read.result.case).toBe("start");
    expect((read.result.value as conversationv1.AgentReadNotificationsStart).startedAt?.atMs).toBe(
      1_700_000_000_000n,
    );
  });
});

describe("readNotificationsConverter.settle", () => {
  it("carries an empty queue's sentence verbatim as a success", () => {
    // Arrange.
    const outcome = outcomeWith(toolResultText("No queued notifications."));

    // Act, Assert.
    expect(successOf(outcome).returnedText).toBe("No queued notifications.");
  });

  it("carries a listed queue verbatim, line breaks and all", () => {
    // Arrange.
    const listing = "Task build finished.\nTask lint failed.";

    // Act, Assert.
    expect(successOf(outcomeWith(toolResultText(listing))).returnedText).toBe(listing);
  });

  it("leaves the returned text unset when the vendor returned no content", () => {
    // Arrange, Act, Assert.
    expect(successOf(outcomeWith(undefined)).returnedText).toBeUndefined();
  });

  it("leaves the returned text unset when the vendor returned only an empty text", () => {
    // Arrange, Act, Assert.
    expect(successOf(outcomeWith(toolResultText(""))).returnedText).toBeUndefined();
  });

  it("states the settle instant with the call's start restated", () => {
    // Arrange, Act.
    const settled = successOf(outcomeWith(toolResultText("No queued notifications."))).settledAt;

    // Assert.
    expect(settled?.atMs).toBe(1_700_000_002_000n);
    expect(settled?.startedAt?.atMs).toBe(1_700_000_000_000n);
  });

  it("answers a vendor error with the failure arm carrying what the tool said", () => {
    // Arrange.
    const outcome = outcomeWith(toolResultText("the queue could not be read"), true);

    // Act.
    const read = readOf(readNotificationsConverter.settle(CALL, outcome));

    // Assert.
    expect(read.result.case).toBe("failure");
    const failure = read.result.value as conversationv1.AgentReadNotificationsFailure;
    expect(failure.error?.content?.blocks[0]?.block.value).toMatchObject({
      text: "the queue could not be read",
    });
  });
});
