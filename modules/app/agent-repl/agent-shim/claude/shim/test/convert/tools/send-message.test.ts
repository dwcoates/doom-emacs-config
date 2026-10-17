/**
 * The message to another agent. Two identifier spaces meet here: the ADDRESS the
 * caller wrote (a plain string, unresolved) and the AGENT it turned out to name
 * (a typed AgentId, resolved only on the result). The tests assert they never
 * cross.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import { sendMessageConverter } from "../../../src/convert/tools/send-message.js";
import { toolProgress } from "../../../src/convert/entries.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { toolUseResult } from "./corpus.js";

function call(input: Record<string, unknown>): PendingCall {
  return {
    toolUseId: "toolu_01SnBym8xzY2VtEG6GGXupwr",
    toolName: "SendMessage",
    input,
    startedAtMs: 3_000,
    agentId: create(conversationv1.AgentIdSchema, { value: "caller" }),
  };
}

function outcome(structured: unknown, isError = false): ToolOutcome {
  return { content: undefined, isError, structured, settledAtMs: 8_000 };
}

function armOf(
  item: conversationv1.AgentActivity["item"] | undefined,
): conversationv1.AgentSendMessage["result"] {
  expect(item?.case).toBe("sendMessage");
  return (item?.value as conversationv1.AgentSendMessage).result;
}

function startOf(item: conversationv1.AgentActivity["item"] | undefined): conversationv1.AgentSendMessageStart {
  return armOf(item).value as conversationv1.AgentSendMessageStart;
}

function successOf(
  item: conversationv1.AgentActivity["item"] | undefined,
): conversationv1.AgentSendMessageSuccess {
  const arm = armOf(item);
  expect(arm.case).toBe("success");
  return arm.value as conversationv1.AgentSendMessageSuccess;
}

/** The agent recipient's id, or undefined when the send reached no agent of this session. */
function agentRecipientOf(success: conversationv1.AgentSendMessageSuccess): string | undefined {
  return success.recipient.case === "recipientAgentId" ? success.recipient.value.value : undefined;
}

describe("sendMessageConverter.start", () => {
  it("carries the address exactly as written, untyped", () => {
    // Arrange, Act.
    const start = startOf(sendMessageConverter.start(call({ to: "vetter", message: "go" })));

    // Assert.
    expect(start.addressedTo).toBe("vetter");
  });

  it("carries the caller's one-line summary", () => {
    // Arrange, Act.
    const start = startOf(
      sendMessageConverter.start(call({ to: "a1", message: "go", summary: "resume the vetting" })),
    );

    // Assert.
    expect(start.summary?.text).toBe("resume the vetting");
  });

  it("leaves the summary UNSET when the caller supplied none", () => {
    // Arrange, Act.
    const start = startOf(sendMessageConverter.start(call({ to: "a1", message: "go" })));

    // Assert.
    expect(start.summary).toBeUndefined();
  });

  it("carries the body from the `message` argument", () => {
    // Arrange, Act.
    const start = startOf(sendMessageConverter.start(call({ to: "a1", message: "the whole relay" })));

    // Assert.
    expect(start.body?.text).toBe("the whole relay");
  });

  it("carries the body from the `body` argument when that is how it was spelled", () => {
    // Arrange, Act.
    const start = startOf(sendMessageConverter.start(call({ to: "a1", body: "the whole relay" })));

    // Assert.
    expect(start.body?.text).toBe("the whole relay");
  });

  it("carries an empty body rather than losing the send when no text was stated", () => {
    // Arrange, Act.
    const start = startOf(sendMessageConverter.start(call({ to: "a1" })));

    // Assert.
    expect(start.body?.text).toBe("");
  });

  it("carries an empty address rather than losing the send when nobody was named", () => {
    // Arrange, Act.
    const start = startOf(sendMessageConverter.start(call({ message: "go" })));

    // Assert.
    expect(start.addressedTo).toBe("");
  });

  it("stamps the issue instant", () => {
    // Arrange, Act.
    const start = startOf(sendMessageConverter.start(call({ to: "a1", message: "go" })));

    // Assert.
    expect(start.startedAt?.atMs).toBe(3_000n);
  });
});

describe("sendMessageConverter.settle", () => {
  it("resolves the corpus send's recipient from resumedAgentId", () => {
    // Arrange, Act.
    const success = successOf(
      sendMessageConverter.settle(call({ to: "a1", message: "go" }), outcome(toolUseResult("send_message"))),
    );

    // Assert.
    expect(agentRecipientOf(success)).toBe("acd910f5fefb75908");
  });

  it("reads the corpus send as a RESUMED recipient, the only structured discriminator", () => {
    // Arrange, Act.
    const success = successOf(
      sendMessageConverter.settle(call({ to: "a1", message: "go" }), outcome(toolUseResult("send_message"))),
    );

    // Assert.
    expect(success.delivery.case).toBe("resumedRecipient");
  });

  it("falls back to the pin's id for the recipient when no resume happened", () => {
    // Arrange.
    const structured = { success: true, pin: { id: "b7c", name: "vetter", ref: "21" } };

    // Act.
    const success = successOf(sendMessageConverter.settle(call({ to: "vetter" }), outcome(structured)));

    // Assert.
    expect(agentRecipientOf(success)).toBe("b7c");
  });

  it("reads a send with no resumedAgentId as QUEUED to a live recipient", () => {
    // Arrange.
    const structured = { success: true, pin: { id: "b7c" } };

    // Act.
    const success = successOf(sendMessageConverter.settle(call({ to: "vetter" }), outcome(structured)));

    // Assert.
    expect(success.delivery.case).toBe("queuedToLive");
  });

  it("stamps the settle instant", () => {
    // Arrange, Act.
    const success = successOf(
      sendMessageConverter.settle(call({ to: "a1" }), outcome(toolUseResult("send_message"))),
    );

    // Assert.
    expect(success.settledAt?.atMs).toBe(8_000n);
  });

  it("produces NO success frame when no recipient identity could be resolved", () => {
    // Arrange, Act, Assert.
    expect(
      sendMessageConverter.settle(call({ to: "vetter" }), outcome({ success: true })),
    ).toBeUndefined();
  });

  it("produces NO success frame when the result carried no typed output at all", () => {
    // Arrange, Act, Assert.
    expect(sendMessageConverter.settle(call({ to: "v" }), outcome("prose"))).toBeUndefined();
  });

  it("settles a vendor-marked error as the failure arm", () => {
    // Arrange, Act.
    const arm = armOf(sendMessageConverter.settle(call({ to: "v" }), outcome(undefined, true)));

    // Assert.
    expect(arm.case).toBe("failure");
  });

  // THE REFUSAL PROSE IS LOAD-BEARING. It is the only thing the vendor says
  // about a refused send — it declares no refusal code — and the frontend's
  // `refused` delivery arm draws it as the reason (feed.proto, landing 14).
  // Dropping it here would leave the surface with a refusal it cannot explain.
  it("carries the vendor's refusal prose into the failure's own content", () => {
    // Arrange.
    const prose = "The agent was stopped by the user.";
    const refused: ToolOutcome = {
      content: create(conversationv1.ToolResultContentSchema, {
        blocks: [{ block: { case: "text", value: { text: prose } } }],
      }),
      isError: true,
      structured: undefined,
      settledAtMs: 8_000,
    };

    // Act.
    const arm = armOf(sendMessageConverter.settle(call({ to: "v" }), refused));

    // Assert.
    const failure = arm.value as conversationv1.AgentSendMessageFailure;
    expect(failure.error?.content?.blocks[0]?.block.value).toMatchObject({ text: prose });
  });
});

// A SETTLED SEND STANDS ALONE. Its start and its settle upsert one unit, so a
// store keeping the latest frame replays the settle with no start beside it;
// the settle therefore restates the address and the summary from the call.
describe("sendMessageConverter.settle for a send to another Claude session", () => {
  // The vendor's cross-session result, as observed on the host: no pin, no
  // resumedAgentId, a delivery id, and the delivery spelled only in prose.
  const crossSession = {
    success: true,
    message: "\u201cReview the fix\u201d \u2192 uds:/tmp/cc-socks/25283.sock; queued there",
    display: "\u201cReview the fix\u201d \u2192 uds:/tmp/cc-socks/25283.sock",
    msg_id: "143abeca-81c5-4e2f-bfa8-ed17dbede26f",
  };

  it("states the peer-session recipient by the vendor's delivery id", () => {
    // Arrange, Act.
    const success = successOf(
      sendMessageConverter.settle(call({ to: "fix-flaky-3a", message: "go" }), outcome(crossSession)),
    );

    // Assert.
    expect(success.recipient.case === "peerSession" ? success.recipient.value.deliveryId?.value : undefined).toBe(
      "143abeca-81c5-4e2f-bfa8-ed17dbede26f",
    );
  });

  it("names no agent recipient for a cross-session send", () => {
    // Arrange, Act.
    const success = successOf(
      sendMessageConverter.settle(call({ to: "fix-flaky-3a", message: "go" }), outcome(crossSession)),
    );

    // Assert.
    expect(agentRecipientOf(success)).toBeUndefined();
  });

  it("reads a cross-session send as QUEUED to a live recipient", () => {
    // Arrange, Act.
    const success = successOf(
      sendMessageConverter.settle(call({ to: "fix-flaky-3a", message: "go" }), outcome(crossSession)),
    );

    // Assert.
    expect(success.delivery.case).toBe("queuedToLive");
  });

  it("restates the address the caller wrote on a cross-session success", () => {
    // Arrange, Act.
    const success = successOf(
      sendMessageConverter.settle(call({ to: "fix-flaky-3a", message: "go" }), outcome(crossSession)),
    );

    // Assert.
    expect(success.addressedTo).toBe("fix-flaky-3a");
  });

  it("prefers the agent recipient when a result names both an agent and a delivery", () => {
    // Arrange.
    const structured = { success: true, pin: { id: "b7c" }, msg_id: "m-1" };

    // Act.
    const success = successOf(sendMessageConverter.settle(call({ to: "vetter" }), outcome(structured)));

    // Assert.
    expect(agentRecipientOf(success)).toBe("b7c");
  });

  it("produces NO success frame for an empty delivery id with no agent", () => {
    // Arrange.
    const structured = { success: true, msg_id: "" };

    // Act.
    const item = sendMessageConverter.settle(call({ to: "fix-flaky-3a" }), outcome(structured));

    // Assert.
    expect(item).toBeUndefined();
  });
});

describe("sendMessageConverter.settle restates what the start carried", () => {
  function failureOf(
    item: conversationv1.AgentActivity["item"] | undefined,
  ): conversationv1.AgentSendMessageFailure {
    const arm = armOf(item);
    expect(arm.case).toBe("failure");
    return arm.value as conversationv1.AgentSendMessageFailure;
  }

  it("restates the address on the success arm", () => {
    // Arrange, Act.
    const success = successOf(
      sendMessageConverter.settle(call({ to: "vetter", message: "go" }), outcome(toolUseResult("send_message"))),
    );

    // Assert.
    expect(success.addressedTo).toBe("vetter");
  });

  it("restates the summary on the success arm", () => {
    // Arrange, Act.
    const success = successOf(
      sendMessageConverter.settle(
        call({ to: "vetter", message: "go", summary: "Scroll fix landed; merge master in" }),
        outcome(toolUseResult("send_message")),
      ),
    );

    // Assert.
    expect(success.summary?.text).toBe("Scroll fix landed; merge master in");
  });

  it("leaves the success arm's summary UNSET when the caller supplied none", () => {
    // Arrange, Act.
    const success = successOf(
      sendMessageConverter.settle(call({ to: "vetter", message: "go" }), outcome(toolUseResult("send_message"))),
    );

    // Assert.
    expect(success.summary).toBeUndefined();
  });

  it("restates the address on the failure arm", () => {
    // Arrange, Act.
    const failure = failureOf(
      sendMessageConverter.settle(call({ to: "vetter", message: "go" }), outcome(undefined, true)),
    );

    // Assert.
    expect(failure.addressedTo).toBe("vetter");
  });

  it("restates the summary on the failure arm", () => {
    // Arrange, Act.
    const failure = failureOf(
      sendMessageConverter.settle(
        call({ to: "vetter", message: "go", summary: "resume the vetting" }),
        outcome(undefined, true),
      ),
    );

    // Assert.
    expect(failure.summary?.text).toBe("resume the vetting");
  });

  it("leaves the failure arm's summary UNSET when the caller supplied none", () => {
    // Arrange, Act.
    const failure = failureOf(
      sendMessageConverter.settle(call({ to: "vetter", message: "go" }), outcome(undefined, true)),
    );

    // Assert.
    expect(failure.summary).toBeUndefined();
  });
});

describe("sendMessageConverter.progress", () => {
  it("relays the vendor's liveness beat as the unit's progress arm", () => {
    // Arrange, Act.
    const arm = armOf(sendMessageConverter.progress!(toolProgress(6_000)));

    // Assert.
    expect(arm).toEqual({ case: "progress", value: toolProgress(6_000) });
  });

  it("declares a progress arm, because AgentSendMessage carries the vendor's beat", () => {
    // Arrange, Act, Assert.
    expect(sendMessageConverter.carriesProgress).toBe(true);
  });
});
