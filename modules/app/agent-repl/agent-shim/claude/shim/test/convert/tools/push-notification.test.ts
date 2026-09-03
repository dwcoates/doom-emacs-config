/**
 * The push-notification converter. The claim that matters is WHETHER THE VENDOR
 * DELIVERED: an agent that believes it notified an absent user, and did not,
 * left them waiting on nothing — so the presence of `disabledReason` picks the
 * not-sent arm and the reason vocabulary is the vendor's own closed set.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { toolResultText } from "../../../src/convert/entries.js";
import { pushNotificationConverter } from "../../../src/convert/tools/push-notification.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { conversationv1 } from "../../../src/proto.js";

const AGENT_ID = create(conversationv1.AgentIdSchema, { value: "session-1" });

function callWith(input: Record<string, unknown>): PendingCall {
  return {
    toolUseId: "toolu_push",
    toolName: "PushNotification",
    input,
    startedAtMs: 1_700_000_000_000,
    agentId: AGENT_ID,
  };
}

function outcomeWith(structured: unknown, isError = false): ToolOutcome {
  return {
    content: toolResultText("what the model was shown"),
    isError,
    structured,
    settledAtMs: 1_700_000_008_000,
  };
}

function pushOf(
  item: conversationv1.AgentActivity["item"] | undefined,
): conversationv1.AgentPushNotification {
  expect(item?.case).toBe("pushNotification");
  return item?.value as conversationv1.AgentPushNotification;
}

function startOf(call: PendingCall): conversationv1.AgentPushNotificationStart {
  const push = pushOf(pushNotificationConverter.start(call));
  expect(push.state.case).toBe("start");
  return push.state.value as conversationv1.AgentPushNotificationStart;
}

function successOf(structured: unknown): conversationv1.AgentPushNotificationSuccess {
  const push = pushOf(
    pushNotificationConverter.settle(callWith({ message: "m", status: "proactive" }), outcomeWith(structured))!,
  );
  expect(push.state.case).toBe("success");
  return push.state.value as conversationv1.AgentPushNotificationSuccess;
}

describe("pushNotificationConverter kind and arms", () => {
  it("declares the push_notification kind", () => {
    // Arrange, Act, Assert.
    expect(pushNotificationConverter.kind).toBe("push_notification");
  });

  it("carries NO progress, because AgentPushNotification declares no such arm", () => {
    // Arrange, Act, Assert.
    expect(pushNotificationConverter.carriesProgress).toBe(false);
  });
});

describe("pushNotificationConverter.start", () => {
  it("carries the agent's message verbatim", () => {
    // Arrange.
    const call = callWith({ message: "the build finished", status: "proactive" });

    // Act, Assert.
    expect(startOf(call).message).toBe("the build finished");
  });

  it("does NOT carry the input's constant status literal", () => {
    // Arrange.
    const call = callWith({ message: "m", status: "proactive" });

    // Act, Assert: a constant is not a fact.
    const serialized = JSON.stringify(startOf(call), (_key, value) =>
      typeof value === "bigint" ? value.toString() : value,
    );
    expect(serialized).not.toContain("proactive");
  });

  it("stamps the instant the call was issued", () => {
    // Arrange.
    const call = callWith({ message: "m", status: "proactive" });

    // Act, Assert.
    expect(startOf(call).startedAt?.atMs).toBe(1_700_000_000_000n);
  });

  it("still announces a send whose call carried no message", () => {
    // Arrange.
    const call = callWith({ status: "proactive" });

    // Act, Assert.
    expect(startOf(call).message).toBe("");
  });
});

describe("pushNotificationConverter.settle", () => {
  it("states a delivered notification with both transports the vendor reported", () => {
    // Arrange, Act.
    const success = successOf({ message: "m", pushSent: true, localSent: false });

    // Assert.
    expect(success.outcome).toEqual({
      case: "sent",
      value: create(conversationv1.AgentPushNotificationSentSchema, {
        pushSent: true,
        localSent: false,
      }),
    });
  });

  it("parses the vendor's ISO send instant into epoch millis", () => {
    // Arrange, Act.
    const success = successOf({ message: "m", pushSent: true, sentAt: "2026-08-29T12:00:00.000Z" });

    // Assert.
    expect((success.outcome.value as conversationv1.AgentPushNotificationSent).sentAtMs).toBe(
      BigInt(Date.parse("2026-08-29T12:00:00.000Z")),
    );
  });

  it("leaves the send instant UNSET when the vendor stated none", () => {
    // Arrange, Act: resumed sessions replay pre-sentAt outputs verbatim.
    const success = successOf({ message: "m", pushSent: true });

    // Assert.
    expect((success.outcome.value as conversationv1.AgentPushNotificationSent).sentAtMs).toBeUndefined();
  });

  it("leaves the send instant UNSET when the vendor's timestamp will not parse", () => {
    // Arrange, Act.
    const success = successOf({ message: "m", pushSent: true, sentAt: "not a timestamp" });

    // Assert.
    expect((success.outcome.value as conversationv1.AgentPushNotificationSent).sentAtMs).toBeUndefined();
  });

  it("picks the not-sent arm from the PRESENCE of a disabled reason", () => {
    // Arrange, Act.
    const success = successOf({ message: "m", disabledReason: "config_off" });

    // Assert.
    expect(success.outcome).toEqual({
      case: "notSent",
      value: create(conversationv1.AgentPushNotificationNotSentSchema, {
        reason: {
          case: "configOff",
          value: create(conversationv1.AgentPushNotificationConfigOffSchema, {}),
        },
      }),
    });
  });

  it("maps the vendor's user_present reason", () => {
    // Arrange, Act.
    const success = successOf({ message: "m", disabledReason: "user_present" });

    // Assert.
    expect(
      (success.outcome.value as conversationv1.AgentPushNotificationNotSent).reason.case,
    ).toBe("userPresent");
  });

  it("maps the vendor's no_transport reason", () => {
    // Arrange, Act.
    const success = successOf({ message: "m", disabledReason: "no_transport" });

    // Assert.
    expect(
      (success.outcome.value as conversationv1.AgentPushNotificationNotSent).reason.case,
    ).toBe("noTransport");
  });

  it("still states NOT SENT when the vendor's reason is outside its declared set", () => {
    // Arrange, Act: the delivery fact survives; only the reason is lost.
    const success = successOf({ message: "m", disabledReason: "some_new_reason" });

    // Assert.
    expect(success.outcome.case).toBe("notSent");
    expect(
      (success.outcome.value as conversationv1.AgentPushNotificationNotSent).reason.case,
    ).toBeUndefined();
  });

  it("stamps the settle instant on the success arm", () => {
    // Arrange, Act.
    const success = successOf({ message: "m", pushSent: true });

    // Assert.
    expect(success.settledAt?.atMs).toBe(1_700_000_008_000n);
  });

  it("produces NO frame when the settled call carried no typed output", () => {
    // Arrange, Act: sent and not-sent are opposite claims and nothing tells them apart.
    const item = pushNotificationConverter.settle(callWith({ message: "m" }), outcomeWith("just prose"));

    // Assert.
    expect(item).toBeUndefined();
  });

  it("settles an errored call as the failure arm", () => {
    // Arrange, Act.
    const push = pushOf(
      pushNotificationConverter.settle(callWith({ message: "m" }), outcomeWith(undefined, true))!,
    );

    // Assert.
    const failure = push.state.value as conversationv1.AgentPushNotificationFailure;
    expect(push.state.case).toBe("failure");
    expect(failure.error?.settledAt?.atMs).toBe(1_700_000_008_000n);
  });
});
