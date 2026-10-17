/**
 * store/peer-delivery.ts — the one read of the store's delivery index.
 *
 * Every outcome comes back as an answer, never a throw, so the caller that owns
 * the consequence writes the one record for it.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1, storev1 } from "../../src/proto.js";
import type { StoreClient } from "../../src/store/client.js";
import { lookupPeerDelivery } from "../../src/store/peer-delivery.js";

/** The schedule, shortened so the assertions are about behavior, not time. */
const RETRY = { backoffMs: [1], maxAttempts: 2 } as const;
const noSleep = (): Promise<void> => Promise.resolve();

/** A client answering only the peer delivery lookup, recording every request. */
function lookupClient(answer: () => Promise<storev1.GetPeerDeliveryResponse>): {
  client: StoreClient;
  requests: storev1.GetPeerDeliveryRequest[];
} {
  const requests: storev1.GetPeerDeliveryRequest[] = [];
  const refuse = (): never => {
    throw new Error("the peer delivery suite did not expect that call");
  };
  return {
    requests,
    client: {
      openAgentSession: refuse,
      watchAgentSession: refuse,
      watchBashRun: refuse,
      readAgentPage: refuse,
      getWorkflow: refuse,
      getSidecarCursors: refuse,
      getLiveWork: refuse,
      getAgentByVendorTask: refuse,
      getDetachedWork: refuse,
      writeBatch: refuse,
      getPeerDelivery: (request) => {
        requests.push(request);
        return answer();
      },
    },
  };
}

/** One end naming `session`, or not recorded for undefined. */
function end(session: string | undefined): storev1.GetPeerDeliveryEnd {
  return create(storev1.GetPeerDeliveryEndSchema, {
    end:
      session === undefined
        ? { case: "notRecorded", value: create(storev1.GetPeerDeliveryNotRecordedSchema, {}) }
        : { case: "session", value: create(conversationv1.AgentIdSchema, { value: session }) },
  });
}

const read = (sender: string | undefined, recipient: string | undefined): storev1.GetPeerDeliveryResponse =>
  create(storev1.GetPeerDeliveryResponseSchema, {
    result: {
      case: "success",
      value: create(storev1.GetPeerDeliverySuccessSchema, { sender: end(sender), recipient: end(recipient) }),
    },
  });

const failure = (kind: storev1.GetPeerDeliveryFailure["kind"]): storev1.GetPeerDeliveryResponse =>
  create(storev1.GetPeerDeliveryResponseSchema, {
    result: { case: "failure", value: create(storev1.GetPeerDeliveryFailureSchema, { detail: "no", kind }) },
  });

describe("lookupPeerDelivery", () => {
  it("answers the sending session the store names", async () => {
    // Arrange
    const { client } = lookupClient(() => Promise.resolve(read("session-a", "session-b")));

    // Act
    const answer = await lookupPeerDelivery({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect(answer.kind === "read" ? answer.sender?.value : "failed").toBe("session-a");
  });

  it("answers the receiving session the store names", async () => {
    // Arrange
    const { client } = lookupClient(() => Promise.resolve(read("session-a", "session-b")));

    // Act
    const answer = await lookupPeerDelivery({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect(answer.kind === "read" ? answer.recipient?.value : "failed").toBe("session-b");
  });

  it("answers an end no recorded session holds as undefined", async () => {
    // Arrange
    const { client } = lookupClient(() => Promise.resolve(read("session-a", undefined)));

    // Act
    const answer = await lookupPeerDelivery({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect(answer).toEqual({ kind: "read", sender: expect.objectContaining({ value: "session-a" }), recipient: undefined });
  });

  it("asks naming the delivery id verbatim", async () => {
    // Arrange
    const { client, requests } = lookupClient(() => Promise.resolve(read(undefined, undefined)));

    // Act
    await lookupPeerDelivery({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect(requests.map((r) => r.deliveryId?.value)).toEqual(["m-1"]);
  });

  it("answers failed after the retry schedule when the store's storage keeps failing", async () => {
    // Arrange
    const { client, requests } = lookupClient(() =>
      Promise.resolve(failure({ case: "storageFailure", value: create(storev1.GetPeerDeliveryStorageFailureSchema, {}) })),
    );

    // Act
    const answer = await lookupPeerDelivery({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect([answer.kind, requests.length]).toEqual(["failed", RETRY.maxAttempts]);
  });

  it("answers failed at once, never retried, when the store refuses the request as malformed", async () => {
    // Arrange
    const { client, requests } = lookupClient(() =>
      Promise.resolve(
        failure({ case: "invalidRequest", value: create(storev1.GetPeerDeliveryInvalidRequestSchema, { field: "delivery_id" }) }),
      ),
    );

    // Act
    const answer = await lookupPeerDelivery({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect([answer.kind, requests.length]).toEqual(["failed", 1]);
  });

  it("answers failed when the store cannot be reached", async () => {
    // Arrange
    const { client } = lookupClient(() => Promise.reject(new Error("connect ECONNREFUSED")));

    // Act
    const answer = await lookupPeerDelivery({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect(answer).toEqual({ kind: "failed", detail: "connect ECONNREFUSED" });
  });

  it("answers failed when an end names no session", async () => {
    // Arrange
    const { client } = lookupClient(() => Promise.resolve(read("", "session-b")));

    // Act
    const answer = await lookupPeerDelivery({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect(answer.kind).toBe("failed");
  });

  it("answers failed when an end states no arm", async () => {
    // Arrange
    const response = read("session-a", "session-b");
    if (response.result.case === "success") response.result.value.recipient = create(storev1.GetPeerDeliveryEndSchema, {});
    const { client } = lookupClient(() => Promise.resolve(response));

    // Act
    const answer = await lookupPeerDelivery({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect(answer.kind).toBe("failed");
  });

  it("answers failed without asking the store for an empty delivery id", async () => {
    // Arrange
    const { client, requests } = lookupClient(() => Promise.resolve(read(undefined, undefined)));

    // Act
    const answer = await lookupPeerDelivery({ client, retry: RETRY, sleep: noSleep }, "");

    // Assert
    expect([answer.kind, requests.length]).toEqual(["failed", 0]);
  });
});
