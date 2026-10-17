/**
 * store/peer-recipient.ts — the one read of the store's delivery index.
 *
 * Every outcome comes back as an answer, never a throw, so the caller that owns
 * the consequence writes the one record for it.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1, storev1 } from "../../src/proto.js";
import type { StoreClient } from "../../src/store/client.js";
import { lookupPeerRecipient } from "../../src/store/peer-recipient.js";

/** The schedule, shortened so the assertions are about behavior, not time. */
const RETRY = { backoffMs: [1], maxAttempts: 2 } as const;
const noSleep = (): Promise<void> => Promise.resolve();

/** A client answering only the peer recipient lookup, recording every request. */
function lookupClient(answer: () => Promise<storev1.GetPeerRecipientResponse>): {
  client: StoreClient;
  requests: storev1.GetPeerRecipientRequest[];
} {
  const requests: storev1.GetPeerRecipientRequest[] = [];
  const refuse = (): never => {
    throw new Error("the peer recipient suite did not expect that call");
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
      getPeerRecipient: (request) => {
        requests.push(request);
        return answer();
      },
    },
  };
}

const found = (session: string): storev1.GetPeerRecipientResponse =>
  create(storev1.GetPeerRecipientResponseSchema, {
    result: {
      case: "success",
      value: create(storev1.GetPeerRecipientSuccessSchema, {
        session: create(conversationv1.AgentIdSchema, { value: session }),
      }),
    },
  });

const failure = (kind: storev1.GetPeerRecipientFailure["kind"]): storev1.GetPeerRecipientResponse =>
  create(storev1.GetPeerRecipientResponseSchema, {
    result: { case: "failure", value: create(storev1.GetPeerRecipientFailureSchema, { detail: "no", kind }) },
  });

describe("lookupPeerRecipient", () => {
  it("answers found with the session the store names", async () => {
    // Arrange
    const { client } = lookupClient(() => Promise.resolve(found("vendor-session-b")));

    // Act
    const answer = await lookupPeerRecipient({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect(answer).toMatchObject({ kind: "found", session: { value: "vendor-session-b" } });
  });

  it("asks naming the delivery id verbatim", async () => {
    // Arrange
    const { client, requests } = lookupClient(() => Promise.resolve(found("vendor-session-b")));

    // Act
    await lookupPeerRecipient({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect(requests.map((r) => r.deliveryId?.value)).toEqual(["m-1"]);
  });

  it("answers not_found when no session holds the delivery", async () => {
    // Arrange
    const { client } = lookupClient(() =>
      Promise.resolve(
        create(storev1.GetPeerRecipientResponseSchema, {
          result: { case: "notFound", value: create(storev1.GetPeerRecipientNotFoundSchema, {}) },
        }),
      ),
    );

    // Act
    const answer = await lookupPeerRecipient({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect(answer).toEqual({ kind: "not_found" });
  });

  it("answers failed after the retry schedule when the store's storage keeps failing", async () => {
    // Arrange
    const { client, requests } = lookupClient(() =>
      Promise.resolve(failure({ case: "storageFailure", value: create(storev1.GetPeerRecipientStorageFailureSchema, {}) })),
    );

    // Act
    const answer = await lookupPeerRecipient({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect([answer.kind, requests.length]).toEqual(["failed", RETRY.maxAttempts]);
  });

  it("answers failed at once, never retried, when the store refuses the request as malformed", async () => {
    // Arrange
    const { client, requests } = lookupClient(() =>
      Promise.resolve(
        failure({
          case: "invalidRequest",
          value: create(storev1.GetPeerRecipientInvalidRequestSchema, { field: "delivery_id" }),
        }),
      ),
    );

    // Act
    const answer = await lookupPeerRecipient({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect([answer.kind, requests.length]).toEqual(["failed", 1]);
  });

  it("answers failed when the store cannot be reached", async () => {
    // Arrange
    const { client } = lookupClient(() => Promise.reject(new Error("connect ECONNREFUSED")));

    // Act
    const answer = await lookupPeerRecipient({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect(answer).toEqual({ kind: "failed", detail: "connect ECONNREFUSED" });
  });

  it("answers failed when the store's success names no session", async () => {
    // Arrange
    const { client } = lookupClient(() => Promise.resolve(found("")));

    // Act
    const answer = await lookupPeerRecipient({ client, retry: RETRY, sleep: noSleep }, "m-1");

    // Assert
    expect(answer.kind).toBe("failed");
  });

  it("answers failed without asking the store for an empty delivery id", async () => {
    // Arrange
    const { client, requests } = lookupClient(() => Promise.resolve(found("vendor-session-b")));

    // Act
    const answer = await lookupPeerRecipient({ client, retry: RETRY, sleep: noSleep }, "");

    // Assert
    expect([answer.kind, requests.length]).toEqual(["failed", 0]);
  });
});
