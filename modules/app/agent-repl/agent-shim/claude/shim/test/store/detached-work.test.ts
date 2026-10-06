/**
 * store/detached-work.ts — the one read of what the record holds of the
 * detached work one unit left.
 *
 * Every outcome comes back as an answer, never a throw, so the caller that owns
 * the consequence writes the one record for it.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1, storev1 } from "../../src/proto.js";
import type { StoreClient } from "../../src/store/client.js";
import { describeDetachedWorkAnswer, lookupDetachedWork } from "../../src/store/detached-work.js";

/** The schedule, shortened so the assertions are about behavior, not time. */
const RETRY = { backoffMs: [1], maxAttempts: 2 } as const;
const noSleep = (): Promise<void> => Promise.resolve();

const UNIT = create(conversationv1.AgentActivityIdSchema, { value: "toolu_run" });

/** A client answering only the detached-work lookup, recording every request. */
function lookupClient(answer: () => Promise<storev1.GetDetachedWorkResponse>): {
  client: StoreClient;
  requests: storev1.GetDetachedWorkRequest[];
} {
  const requests: storev1.GetDetachedWorkRequest[] = [];
  const refuse = (): never => {
    throw new Error("the detached-work suite did not expect that call");
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
      writeBatch: refuse,
      getDetachedWork: (request) => {
        requests.push(request);
        return answer();
      },
    },
  };
}

/** A success naming `kind` and `state`, either left unset by passing `undefined`. */
function success(
  kind: storev1.GetDetachedWorkKind["kind"] | undefined,
  state: storev1.GetDetachedWorkSuccess["state"] | undefined,
): storev1.GetDetachedWorkResponse {
  return create(storev1.GetDetachedWorkResponseSchema, {
    result: {
      case: "success",
      value: create(storev1.GetDetachedWorkSuccessSchema, {
        ...(kind === undefined ? {} : { kind: create(storev1.GetDetachedWorkKindSchema, { kind }) }),
        ...(state === undefined ? {} : { state }),
      }),
    },
  });
}

const BASH: storev1.GetDetachedWorkKind["kind"] = { case: "bash", value: create(storev1.GetDetachedWorkKindBashSchema, {}) };
const ENDED: storev1.GetDetachedWorkSuccess["state"] = {
  case: "ended",
  value: create(storev1.GetDetachedWorkEndedSchema, { endedAtMs: 5n }),
};
const LIVE: storev1.GetDetachedWorkSuccess["state"] = { case: "live", value: create(storev1.GetDetachedWorkLiveSchema, {}) };

const failure = (kind: storev1.GetDetachedWorkFailure["kind"]): storev1.GetDetachedWorkResponse =>
  create(storev1.GetDetachedWorkResponseSchema, {
    result: { case: "failure", value: create(storev1.GetDetachedWorkFailureSchema, { detail: "no", kind }) },
  });

describe("lookupDetachedWork", () => {
  it.each([
    {
      name: "an ended bash run",
      response: success(BASH, ENDED),
      answer: { kind: "found", workKind: "bash", ended: true },
    },
    {
      name: "a live bash run",
      response: success(BASH, LIVE),
      answer: { kind: "found", workKind: "bash", ended: false },
    },
    {
      name: "a run of each other recorded kind",
      response: success({ case: "monitor", value: create(storev1.GetDetachedWorkKindMonitorSchema, {}) }, LIVE),
      answer: { kind: "found", workKind: "monitor", ended: false },
    },
    {
      name: "a run of no recorded kind",
      response: success({ case: "unstated", value: create(storev1.GetDetachedWorkKindUnstatedSchema, {}) }, ENDED),
      answer: { kind: "found", workKind: undefined, ended: true },
    },
  ])("answers found for $name", async ({ response, answer }) => {
    // Arrange
    const { client } = lookupClient(() => Promise.resolve(response));

    // Act
    const got = await lookupDetachedWork({ client, retry: RETRY, sleep: noSleep }, UNIT);

    // Assert
    expect(got).toEqual(answer);
  });

  it("asks by the unit", async () => {
    // Arrange
    const { client, requests } = lookupClient(() => Promise.resolve(success(BASH, ENDED)));

    // Act
    await lookupDetachedWork({ client, retry: RETRY, sleep: noSleep }, UNIT);

    // Assert
    expect(requests.map((r) => r.unit?.value)).toEqual(["toolu_run"]);
  });

  it("answers not_found when the record holds no work for the unit", async () => {
    // Arrange
    const { client } = lookupClient(() =>
      Promise.resolve(
        create(storev1.GetDetachedWorkResponseSchema, {
          result: { case: "notFound", value: create(storev1.GetDetachedWorkNotFoundSchema, {}) },
        }),
      ),
    );

    // Act
    const answer = await lookupDetachedWork({ client, retry: RETRY, sleep: noSleep }, UNIT);

    // Assert
    expect(answer).toEqual({ kind: "not_found" });
  });

  it("answers failed after the retry schedule when the store's storage keeps failing", async () => {
    // Arrange
    const { client, requests } = lookupClient(() =>
      Promise.resolve(failure({ case: "storageFailure", value: create(storev1.GetDetachedWorkStorageFailureSchema, {}) })),
    );

    // Act
    const answer = await lookupDetachedWork({ client, retry: RETRY, sleep: noSleep }, UNIT);

    // Assert
    expect([answer.kind, requests.length]).toEqual(["failed", RETRY.maxAttempts]);
  });

  it("answers failed at once, never retried, when the store refuses the request as malformed", async () => {
    // Arrange
    const { client, requests } = lookupClient(() =>
      Promise.resolve(
        failure({ case: "invalidRequest", value: create(storev1.GetDetachedWorkInvalidRequestSchema, { field: "unit" }) }),
      ),
    );

    // Act
    const answer = await lookupDetachedWork({ client, retry: RETRY, sleep: noSleep }, UNIT);

    // Assert
    expect([answer.kind, requests.length]).toEqual(["failed", 1]);
  });

  it("answers failed when the store cannot be reached", async () => {
    // Arrange
    const { client } = lookupClient(() => Promise.reject(new Error("connect ECONNREFUSED")));

    // Act
    const answer = await lookupDetachedWork({ client, retry: RETRY, sleep: noSleep }, UNIT);

    // Assert
    expect(answer).toEqual({ kind: "failed", detail: "connect ECONNREFUSED" });
  });

  it("answers failed when the store's success names no kind arm", async () => {
    // Arrange
    const { client } = lookupClient(() => Promise.resolve(success(undefined, ENDED)));

    // Act
    const answer = await lookupDetachedWork({ client, retry: RETRY, sleep: noSleep }, UNIT);

    // Assert
    expect(answer.kind).toBe("failed");
  });

  it("answers failed when the store's success names no state arm", async () => {
    // Arrange
    const { client } = lookupClient(() => Promise.resolve(success(BASH, undefined)));

    // Act
    const answer = await lookupDetachedWork({ client, retry: RETRY, sleep: noSleep }, UNIT);

    // Assert
    expect(answer.kind).toBe("failed");
  });

  it("answers failed without asking the store for an empty unit", async () => {
    // Arrange
    const { client, requests } = lookupClient(() => Promise.resolve(success(BASH, ENDED)));

    // Act
    const answer = await lookupDetachedWork(
      { client, retry: RETRY, sleep: noSleep },
      create(conversationv1.AgentActivityIdSchema, { value: "" }),
    );

    // Assert
    expect([answer.kind, requests.length]).toEqual(["failed", 0]);
  });
});

describe("describeDetachedWorkAnswer", () => {
  it.each([
    { answer: { kind: "found", workKind: "bash", ended: true }, text: "found ended bash" },
    { answer: { kind: "found", workKind: "subagent", ended: false }, text: "found live subagent" },
    { answer: { kind: "found", workKind: undefined, ended: true }, text: "found ended work of no recorded kind" },
    { answer: { kind: "not_found" }, text: "not_found: no detached work on record left the unit" },
    { answer: { kind: "failed", detail: "boom" }, text: "failed: boom" },
  ] as const)("names the answer as $text", ({ answer, text }) => {
    // Arrange, Act, Assert
    expect(describeDetachedWorkAnswer(answer)).toBe(text);
  });
});
