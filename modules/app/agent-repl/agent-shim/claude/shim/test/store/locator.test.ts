/**
 * store/locator.ts — the one read of the vendor task locator pairing.
 *
 * Every outcome comes back as an answer, never a throw, so the caller that owns
 * the consequence writes the one record for it.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1, storev1 } from "../../src/proto.js";
import type { StoreClient } from "../../src/store/client.js";
import { describeVendorTaskAnswer, lookupAgentByVendorTask } from "../../src/store/locator.js";

/** The schedule, shortened so the assertions are about behavior, not time. */
const RETRY = { backoffMs: [1], maxAttempts: 2 } as const;
const noSleep = (): Promise<void> => Promise.resolve();

const SESSION = create(conversationv1.AgentIdSchema, { value: "main-1" });

/** A client answering only the locator lookup, recording every request. */
function lookupClient(answer: () => Promise<storev1.GetAgentByVendorTaskResponse>): {
  client: StoreClient;
  requests: storev1.GetAgentByVendorTaskRequest[];
} {
  const requests: storev1.GetAgentByVendorTaskRequest[] = [];
  const refuse = (): never => {
    throw new Error("the locator suite did not expect that call");
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
      writeBatch: refuse,
      getAgentByVendorTask: (request) => {
        requests.push(request);
        return answer();
      },
    },
  };
}

const found = (agent: string): storev1.GetAgentByVendorTaskResponse =>
  create(storev1.GetAgentByVendorTaskResponseSchema, {
    result: {
      case: "success",
      value: create(storev1.GetAgentByVendorTaskSuccessSchema, {
        agent: create(conversationv1.AgentIdSchema, { value: agent }),
      }),
    },
  });

const failure = (
  kind: storev1.GetAgentByVendorTaskFailure["kind"],
): storev1.GetAgentByVendorTaskResponse =>
  create(storev1.GetAgentByVendorTaskResponseSchema, {
    result: { case: "failure", value: create(storev1.GetAgentByVendorTaskFailureSchema, { detail: "no", kind }) },
  });

describe("lookupAgentByVendorTask", () => {
  it("answers found with the commission the store recorded", async () => {
    // Arrange
    const response = found("toolu_spawn");
    if (response.result.case === "success") {
      response.result.value.commission = create(conversationv1.AgentSubagentPromptSchema, { text: "go", description: "fix the shim" });
    }
    const { client } = lookupClient(() => Promise.resolve(response));

    // Act
    const answer = await lookupAgentByVendorTask({ client, retry: RETRY, sleep: noSleep }, SESSION, "a1b2");

    // Assert
    expect(answer.kind === "found" ? answer.commission?.description : "not found").toBe("fix the shim");
  });

  it("answers found with no commission when the store recorded none", async () => {
    // Arrange
    const { client } = lookupClient(() => Promise.resolve(found("toolu_spawn")));

    // Act
    const answer = await lookupAgentByVendorTask({ client, retry: RETRY, sleep: noSleep }, SESSION, "a1b2");

    // Assert
    expect(answer.kind === "found" ? answer.commission : "not found").toBeUndefined();
  });

  it("answers found with the agent the store names", async () => {
    // Arrange
    const { client } = lookupClient(() => Promise.resolve(found("toolu_spawn")));

    // Act
    const answer = await lookupAgentByVendorTask({ client, retry: RETRY, sleep: noSleep }, SESSION, "a1b2");

    // Assert
    expect(answer).toMatchObject({ kind: "found", agent: { value: "toolu_spawn" } });
  });

  it("asks scoped to the session, naming the locator", async () => {
    // Arrange
    const { client, requests } = lookupClient(() => Promise.resolve(found("toolu_spawn")));

    // Act
    await lookupAgentByVendorTask({ client, retry: RETRY, sleep: noSleep }, SESSION, "a1b2");

    // Assert
    expect(requests.map((r) => [r.session?.value, r.vendorTaskId])).toEqual([["main-1", "a1b2"]]);
  });

  it("answers not_found when the store holds no pairing", async () => {
    // Arrange
    const { client } = lookupClient(() =>
      Promise.resolve(
        create(storev1.GetAgentByVendorTaskResponseSchema, {
          result: { case: "notFound", value: create(storev1.GetAgentByVendorTaskNotFoundSchema, {}) },
        }),
      ),
    );

    // Act
    const answer = await lookupAgentByVendorTask({ client, retry: RETRY, sleep: noSleep }, SESSION, "a1b2");

    // Assert
    expect(answer).toEqual({ kind: "not_found" });
  });

  it("answers failed after the retry schedule when the store's storage keeps failing", async () => {
    // Arrange
    const { client, requests } = lookupClient(() =>
      Promise.resolve(
        failure({ case: "storageFailure", value: create(storev1.GetAgentByVendorTaskStorageFailureSchema, {}) }),
      ),
    );

    // Act
    const answer = await lookupAgentByVendorTask({ client, retry: RETRY, sleep: noSleep }, SESSION, "a1b2");

    // Assert
    expect([answer.kind, requests.length]).toEqual(["failed", RETRY.maxAttempts]);
  });

  it("answers failed at once, never retried, when the store refuses the request as malformed", async () => {
    // Arrange
    const { client, requests } = lookupClient(() =>
      Promise.resolve(
        failure({
          case: "invalidRequest",
          value: create(storev1.GetAgentByVendorTaskInvalidRequestSchema, { field: "vendor_task_id" }),
        }),
      ),
    );

    // Act
    const answer = await lookupAgentByVendorTask({ client, retry: RETRY, sleep: noSleep }, SESSION, "a1b2");

    // Assert
    expect([answer.kind, requests.length]).toEqual(["failed", 1]);
  });

  it("answers failed when the store cannot be reached", async () => {
    // Arrange
    const { client } = lookupClient(() => Promise.reject(new Error("connect ECONNREFUSED")));

    // Act
    const answer = await lookupAgentByVendorTask({ client, retry: RETRY, sleep: noSleep }, SESSION, "a1b2");

    // Assert
    expect(answer).toEqual({ kind: "failed", detail: "connect ECONNREFUSED" });
  });

  it("answers failed when the store's success names no agent", async () => {
    // Arrange
    const { client } = lookupClient(() => Promise.resolve(found("")));

    // Act
    const answer = await lookupAgentByVendorTask({ client, retry: RETRY, sleep: noSleep }, SESSION, "a1b2");

    // Assert
    expect(answer.kind).toBe("failed");
  });

  it.each([
    { name: "no session", session: "", locator: "a1b2" },
    { name: "no locator", session: "main-1", locator: "" },
  ])("answers failed without asking the store for $name", async ({ session, locator }) => {
    // Arrange
    const { client, requests } = lookupClient(() => Promise.resolve(found("toolu_spawn")));

    // Act
    const answer = await lookupAgentByVendorTask(
      { client, retry: RETRY, sleep: noSleep },
      create(conversationv1.AgentIdSchema, { value: session }),
      locator,
    );

    // Assert
    expect([answer.kind, requests.length]).toEqual(["failed", 0]);
  });
});

describe("describeVendorTaskAnswer", () => {
  it.each([
    {
      answer: {
        kind: "found",
        agent: create(conversationv1.AgentIdSchema, { value: "toolu_x" }),
        commission: create(conversationv1.AgentSubagentPromptSchema, { text: "go" }),
      },
      text: "found toolu_x",
    },
    {
      answer: { kind: "found", agent: create(conversationv1.AgentIdSchema, { value: "toolu_x" }), commission: undefined },
      text: "found toolu_x, with no recorded commission",
    },
    { answer: { kind: "not_found" }, text: "not_found: no agent of this session's lineage is paired with the locator" },
    { answer: { kind: "failed", detail: "boom" }, text: "failed: boom" },
  ] as const)("names the $answer.kind answer as $text", ({ answer, text }) => {
    // Arrange, Act, Assert
    expect(describeVendorTaskAnswer(answer)).toBe(text);
  });
});
