/**
 * store/locator.ts — WHICH AGENT a vendor task locator names, as the store
 * answers it.
 *
 * A subagent's vendor task id is the agent's own LOCATOR (the `<id>` of
 * `agent-<id>.jsonl`, `task_started.task_id`), and it comes back when the agent
 * is RESUMED — but the resume's own call is the `SendMessage` that woke it, not
 * the spawn whose `tool_use_id` IS the agent under the cross-plane minting
 * rule. A shim that restarted since the spawn has nothing on the stream to name
 * the agent by. The sidecar reads both halves of the pairing from the vendor's
 * own files and writes them with the agent's rows; this is the one read of it.
 * The shim never reads a vendor file itself.
 *
 * # Every outcome is an answer, and the caller records it
 *
 * The three callers — the announcement of a resumed agent, the restore that
 * re-announces one, and the permission gate crediting its asks — each know what
 * a miss MEANS for them, and each writes the one record for its own outcome
 * (INFO found, ERROR not-found or failed). So this module returns a failure as
 * a value rather than recording it here too: an error entered the log once, at
 * the site that owns its consequence. Its own lines are debug.
 *
 * # Scoped, like every session read
 *
 * The store is shared by every session on the host, so the lookup names this
 * conversation's main agent and the store answers only that agent's lineage.
 * An empty session or locator is refused here, before the store is asked.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { type conversationv1, storev1 } from "../proto.js";
import type { StoreClient } from "./client.js";
import { PersistenceError } from "./persistence.js";
import { readFailure, transportFailure } from "./reader.js";
import { readWithRetry, type ReadRetryOptions } from "./retry.js";

const LOGGER = bindLog({ component: "shim-store-locator", operation: "shim.store.locator" });

/** What the store said about one vendor task locator. THE KIND IS THE ANSWER. */
export type VendorTaskAnswer =
  | {
      readonly kind: "found";
      readonly agent: conversationv1.AgentId;
      /**
       * What the agent was commissioned with, as the store recorded it from its
       * spawn start (`GetAgentByVendorTaskSuccess.commission`); `undefined` when
       * the record holds the agent's lineage but no start.
       */
      readonly commission: conversationv1.AgentSubagentPrompt | undefined;
    }
  | { readonly kind: "not_found" }
  | { readonly kind: "failed"; readonly detail: string };

/** One line naming an answer, for the record its caller writes. */
export function describeVendorTaskAnswer(answer: VendorTaskAnswer): string {
  switch (answer.kind) {
    case "found":
      return answer.commission === undefined
        ? `found ${answer.agent.value}, with no recorded commission`
        : `found ${answer.agent.value}`;
    case "not_found":
      return "not_found: no agent of this session's lineage is paired with the locator";
    case "failed":
      return `failed: ${answer.detail}`;
  }
}

/** What the lookup needs beyond the request. */
export interface VendorTaskLookupOptions extends ReadRetryOptions {
  readonly client: StoreClient;
}

/** One attempt: the store's answer, or a thrown {@link PersistenceError}. */
async function lookupOnce(
  client: StoreClient,
  session: conversationv1.AgentId,
  vendorTaskId: string,
): Promise<VendorTaskAnswer> {
  let response: storev1.GetAgentByVendorTaskResponse;
  try {
    response = await client.getAgentByVendorTask(
      create(storev1.GetAgentByVendorTaskRequestSchema, { session, vendorTaskId }),
    );
  } catch (error) {
    throw transportFailure(error);
  }
  const result = response.result;
  switch (result.case) {
    case "success": {
      const agent = result.value.agent;
      if (agent === undefined || agent.value === "") {
        throw new PersistenceError("invalid_request", "the store answered success naming no agent");
      }
      return { kind: "found", agent, commission: result.value.commission };
    }
    case "notFound":
      return { kind: "not_found" };
    case "failure":
      if (result.value.kind.case === "invalidRequest") {
        // A REQUEST THIS PROCESS BUILT, REFUSED: a shim defect, never retried.
        throw new PersistenceError(
          "invalid_request",
          `the store refused the lookup as malformed (field ${result.value.kind.value.field}): ${result.value.detail}`,
        );
      }
      throw readFailure(result.value);
    default:
      throw new PersistenceError("store_unavailable", "the store answered GetAgentByVendorTask with no result arm set");
  }
}

/**
 * Ask the store which agent of `session`'s lineage `vendorTaskId` names.
 *
 * ON THE READ RETRY SCHEDULE, like every other store read: a busy database is
 * a condition another attempt can change. Whatever is left after it is the
 * `failed` answer, never a throw — see the header for why.
 */
export async function lookupAgentByVendorTask(
  options: VendorTaskLookupOptions,
  session: conversationv1.AgentId,
  vendorTaskId: string,
): Promise<VendorTaskAnswer> {
  if (session.value === "" || vendorTaskId === "") {
    return {
      kind: "failed",
      detail: `the lookup names ${session.value === "" ? "no session" : "no vendor task locator"}; the store is never asked an incomplete question`,
    };
  }
  try {
    const answer = await readWithRetry(
      "GetAgentByVendorTask",
      () => lookupOnce(options.client, session, vendorTaskId),
      options,
    );
    LOGGER.debug(
      { agent: session.value, task_id: vendorTaskId, answer: describeVendorTaskAnswer(answer) },
      "the store answered a vendor task locator lookup",
    );
    return answer;
  } catch (error) {
    const detail = error instanceof Error ? error.message : String(error);
    LOGGER.debug(
      { agent: session.value, task_id: vendorTaskId, detail },
      "the vendor task locator lookup failed; the caller records the outcome",
    );
    return { kind: "failed", detail };
  }
}
