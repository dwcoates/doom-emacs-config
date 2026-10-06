/**
 * store/detached-work.ts — WHAT KIND of detached work one unit left as, and
 * WHETHER the record already holds it as ended, as the store answers it.
 *
 * A task-stream message names its task, but only `task_started` states its
 * kind; a `task_notification` states none. The fold learns a kind from the
 * start it observed and forgets it once the task settles, so a shim that
 * restarted since, or a keep-alive rewind whose new vendor query re-reports an
 * old backgrounded shell as `stopped`, meets a notification it cannot type.
 * The run's record holds both its kind and its end, by the spawning call's
 * activity id (`store.v1.GetDetachedWork`); this is the one read of it.
 *
 * # Every outcome is an answer, and the caller records it
 *
 * Exactly as store/locator.ts: a failure is returned as a value, never
 * thrown, so the one site that owns its consequence writes the one record of
 * it. Its own lines are debug.
 *
 * # Unscoped, like the run settlements the sidecar reads
 *
 * The unit is the vendor's own call id, compared for equality; no two
 * sessions share one. An empty unit is refused here, before the store is asked.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { type conversationv1, storev1 } from "../proto.js";
import type { StoreClient } from "./client.js";
import { PersistenceError } from "./persistence.js";
import { readFailure, transportFailure } from "./reader.js";
import { readWithRetry, type ReadRetryOptions } from "./retry.js";

const LOGGER = bindLog({ component: "shim-store-detached-work", operation: "shim.store.detached-work" });

/** The kinds the record classifies. */
type RecordedWorkKind = "subagent" | "bash" | "workflow" | "monitor";

/** What the store said about the detached work one unit left. THE KIND IS THE ANSWER. */
export type DetachedWorkAnswer =
  | {
      readonly kind: "found";
      /**
       * The kind the record holds, or `undefined` when the record holds the
       * work with no classified kind (the `unstated` arm): never a guess.
       */
      readonly workKind: RecordedWorkKind | undefined;
      /** Whether the record holds the work as ended. */
      readonly ended: boolean;
    }
  | { readonly kind: "not_found" }
  | { readonly kind: "failed"; readonly detail: string };

/** One line naming an answer, for the record its caller writes. */
export function describeDetachedWorkAnswer(answer: DetachedWorkAnswer): string {
  switch (answer.kind) {
    case "found":
      return `found ${answer.ended ? "ended" : "live"} ${answer.workKind ?? "work of no recorded kind"}`;
    case "not_found":
      return "not_found: no detached work on record left the unit";
    case "failed":
      return `failed: ${answer.detail}`;
  }
}

/** The recorded kind an answer's kind arm names; `undefined` for `unstated`. */
function recordedKind(kind: storev1.GetDetachedWorkKind | undefined): RecordedWorkKind | undefined | "unset" {
  switch (kind?.kind.case) {
    case "subagent":
      return "subagent";
    case "bash":
      return "bash";
    case "workflow":
      return "workflow";
    case "monitor":
      return "monitor";
    case "unstated":
      return undefined;
    default:
      return "unset";
  }
}

/** What the lookup needs beyond the request. */
export interface DetachedWorkLookupOptions extends ReadRetryOptions {
  readonly client: StoreClient;
}

/** One attempt: the store's answer, or a thrown {@link PersistenceError}. */
async function lookupOnce(client: StoreClient, unit: conversationv1.AgentActivityId): Promise<DetachedWorkAnswer> {
  let response: storev1.GetDetachedWorkResponse;
  try {
    response = await client.getDetachedWork(create(storev1.GetDetachedWorkRequestSchema, { unit }));
  } catch (error) {
    throw transportFailure(error);
  }
  const result = response.result;
  switch (result.case) {
    case "success": {
      const workKind = recordedKind(result.value.kind);
      if (workKind === "unset") {
        throw new PersistenceError("store_unavailable", "the store answered success naming no kind arm");
      }
      const state = result.value.state.case;
      if (state !== "live" && state !== "ended") {
        throw new PersistenceError("store_unavailable", "the store answered success naming no state arm");
      }
      return { kind: "found", workKind, ended: state === "ended" };
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
      throw new PersistenceError("store_unavailable", "the store answered GetDetachedWork with no result arm set");
  }
}

/**
 * Ask the store what the record holds of the detached work that left `unit`.
 *
 * ON THE READ RETRY SCHEDULE, like every other store read. Whatever is left
 * after it is the `failed` answer, never a throw — see the header for why.
 */
export async function lookupDetachedWork(
  options: DetachedWorkLookupOptions,
  unit: conversationv1.AgentActivityId,
): Promise<DetachedWorkAnswer> {
  if (unit.value === "") {
    return { kind: "failed", detail: "the lookup names no unit; the store is never asked an incomplete question" };
  }
  try {
    const answer = await readWithRetry("GetDetachedWork", () => lookupOnce(options.client, unit), options);
    LOGGER.debug(
      { activity_id: unit.value, answer: describeDetachedWorkAnswer(answer) },
      "the store answered a detached work lookup",
    );
    return answer;
  } catch (error) {
    const detail = error instanceof Error ? error.message : String(error);
    LOGGER.debug(
      { activity_id: unit.value, detail },
      "the detached work lookup failed; the caller records the outcome",
    );
    return { kind: "failed", detail };
  }
}
