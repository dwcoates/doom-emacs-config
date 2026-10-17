/**
 * store/peer-delivery.ts — WHICH SESSIONS are the two ends of one cross-session
 * message, as the store answers it (store.v1.GetPeerDelivery).
 *
 * The daemon names the session at the other end of a cross-session message by
 * the delivery id both ends' records share, and asks a shim
 * (shim.v1.ResolvePeerDelivery) because the store is not its to read. This is
 * the one read of the store's delivery index.
 *
 * # Every outcome is an answer, and the caller records it
 *
 * As in store/locator.ts: a failure comes back as a value, and the caller that
 * owns its consequence writes the one record for it. Its own lines are debug.
 *
 * # Unscoped, unlike every other session read
 *
 * The answer is always ANOTHER session, so naming this conversation's lineage
 * would refuse every answer the read exists to give.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1, storev1 } from "../proto.js";
import type { StoreClient } from "./client.js";
import { PersistenceError } from "./persistence.js";
import { readFailure, transportFailure } from "./reader.js";
import { readWithRetry, type ReadRetryOptions } from "./retry.js";

const LOGGER = bindLog({ component: "shim-store-peer-delivery", operation: "shim.store.peer_delivery" });

/**
 * One end of the message: the session that holds its record, or undefined when
 * no recorded session holds it.
 */
export type PeerDeliveryEnd = conversationv1.AgentId | undefined;

/** What the store said about one delivery id. THE KIND IS THE ANSWER. */
export type PeerDeliveryAnswer =
  | { readonly kind: "read"; readonly sender: PeerDeliveryEnd; readonly recipient: PeerDeliveryEnd }
  | { readonly kind: "failed"; readonly detail: string };

/** What the lookup needs beyond the request. */
export interface PeerDeliveryLookupOptions extends ReadRetryOptions {
  readonly client: StoreClient;
}

/** One end as the store answered it; an end naming no session is a store defect. */
function endOf(end: storev1.GetPeerDeliveryEnd | undefined, which: string): PeerDeliveryEnd {
  switch (end?.end.case) {
    case "session":
      if (end.end.value.value === "") {
        throw new PersistenceError("invalid_request", `the store answered the ${which} end naming no session`);
      }
      return end.end.value;
    case "notRecorded":
      return undefined;
    default:
      throw new PersistenceError("invalid_request", `the store answered the ${which} end with no arm set`);
  }
}

/** One attempt: the store's answer, or a thrown {@link PersistenceError}. */
async function lookupOnce(client: StoreClient, deliveryId: string): Promise<PeerDeliveryAnswer> {
  let response: storev1.GetPeerDeliveryResponse;
  try {
    response = await client.getPeerDelivery(
      create(storev1.GetPeerDeliveryRequestSchema, {
        deliveryId: create(conversationv1.PeerDeliveryIdSchema, { value: deliveryId }),
      }),
    );
  } catch (error) {
    throw transportFailure(error);
  }
  const result = response.result;
  switch (result.case) {
    case "success":
      return {
        kind: "read",
        sender: endOf(result.value.sender, "sender"),
        recipient: endOf(result.value.recipient, "recipient"),
      };
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
      throw new PersistenceError("store_unavailable", "the store answered GetPeerDelivery with no result arm set");
  }
}

/**
 * Ask the store which sessions hold the two ends of `deliveryId`.
 *
 * ON THE READ RETRY SCHEDULE, like every other store read. Whatever is left
 * after it is the `failed` answer, never a throw.
 */
export async function lookupPeerDelivery(
  options: PeerDeliveryLookupOptions,
  deliveryId: string,
): Promise<PeerDeliveryAnswer> {
  if (deliveryId === "") {
    return { kind: "failed", detail: "the lookup names no delivery id; the store is never asked an incomplete question" };
  }
  try {
    const answer = await readWithRetry("GetPeerDelivery", () => lookupOnce(options.client, deliveryId), options);
    LOGGER.debug(
      {
        delivery_id: deliveryId,
        sender: answer.kind === "read" ? (answer.sender?.value ?? "not recorded") : "",
        recipient: answer.kind === "read" ? (answer.recipient?.value ?? "not recorded") : "",
      },
      "the store answered a peer delivery lookup",
    );
    return answer;
  } catch (error) {
    const detail = error instanceof Error ? error.message : String(error);
    LOGGER.debug({ delivery_id: deliveryId, detail }, "the peer delivery lookup failed; the caller records the outcome");
    return { kind: "failed", detail };
  }
}
