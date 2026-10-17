/**
 * store/peer-recipient.ts — WHICH SESSION received one cross-session message,
 * as the store answers it (store.v1.GetPeerRecipient).
 *
 * The daemon names the session a send reached by the delivery id both ends of
 * the message share, and asks a shim (shim.v1.ResolvePeerDelivery) because the
 * store is not its to read. This is the one read of the store's delivery index.
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

const LOGGER = bindLog({ component: "shim-store-peer-recipient", operation: "shim.store.peer_recipient" });

/** What the store said about one delivery id. THE KIND IS THE ANSWER. */
export type PeerRecipientAnswer =
  | { readonly kind: "found"; readonly session: conversationv1.AgentId }
  | { readonly kind: "not_found" }
  | { readonly kind: "failed"; readonly detail: string };

/** What the lookup needs beyond the request. */
export interface PeerRecipientLookupOptions extends ReadRetryOptions {
  readonly client: StoreClient;
}

/** One attempt: the store's answer, or a thrown {@link PersistenceError}. */
async function lookupOnce(client: StoreClient, deliveryId: string): Promise<PeerRecipientAnswer> {
  let response: storev1.GetPeerRecipientResponse;
  try {
    response = await client.getPeerRecipient(
      create(storev1.GetPeerRecipientRequestSchema, {
        deliveryId: create(conversationv1.PeerDeliveryIdSchema, { value: deliveryId }),
      }),
    );
  } catch (error) {
    throw transportFailure(error);
  }
  const result = response.result;
  switch (result.case) {
    case "success": {
      const session = result.value.session;
      if (session === undefined || session.value === "") {
        throw new PersistenceError("invalid_request", "the store answered success naming no session");
      }
      return { kind: "found", session };
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
      throw new PersistenceError("store_unavailable", "the store answered GetPeerRecipient with no result arm set");
  }
}

/**
 * Ask the store which session holds the delivered copy of `deliveryId`.
 *
 * ON THE READ RETRY SCHEDULE, like every other store read. Whatever is left
 * after it is the `failed` answer, never a throw.
 */
export async function lookupPeerRecipient(
  options: PeerRecipientLookupOptions,
  deliveryId: string,
): Promise<PeerRecipientAnswer> {
  if (deliveryId === "") {
    return { kind: "failed", detail: "the lookup names no delivery id; the store is never asked an incomplete question" };
  }
  try {
    const answer = await readWithRetry("GetPeerRecipient", () => lookupOnce(options.client, deliveryId), options);
    LOGGER.debug(
      { delivery_id: deliveryId, answer: answer.kind === "found" ? `found ${answer.session.value}` : answer.kind },
      "the store answered a peer recipient lookup",
    );
    return answer;
  } catch (error) {
    const detail = error instanceof Error ? error.message : String(error);
    LOGGER.debug({ delivery_id: deliveryId, detail }, "the peer recipient lookup failed; the caller records the outcome");
    return { kind: "failed", detail };
  }
}
