/**
 * engine/network-reach.ts — whether the vendor's answers say this machine can
 * reach the network, read off every SDK message.
 *
 * THE SHIM TELLS AN UNREACHABLE NETWORK APART FROM AN ANSWER THE VENDOR GAVE
 * (owner ruling, 2026-10-02; `conversation.v1.SessionFaultNetworkUnreachable`).
 * A message that FAILED below any answer — an error notice whose words name an
 * outage, a result that ended with no HTTP status and outage words — says the
 * network is unreachable. A message the API PRODUCED — a model response, a
 * stream event, any HTTP status, a refusal with a vendor error class — says it
 * is reachable: the request got there. Everything else says nothing.
 *
 * THE ONE CLASSIFIER: every judgment here goes through
 * {@link classifyAgentFailure}, the same one a failed start and a background
 * agent's failure are read by.
 */
import type { SdkMessage } from "../sdk/types.js";
import { classifyAgentFailure, noticeText } from "./network-resume.js";

/** What one message says about the network. */
export type NetworkReach =
  | { readonly kind: "unreachable"; readonly detail: string }
  | { readonly kind: "reached" };

/**
 * Read one SDK message's account of the network, or `undefined` when it gives
 * none. A synthetic notice (`model: "<synthetic>"`) is the vendor's own prose,
 * never a response the API produced, so it can only say "unreachable".
 */
export function networkReach(message: SdkMessage): NetworkReach | undefined {
  switch (message.type) {
    case "stream_event":
      return { kind: "reached" };
    case "assistant": {
      const text = noticeText(message.message);
      if (message.error === undefined) {
        return message.message.model === "<synthetic>" ? undefined : { kind: "reached" };
      }
      const verdict = classifyAgentFailure({ errorClass: message.error, ...(text === undefined ? {} : { text }) });
      if (verdict.network) return { kind: "unreachable", detail: text ?? verdict.detail };
      return verdict.basis === "error_class" ? { kind: "reached" } : undefined;
    }
    case "result": {
      const { api_error_status: status } = message as unknown as { api_error_status?: number | null };
      if (typeof status === "number" || !message.is_error) return { kind: "reached" };
      const words = message.subtype === "success" ? message.result : message.errors.join("; ");
      const verdict = classifyAgentFailure({ text: words });
      return verdict.network ? { kind: "unreachable", detail: words } : undefined;
    }
    case "system":
      if (message.subtype === "api_retry") {
        if (typeof message.error_status === "number") return { kind: "reached" };
        const verdict = classifyAgentFailure({ errorClass: message.error });
        return verdict.network ? { kind: "unreachable", detail: verdict.detail } : undefined;
      }
      return undefined;
    default:
      return undefined;
  }
}
