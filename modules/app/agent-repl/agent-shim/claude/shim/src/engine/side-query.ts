/**
 * engine/side-query.ts — ask a throwaway query ONE question and read its one
 * result.
 *
 * THE LIVE QUERY NEVER CARRIES HARNESS WORK. It owns the session's identity,
 * its locks and its in-flight turn, and a prompt pushed into it lands in the
 * user's conversation. Work the shim does ABOUT the conversation (the cold
 * gate's compaction summary, a response revision) runs on a query of its own,
 * built for the one question and closed after its first result. This is the
 * one loop every such query is read through, so they agree on what an answer
 * is and on how a query that ends without one is told apart from one whose
 * result is an error.
 */
import type { QueryLike, SdkUserMessage } from "../sdk/types.js";
import { normalizeApiUsage, type NormalizedApiUsage } from "../api-usage.js";
import { PromptQueue } from "./prompt-queue.js";

/** How a side query ended. THE KIND IS THE OUTCOME. */
export type SideQueryOutcome =
  /** The query answered: its result's text, and the model its opening stated. */
  | {
      readonly kind: "answered";
      readonly text: string;
      /** UNDEFINED when the stream answered before its own opening. */
      readonly model: string | undefined;
      readonly usage: NormalizedApiUsage;
    }
  /**
   * The query's result is an error. The vendor ends a turn whose API call
   * failed with `subtype: "success"` and `is_error`, its `result` the error's
   * own sentence, so an error is told by both, never by the subtype alone.
   */
  | {
      readonly kind: "errorResult";
      readonly subtype: string;
      /** The vendor's words: the result's sentence, or its listed errors. */
      readonly words: string;
      /** The API status the result carries, when it carries one. */
      readonly status: number | undefined;
    }
  /** The stream ended without any result: a child that went away. */
  | { readonly kind: "noResult" }
  /** The query could not be built at all. */
  | { readonly kind: "unbuilt"; readonly error: string }
  /** The stream threw while it was being read. */
  | { readonly kind: "broken"; readonly error: string };

/**
 * Build one query with `build`, prompt it with `content` (text, or the blocks
 * a said converts to), and read it to its first result. The query, its prompt queue and its abort controller are closed
 * however it ends.
 */
export async function askSideQuery(
  build: (prompt: PromptQueue, controller: AbortController) => Promise<QueryLike>,
  content: SdkUserMessage["message"]["content"],
): Promise<SideQueryOutcome> {
  const queue = new PromptQueue();
  const controller = new AbortController();
  let query: QueryLike | undefined;
  try {
    try {
      query = await build(queue, controller);
    } catch (err) {
      return { kind: "unbuilt", error: messageOf(err) };
    }
    queue.push({ type: "user", message: { role: "user", content }, parent_tool_use_id: null });
    let model: string | undefined;
    try {
      for await (const message of query) {
        if (message.type === "system" && message.subtype === "init") {
          model = message.model;
          continue;
        }
        if (message.type !== "result") continue;
        if (message.subtype !== "success" || message.is_error) {
          // READ DEFENSIVELY: this result is the vendor's, and one that states
          // no `errors` must still be labeled, not throw.
          const { api_error_status: status, errors } = message as unknown as {
            api_error_status?: number | null;
            errors?: unknown;
          };
          const words =
            message.subtype === "success" ? message.result : Array.isArray(errors) ? errors.map(String).join("; ") : "";
          return { kind: "errorResult", subtype: message.subtype, words, status: typeof status === "number" ? status : undefined };
        }
        return { kind: "answered", text: message.result, model, usage: normalizeApiUsage(message.usage) };
      }
    } catch (err) {
      return { kind: "broken", error: messageOf(err) };
    }
    return { kind: "noResult" };
  } finally {
    queue.close();
    controller.abort();
    query?.close();
  }
}

function messageOf(err: unknown): string {
  return err instanceof Error ? err.message : String(err);
}
