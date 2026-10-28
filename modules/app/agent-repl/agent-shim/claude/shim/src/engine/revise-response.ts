/**
 * engine/revise-response.ts — ReviseResponse: rewrite one settled response on a
 * side query, and write the answer as the response_revision page line.
 *
 * THE USER ASKED ABOUT WORDS THEY HIGHLIGHTED, and the answer is the response
 * itself, revised (proto/src/shim/v1/endpoint_revise_response.proto). The
 * daemon composes the whole instruction; this module only runs it.
 *
 * THE SIDE QUERY IS AN EPHEMERAL FORK cut at the newest finished turn — the
 * point GetForkPoint names — so it reads the response where it was written,
 * from the prompt cache, and a turn in flight is outside it by construction.
 * The fork persists nothing, so the session's transcript is untouched and the
 * live query is never involved: a revision runs beside a turn in flight.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1, shimv1 } from "../proto.js";
import type { QueryLike } from "../sdk/types.js";
import type { PromptQueue } from "./prompt-queue.js";
import { askSideQuery } from "./side-query.js";
import { saidContent } from "./said-content.js";

const LOGGER = bindLog({ component: "shim-engine-revise-response", operation: "shim.engine.revise_response" });

/** Where the side query's fork ends. THE KIND IS THE OUTCOME. */
export type RevisionCut =
  /** Through `upTo`, or the whole conversation when it is absent. */
  | { readonly kind: "cut"; readonly upTo: string | undefined }
  /** No point could be named. */
  | { readonly kind: "unresolved"; readonly detail: string };

/** What a revision needs from the session it revises. */
export interface ReviseResponseDeps {
  /** The vendor session the conversation lives in, or absence when no session is started. */
  readonly vendorSessionId: () => string | undefined;
  /** Where the fork ends: the newest finished turn's final record. */
  readonly cut: (vendorSessionId: string) => RevisionCut;
  /**
   * Build the side query: an ephemeral fork of `parentSessionId`, cut at
   * `upTo`, taking no tools.
   */
  readonly build: (
    spec: { readonly parentSessionId: string; readonly upTo: string | undefined },
    prompt: PromptQueue,
    controller: AbortController,
  ) => Promise<QueryLike>;
  /** Write the page line the revision lands as. */
  readonly writeRevision: (revision: conversationv1.AgentResponseRevision) => void;
  /** Milliseconds since the epoch. */
  readonly nowMs: () => number;
}

/** Revise one response, answering how it went. */
export async function reviseResponse(
  deps: ReviseResponseDeps,
  request: shimv1.ReviseResponseRequest,
  signal?: AbortSignal,
): Promise<shimv1.ReviseResponseResponse> {
  const response = request.response?.value ?? "";
  const context = { response };
  // THE VALIDATOR REFUSED A REQUEST WITH NO INSTRUCTION before the engine.
  const instruction = request.instruction;
  if (instruction === undefined) throw new Error("revise response: a request with no instruction reached the engine");
  const parentSessionId = deps.vendorSessionId();
  if (parentSessionId === undefined) {
    LOGGER.info(context, "refused ReviseResponse: no session is started");
    return refused({ case: "noSession", value: create(shimv1.ReviseResponseNoSessionSchema, {}) });
  }
  const cut = deps.cut(parentSessionId);
  if (cut.kind === "unresolved") {
    LOGGER.error({ ...context, vendor_session_id: parentSessionId, cause: cut.detail }, "ReviseResponse could not name the point its fork ends at");
    return refused({ case: "noForkPoint", value: create(shimv1.ReviseResponseNoForkPointSchema, { detail: cut.detail }) });
  }
  LOGGER.info(
    { ...context, vendor_session_id: parentSessionId, up_to: cut.upTo ?? "" },
    "revising a response on an ephemeral fork of the conversation",
  );
  const asked = await askSideQuery(
    (prompt, controller) => {
      // THE CALL'S LIFETIME IS THE REVISION'S: a caller that goes away aborts
      // the side query, and nothing is written.
      signal?.addEventListener("abort", () => controller.abort(), { once: true });
      return deps.build({ parentSessionId, upTo: cut.upTo }, prompt, controller);
    },
    // THE USER'S IMAGES RIDE ALONG: the instruction is a said, converted as
    // every prompt the shim sends is.
    saidContent(instruction),
  );
  const failed = (detail: string): shimv1.ReviseResponseResponse => {
    LOGGER.error({ ...context, vendor_session_id: parentSessionId, outcome: asked.kind, cause: detail }, "the revising side query failed; no revision was written");
    return refused({ case: "vendorFailed", value: create(shimv1.ReviseResponseVendorFailedSchema, { detail }) });
  };
  switch (asked.kind) {
    case "unbuilt":
      return failed(`the revising query could not be built: ${asked.error}`);
    case "broken":
      return failed(`the revising query's stream broke: ${asked.error}`);
    case "noResult":
      return failed("the revising query ended without a result");
    case "errorResult":
      return failed(`the revising query ended with an error result (${asked.subtype}): ${asked.words}`);
    case "answered":
      break;
  }
  if (asked.text.trim() === "") {
    LOGGER.error({ ...context, vendor_session_id: parentSessionId }, "the revising side query answered with no text; no revision was written");
    return refused({ case: "emptyAnswer", value: create(shimv1.ReviseResponseEmptyAnswerSchema, {}) });
  }
  deps.writeRevision(
    create(conversationv1.AgentResponseRevisionSchema, {
      response: create(conversationv1.AgentActivityIdSchema, { value: response }),
      prose: create(conversationv1.AgentResponseProseSchema, { markdown: asked.text }),
      settledAt: create(conversationv1.AgentActivitySettledAtSchema, { atMs: BigInt(deps.nowMs()) }),
    }),
  );
  const model = asked.model ?? "";
  LOGGER.info(
    { ...context, vendor_session_id: parentSessionId, model, characters: asked.text.length },
    "wrote the response revision",
  );
  return create(shimv1.ReviseResponseResponseSchema, {
    result: { case: "success", value: create(shimv1.ReviseResponseSuccessSchema, { model }) },
  });
}

function refused(cause: shimv1.ReviseResponseError["cause"]): shimv1.ReviseResponseResponse {
  return create(shimv1.ReviseResponseResponseSchema, {
    result: { case: "error", value: create(shimv1.ReviseResponseErrorSchema, { cause }) },
  });
}
