/**
 * The one hand-built store client the store suites drive the record plane
 * through, when the fake store cannot produce what a case needs: a malformed
 * answer, a refusal on cue, a busy database for one call.
 */
import type { StoreClient } from "../../src/store/client.js";

/** A store client whose every verb is a defect until an override names it. */
export function stubClient(overrides: Partial<StoreClient>): StoreClient {
  const refuse = (): never => {
    throw new Error("stub store client: this suite did not expect that call");
  };
  return {
    openAgentSession: refuse,
    watchAgentSession: refuse,
    watchBashRun: refuse,
    readAgentPage: refuse,
    getWorkflow: refuse,
    getSidecarCursors: refuse,
    getLiveWork: refuse,
    getAgentByVendorTask: refuse,
    getDetachedWork: refuse,
    getPeerDelivery: refuse,
    writeBatch: refuse,
    ...overrides,
  };
}
