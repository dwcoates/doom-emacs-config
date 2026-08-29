/**
 * AppContext — the one bag of app-wide capabilities every component mount
 * receives, and the ONLY thing they share.
 *
 * The webapp is a stateless renderer, so there is no store here and no view
 * state: what a component needs from the app is the client to call, the
 * workspace to address, a clock to tick on, and somewhere to report its own
 * machinery failing.
 *
 * WHY THE CLIENT IS REPLACEABLE RATHER THAN FIXED. WatchWebWorkspace's
 * `transferred` push hands the page a NEW daemon to adopt; every standing
 * stream must move to it and every subsequent verb must go there. Making that
 * one mutation on the context — instead of tearing the page down — is what
 * keeps a rollout invisible to the user. `onClientReplaced` is how a holder of
 * something client-derived (a stream registry, a cached call) re-derives it.
 */
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { Ticker } from "../clock.js";
import type { FailureSink } from "../failure/sink.js";
import type { AgentReplClient } from "./client.js";

export interface AppContext {
  /** The daemon client as of NOW; re-read it per call, never cache it. */
  readonly client: AgentReplClient;
  /** The workspace every request on this page is addressed to. */
  readonly workspace: WorkspaceRef;
  /** The shared one-second clock; components subscribe, never setInterval. */
  readonly ticker: Ticker;
  /** Where a component reports its OWN machinery failing. */
  readonly failures: FailureSink;
  /**
   * Whether the browser-local dev composer is on (`&composer=1`). Production
   * runs composer-less: the root composer is host-native (Emacs).
   */
  readonly composerEnabled: boolean;
  /** Adopt a new daemon; every registered stream reopens on it. */
  replaceClient(next: AgentReplClient): void;
  /** Run FN after each replacement. Returns its unsubscriber. */
  onClientReplaced(fn: () => void): () => void;
}

export interface AppContextInit {
  client: AgentReplClient;
  workspace: WorkspaceRef;
  ticker: Ticker;
  failures: FailureSink;
  composerEnabled: boolean;
}

/** Build the context main.ts hands to every mount. */
export function createAppContext(init: AppContextInit): AppContext {
  let client = init.client;
  const listeners = new Set<() => void>();
  return {
    get client(): AgentReplClient {
      return client;
    },
    workspace: init.workspace,
    ticker: init.ticker,
    failures: init.failures,
    composerEnabled: init.composerEnabled,
    replaceClient(next: AgentReplClient): void {
      client = next;
      // A copy, because a listener may unsubscribe itself while reopening.
      for (const fn of [...listeners]) fn();
    },
    onClientReplaced(fn: () => void): () => void {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    },
  };
}
