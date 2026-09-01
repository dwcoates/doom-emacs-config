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
 *
 * WHY THE CONTEXT CAN GO QUIET. The webapp does NOT redial a successor daemon
 * (ruled 2026-08-29): a different loopback port is a different origin, so
 * re-pointing the webview is the HOST's job and this page's job is to stop
 * talking. `quiesce()` is that stop, made structural rather than hoped for —
 * `callUnary` refuses locally while it holds, and every registered stream
 * cancels itself through `onQuiesced` with no reconnect. A page that merely
 * stopped drawing would still be firing verbs at a daemon that has handed the
 * workspace away, and every one of them would come back as a refusal the user
 * did nothing to cause.
 */
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { Ticker } from "../clock.js";
import type { FailureSink } from "../failure/sink.js";
import { log } from "../log.js";
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
  /**
   * Stop talking to this daemon. Idempotent; `replaceClient` lifts it.
   *
   * Every subsequent `callUnary` refuses locally and every registered stream
   * cancels itself, so nothing this page holds keeps addressing a daemon that
   * has released the workspace.
   */
  quiesce(): void;
  /** Whether the page has gone quiet. */
  isQuiesced(): boolean;
  /**
   * Run FN the first time the page goes quiet. Returns its unsubscriber.
   *
   * This is the STREAM REGISTRY's half of quiescing: `watchStream` subscribes
   * each handle here, so "cancel every registered stream" is one call rather
   * than a list every caller has to remember to keep.
   */
  onQuiesced(fn: () => void): () => void;
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
  let quiesced = false;
  const listeners = new Set<() => void>();
  const quietListeners = new Set<() => void>();
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
      // Adopting a daemon is the one thing that makes talking legitimate
      // again: the quiet window existed only because there was nowhere to
      // send. Clearing it here (rather than asking the caller to) is what
      // keeps the flag from outliving the condition it describes.
      quiesced = false;
      // A copy, because a listener may unsubscribe itself while reopening.
      for (const fn of [...listeners]) fn();
    },
    onClientReplaced(fn: () => void): () => void {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    },
    quiesce(): void {
      if (quiesced) return;
      quiesced = true;
      // No workspace fields on the record: `bindLogContext` already carries
      // this page's workspace on every line, and restating half of that pair
      // is what the logger's own field validation refuses.
      log("info", "the page is going quiet; nothing more will be sent on this client", {
        operation: "rpc.context-quiesce",
      });
      // A copy: a listener cancelling its stream unsubscribes itself here.
      for (const fn of [...quietListeners]) fn();
    },
    isQuiesced(): boolean {
      return quiesced;
    },
    onQuiesced(fn: () => void): () => void {
      // A subscriber arriving AFTER the page went quiet is told at once, or a
      // stream opened during the window would stand forever waiting for an
      // event that has already happened.
      if (quiesced) {
        fn();
        return () => undefined;
      }
      quietListeners.add(fn);
      return () => {
        quietListeners.delete(fn);
      };
    },
  };
}
