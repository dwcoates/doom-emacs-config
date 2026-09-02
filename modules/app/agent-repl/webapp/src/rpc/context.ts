/**
 * AppContext — the one bag of app-wide capabilities every component mount
 * receives, and the ONLY thing they share.
 *
 * The webapp is a stateless renderer, so there is no store here and no view
 * state: what a component needs from the app is the client to call, the
 * workspace to address, a clock to tick on, and somewhere to report its own
 * machinery failing.
 *
 * THE CLIENT IS FIXED FOR THE LIFE OF THE PAGE. WatchWebWorkspace's
 * `transferred` push does NOT hand this page a new daemon to adopt: a
 * successor on another loopback port is a different ORIGIN, so re-pointing the
 * webview is the HOST's job (it reloads the view at the new address) and this
 * page's job is to stop talking. There is deliberately no way to swap the
 * client here — the machinery for it would be machinery for a handover this
 * end never performs.
 *
 * WHICH IS WHY THE CONTEXT CAN GO QUIET instead. `quiesce()` is that stop,
 * made structural rather than hoped for: `callUnary` refuses locally while it
 * holds, and every registered stream cancels itself through `onQuiesced` with
 * no reconnect. A page that merely stopped drawing would still be firing verbs
 * at a daemon that has handed the workspace away, and every one of them would
 * come back as a refusal the user did nothing to cause.
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
  /**
   * Stop talking to this daemon. Idempotent, and one-way: nothing lifts it,
   * because nothing on this page can give the workspace a daemon back.
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
  /**
   * Announce that a stream frame arrived and could be read.
   *
   * THE LINK BEING UP IS A PAGE-WIDE FACT, and only the stream loops can
   * observe it. The lifecycle's restarting notice comes down when the daemon
   * is answering again — from ANY stream, since a bounce takes them all — so
   * the observation is published here rather than on one component's handle.
   */
  notePush(): void;
  /** Run FN on every frame any stream reads. Returns its unsubscriber. */
  onPush(fn: () => void): () => void;
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
  let quiesced = false;
  const quietListeners = new Set<() => void>();
  const pushListeners = new Set<() => void>();
  return {
    client: init.client,
    workspace: init.workspace,
    ticker: init.ticker,
    failures: init.failures,
    composerEnabled: init.composerEnabled,
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
    notePush(): void {
      // A copy: a listener may unsubscribe itself as it runs.
      for (const fn of [...pushListeners]) fn();
    },
    onPush(fn: () => void): () => void {
      pushListeners.add(fn);
      return () => {
        pushListeners.delete(fn);
      };
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
