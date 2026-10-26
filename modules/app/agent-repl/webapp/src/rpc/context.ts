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
import type { StreamCostMeter } from "../stream-cost.js";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { Ticker } from "../clock.js";
import type { FailureSink } from "../failure/sink.js";
import type { PageSeen } from "../page-seen.js";
import { log } from "../log.js";
import type { AgentReplClient } from "./client.js";
import type { DaemonOrigin } from "./daemon-origin.js";
import { startPageStreams, type PageStreams, type PageStreamContext } from "./page-streams.js";

/**
 * What talking to the daemon takes, with no workspace in it: the client, the
 * one standing stream, the clock, where failures go, and the quiet switch.
 * The webhost holds one of these for the sidebar it draws, which shows every
 * workspace and owns none; a workspace page's AppContext is one of these plus
 * its workspace.
 */
export interface DaemonContext {
  /** The daemon client as of NOW; re-read it per call, never cache it. */
  readonly client: AgentReplClient;
  /**
   * The page's ONE standing stream, which every watch on this page rides.
   *
   * A browser holds about six connections per host over HTTP/1.1 and a
   * server-streaming call pins one for its whole life, so a page that opened a
   * stream per view ran out — silently, with the calls past the cap queued
   * forever rather than refused. Nothing here opens a stream of its own any
   * more; see page-streams.ts for what that cost when it did.
   */
  readonly streams: PageStreams;
  /** The shared one-second clock; components subscribe, never setInterval. */
  readonly ticker: Ticker;
  /** Where a component reports its OWN machinery failing. */
  readonly failures: FailureSink;
  /**
   * Stop talking to this daemon. Idempotent, and one-way: nothing lifts it,
   * because nothing on this page can give the workspace a daemon back.
   *
   * Every subsequent `callUnary` refuses locally and every registered stream
   * cancels itself, so nothing this page holds keeps addressing a daemon that
   * has released the workspace. The stream-cost meter's window timer stops
   * with them: with no stream left there is no cost to state, and a timer left
   * running would outlive the page that armed it.
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
   * Run FN every time the daemon this context dials moves. See
   * StreamContext.onDaemonMoved: only the web view host's daemon moves, and a
   * context built without one (`DaemonContextInit.daemon`) never calls FN.
   */
  onDaemonMoved(fn: () => void): () => void;
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
  /**
   * Announce that a stream which had dropped is reading frames again: the link
   * came back. See StreamContext.noteLinkRestored.
   */
  noteLinkRestored(): void;
  /** Run FN every time the link comes back. Returns its unsubscriber. */
  onLinkRestored(fn: () => void): () => void;
  /**
   * Record one frame's main-thread cost. See StreamContext.noteStreamCost.
   */
  noteStreamCost(stream: string, ms: number): void;
}

/** A workspace page's context: the daemon context, plus the workspace it shows. */
export interface AppContext extends DaemonContext {
  /** The workspace every request on this page is addressed to. */
  readonly workspace: WorkspaceRef;
  /**
   * Whether a reader can see this page now (page-seen.ts). Work only a reader
   * would notice, a type-out say, is skipped while it says no.
   */
  readonly seen: PageSeen;
  /**
   * Whether the browser-local dev composer is on (`&composer=1`). Production
   * runs composer-less: the root composer is host-native (Emacs).
   */
  readonly composerEnabled: boolean;
}

/** What a daemon context is built from. */
export interface DaemonContextInit {
  client: AgentReplClient;
  ticker: Ticker;
  failures: FailureSink;
  /**
   * This page's own id, which its one standing stream is registered under.
   *
   * It is the connection id the logs already correlate by: a page and its
   * stream are the same thing to the daemon, and two identities for one page
   * would be two things to line up in a log that already lines one up.
   */
  page: string;
  /**
   * The webapp build the web view host was loaded from, set by the host's
   * own context alone: its page stream states it (WatchPageRequest.webhost),
   * so a deploy that built another reloads the host. Unset on every
   * workspace page, which states its build on WatchWebWorkspace.
   */
  webhostBuild?: string;
  /**
   * The page's stream, for a test that drives ONE component's watch against a
   * scripted client. Production leaves it unset and gets the real one, opened
   * here; a substitute cannot reach production because nothing but a test
   * passes it.
   */
  streams?: PageStreams;
  /**
   * Where each stream frame's cost is recorded (stream-cost.ts). main.ts
   * passes the page's meter; a test that is not about the cost passes none.
   */
  costs?: StreamCostMeter;
  /**
   * The movable daemon origin the client's transport reads, set by the web
   * view host's context alone (host/sidebar.ts): its moves reopen every
   * standing stream at the daemon now named. Unset on every workspace page,
   * whose daemon is its own origin for its whole life.
   */
  daemon?: DaemonOrigin;
}

/** What a workspace page's context is built from. */
export interface AppContextInit extends DaemonContextInit {
  workspace: WorkspaceRef;
  /** The page's seen state; main.ts builds it over the page's own document. */
  seen: PageSeen;
  composerEnabled: boolean;
}

/**
 * Build the context main.ts hands to every mount, and OPEN THE PAGE'S ONE
 * STANDING STREAM as part of building it.
 *
 * The stream is opened HERE rather than by the caller because a context without
 * one cannot serve a single view: every watch on this page rides it. Building
 * them together means there is no window in which a mount could find a context
 * whose stream has not been asked for.
 */
export function createAppContext(init: AppContextInit): AppContext {
  return {
    ...createDaemonContext(init),
    workspace: init.workspace,
    seen: init.seen,
    composerEnabled: init.composerEnabled,
  };
}

/**
 * Build a daemon context, and OPEN ITS ONE STANDING STREAM as part of building
 * it, for the same reason `createAppContext` gives.
 */
export function createDaemonContext(init: DaemonContextInit): DaemonContext {
  let quiesced = false;
  const quietListeners = new Set<() => void>();
  const pushListeners = new Set<() => void>();
  const restoredListeners = new Set<() => void>();
  const base = {
    client: init.client,
    ticker: init.ticker,
    failures: init.failures,
    quiesce(): void {
      if (quiesced) return;
      quiesced = true;
      // No workspace fields on the record: `bindLogContext` already carries
      // this page's workspace on every line, and restating half of that pair
      // is what the logger's own field validation refuses.
      log.info("the page is going quiet; nothing more will be sent on this client", {
        operation: "rpc.context-quiesce",
      });
      // A copy: a listener cancelling its stream unsubscribes itself here.
      for (const fn of [...quietListeners]) fn();
      init.costs?.dispose();
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
    noteStreamCost(stream: string, ms: number): void {
      init.costs?.record(stream, ms);
    },
    noteLinkRestored(): void {
      // A copy: a listener may unsubscribe itself as it runs.
      for (const fn of [...restoredListeners]) fn();
    },
    onLinkRestored(fn: () => void): () => void {
      restoredListeners.add(fn);
      return () => {
        restoredListeners.delete(fn);
      };
    },
    onDaemonMoved(fn: () => void): () => void {
      // A PAGE'S DAEMON IS ITS OWN ORIGIN and never moves, so there is
      // nothing to subscribe to.
      return init.daemon?.onMoved(fn) ?? ((): void => undefined);
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
  // The stream subscribes to `onQuiesced` and reports through `failures` off
  // this same object, so it is opened once the rest of the context exists and
  // handed back on the finished one.
  const streams: PageStreams =
    init.streams ?? startPageStreams(base satisfies PageStreamContext, init.page, init.webhostBuild);
  return { ...base, streams };
}
