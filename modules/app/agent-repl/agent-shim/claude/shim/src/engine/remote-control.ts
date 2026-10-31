/**
 * engine/remote-control.ts — the session's Remote Control bridge, held.
 *
 * THE BRIDGE IS THE VENDOR'S. `Query.enableRemoteControl` asks the CLI to
 * connect the session to claude.ai itself (the `remote_control` control
 * request, sdk/types.ts `RemoteControlOptionsLike`), so the session keeps
 * running here and claude.ai, or its mobile app, attaches to it. This module
 * holds what the daemon asked for (shim.v1.SetSessionRemoteControl) and puts
 * it in force.
 *
 * THE SETTING OUTLIVES THE VENDOR PROCESS, NOT THE SHIM. The held state is put
 * in force on the live query when it is asked for ({@link RemoteControl.set}),
 * and again on every vendor process the session opens afterwards
 * ({@link RemoteControl.reapply}: a keep-alive rewind, a rollback's restart, a
 * plain resume after a refused cut).
 *
 * THE SAME claude.ai SESSION ACROSS PROCESSES. Every bridge is asked for with
 * `keepSessionOnExit`, so the claude.ai session stays listed when its process
 * ends, and every later bridge re-binds the LATEST bridge session id the
 * vendor answered (`reattachSessionId`): the one the daemon handed in, until
 * the vendor names another.
 *
 * ONLY AN ACCEPTED REQUEST IS HELD. A request the vendor refused leaves the
 * state that was in force before it, so a refused ON is not retried on every
 * later process, and a refused OFF leaves the bridge the vendor still runs
 * stated as on.
 *
 * ONE CALL AT A TIME. Every call to the vendor runs behind the one before it,
 * so an ON and an OFF sent close together reach the vendor in the order they
 * were made, and a reapply always reads the state the call ahead of it left.
 *
 * CONSTANT SIZE: one name, one id, one flag.
 */
import { bindLog } from "../log.js";
import type { QueryLike, RemoteControlAnswerLike } from "../sdk/types.js";

const LOGGER = bindLog({ component: "shim-engine-remote-control", operation: "shim.engine.remote-control" });

/** What the daemon asked for (shim.v1.SetSessionRemoteControlRequest). */
export interface RemoteControlRequest {
  readonly enabled: boolean;
  /** The name claude.ai lists the session under; meaningful only when `enabled`. */
  readonly name: string;
  /** A previous shim's bridge session to re-bind; meaningful only when `enabled`. */
  readonly reattachBridgeSessionId?: string;
}

/** How one call to the vendor ended. */
export type RemoteControlOutcome =
  | { readonly kind: "on"; readonly bridgeSessionId: string }
  | { readonly kind: "off" }
  | { readonly kind: "refused"; readonly detail: string };

/**
 * The bridge session id a `remote_control` answer states, or absence when it
 * states none. The answer is undeclared (sdk/types.ts
 * `RemoteControlAnswerLike`), so the field is read as what it is: unknown.
 */
export function bridgeSessionIdOf(answer: RemoteControlAnswerLike | undefined): string | undefined {
  const id = answer?.bridge_session_id;
  return typeof id === "string" && id !== "" ? id : undefined;
}

/** The bridge state this session holds, and the one path to the vendor's bridge. */
export class RemoteControl {
  /** The bridge in force: its name and the latest bridge session id. Absent when off. */
  private held: { readonly name: string; readonly bridgeSessionId: string } | undefined;
  /** The last call to the vendor, which the next one runs behind. Never rejects. */
  private chain: Promise<unknown> = Promise.resolve();

  /** Whether the bridge is on: a request to turn it on was the last the vendor accepted. */
  get enabled(): boolean {
    return this.held !== undefined;
  }

  /** The latest bridge session id the vendor answered, while the bridge is on. */
  get bridgeSessionId(): string | undefined {
    return this.held?.bridgeSessionId;
  }

  /**
   * Put the requested state in force on `query`, the live one, and hold it.
   *
   * An ON re-binds the request's reattach id, else the bridge session this
   * session already holds, else none (the vendor creates one).
   */
  set(query: QueryLike, request: RemoteControlRequest): Promise<RemoteControlOutcome> {
    return this.serially(async () => {
      if (!request.enabled) return this.turnOff(query);
      const reattach = request.reattachBridgeSessionId ?? this.held?.bridgeSessionId;
      return this.turnOn(query, request.name, reattach, "SetSessionRemoteControl");
    });
  }

  /**
   * A new vendor process came up: put the held bridge in force on it,
   * re-binding the latest bridge session. Answers absence when the bridge is
   * off (nothing is asked of the vendor), and when `live()` says the query was
   * let go before its turn came.
   */
  reapply(query: QueryLike, why: string, live: () => boolean): Promise<RemoteControlOutcome | undefined> {
    return this.serially(async () => {
      const held = this.held;
      if (held === undefined) return undefined;
      if (!live()) {
        LOGGER.debug({ why }, "the query a bridge was owed to was let go first; the next query is owed it instead");
        return undefined;
      }
      return this.turnOn(query, held.name, held.bridgeSessionId, why);
    });
  }

  private async turnOn(
    query: QueryLike,
    name: string,
    reattach: string | undefined,
    why: string,
  ): Promise<RemoteControlOutcome> {
    let answer: RemoteControlAnswerLike;
    try {
      answer = await query.enableRemoteControl(true, name, {
        keepSessionOnExit: true,
        ...(reattach === undefined ? {} : { reattachSessionId: reattach }),
      });
    } catch (err) {
      const detail = err instanceof Error ? err.message : String(err);
      LOGGER.info(
        { why, name, reattach_bridge_session_id: reattach ?? "", cause: detail },
        "the vendor refused to turn Remote Control on; the bridge state in force is unchanged",
      );
      return { kind: "refused", detail };
    }
    const bridgeSessionId = bridgeSessionIdOf(answer);
    if (bridgeSessionId === undefined) {
      // NOTHING TO HAND BACK, so nothing is held: the daemon keeps the id to
      // re-bind the same claude.ai session, and a bridge without one cannot be
      // found again.
      const detail = "the vendor turned Remote Control on but answered no bridge session id";
      LOGGER.error({ why, name, answer: JSON.stringify(answer), detail }, "the vendor's Remote Control answer named no bridge session");
      return { kind: "refused", detail };
    }
    this.held = { name, bridgeSessionId };
    LOGGER.info(
      {
        why,
        name,
        bridge_session_id: bridgeSessionId,
        reattached: reattach === bridgeSessionId,
        reattach_bridge_session_id: reattach ?? "",
        session_url: typeof answer.session_url === "string" ? answer.session_url : "",
        environment_id: typeof answer.environment_id === "string" ? answer.environment_id : "",
      },
      "Remote Control is on: claude.ai can reach this session",
    );
    return { kind: "on", bridgeSessionId };
  }

  private async turnOff(query: QueryLike): Promise<RemoteControlOutcome> {
    try {
      await query.enableRemoteControl(false);
    } catch (err) {
      const detail = err instanceof Error ? err.message : String(err);
      LOGGER.info(
        { cause: detail, was_on: this.held !== undefined },
        "the vendor refused to turn Remote Control off; the bridge state in force is unchanged",
      );
      return { kind: "refused", detail };
    }
    const was = this.held;
    this.held = undefined;
    LOGGER.info(
      { was_on: was !== undefined, bridge_session_id: was?.bridgeSessionId ?? "" },
      "Remote Control is off: claude.ai can no longer reach this session",
    );
    return { kind: "off" };
  }

  /** Run `call` behind every call made before it; a call's failure never blocks the next. */
  private serially<T>(call: () => Promise<T>): Promise<T> {
    const next = this.chain.then(call);
    this.chain = next.catch(() => undefined);
    return next;
  }
}
