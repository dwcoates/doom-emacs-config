/**
 * link — the CLIENT'S OWN VERDICT on the webapp→daemon link, and the one place
 * a failure this end observed becomes something the footer can draw.
 *
 * THE ONE DELIBERATE EXCEPTION TO THE STATELESS RENDERER RULE (owner ruling,
 * 2026-09-13, `docs/REALTEST-JUDGEMENT-CALLS.md`, "webapp-side failures reach
 * the footer"). `FooterView` is written only by the daemon, and
 * `FooterStatusDisconnected` describes the daemon→shim link, not this one — so
 * a webapp that cannot reach the daemon has no arm to be pushed and no link to
 * be pushed it over (`docs/FOOTER-TOPOLOGY-AUDIT.md`, F1 and F2). When the
 * webapp knows the link is down it knows it ALONE, so it draws it itself: the
 * footer overlays a locally composed disconnected strip while a verdict
 * stands, and the daemon's pushed view returns the moment one does not.
 *
 * THE DAEMON STAYS AUTHORITATIVE WHENEVER THE LINK IS UP. A verdict is not
 * lifted by a push — a footer that keeps pushing while a verb cannot be
 * delivered is exactly the state the ruling's incident produced — it is lifted
 * only when something this page did PROVED the link works: a unary that was
 * answered, or a stream that started reading again.
 *
 * ONE FUNCTION IN, ONE VERDICT OUT. Every failing call site reports through
 * `reportClientFailure(kind, context)`; the kind picks the substatus word and
 * the context IS the activity line, composed at the site because only the site
 * knows what was being attempted.
 *
 * A REPEAT OF THE SAME KIND AND CONTEXT IS SILENT, and that is structural
 * rather than tidy: the `ClientLog` sink reports through here, this module
 * logs, that log is forwarded through the same sink, and the forward fails
 * again. Dropping an identical repeat is what makes that loop terminate.
 */
import { log } from "../log.js";

/**
 * Every site that can report, spelled once.
 *
 * The kind is the SITE CLASS, not the sentence: two sites that fail the same
 * way (a unary that never reached the daemon, from a verb or from the feed's
 * reveal probe) share a kind and differ in their context line.
 */
export type ClientFailureKind =
  | "unary_transport"
  | "stream_ended"
  | "subscription_source_ended"
  | "unsubscribe_failed"
  | "client_log_failed"
  | "daemon_unreachable_card"
  | "frame_undecodable_card"
  | "feed_not_tailing";

/**
 * The substatus word each kind draws under the disconnected status.
 *
 * THE STATUS IS ALWAYS `disconnected` (the ruling), and the default substatus
 * is the ruling's own "daemon unreachable". Two kinds deviate, and both are
 * argued in `docs/REALTEST-JUDGEMENT-CALLS.md`:
 *
 *   - `frame_undecodable_card`: the daemon was reached and answered; one frame
 *     could not be read. Calling that "daemon unreachable" would say something
 *     the page can see is false.
 *   - `feed_not_tailing`: the daemon ANSWERED and refused an `OpenFeed`. The
 *     audit records that misnaming as a defect in its own right (§3, N3 row
 *     14: "a card that says the daemon is unreachable when it is not"), so the
 *     word here is what actually stopped — the feed's tail.
 */
const KIND_SUBSTATUS: Readonly<Record<ClientFailureKind, string>> = Object.freeze({
  unary_transport: "daemon unreachable",
  stream_ended: "daemon unreachable",
  subscription_source_ended: "daemon unreachable",
  unsubscribe_failed: "daemon unreachable",
  client_log_failed: "daemon unreachable",
  daemon_unreachable_card: "daemon unreachable",
  frame_undecodable_card: "frame unreadable",
  feed_not_tailing: "feed not tailing",
});

/** The standing verdict, as the footer draws it. */
export interface ClientVerdict {
  /** Which site reported it. */
  readonly kind: ClientFailureKind;
  /** The substatus cell's word. */
  readonly substatus: string;
  /** The activity cell's ad-hoc line, composed at the reporting site. */
  readonly activity: string;
}

/** The verdict now standing, or null when the link is believed up. */
let standing: ClientVerdict | null = null;

/** Everyone redrawing off the verdict; today, the footer. */
const listeners = new Set<(verdict: ClientVerdict | null) => void>();

/** The substatus word KIND draws. Exported so the suite reads one table. */
export function clientFailureSubstatus(kind: ClientFailureKind): string {
  return KIND_SUBSTATUS[kind];
}

/**
 * Report that something this page tried could not be completed against the
 * daemon. CONTEXT is the ad-hoc line the footer draws, in the site's own
 * words ("WatchFooter stream ended (source_ended)", "AnswerColdGate: ...").
 */
export function reportClientFailure(kind: ClientFailureKind, context: string): void {
  if (standing !== null && standing.kind === kind && standing.activity === context) {
    // The identical verdict already stands. Saying it again would change
    // nothing on the screen and, for the ClientLog sink, would not terminate.
    return;
  }
  standing = { kind, substatus: KIND_SUBSTATUS[kind], activity: context };
  log.info(`the webapp cannot complete a call to the daemon: ${context}`, {
    operation: "rpc.client-failure",
    context: { kind, detail: context },
  });
  publish();
}

/**
 * The link is demonstrably up: drop whatever verdict stands.
 *
 * ONE CLEAR FOR EVERY KIND, because every kind is a statement about the same
 * one link. A unary the daemon answered, or a stream frame this page read,
 * disproves all of them at once, and a per-kind clear would leave the footer
 * detached over a `ClientLog` that failed a minute ago.
 */
export function clearClientFailures(): void {
  if (standing === null) return;
  log.info(`the daemon answered again; dropping the ${standing.kind} client verdict`, {
    operation: "rpc.client-failure-cleared",
    context: { kind: standing.kind },
  });
  standing = null;
  publish();
}

/**
 * Drop the standing verdict only if it is KIND's.
 *
 * The narrow clear, for the one failure a single event disproves BY ITSELF: a
 * frame that decoded says nothing about a verb that never arrived, but it does
 * say the last frame's undecodability is over. Everything else clears through
 * `clearClientFailures`.
 */
export function clearClientFailure(kind: ClientFailureKind): void {
  if (standing === null || standing.kind !== kind) return;
  clearClientFailures();
}

/** The verdict the footer should draw, or null. */
export function standingClientFailure(): ClientVerdict | null {
  return standing;
}

/**
 * Observe the verdict changing, and be told the current one at once.
 *
 * The immediate call is what lets the footer mount into a page that is ALREADY
 * detached: a component that waited for the next change would draw the
 * daemon's last view, or nothing, over a link that is down.
 */
export function onClientVerdict(fn: (verdict: ClientVerdict | null) => void): () => void {
  listeners.add(fn);
  fn(standing);
  return () => {
    listeners.delete(fn);
  };
}

function publish(): void {
  // A copy: a listener may unsubscribe itself as it runs.
  for (const fn of [...listeners]) fn(standing);
}
