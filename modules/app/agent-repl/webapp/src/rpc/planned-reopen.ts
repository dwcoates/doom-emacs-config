/**
 * planned-reopen — an end of a stream that the daemon PLANNED and that the
 * stream reopens from at once.
 *
 * WHY IT IS AN ERROR TYPE. A page subscription ends by its `watch()` iterable
 * ending, and its caller's `watchStream` reads every end it did not cancel as
 * the link failing: it files `daemon_unreachable` and backs off. Two ends are
 * neither. The page's ATTACHMENT IS OVER (`SubscribePage` answered
 * `page_gone`): the page is reattaching, and the subscription is made again
 * under the next attachment. And the daemon serving a RELAYED view ended it
 * cleanly (`PageSubscriptionEnded.moved`): the view is made again and is
 * served by whichever daemon serves it now. Both throw this, which
 * `watchStream` recognizes by TYPE: nothing is filed and the stream reopens
 * with no backoff.
 *
 * It lives in its own module because `streams.ts` recognizes it and
 * `page-streams.ts`, which imports `streams.ts`, throws it.
 */
export class PlannedReopen extends Error {
  constructor(reason: string) {
    super(reason);
    this.name = "PlannedReopen";
  }
}
