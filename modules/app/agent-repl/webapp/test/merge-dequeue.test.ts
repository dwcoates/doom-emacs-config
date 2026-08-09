/**
 * merge-dequeue — the card an interrupt raises over a queued merge.
 *
 * Its whole job is to say WHERE the merge stands and to make the destructive
 * answer deliberate, so these assert the standing arm reaching the prose, the
 * card being exactly as present as the pushed offer, and the answer reaching
 * the socket as itself. One edge per test.
 */
import { describe, expect, it } from "vitest";

import type { MergeDequeueOffer, MergeStatus } from "../src/frontend-proto.js";
import {
  DEQUEUE_ATTR,
  DEQUEUE_HEADING,
  dequeueAnswerFromAttr,
  dequeueOfferText,
  dequeueRefusedNotice,
  dequeueStageText,
  mergeDequeueCardHtml,
} from "../src/merge-dequeue.js";

function waitingOffer(ahead: number, position: number, depth: number): MergeDequeueOffer {
  return {
    offerId: "offer-1",
    runId: "run-7",
    raisedAtMs: 1000,
    standing: { case: "waiting", value: { ahead, position, depth } },
  };
}

function runningOffer(status?: MergeStatus): MergeDequeueOffer {
  return {
    offerId: "offer-1",
    runId: "run-7",
    raisedAtMs: 1000,
    standing: { case: "running", value: status === undefined ? {} : { status } },
  };
}

function statusWith(phase: MergeStatus["phase"]): MergeStatus {
  return { runId: "run-7", phaseStartedAtMs: 1, updatedAtMs: 2, phase };
}

describe("no outstanding question", () => {
  // The empty string IS the absent card: the caller writes this into its slot
  // every frame, so a cleared offer takes the card down by the same path that
  // put it up.
  it("draws nothing at all", () => {
    expect(mergeDequeueCardHtml(null)).toBe("");
  });
});

describe("a waiting merge", () => {
  // The count is the fact that made the user press the key: "how much longer".
  it("says how many merges are ahead of it", () => {
    expect(dequeueOfferText(waitingOffer(2, 3, 5))).toContain("with 2 merges ahead of it");
  });

  it("says where it sits on the queue", () => {
    expect(dequeueOfferText(waitingOffer(2, 3, 5))).toContain("It is 3 of 5 on the queue");
  });

  // Singular, because "with 1 merges ahead of it" is the kind of seam that
  // makes a reader distrust the number beside it.
  it("says one merge ahead in the singular", () => {
    expect(dequeueOfferText(waitingOffer(1, 2, 4))).toContain("with 1 merge ahead of it");
  });

  // The reassurance is the point of the waiting arm: nothing has run, so the
  // destructive button is not destructive here.
  it("says taking it off costs nothing", () => {
    expect(dequeueOfferText(waitingOffer(2, 3, 5))).toContain("costs nothing");
  });
});

describe("a running merge", () => {
  // The stage is what the running arm exists to say, and it comes from the
  // run's own MergeStatus rather than from a stage word the card invented.
  it("names the cherry-pick it is on", () => {
    const offer = runningOffer(
      statusWith({
        case: "cherryPicking",
        value: {
          commitsTotal: 7,
          commitsLanded: 3,
          currentSha: "abc123",
          currentSubject: "fix the parser",
        },
      }),
    );
    expect(dequeueOfferText(offer)).toContain("cherry-picking commit 4 of 7");
  });

  it("names the test gate it is on", () => {
    const offer = runningOffer(
      statusWith({
        case: "testing",
        value: {
          commitsTotal: 7,
          commitsLanded: 5,
          currentSha: "abc123",
          currentSubject: "fix the parser",
        },
      }),
    );
    expect(dequeueOfferText(offer)).toContain("testing 5 of 7 landed commits");
  });

  it("names a parked conflict", () => {
    const offer = runningOffer(
      statusWith({
        case: "conflict",
        value: {
          conflictedSha: "abc123",
          conflictedSubject: "fix the parser",
          commitsTotal: 7,
          commitsLanded: 3,
        },
      }),
    );
    expect(dequeueOfferText(offer)).toContain("parked on a conflict");
  });

  // A head that has published nothing yet is a real state, not a gap: the arm
  // alone is enough to say the merge is running.
  it("still says the merge is running with no status published", () => {
    expect(dequeueOfferText(runningOffer())).toContain("This merge is running");
  });

  // THE WARNING IS THE POINT OF THE RED. The one case where an abort is not
  // free is unrecoverable, so the card never implies it is.
  it("warns that anything already landed stays landed", () => {
    expect(dequeueOfferText(runningOffer())).toContain(
      "anything it has already landed on the target stays there",
    );
  });

  it("says nothing about a stage for a status that has not been published", () => {
    expect(dequeueStageText(undefined)).toBe("");
  });
});

describe("the card", () => {
  it("carries the heading naming the verb", () => {
    expect(mergeDequeueCardHtml(waitingOffer(2, 3, 5))).toContain(DEQUEUE_HEADING);
  });

  it("offers both answers as buttons", () => {
    const html = mergeDequeueCardHtml(waitingOffer(2, 3, 5));
    expect(html).toContain(`${DEQUEUE_ATTR}="dequeue"`);
    expect(html).toContain(`${DEQUEUE_ATTR}="keep"`);
  });

  // The class is what the stylesheet's red frame hangs off; a card that lost it
  // would ask a destructive question dressed as an ordinary one.
  it("marks a running offer so the frame can differ from a waiting one", () => {
    expect(mergeDequeueCardHtml(runningOffer())).toContain('class="merge-dequeue running"');
    expect(mergeDequeueCardHtml(waitingOffer(1, 2, 2))).toContain(
      'class="merge-dequeue waiting"',
    );
  });
});

describe("reading the clicked answer", () => {
  it("reads dequeue", () => {
    expect(dequeueAnswerFromAttr("dequeue")).toBe("dequeue");
  });

  it("reads keep", () => {
    expect(dequeueAnswerFromAttr("keep")).toBe("keep");
  });

  // "Not the keep button" is not the same statement as "dequeue" when one of
  // the two answers destroys work, so an unrecognized value throws rather than
  // resolving to whichever the reader guessed.
  it("throws on anything else rather than guessing", () => {
    expect(() => dequeueAnswerFromAttr("maybe")).toThrow(/unrecognized answer/);
  });

  it("throws on a missing attribute", () => {
    expect(() => dequeueAnswerFromAttr(null)).toThrow(/unrecognized answer/);
  });
});

describe("a refused answer", () => {
  // The card comes down only when the daemon clears the offer, so a refusal
  // rendered nowhere would read as the button doing nothing.
  it("names the verb that was refused and the daemon's own words", () => {
    const notice = dequeueRefusedNotice("dequeue", new Error("no dequeue offer is outstanding"));
    expect(notice).toBe(
      "could not dequeue this merge: no dequeue offer is outstanding",
    );
  });

  it("names the keep verb for a refused keep", () => {
    expect(dequeueRefusedNotice("keep", new Error("offer is stale"))).toContain(
      "could not keep this merge queued",
    );
  });
});
