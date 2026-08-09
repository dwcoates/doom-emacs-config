/**
 * The held-prompt queue card (E4): the subdued affordance for a prompt the
 * DAEMON is holding because a turn was already running. One edge per test.
 */
import { describe, expect, it } from "vitest";
import {
  LEASE_FORCE_TITLE,
  KEEP_ALIVE_HELD_REASON,
  LEASE_HELD_REASON,
  REVIVAL_HELD_REASON,
  QueuedCard,
  queuedCardKey,
} from "../src/render.js";
import { QueuedItem } from "../src/protocol.js";

/** A held prompt, still being classified unless overridden. */
function queued(over: Partial<QueuedItem> = {}): QueuedItem {
  return {
    id: "q1",
    text: "run this later",
    queuedAtMs: 1_700_000_000_000,
    classification: "pending",
    rationale: "",
    accepted: false,
    ...over,
  };
}

describe("QueuedCard", () => {
  it("renders the held prompt text", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued())).toContain("run this later");
  });

  it("is a subdued queued-card, not a live user bubble", () => {
    // Arrange / Act — the whole point: a held prompt must read as distinct
    // from a message that actually reached the agent.
    const html = QueuedCard(queued());
    // Assert
    expect(html).toContain("queued-card");
    expect(html).not.toContain("bubble user");
  });

  it("labels a pre-verdict entry as classifying", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ classification: "pending" }))).toContain("queued — classifying…");
  });

  it("labels a hold verdict by when it will run", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ classification: "hold" }))).toContain(
      "queued — will run after this turn",
    );
  });

  it("labels an interject verdict as interrupting", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ classification: "interject" }))).toContain("interrupting…");
  });

  it("labels an error entry as unclassified, never as a verdict", () => {
    // Arrange / Act — ERROR means NOTHING decided this; it must not read as a
    // hold the classifier actually chose.
    const html = QueuedCard(queued({ classification: "error" }));
    // Assert
    expect(html).toContain("queued — unclassified");
    expect(html).not.toContain("will run after this turn");
  });

  it("shows the classifier's reason once known", () => {
    // Arrange / Act / Assert
    expect(
      QueuedCard(queued({ classification: "hold", rationale: "independent of the running turn" })),
    ).toContain("independent of the running turn");
  });

  it("omits the reason line before a verdict lands", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued())).not.toContain("queued-reason");
  });

  it("carries the entry id on the cancel button for delegated handling", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ id: "q_abc" }))).toContain('data-queue-cancel="q_abc"');
  });

  it("carries the entry id on the run-now button for delegated handling", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ id: "q_abc" }))).toContain('data-queue-run-now="q_abc"');
  });

  it("offers accept once a verdict has landed", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ classification: "hold", id: "q_abc" }))).toContain(
      'data-queue-accept="q_abc"',
    );
  });

  it("offers no accept while the entry is still pending", () => {
    // Arrange / Act — there is nothing to confirm about a verdict that has not
    // been reached.
    expect(QueuedCard(queued({ classification: "pending" }))).not.toContain("data-queue-accept");
  });

  it("offers no accept on an already-accepted entry", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ classification: "hold", accepted: true }))).not.toContain(
      "data-queue-accept",
    );
  });

  it("marks an accepted entry as accepted", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ classification: "hold", accepted: true }))).toContain(
      "queued-accepted",
    );
  });

  it("escapes markup in the prompt text", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ text: "<img src=x onerror=1>" }))).not.toContain("<img");
  });

  it("escapes markup in the entry id it puts in a data attribute", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued({ id: `"><img>` }))).not.toContain("<img>");
  });
});

describe("queuedCardKey", () => {
  it("keys a queued card by its entry id", () => {
    // Arrange / Act / Assert
    expect(queuedCardKey(queued({ id: "q7" }))).toBe("queued:q7");
  });
});

describe("classification exhaustiveness (3e)", () => {
  it("renders a distinct badge for every classification the decoder admits", () => {
    // Arrange — the four the frontend-proto decoder can produce. Each must get
    // its OWN label; a fallthrough would silently print "classifying…" for
    // three of them.
    const want: Array<[QueuedItem["classification"], string]> = [
      ["pending", "classifying"],
      ["hold", "will run after this turn"],
      ["interject", "interrupting"],
      ["error", "unclassified"],
    ];
    for (const [classification, label] of want) {
      // Act
      const html = QueuedCard(queued({ classification }));
      // Assert
      expect(html).toContain(label);
    }
  });

  it("throws on a classification outside the union instead of rendering it as pending", () => {
    // Arrange — the runtime half of the never-check. The COMPILE-time half is
    // the `const unhandled: never = cls` in queuedBadge: adding a
    // QueueClassification arm without a case here fails `npm run typecheck`,
    // which is the guarantee that actually matters. This asserts the escape
    // hatch is loud rather than silently reading as "still being judged".
    const rogue = queued({ classification: "teleported" as QueuedItem["classification"] });
    // Act / Assert
    expect(() => QueuedCard(rogue)).toThrow(/unhandled queue classification/);
  });
});

// --- the drain-lease bubble ---------------------------------------------------

/** A prompt parked by a scheduled daemon bounce rather than by a turn. */
function leased(over: Partial<QueuedItem> = {}): QueuedItem {
  return queued({ shutdownHold: { scheduleId: "sched-1" }, ...over });
}

describe("QueuedCard — drain-lease dispatch", () => {
  it("renders the lease bubble for an entry the drain lease holds", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(leased())).toContain("lease-card");
  });

  it("renders the classifier bubble for an entry with no lease hold", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued())).not.toContain("lease-card");
  });

  it("never shows a classifier verdict on a lease-held entry", () => {
    // Arrange — the classifier NEVER ran, so its badge would claim a
    // judgment nothing made.
    const html = QueuedCard(leased({ classification: "hold" }));
    // Act / Assert
    expect(html).not.toContain("will run after this turn");
  });

  it("never shows the classifying badge on a lease-held pending entry", () => {
    // Arrange — PENDING is what the daemon stamps at enqueue; on a leased
    // entry it means nothing is judging it, not that judging is underway.
    const html = QueuedCard(leased({ classification: "pending" }));
    // Act / Assert
    expect(html).not.toContain("classifying");
  });

  it("never shows the classifier's rationale line on a lease-held entry", () => {
    // Arrange
    const html = QueuedCard(leased({ rationale: "independent work" }));
    // Act / Assert
    expect(html).not.toContain("independent work");
  });

  it("says the bounce is why the prompt is waiting", () => {
    expect(QueuedCard(leased())).toContain("daemon bounce scheduled");
  });

  it("says the prompt survives the bounce rather than being dropped", () => {
    expect(QueuedCard(leased())).toContain(LEASE_HELD_REASON);
  });

  it("carries the entry id on the cancel control", () => {
    expect(QueuedCard(leased())).toContain('data-queue-cancel="q1"');
  });

  it("carries the entry id on the force control", () => {
    expect(QueuedCard(leased())).toContain('data-queue-run-now="q1"');
  });

  it("says out loud that forcing delays the bounce", () => {
    expect(QueuedCard(leased())).toContain(LEASE_FORCE_TITLE);
  });

  it("offers no accept control, since there is no verdict to confirm", () => {
    expect(QueuedCard(leased())).not.toContain("data-queue-accept");
  });

  it("joins the bubble to the schedule that explains it", () => {
    expect(QueuedCard(leased())).toContain('data-schedule-id="sched-1"');
  });

  it("renders the user's prompt text", () => {
    expect(QueuedCard(leased({ text: "rerun the suite" }))).toContain("rerun the suite");
  });

  it("escapes markup in the prompt text", () => {
    expect(QueuedCard(leased({ text: "<script>x</script>" }))).not.toContain("<script>");
  });

  it("escapes markup in the entry id it puts in a data attribute", () => {
    const html = QueuedCard(leased({ id: 'q"1' }));
    expect(html).toContain("q&quot;1");
  });

  it("escapes markup in the schedule id it puts in a data attribute", () => {
    const html = QueuedCard(leased({ shutdownHold: { scheduleId: 's"1' } }));
    expect(html).toContain("s&quot;1");
  });

  it("keys a lease bubble by its entry id, exactly as a queued card is keyed", () => {
    // Arrange — the tail queue reconciles both kinds through one key space.
    expect(queuedCardKey(leased())).toBe("queued:q1");
  });
});

/** A held prompt parked behind an in-flight cache keep-alive turn. */
function keepAliveHeld(over: Partial<QueuedItem> = {}): QueuedItem {
  return queued({ keepAliveHold: { turnId: "turn-9" }, ...over });
}

describe("QueuedCard — keep-alive dispatch", () => {
  it("renders the keep-alive bubble for an entry a keep-alive turn holds", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(keepAliveHeld())).toContain("keep-alive-card");
  });

  it("renders the classifier bubble for an entry with no keep-alive hold", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued())).not.toContain("keep-alive-card");
  });

  it("is modeled on the lease bubble, not the classifier one", () => {
    // Arrange — both are "held by something that is not the turn above you";
    // the classifier card would claim a verdict nothing produced.
    const html = QueuedCard(keepAliveHeld());
    // Act / Assert
    expect(html).toContain("queued-badge keep-alive");
  });

  it("says it is waiting on a keep-alive response", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(keepAliveHeld())).toContain("waiting on a keep-alive response");
  });

  it("explains that the prompt is delivered when the ping's turn ends", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(keepAliveHeld())).toContain(KEEP_ALIVE_HELD_REASON);
  });

  it("offers NO force control, because no ordering exists in which forcing works", () => {
    // Arrange — the keep-alive must complete before the daemon can rewind and
    // submit this entry, so a Deliver-now button would promise the impossible.
    const html = QueuedCard(keepAliveHeld());
    // Act / Assert
    expect(html).not.toContain("data-queue-run-now");
  });

  it("offers no accept control, since there is no verdict to confirm", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(keepAliveHeld())).not.toContain("data-queue-accept");
  });

  it("keeps cancel available, which is the entry's one user-driven exit", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(keepAliveHeld())).toContain('data-queue-cancel="q1"');
  });

  it("never shows a classifier verdict on a keep-alive-held entry", () => {
    // Arrange — the classifier NEVER runs on such an entry by contract.
    const html = QueuedCard(keepAliveHeld({ classification: "hold" }));
    // Act / Assert
    expect(html).not.toContain("will run after this turn");
  });

  it("never shows the classifying badge on a keep-alive-held pending entry", () => {
    // Arrange / Act
    const html = QueuedCard(keepAliveHeld({ classification: "pending" }));
    // Assert
    expect(html).not.toContain("classifying");
  });

  it("never shows a rationale line on a keep-alive-held entry", () => {
    // Arrange / Act
    const html = QueuedCard(keepAliveHeld({ rationale: "independent work" }));
    // Assert
    expect(html).not.toContain("independent work");
  });

  it("joins the bubble to the turn whose completion releases it", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(keepAliveHeld())).toContain('data-keep-alive-turn-id="turn-9"');
  });

  it("renders the user's prompt text", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(keepAliveHeld({ text: "rerun the suite" }))).toContain("rerun the suite");
  });

  it("escapes markup in the prompt text", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(keepAliveHeld({ text: "<script>x</script>" }))).not.toContain("<script>");
  });

  it("lets the drain lease outrank the keep-alive hold when both are set", () => {
    // Arrange — the two cannot co-occur in practice (a keep-alive IS a turn,
    // and the drain lease stops turns starting), but the dispatch must still
    // be deterministic rather than depending on property order.
    const html = QueuedCard(keepAliveHeld({ shutdownHold: { scheduleId: "sched-1" } }));
    // Act / Assert
    expect(html).toContain("lease-card");
  });
});

/** A held prompt parked behind a pending compact-first revival. */
function revivalHeld(over: Partial<QueuedItem> = {}): QueuedItem {
  return queued({ revivalHold: {}, ...over });
}

describe("QueuedCard — revival dispatch", () => {
  it("renders the revival bubble for an entry a pending revival holds", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(revivalHeld())).toContain("revival-card");
  });

  it("renders the classifier bubble for an entry with no revival hold", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(queued())).not.toContain("revival-card");
  });

  it("is modeled on the keep-alive bubble, not the classifier one", () => {
    // Arrange — the classifier card would claim a verdict nothing produced.
    const html = QueuedCard(revivalHeld());
    // Act / Assert
    expect(html).toContain("queued-badge revival");
  });

  it("says it is waiting on the revival's compaction", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(revivalHeld())).toContain("waiting on the revival's compaction");
  });

  it("explains that the prompt is delivered once the compaction lands", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(revivalHeld())).toContain(REVIVAL_HELD_REASON);
  });

  it("offers NO force control, because a sleeping session has nowhere to deliver", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(revivalHeld())).not.toContain("data-queue-run-now");
  });

  it("offers no accept control, since there is no verdict to confirm", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(revivalHeld())).not.toContain("data-queue-accept");
  });

  it("keeps cancel available, which is the entry's one user-driven exit", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(revivalHeld())).toContain('data-queue-cancel="q1"');
  });

  it("never shows a classifier verdict on a revival-held entry", () => {
    // Arrange — the classifier NEVER runs on such an entry by contract.
    const html = QueuedCard(revivalHeld({ classification: "hold" }));
    // Act / Assert
    expect(html).not.toContain("will run after this turn");
  });

  it("never shows the classifying badge on a revival-held pending entry", () => {
    // Arrange / Act
    const html = QueuedCard(revivalHeld({ classification: "pending" }));
    // Assert
    expect(html).not.toContain("classifying");
  });

  it("never shows a rationale line on a revival-held entry", () => {
    // Arrange / Act
    const html = QueuedCard(revivalHeld({ rationale: "independent work" }));
    // Assert
    expect(html).not.toContain("independent work");
  });

  it("names no session, because the hold is a bare marker after the reshape", () => {
    // Arrange / Act / Assert — QueueEntryRevivalHold carries no session id, and
    // the workspace's owning session is WorkspaceState's to state, not this
    // card's to copy.
    expect(QueuedCard(revivalHeld())).not.toContain("data-revival-session-id");
  });

  it("renders the user's prompt text", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(revivalHeld({ text: "rerun the suite" }))).toContain("rerun the suite");
  });

  it("escapes markup in the prompt text", () => {
    // Arrange / Act / Assert
    expect(QueuedCard(revivalHeld({ text: "<script>x</script>" }))).not.toContain("<script>");
  });

  it("lets the drain lease outrank the revival hold when both are set", () => {
    // Arrange — deterministic dispatch rather than property order.
    const html = QueuedCard(revivalHeld({ shutdownHold: { scheduleId: "sched-1" } }));
    // Act / Assert
    expect(html).toContain("lease-card");
  });

  it("keys a revival bubble by its entry id, exactly as a queued card is keyed", () => {
    // Arrange / Act / Assert — the tail queue reconciles all kinds through one
    // key space.
    expect(queuedCardKey(revivalHeld())).toBe("queued:q1");
  });
});
