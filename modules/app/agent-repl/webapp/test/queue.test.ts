/**
 * The held-prompt queue card (E4): the subdued affordance for a prompt the
 * DAEMON is holding because a turn was already running. One edge per test.
 */
import { describe, expect, it } from "vitest";
import {
  LEASE_FORCE_TITLE,
  LEASE_HELD_REASON,
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
