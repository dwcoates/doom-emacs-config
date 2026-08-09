/**
 * failure-card — kind arm → SIDE → TONE, and the card the three produce.
 *
 * The side table is asserted against `proto/vocab/render-colors.json` itself,
 * which is what keeps this renderer from drifting away from the workspace color
 * the same failure paints. One edge per test.
 */
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FailureKindSchema } from "../../proto/gen/ts/agentshim/frontend/v1/errors_pb";
import fixtureRaw from "../../proto/vocab/render-colors.json?raw";
import {
  CONNECTIVITY_WINDOW_KINDS,
  FAILURE_SIDE_TONE,
  failureCardHtml,
  failureKindName,
  failureResolvedAtMs,
  failureSide,
  failureTone,
  failureToneClass,
} from "../src/failure-card.js";
import { FAILURE_KIND_ARMS, FAILURE_KIND_SIDE } from "../src/proto-names.js";
import type { FailureCardView, FailureKind } from "../src/frontend-proto.js";

const fixture = JSON.parse(fixtureRaw) as { error_classes: Record<string, string> };

/** A `FailureKind` with only its arm set — the side never reads a payload. */
function kind(arm: string): FailureKind {
  return create(FailureKindSchema, {
    kind: { case: arm, value: {} },
  } as Parameters<typeof create<typeof FailureKindSchema>>[1]);
}

function view(over: Partial<FailureCardView> = {}): FailureCardView {
  return {
    kind: kind("apiOverloaded"),
    message: "the API is overloaded",
    detail: "status=529",
    lifecycle: { case: "open" },
    ...over,
  };
}

describe("the side vocabulary", () => {
  it("assigns a side to EVERY arm the wire can carry", () => {
    // Arrange / Act — the record is keyed by the generated case union, so this
    // is the runtime half of a guarantee the type system already makes.
    const missing = FAILURE_KIND_ARMS.filter((arm) => FAILURE_KIND_SIDE[arm] === undefined);
    // Assert
    expect(missing).toEqual([]);
  });

  it("puts a daemon machinery arm on the machinery side", () => {
    // Arrange / Act / Assert
    expect(failureSide(kind("shimNotConnected"))).toBe("machinery");
  });

  it("puts a vendor arm on the vendor side", () => {
    // Arrange / Act / Assert
    expect(failureSide(kind("apiRateLimit"))).toBe("vendor");
  });

  it("puts a CLIENT-LOCAL arm on the machinery side", () => {
    // Arrange / Act / Assert — a frontend can only ever observe its own
    // plumbing failing.
    expect(failureSide(kind("daemonUnreachable"))).toBe("machinery");
  });

  it("throws on an arm it has no side for rather than picking one", () => {
    // Arrange — reaching this means a FailureKind was built somewhere that
    // skipped the decoder, which is itself the bug worth failing on.
    const rogue = { kind: { case: "somethingNew", value: {} } } as unknown as FailureKind;
    // Act / Assert
    expect(() => failureSide(rogue)).toThrow(/has no side in the vocabulary/);
  });

  it("throws on an UNSET kind rather than naming a side for it", () => {
    // Arrange
    const unset = { kind: { case: undefined } } as unknown as FailureKind;
    // Act / Assert
    expect(() => failureSide(unset)).toThrow(/sets no arm/);
  });
});

describe("the tone table and the shared color fixture", () => {
  it("gives machinery the color the fixture assigns ERROR_CLASS_INTERNAL", () => {
    // Arrange / Act — card color IS state color, from one table.
    // Assert
    expect(FAILURE_SIDE_TONE.machinery).toBe(fixture.error_classes.ERROR_CLASS_INTERNAL);
  });

  it("gives vendor the color the fixture assigns ERROR_CLASS_API", () => {
    // Arrange / Act / Assert
    expect(FAILURE_SIDE_TONE.vendor).toBe(fixture.error_classes.ERROR_CLASS_API);
  });

  it("resolves a vendor arm to the purple tone", () => {
    // Arrange / Act / Assert
    expect(failureTone(kind("apiRefusal"))).toBe("purple");
  });

  it("resolves a machinery arm to the blue tone", () => {
    // Arrange / Act / Assert
    expect(failureTone(kind("shimDegraded"))).toBe("blue");
  });

  it("names the stylesheet class the vendor tone is drawn with", () => {
    // Arrange / Act / Assert
    expect(failureToneClass(kind("apiRefusal"))).toBe("failure-api");
  });

  it("names the stylesheet class the machinery tone is drawn with", () => {
    // Arrange / Act / Assert
    expect(failureToneClass(kind("shimDegraded"))).toBe("failure-internal");
  });
});

describe("the retracting connectivity windows", () => {
  it.each(CONNECTIVITY_WINDOW_KINDS)("names %s as an arm the wire really has", (arm) => {
    // Arrange / Act — a retraction rule keyed on an arm nothing can send would
    // silently never fire.
    // Assert
    expect(FAILURE_KIND_ARMS).toContain(arm);
  });

  it("leaves a store-write outage OUT, so its resolved card settles in place", () => {
    // Arrange / Act — dropped conversation is permanently gone; its record stays.
    // Assert
    expect(CONNECTIVITY_WINDOW_KINDS).not.toContain("shimStoreWriteRejected");
  });
});

describe("the lifecycle", () => {
  it("reports the stamp a resolved card closed at", () => {
    // Arrange / Act / Assert
    expect(
      failureResolvedAtMs(view({ lifecycle: { case: "resolved", resolvedAtMs: 1700000000000 } })),
    ).toBe(1700000000000);
  });

  it("reports no stamp for an open card", () => {
    // Arrange / Act / Assert
    expect(failureResolvedAtMs(view())).toBe(0);
  });

  it("reports no stamp for a TERMINAL card", () => {
    // Arrange / Act — terminal has no closing edge and never will.
    // Assert
    expect(failureResolvedAtMs(view({ lifecycle: { case: "terminal" } }))).toBe(0);
  });
});

describe("the card body", () => {
  it("renders the daemon's sentence verbatim", () => {
    // Arrange / Act
    const html = failureCardHtml(view(), "failure:e9");
    // Assert
    expect(html).toContain("the API is overloaded");
  });

  it("omits the detail block when the daemon gave none", () => {
    // Arrange / Act — the proto states outright that detail may be empty.
    const html = failureCardHtml(view({ detail: "" }), "failure:e9");
    // Assert
    expect(html).not.toContain("failure-detail");
  });

  it("carries the card's uuid, which is the address other surfaces reveal", () => {
    // Arrange / Act
    const html = failureCardHtml(view(), "failure:e9");
    // Assert
    expect(html).toContain('data-failure-uuid="failure:e9"');
  });

  it("carries the kind arm as data", () => {
    // Arrange / Act
    const html = failureCardHtml(view(), "failure:e9");
    // Assert
    expect(html).toContain('data-failure-kind="apiOverloaded"');
  });

  it("distinguishes terminal from open in the class list", () => {
    // Arrange / Act — an open card invites waiting and a terminal one does not.
    const html = failureCardHtml(view({ lifecycle: { case: "terminal" } }), "f1");
    // Assert
    expect(html).toContain("failure-terminal");
  });

  it("escapes the message", () => {
    // Arrange / Act
    const html = failureCardHtml(view({ message: "<img src=x>" }), "f1");
    // Assert
    expect(html).not.toContain("<img");
  });

  it("escapes the detail", () => {
    // Arrange / Act
    const html = failureCardHtml(view({ detail: "<script>x</script>" }), "f1");
    // Assert
    expect(html).not.toContain("<script>");
  });

  it("names the arm through the shared helper, not a local spelling", () => {
    // Arrange / Act / Assert
    expect(failureKindName(kind("apiMaxTurns"))).toBe("apiMaxTurns");
  });
});
