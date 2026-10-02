import { describe, expect, it } from "vitest";
import { RosterRowSchema } from "../../proto/gen/ts/frontend/v1/sidebar_pb";
import { FooterStatusSchema } from "../../proto/gen/ts/frontend/v1/footer_pb";
import {
  FeedShellSchema,
  FeedShellSettledSchema,
  FeedShellCompletedSchema,
  FeedSubagentSchema,
  FeedSubagentSettledSchema,
} from "../../proto/gen/ts/frontend/v1/feed_pb";
import renderColors from "../../proto/vocab/render-colors.json";
import paintClasses from "../../proto/vocab/paint-classes.json";
import { MalformedView } from "../src/rpc/malformed.js";
import { ForwardingLogger, setLogger } from "../src/log.js";
import {
  FEED_MERGE_HEAD_GLYPH,
  composerClosedFor,
  FEED_SHELL_DOT_KEYS,
  FEED_SUBAGENT_DOT_KEYS,
  feedShellDotColor,
  feedSubagentDotColor,
  PAINT_CLASS_NAMES,
  TOPBAR_TONES,
  failureSideColor,
  footerStatusColor,
  mergeGlyph,
  paintClass,
  protoArmName,
  rosterStatusColor,
  toneClass,
  topbarConnectivityColor,
  topbarTone,
} from "../src/vocab.js";

/** The proto (snake_case) arm names of the named oneof on a generated schema. */
function oneofArmNames(schema: { oneofs: readonly { name: string; fields: readonly { name: string }[] }[] }, oneof: string): string[] {
  const group = schema.oneofs.find((o) => o.name === oneof);
  if (group === undefined) throw new Error(`no oneof '${oneof}' on the schema`);
  return group.fields.map((f) => f.name).sort();
}

describe("protoArmName", () => {
  const cases: ReadonlyArray<[string, string]> = [
    ["idleAsync", "idle_async"],
    ["mergeEnqueuing", "merge_enqueuing"],
    ["vendorBlocked", "vendor_blocked"],
    ["ready", "ready"],
    ["startFailed", "start_failed"],
  ];
  for (const [generated, proto] of cases) {
    it(`spells ${generated} as ${proto}`, () => {
      expect(protoArmName(generated)).toBe(proto);
    });
  }
});

describe("roster_status is the RosterRow.status arm set, row for row", () => {
  it("names every arm the proto declares", () => {
    // ARRANGE / ACT
    const armsInProto = oneofArmNames(RosterRowSchema, "status");
    const keysInFile = Object.keys(renderColors.roster_status).sort();
    // ASSERT
    expect(keysInFile).toEqual(armsInProto);
  });

  it("resolves a color for every arm through the accessor", () => {
    const group = RosterRowSchema.oneofs.find((o) => o.name === "status");
    const unresolved = (group?.fields ?? [])
      .map((f) => f.localName)
      .filter((arm) => {
        try {
          rosterStatusColor(arm);
          return false;
        } catch {
          return true;
        }
      });
    expect(unresolved).toEqual([]);
  });
});

describe("footer_status is the FooterStatus.status arm set, row for row", () => {
  it("names every arm the proto declares", () => {
    const armsInProto = oneofArmNames(FooterStatusSchema, "status");
    const keysInFile = Object.keys(renderColors.footer_status).sort();
    expect(keysInFile).toEqual(armsInProto);
  });

  it("resolves a color for every arm through the accessor", () => {
    const group = FooterStatusSchema.oneofs.find((o) => o.name === "status");
    const unresolved = (group?.fields ?? [])
      .map((f) => f.localName)
      .filter((arm) => {
        try {
          footerStatusColor(arm);
          return false;
        } catch {
          return true;
        }
      });
    expect(unresolved).toEqual([]);
  });
});

describe("rosterStatusColor", () => {
  const cases: ReadonlyArray<[string, string]> = [
    ["init", "blue"],
    // A VENDOR FAULT IS TURQUOISE (owner ruling, 2026-10-02): the workspace
    // stays usable and its prompts are held until the vendor serves.
    ["vendorBlocked", "turquoise"],
    ["vendorFault", "turquoise"],
    ["apiRetrying", "turquoise"],
    // A NETWORK FAULT IS BLUE: nothing that needs the network can work.
    ["networkFault", "blue"],
    ["thinking", "red"],
    ["idleAsync", "yellow"],
    ["ready", "green"],
    ["merging", "purple"],
    ["turnFailed", "turquoise"],
    ["none", "none"],
  ];
  for (const [arm, color] of cases) {
    it(`paints ${arm} ${color}`, () => {
      expect(rosterStatusColor(arm)).toBe(color);
    });
  }

  it("refuses an arm the vocabulary does not carry", () => {
    expect(() => rosterStatusColor("hibernated")).toThrow(MalformedView);
  });
});

describe("footerStatusColor", () => {
  it("paints merging purple, the same as the roster's merging", () => {
    expect(footerStatusColor("merging")).toBe("purple");
    expect(rosterStatusColor("merging")).toBe("purple");
  });

  it("paints a failed turn turquoise, the same as the roster's turn_failed", () => {
    expect(footerStatusColor("turnFailed")).toBe("turquoise");
    expect(rosterStatusColor("turnFailed")).toBe("turquoise");
  });

  it("paints a degraded view turquoise, the same as the roster's degraded", () => {
    expect(footerStatusColor("degraded")).toBe("turquoise");
    expect(rosterStatusColor("degraded")).toBe("turquoise");
  });

  it("paints a vendor fault turquoise, the same as the roster's vendor_fault", () => {
    expect(footerStatusColor("vendorFault")).toBe("turquoise");
    expect(rosterStatusColor("vendorFault")).toBe("turquoise");
  });

  it("paints a network fault blue, the same as the roster's network_fault", () => {
    expect(footerStatusColor("networkFault")).toBe("blue");
    expect(rosterStatusColor("networkFault")).toBe("blue");
  });

  it("paints an agent-repl fault blue, the same as the roster's link arms", () => {
    expect(footerStatusColor("agentReplFault")).toBe("blue");
    expect(rosterStatusColor("severed")).toBe("blue");
  });

  it("refuses an arm the vocabulary does not carry", () => {
    expect(() => footerStatusColor("hibernating")).toThrow(MalformedView);
  });
});

describe("topbarConnectivityColor", () => {
  const cases: ReadonlyArray<[string, string]> = [
    ["connected", "green"],
    ["connecting", "blue"],
    ["severed", "blue"],
    ["dead", "blue"],
    ["noSession", "none"],
  ];
  for (const [arm, color] of cases) {
    it(`paints ${arm} ${color}`, () => {
      expect(topbarConnectivityColor(arm)).toBe(color);
    });
  }
});

describe("topbarTone", () => {
  for (const tone of TOPBAR_TONES) {
    it(`accepts the declared tone ${tone}`, () => {
      expect(topbarTone(tone)).toBe(tone);
    });
  }

  it("refuses teal, which the stale topbar.proto comment still lists", () => {
    expect(() => topbarTone("teal")).toThrow(MalformedView);
  });

  it("refuses an empty tone", () => {
    expect(() => topbarTone("")).toThrow(MalformedView);
  });
});

describe("mergeGlyph", () => {
  const cases: ReadonlyArray<[string, string]> = [
    ["mergeQueued", "queue"],
    ["merging", "recycle"],
    ["mergeFailed", "failed"],
    ["merged", "check"],
  ];
  for (const [arm, glyph] of cases) {
    it(`reports ${arm} with the ${glyph} glyph`, () => {
      expect(mergeGlyph(arm)).toBe(glyph);
    });
  }

  it("refuses a non-merge arm, which has no glyph to report with", () => {
    expect(() => mergeGlyph("ready")).toThrow(MalformedView);
  });

  it("names the feed merge head's own glyph", () => {
    expect(FEED_MERGE_HEAD_GLYPH).toBe("merge");
  });
});

describe("failureSideColor", () => {
  const cases: ReadonlyArray<[Parameters<typeof failureSideColor>[0], string]> = [
    ["machinery", "blue"],
    ["vendor", "purple"],
    ["client_local", "blue"],
  ];
  for (const [side, color] of cases) {
    it(`paints ${side} ${color}`, () => {
      expect(failureSideColor(side)).toBe(color);
    });
  }
});

describe("toneClass", () => {
  it("names the one class set the chassis defines", () => {
    expect(toneClass("blue")).toBe("tone-blue");
  });
});

describe("paintClass", () => {
  it("loads the whole syntax family", () => {
    expect(PAINT_CLASS_NAMES).toEqual(expect.arrayContaining(paintClasses.syntax));
  });

  it("loads the whole ansi family", () => {
    expect(PAINT_CLASS_NAMES).toEqual(expect.arrayContaining(paintClasses.ansi));
  });

  it("reads the empty string as plain, the wire's one spelling of unstyled", () => {
    expect(paintClass("")).toBeNull();
  });

  it("prefixes a syntax class", () => {
    expect(paintClass("keyword")).toBe("paint-keyword");
  });

  it("prefixes an ansi class", () => {
    expect(paintClass("ansi-fg-bright-red")).toBe("paint-ansi-fg-bright-red");
  });

  it("draws an unknown class unstyled rather than throwing", () => {
    expect(paintClass("nonesuch")).toBeNull();
  });

  it("warns once about an unknown class so the drift stays visible", () => {
    // ARRANGE
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
    // ACT
    paintClass("also-nonesuch");
    // ASSERT
    expect(lines.filter(([level]) => level === "warn")).toHaveLength(1);
  });
});

describe("failureSideColor: a side the color table does not name", () => {
  it("refuses a side with no row rather than painting it a default", () => {
    // ARRANGE / ACT / ASSERT
    expect(() => failureSideColor("nonesuch" as never)).toThrow(MalformedView);
  });

  it("names the vocab file and the side in the refusal", () => {
    // ARRANGE / ACT
    try {
      failureSideColor("nonesuch" as never);
      expect.unreachable();
    } catch (err) {
      // ASSERT
      expect((err as MalformedView).path).toBe("render-colors.json#failure_sides");
    }
  });
});

/**
 * THE FEED WORK DOTS (owner request, 2026-10-01): one row per head state, the
 * `live` arm plus every settled outcome arm, row for row against the schema.
 */
describe("feed_subagent_dot and feed_shell_dot are their heads' state arms, row for row", () => {
  /** `live` plus the settled outcome arms, the state the dot is keyed by. */
  const dotStates = (
    head: Parameters<typeof oneofArmNames>[0],
    settled: Parameters<typeof oneofArmNames>[0],
  ): string[] => [...oneofArmNames(head, "state").filter((arm) => arm !== "settled"), ...oneofArmNames(settled, "outcome")].sort();

  it("names every subagent head state", () => {
    expect([...FEED_SUBAGENT_DOT_KEYS].sort()).toEqual(dotStates(FeedSubagentSchema, FeedSubagentSettledSchema));
  });

  it("names every shell head state", () => {
    // A completed run's daemon judgment refines its key (`completed_failed`).
    const judged = oneofArmNames(FeedShellCompletedSchema, "judgment").map((arm) => `completed_${arm}`);
    expect([...FEED_SHELL_DOT_KEYS].sort()).toEqual([...dotStates(FeedShellSchema, FeedShellSettledSchema), ...judged].sort());
  });

  it("spends only the file's colors or none", () => {
    const allowed = ["none", ...renderColors.colors];
    const used = [...Object.values(renderColors.feed_subagent_dot), ...Object.values(renderColors.feed_shell_dot)];
    expect(used.filter((c) => !allowed.includes(c))).toEqual([]);
  });

  it.each([
    ["live", "green"],
    ["succeeded", "none"],
    ["failed", "red"],
    ["cancelled", "none"],
    ["lost", "blue"],
  ])("paints a subagent head %s %s", (state, color) => {
    expect(feedSubagentDotColor(state)).toBe(color);
  });

  it.each([
    ["live", "green"],
    ["completed", "none"],
    ["cancelled", "none"],
    ["lost", "blue"],
  ])("paints a shell head %s %s", (state, color) => {
    expect(feedShellDotColor(state)).toBe(color);
  });

  it("refuses a state the table does not name", () => {
    expect(() => feedSubagentDotColor("nonesuch")).toThrow(MalformedView);
  });
});

describe("composerClosedFor", () => {
  // THE FAULT DOMAINS (owner ruling, 2026-10-02): an agent-repl fault and a
  // network fault are blue and close the composer; a vendor fault is
  // turquoise and leaves it open, its prompts held until the vendor serves.
  it.each([
    ["an agent-repl fault", "agentReplFault", "severed", true],
    ["an impaired daemon", "agentReplFault", "daemonImpaired", true],
    ["a network fault", "networkFault", "offline", true],
    ["a vendor or account block", "vendorFault", "usageLimit", false],
    ["a vendor start being retried", "vendorFault", "vendorRetry", false],
    ["a vendor that refused to start", "vendorFault", "vendorRejection", false],
    ["a call the vendor retries", "vendorFault", "apiRetrying", false],
    ["a close under way", "closing", "blocked", true],
    ["a red status", "working", undefined, false],
  ])("%s", (_name, arm, substatus, closed) => {
    // Act
    const got = composerClosedFor(arm, substatus);

    // Assert
    expect(got).toBe(closed);
  });

  it("declares no composer-open exception", () => {
    // Assert
    expect(renderColors.composer_open_substatuses).toEqual({});
  });
});
