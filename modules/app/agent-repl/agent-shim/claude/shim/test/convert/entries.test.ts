/**
 * The THREE ENVELOPES every unit frame is built from, and the rows they become.
 *
 * These are the shapes ~90 call sites share, so a mistake in one of them is a
 * mistake everywhere and invisible at each site. That is the whole reason they
 * are functions with a suite rather than `create()` calls spread around.
 */
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../../src/proto.js";
import {
  ACTIVITY_CONTRACT,
  activityEntry,
  agentActivity,
  agentFrame,
  pageLineEntry,
  prose,
  settledAt,
  sourceOf,
  startedAt,
  terminalEntry,
  textBlock,
  toolFailure,
  toolProgress,
  toolResultText,
  updateFrame,
} from "../../src/convert/entries.js";
import { foldContext, MAIN_AGENT } from "./fold-harness.js";

const UNIT = create(conversationv1.AgentActivityIdSchema, { value: "toolu_1" });

/** A read unit's start arm, as the simplest real item. */
function readStart(): conversationv1.AgentActivity["item"] {
  return {
    case: "read",
    value: create(conversationv1.AgentReadSchema, {
      result: {
        case: "start",
        value: create(conversationv1.AgentReadStartSchema, {
          path: create(conversationv1.ReadPathSchema, { path: "/tmp/a" }),
          startedAt: startedAt(5),
        }),
      },
    }),
  };
}

describe("instants", () => {
  it("carries a start as a wall-clock instant, never a duration", () => {
    expect(startedAt(1_700).atMs).toBe(1_700n);
  });

  it("truncates a fractional clock rather than rejecting it", () => {
    expect(settledAt(1_700.9, undefined).atMs).toBe(1_700n);
  });

  it("restates the start a settle closes, so the settled frame alone states a runtime", () => {
    expect(settledAt(1_700, 1_200).startedAt?.atMs).toBe(1_200n);
  });

  it("leaves the restated start UNSET when the settle's arm knows no start", () => {
    expect(settledAt(1_700, undefined).startedAt).toBeUndefined();
  });

  it("relays a progress beat as the instant the vendor last reported alive", () => {
    expect(toolProgress(2_500).lastProgressAtMs).toBe(2_500n);
  });
});

describe("content", () => {
  it("carries text verbatim, declaring no format", () => {
    expect(textBlock("# hi").text).toBe("# hi");
  });

  it("carries settled prose as markdown", () => {
    expect(prose("**done**").markdown).toBe("**done**");
  });

  it("wraps one string as a tool result's single text block", () => {
    const content = toolResultText("ok");

    expect(content.blocks).toHaveLength(1);
    expect(content.blocks[0]?.block.case).toBe("text");
  });
});

describe("toolFailure", () => {
  it("carries the account the tool gave and when it settled", () => {
    const failure = toolFailure(toolResultText("boom"), 9, 4);

    expect(failure.content?.blocks).toHaveLength(1);
    expect(failure.settledAt?.atMs).toBe(9n);
  });

  it("restates the failed call's start beside its settle instant", () => {
    expect(toolFailure(toolResultText("boom"), 9, 4).settledAt?.startedAt?.atMs).toBe(4n);
  });

  it("leaves content UNSET for a failure with no error content at all", () => {
    // Different from an empty text block: a consumer draws the failure with no
    // detail rather than an empty card.
    expect(toolFailure(undefined, 9, 4).content).toBeUndefined();
  });
});

describe("the activity envelope", () => {
  it("carries usage only when this is the unit that carries it", () => {
    const withUsage = agentActivity(UNIT, readStart(), {
      usage: create(conversationv1.TokenUsageSchema, { outputTokens: 3n }),
    });

    expect(withUsage.usage?.outputTokens).toBe(3n);
    expect(agentActivity(UNIT, readStart()).usage).toBeUndefined();
  });

  it("carries the unit's identity, so a later frame reaches the same one", () => {
    expect(agentActivity(UNIT, readStart()).activityId?.value).toBe("toolu_1");
  });

  it("carries effort on the same unit as usage, and no other", () => {
    const activity = agentActivity(UNIT, readStart(), {
      effort: conversationv1.AgentEffortLevel.HIGH,
    });

    expect(activity.effort).toBe(conversationv1.AgentEffortLevel.HIGH);
  });

  it("stamps the stands-alone contract on a start, where no per-kind code restates anything", () => {
    const activity = agentActivity(UNIT, readStart());

    expect(activity.contract).toBe(conversationv1.AgentActivityContract.SETTLES_STAND_ALONE);
  });

  it("stamps the stands-alone contract on a settle whatever its arm restated", () => {
    const bare: conversationv1.AgentActivity["item"] = {
      case: "read",
      value: create(conversationv1.AgentReadSchema, {
        result: { case: "failure", value: create(conversationv1.AgentReadFailureSchema, {}) },
      }),
    };

    const activity = agentActivity(UNIT, bare);

    expect(activity.contract).toBe(ACTIVITY_CONTRACT);
  });
});

describe("frames", () => {
  it("names the agent as the whole of a frame's attribution", () => {
    const frame = agentFrame(MAIN_AGENT, {
      case: "update",
      value: create(conversationv1.AgentUpdateSchema, {}),
    });

    expect(frame.agentId?.value).toBe("main-agent");
  });

  it("wraps an activity as read-only conversation content", () => {
    const frame = updateFrame(
      MAIN_AGENT,
      create(conversationv1.AgentUpdateSchema, {
        update: { case: "activity", value: agentActivity(UNIT, readStart()) },
      }),
    );

    expect(frame.result.case).toBe("update");
  });
});

describe("rows", () => {
  it("keys a unit's row by the UNIT, so every frame of it replaces one row", () => {
    const entry = activityEntry(
      foldContext(),
      { agentId: MAIN_AGENT, vendorUuid: "u", discriminator: "activity.read.start" },
      agentActivity(UNIT, readStart()),
    );

    expect(entry.upsertKey).toBe("activity:toolu_1");
  });

  it("refuses an activity with no identity, which could not be upserted", () => {
    expect(() =>
      activityEntry(
        foldContext(),
        { agentId: MAIN_AGENT, vendorUuid: "u", discriminator: "d" },
        create(conversationv1.AgentActivitySchema, { item: readStart() }),
      ),
    ).toThrow(/no identity/);
  });

  it("keys a terminal by agent AND vendor record, so each ending stays visible", () => {
    const entry = terminalEntry(
      foldContext(),
      { agentId: MAIN_AGENT, vendorUuid: "uuid-7", discriminator: "d" },
      { case: "success", value: create(conversationv1.AgentSuccessSchema, {}) },
    );

    expect(entry.upsertKey).toBe("terminal:main-agent:uuid-7");
  });

  it("marks a keep-alive turn's row never-served", () => {
    const entry = activityEntry(
      foldContext({ keepalive: true }),
      { agentId: MAIN_AGENT, vendorUuid: "u", discriminator: "d" },
      agentActivity(UNIT, readStart()),
    );

    expect(entry.keepalive).toBe(true);
  });

  it("lets a non-activity page line bring its own key", () => {
    const entry = pageLineEntry(
      foldContext(),
      { agentId: MAIN_AGENT, vendorUuid: "u", discriminator: "d" },
      "cut:uuid-1",
      create(conversationv1.AgentUpdateSchema, {}),
    );

    expect(entry.upsertKey).toBe("cut:uuid-1");
  });
});

describe("source coordinates", () => {
  it("omits the block index for a frame no block derived", () => {
    expect(sourceOf({ agentId: MAIN_AGENT, vendorUuid: "u", discriminator: "d" })).toEqual({
      vendorUuid: "u",
      discriminator: "d",
    });
  });

  it("carries the block index for a block-derived frame, so blocks do not collide", () => {
    expect(
      sourceOf({ agentId: MAIN_AGENT, vendorUuid: "u", blockIndex: 2, discriminator: "d" }),
    ).toEqual({ vendorUuid: "u", blockIndex: 2, discriminator: "d" });
  });

  it("keeps a block index of ZERO, which is a real position", () => {
    expect(
      sourceOf({ agentId: MAIN_AGENT, vendorUuid: "u", blockIndex: 0, discriminator: "d" })
        .blockIndex,
    ).toBe(0);
  });
});
