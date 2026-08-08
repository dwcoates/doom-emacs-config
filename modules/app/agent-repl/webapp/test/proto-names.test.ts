/**
 * proto-names — the spelling table, asserted against the GENERATED DESCRIPTORS.
 *
 * The table's types already fail the build on a renamed oneof arm or field
 * (see the module header). What a type cannot see is the wire SPELLING: an enum
 * member's canonical name is not a TypeScript name, and a proto that renames a
 * field while keeping a compatible shape type-checks fine. These tests read the
 * committed descriptors and compare, so that drift fails the build too.
 *
 * One name per test.
 */
import { describe, expect, it } from "vitest";

import {
  FrontendCommandSchema,
} from "../../proto/gen/ts/agentshim/frontend/v1/frame_pb";
import {
  CompactionScopeSchema,
  HibernationDetailSchema,
  ReviveCompactFirstSchema,
  ReviveSessionCmdSchema,
} from "../../proto/gen/ts/agentshim/frontend/v1/gate-revival_pb";
import {
  QueueEntryKeepAliveHoldSchema,
  QueueEntryRevivalHoldSchema,
  QueueEntrySchema,
} from "../../proto/gen/ts/agentshim/frontend/v1/prompt-queue_pb";
import { PromptOriginSchema } from "../../proto/gen/ts/agentshim/core/v1/core_pb";
import { FailureKindSchema } from "../../proto/gen/ts/agentshim/frontend/v1/errors_pb";
import { FailureCardViewSchema } from "../../proto/gen/ts/agentshim/frontend/v1/failure-card_pb";
import { WorkspaceGateViewSchema } from "../../proto/gen/ts/agentshim/frontend/v1/gate-revival_pb";
import {
  COMMAND_ARM,
  FAILURE_CARD_LIFECYCLE_ARM,
  FAILURE_KIND_ARMS,
  FAILURE_KIND_SIDE,
  HIBERNATION_CAUSE,
  KEEP_ALIVE_HOLD_TURN_ID,
  PROMPT_ORIGIN_CACHE_KEEP_ALIVE,
  PROMPT_ORIGIN_UNSPECIFIED,
  PROMPT_ORIGIN_WEBAPP_CARD_ACTION,
  PROMPT_ORIGIN_WEBAPP_USER_SENT,
  QUEUE_CLASSIFICATION_ARM,
  QUEUE_HOLD_ARM,
  REVIVAL_HOLD_FIELDS,
  REVIVE_COMPACT_SCOPE,
  REVIVE_MODE,
  WORKSPACE_GATE_ARM,
  compactionScopeName,
} from "../src/proto-names.js";

/** The protojson keys of one generated oneof's arms, as the wire spells them. */
function oneofJsonNames(
  schema: { oneofs: readonly { name: string; fields: readonly { jsonName: string }[] }[] },
  oneof: string,
): string[] {
  const desc = schema.oneofs.find((o) => o.name === oneof);
  if (desc === undefined) throw new Error(`generated schema has no oneof '${oneof}'`);
  return desc.fields.map((f) => f.jsonName).sort();
}

/** The protojson keys of one generated message's fields. */
function fieldJsonNames(schema: { fields: readonly { jsonName: string }[] }): string[] {
  return schema.fields.map((f) => f.jsonName).sort();
}

/** Every canonical `PromptOrigin` member name the generated enum declares. */
const PROMPT_ORIGIN_NAMES = new Set(PromptOriginSchema.values.map((v) => v.name));

/** Every canonical `CompactionScope` member name the generated enum declares. */
const COMPACTION_SCOPE_NAMES = new Set(CompactionScopeSchema.values.map((v) => v.name));

describe("HIBERNATION_CAUSE: the gate's three arms, as the wire spells them", () => {
  it("covers exactly the generated cause arms", () => {
    // Arrange / Act
    const generated = oneofJsonNames(HibernationDetailSchema, "cause");
    // Assert — a fourth arm the table has not adopted would decode as an
    // unrecognized field and throw, which is the gate refusing to guess.
    expect(Object.values(HIBERNATION_CAUSE).sort()).toEqual(generated);
  });

  it("spells the idle-cutoff arm as the descriptor does", () => {
    // Arrange / Act / Assert
    expect(oneofJsonNames(HibernationDetailSchema, "cause")).toContain(
      HIBERNATION_CAUSE.idleCutoff,
    );
  });

  it("spells the forced arm as the descriptor does", () => {
    // Arrange / Act / Assert
    expect(oneofJsonNames(HibernationDetailSchema, "cause")).toContain(HIBERNATION_CAUSE.forced);
  });

  it("spells the cache-expired arm as the descriptor does", () => {
    // Arrange / Act / Assert
    expect(oneofJsonNames(HibernationDetailSchema, "cause")).toContain(
      HIBERNATION_CAUSE.cacheExpired,
    );
  });
});

describe("REVIVE_MODE: the revival decision's arm keys", () => {
  it("covers exactly the generated mode arms", () => {
    // Arrange / Act
    const generated = oneofJsonNames(ReviveSessionCmdSchema, "mode");
    // Assert — the arm key IS the decision, so a missing one is a mode the user
    // could not choose and an extra one is a command the daemon refuses.
    expect(Object.values(REVIVE_MODE).sort()).toEqual(generated);
  });
});

describe("COMMAND_ARM: the command arm keys the webapp sends", () => {
  it("names only arms the generated command oneof declares", () => {
    // Arrange
    const generated = new Set(oneofJsonNames(FrontendCommandSchema, "command"));
    // Act
    const unknown = Object.values(COMMAND_ARM).filter((arm) => !generated.has(arm));
    // Assert — a subset by design (workspace lifecycle is the Emacs frontend's),
    // so only INVENTED arms are a defect.
    expect(unknown).toEqual([]);
  });

  it("carries the hibernate arm the topbar's sleep verb sends", () => {
    // Arrange / Act / Assert
    expect(oneofJsonNames(FrontendCommandSchema, "command")).toContain(
      COMMAND_ARM.hibernateWorkspace,
    );
  });

  it("carries the revive arm the gate's two buttons send", () => {
    // Arrange / Act / Assert
    expect(oneofJsonNames(FrontendCommandSchema, "command")).toContain(COMMAND_ARM.reviveSession);
  });
});

describe("queue-entry classification arms", () => {
  it("covers exactly the generated classification arms", () => {
    // Arrange — the arm IS the verdict, so a missed arm is a verdict the
    // decoder would reject as an unset oneof.
    const generated = oneofJsonNames(QueueEntrySchema, "classification");
    // Act / Assert
    expect(Object.values(QUEUE_CLASSIFICATION_ARM).sort()).toEqual(generated);
  });
});

describe("queue-entry keep-alive keys", () => {
  it("names the queue entry's keep-alive hold arm as the descriptor does", () => {
    // Arrange / Act / Assert — the arm SELECTS the keep-alive bubble over the
    // classifier bubble, so a drifted key silently renders the wrong reason.
    expect(oneofJsonNames(QueueEntrySchema, "hold")).toContain(QUEUE_HOLD_ARM.keepAlive);
  });

  it("names the hold's turn id as the descriptor does", () => {
    // Arrange / Act / Assert
    expect(fieldJsonNames(QueueEntryKeepAliveHoldSchema)).toEqual([KEEP_ALIVE_HOLD_TURN_ID]);
  });
});

describe("queue-entry revival keys", () => {
  it("names the queue entry's revival hold arm as the descriptor does", () => {
    // Arrange / Act / Assert — the arm SELECTS the revival bubble over the
    // classifier bubble, so a drifted key silently renders the wrong reason.
    expect(oneofJsonNames(QueueEntrySchema, "hold")).toContain(QUEUE_HOLD_ARM.revival);
  });

  it("carries no fields, so the arm's presence is the whole claim", () => {
    // Arrange / Act / Assert — the retired session id was the only field; a
    // field ADDED back must fail here rather than be silently dropped.
    expect(fieldJsonNames(QueueEntryRevivalHoldSchema)).toEqual([...REVIVAL_HOLD_FIELDS]);
  });
});

describe("PromptOrigin names: the one part no type can spell", () => {
  it("builds the keep-alive origin name the generated enum declares", () => {
    // Arrange / Act / Assert — this name is the whole difference between "that
    // turn was expensive" and "the ping came back cold".
    expect(PROMPT_ORIGIN_NAMES.has(PROMPT_ORIGIN_CACHE_KEEP_ALIVE)).toBe(true);
  });

  it("builds the unspecified origin name the decoder refuses on", () => {
    // Arrange / Act / Assert
    expect(PROMPT_ORIGIN_NAMES.has(PROMPT_ORIGIN_UNSPECIFIED)).toBe(true);
  });

  it("builds the composer's origin name", () => {
    // Arrange / Act / Assert
    expect(PROMPT_ORIGIN_NAMES.has(PROMPT_ORIGIN_WEBAPP_USER_SENT)).toBe(true);
  });

  it("builds the card-action origin name", () => {
    // Arrange / Act / Assert
    expect(PROMPT_ORIGIN_NAMES.has(PROMPT_ORIGIN_WEBAPP_CARD_ACTION)).toBe(true);
  });
});

describe("the WorkspaceGateView.gate arms", () => {
  it("names only arms the generated gate oneof declares", () => {
    // Arrange / Act — a mis-spelled arm would make the decoder refuse a frame
    // the daemon considers well-formed.
    const generated = new Set(oneofJsonNames(WorkspaceGateViewSchema, "gate"));
    const unknown = Object.values(WORKSPACE_GATE_ARM).filter((arm) => !generated.has(arm));
    // Assert
    expect(unknown).toEqual([]);
  });

  it("names EVERY arm the generated gate oneof declares", () => {
    // Arrange / Act — a missing arm is a gate the client silently cannot read.
    const spelled = new Set<string>(Object.values(WORKSPACE_GATE_ARM));
    const missing = oneofJsonNames(WorkspaceGateViewSchema, "gate").filter(
      (arm) => !spelled.has(arm),
    );
    // Assert
    expect(missing).toEqual([]);
  });
});

describe("the FailureCardView.lifecycle arms", () => {
  it("names EVERY lifecycle arm the generated oneof declares", () => {
    // Arrange / Act — an unspelled arm would be rejected as "no lifecycle arm"
    // on a card the daemon considers complete.
    const spelled = new Set<string>(Object.values(FAILURE_CARD_LIFECYCLE_ARM));
    const missing = oneofJsonNames(FailureCardViewSchema, "lifecycle").filter(
      (arm) => !spelled.has(arm),
    );
    // Assert
    expect(missing).toEqual([]);
  });
});

describe("the FailureKind side table", () => {
  it("assigns a side to EVERY arm the generated oneof declares", () => {
    // Arrange / Act — an arm with no side is a failure whose color this end
    // would have to invent.
    const missing = oneofJsonNames(FailureKindSchema, "kind").filter(
      (arm) => FAILURE_KIND_SIDE[arm as keyof typeof FAILURE_KIND_SIDE] === undefined,
    );
    // Assert
    expect(missing).toEqual([]);
  });

  it("names no arm the generated oneof does not declare", () => {
    // Arrange / Act — the other direction: a stale arm here would be dead
    // vocabulary nothing can ever match.
    const generated = new Set(oneofJsonNames(FailureKindSchema, "kind"));
    const unknown = FAILURE_KIND_ARMS.filter((arm) => !generated.has(arm));
    // Assert
    expect(unknown).toEqual([]);
  });

  it("puts every daemon-minted arm on exactly one side", () => {
    // Arrange / Act — the machinery/vendor distinction is carried BY THE ARM,
    // so a side that was not one of the two would be a third reading.
    const sides = new Set(Object.values(FAILURE_KIND_SIDE));
    // Assert
    expect([...sides].sort()).toEqual(["machinery", "vendor"]);
  });
});

describe("CompactionScope names: what a revival compaction may swallow", () => {
  it("names the compact-first arm's scope field as the descriptor does", () => {
    // Arrange / Act / Assert — a drifted key is an unrecognized field the
    // daemon's decoder throws on, taking every compacting option out at once.
    expect(fieldJsonNames(ReviveCompactFirstSchema)).toEqual([REVIVE_COMPACT_SCOPE]);
  });

  it("builds the whole-conversation scope name", () => {
    // Arrange / Act / Assert
    expect(COMPACTION_SCOPE_NAMES.has(compactionScopeName("ALL"))).toBe(true);
  });

  it("builds the responses-only scope name", () => {
    // Arrange / Act / Assert
    expect(COMPACTION_SCOPE_NAMES.has(compactionScopeName("RESPONSES"))).toBe(true);
  });

  it("builds the prompts-only scope name", () => {
    // Arrange / Act / Assert
    expect(COMPACTION_SCOPE_NAMES.has(compactionScopeName("PROMPTS"))).toBe(true);
  });

  it("builds the prompts-and-responses scope name", () => {
    // Arrange / Act / Assert
    expect(COMPACTION_SCOPE_NAMES.has(compactionScopeName("PROMPTS_AND_RESPONSES"))).toBe(true);
  });

  it("builds the refused zero's name, which both ends must agree on", () => {
    // Arrange / Act / Assert — the daemon nacks it; the webapp must never send
    // it, and naming it is how that stays checkable.
    expect(COMPACTION_SCOPE_NAMES.has(compactionScopeName("UNSPECIFIED"))).toBe(true);
  });
});
