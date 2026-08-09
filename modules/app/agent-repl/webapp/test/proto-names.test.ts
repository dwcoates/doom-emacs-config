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
  HibernationDetailSchema,
  ReviveSessionCmdSchema,
} from "../../proto/gen/ts/agentshim/frontend/v1/gate-revival_pb";
import {
  QueueEntryKeepAliveHoldSchema,
  QueueEntryRevivalHoldSchema,
  QueueEntrySchema,
} from "../../proto/gen/ts/agentshim/frontend/v1/prompt-queue_pb";
import { PromptOriginSchema } from "../../proto/gen/ts/agentshim/core/v1/core_pb";
import {
  COMMAND_ARM,
  HIBERNATION_CAUSE,
  KEEP_ALIVE_HOLD_TURN_ID,
  PROMPT_ORIGIN_CACHE_KEEP_ALIVE,
  PROMPT_ORIGIN_UNSPECIFIED,
  PROMPT_ORIGIN_WEBAPP_CARD_ACTION,
  PROMPT_ORIGIN_WEBAPP_USER_SENT,
  QUEUE_ENTRY_KEEP_ALIVE_HOLD,
  QUEUE_ENTRY_REVIVAL_HOLD,
  REVIVAL_HOLD_SESSION_ID,
  REVIVE_MODE,
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

describe("queue-entry keep-alive keys", () => {
  it("names the queue entry's keep-alive hold field as the descriptor does", () => {
    // Arrange / Act / Assert — the field SELECTS the keep-alive bubble over the
    // classifier bubble, so a drifted key silently renders the wrong reason.
    expect(fieldJsonNames(QueueEntrySchema)).toContain(QUEUE_ENTRY_KEEP_ALIVE_HOLD);
  });

  it("names the hold's turn id as the descriptor does", () => {
    // Arrange / Act / Assert
    expect(fieldJsonNames(QueueEntryKeepAliveHoldSchema)).toEqual([KEEP_ALIVE_HOLD_TURN_ID]);
  });
});

describe("queue-entry revival keys", () => {
  it("names the queue entry's revival hold field as the descriptor does", () => {
    // Arrange / Act / Assert — the field SELECTS the revival bubble over the
    // classifier bubble, so a drifted key silently renders the wrong reason.
    expect(fieldJsonNames(QueueEntrySchema)).toContain(QUEUE_ENTRY_REVIVAL_HOLD);
  });

  it("names the hold's session id as the descriptor does", () => {
    // Arrange / Act / Assert
    expect(fieldJsonNames(QueueEntryRevivalHoldSchema)).toEqual([REVIVAL_HOLD_SESSION_ID]);
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
