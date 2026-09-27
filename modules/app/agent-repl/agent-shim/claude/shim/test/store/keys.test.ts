/**
 * The store keys are a CROSS-PLANE contract: the sidecar mints the same key for
 * the same thing from the file plane, so a wrong spelling here does not fail
 * loudly — it produces a second row for a unit that already has one, and the
 * feed shows the same work twice. Every format is therefore asserted literally.
 */
import { createHash } from "node:crypto";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../src/proto.js";
import * as keys from "../../src/store/keys.js";

const activityId = (value: string): conversationv1.AgentActivityId =>
  create(conversationv1.AgentActivityIdSchema, { value });

describe("producerId", () => {
  it("names the writer by the ORIGINAL vendor session id", () => {
    // Arrange, Act.
    const producer = keys.producerId("vendor-original");

    // Assert.
    expect(producer).toBe("claude-shim:vendor-original");
  });

  it("refuses an empty session id rather than minting a colliding producer", () => {
    // Arrange, Act, Assert.
    expect(() => keys.producerId("")).toThrow(/original vendor session id is empty/);
  });
});

describe("activityUpsertKey", () => {
  it("keys a tool call by its vendor tool_use_id", () => {
    // Arrange, Act.
    const key = keys.activityUpsertKey(activityId("toolu_01ABC"));

    // Assert.
    expect(key).toBe("activity:toolu_01ABC");
  });

  it("keys a text block by its message id and 0-based block index", () => {
    // Arrange, Act.
    const key = keys.activityUpsertKey(activityId("msg_01XYZ:0"));

    // Assert.
    expect(key).toBe("activity:msg_01XYZ:0");
  });

  it("refuses an empty activity id", () => {
    // Arrange, Act, Assert.
    expect(() => keys.activityUpsertKey(activityId(""))).toThrow(/activity id is empty/);
  });
});

describe("promptUpsertKey", () => {
  it("keys the one served prompt row by its daemon-minted turn id", () => {
    // Arrange.
    const turn = create(conversationv1.TurnIdSchema, { value: "turn-9" });

    // Act.
    const key = keys.promptUpsertKey(turn);

    // Assert.
    expect(key).toBe("prompt:turn-9");
  });
});

describe("peerUpsertKey", () => {
  it("keys a peer message by the vendor record uuid (the cross-plane contract)", () => {
    // Arrange, Act.
    const key = keys.peerUpsertKey("uuid-XYZ");

    // Assert.
    expect(key).toBe("peer:uuid-XYZ");
  });
});

describe("questionUpsertKey", () => {
  it("keys a question by the AskUserQuestion call's own tool_use_id", () => {
    // Arrange.
    const question = create(conversationv1.AgentQuestionIdSchema, { value: "toolu_ask" });

    // Act.
    const key = keys.questionUpsertKey(question);

    // Assert.
    expect(key).toBe("question:toolu_ask");
  });
});

describe("permissionUpsertKey", () => {
  it("keys consent by the GATED call, so it joins the work it gates", () => {
    // Arrange.
    const permission = create(conversationv1.AgentPermissionIdSchema, { value: "toolu_bash" });

    // Act.
    const key = keys.permissionUpsertKey(permission);

    // Assert.
    expect(key).toBe("permission:toolu_bash");
  });
});

describe("terminalUpsertKey", () => {
  it("keys a terminal by agent AND vendor record, so repeated endings both survive", () => {
    // Arrange.
    const agent = create(conversationv1.AgentIdSchema, { value: "agent-main" });

    // Act.
    const key = keys.terminalUpsertKey(agent, "uuid-3");

    // Assert.
    expect(key).toBe("terminal:agent-main:uuid-3");
  });

  it("gives two endings of ONE agent two different keys", () => {
    // Arrange.
    const agent = create(conversationv1.AgentIdSchema, { value: "agent-main" });

    // Act.
    const first = keys.terminalUpsertKey(agent, "uuid-1");
    const second = keys.terminalUpsertKey(agent, "uuid-2");

    // Assert.
    expect(first).not.toBe(second);
  });

  it("refuses a terminal with no vendor record to name it", () => {
    // Arrange.
    const agent = create(conversationv1.AgentIdSchema, { value: "agent-main" });

    // Act, Assert.
    expect(() => keys.terminalUpsertKey(agent, "")).toThrow(/vendor record uuid is empty/);
  });
});

describe("bashStartUpsertKey", () => {
  it("keys a detached shell's START row by the RUN's activity id, in the sidecar's spelling", () => {
    // Arrange, Act.
    const key = keys.bashStartUpsertKey(activityId("toolu_bash1"));

    // Assert. `bash:<run>:start` is the sidecar's `BashStartKey`: one fact,
    // one key, whichever plane writes it.
    expect(key).toBe("bash:toolu_bash1:start");
  });

  it("does not collide with the same run's page-line key", () => {
    // Arrange.
    const run = activityId("toolu_bash1");

    // Act, Assert.
    expect(keys.bashStartUpsertKey(run)).not.toBe(keys.activityUpsertKey(run));
  });

  it("keys the run's rendered tail as ONE row every write supersedes", () => {
    // Arrange, Act, Assert. Output beyond what is rendered is not stored, so
    // the tail has one key however often it grows.
    expect(keys.bashTailUpsertKey(activityId("toolu_bash1"))).toBe("bash:toolu_bash1:tail");
  });

  it("keys the terminal so it SUPERSEDES NOTHING the run produced", () => {
    // Arrange, Act, Assert.
    expect(keys.bashTerminalUpsertKey(activityId("toolu_bash1"))).toBe(
      "bash:toolu_bash1:terminal",
    );
  });

  it("gives the start, the tail and the terminal three distinct keys", () => {
    // Arrange.
    const run = activityId("toolu_bash1");

    // Act.
    const minted = new Set([
      keys.bashStartUpsertKey(run),
      keys.bashTailUpsertKey(run),
      keys.bashTerminalUpsertKey(run),
    ]);

    // Assert.
    expect(minted.size).toBe(3);
  });
});

describe("sessionUpsertKey", () => {
  it("keys a session fact by its arm and the vendor record that stated it", () => {
    // Arrange, Act.
    const key = keys.sessionUpsertKey("identity_rotated", "uuid-7");

    // Assert.
    expect(key).toBe("session:identity_rotated:uuid-7");
  });

  it("keeps two different facts from ONE vendor record as two rows", () => {
    // Arrange, Act.
    const rotated = keys.sessionUpsertKey("identity_rotated", "uuid-7");
    const model = keys.sessionUpsertKey("model_changed", "uuid-7");

    // Assert.
    expect(rotated).not.toBe(model);
  });
});

describe("formatSourceCoordinates", () => {
  it("is the vendor record uuid alone for a whole-message frame", () => {
    // Arrange, Act.
    const formatted = keys.formatSourceCoordinates({ vendorUuid: "uuid-1", discriminator: "d" });

    // Assert.
    expect(formatted).toBe("uuid-1");
  });

  it("appends the block index for a block-derived frame", () => {
    // Arrange, Act.
    const formatted = keys.formatSourceCoordinates({ vendorUuid: "uuid-1", blockIndex: 2, discriminator: "d" });

    // Assert.
    expect(formatted).toBe("uuid-1:2");
  });

  it("keeps block 0 distinct from the whole message", () => {
    // Arrange, Act.
    const block = keys.formatSourceCoordinates({ vendorUuid: "uuid-1", blockIndex: 0, discriminator: "d" });

    // Assert.
    expect(block).toBe("uuid-1:0");
  });

  it("refuses a negative block index", () => {
    // Arrange, Act, Assert.
    expect(() =>
      keys.formatSourceCoordinates({ vendorUuid: "uuid-1", blockIndex: -1, discriminator: "d" }),
    ).toThrow(/0-based integer/);
  });
});

describe("writeId", () => {
  it("hashes producer, source coordinates and arm path in that order", () => {
    // Arrange.
    const expected = createHash("sha256")
      .update("claude-shim:v1|uuid-1:0|agent_frame.update.activity", "utf8")
      .digest("hex");

    // Act.
    const id = keys.writeId("claude-shim:v1", {
      vendorUuid: "uuid-1",
      blockIndex: 0,
      discriminator: "agent_frame.update.activity",
    });

    // Assert.
    expect(id).toBe(expected);
  });

  it("mints the SAME id for a re-sent frame, so a retry is absorbed", () => {
    // Arrange.
    const mint = (): string =>
      keys.writeId("claude-shim:v1", {
        vendorUuid: "uuid-1",
        discriminator: "agent_frame.success",
      });

    // Act, Assert.
    expect(mint()).toBe(mint());
  });

  it("separates two frames the same vendor record produced", () => {
    // Arrange.
    const coordinates = { vendorUuid: "uuid-1" };

    // Act.
    const activity = keys.writeId("claude-shim:v1", {
      ...coordinates,
      discriminator: "agent_frame.update.activity",
    });
    const session = keys.writeId("claude-shim:v1", {
      ...coordinates,
      discriminator: "session_update.model_changed",
    });

    // Assert.
    expect(activity).not.toBe(session);
  });

  it("separates two blocks of one message", () => {
    // Arrange.
    const discriminator = "agent_frame.update.activity";

    // Act.
    const first = keys.writeId("claude-shim:v1", { vendorUuid: "u", blockIndex: 0, discriminator });
    const second = keys.writeId("claude-shim:v1", { vendorUuid: "u", blockIndex: 1, discriminator });

    // Assert.
    expect(first).not.toBe(second);
  });

  it("separates two producers writing the same vendor record", () => {
    // Arrange.
    const coordinates = { vendorUuid: "uuid-1", discriminator: "agent_frame.success" };

    // Act.
    const mine = keys.writeId("claude-shim:v1", coordinates);
    const theirs = keys.writeId("claude-shim:v2", coordinates);

    // Assert.
    expect(mine).not.toBe(theirs);
  });

  it("refuses a write with no arm path to discriminate it", () => {
    // Arrange, Act, Assert.
    expect(() => keys.writeId("claude-shim:v1", { vendorUuid: "u", discriminator: "" })).toThrow(
      /arm path is empty/,
    );
  });
});

describe("the cross-plane key spellings", () => {
  it("spells a context-budget warning as session:context_budget_warning:<uuid>", () => {
    // BOTH PLANES produce this fact from one transcript line, and write_id
    // dedup collapses them into one row only if the key bytes match.
    expect(keys.contextBudgetWarningUpsertKey("11111111-2222-4333-8444-555555555555")).toBe(
      "session:context_budget_warning:11111111-2222-4333-8444-555555555555",
    );
  });

  it("spells a context cut as session:context_cut:<uuid>, the way the sidecar mints it", () => {
    // The stream's `compact_boundary` and the transcript's carry ONE uuid (see
    // testdata/captures/compaction-directed), so this is the one spelling that
    // lets the two planes' writes collapse onto one row instead of drawing the
    // divider twice.
    expect(keys.contextCutUpsertKey("11111111-2222-4333-8444-555555555555")).toBe(
      "session:context_cut:11111111-2222-4333-8444-555555555555",
    );
  });

  it("spells residue as residue:<uuid>, with no kind segment", () => {
    expect(keys.residueUpsertKey("11111111-2222-4333-8444-555555555555")).toBe(
      "residue:11111111-2222-4333-8444-555555555555",
    );
  });

  it("spells a uuid-less stream record's residue as residue:stream:<sequence>", () => {
    expect(keys.streamResidueUpsertKey(7)).toBe("residue:stream:7");
  });

  it("refuses residue with no vendor record uuid", () => {
    expect(() => keys.residueUpsertKey("")).toThrow();
  });
});
