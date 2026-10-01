/**
 * Identity minting. A WRONG id here does not crash — it produces plausible,
 * silently wrong attribution: a tool result on another call's unit, a
 * subagent's frames drawn under the main agent, a book split in two at a
 * rotation. Each format is therefore asserted literally.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../src/proto.js";
import * as ids from "../../src/convert/ids.js";

describe("mainAgentId", () => {
  it("is the conversation's ORIGINAL vendor session id", () => {
    // Arrange, Act.
    const id = ids.mainAgentId("vendor-original");

    // Assert.
    expect(id.value).toBe("vendor-original");
  });

  it("refuses an empty session id", () => {
    // Arrange, Act, Assert.
    expect(() => ids.mainAgentId("")).toThrow(/original vendor session id is empty/);
  });
});

describe("subagentId", () => {
  it("is the vendor's own agent id, verbatim", () => {
    // Arrange, Act.
    const id = ids.subagentId("agent_01ABC");

    // Assert.
    expect(id.value).toBe("agent_01ABC");
  });
});

describe("toolCallActivityId", () => {
  it("is the tool_use_id, so the RESULT joins its call by equality", () => {
    // Arrange, Act.
    const id = ids.toolCallActivityId("toolu_01ABC");

    // Assert.
    expect(id.value).toBe("toolu_01ABC");
  });

  it("refuses an empty tool use id", () => {
    // Arrange, Act, Assert.
    expect(() => ids.toolCallActivityId("")).toThrow(/tool use id is empty/);
  });
});

describe("blockActivityId", () => {
  it("is <message id>:<block index>, 0-based", () => {
    // Arrange, Act.
    const id = ids.blockActivityId("msg_01XYZ", 0);

    // Assert.
    expect(id.value).toBe("msg_01XYZ:0");
  });

  it("gives two blocks of one message two identities", () => {
    // Arrange, Act.
    const first = ids.blockActivityId("msg_01XYZ", 0);
    const second = ids.blockActivityId("msg_01XYZ", 1);

    // Assert.
    expect(first.value).not.toBe(second.value);
  });

  it("refuses a negative block index", () => {
    // Arrange, Act, Assert.
    expect(() => ids.blockActivityId("msg_01XYZ", -1)).toThrow(/0-based integer/);
  });

  it("refuses a fractional block index", () => {
    // Arrange, Act, Assert.
    expect(() => ids.blockActivityId("msg_01XYZ", 1.5)).toThrow(/0-based integer/);
  });

  it("refuses a block with no message to belong to", () => {
    // Arrange, Act, Assert.
    expect(() => ids.blockActivityId("", 0)).toThrow(/api message id is empty/);
  });
});

describe("questionId", () => {
  it("is the AskUserQuestion call's own tool_use_id", () => {
    // Arrange, Act.
    const id = ids.questionId("toolu_ask");

    // Assert.
    expect(id.value).toBe("toolu_ask");
  });
});

describe("permissionId", () => {
  it("is the GATED call's tool_use_id, so consent joins the work it gates", () => {
    // Arrange, Act.
    const id = ids.permissionId("toolu_bash");

    // Assert.
    expect(id.value).toBe("toolu_bash");
  });
});

describe("detachedWorkId", () => {
  it("is the vendor task id, verbatim", () => {
    // Arrange, Act.
    const id = ids.detachedWorkId("b1a2b3");

    // Assert.
    expect(id.value).toBe("b1a2b3");
  });
});

describe("historyPointer", () => {
  it("carries the store's pointer through WITHOUT parsing it", () => {
    // Arrange, Act.
    const pointer = ids.historyPointer("{\"seq\":42}");

    // Assert.
    expect(pointer.value).toBe("{\"seq\":42}");
  });

  it("round-trips back to the store's own value", () => {
    // Arrange.
    const pointer = create(conversationv1.HistoryPointerSchema, { value: "opaque-1" });

    // Act.
    const back = ids.storeItemPointerValue(pointer);

    // Assert.
    expect(back).toBe("opaque-1");
  });

  it("refuses an empty pointer from the daemon", () => {
    // Arrange.
    const pointer = create(conversationv1.HistoryPointerSchema, { value: "" });

    // Act, Assert.
    expect(() => ids.storeItemPointerValue(pointer)).toThrow(/history pointer is empty/);
  });
});

describe("promptVendorUuid", () => {
  it("passes a turn id that is already a uuid through as its own vendor uuid", () => {
    // Arrange.
    const turn = "1b4e28ba-2fa1-11d2-883f-0016d3cca427";

    // Act.
    const uuid = ids.promptVendorUuid(turn);

    // Assert.
    expect(uuid).toBe(turn);
  });

  it("maps a daemon's 16-hex turn id to the version-5 uuid under the fixed namespace", () => {
    // Arrange. The expectation is Python's `uuid.uuid5(namespace, name)`, an
    // independent implementation of RFC 4122 §4.3.
    const turn = "0123456789abcdef";

    // Act.
    const uuid = ids.promptVendorUuid(turn);

    // Assert.
    expect(uuid).toBe("6dd0ffc6-4a38-5e10-a69b-93b2fd0d5b5f");
  });

  it("derives the same uuid every time for the same turn id", () => {
    // Arrange.
    const turn = "adopted-x";

    // Act.
    const first = ids.promptVendorUuid(turn);
    const second = ids.promptVendorUuid(turn);

    // Assert.
    expect([first, second]).toEqual(["cddc315c-f403-5457-8c93-aaa4c5f84b12", "cddc315c-f403-5457-8c93-aaa4c5f84b12"]);
  });

  it("refuses an empty turn id", () => {
    // Arrange, Act, Assert.
    expect(() => ids.promptVendorUuid("")).toThrow(/turn id is empty/);
  });
});
