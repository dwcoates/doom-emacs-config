import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { StructSchema, ValueSchema } from "@bufbuild/protobuf/wkt";
import { WatchFooterResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_footer_pb";
import {
  FooterAgentRowLabelSchema,
  FooterAgentRowSchema,
  FooterExpandedAgentsSchema,
  FooterViewSchema,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  assertNoUnknownFields,
  msOf,
  requireCase,
  requireMessage,
  unreachableArm,
} from "../../src/rpc/strict.js";

/** One unknown field, as protobuf-es preserves it off the wire. */
const UNKNOWN = [{ no: 999, wireType: 0, data: new Uint8Array([1]) }];

describe("assertNoUnknownFields", () => {
  it("accepts a message that carries only fields this build knows", () => {
    // ARRANGE
    const msg = create(WatchFooterResponseSchema, { footer: create(FooterViewSchema, {}) });
    // ACT / ASSERT
    expect(() => assertNoUnknownFields(WatchFooterResponseSchema, msg)).not.toThrow();
  });

  it("refuses an unknown field on the envelope itself", () => {
    const msg = create(WatchFooterResponseSchema, {});
    msg.$unknown = UNKNOWN;
    expect(() => assertNoUnknownFields(WatchFooterResponseSchema, msg)).toThrow(MalformedView);
  });

  it("names the envelope's own type in the refusal path", () => {
    const msg = create(WatchFooterResponseSchema, {});
    msg.$unknown = UNKNOWN;
    try {
      assertNoUnknownFields(WatchFooterResponseSchema, msg);
      expect.unreachable();
    } catch (err) {
      expect((err as MalformedView).path).toBe("agentrepl.v1.WatchFooterResponse");
    }
  });

  it("refuses an unknown field on a nested singular message", () => {
    const footer = create(FooterViewSchema, {});
    footer.$unknown = UNKNOWN;
    const msg = create(WatchFooterResponseSchema, { footer });
    expect(() => assertNoUnknownFields(WatchFooterResponseSchema, msg)).toThrow(MalformedView);
  });

  it("names the nested field in the refusal path", () => {
    const footer = create(FooterViewSchema, {});
    footer.$unknown = UNKNOWN;
    const msg = create(WatchFooterResponseSchema, { footer });
    try {
      assertNoUnknownFields(WatchFooterResponseSchema, msg);
      expect.unreachable();
    } catch (err) {
      expect((err as MalformedView).path).toBe("agentrepl.v1.WatchFooterResponse.footer");
    }
  });

  it("refuses an unknown field on a repeated message element", () => {
    const row = create(FooterAgentRowSchema, {});
    row.$unknown = UNKNOWN;
    const panel = create(FooterExpandedAgentsSchema, { rows: [row] });
    expect(() => assertNoUnknownFields(FooterExpandedAgentsSchema, panel)).toThrow(MalformedView);
  });

  it("names the repeated element's index in the refusal path", () => {
    const clean = create(FooterAgentRowSchema, {});
    const dirty = create(FooterAgentRowSchema, {});
    dirty.$unknown = UNKNOWN;
    const panel = create(FooterExpandedAgentsSchema, { rows: [clean, dirty] });
    try {
      assertNoUnknownFields(FooterExpandedAgentsSchema, panel);
      expect.unreachable();
    } catch (err) {
      expect((err as MalformedView).path).toBe("frontend.v1.FooterExpandedAgents.rows[1]");
    }
  });

  it("descends two levels into a repeated element's own message field", () => {
    const label = create(FooterAgentRowLabelSchema, { text: "Explore" });
    label.$unknown = UNKNOWN;
    const panel = create(FooterExpandedAgentsSchema, {
      rows: [create(FooterAgentRowSchema, { label })],
    });
    try {
      assertNoUnknownFields(FooterExpandedAgentsSchema, panel);
      expect.unreachable();
    } catch (err) {
      expect((err as MalformedView).path).toBe("frontend.v1.FooterExpandedAgents.rows[0].label");
    }
  });

  it("refuses an unknown field on a message map VALUE", () => {
    // ARRANGE: Struct is the contract's one map<string, message>.
    const value = create(ValueSchema, { kind: { case: "stringValue", value: "x" } });
    value.$unknown = UNKNOWN;
    const struct = create(StructSchema, { fields: { evidence: value } });
    // ACT / ASSERT
    expect(() => assertNoUnknownFields(StructSchema, struct)).toThrow(MalformedView);
  });

  it("names the map key in the refusal path", () => {
    const value = create(ValueSchema, { kind: { case: "stringValue", value: "x" } });
    value.$unknown = UNKNOWN;
    const struct = create(StructSchema, { fields: { evidence: value } });
    try {
      assertNoUnknownFields(StructSchema, struct);
      expect.unreachable();
    } catch (err) {
      expect((err as MalformedView).path).toBe("google.protobuf.Struct.fields[evidence]");
    }
  });

  it("refuses an unknown field inside a set oneof arm", () => {
    const inner = create(ValueSchema, { kind: { case: "numberValue", value: 1 } });
    inner.$unknown = UNKNOWN;
    expect(() => assertNoUnknownFields(ValueSchema, inner)).toThrow(MalformedView);
  });

  it("reports the offending field number, so the producer's field is findable", () => {
    const msg = create(WatchFooterResponseSchema, {});
    msg.$unknown = UNKNOWN;
    try {
      assertNoUnknownFields(WatchFooterResponseSchema, msg);
      expect.unreachable();
    } catch (err) {
      expect((err as MalformedView).detail).toContain("999");
    }
  });

  it("ignores an unset message field rather than walking a synthesized empty one", () => {
    const msg = create(WatchFooterResponseSchema, {});
    expect(() => assertNoUnknownFields(WatchFooterResponseSchema, msg)).not.toThrow();
  });
});

describe("requireMessage", () => {
  it("returns a set value unchanged", () => {
    const footer = create(FooterViewSchema, {});
    expect(requireMessage(footer, "WatchFooterResponse.footer")).toBe(footer);
  });

  it("refuses undefined, which is an unset non-optional message field", () => {
    expect(() => requireMessage(undefined, "WatchFooterResponse.footer")).toThrow(MalformedView);
  });

  it("carries the path into the refusal", () => {
    try {
      requireMessage(undefined, "WatchFooterResponse.footer");
      expect.unreachable();
    } catch (err) {
      expect((err as MalformedView).path).toBe("WatchFooterResponse.footer");
    }
  });

  it("refuses null, which no producer should send but a decoder could yield", () => {
    expect(() => requireMessage(null as unknown as string, "A.b")).toThrow(MalformedView);
  });
});

describe("requireCase", () => {
  it("returns the oneof when an arm is set", () => {
    const oneof = { case: "success" as const, value: {} };
    expect(requireCase(oneof, "R.result").case).toBe("success");
  });

  it("refuses an unset oneof", () => {
    expect(() => requireCase({ case: undefined }, "R.result")).toThrow(MalformedView);
  });

  it("refuses an absent oneof object", () => {
    expect(() => requireCase(undefined as unknown as { case?: string }, "R.result")).toThrow(MalformedView);
  });

  it("carries the path into the refusal", () => {
    try {
      requireCase({ case: undefined }, "R.result");
      expect.unreachable();
    } catch (err) {
      expect((err as MalformedView).path).toBe("R.result");
    }
  });
});

describe("unreachableArm", () => {
  it("throws rather than returning, so a switch default cannot fall through", () => {
    expect(() => unreachableArm("R.result", "somethingNew")).toThrow(MalformedView);
  });

  it("names the arm it could not draw", () => {
    try {
      unreachableArm("R.result", "somethingNew");
    } catch (err) {
      expect((err as MalformedView).detail).toContain("somethingNew");
    }
  });
});

describe("msOf", () => {
  it("converts an ordinary instant", () => {
    expect(msOf(1_700_000_000_000n, "FeedRow.at_ms")).toBe(1_700_000_000_000);
  });

  it("converts zero, which is a legitimate instant and not an absence", () => {
    expect(msOf(0n, "FeedRow.at_ms")).toBe(0);
  });

  it("converts the largest safe integer", () => {
    expect(msOf(BigInt(Number.MAX_SAFE_INTEGER), "A.b")).toBe(Number.MAX_SAFE_INTEGER);
  });

  it("refuses a value past the safe range rather than rounding the clock", () => {
    expect(() => msOf(BigInt(Number.MAX_SAFE_INTEGER) + 1n, "A.b")).toThrow(MalformedView);
  });

  it("refuses a negative value past the safe range", () => {
    expect(() => msOf(-BigInt(Number.MAX_SAFE_INTEGER) - 1n, "A.b")).toThrow(MalformedView);
  });

  it("converts a negative instant, which a clock skew can legitimately produce", () => {
    expect(msOf(-1000n, "A.b")).toBe(-1000);
  });
});
