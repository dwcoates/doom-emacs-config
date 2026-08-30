/**
 * WHAT COULD NOT BE CONVERTED, kept whole.
 *
 * Residue is what makes eager conversion at the edge a REVERSIBLE bet, so what
 * is pinned here is that each arm means a different follow-up — a converter, a
 * model, or a fix — and that the `kind` spelling is the one the sidecar mints
 * from the file plane. A divergence in that spelling makes one plane's residue
 * invisible to a search over the other's.
 */
import { describe, expect, it } from "vitest";
import {
  rawStruct,
  residueEntry,
  residueForMessage,
  residueKind,
  unknownResidue,
  unparsedResidue,
  vendorSpecificResidue,
} from "../../src/convert/residue.js";
import { foldContext } from "./fold-harness.js";

describe("rawStruct", () => {
  it("keeps the record entire and verbatim", () => {
    expect(rawStruct({ a: 1, b: { c: "x" } })).toEqual({ a: 1, b: { c: "x" } });
  });

  it("answers nothing for a record that cannot be represented at all", () => {
    const cyclic: Record<string, unknown> = {};
    cyclic.self = cyclic;

    expect(rawStruct(cyclic)).toBeUndefined();
  });

  it("wraps a bare scalar, which is not an object a Struct field can hold", () => {
    expect(rawStruct(7)).toEqual({ value: 7 });
  });
});

describe("residueKind — the CROSS-PLANE spelling", () => {
  it("spells a system record as system/<subtype>", () => {
    expect(residueKind({ type: "system", subtype: "notification" })).toBe("system/notification");
  });

  it("spells an attachment record as attachment/<type>", () => {
    expect(residueKind({ type: "attachment", subtype: "deferred_tools_delta" })).toBe(
      "attachment/deferred_tools_delta",
    );
  });

  it("spells any other record kind verbatim", () => {
    expect(residueKind({ type: "prompt_suggestion" })).toBe("prompt_suggestion");
  });

  it("names an unknown record rather than leaving it nameless", () => {
    expect(residueKind({})).toBe("unknown");
  });
});

describe("the three arms", () => {
  it("vendor_specific is UNDERSTOOD and deliberately not carried", () => {
    const residue = vendorSpecificResidue("system/notification", { type: "system" });

    expect(residue.unservedItem.case).toBe("vendorSpecific");
  });

  it("unknown carries the FIELD the discriminator was read from", () => {
    // A producer that looked in the wrong place and a genuinely new kind
    // otherwise produce the same entry.
    const residue = unknownResidue("newkind", "subtype", { type: "system" });
    const unknown = residue.unservedItem.value as { discriminatorField: string };

    expect(unknown.discriminatorField).toBe("subtype");
  });

  it("unparsed says WHY parsing failed, so it is investigable", () => {
    const residue = unparsedResidue("system/x", "no uuid", { type: "system" });
    const unparsed = residue.unservedItem.value as { parseError: string };

    expect(unparsed.parseError).toBe("no uuid");
  });

  it("unparsed states offset zero: a stream has no byte offset to name", () => {
    const residue = unparsedResidue("system/x", "broken", {});
    const unparsed = residue.unservedItem.value as { offset: bigint };

    expect(unparsed.offset).toBe(0n);
  });

  it("degrades an unrepresentable vendor_specific record to unparsed rather than losing it", () => {
    const cyclic: Record<string, unknown> = {};
    cyclic.self = cyclic;

    expect(vendorSpecificResidue("system/x", cyclic).unservedItem.case).toBe("unparsed");
  });
});

describe("residueForMessage", () => {
  it("is UNKNOWN for a kind no converter owns", () => {
    const residue = residueForMessage({ type: "system", subtype: "novel", uuid: "u" });

    expect(residue.unservedItem.case).toBe("unknown");
  });

  it("is UNPARSED when a converter recognized the record and then failed on it", () => {
    const residue = residueForMessage({ type: "assistant", uuid: "u" }, "no message id");

    expect(residue.unservedItem.case).toBe("unparsed");
  });
});

describe("residueEntry", () => {
  it("keys the row by what the record was and which record it was", () => {
    const entry = residueEntry(
      foldContext(),
      { type: "system", subtype: "notification", uuid: "uuid-1" },
      vendorSpecificResidue("system/notification", {}),
      "residue.vendor_specific",
    );

    expect(entry.upsertKey).toBe("residue:system/notification:uuid-1");
  });

  it("never marks residue keep-alive, which would lose WHY it is unserved", () => {
    const entry = residueEntry(
      foldContext({ keepalive: true }),
      { type: "system", subtype: "notification", uuid: "uuid-1" },
      vendorSpecificResidue("system/notification", {}),
      "residue.vendor_specific",
    );

    expect(entry.keepalive).toBe(false);
  });

  it("names a coordinate for a record that carries no uuid", () => {
    const entry = residueEntry(
      foldContext(),
      { type: "prompt_suggestion" },
      vendorSpecificResidue("prompt_suggestion", {}),
      "residue.vendor_specific",
    );

    expect(entry.source.vendorUuid).toBe("residue:prompt_suggestion");
  });
});
