// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import {
  AnswerPermissionErrorSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_answer_permission_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  CROSS_CUTTING_SENTENCES,
  refusalOf,
  type SentenceTable,
} from "../../src/feed/refusal-text.js";
import { armsOf } from "./arms.js";

/** A verb's own causes, for the table-merge assertions. */
const OWN: SentenceTable = {
  askNotStanding: () => "this ask is no longer standing",
} as unknown as SentenceTable;

describe("refusalOf", () => {
  const four = [
    {
      arm: "unknownWorkspace",
      cause: { case: "unknownWorkspace", value: {} },
      text: "unknown workspace",
    },
    {
      arm: "workspaceRefMismatch",
      cause: { case: "workspaceRefMismatch", value: { registryDir: "/elsewhere" } },
      text: "workspace ref mismatch — registry says /elsewhere",
    },
    {
      arm: "transferringAway",
      cause: { case: "transferringAway", value: { address: "127.0.0.1:9931" } },
      text: "this workspace is transferring to 127.0.0.1:9931",
    },
    {
      arm: "notYetAdopted",
      cause: { case: "notYetAdopted", value: {} },
      text: "the new daemon has not adopted this workspace yet",
    },
  ] as const;

  for (const c of four) {
    it(`words the cross-cutting ${c.arm} cause`, () => {
      expect(refusalOf(c.cause, OWN, "path").text).toBe(c.text);
    });

    it(`carries ${c.arm} as the refusal's arm`, () => {
      expect(refusalOf(c.cause, OWN, "path").arm).toBe(c.arm);
    });
  }

  it("prefers the verb's own wording for an arm it owns", () => {
    const said = refusalOf({ case: "askNotStanding", value: {} }, OWN, "path");
    expect(said.text).toBe("this ask is no longer standing");
  });

  it("refuses an error whose cause oneof is unset", () => {
    expect(() => refusalOf({ case: undefined }, OWN, "path")).toThrow(MalformedView);
  });

  it("refuses a cause a newer daemon added", () => {
    expect(() => refusalOf({ case: "quarantined", value: {} }, OWN, "path")).toThrow(
      MalformedView,
    );
  });
});

describe("the cross-cutting table", () => {
  it("words exactly the four causes every verb shares", () => {
    expect(Object.keys(CROSS_CUTTING_SENTENCES).sort()).toEqual(
      ["notYetAdopted", "transferringAway", "unknownWorkspace", "workspaceRefMismatch"].sort(),
    );
  });

  it("covers the four as the schema spells them", () => {
    const arms = armsOf(AnswerPermissionErrorSchema.oneofs, "cause");
    expect(arms).toEqual(expect.arrayContaining(Object.keys(CROSS_CUTTING_SENTENCES)));
  });
});
