/**
 * The shim's half of the shared `proto/ts/schema-literals.ts` contract.
 *
 * WHY THIS SUITE EXISTS ALONGSIDE THE WEBAPP'S. The accessors are shared
 * precisely so the shim and the webapp cannot answer differently, and the two
 * packages resolve modules differently (NodeNext here, a bundler there). A
 * module that is only ever exercised from the webapp would prove nothing about
 * whether the shim can load it, and the divergence would surface as a build
 * break the day someone first imports it in anger.
 */

import { describe, expect, it } from "vitest";
import {
  sessionCommandSpecs,
  syntheticModelLiteral,
} from "../../../../proto/ts/schema-literals.js";
import { SessionCommand } from "../../../../proto/gen/ts/agentshim/frontend/v1/slash-menu_pb.js";

describe("schema literals from the shim", () => {
  it("reads the model command's literal and argument form", () => {
    // Arrange, Act.
    const spec = sessionCommandSpecs().get(SessionCommand.MODEL);

    // Assert.
    expect(spec).toEqual({ literal: "/model", takesArgs: true });
  });

  it("reads a command that takes no argument", () => {
    // Arrange, Act.
    const spec = sessionCommandSpecs().get(SessionCommand.CLEAR);

    // Assert.
    expect(spec).toEqual({ literal: "/clear", takesArgs: false });
  });

  it("reads the synthetic model marker", () => {
    // Arrange, Act.
    const literal = syntheticModelLiteral();

    // Assert.
    expect(literal).toBe("<synthetic>");
  });
});
