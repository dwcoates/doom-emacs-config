/**
 * Tests for the shared `proto/ts/schema-literals.ts` accessors.
 *
 * The concrete strings are asserted LITERALLY on purpose: the point of moving
 * them into the schema is that they exist in exactly one place, and a test
 * that recomputed them from the same descriptor would agree with any spelling
 * at all.
 */

import { describe, expect, it } from "vitest";
import {
  selectedModel,
  sessionCommandSpecs,
  syntheticModelLiteral,
} from "../../proto/ts/schema-literals";
import {
  SessionCommand,
  SessionCommandSchema,
} from "../../proto/gen/ts/agentshim/frontend/v1/slash-menu_pb";

describe("sessionCommandSpecs", () => {
  const cases: ReadonlyArray<{
    name: string;
    command: SessionCommand;
    literal: string;
    takesArgs: boolean;
  }> = [
    {
      name: "model takes an inline argument",
      command: SessionCommand.MODEL,
      literal: "/model",
      takesArgs: true,
    },
    {
      // The exactness matters most here: mistaking "/clear the build cache"
      // for the command that DISCARDS THE CONVERSATION would destroy the
      // context the user was speaking into.
      name: "clear takes none",
      command: SessionCommand.CLEAR,
      literal: "/clear",
      takesArgs: false,
    },
    {
      name: "compact steers its summary",
      command: SessionCommand.COMPACT,
      literal: "/compact",
      takesArgs: true,
    },
    {
      name: "hyphenated literals survive the descriptor round trip",
      command: SessionCommand.OUTPUT_STYLE,
      literal: "/output-style",
      takesArgs: true,
    },
  ];

  for (const testCase of cases) {
    it(testCase.name, () => {
      // Arrange.
      const specs = sessionCommandSpecs();

      // Act.
      const spec = specs.get(testCase.command);

      // Assert.
      expect(spec).toEqual({ literal: testCase.literal, takesArgs: testCase.takesArgs });
    });
  }

  it("covers every command the wire can name", () => {
    // Arrange.
    const named = SessionCommandSchema.values.filter(
      (value) => value.number !== SessionCommand.UNSPECIFIED,
    );

    // Act.
    const specs = sessionCommandSpecs();

    // Assert.
    const missing = named
      .filter((value) => !specs.has(value.number as SessionCommand))
      .map((value) => value.name);
    expect(missing).toEqual([]);
  });

  it("omits UNSPECIFIED, which names no command", () => {
    // Arrange, Act.
    const specs = sessionCommandSpecs();

    // Assert.
    expect(specs.has(SessionCommand.UNSPECIFIED)).toBe(false);
  });

  it("carries a non-empty literal for every entry", () => {
    // Arrange, Act.
    const specs = sessionCommandSpecs();

    // Assert.
    const empty = [...specs.entries()]
      .filter(([, spec]) => spec.literal === "")
      .map(([command]) => command);
    expect(empty).toEqual([]);
  });
});

describe("syntheticModelLiteral", () => {
  it("is the marker the CLI reports when no real model is in play", () => {
    // Arrange, Act.
    const literal = syntheticModelLiteral();

    // Assert.
    expect(literal).toBe("<synthetic>");
  });
});

describe("selectedModel", () => {
  it("accepts a real model id", () => {
    // Arrange + Act + Assert.
    expect(selectedModel("claude-opus-5", "SetModel.selectedModel")).toBe("claude-opus-5");
  });

  it("refuses the synthetic marker", () => {
    // Arrange + Act + Assert — a picker offering the marker as a selectable
    // option was the concrete failure this type prevents.
    expect(() => selectedModel(syntheticModelLiteral(), "SessionView.model")).toThrow(/marker/);
  });

  it("refuses a surrounded synthetic marker", () => {
    // Arrange + Act + Assert — the marker arrives off a vendor stream, where
    // surrounding whitespace is ordinary.
    expect(() => selectedModel(` ${syntheticModelLiteral()}\n`, "SessionView.model")).toThrow(/marker/);
  });

  it("refuses an empty model", () => {
    // Arrange + Act + Assert — every frontend call site was promised a real
    // selection, so absence is a producer violation rather than "pin nothing".
    expect(() => selectedModel("", "SetModel.selectedModel")).toThrow(/absent, empty/);
  });

  it("names the field the violating producer filled", () => {
    // Arrange + Act + Assert — the throw must say WHICH producer broke the
    // contract, not only that one did.
    expect(() => selectedModel("", "SessionView.modelOptions[2].value")).toThrow(
      /SessionView\.modelOptions\[2\]\.value/,
    );
  });
});
