import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { profileFromProto } from "../../src/engine/profile.js";
import { shimv1 } from "../../src/proto.js";

describe("profileFromProto", () => {
  it.each([
    {
      name: "an agent profile",
      wire: create(shimv1.SessionProfileSchema, { kind: { case: "agent", value: { standing: { kind: { case: "linkedWorkspace", value: {} } } } } }),
      want: { kind: "agent", standing: "linked_workspace" },
    },
    {
      name: "an agent profile in its repository's main workspace",
      wire: create(shimv1.SessionProfileSchema, {
        kind: { case: "agent", value: { standing: { kind: { case: "mainWorkspace", value: {} } } } },
      }),
      want: { kind: "agent", standing: "main_workspace" },
    },
    {
      name: "a chat profile with its prompt",
      wire: create(shimv1.SessionProfileSchema, { kind: { case: "chat", value: { systemPrompt: { text: "Answer." } } } }),
      want: { kind: "chat", systemPrompt: "Answer." },
    },
  ])("translates $name", ({ wire, want }) => {
    // Arrange, Act.
    const got = profileFromProto(wire);

    // Assert.
    expect(got).toEqual(want);
  });

  it.each([
    { name: "an unset profile", wire: undefined },
    { name: "a profile with no arm", wire: create(shimv1.SessionProfileSchema, {}) },
  ])("throws on $name, which validation refuses first", ({ wire }) => {
    // Arrange, Act, Assert.
    expect(() => profileFromProto(wire)).toThrow(/no session profile/);
  });

  it("throws on an agent profile with no standing, which validation refuses first", () => {
    // Arrange
    const wire = create(shimv1.SessionProfileSchema, { kind: { case: "agent", value: {} } });

    // Act, Assert
    expect(() => profileFromProto(wire)).toThrow(/names no workspace standing/);
  });
});
