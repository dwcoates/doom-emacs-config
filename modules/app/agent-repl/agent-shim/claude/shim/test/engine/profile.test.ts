import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { profileFromProto } from "../../src/engine/profile.js";
import { shimv1 } from "../../src/proto.js";

describe("profileFromProto", () => {
  it.each([
    {
      name: "an agent profile",
      wire: create(shimv1.SessionProfileSchema, { kind: { case: "agent", value: {} } }),
      want: { kind: "agent" },
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
});
