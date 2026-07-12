import { describe, expect, it } from "vitest";
import { META_CLOSE, META_OPEN, stripMetaSpans } from "../src/meta.js";

/** One marked span, as the Emacs host composes it. */
const meta = (text: string) => `${META_OPEN}${text}${META_CLOSE}`;

describe("stripMetaSpans", () => {
  it("drops a marked read-directive, keeping the user's prompt", () => {
    const turn = `${meta("Before taking any action, read the file at /repo/metaprompt.md")}\n\nfix the bug`;
    expect(stripMetaSpans(turn)).toBe("fix the bug");
  });

  it("drops every marked span of a workspace-generation first send", () => {
    const turn = [
      meta("read the file at /repo/metaprompt.md"),
      "\n\n",
      meta("Do not wait for further instructions. Here is the task:\n\n"),
      "move the metaprompt into the repo",
      meta("\n\nWhen you have implemented it, invoke the /workspace-merge skill."),
    ].join("");
    expect(stripMetaSpans(turn)).toBe("move the metaprompt into the repo");
  });

  it("leaves a turn with no markers untouched", () => {
    expect(stripMetaSpans("just a prompt")).toBe("just a prompt");
  });

  it("returns the empty string for a turn that is nothing but meta", () => {
    expect(stripMetaSpans(meta("read the file"))).toBe("");
  });

  it("keeps user text that merely mentions the metaprompt", () => {
    const turn = `${meta("read the file")}\n\nwhy does the metaprompt say <!-- is fine?`;
    expect(stripMetaSpans(turn)).toBe("why does the metaprompt say <!-- is fine?");
  });

  it("leaves an unpaired opening marker verbatim rather than eating the prompt", () => {
    const turn = `${META_OPEN}truncated directive\n\nfix the bug`;
    expect(stripMetaSpans(turn)).toBe(`${META_OPEN}truncated directive\n\nfix the bug`);
  });
});
