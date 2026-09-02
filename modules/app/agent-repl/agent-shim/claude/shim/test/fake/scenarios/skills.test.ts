/**
 * The skill family. The unit settles on the DOCUMENT, not the two-field
 * acknowledgement, and injected context is a file-plane fact the vendor never
 * streams.
 */
import { describe, expect, it } from "vitest";

import { driveScenario, ofType, recordsOfType, toolUseResults, toolUses } from "../harness.js";

describe("a skill invocation", () => {
  it("acknowledges with success and the command name", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!skill"]);
    const ack = toolUseResults(driven.transcript())[0] as Record<string, unknown>;

    // Assert
    expect({ success: ack.success, name: ack.commandName }).toEqual({
      success: true,
      name: "fake-skill",
    });
  });

  it("carries the skill's ALLOWANCES on the acknowledgement and nowhere else", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!skill"]);
    const ack = toolUseResults(driven.transcript())[0] as { allowedTools: string[] };

    // Assert
    expect(ack.allowedTools).toHaveLength(2);
  });

  it("writes the DOCUMENT as an isMeta user record", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!skill"]);
    const meta = recordsOfType(driven.transcript(), "user").find((l) => l.isMeta === true);

    // Assert. A converter that settled on the acknowledgement would carry no
    // document at all.
    expect(meta).toBeDefined();
  });

  it("joins the document to its call by sourceToolUseID", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!skill"]);
    const meta = recordsOfType(driven.transcript(), "user").find((l) => l.isMeta === true);

    // Assert
    expect(meta?.sourceToolUseID).toBe((toolUses(driven)[0] as { id: string }).id);
  });

  it("writes NO document when the skill fails to resolve", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!skill-fail"]);

    // Assert
    expect(recordsOfType(driven.transcript(), "user").some((l) => l.isMeta === true)).toBe(false);
  });

  it("marks a failed skill's acknowledgement unsuccessful", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!skill-fail"]);
    const ack = toolUseResults(driven.transcript())[0] as { success: boolean };

    // Assert
    expect(ack.success).toBe(false);
  });
});

describe("a PARAMETERIZED skill invocation", () => {
  it("names the skill from the prompt's first argument", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!skill create-or-update-workspace merge"]);
    const call = toolUses(driven)[0] as { input: { skill: string } };

    // Assert
    expect(call.input.skill).toBe("create-or-update-workspace");
  });

  it("passes the rest of the prompt as the skill's args", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!skill create-or-update-workspace merge"]);
    const call = toolUses(driven)[0] as { input: { args: string } };

    // Assert
    expect(call.input.args).toBe("merge");
  });

  it("rejoins a MULTI-WORD args string", async () => {
    // Arrange + Act. `create feat/thing` is two tokens once split on
    // whitespace; the scenario must rejoin them rather than keeping only one.
    const driven = await driveScenario(["!skill create-or-update-workspace create feat/thing"]);
    const call = toolUses(driven)[0] as { input: { args: string } };

    // Assert
    expect(call.input.args).toBe("create feat/thing");
  });

  it("acknowledges under the named skill, not the default", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!skill create-or-update-workspace merge"]);
    const ack = toolUseResults(driven.transcript())[0] as { commandName: string };

    // Assert
    expect(ack.commandName).toBe("create-or-update-workspace");
  });

  it("derives the document body from the named skill", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!skill create-or-update-workspace merge"]);
    const meta = recordsOfType(driven.transcript(), "user").find((l) => l.isMeta === true);
    const text = ((meta?.message as { content: { text: string }[] }).content[0] as { text: string }).text;

    // Assert
    expect(text).toContain("/w/s/.claude/skills/create-or-update-workspace");
  });
});

describe("injected memory", () => {
  it("puts the attachment on the STREAM as well as in the transcript", async () => {
    // Attachments were modelled as a file-plane fact only, which left the
    // shim's whole attachment converter unreachable from production and its
    // residue rows unwritten. Both planes see the record; one uuid keeps the
    // two producers' rows collapsing into one.
    // Arrange + Act
    const driven = await driveScenario(["!memory"]);

    // Assert. `!memory` writes two: the nested memory and the file attachment.
    expect(ofType(driven, "attachment")).toHaveLength(2);
  });

  it("gives the streamed attachment the SAME uuid as its transcript line", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!memory"]);
    const streamed = ofType(driven, "attachment")[0] as { uuid?: string } | undefined;
    const recorded = recordsOfType(driven.transcript(), "attachment")[0];

    // Assert
    expect(streamed?.uuid).toBe(recorded?.uuid);
  });

  it("writes a nested_memory and a file attachment", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!memory"]);
    const types = recordsOfType(driven.transcript(), "attachment").map(
      (l) => (l.attachment as { type: string }).type,
    );

    // Assert
    expect(types).toEqual(["nested_memory", "file"]);
  });

  it("carries the memory file's body inside the attachment", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!memory"]);
    const nested = recordsOfType(driven.transcript(), "attachment")[0]?.attachment as {
      content: { content: string };
    };

    // Assert
    expect(nested.content.content).toBe("@./AGENTS.md\n");
  });
});

describe("injected skills", () => {
  it("writes all three skill attachment kinds", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!skills-injected"]);
    const types = recordsOfType(driven.transcript(), "attachment").map(
      (l) => (l.attachment as { type: string }).type,
    );

    // Assert
    expect(types).toEqual(["invoked_skills", "dynamic_skill", "skill_listing"]);
  });

  it("carries the invoked skill's body, which is what the model actually read", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!skills-injected"]);
    const invoked = recordsOfType(driven.transcript(), "attachment")[0]?.attachment as {
      skills: { content: string }[];
    };

    // Assert
    expect(invoked.skills[0]?.content).toContain("# Fake skill");
  });
});
