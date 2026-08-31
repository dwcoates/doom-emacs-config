/**
 * The ATTACHMENT converters, driven with the real attachment corpus.
 *
 * These records reach this system through the SIDECAR's file plane — the SDK's
 * message union declares no attachment message at all — so the fold never
 * dispatches to them and this suite is their only exercise. It is here anyway
 * because the mapping must be ONE mapping: the sidecar and the shim converting
 * the same record differently is the divergence the shared spelling exists to
 * prevent.
 */
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { conversationv1 } from "../../src/proto.js";
import {
  BOOKKEEPING_ATTACHMENTS,
  convertAttachment,
  diagnosticSeverity,
  type AttachmentRecord,
} from "../../src/convert/attachments.js";
import { activityOf, corpusLine, foldContext, residueOf } from "./fold-harness.js";
import { join } from "node:path";

const UNIT = create(conversationv1.AgentActivityIdSchema, { value: "injected-1" });
const LAST_CHANGE = create(conversationv1.AgentActivityIdSchema, { value: "toolu_edit" });

/** One attachment fixture from the corpus, as the record the converter reads. */
function attachment(name: string): AttachmentRecord {
  return corpusLine(join("attachments", `${name}.jsonl`)) as unknown as AttachmentRecord;
}

describe("injected context", () => {
  it("records a memory file the vendor pulled in silently", () => {
    const entries = convertAttachment(attachment("nested_memory"), foldContext(), UNIT);

    const injected = activityOf(entries[0])?.item.value as conversationv1.AgentContextInjected;
    expect(injected.injected.case).toBe("memory");
  });

  it("carries the memory file's path and content verbatim", () => {
    const entries = convertAttachment(attachment("nested_memory"), foldContext(), UNIT);

    const injected = activityOf(entries[0])?.item.value as conversationv1.AgentContextInjected;
    const memory = injected.injected.value as conversationv1.AgentInjectedMemory;
    expect(memory.path).toContain("CLAUDE.md");
    expect(memory.content).toBe("@./AGENTS.md\n");
  });

  it("records skills the vendor invoked without a tool call", () => {
    const entries = convertAttachment(attachment("invoked_skills"), foldContext(), UNIT);

    const injected = activityOf(entries[0])?.item.value as conversationv1.AgentContextInjected;
    const skills = injected.injected.value as conversationv1.AgentInjectedSkills;
    expect(skills.skills[0]?.name).toBe("create-or-update-workspace");
    expect(skills.skills[0]?.content).toBeDefined();
  });

  it("leaves a dynamic-discovery delta's per-skill content unset, since only a listing was injected", () => {
    const entries = convertAttachment(attachment("dynamic_skill"), foldContext(), UNIT);

    const injected = activityOf(entries[0])?.item.value as conversationv1.AgentContextInjected;
    const skills = injected.injected.value as conversationv1.AgentInjectedSkills;
    expect(skills.skills[0]?.content).toBeUndefined();
    expect(skills.skills[0]?.path).toBe("/Users/dodgecoates/.claude/skills");
  });

  it("is INSTANTANEOUS: one record, one row, no lifecycle", () => {
    const entries = convertAttachment(attachment("nested_memory"), foldContext(), UNIT);

    expect(entries).toHaveLength(1);
  });
});

describe("IDE diagnostics", () => {
  it("attaches the report to the last change unit, by adjacency", () => {
    const entries = convertAttachment(
      attachment("diagnostics"),
      foldContext({ lastWriteOrEditUnit: LAST_CHANGE }),
      UNIT,
    );

    expect(activityOf(entries[0])?.activityId?.value).toBe("toolu_edit");
  });

  it("carries the findings as the change unit's post-terminal consequence arm", () => {
    const entries = convertAttachment(
      attachment("diagnostics"),
      foldContext({ lastWriteOrEditUnit: LAST_CHANGE }),
      UNIT,
    );

    const edit = activityOf(entries[0])?.item.value as conversationv1.AgentEdit;
    expect(edit.result.case).toBe("diagnostics");
  });

  it("carries each finding's file, message and zero-based lines", () => {
    const entries = convertAttachment(
      attachment("diagnostics"),
      foldContext({ lastWriteOrEditUnit: LAST_CHANGE }),
      UNIT,
    );

    const edit = activityOf(entries[0])?.item.value as conversationv1.AgentEdit;
    const report = edit.result.value as conversationv1.AgentDiagnosticsReport;
    const finding = report.files[0]?.diagnostics[0];
    expect(report.files[0]?.path).toContain("render.ts");
    expect(finding?.message).toBe("Cannot find name 'monitoringRowHtml'.");
    expect(finding?.startLine).toBe(1100);
  });

  it("residues the report when no write or edit precedes it, rather than inventing a unit", () => {
    const entries = convertAttachment(attachment("diagnostics"), foldContext(), UNIT);

    expect(residueOf(entries[0])?.unservedItem.case).toBe("vendorSpecific");
  });

  it("maps the vendor's severity spelling onto the LSP vocabulary", () => {
    expect(diagnosticSeverity("Error")).toBe(conversationv1.AgentDiagnosticSeverity.ERROR);
  });

  it("refuses to guess a severity the contract does not spell", () => {
    expect(diagnosticSeverity("Catastrophe")).toBe(
      conversationv1.AgentDiagnosticSeverity.UNSPECIFIED,
    );
  });
});

describe("vendor bookkeeping", () => {
  it("residues a tool-availability delta rather than calling it injected context", () => {
    const entries = convertAttachment(attachment("deferred_tools_delta"), foldContext(), UNIT);

    expect(residueOf(entries[0])?.unservedItem.case).toBe("vendorSpecific");
  });

  it("spells the residue kind as attachment/<type>, the spelling the sidecar mints", () => {
    const entries = convertAttachment(attachment("deferred_tools_delta"), foldContext(), UNIT);

    const specific = residueOf(entries[0])?.unservedItem.value as { kind: string };
    expect(specific.kind).toBe("attachment/deferred_tools_delta");
  });

  it("names both bookkeeping kinds the cross-plane ruling covers", () => {
    expect([...BOOKKEEPING_ATTACHMENTS].sort()).toEqual([
      "agent_listing_delta",
      "deferred_tools_delta",
    ]);
  });
});

describe("the context-budget warning", () => {
  it("records the vendor's warning as a PAGE LINE of the agent's book", () => {
    // It exists only as a transcript attachment, so it is a file-plane fact
    // with a position in the conversation — SessionUpdate tag 24 was retired
    // because nothing on the live session stream ever produced one.
    const entries = convertAttachment(
      {
        type: "attachment",
        uuid: "uuid-tip",
        attachment: { type: "context_budget_warning", content: "the window is filling" },
      },
      foldContext(),
      UNIT,
    );

    const frame = entries[0]?.item.kind === "frame" ? entries[0].item.frame : undefined;
    const update = (frame?.result.value as conversationv1.AgentUpdate).update;
    expect(update.case).toBe("contextBudgetWarning");
    expect((update.value as conversationv1.ContextBudgetWarning).text).toBe(
      "the window is filling",
    );
  });

  it("keys each warning under the CROSS-PLANE session:<arm>:<uuid> spelling", () => {
    // Both planes produce this fact from one transcript line, and write_id
    // dedup collapses them into one row only if the key bytes match.
    const entries = convertAttachment(
      {
        type: "attachment",
        uuid: "uuid-tip",
        attachment: { type: "context_budget_warning", content: "the window is filling" },
      },
      foldContext(),
      UNIT,
    );

    expect(entries[0]?.upsertKey).toBe("session:context_budget_warning:uuid-tip");
  });

  it("records a generic context_tip as RESIDUE, never as the budget warning", () => {
    // RULING (landing 5): the one real `context_tip` capture is a generic
    // `/goal` tip, so drawing it as "your context is filling" would put a
    // sentence in the feed that the vendor never said about the context.
    const entries = convertAttachment(
      {
        type: "attachment",
        uuid: "uuid-tip",
        attachment: { type: "context_tip", tip: { tip: "try /goal" } },
      },
      foldContext(),
      UNIT,
    );

    expect(entries[0]?.item.kind).toBe("residue");
  });

  it("produces no update at all when the record carried no text", () => {
    const entries = convertAttachment(
      { type: "attachment", uuid: "uuid-tip", attachment: { type: "context_budget_warning" } },
      foldContext(),
      UNIT,
    );

    expect(entries).toHaveLength(0);
  });
});

describe("anything else", () => {
  it("residues an attachment kind no converter owns", () => {
    const entries = convertAttachment(attachment("task_reminder"), foldContext(), UNIT);

    expect(residueOf(entries[0])?.unservedItem.case).toBe("vendorSpecific");
  });

  it("records a record that names no attachment type as unparsed", () => {
    const entries = convertAttachment({ type: "attachment", uuid: "u" }, foldContext(), UNIT);

    // A record that names no type is a FAILURE to read, not a gap in the
    // model — so it lands unparsed, with the reason it could not be read.
    expect(residueOf(entries[0])?.unservedItem.case).toBe("unparsed");
  });
});
