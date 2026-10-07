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
import { HOME_TOKEN } from "../../scripts/capture/anonymize.mjs";

const UNIT = create(conversationv1.AgentActivityIdSchema, { value: "injected-1" });
const LAST_EDIT = {
  unit: create(conversationv1.AgentActivityIdSchema, { value: "toolu_edit" }),
  kind: "edit",
} as const;
const LAST_WRITE = {
  unit: create(conversationv1.AgentActivityIdSchema, { value: "toolu_write" }),
  kind: "write",
} as const;

/** One attachment fixture from the corpus, as the record the converter reads. */
function attachment(name: string): AttachmentRecord {
  return corpusLine(join("attachments", `${name}.jsonl`));
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
    // The recording names no one: its home is the capture scrub's token.
    expect(skills.skills[0]?.path).toBe(`${HOME_TOKEN}/.claude/skills`);
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
      foldContext({ lastChange: LAST_EDIT }),
      UNIT,
    );

    expect(activityOf(entries[0])?.activityId?.value).toBe("toolu_edit");
  });

  it("rides the edit arm when the last change was an edit", () => {
    const entries = convertAttachment(
      attachment("diagnostics"),
      foldContext({ lastChange: LAST_EDIT }),
      UNIT,
    );

    expect(activityOf(entries[0])?.item.case).toBe("edit");
  });

  it("carries the findings as the change unit's post-terminal consequence arm", () => {
    const entries = convertAttachment(
      attachment("diagnostics"),
      foldContext({ lastChange: LAST_EDIT }),
      UNIT,
    );

    const edit = activityOf(entries[0])?.item.value as conversationv1.AgentEdit;
    expect(edit.result.case).toBe("diagnostics");
  });

  it("carries each finding's file, message and zero-based lines", () => {
    const entries = convertAttachment(
      attachment("diagnostics"),
      foldContext({ lastChange: LAST_EDIT }),
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

describe("the retired context-budget warning", () => {
  it("records a context_budget_warning attachment as RESIDUE, never as an update", () => {
    // RULING (owner, 2026-10-06): no footer line warns that the context is
    // nearly full, so no converter owns the record and it lands as itself.
    const entries = convertAttachment(
      {
        type: "attachment",
        uuid: "uuid-tip",
        attachment: { type: "context_budget_warning", content: "the window is filling" },
      },
      foldContext(),
      UNIT,
    );

    expect(entries[0]?.item.kind).toBe("residue");
  });

  it("records a generic context_tip as RESIDUE", () => {
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

describe("the LSP severity vocabulary", () => {
  const CASES: readonly (readonly [string, conversationv1.AgentDiagnosticSeverity])[] = [
    ["warning", conversationv1.AgentDiagnosticSeverity.WARNING],
    ["information", conversationv1.AgentDiagnosticSeverity.INFORMATION],
    ["info", conversationv1.AgentDiagnosticSeverity.INFORMATION],
    ["hint", conversationv1.AgentDiagnosticSeverity.HINT],
  ];

  for (const [literal, expected] of CASES) {
    it(`maps the vendor's \`${literal}\` onto its own arm`, () => {
      expect(diagnosticSeverity(literal)).toBe(expected);
    });
  }
});

describe("reading one diagnostics report", () => {
  /** A diagnostics attachment carrying exactly the files a test wants read. */
  function report(files: unknown, uuid?: string): AttachmentRecord {
    return {
      type: "attachment",
      ...(uuid === undefined ? {} : { uuid }),
      attachment: { type: "diagnostics", files },
    };
  }

  /** The report the converter built, off the entry it produced. */
  function reportOf(record: AttachmentRecord): conversationv1.AgentDiagnosticsReport {
    const entries = convertAttachment(record, foldContext({ lastChange: LAST_EDIT }), UNIT);
    const edit = activityOf(entries[0])?.item.value as conversationv1.AgentEdit;
    return edit.result.value as conversationv1.AgentDiagnosticsReport;
  }

  it("carries no files at all when the record's `files` is not a list", () => {
    expect(reportOf(report("not-a-list")).files).toHaveLength(0);
  });

  it("drops a file entry that names no uri, rather than reporting a pathless file", () => {
    expect(reportOf(report([{ diagnostics: [] }])).files).toHaveLength(0);
  });

  it("drops a file entry whose uri is the empty string", () => {
    expect(reportOf(report([{ uri: "", diagnostics: [] }])).files).toHaveLength(0);
  });

  it("reads a file whose `diagnostics` is not a list as having no findings", () => {
    const files = reportOf(report([{ uri: "/a.ts", diagnostics: "none" }])).files;

    expect(files[0]?.path).toBe("/a.ts");
    expect(files[0]?.diagnostics).toHaveLength(0);
  });

  it("reads a non-numeric line as zero rather than inventing a position", () => {
    const files = reportOf(
      report([{ uri: "/a.ts", diagnostics: [{ range: { start: { line: "9" }, end: {} } }] }]),
    ).files;

    expect(files[0]?.diagnostics[0]?.startLine).toBe(0);
    expect(files[0]?.diagnostics[0]?.endLine).toBe(0);
  });

  it("truncates a fractional line rather than carrying it", () => {
    const files = reportOf(
      report([{ uri: "/a.ts", diagnostics: [{ range: { start: { line: 3.7 }, end: { line: 4.2 } } }] }]),
    ).files;

    expect(files[0]?.diagnostics[0]?.startLine).toBe(3);
    expect(files[0]?.diagnostics[0]?.endLine).toBe(4);
  });

  it("reads a finding with no message as an empty message, never as undefined", () => {
    const files = reportOf(report([{ uri: "/a.ts", diagnostics: [{ severity: "error" }] }])).files;

    expect(files[0]?.diagnostics[0]?.message).toBe("");
  });

  it("leaves a finding's source and code unset when the vendor stated neither as a string", () => {
    const files = reportOf(
      report([{ uri: "/a.ts", diagnostics: [{ message: "m", source: 7, code: 42 }] }]),
    ).files;

    expect(files[0]?.diagnostics[0]?.source).toBeUndefined();
    expect(files[0]?.diagnostics[0]?.code).toBeUndefined();
  });

  it("stands a uuid-less diagnostics record on the unit it attaches to", () => {
    const entries = convertAttachment(
      report([]),
      foldContext({ lastChange: LAST_EDIT }),
      UNIT,
    );

    expect(entries[0]?.source.vendorUuid).toBe("diagnostics:toolu_edit");
  });
});

describe("reading one injected-context record", () => {
  /** A context-injection attachment, exactly as the fixture would carry it. */
  function injectedRecord(attachment: Record<string, unknown>, uuid?: string): AttachmentRecord {
    return { type: "attachment", ...(uuid === undefined ? {} : { uuid }), attachment };
  }

  it("takes the memory file's path off the nested content when the record states none itself", () => {
    const entries = convertAttachment(
      injectedRecord({ type: "nested_memory", content: { path: "/nested/CLAUDE.md", content: "x" } }),
      foldContext(),
      UNIT,
    );

    const injected = activityOf(entries[0])?.item.value as conversationv1.AgentContextInjected;
    expect((injected.injected.value as conversationv1.AgentInjectedMemory).path).toBe(
      "/nested/CLAUDE.md",
    );
  });

  it("produces no unit at all when an injected memory file names no path", () => {
    const entries = convertAttachment(
      injectedRecord({ type: "nested_memory", content: { content: "x" } }),
      foldContext(),
      UNIT,
    );

    expect(entries).toHaveLength(0);
  });

  it("produces no unit when the memory file's path is the empty string", () => {
    const entries = convertAttachment(
      injectedRecord({ type: "nested_memory", path: "" }),
      foldContext(),
      UNIT,
    );

    expect(entries).toHaveLength(0);
  });

  it("reads a memory file with no string content as empty content, never as undefined", () => {
    const entries = convertAttachment(
      injectedRecord({ type: "nested_memory", path: "/CLAUDE.md" }),
      foldContext(),
      UNIT,
    );

    const injected = activityOf(entries[0])?.item.value as conversationv1.AgentContextInjected;
    expect((injected.injected.value as conversationv1.AgentInjectedMemory).content).toBe("");
  });

  it("reads an invoked-skills record whose `skills` is not a list as injecting no skills", () => {
    const entries = convertAttachment(
      injectedRecord({ type: "invoked_skills", skills: "one" }),
      foldContext(),
      UNIT,
    );

    const injected = activityOf(entries[0])?.item.value as conversationv1.AgentContextInjected;
    expect((injected.injected.value as conversationv1.AgentInjectedSkills).skills).toHaveLength(0);
  });

  it("names an invoked skill the empty string, and leaves its path and content unset, when none is a string", () => {
    const entries = convertAttachment(
      injectedRecord({ type: "invoked_skills", skills: [{ name: 1, path: 2, content: 3 }] }),
      foldContext(),
      UNIT,
    );

    const injected = activityOf(entries[0])?.item.value as conversationv1.AgentContextInjected;
    const skill = (injected.injected.value as conversationv1.AgentInjectedSkills).skills[0];
    expect(skill?.name).toBe("");
    expect(skill?.path).toBeUndefined();
    expect(skill?.content).toBeUndefined();
  });

  it("reads a dynamic-discovery delta whose `skillNames` is not a list as injecting no skills", () => {
    const entries = convertAttachment(
      injectedRecord({ type: "dynamic_skill", skillNames: "a" }),
      foldContext(),
      UNIT,
    );

    const injected = activityOf(entries[0])?.item.value as conversationv1.AgentContextInjected;
    expect((injected.injected.value as conversationv1.AgentInjectedSkills).skills).toHaveLength(0);
  });

  it("drops a non-string entry from a dynamic-discovery delta's listing", () => {
    const entries = convertAttachment(
      injectedRecord({ type: "dynamic_skill", skillNames: ["real", 7] }),
      foldContext(),
      UNIT,
    );

    const injected = activityOf(entries[0])?.item.value as conversationv1.AgentContextInjected;
    const skills = (injected.injected.value as conversationv1.AgentInjectedSkills).skills;
    expect(skills.map((skill) => skill.name)).toEqual(["real"]);
  });

  it("leaves a dynamic-discovery skill's path unset when the record named no directory", () => {
    const entries = convertAttachment(
      injectedRecord({ type: "dynamic_skill", skillNames: ["real"], skillDir: 7 }),
      foldContext(),
      UNIT,
    );

    const injected = activityOf(entries[0])?.item.value as conversationv1.AgentContextInjected;
    const skills = (injected.injected.value as conversationv1.AgentInjectedSkills).skills;
    expect(skills[0]?.path).toBeUndefined();
  });

  it("stands a uuid-less injection on the activity id the caller minted", () => {
    const entries = convertAttachment(
      injectedRecord({ type: "nested_memory", path: "/CLAUDE.md" }),
      foldContext(),
      UNIT,
    );

    expect(entries[0]?.source.vendorUuid).toBe("injected:injected-1");
  });
});

/**
 * THE WRITE ARM OF THE DIAGNOSTICS REPORT.
 *
 * `convertDiagnostics` puts the report on the remembered unit's OWN arm, and
 * the kind comes from `FoldContext.lastChange` rather than from a guess at the
 * call site. It used to be hardcoded to the edit arm, which attributed a new
 * file's findings to an edit that never happened.
 */
describe("IDE diagnostics after a Write", () => {
  it("rides the write arm rather than the edit arm", () => {
    const entries = convertAttachment(
      attachment("diagnostics"),
      foldContext({ lastChange: LAST_WRITE }),
      UNIT,
    );

    expect(activityOf(entries[0])?.item.case).toBe("write");
  });

  it("attaches the report to the write's own unit", () => {
    const entries = convertAttachment(
      attachment("diagnostics"),
      foldContext({ lastChange: LAST_WRITE }),
      UNIT,
    );

    expect(activityOf(entries[0])?.activityId?.value).toBe("toolu_write");
  });

  it("carries the findings as the write unit's post-terminal consequence arm", () => {
    const entries = convertAttachment(
      attachment("diagnostics"),
      foldContext({ lastChange: LAST_WRITE }),
      UNIT,
    );

    const write = activityOf(entries[0])?.item.value as conversationv1.AgentWrite;
    expect(write.result.case).toBe("diagnostics");
  });

  it("keys the row under the write arm, so an edit's row never absorbs it", () => {
    const entries = convertAttachment(
      attachment("diagnostics"),
      foldContext({ lastChange: LAST_WRITE }),
      UNIT,
    );

    expect(entries[0]?.source.discriminator).toBe("activity.write.diagnostics");
  });
});
