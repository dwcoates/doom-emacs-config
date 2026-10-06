/**
 * convert/attachments.ts — the vendor's ATTACHMENT records.
 *
 * # A GAP, stated here rather than discovered later
 *
 * The SDK's message union declares NO attachment message. Every record this file
 * converts — the injected memory file, the injected skills, the IDE diagnostics,
 * the tool-availability deltas — is written to the
 * vendor's TRANSCRIPT and reaches this system through the SIDECAR's file plane,
 * not through the stream the shim watches. The converters are here anyway, and
 * they are pure functions over the record's own shape, for three reasons: the
 * golden corpus can drive them, the sidecar and the shim then agree on one
 * mapping instead of two, and a vendor that ever does put one of these on the
 * stream is handled rather than residued.
 *
 * # Injection is INSTANTANEOUS
 *
 * One record, no lifecycle: the unit arrives whole. Several injections per turn
 * are routine, and everything injected stays resident until a clear or a
 * compaction drops it.
 *
 * # Diagnostics join by ADJACENCY
 *
 * The vendor's diagnostics record carries NO tool-call id, so it attaches to the
 * LAST write or edit unit — one remembered value, which the engine holds and
 * hands in on the context. With no such unit the record is residue, because a
 * diagnostics arm on a unit that does not exist is unbuildable.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";
import { activityUpsertKey } from "../store/keys.js";
import type { PersistEntry } from "../store/persistence.js";
import { agentActivity, updateFrame } from "./entries.js";
import type { FoldContext, LastChange } from "./fold-context.js";
import { residueEntry, residueForMessage, vendorSpecificResidue } from "./residue.js";

const LOGGER = bindLog({ component: "shim-convert-attach", operation: "shim.convert.attachments" });

/** An attachment record, read loosely before it is typed. */
export interface AttachmentRecord {
  readonly type?: string;
  readonly uuid?: string;
  readonly attachment?: Record<string, unknown>;
}

/**
 * The attachment kinds that are VENDOR BOOKKEEPING, never injected context.
 *
 * A tool-availability delta and an agent-type listing say what the model MAY
 * call; `context_injected` carries instructions the model READS. Conflating them
 * would put the vendor's own inventory in the user's feed. Both planes drop them
 * from every page under the same `attachment/<type>` spelling.
 */
export const BOOKKEEPING_ATTACHMENTS: ReadonlySet<string> = new Set([
  "deferred_tools_delta",
  "agent_listing_delta",
]);

/** The LSP severity vocabulary, from the vendor's own spelling. */
export function diagnosticSeverity(literal: unknown): conversationv1.AgentDiagnosticSeverity {
  switch (String(literal).toLowerCase()) {
    case "error":
      return conversationv1.AgentDiagnosticSeverity.ERROR;
    case "warning":
      return conversationv1.AgentDiagnosticSeverity.WARNING;
    case "information":
    case "info":
      return conversationv1.AgentDiagnosticSeverity.INFORMATION;
    case "hint":
      return conversationv1.AgentDiagnosticSeverity.HINT;
    default:
      // warn: a defect because an unknown diagnostic severity cannot be represented on the wire.
      LOGGER.warn(
        { severity: String(literal) },
        "the vendor named a diagnostic severity this contract does not spell",
      );
      return conversationv1.AgentDiagnosticSeverity.UNSPECIFIED;
  }
}

/** One file's findings, in the vendor's order. */
function diagnosticsFile(entry: unknown): conversationv1.AgentDiagnosticsFile | undefined {
  const record = entry as Record<string, unknown> | undefined;
  const path = record?.uri;
  if (typeof path !== "string" || path === "") return undefined;
  const raw = Array.isArray(record?.diagnostics) ? (record.diagnostics as Record<string, unknown>[]) : [];
  return create(conversationv1.AgentDiagnosticsFileSchema, {
    path,
    diagnostics: raw.map((finding) => {
      const range = finding.range as
        | { start?: { line?: unknown }; end?: { line?: unknown } }
        | undefined;
      const line = (value: unknown): number =>
        typeof value === "number" && Number.isFinite(value) ? Math.trunc(value) : 0;
      return create(conversationv1.AgentDiagnosticSchema, {
        severity: diagnosticSeverity(finding.severity),
        message: typeof finding.message === "string" ? finding.message : "",
        source: typeof finding.source === "string" ? finding.source : undefined,
        code: typeof finding.code === "string" ? finding.code : undefined,
        // CHARACTER PRECISION IS DELIBERATELY DROPPED: nothing draws columns.
        startLine: line(range?.start?.line),
        endLine: line(range?.end?.line),
      });
    }),
  });
}

/**
 * The IDE diagnostics report, as the last write/edit unit's consequence arm.
 *
 * A CONSEQUENCE, NOT A STATE: it arrives as a frame of that same unit FOLLOWING
 * the terminal, and the consumer applies it to the settled card. No frame ever
 * says "none are coming"; absence is simply no such frame.
 */
function convertDiagnostics(
  record: AttachmentRecord,
  context: FoldContext,
  change: LastChange,
): readonly PersistEntry[] {
  const { unit, kind } = change;
  const files = Array.isArray(record.attachment?.files) ? record.attachment.files : [];
  const report = create(conversationv1.AgentDiagnosticsReportSchema, {
    files: files
      .map(diagnosticsFile)
      .filter((file): file is conversationv1.AgentDiagnosticsFile => file !== undefined),
  });
  const item: conversationv1.AgentActivity["item"] = kind === "edit"
    ? {
        case: "edit",
        value: create(conversationv1.AgentEditSchema, {
          result: { case: "diagnostics", value: report },
        }),
      }
    : {
        case: "write",
        value: create(conversationv1.AgentWriteSchema, {
          result: { case: "diagnostics", value: report },
        }),
      };
  LOGGER.debug(
    { unit: unit.value, files: report.files.length, arm: kind },
    "attaching IDE diagnostics to the last change unit by adjacency",
  );
  const activity = agentActivity(unit, item);
  return [
    {
      agentId: context.mainAgentId,
      upsertKey: activityUpsertKey(unit),
      source: {
        vendorUuid: record.uuid ?? `diagnostics:${unit.value}`,
        discriminator: `activity.${kind}.diagnostics`,
      },
      keepalive: context.keepalive,
      turn: context.turnId,
      item: { kind: "frame", frame: updateFrame(context.mainAgentId, {
        ...create(conversationv1.AgentUpdateSchema, {
          update: { case: "activity", value: activity },
        }),
      }) },
    },
  ];
}

/**
 * Context the vendor SILENTLY pulled in, with no tool call announcing it.
 *
 * Surfaced so the user can see what shaped the agent's behavior. The unit has no
 * lifecycle: one record, whole.
 */
function convertContextInjected(
  record: AttachmentRecord,
  context: FoldContext,
  activityId: conversationv1.AgentActivityId,
): readonly PersistEntry[] {
  const attachment = record.attachment ?? {};
  const type = attachment.type;
  let injected: conversationv1.AgentContextInjected["injected"] | undefined;

  if (type === "nested_memory") {
    const content = attachment.content as { path?: unknown; content?: unknown } | undefined;
    const path = typeof attachment.path === "string" ? attachment.path : content?.path;
    if (typeof path !== "string" || path === "") {
      LOGGER.debug({}, "an injected memory file named no path; no unit is produced");
      return [];
    }
    injected = {
      case: "memory",
      value: create(conversationv1.AgentInjectedMemorySchema, {
        path,
        content: typeof content?.content === "string" ? content.content : "",
      }),
    };
  } else if (type === "invoked_skills") {
    const skills = Array.isArray(attachment.skills) ? (attachment.skills as Record<string, unknown>[]) : [];
    injected = {
      case: "skills",
      value: create(conversationv1.AgentInjectedSkillsSchema, {
        skills: skills.map((skill) =>
          create(conversationv1.AgentInjectedSkillSchema, {
            name: typeof skill.name === "string" ? skill.name : "",
            path: typeof skill.path === "string" ? skill.path : undefined,
            content: typeof skill.content === "string" ? skill.content : undefined,
          }),
        ),
      }),
    };
  } else if (type === "dynamic_skill" || type === "skill_listing") {
    const names = Array.isArray(attachment.skillNames) ? attachment.skillNames : [];
    const dir = typeof attachment.skillDir === "string" ? attachment.skillDir : undefined;
    injected = {
      case: "skills",
      value: create(conversationv1.AgentInjectedSkillsSchema, {
        skills: names
          .filter((name): name is string => typeof name === "string")
          .map((name) =>
            create(conversationv1.AgentInjectedSkillSchema, {
              name,
              // A DYNAMIC-DISCOVERY DELTA NAMES THE DIRECTORY, NOT EACH FILE, and
              // injects only the listing — so neither the path nor the content of
              // an individual skill is stated, and both stay unset.
              path: dir,
            }),
          ),
      }),
    };
  }

  if (injected === undefined) {
    LOGGER.debug(
      { attachment_type: String(type) },
      "no context-injection converter owns this attachment; it lands as residue",
    );
    return [
      residueEntry(
        context,
        record,
        vendorSpecificResidue(`attachment/${String(type)}`, record),
        `residue.vendor_specific.attachment.${String(type)}`,
      ),
    ];
  }

  LOGGER.debug({ attachment_type: String(type) }, "recording context the vendor injected silently");
  const activity = agentActivity(activityId, {
    case: "contextInjected",
    value: create(conversationv1.AgentContextInjectedSchema, { injected }),
  });
  return [
    {
      agentId: context.mainAgentId,
      upsertKey: activityUpsertKey(activityId),
      source: {
        vendorUuid: record.uuid ?? `injected:${activityId.value}`,
        discriminator: `activity.context_injected.${injected.case}`,
      },
      keepalive: context.keepalive,
      turn: context.turnId,
      item: {
        kind: "frame",
        frame: updateFrame(
          context.mainAgentId,
          create(conversationv1.AgentUpdateSchema, { update: { case: "activity", value: activity } }),
        ),
      },
    },
  ];
}

/**
 * Every attachment record, routed.
 *
 * `blockActivity` is the identity an injection unit takes; the caller mints it
 * from the record's own uuid, because an injection has no vendor id of its own
 * and no tool call to borrow one from.
 */
export function convertAttachment(
  record: AttachmentRecord,
  context: FoldContext,
  activityId: conversationv1.AgentActivityId,
): readonly PersistEntry[] {
  const type = record.attachment?.type;
  if (typeof type !== "string") {
    return [
      residueEntry(
        context,
        record,
        residueForMessage(record, "attachment record names no type"),
        "residue.unparsed",
      ),
    ];
  }
  if (BOOKKEEPING_ATTACHMENTS.has(type)) {
    LOGGER.logVerbose(
      { attachment_type: type },
      "vendor bookkeeping about what the model may call; residue, never context_injected",
    );
    return [
      residueEntry(
        context,
        record,
        vendorSpecificResidue(`attachment/${type}`, record),
        `residue.vendor_specific.attachment.${type}`,
      ),
    ];
  }
  if (type === "diagnostics") {
    const change = context.lastChange;
    if (change === undefined) {
      // warn: a defect because orphaned IDE diagnostics cannot be attached to the edit that caused them.
      LOGGER.warn(
        {},
        "IDE diagnostics arrived with no preceding write or edit; nothing to attach them to",
      );
      return [
        residueEntry(
          context,
          record,
          vendorSpecificResidue(`attachment/${type}`, record),
          `residue.vendor_specific.attachment.${type}`,
        ),
      ];
    }
    // WHICH ARM the report rides is the unit's own kind, and the fold knows the
    // last change was an edit or a write from the same remembered value.
    return convertDiagnostics(record, context, change);
  }
  // NO ATTACHMENT IS A CONTEXT-BUDGET WARNING (owner ruling, 2026-10-06): no
  // footer line warns that the context is nearly full, and the real vendor
  // never wrote a `context_budget_warning` record. A `context_tip` is a generic
  // CLI tip; it, like any record no converter owns, lands as residue below.
  if (type === "nested_memory" || type === "invoked_skills" || type === "dynamic_skill") {
    return convertContextInjected(record, context, activityId);
  }
  LOGGER.debug(
    { attachment_type: type },
    "no attachment converter owns this record; it lands as vendor-specific residue",
  );
  return [
    residueEntry(
      context,
      record,
      vendorSpecificResidue(`attachment/${type}`, record),
      `residue.vendor_specific.attachment.${type}`,
    ),
  ];
}
