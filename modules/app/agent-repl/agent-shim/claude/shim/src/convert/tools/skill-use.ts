/**
 * convert/tools/skill-use.ts — a skill invocation, and the document it loaded.
 *
 * # The unit settles on the DOCUMENT, not on the acknowledgement
 *
 * The tool answers a skill invocation with "Launching skill: <name>" and a typed
 * output of `{ success, commandName }`. That carries no document, and
 * `AgentSkillUseSuccess.document` is not optional — a success frame built from
 * the acknowledgement would state that a document loaded while carrying none.
 *
 * So {@link skillUseConverter.settle} answers `undefined` for a successful
 * acknowledgement: the unit stays open, the call stays remembered, and
 * {@link skillDocumentSettle} concludes it when the injected context record
 * carrying the markdown arrives and joins back by its `sourceToolUseID`.
 *
 * A vendor-STATED error is different: nothing further will arrive, so an error
 * result settles the unit as a failure.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { settledAt, startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { arr, asRecord, failureOf, str } from "./support.js";

const LOGGER = bindLog({ component: "shim-convert-skill", operation: "shim.convert.skill" });

/**
 * The skill's name as the agent named it.
 *
 * The corpus spells the argument `skill`; `command` and `name` are read after it
 * because the vendor declares no `SkillInput` at all and the observed shape is
 * the only evidence there is.
 */
function skillNameOf(call: PendingCall): conversationv1.AgentSkillName | undefined {
  const name = str(call.input, "skill") ?? str(call.input, "command") ?? str(call.input, "name");
  if (name === undefined || name === "") {
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a skill invocation names no skill; no frame is produced, since the unit IS the skill",
    );
    return undefined;
  }
  return create(conversationv1.AgentSkillNameSchema, { name });
}

/** One skill-use item, whatever arm it carries. */
function skillItem(
  result: conversationv1.AgentSkillUse["result"],
): conversationv1.AgentActivity["item"] {
  return {
    case: "skillUse",
    value: create(conversationv1.AgentSkillUseSchema, { result }),
  };
}

/**
 * The invocation's conclusion, once the document has landed.
 *
 * Called by whatever joins the injected context record back to this call by its
 * `sourceToolUseID` — the acknowledgement itself never reaches here.
 *
 * `allowedTools` is UNSET when the skill declared no allowances, which the proto
 * distinguishes from declaring an empty set; pass `undefined` for the first and
 * an empty array for the second.
 */
export function skillDocumentSettle(
  call: PendingCall,
  document: string,
  allowedTools: readonly string[] | undefined,
  settledAtMs: number,
): conversationv1.AgentActivity["item"] | undefined {
  const skill = skillNameOf(call);
  if (skill === undefined) return undefined;
  LOGGER.logVerbose(
    { tool_use_id: call.toolUseId, skill: skill.name, markdown_length: document.length },
    "a skill's document landed; the invocation settles on it",
  );
  return skillItem({
    case: "success",
    value: create(conversationv1.AgentSkillUseSuccessSchema, {
      skill,
      document: create(conversationv1.AgentSkillDocumentSchema, { markdown: document }),
      allowedTools:
        allowedTools === undefined
          ? undefined
          : create(conversationv1.AgentSkillAllowedToolsSchema, { toolNames: [...allowedTools] }),
      settledAt: settledAt(settledAtMs, call.startedAtMs),
    }),
  });
}

/** The `Skill` tool: instructions loaded into THIS agent's context. */
export const skillUseConverter: ToolConverter = {
  kind: "skill_use",
  // `AgentSkillUse` declares the vendor's liveness beat as an arm of its own.
  carriesProgress: true,

  start(call) {
    const skill = skillNameOf(call);
    if (skill === undefined) return undefined;
    return skillItem({
      case: "start",
      value: create(conversationv1.AgentSkillUseStartSchema, {
        skill,
        // UNSET rather than "" when the skill was invoked bare, so "no
        // arguments" and "an empty argument" stay distinguishable.
        args: str(call.input, "args"),
        startedAt: startedAt(call.startedAtMs),
      }),
    });
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    if (outcome.isError) {
      LOGGER.logVerbose(
        { tool_use_id: call.toolUseId },
        "a skill could not be loaded; the invocation settles as a failure",
      );
      // THE SETTLED FRAME STANDS ALONE: it restates the skill, exactly as the
      // success does, since the start it upserts over is gone once it lands.
      // An invocation that named no skill had no start either, so it gets no
      // failure frame (skillNameOf says why at debug).
      const skill = skillNameOf(call);
      if (skill === undefined) return undefined;
      return skillItem({
        case: "failure",
        value: create(conversationv1.AgentSkillUseFailureSchema, { error: failureOf(call, outcome), skill }),
      });
    }
    LOGGER.debug(
      { tool_use_id: call.toolUseId },
      "a skill's acknowledgement carries no document; the invocation stays open until the document lands",
    );
    return undefined;
  },

  progress(beat) {
    return skillItem({ case: "progress", value: beat });
  },

  /**
   * The ALLOWANCES ride the acknowledgement and nothing else.
   *
   * The acknowledgement settles nothing, and the document record that does
   * settle the unit states no allowances of its own, so the declared set is
   * read here and carried on the re-remembered call. A non-array `allowedTools`
   * is not a declared set, so it stays UNSET rather than being read as empty.
   */
  retain(call, outcome) {
    const declared = arr(asRecord(outcome.structured), "allowedTools");
    if (declared === undefined) return call;
    const toolNames = declared.filter((name): name is string => typeof name === "string");
    if (toolNames.length !== declared.length) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId, declared: declared.length, read: toolNames.length },
        "a skill's acknowledgement declared allowances that are not tool names; only the named ones are carried",
      );
    }
    return { ...call, retainedAllowedTools: toolNames };
  },
};
