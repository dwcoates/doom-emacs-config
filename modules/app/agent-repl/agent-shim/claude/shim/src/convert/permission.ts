/**
 * convert/permission.ts — the ONE permission shape the FOLD produces.
 *
 * # The division with the engine
 *
 * There is one vendor gate (`canUseTool`) and the ENGINE holds its callback. So
 * an ask that was actually PUT to the user — its `start` frame and the `success`
 * frame carrying the decision — is the engine's to produce: it is the only thing
 * that has the ask in hand, and answer validation is free there because the
 * pending callback is already held.
 *
 * What the fold produces is the shape that NEVER HAD AN OPEN ASK: the vendor's
 * own `permission_denied` record, emitted when a tool call is auto-denied by a
 * deny rule, by `dontAsk` mode, or by the auto-mode classifier. A consumer sees
 * `start` and the denial in one frame, or the denial alone.
 *
 * The fold consults `FoldContext.pendingAsk` before producing anything, so the
 * two can never double-produce one unit.
 *
 * # Why the permission's id IS the gated call's
 *
 * Consent joins to THE WORK IT GATES. A consumer drawing an allow decision needs
 * to place it against the call it allowed, and any other id would require a side
 * table to find it. It never collides with a question id because a question is
 * never a permission's gated call.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";
import type { SdkMessage } from "../sdk/types.js";
import { permissionUpsertKey } from "../store/keys.js";
import type { PersistEntry } from "../store/persistence.js";
import { pageLineEntry } from "./entries.js";
import type { FoldContext } from "./fold-context.js";
import { permissionId, toolCallActivityId } from "./ids.js";

const LOGGER = bindLog({ component: "shim-convert-permission", operation: "shim.convert.permission" });

/**
 * A policy denial: refused without asking anyone.
 *
 * `decider` and `reason` are the vendor's own discriminator and sentence, and
 * both are UNSET when it named none — absence is a legal answer here, never an
 * empty string.
 */
/**
 * The `decision_reason_type` that means the DECIDER could not answer.
 *
 * `sdk.d.ts` declares no discriminator separating "nobody could decide" from an
 * ordinary policy deny, so the vendor's own classifier label is the closest
 * producer there is -- a KNOWN-OPEN mapping, recorded as one.
 */
const UNDECIDED_DECIDER = "classifier";

function policyDenial(
  toolUseId: string,
  message: string,
  decider: string | undefined,
  reason: string | undefined,
): conversationv1.AgentPermission {
  return create(conversationv1.AgentPermissionSchema, {
    id: permissionId(toolUseId),
    gatedCall: toolCallActivityId(toolUseId),
    result: {
      case: "success",
      value: create(conversationv1.AgentPermissionSuccessSchema, {
        decision: {
          case: "denied",
          value: create(conversationv1.AgentPermissionDeniedSchema, {
            // NOBODY REFUSED is not the same fact as POLICY REFUSED. When the
            // vendor names the CLASSIFIER as the decider, the deciding
            // machinery is what could not reach a verdict; drawing that as
            // policy implies a rule that does not exist, and it is the only
            // denial here that retrying may resolve. Every other discriminator
            // -- a rule, a mode -- is a judgement, and stays `policy`.
            by:
              decider === UNDECIDED_DECIDER
                ? {
                    case: "undecidable",
                    value: create(conversationv1.AgentPermissionDeniedForWantOfDeciderSchema, {
                      ...(reason === undefined ? {} : { detail: reason }),
                    }),
                  }
                : {
                    case: "policy",
                    value: create(conversationv1.AgentPermissionDeniedByPolicySchema, {
                      decider,
                      reason,
                      message,
                    }),
                  },
          }),
        },
      }),
    },
  });
}

/**
 * The vendor's `permission_denied` record.
 *
 * A DENIAL IS AN ANSWER, not a failure — which is why it settles the gate's
 * `success` arm — and a denied tool NEVER STARTS, so no activity frames follow.
 */
export function convertPermissionDenied(
  message: Extract<SdkMessage, { type: "system"; subtype: "permission_denied" }>,
  context: FoldContext,
): readonly PersistEntry[] {
  const toolUseId = message.tool_use_id;
  if (toolUseId === "") {
    LOGGER.debug(
      { uuid: message.uuid },
      "a permission denial named no gated call; no unit can be identified",
    );
    return [];
  }
  const pending = context.pendingAsk(toolUseId);
  if (pending !== undefined) {
    // THE ENGINE HOLDS THIS ASK. It produced the start and will produce the
    // decision from its own resolve; producing one here would be two producers
    // for one unit, disagreeing whenever they disagree.
    LOGGER.logVerbose(
      { tool_use_id: toolUseId, ask: pending.kind },
      "the engine's gate holds this ask; the fold produces nothing",
    );
    return [];
  }
  // A DENIAL INSIDE A SUBAGENT names the subagent on the vendor's own record,
  // which is the one place the stream plane does state an agent id.
  const agentId =
    message.agent_id === undefined || message.agent_id === ""
      ? context.mainAgentId
      : create(conversationv1.AgentIdSchema, { value: message.agent_id });
  LOGGER.debug(
    {
      tool: message.tool_name,
      tool_use_id: toolUseId,
      decider: message.decision_reason_type,
    },
    "policy denied a tool call without asking anyone",
  );
  return [
    pageLineEntry(
      context,
      {
        agentId,
        vendorUuid: message.uuid,
        discriminator: "agent_update.permission.success.denied.by_policy",
      },
      permissionUpsertKey(permissionId(toolUseId)),
      create(conversationv1.AgentUpdateSchema, {
        update: {
          case: "permission",
          value: policyDenial(
            toolUseId,
            message.message,
            message.decision_reason_type,
            message.decision_reason,
          ),
        },
      }),
    ),
  ];
}

