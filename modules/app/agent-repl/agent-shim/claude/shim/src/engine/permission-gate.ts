/**
 * engine/permission-gate.ts — `canUseTool`, and everything it owes.
 *
 * RESPONSIBILITY. Turn the vendor's `canUseTool` callback into an
 * `AgentPermission` (a gate on a tool call) or an `AgentQuestion` (an
 * AskUserQuestion), carry the consumer's decision back, and resolve the pending
 * promise.
 *
 * CALLBACK LIVENESS IS A HARD OBLIGATION. Every teardown path — interrupt,
 * KillTurn, KillSession, query death, SDK abort — resolves ALL pending
 * permission callbacks (as denied) BEFORE proceeding. An unresolved
 * `canUseTool` promise wedges the vendor process: it is waiting for an answer
 * that will never come, and nothing else can proceed past it.
 *
 * THE ANSWER BOUNDARY, UNDONE. The question tool keys answers by question TEXT
 * and comma-joins multi-selects (verified in the corpus: `toolUseResult.answers`
 * is `{ "<question text>": "<label>" }`). The shim undoes both here using the
 * ECHOED VALUES, validated against the pending callback it already holds — no
 * new state is introduced to do it. FREE-TEXT RESIDUE: whatever remains of the
 * joined answer string after every validated label is removed IS the typed free
 * text, so the SERIALIZATION here is that rule's inverse — the chosen labels in
 * order, then the free text, joined by `, `.
 *
 * AN ECHO THAT DOES NOT MATCH IS `UpdateAgentAnswerMismatch`, never a guess: a
 * mismatched echo means the consumer answered a question the shim is not
 * holding, and choosing an option on the user's behalf is the one outcome a
 * permission gate must never produce.
 */
import { create, equals } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";
import { permissionId, questionId, toolCallActivityId } from "../convert/ids.js";
import { permissionUpsertKey, questionUpsertKey } from "../store/keys.js";
import type { PersistEntry } from "../store/persistence.js";
import type { PermissionModeLike, PermissionResultLike, PermissionUpdateLike } from "../sdk/types.js";

const LOGGER = bindLog({ component: "shim-engine-permission", operation: "shim.engine.permission" });

/** The vendor's name for the one tool that is a question rather than an act. */
export const ASK_USER_QUESTION_TOOL = "AskUserQuestion";

/** How a joined multi-select answer is spelled, in both directions. */
const ANSWER_JOIN = ", ";

/**
 * How many denied calls are remembered.
 *
 * The memory exists only for the `tool_result` that arrives immediately after
 * a denial, so it has to outlive one message and nothing more; the bound is
 * what keeps it from becoming a second history of the session.
 */
const DENIED_MEMORY = 256;

// ---------------------------------------------------------------------------
// permission mode, both ways
// ---------------------------------------------------------------------------

/**
 * THE MODE A SESSION RUNS UNDER WHEN NOTHING STATES ONE (owner ruling
 * 2026-09-14: "the default permission mode should be auto for the SDK/shim").
 *
 * NOT the vendor's `default`. `auto` keeps the gate — a classifier decides
 * each ask rather than the user — so choosing it as the unstated mode drops no
 * consent; `default` remains a mode the vendor can REPORT, and both conversion
 * tables below carry it, but this shim never picks it for anyone.
 */
export const DEFAULT_PERMISSION_MODE: PermissionModeLike = "auto";

/** The vendor's mode word for a wire mode. Total: the two vocabularies are 1:1. */
export function toVendorPermissionMode(mode: conversationv1.AgentPermissionMode): PermissionModeLike {
  switch (mode.mode.case) {
    case "default":
      return "default";
    case "acceptEdits":
      return "acceptEdits";
    case "bypass":
      return "bypassPermissions";
    case "plan":
      return "plan";
    case "dontAsk":
      return "dontAsk";
    case "auto":
      return "auto";
    default: {
      // An unset oneof never reaches here: validate/ refuses it at the wire.
      throw new Error("shim permission gate: a permission mode with no arm set cannot be sent to the vendor");
    }
  }
}

/** The wire mode for a vendor mode word. */
export function fromVendorPermissionMode(mode: PermissionModeLike): conversationv1.AgentPermissionMode {
  const arm = (): conversationv1.AgentPermissionMode["mode"] => {
    switch (mode) {
      case "default":
        return { case: "default", value: create(conversationv1.AgentPermissionModeDefaultSchema, {}) };
      case "acceptEdits":
        return { case: "acceptEdits", value: create(conversationv1.AgentPermissionModeAcceptEditsSchema, {}) };
      case "bypassPermissions":
        return { case: "bypass", value: create(conversationv1.AgentPermissionModeBypassSchema, {}) };
      case "plan":
        return { case: "plan", value: create(conversationv1.AgentPermissionModePlanSchema, {}) };
      case "dontAsk":
        return { case: "dontAsk", value: create(conversationv1.AgentPermissionModeDontAskSchema, {}) };
      case "auto":
        return { case: "auto", value: create(conversationv1.AgentPermissionModeAutoSchema, {}) };
    }
  };
  return create(conversationv1.AgentPermissionModeSchema, { mode: arm() });
}

// ---------------------------------------------------------------------------
// standing permissions, as a typed echo token
// ---------------------------------------------------------------------------

const DESTINATIONS: Record<string, conversationv1.AgentPermissionDestination> = {
  userSettings: conversationv1.AgentPermissionDestination.USER_SETTINGS,
  projectSettings: conversationv1.AgentPermissionDestination.PROJECT_SETTINGS,
  localSettings: conversationv1.AgentPermissionDestination.LOCAL_SETTINGS,
  session: conversationv1.AgentPermissionDestination.SESSION,
  cliArg: conversationv1.AgentPermissionDestination.CLI_ARG,
};

const BEHAVIORS: Record<string, conversationv1.AgentPermissionBehavior> = {
  allow: conversationv1.AgentPermissionBehavior.ALLOW,
  deny: conversationv1.AgentPermissionBehavior.DENY,
  ask: conversationv1.AgentPermissionBehavior.ASK,
};

function rules(values: readonly { toolName: string; ruleContent?: string }[]): conversationv1.AgentPermissionRule[] {
  return values.map((rule) =>
    create(conversationv1.AgentPermissionRuleSchema, {
      toolName: rule.toolName,
      ...(rule.ruleContent === undefined ? {} : { ruleContent: rule.ruleContent }),
    }),
  );
}

/**
 * The vendor's offered standing grants, as the typed token the daemon echoes.
 *
 * TYPED, NOT OPAQUE: the consumer must be able to SAY what a standing grant
 * would do ("always allow Bash(git status)") without the shim shipping a blob
 * it would then have to parse back.
 */
export function toStanding(
  suggestions: readonly PermissionUpdateLike[],
): conversationv1.AgentPermissionStanding {
  return create(conversationv1.AgentPermissionStandingSchema, {
    changes: suggestions.map((suggestion) => {
      const destination = DESTINATIONS[suggestion.destination] ?? conversationv1.AgentPermissionDestination.UNSPECIFIED;
      switch (suggestion.type) {
        case "addRules":
          return create(conversationv1.AgentPermissionChangeSchema, {
            destination,
            change: {
              case: "addRules",
              value: create(conversationv1.AgentPermissionRulesAddedSchema, {
                rules: rules(suggestion.rules),
                behavior: BEHAVIORS[suggestion.behavior] ?? conversationv1.AgentPermissionBehavior.UNSPECIFIED,
              }),
            },
          });
        case "replaceRules":
          return create(conversationv1.AgentPermissionChangeSchema, {
            destination,
            change: {
              case: "replaceRules",
              value: create(conversationv1.AgentPermissionRulesReplacedSchema, {
                rules: rules(suggestion.rules),
                behavior: BEHAVIORS[suggestion.behavior] ?? conversationv1.AgentPermissionBehavior.UNSPECIFIED,
              }),
            },
          });
        case "removeRules":
          return create(conversationv1.AgentPermissionChangeSchema, {
            destination,
            change: {
              case: "removeRules",
              value: create(conversationv1.AgentPermissionRulesRemovedSchema, {
                rules: rules(suggestion.rules),
                behavior: BEHAVIORS[suggestion.behavior] ?? conversationv1.AgentPermissionBehavior.UNSPECIFIED,
              }),
            },
          });
        case "setMode":
          return create(conversationv1.AgentPermissionChangeSchema, {
            destination,
            change: {
              case: "setMode",
              value: create(conversationv1.AgentPermissionModeSetSchema, {
                mode: fromVendorPermissionMode(suggestion.mode),
              }),
            },
          });
        case "addDirectories":
          return create(conversationv1.AgentPermissionChangeSchema, {
            destination,
            change: {
              case: "addDirectories",
              value: create(conversationv1.AgentPermissionDirectoriesAddedSchema, {
                directories: [...suggestion.directories],
              }),
            },
          });
        case "removeDirectories":
          return create(conversationv1.AgentPermissionChangeSchema, {
            destination,
            change: {
              case: "removeDirectories",
              value: create(conversationv1.AgentPermissionDirectoriesRemovedSchema, {
                directories: [...suggestion.directories],
              }),
            },
          });
      }
    }),
  });
}

const VENDOR_DESTINATIONS: Record<number, PermissionUpdateLike["destination"]> = {
  [conversationv1.AgentPermissionDestination.USER_SETTINGS]: "userSettings",
  [conversationv1.AgentPermissionDestination.PROJECT_SETTINGS]: "projectSettings",
  [conversationv1.AgentPermissionDestination.LOCAL_SETTINGS]: "localSettings",
  [conversationv1.AgentPermissionDestination.SESSION]: "session",
  [conversationv1.AgentPermissionDestination.CLI_ARG]: "cliArg",
};

const VENDOR_BEHAVIORS: Record<number, "allow" | "deny" | "ask"> = {
  [conversationv1.AgentPermissionBehavior.ALLOW]: "allow",
  [conversationv1.AgentPermissionBehavior.DENY]: "deny",
  [conversationv1.AgentPermissionBehavior.ASK]: "ask",
};

/** The echoed standing token, back in the vendor's own vocabulary. */
export function fromStanding(
  standing: conversationv1.AgentPermissionStanding,
): PermissionUpdateLike[] {
  const updates: PermissionUpdateLike[] = [];
  for (const change of standing.changes) {
    const destination = VENDOR_DESTINATIONS[change.destination];
    if (destination === undefined) {
      throw new Error(
        `shim permission gate: standing change names destination ${String(change.destination)}, which has no vendor spelling`,
      );
    }
    const ruleValues = (
      list: readonly conversationv1.AgentPermissionRule[],
    ): { toolName: string; ruleContent?: string }[] =>
      list.map((rule) => ({
        toolName: rule.toolName,
        ...(rule.ruleContent === undefined ? {} : { ruleContent: rule.ruleContent }),
      }));
    switch (change.change.case) {
      case "addRules":
        updates.push({
          type: "addRules",
          destination,
          rules: ruleValues(change.change.value.rules),
          behavior: VENDOR_BEHAVIORS[change.change.value.behavior] ?? "ask",
        });
        break;
      case "replaceRules":
        updates.push({
          type: "replaceRules",
          destination,
          rules: ruleValues(change.change.value.rules),
          behavior: VENDOR_BEHAVIORS[change.change.value.behavior] ?? "ask",
        });
        break;
      case "removeRules":
        updates.push({
          type: "removeRules",
          destination,
          rules: ruleValues(change.change.value.rules),
          behavior: VENDOR_BEHAVIORS[change.change.value.behavior] ?? "ask",
        });
        break;
      case "setMode": {
        const mode = change.change.value.mode;
        if (mode === undefined) {
          throw new Error("shim permission gate: a set_mode standing change carries no mode");
        }
        updates.push({ type: "setMode", destination, mode: toVendorPermissionMode(mode) });
        break;
      }
      case "addDirectories":
        updates.push({ type: "addDirectories", destination, directories: [...change.change.value.directories] });
        break;
      case "removeDirectories":
        updates.push({ type: "removeDirectories", destination, directories: [...change.change.value.directories] });
        break;
      default:
        throw new Error("shim permission gate: a standing change carries no arm");
    }
  }
  return updates;
}

// ---------------------------------------------------------------------------
// the question batch, both ways
// ---------------------------------------------------------------------------

interface AskedQuestionInput {
  question?: unknown;
  header?: unknown;
  multiSelect?: unknown;
  options?: unknown;
}

/**
 * The batch, read from the tool's own input.
 *
 * Read FIELD BY FIELD rather than cast: the input is a `Record<string,
 * unknown>` at the SDK boundary, and a batch built from an input that does not
 * have the shape would be a question nobody asked.
 */
export function toQuestionBatch(input: Record<string, unknown>): conversationv1.AgentQuestionBatch {
  const raw = input.questions;
  if (!Array.isArray(raw)) {
    throw new Error("shim permission gate: AskUserQuestion input carries no `questions` array");
  }
  return create(conversationv1.AgentQuestionBatchSchema, {
    questions: raw.map((entry) => {
      const asked = entry as AskedQuestionInput;
      if (typeof asked.question !== "string" || typeof asked.header !== "string") {
        throw new Error("shim permission gate: an asked question carries no question text or header");
      }
      const options = Array.isArray(asked.options) ? asked.options : [];
      const built = options.map((option) => {
        const value = option as { label?: unknown; description?: unknown; preview?: unknown };
        if (typeof value.label !== "string" || typeof value.description !== "string") {
          throw new Error("shim permission gate: a question option carries no label or description");
        }
        return create(conversationv1.AgentQuestionOptionSchema, {
          label: create(conversationv1.AgentQuestionOptionLabelSchema, { label: value.label }),
          description: value.description,
          ...(typeof value.preview === "string" ? { preview: value.preview } : {}),
        });
      });
      return create(conversationv1.AgentQuestionAskedSchema, {
        question: create(conversationv1.AgentQuestionTextSchema, { text: asked.question }),
        header: asked.header,
        choices:
          asked.multiSelect === true
            ? { case: "multiSelect", value: create(conversationv1.AgentQuestionMultiSelectSchema, { options: built }) }
            : { case: "singleSelect", value: create(conversationv1.AgentQuestionSingleSelectSchema, { options: built }) },
      });
    }),
  });
}

/**
 * The answers, in the vendor's own form: keyed by question TEXT, multi-selects
 * comma-joined, the free text last.
 *
 * The inverse of the free-text-residue rule, so a round trip through the vendor
 * and back through the residue rule reproduces exactly what was answered.
 */
export function toVendorAnswers(
  answers: conversationv1.AgentQuestionAnswers,
): Record<string, string> {
  const serialized: Record<string, string> = {};
  for (const selection of answers.answers) {
    const text = selection.question?.text;
    if (text === undefined || text === "") {
      throw new Error("shim permission gate: an answer names no question");
    }
    const parts = selection.chosen.map((choice) => choice.label?.label ?? "");
    if (selection.freeText !== undefined && selection.freeText.text !== "") {
      parts.push(selection.freeText.text);
    }
    serialized[text] = parts.join(ANSWER_JOIN);
  }
  return serialized;
}

/**
 * Check an echo against the batch the shim is holding.
 *
 * Every answered question must BE one of the asked ones, and every chosen label
 * must be one of that question's options. A free text is always legal: it is by
 * definition what was not an option.
 */
export function validateAnswers(
  batch: conversationv1.AgentQuestionBatch,
  answers: conversationv1.AgentQuestionAnswers,
): string | undefined {
  for (const selection of answers.answers) {
    const text = selection.question?.text ?? "";
    const asked = batch.questions.find((question) => question.question?.text === text);
    if (asked === undefined) return `no question with the text ${JSON.stringify(text)} is open`;
    const options =
      asked.choices.case === "multiSelect"
        ? asked.choices.value.options
        : asked.choices.case === "singleSelect"
          ? asked.choices.value.options
          : [];
    const labels = new Set(options.map((option) => option.label?.label ?? ""));
    for (const choice of selection.chosen) {
      const label = choice.label?.label ?? "";
      if (!labels.has(label)) {
        return `${JSON.stringify(label)} is not an option of ${JSON.stringify(text)}`;
      }
    }
    if (asked.choices.case === "singleSelect" && selection.chosen.length > 1) {
      return `${JSON.stringify(text)} is single-select but ${selection.chosen.length} options were chosen`;
    }
  }
  return undefined;
}

// ---------------------------------------------------------------------------
// the gate itself
// ---------------------------------------------------------------------------

/** What the gate needs from the session around it. */
interface PermissionGateDeps {
  /** The book a gate frame lands on when the vendor named no agent. */
  mainAgentId(): conversationv1.AgentId;
  /**
   * The minted AgentId behind a vendor `agentID`, or undefined when this
   * session has never announced that agent.
   *
   * A DETACHED SUBAGENT RAISES ITS OWN GATED CALL, and `canUseTool` carries its
   * `agentID`. Writing that ask on the main agent's book put a subagent's
   * question in front of the wrong conversation.
   */
  agentFor(vendorAgentId: string): conversationv1.AgentId | undefined;
  /** Record a frame. Enqueued, never awaited: the vendor is blocked on us. */
  persist(entries: PersistEntry[]): void;
  /**
   * True when the keep-alive produced an ask raised on `agentId`'s book: the
   * running vendor turn for the main agent, the keep-alive's own spawn for a
   * subagent. A backgrounded subagent asking while a keep-alive runs is a real
   * question.
   */
  keepalive(agentId: conversationv1.AgentId): boolean;
  readonly nowMs: () => number;
  /** A standing grant carried a mode change; the session restates it authoritatively. */
  onPermissionModeSet(mode: conversationv1.AgentPermissionMode): void;
}

interface PendingBase {
  readonly toolUseId: string;
  readonly agentId: conversationv1.AgentId;
  resolve(result: PermissionResultLike): void;
}

interface PendingQuestion extends PendingBase {
  readonly kind: "question";
  readonly batch: conversationv1.AgentQuestionBatch;
  readonly input: Record<string, unknown>;
  readonly startedAtMs: number;
}

interface PendingPermission extends PendingBase {
  readonly kind: "permission";
  readonly toolName: string;
  readonly offeredStanding?: conversationv1.AgentPermissionStanding;
  readonly startedAtMs: number;
}

type Pending = PendingQuestion | PendingPermission;

/** Why an answer or decision could not be applied. */
type AnswerOutcome = "delivered" | "no_open_ask" | "answer_mismatch";

/**
 * The one vendor gate.
 *
 * Every tool passes through it, and AskUserQuestion is a tool riding the same
 * gate. Mechanism shared, meaning not: a QUESTION's "allow" is answer
 * transport; a PERMISSION's allow IS consent.
 */
export class PermissionGate {
  private readonly pendingByToolUse = new Map<string, Pending>();

  constructor(private readonly deps: PermissionGateDeps) {}

  /**
   * Whose book this ask belongs on.
   *
   * An `agentID` this session never announced is NEVER dropped: the ask still
   * blocks the vendor and still has to reach somebody, so it lands on the main
   * agent with the vendor's own spelling recorded in the log.
   */
  private bookFor(vendorAgentId: string | undefined): conversationv1.AgentId {
    if (vendorAgentId === undefined || vendorAgentId === "") return this.deps.mainAgentId();
    const resolved = this.deps.agentFor(vendorAgentId);
    if (resolved !== undefined) return resolved;
    LOGGER.debug(
      { vendor_agent_id: vendorAgentId },
      "the vendor raised an ask under an agent this session never announced; it lands on the main agent",
    );
    return this.deps.mainAgentId();
  }

  /** The callback handed to the SDK. */
  readonly canUseTool = async (
    toolName: string,
    input: Record<string, unknown>,
    options: {
      signal: AbortSignal;
      suggestions?: PermissionUpdateLike[];
      blockedPath?: string;
      decisionReason?: string;
      title?: string;
      displayName?: string;
      description?: string;
      toolUseID: string;
      agentID?: string;
      requestId: string;
      matchedAskRule?: { source: string; toolName: string; ruleContent?: string };
    },
  ): Promise<PermissionResultLike> => {
    return toolName === ASK_USER_QUESTION_TOOL
      ? this.openQuestion(input, options)
      : this.openPermission(toolName, options);
  };

  /**
   * Calls this gate denied, newest last, under a bound.
   *
   * Bounded because it exists only to be consulted by the tool_result that
   * arrives immediately after the denial; the store is the history of the
   * session and this must never become a second one.
   */
  private readonly denied: string[] = [];
  private readonly deniedSet = new Set<string>();

  /**
   * Remember a denial the gate did not make.
   *
   * The vendor's own `permission_denied` records -- a policy rule, or the
   * classifier failing to decide -- never reach an ask here, so the engine
   * relays them from the fold's output. One memory rather than two, because
   * `deniedCall` answers one question and its answer must not depend on which
   * half of the shim happened to see the denial.
   */
  noteVendorDenial(toolUseId: string): void {
    this.noteDenied(toolUseId);
  }

  /** Remember a denial, evicting the oldest once the bound is reached. */
  private noteDenied(toolUseId: string): void {
    if (toolUseId === "" || this.deniedSet.has(toolUseId)) return;
    this.denied.push(toolUseId);
    this.deniedSet.add(toolUseId);
    while (this.denied.length > DENIED_MEMORY) {
      const evicted = this.denied.shift();
      if (evicted !== undefined) this.deniedSet.delete(evicted);
    }
  }

  /** Did this gate deny that call — the fold's `deniedCall`. */
  deniedCall(toolUseId: string): boolean {
    return this.deniedSet.has(toolUseId);
  }

  /** What kind of ask, if any, is open on a call — the fold's `pendingAsk`. */
  pendingAsk(toolUseId: string): { kind: "permission" | "question" } | undefined {
    const pending = this.pendingByToolUse.get(toolUseId);
    return pending === undefined ? undefined : { kind: pending.kind };
  }

  /**
   * The refusal for an answer that names no ask the gate is holding.
   *
   * THE TWO ARMS ARE DIFFERENT FACTS and the daemon acts on them differently.
   * `no_open_ask` means the agent is not waiting on anything of this kind, so
   * there is nothing to answer and nothing to retry. `answer_mismatch` means an
   * ask of this kind IS open and the answer named a different one -- a stale or
   * crossed answer, where the right move is to answer the ask actually in hand.
   * Collapsing the second onto the first told the daemon to stop when it should
   * re-send.
   */
  private unmatchedAnswer(kind: "permission" | "question", id: string): AnswerOutcome {
    const openOfKind = [...this.pendingByToolUse.values()].some(
      (pending) => pending.kind === kind,
    );
    LOGGER.debug(
      { ask_kind: kind, ask_id: id, another_open: openOfKind },
      openOfKind
        ? "refused an answer for an unknown ask while another ask of its kind is open"
        : "an answer arrived for an ask that is not open",
    );
    return openOfKind ? "answer_mismatch" : "no_open_ask";
  }

  /** How many asks are blocking the vendor right now. */
  get pendingCount(): number {
    return this.pendingByToolUse.size;
  }

  // -- questions ------------------------------------------------------------

  private openQuestion(
    input: Record<string, unknown>,
    options: { toolUseID: string; agentID?: string },
  ): Promise<PermissionResultLike> {
    const batch = toQuestionBatch(input);
    const id = questionId(options.toolUseID);
    const startedAtMs = this.deps.nowMs();
    const agentId = this.bookFor(options.agentID);
    return new Promise<PermissionResultLike>((resolve) => {
      this.pendingByToolUse.set(options.toolUseID, {
        kind: "question",
        toolUseId: options.toolUseID,
        agentId,
        batch,
        input,
        startedAtMs,
        resolve,
      });
      this.deps.persist([
        this.entry(agentId, questionUpsertKey(id), options.toolUseID, "agent_frame.update.question.start", {
          case: "question",
          value: create(conversationv1.AgentQuestionSchema, {
            id,
            result: {
              case: "start",
              value: create(conversationv1.AgentQuestionStartSchema, {
                batch,
                startedAt: create(conversationv1.AgentActivityStartedAtSchema, { atMs: BigInt(startedAtMs) }),
              }),
            },
          }),
        }),
      ]);
      LOGGER.debug(
        { tool_use_id: options.toolUseID, questions: batch.questions.length },
        "opened a question and BLOCKED the vendor until it is answered",
      );
    });
  }

  /** The consumer answered. */
  answerQuestion(
    ask: conversationv1.AgentQuestionId,
    answers: conversationv1.AgentQuestionAnswers,
  ): AnswerOutcome {
    const pending = this.pendingByToolUse.get(ask.value);
    if (pending === undefined || pending.kind !== "question") {
      return this.unmatchedAnswer("question", ask.value);
    }
    const problem = validateAnswers(pending.batch, answers);
    if (problem !== undefined) {
      LOGGER.debug(
        { question_id: ask.value, problem },
        "refused an answer whose echo does not match the open question",
      );
      return "answer_mismatch";
    }
    this.pendingByToolUse.delete(ask.value);
    this.settleQuestion(pending, answers);
    pending.resolve({
      behavior: "allow",
      updatedInput: { ...pending.input, answers: toVendorAnswers(answers) },
    });
    LOGGER.debug({ question_id: ask.value, answers: answers.answers.length }, "delivered the user's answers to the vendor");
    return "delivered";
  }

  private settleQuestion(
    pending: PendingQuestion,
    answers: conversationv1.AgentQuestionAnswers | undefined,
  ): void {
    const id = questionId(pending.toolUseId);
    this.deps.persist([
      this.entry(pending.agentId, questionUpsertKey(id), pending.toolUseId, "agent_frame.update.question.success", {
        case: "question",
        value: create(conversationv1.AgentQuestionSchema, {
          id,
          result: {
            case: "success",
            value: create(conversationv1.AgentQuestionSuccessSchema, {
              batch: pending.batch,
              outcome:
                answers === undefined
                  ? { case: "unanswered", value: create(conversationv1.AgentQuestionUnansweredSchema, {}) }
                  : { case: "answered", value: answers },
            }),
          },
        }),
      }),
    ]);
  }

  // -- permissions ----------------------------------------------------------

  private openPermission(
    toolName: string,
    options: {
      toolUseID: string;
      suggestions?: PermissionUpdateLike[];
      blockedPath?: string;
      decisionReason?: string;
      title?: string;
      displayName?: string;
      description?: string;
      agentID?: string;
      matchedAskRule?: { source: string; toolName: string; ruleContent?: string };
    },
  ): Promise<PermissionResultLike> {
    const id = permissionId(options.toolUseID);
    const startedAtMs = this.deps.nowMs();
    const agentId = this.bookFor(options.agentID);
    const offeredStanding =
      options.suggestions === undefined || options.suggestions.length === 0
        ? undefined
        : toStanding(options.suggestions);
    const trigger = permissionTrigger(options);
    return new Promise<PermissionResultLike>((resolve) => {
      this.pendingByToolUse.set(options.toolUseID, {
        kind: "permission",
        toolUseId: options.toolUseID,
        agentId,
        toolName,
        startedAtMs,
        resolve,
        ...(offeredStanding === undefined ? {} : { offeredStanding }),
      });
      this.deps.persist([
        this.entry(agentId, permissionUpsertKey(id), options.toolUseID, "agent_frame.update.permission.start", {
          case: "permission",
          value: create(conversationv1.AgentPermissionSchema, {
            id,
            gatedCall: toolCallActivityId(options.toolUseID),
            result: {
              case: "start",
              value: create(conversationv1.AgentPermissionStartSchema, {
                // The VENDOR renders the prompt sentence; the shim never writes
                // one of its own, because the vendor's wording is what the user
                // would have seen in the terminal.
                prompt: create(conversationv1.AgentPermissionPromptSchema, {
                  title: options.title ?? toolName,
                  displayName: options.displayName ?? toolName,
                  ...(options.description === undefined ? {} : { description: options.description }),
                }),
                startedAt: create(conversationv1.AgentActivityStartedAtSchema, { atMs: BigInt(startedAtMs) }),
                ...(trigger === undefined ? {} : { trigger }),
                ...(offeredStanding === undefined ? {} : { offeredStanding }),
              }),
            },
          }),
        }),
      ]);
      LOGGER.debug(
        { tool_use_id: options.toolUseID, tool_name: toolName, offered_standing: offeredStanding !== undefined },
        "opened a permission gate and BLOCKED the vendor until it is decided",
      );
    });
  }

  /** The consumer decided. */
  decidePermission(decision: conversationv1.AgentPermissionDecision): AnswerOutcome {
    const askId = decision.ask?.value ?? "";
    const pending = this.pendingByToolUse.get(askId);
    if (pending === undefined || pending.kind !== "permission") {
      return this.unmatchedAnswer("permission", askId);
    }
    switch (decision.decision.case) {
      case "allowed": {
        const scope = decision.decision.value.scope;
        if (scope.case === "standing") {
          const standing = scope.value.standing;
          if (standing === undefined) {
            // warn: a defect because a standing permission decision omitted the standing it grants.
            LOGGER.warn({ permission_id: askId }, "a standing allow carries no standing");
            return "answer_mismatch";
          }
          // THE STANDING IS A TYPED ECHO TOKEN, VALIDATED LIKE ANY OTHER ECHO.
          // The shim already holds the ask it offered, so a grant that is not
          // BYTE-FOR-BYTE the offered standing -- altered rules, an extra
          // set_mode nobody offered, or a standing where the vendor offered
          // none at all -- is a consumer answering a question the shim is not
          // holding. Accepting it would let a caller install permission rules
          // and change the session's mode through a grant the vendor never
          // proposed.
          if (
            pending.offeredStanding === undefined ||
            !equals(conversationv1.AgentPermissionStandingSchema, pending.offeredStanding, standing)
          ) {
            LOGGER.debug(
              {
                permission_id: askId,
                offered: pending.offeredStanding !== undefined,
              },
              "refused a standing grant that does not match the standing this ask offered",
            );
            return "answer_mismatch";
          }
          const updates = fromStanding(standing);
          this.pendingByToolUse.delete(askId);
          this.settlePermission(pending, {
            case: "allowed",
            value: create(conversationv1.AgentPermissionAllowedSchema, {
              scope: {
                case: "standing",
                value: create(conversationv1.AgentPermissionAllowedStandingSchema, { standing }),
              },
            }),
          });
          pending.resolve({ behavior: "allow", updatedPermissions: updates });
          for (const change of standing.changes) {
            if (change.change.case === "setMode" && change.change.value.mode !== undefined) {
              // A standing grant that changes the session's mode is a SESSION
              // fact; the consumer learns it authoritatively on WatchSession,
              // not by inferring it from the grant it sent.
              this.deps.onPermissionModeSet(change.change.value.mode);
            }
          }
          LOGGER.debug({ permission_id: askId, scope: "standing" }, "allowed a tool call with a standing grant");
          return "delivered";
        }
        this.pendingByToolUse.delete(askId);
        this.settlePermission(pending, {
          case: "allowed",
          value: create(conversationv1.AgentPermissionAllowedSchema, {
            scope: { case: "once", value: create(conversationv1.AgentPermissionAllowedOnceSchema, {}) },
          }),
        });
        pending.resolve({ behavior: "allow" });
        LOGGER.debug({ permission_id: askId, scope: "once" }, "allowed a tool call once");
        return "delivered";
      }
      case "denied": {
        const message = decision.decision.value.message;
        this.pendingByToolUse.delete(askId);
        this.settlePermission(pending, {
          case: "denied",
          value: create(conversationv1.AgentPermissionDeniedSchema, {
            by: { case: "user", value: create(conversationv1.AgentPermissionDeniedByUserSchema, { message }) },
          }),
        });
        pending.resolve({ behavior: "deny", message });
        LOGGER.debug({ permission_id: askId }, "denied a tool call by the user");
        return "delivered";
      }
      default:
        // warn: a defect because an arm-less permission decision cannot resolve the vendor callback faithfully.
        LOGGER.warn({ permission_id: askId }, "a permission decision carries no arm");
        return "answer_mismatch";
    }
  }

  private settlePermission(
    pending: PendingPermission,
    decision: conversationv1.AgentPermissionSuccess["decision"],
  ): void {
    // A DENIED TOOL NEVER STARTS, and the vendor still emits a `tool_result`
    // for it -- the deny message IS the result the model sees. Remembering the
    // denial is what lets the fold tell that result apart from a call that ran
    // and failed; the stream carries no `toolDenialKind`, so this is the only
    // signal on this plane.
    if (decision.case === "denied") this.noteDenied(pending.toolUseId);
    const id = permissionId(pending.toolUseId);
    this.deps.persist([
      this.entry(pending.agentId, permissionUpsertKey(id), pending.toolUseId, "agent_frame.update.permission.success", {
        case: "permission",
        value: create(conversationv1.AgentPermissionSchema, {
          id,
          gatedCall: toolCallActivityId(pending.toolUseId),
          result: {
            case: "success",
            value: create(conversationv1.AgentPermissionSuccessSchema, { decision }),
          },
        }),
      }),
    ]);
  }

  // -- liveness -------------------------------------------------------------

  /**
   * Resolve EVERY pending callback as denied.
   *
   * The first act of every teardown path. An unresolved `canUseTool` promise
   * wedges the vendor process, so this runs before the interrupt, before the
   * query is closed, and before the process exits — never after.
   */
  standDown(reason: string): number {
    const pending = [...this.pendingByToolUse.values()];
    this.pendingByToolUse.clear();
    for (const ask of pending) {
      if (ask.kind === "question") {
        this.settleQuestion(ask, undefined);
      } else {
        this.settlePermission(ask, {
          case: "denied",
          value: create(conversationv1.AgentPermissionDeniedSchema, {
            by: {
              case: "user",
              value: create(conversationv1.AgentPermissionDeniedByUserSchema, { message: reason }),
            },
          }),
        });
      }
      ask.resolve({ behavior: "deny", message: reason });
    }
    if (pending.length > 0) {
      LOGGER.info(
        { reason, resolved: pending.length },
        "resolved every pending permission callback as denied before teardown",
      );
    }
    return pending.length;
  }

  private entry(
    agentId: conversationv1.AgentId,
    upsertKey: string,
    toolUseId: string,
    discriminator: string,
    update: conversationv1.AgentUpdate["update"],
  ): PersistEntry {
    return {
      agentId,
      upsertKey,
      // The gate has no vendor RECORD uuid: it fires before the vendor writes
      // anything about the call. The call's own tool_use_id is the stable
      // coordinate for these rows, and it is unique per call by construction.
      source: { vendorUuid: toolUseId, discriminator },
      keepalive: this.deps.keepalive(agentId),
      item: {
        kind: "frame",
        frame: create(conversationv1.AgentFrameSchema, {
          agentId,
          result: {
            case: "update",
            value: create(conversationv1.AgentUpdateSchema, { update }),
          },
        }),
      },
    };
  }
}

/** The trigger facts the vendor stated, or absence when it stated none. */
function permissionTrigger(options: {
  blockedPath?: string;
  decisionReason?: string;
  matchedAskRule?: { source: string; toolName: string; ruleContent?: string };
}): conversationv1.AgentPermissionTrigger | undefined {
  const blockedPath =
    options.blockedPath === undefined
      ? undefined
      : create(conversationv1.AgentPermissionBlockedPathSchema, { path: options.blockedPath });
  const askRule =
    options.matchedAskRule === undefined
      ? undefined
      : create(conversationv1.AgentPermissionAskRuleSchema, {
          source: options.matchedAskRule.source,
          toolName: options.matchedAskRule.toolName,
          ...(options.matchedAskRule.ruleContent === undefined
            ? {}
            : { ruleContent: options.matchedAskRule.ruleContent }),
        });
  const note =
    options.decisionReason === undefined
      ? undefined
      : create(conversationv1.AgentPermissionTriggerNoteSchema, { text: options.decisionReason });
  if (blockedPath === undefined && askRule === undefined && note === undefined) return undefined;
  return create(conversationv1.AgentPermissionTriggerSchema, {
    ...(blockedPath === undefined ? {} : { blockedPath }),
    ...(askRule === undefined ? {} : { askRule }),
    ...(note === undefined ? {} : { note }),
  });
}
