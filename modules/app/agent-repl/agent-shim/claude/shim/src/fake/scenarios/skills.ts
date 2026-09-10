/**
 * fake/scenarios/skills.ts — skill invocation and injected context.
 *
 * # A skill unit settles on the DOCUMENT, not the acknowledgement
 *
 * The `Skill` tool's result is a two-field acknowledgement
 * (`{success, commandName}` in the corpus, plus `allowedTools` when the skill
 * declares any). The skill's actual CONTENT arrives separately, as an `isMeta`
 * user record joined back by `sourceToolUseID`. So this family emits BOTH, and
 * a converter that settled on the acknowledgement would carry no document at
 * all — which is exactly the mistake the corpus exists to prevent.
 *
 * # Injected context is a FILE-PLANE fact
 *
 * `nested_memory`, `invoked_skills` and `dynamic_skill` are attachment records:
 * the vendor writes them and streams nothing. They are what `AgentContextInjected`
 * is made of, and the mock writes them exactly the same way — which is why
 * those scenarios emit only prose on the stream.
 */
import { conclude, scenario } from "./support.js";

/** Split `!skill <skill-name> [args...]` into its two fields. */
function skillInvocationOf(args: string): { skill: string; skillArgs: string } {
  if (args === "") return { skill: "fake-skill", skillArgs: "--target one" };
  const [skill, ...rest] = args.split(/\s+/).filter((s) => s !== "");
  return { skill: skill ?? "fake-skill", skillArgs: rest.join(" ") };
}

/** The DOCUMENT body a skill's directory injection carries, named by the skill. */
function skillDocument(skill: string): string {
  return (
    `Base directory for this skill: /w/s/.claude/skills/${skill}\n\n` +
    `# ${skill}\n\nThe offline skill's instructions, as the vendor injects them.`
  );
}

const SKILL = scenario({
  name: "skill",
  prompt: "!skill [skill-name] [args]",
  emits:
    "a `Skill` tool_use, its `{success, commandName, allowedTools}` acknowledgement, then the skill DOCUMENT " +
    "as an `isMeta` user record joined by `sourceToolUseID`. The skill name and args are parameterized by the " +
    "prompt (first token = name, rest = args), and the document body is derived from the name — a caller can " +
    "name e.g. `create-or-update-workspace` with args `merge`",
  writes: "the tool_use line, the acknowledgement line, the isMeta document line, the closing text line",
  arms: "AgentSkillUse.start + AgentSkillUseSuccess settled on the document, with the allowances",
  run(ctx) {
    const { skill, skillArgs } = skillInvocationOf(ctx.args);
    ctx.log.debug({ turn: ctx.turn, branch: "skill", skill, skill_args: skillArgs }, "fake skill-invocation turn");
    const call = ctx.toolUse("Skill", { skill, args: skillArgs });
    ctx.toolResult(call, `Launching skill: ${skill}`, {
      success: true,
      commandName: skill,
      // The ALLOWANCES the skill carries: tool patterns pre-approved for its
      // duration. They ride the acknowledgement and nothing else.
      allowedTools: [`Bash(.claude/skills/${skill}/run.sh:*)`, `Read(/tmp/${skill}-result.json)`],
    });
    // THE DOCUMENT. `isMeta: true` marks it as instructions the model reads
    // rather than a user's words, and `sourceToolUseID` is the only join back
    // to the call it belongs to.
    ctx.files.transcript.append({
      type: "user",
      isMeta: true,
      sourceToolUseID: call.toolUseId,
      message: {
        role: "user",
        content: [{ type: "text", text: skillDocument(skill) }],
      },
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    conclude(ctx, `Invoked the ${skill} skill and read its document.`);
  },
});

const SKILL_FAILURE = scenario({
  name: "skill-fail",
  prompt: "!skill-fail",
  emits: "a `Skill` for a name that does not resolve, answered with an error result and no document",
  writes: "the tool_use line, the error tool_result line, the closing text line",
  arms: "AgentSkillUse.start + AgentSkillUseFailure",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "skill-fail" }, "fake failing-skill turn");
    const call = ctx.toolUse("Skill", { skill: "absent-skill" });
    ctx.toolResult(call, "Error: no such skill: absent-skill", { success: false, commandName: "absent-skill" }, {
      isError: true,
    });
    conclude(ctx, "The skill did not resolve.");
  },
});

const MEMORY_INJECTED = scenario({
  name: "memory",
  prompt: "!memory",
  emits: "prose only; the injected memory is a FILE-PLANE fact the vendor never streams",
  writes: "a `nested_memory` attachment line and a `file` attachment line carrying a memory file's body",
  arms: "AgentContextInjected.injected=memory",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "memory" }, "fake memory-injection turn");
    ctx.attachment({
      type: "nested_memory",
      path: `${ctx.cwd}/CLAUDE.md`,
      content: {
        path: `${ctx.cwd}/CLAUDE.md`,
        type: "Project",
        content: "@./AGENTS.md\n",
        contentDiffersFromDisk: false,
      },
      displayPath: "CLAUDE.md",
    });
    ctx.attachment({
      type: "file",
      filename: `${ctx.configDir}/memory/offline-convention.md`,
      content: {
        type: "text",
        file: {
          filePath: `${ctx.configDir}/memory/offline-convention.md`,
          content: "---\nname: offline-convention\n---\n\nThe offline session's remembered convention.\n",
          numLines: 5,
          startLine: 1,
          totalLines: 5,
        },
      },
      displayPath: "memory/offline-convention.md",
    });
    conclude(ctx, "Read the injected memory.");
  },
});

const SKILLS_INJECTED = scenario({
  name: "skills-injected",
  prompt: "!skills-injected",
  emits: "prose only; the injected skills are attachment records",
  writes: "`invoked_skills`, `dynamic_skill` and `skill_listing` attachment lines",
  arms: "AgentContextInjected.injected=skills",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "skills-injected" }, "fake skill-injection turn");
    ctx.attachment({
      type: "invoked_skills",
      skills: [
        {
          name: "fake-skill",
          path: "userSettings:fake-skill",
          content: "Base directory for this skill: /w/s/.claude/skills/fake-skill\n\n# Fake skill\n",
        },
      ],
    });
    ctx.attachment({
      type: "dynamic_skill",
      skillDir: "/w/s/.claude/skills",
      skillNames: ["fake-skill", "other-skill"],
      displayPath: "w/s/.claude/skills",
    });
    ctx.attachment({
      type: "skill_listing",
      content: "- fake-skill: the offline skill that takes an argument\n",
      skillCount: 2,
      isInitial: true,
      names: ["fake-skill", "other-skill"],
    });
    conclude(ctx, "Loaded the injected skills.");
  },
});

export const SKILL_SCENARIOS = [SKILL, SKILL_FAILURE, MEMORY_INJECTED, SKILLS_INJECTED];
