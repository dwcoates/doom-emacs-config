package convert

// attachment.go — the `attachment` records: hooks firing around the agent's
// work, the IDE's diagnostics, and context the vendor SILENTLY pulled in.
//
// Most attachment types are context-cut exclusions and CLI machinery, withheld
// so no resolver ever sees them as prose. Four families are conversation facts
// and are modeled: the hook outcomes, the diagnostics report, the injected
// memory/skills, and the skill listing.

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
)

// attachmentLine converts an `attachment` record by its attachment type.
func (c *Converter) attachmentLine(record map[string]any, at Attribution) []*storev1.StoreEntry {
	attachment := obj(record["attachment"])
	if attachment == nil {
		c.log.With(logging.Context{Operation: "convert-line", Path: at.Path, Level: "warn"}).
			Log("attachment line at offset=%d carries no %q object; stored as unknown residue", at.Offset, "attachment")
		return []*storev1.StoreEntry{UnknownEntry(at, "", "attachment", record)}
	}
	env := readEnvelope(record)
	agent := c.frameAgent(at, env)
	kind := str(attachment["type"])

	switch kind {
	case "hook_success", "hook_blocking_error", "hook_non_blocking_error", "hook_cancelled":
		return []*storev1.StoreEntry{c.hookAttachment(kind, attachment, at, env, agent)}
	case "diagnostics":
		return c.diagnosticsAttachment(attachment, at, env, agent)
	case "nested_memory":
		return []*storev1.StoreEntry{c.injectedMemory(attachment, at, env, agent)}
	case "dynamic_skill", "invoked_skills", "skill_listing":
		return []*storev1.StoreEntry{c.injectedSkills(kind, attachment, at, env, agent)}
	default:
		// Context-cut exclusions and CLI machinery: understood, and deliberately
		// not carried into a vendor-agnostic feed.
		c.log.With(logging.Context{Operation: "withhold", Path: at.Path}).
			LogVerbose("attachment/%s at offset=%d withheld as vendor_specific", kind, at.Offset)
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "attachment/"+kind, record)}
	}
}

// ---------------------------------------------------------------------------
// hooks
// ---------------------------------------------------------------------------

// hookAttachment converts one hook execution.
//
// THE LARGEST RECORD CLASS THE VENDOR WRITES, and quiet by default in the UI: a
// succeeded hook draws nothing, a failing one draws a card, and a BLOCKING one is
// a refusal the user must be able to understand — which is why the refusal text
// is the whole point of that arm.
func (c *Converter) hookAttachment(kind string, attachment map[string]any, at Attribution, env envelope, agent string) *storev1.StoreEntry {
	// A hook's unit identity is the vendor's own toolUseID for the firing: a
	// PreToolUse and a PostToolUse around one call are separate firings and must
	// not collapse onto one row, so the kind rides the identity.
	unitID := "hook:" + str(pick(attachment, "hookName", "hook_name")) + ":" + str(pick(attachment, "toolUseID", "toolUseId", "tool_use_id"))

	hook := &conversationv1.AgentHook{}
	switch kind {
	case "hook_success":
		hook.Result = &conversationv1.AgentHook_Succeeded{Succeeded: &conversationv1.AgentHookSucceeded{
			Command:    str(attachment["command"]),
			ExitCode:   int32(number(attachment["exitCode"])),
			DurationMs: int64(number(attachment["durationMs"])),
			Output:     hookOutput(attachment),
		}}
	case "hook_blocking_error":
		blocking := obj(attachment["blockingError"])
		hook.Result = &conversationv1.AgentHook_BlockingError{BlockingError: &conversationv1.AgentHookBlockingError{
			Command:      firstNonEmpty(str(blocking["command"]), str(attachment["command"])),
			BlockingText: firstNonEmpty(str(blocking["blockingError"]), str(attachment["blockingError"])),
		}}
		c.log.With(logging.Context{Operation: "hook", Path: at.Path, Level: "warn"}).
			Log("hook %q BLOCKED the gated call at offset=%d; the agent proceeds without it", str(pick(attachment, "hookName", "hook_name")), at.Offset)
	case "hook_non_blocking_error":
		hook.Result = &conversationv1.AgentHook_NonBlockingError{NonBlockingError: &conversationv1.AgentHookNonBlockingError{
			Command:    str(attachment["command"]),
			ExitCode:   int32(number(attachment["exitCode"])),
			DurationMs: int64(number(attachment["durationMs"])),
			Output:     hookOutput(attachment),
		}}
		c.log.With(logging.Context{Operation: "hook", Path: at.Path, Level: "warn"}).
			Log("hook %q failed without blocking at offset=%d", str(pick(attachment, "hookName", "hook_name")), at.Offset)
	default:
		hook.Result = &conversationv1.AgentHook_Cancelled{Cancelled: &conversationv1.AgentHookCancelled{}}
	}

	c.log.With(logging.Context{Operation: "hook", Path: at.Path}).
		LogVerbose("hook attachment kind=%s activity_id=%s upsert_key=%s at offset=%d", kind, unitID, ActivityKey(unitID), at.Offset)

	activity := item(&conversationv1.AgentActivity_Hook{Hook: hook})
	activity.ActivityId = activityID(unitID)
	return c.landFrame(at, agent, ActivityKey(unitID), "hook:"+kind, activityFrame(agent, activity))
}

// hookStartFor is not minted from an attachment: the vendor writes ONE record per
// hook execution carrying its outcome, never a separate announcement, so a start
// frame would be a fact this reader invented.

// hookOutput reads what a hook printed. UNSET when it printed nothing.
func hookOutput(attachment map[string]any) *conversationv1.AgentHookOutput {
	stdout, stderr := str(attachment["stdout"]), str(attachment["stderr"])
	if stdout == "" && stderr == "" {
		return nil
	}
	return &conversationv1.AgentHookOutput{Stdout: stdout, Stderr: stderr}
}

// ---------------------------------------------------------------------------
// IDE diagnostics
// ---------------------------------------------------------------------------

// diagnosticsAttachment joins the IDE's findings to the change that caused them.
//
// THE JOIN IS BY ADJACENCY and nothing else: the vendor's record carries no
// tool-call id, so the converter holds ONE remembered "last write/edit unit"
// value — constant, and stated here because nothing in the schema implies it.
// The report arrives as a frame of that same unit FOLLOWING its terminal — a
// consequence, not a state.
func (c *Converter) diagnosticsAttachment(attachment map[string]any, at Attribution, env envelope, agent string) []*storev1.StoreEntry {
	if c.lastChangeUnit == "" {
		// Nothing to attach them to: the change was read before this reader's
		// cursor. The findings are kept whole rather than pinned onto a unit
		// this reader guessed at.
		c.log.With(logging.Context{Operation: "diagnostics", Path: at.Path, Level: "warn"}).
			Log("IDE diagnostics at offset=%d follow no write or edit this reader observed; stored as vendor_specific rather than attached by guess", at.Offset)
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "attachment/diagnostics", attachment)}
	}

	report := &conversationv1.AgentDiagnosticsReport{}
	for _, rawFile := range list(attachment["files"]) {
		file := obj(rawFile)
		if file == nil {
			continue
		}
		entry := &conversationv1.AgentDiagnosticsFile{Path: str(pick(file, "uri", "path"))}
		for _, rawDiag := range list(file["diagnostics"]) {
			diag := obj(rawDiag)
			if diag == nil {
				continue
			}
			rng := obj(diag["range"])
			entry.Diagnostics = append(entry.Diagnostics, &conversationv1.AgentDiagnostic{
				Severity:  diagnosticSeverity(str(diag["severity"])),
				Message:   str(diag["message"]),
				Source:    optionalString(diag["source"]),
				Code:      optionalString(diag["code"]),
				StartLine: int32(number(obj(rng["start"])["line"])),
				EndLine:   int32(number(obj(rng["end"])["line"])),
			})
		}
		report.Files = append(report.Files, entry)
	}

	unit := c.lastChangeUnit
	c.log.With(logging.Context{Operation: "diagnostics", Path: at.Path}).
		Log("IDE diagnostics at offset=%d joined by adjacency to activity_id=%s upsert_key=%s (%d file(s))", at.Offset, unit, ActivityKey(unit), len(report.Files))

	// The report rides the CHANGE's own unit as a further frame of it. Which
	// change kind it was decides the arm, so the remembered value carries it.
	var activity *conversationv1.AgentActivity
	if c.lastChangeWasEdit {
		activity = item(&conversationv1.AgentActivity_Edit{Edit: &conversationv1.AgentEdit{
			Result: &conversationv1.AgentEdit_Diagnostics{Diagnostics: report},
		}})
	} else {
		activity = item(&conversationv1.AgentActivity_Write{Write: &conversationv1.AgentWrite{
			Result: &conversationv1.AgentWrite_Diagnostics{Diagnostics: report},
		}})
	}
	activity.ActivityId = activityID(unit)
	return []*storev1.StoreEntry{c.landFrame(at, agent, ActivityKey(unit), "diag", activityFrame(agent, activity))}
}

func diagnosticSeverity(s string) conversationv1.AgentDiagnosticSeverity {
	switch s {
	case "Error", "error":
		return conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_ERROR
	case "Warning", "warning":
		return conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_WARNING
	case "Information", "information", "Info", "info":
		return conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_INFORMATION
	case "Hint", "hint":
		return conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_HINT
	default:
		return conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_UNSPECIFIED
	}
}

// ---------------------------------------------------------------------------
// injected context
// ---------------------------------------------------------------------------

// injectedMemory carries a memory file the vendor SILENTLY pulled in — no tool
// call announced it, and the user could not otherwise see what shaped the
// agent's behavior. An injection is INSTANTANEOUS: one record, no lifecycle.
func (c *Converter) injectedMemory(attachment map[string]any, at Attribution, env envelope, agent string) *storev1.StoreEntry {
	content := obj(attachment["content"])
	path := firstNonEmpty(str(attachment["path"]), str(content["path"]))
	unitID := "context:memory:" + env.uuid

	c.log.With(logging.Context{Operation: "context-injected", Path: at.Path}).
		LogVerbose("memory file %q injected at offset=%d activity_id=%s", path, at.Offset, unitID)

	activity := item(&conversationv1.AgentActivity_ContextInjected{ContextInjected: &conversationv1.AgentContextInjected{
		Injected: &conversationv1.AgentContextInjected_Memory{Memory: &conversationv1.AgentInjectedMemory{
			Path:    path,
			Content: firstNonEmpty(str(content["content"]), str(attachment["contentText"])),
		}},
	}})
	activity.ActivityId = activityID(unitID)
	return c.landFrame(at, agent, ActivityKey(unitID), "context_injected", activityFrame(agent, activity))
}

// injectedSkills carries skills discovered or invoked WITHOUT a tool call.
func (c *Converter) injectedSkills(kind string, attachment map[string]any, at Attribution, env envelope, agent string) *storev1.StoreEntry {
	unitID := "context:skills:" + env.uuid
	injected := &conversationv1.AgentInjectedSkills{}

	for _, raw := range list(attachment["skills"]) {
		skill := obj(raw)
		if skill == nil {
			continue
		}
		injected.Skills = append(injected.Skills, &conversationv1.AgentInjectedSkill{
			Name:    str(skill["name"]),
			Path:    optionalString(skill["path"]),
			Content: optionalString(skill["content"]),
		})
	}
	// A dynamic-discovery delta names the DIRECTORY and the names, not each
	// file's body — so path and content stay UNSET rather than being invented
	// from the directory.
	for _, raw := range list(attachment["skillNames"]) {
		if name := str(raw); name != "" {
			injected.Skills = append(injected.Skills, &conversationv1.AgentInjectedSkill{Name: name})
		}
	}
	for _, raw := range list(attachment["names"]) {
		if name := str(raw); name != "" {
			injected.Skills = append(injected.Skills, &conversationv1.AgentInjectedSkill{Name: name})
		}
	}

	c.log.With(logging.Context{Operation: "context-injected", Path: at.Path}).
		LogVerbose("attachment/%s injected %d skill(s) at offset=%d activity_id=%s", kind, len(injected.Skills), at.Offset, unitID)

	activity := item(&conversationv1.AgentActivity_ContextInjected{ContextInjected: &conversationv1.AgentContextInjected{
		Injected: &conversationv1.AgentContextInjected_Skills{Skills: injected},
	}})
	activity.ActivityId = activityID(unitID)
	return c.landFrame(at, agent, ActivityKey(unitID), "context_injected", activityFrame(agent, activity))
}
