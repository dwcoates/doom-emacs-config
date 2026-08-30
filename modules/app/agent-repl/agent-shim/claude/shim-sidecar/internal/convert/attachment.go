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
		c.log.With(at.ctxWarn("convert-line")).
			Log("attachment line carries no %q object; stored as unknown residue", "attachment")
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
	case contextBudgetAttachment:
		return []*storev1.StoreEntry{c.contextBudgetWarning(attachment, at, env, agent)}
	default:
		// Context-cut exclusions and CLI machinery: understood, and deliberately
		// not carried into a vendor-agnostic feed.
		c.log.With(at.ctxFor("withhold")).
			LogVerbose("attachment/%s withheld as vendor_specific", kind)
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
		// VERBOSE, NOT A WARNING. A hook the vendor recorded as blocking is
		// CONTENT this copier is faithfully carrying into the blocking_error
		// arm — the conversion went perfectly. A warning here would say "the
		// sidecar is degraded" about a transcript that merely describes a
		// blocked call, and the census that drives the reader's warnings to zero
		// could then never reach zero over a real capture.
		c.log.With(at.ctxFor("hook")).With(logging.Context{ActivityID: unitID}).
			LogVerbose("hook %q BLOCKED the gated call; carried on the blocking_error arm", str(pick(attachment, "hookName", "hook_name")))
	case "hook_non_blocking_error":
		hook.Result = &conversationv1.AgentHook_NonBlockingError{NonBlockingError: &conversationv1.AgentHookNonBlockingError{
			Command:    str(attachment["command"]),
			ExitCode:   int32(number(attachment["exitCode"])),
			DurationMs: int64(number(attachment["durationMs"])),
			Output:     hookOutput(attachment),
		}}
		c.log.With(at.ctxFor("hook")).With(logging.Context{ActivityID: unitID}).
			LogVerbose("hook %q failed without blocking; carried on the non_blocking_error arm", str(pick(attachment, "hookName", "hook_name")))
	default:
		hook.Result = &conversationv1.AgentHook_Cancelled{Cancelled: &conversationv1.AgentHookCancelled{}}
	}

	c.log.With(at.ctxFor("hook")).With(logging.Context{ActivityID: unitID, UpsertKey: ActivityKey(unitID)}).
		LogVerbose("hook attachment kind=%s", kind)

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
		c.log.With(at.ctxWarn("diagnostics")).
			Log("IDE diagnostics follow no write or edit this reader observed; stored as vendor_specific rather than attached by guess")
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
	c.log.With(at.ctxFor("diagnostics")).With(logging.Context{ActivityID: unit, UpsertKey: ActivityKey(unit)}).
		Log("IDE diagnostics joined by adjacency to the last change unit (%d file(s))", len(report.Files))

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

	c.log.With(at.ctxFor("context-injected")).With(logging.Context{ActivityID: unitID, UpsertKey: ActivityKey(unitID)}).
		LogVerbose("memory file %q injected", path)

	activity := item(&conversationv1.AgentActivity_ContextInjected{ContextInjected: &conversationv1.AgentContextInjected{
		Injected: &conversationv1.AgentContextInjected_Memory{Memory: &conversationv1.AgentInjectedMemory{
			Path:    path,
			Content: firstNonEmpty(str(content["content"]), str(attachment["contentText"])),
		}},
	}})
	activity.ActivityId = activityID(unitID)
	return c.landFrame(at, agent, ActivityKey(unitID), "context_injected", activityFrame(agent, activity))
}

// ---------------------------------------------------------------------------
// the context-budget warning
// ---------------------------------------------------------------------------

// contextBudgetAttachment is the vendor's attachment type for the warning it
// injects into the prompt as the context window fills.
//
// SYNTHETIC, AND SAID SO. No capture in testdata/corpus or in the checked-in
// transcript carries this record, so the spelling is taken from the proto's
// description of it (a prompt-injected attachment whose text is the warning)
// rather than from an observed line. It is the one conversion here not grounded
// in a real fixture, and it is a CONCERN for the next capture run: if the vendor
// names the type differently, this converter withholds the record as
// vendor_specific like any other unmodeled attachment, which is a visible
// residue entry rather than a silent loss.
const contextBudgetAttachment = "context_budget_warning"

// contextBudgetWarning converts the vendor's context-budget warning.
//
// A FILE-PLANE FACT WITH NO STREAM PRODUCER: it exists only as an attachment
// line in the agent's transcript, so the sidecar is its ONLY producer (landing 4
// moved it off SessionUpdate, which had no producer on the live session stream).
// It is a page line of the agent's own book, instantaneous, with no lifecycle:
// the footer's activity line draws it and nothing settles it later.
func (c *Converter) contextBudgetWarning(attachment map[string]any, at Attribution, env envelope, agent string) *storev1.StoreEntry {
	// The vendor composes the sentence; no structured figure rides the record,
	// so the text is carried verbatim and nothing is parsed out of it.
	text := firstNonEmpty(
		str(attachment["text"]),
		str(pick(attachment, "warning", "message")),
		str(obj(attachment["content"])["text"]),
	)
	key := SessionKey(contextBudgetAttachment, env.uuid)
	if text == "" {
		// The record's whole content IS the sentence, so one without it carries
		// nothing to draw. Stored whole rather than landed as an empty warning.
		c.log.With(at.ctxWarn("context-budget-warning")).With(logging.Context{UpsertKey: key}).
			Log("context-budget warning carries no text; there is nothing to draw and the record is stored as vendor_specific")
		return VendorSpecificEntry(at, "attachment/"+contextBudgetAttachment, attachment)
	}
	c.log.With(at.ctxFor("context-budget-warning")).With(logging.Context{UpsertKey: key}).
		LogVerbose("context-budget warning injected chars=%d", len(text))
	return c.landFrame(at, agent, key, "context_budget_warning", updateFrame(agent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ContextBudgetWarning{
			ContextBudgetWarning: &conversationv1.ContextBudgetWarning{Text: text},
		},
	}))
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

	c.log.With(at.ctxFor("context-injected")).With(logging.Context{ActivityID: unitID, UpsertKey: ActivityKey(unitID)}).
		LogVerbose("attachment/%s injected %d skill(s)", kind, len(injected.Skills))

	activity := item(&conversationv1.AgentActivity_ContextInjected{ContextInjected: &conversationv1.AgentContextInjected{
		Injected: &conversationv1.AgentContextInjected_Skills{Skills: injected},
	}})
	activity.ActivityId = activityID(unitID)
	return c.landFrame(at, agent, ActivityKey(unitID), "context_injected", activityFrame(agent, activity))
}
