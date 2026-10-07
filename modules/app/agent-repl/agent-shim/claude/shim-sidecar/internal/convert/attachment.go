package convert

// attachment.go — the `attachment` records: hooks firing around the agent's
// work, the IDE's diagnostics, and context the vendor SILENTLY pulled in.
//
// Most attachment types are context-cut exclusions and CLI machinery, withheld
// so no resolver ever sees them as prose. Three families are conversation facts
// and are modeled: the diagnostics report, the injected memory/skills, and the
// skill listing. The HOOK outcomes are read and kept whole but deliberately
// UNSERVED — the stream plane owns the hook row (ruling 2026-09-04, see
// hookAttachment).

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
		return []*storev1.StoreEntry{c.hookAttachment(kind, record, attachment, at)}
	case "diagnostics":
		return c.diagnosticsAttachment(attachment, at, env, agent)
	case "nested_memory":
		return []*storev1.StoreEntry{c.injectedMemory(attachment, at, env, agent)}
	case "dynamic_skill", "invoked_skills", "skill_listing":
		return []*storev1.StoreEntry{c.injectedSkills(kind, attachment, at, env, agent)}
	case "queued_command":
		// A NOTIFICATION THAT ARRIVED MID-TURN is written as a queued command,
		// its `<task-notification>` in `prompt`: the same conclusion a
		// user-role notification states, so it is read by the same
		// conversion. Missing it left every run that ended while the agent was
		// busy standing open in the record (2026-10-07).
		if prompt := str(attachment["prompt"]); str(attachment["commandMode"]) == originTaskNotification || isTaskNotification(prompt) {
			return c.taskNotification(record, prompt, at, env, agent)
		}
		c.log.With(at.ctxFor("withhold")).
			LogVerbose("attachment/queued_command carrying no task notification withheld as vendor_specific")
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "attachment/queued_command", record)}
	default:
		// Context-cut exclusions and CLI machinery: understood, and deliberately
		// not carried into a vendor-agnostic feed. A `context_budget_warning`
		// lands here too: no footer line warns that the context is nearly full
		// (owner ruling, 2026-10-06), and the real CLI never writes one.
		c.log.With(at.ctxFor("withhold")).
			LogVerbose("attachment/%s withheld as vendor_specific", kind)
		return []*storev1.StoreEntry{VendorSpecificEntry(at, "attachment/"+kind, record)}
	}
}

// ---------------------------------------------------------------------------
// hooks
// ---------------------------------------------------------------------------

// hookAttachment stores one hook firing the vendor recorded in the transcript.
//
// THE STREAM PLANE OWNS THE SERVED HOOK ROW (ruling 2026-09-04), which is why
// nothing here lands on a page. A hook reaches this system on BOTH planes by
// vendor design, and the two planes are handed DISJOINT IDENTITY MATERIAL: the
// stream's `hook_started`/`hook_response` pair carries a `hook_id` and never a
// `tool_use_id`, the transcript's attachment carries a `toolUseID` and never a
// `hook_id`, and the two records' uuids differ. No key spans them, so a hook
// converted on both planes drew TWO rows a reader could not reconcile — the
// second of them degraded, since this plane writes no start frame and so
// carries neither the hook's name nor its event.
//
// It is the SAME precedent R15 sets for the transcript's user records (user.go):
// the shim's row is the one served form, and the file plane keeps the record
// durable and investigable as an UNSERVED item rather than regrowing a second,
// poorer copy of a row the stream already serves. The cost is stated rather than
// hidden: a transcript-only session — one this sidecar read with no shim ever
// having watched it — shows its hooks as unserved items, not as live rows.
//
// THE KEY IS THE ATTACHMENT RECORD'S OWN UUID (ResidueKey), which is also what
// makes distinct firings distinct: the vendor reuses one `toolUseID` across
// every hook that gates the same call — four PreToolUse:Bash firings share it in
// testdata/captures/hook-blocked — so the old `hook:<hookName>:<toolUseID>`
// identity collapsed them onto one row and left only the last.
func (c *Converter) hookAttachment(kind string, record, attachment map[string]any, at Attribution) *storev1.StoreEntry {
	name := str(pick(attachment, "hookName", "hook_name"))
	// VERBOSE, NOT A WARNING. A hook the vendor recorded as blocking or failing
	// is CONTENT this copier carried perfectly. A warning here would say "the
	// sidecar is degraded" about a transcript that merely describes a blocked
	// call, and the census that drives the reader's warnings to zero could then
	// never reach zero over a real capture.
	log := c.log.With(at.ctxFor("hook")).With(logging.Context{UpsertKey: ResidueKey(at)})
	switch kind {
	case "hook_blocking_error":
		log.LogVerbose("hook %q BLOCKED the gated call; the stream plane serves the row and the record is kept whole as an unserved item", name)
	case "hook_non_blocking_error":
		log.LogVerbose("hook %q failed without blocking; the stream plane serves the row and the record is kept whole as an unserved item", name)
	default:
		log.LogVerbose("hook attachment kind=%s kept whole as an unserved item; the stream plane serves the row", kind)
	}
	return VendorSpecificEntry(at, "attachment/"+kind, record)
}

// No start frame is minted here either: the vendor writes ONE transcript record
// per hook execution carrying its outcome, never a separate announcement, so a
// start frame would be a fact this reader invented. AgentHookStart.gated_call
// likewise stays UNSET on the stream (daemon.md ~1306) — it is never invented
// from this plane's `toolUseID`, which names the gated call but belongs to a
// record the stream's firing cannot be joined to.

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
		//
		// BENIGN ON A RE-SCAN — debug, not warn. An in-window write or edit
		// ALWAYS sets lastChangeUnit, so an empty lastChangeUnit means no such
		// change was observed in this reader's window: a cursor-resumed reader
		// (a cold restart, a boot rewind) legitimately sees the report after its
		// causing change scrolled past the cursor. Storing it whole as residue
		// is the correct forward path — the residue record IS the coverage — so
		// this must not flood the strict all-logs harvest. A diagnostics report
		// whose change IS in-window cannot reach this branch (lastChangeUnit is
		// set and the report attaches below); were that attachment ever to fail
		// it would be a real defect deserving a warn, which is why the
		// distinction lives in lastChangeUnit and not in the severity here.
		c.log.With(at.ctxFor("diagnostics")).
			LogVerbose("IDE diagnostics follow no write or edit this reader observed; stored as vendor_specific rather than attached by guess")
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
	unitID := ContextInjectedUnitID("memory", agent, env.uuid)

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

// injectedSkills carries skills discovered or invoked WITHOUT a tool call.
func (c *Converter) injectedSkills(kind string, attachment map[string]any, at Attribution, env envelope, agent string) *storev1.StoreEntry {
	unitID := ContextInjectedUnitID("skills", agent, env.uuid)
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
