package feed

import (
	"fmt"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/effortlevel"
	"claude-repld/internal/feedid"
)

// The response-styled bubbles: the plan, the findings report, the published
// artifact — and the hook card, which is the grey shell hook-flavored.

// ---- THE PLAN BUBBLE ----

// drawPlan draws the plan-mode pair as ONE bubble. The vendor makes two calls
// with two identities and offers no pairing key; the daemon coalesces them by
// the EPISODE INVARIANT — an agent has at most one open plan episode at a time
// — so entering draws the planning state and the exit fills the same bubble.
//
// THE BUBBLE'S IDENTITY IS THE OPENING CALL'S ACTIVITY ID, exactly as every
// other unit here keys on `act.GetActivityId()`. That is what makes the two
// planes — the shim's stream and the sidecar's file tail, which convert the
// SAME vendor record — collapse onto one row instead of drawing it twice.
func (r *resolver) drawPlan(s *wsState, at placement, agent *conversationv1.AgentId, act *conversationv1.AgentActivity, plan *conversationv1.AgentPlanMode) (*frontendv1.FeedRow, error) {
	agentID := agent.GetValue()
	unitID := act.GetActivityId().GetValue()

	// WHICH EPISODE THIS CALL BELONGS TO, and it is answered from the call's
	// own identity first. A call already attributed to an episode is attributed
	// to the SAME one however often it arrives; otherwise it joins the agent's
	// open episode, and opens one keyed on itself when there is none.
	episode := s.planUnits[unitID]
	if episode == nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "episode == nil"})
		episode = s.plans[agentID]
		if episode == nil || episode.closed {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "episode == nil || episode.closed"})
			episode = &planState{opener: unitID, feed: at}
			s.plans[agentID] = episode
		}
		s.planUnits[unitID] = episode
	}
	if episode.closed {
		// A SETTLED EPISODE IS INERT. Its bubble already carries its final
		// state, and the only frames that can still arrive for it are the
		// other plane's copies of the calls it was drawn from — a `start` among
		// them, which redrawn would put a finished plan back into its planning
		// treatment. Nothing is lost by declining: the row stands as drawn.
		r.logger(s.id).Debug("daemon.feed.plan_redelivered",
			"a plan-mode call arrived again for an episode already settled; the bubble stands as drawn",
			dlog.Context{"agent": agentID, "episode": episode.opener, "unit": unitID})
		return nil, errNotARow
	}

	bubble := &frontendv1.FeedPlan{}
	switch frame := plan.GetState().(type) {
	case *conversationv1.AgentPlanMode_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawPlan", "branch": "case *conversationv1.AgentPlanMode_Start"})
		switch frame.Start.GetAct().(type) {
		case *conversationv1.AgentPlanModeStart_Enter:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawPlan", "branch": "case *conversationv1.AgentPlanModeStart_Enter"})
			bubble.State = &frontendv1.FeedPlan_Planning{Planning: &frontendv1.FeedPlanPlanning{}}
		case *conversationv1.AgentPlanModeStart_Exit:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawPlan", "branch": "case *conversationv1.AgentPlanModeStart_Exit"})
			// AN EXIT WITH NO ENTER IS LEGAL: a session started in the plan
			// permission mode never calls EnterPlanMode at all.
			bubble.State = &frontendv1.FeedPlan_Planning{Planning: &frontendv1.FeedPlanPlanning{}}
		default:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawPlan", "branch": "default"})
			return nil, errNotARow
		}
	case *conversationv1.AgentPlanMode_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawPlan", "branch": "case *conversationv1.AgentPlanMode_Success"})
		switch outcome := frame.Success.GetAct().(type) {
		case *conversationv1.AgentPlanModeSuccess_Entered:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawPlan", "branch": "case *conversationv1.AgentPlanModeSuccess_Entered"})
			bubble.State = &frontendv1.FeedPlan_Planning{Planning: &frontendv1.FeedPlanPlanning{}}
		case *conversationv1.AgentPlanModeSuccess_Exited:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawPlan", "branch": "case *conversationv1.AgentPlanModeSuccess_Exited"})
			planned := &frontendv1.FeedPlanPlanned{
				Prose: &frontendv1.FeedPlanProse{Markdown: outcome.Exited.GetPlan().GetMarkdown()},
			}
			// The edit affordance draws only when the vendor NAMED the file.
			if outcome.Exited.FilePath != nil && outcome.Exited.GetFilePath() != "" {
				r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "outcome.Exited.FilePath != nil && outcome.Exited.GetFilePath() != \"\""})
				planned.Edit = &frontendv1.FeedPlanEditTarget{Path: outcome.Exited.GetFilePath()}
			}
			bubble.State = &frontendv1.FeedPlan_Planned{Planned: planned}
			// The episode is over; the next enter opens a new one. It is KEPT
			// rather than deleted so a re-delivery of either of its calls is
			// recognized as one instead of opening a second episode.
			episode.closed = true
		default:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawPlan", "branch": "default"})
			return nil, errNotARow
		}
	case *conversationv1.AgentPlanMode_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawPlan", "branch": "case *conversationv1.AgentPlanMode_Failure"})
		reason := planFailureText(frame.Failure.GetError())
		bubble.State = &frontendv1.FeedPlan_Failed{Failed: &frontendv1.FeedPlanFailed{
			Text: reason, Marker: planFailedMarker(reason),
		}}
		episode.closed = true
	default:
		return nil, errNotARow
	}

	id := r.rowID(s.id, episode.feed.feed, feedid.RowKey{
		Kind: feedid.KindActivity,
		ID:   "plan:" + episode.opener,
	})
	episode.row = id
	r.logger(s.id).Debug("daemon.feed.plan",
		"a plan-mode call was coalesced onto its episode's bubble",
		dlog.Context{"agent": agentID, "episode": episode.opener, "row": id.GetValue()})
	return &frontendv1.FeedRow{
		Id: id,
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Plan{Plan: bubble},
		}},
	}, nil
}

// breakPlanEpisodes fails every open plan episode. A turn that ended still in
// plan mode BROKE its episode: the bubble says so rather than sitting in the
// planning state forever.
func (r *resolver) breakPlanEpisodes(s *wsState, reason string) {
	for agentID, episode := range s.plans {
		if episode.closed {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "episode.closed"})
			// An episode that already reached its final state is not open, and
			// breaking it would put "the turn ended while plan mode was still
			// open" over a plan the agent DID present. It is only still in this
			// map so a re-delivery of its calls is recognized as one.
			continue
		}
		row := &frontendv1.FeedRow{
			Id: r.rowID(s.id, episode.feed.feed, feedid.RowKey{
				Kind: feedid.KindActivity,
				ID:   "plan:" + episode.opener,
			}),
			Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
				Unit: &frontendv1.FeedTurnActivity_Plan{Plan: &frontendv1.FeedPlan{
					State: &frontendv1.FeedPlan_Failed{Failed: &frontendv1.FeedPlanFailed{Text: reason, Marker: planFailedMarker(reason)}},
				}},
			}},
		}
		r.logger(s.id).Debug("daemon.feed.plan_episode_broken",
			"a plan episode broke and its bubble was failed",
			dlog.Context{"agent": agentID, "episode": episode.opener, "reason": reason})
		r.stampTurn(s, row, nil)
		r.upsert(s, episode.feed, row, true)
		episode.closed = true
	}
}

// planFailureText composes the reason a plan-mode call failed.
func planFailureText(failure *conversationv1.AgentToolFailure) string {
	if text := failureText(failure); text != "" {
		return text
	}
	return "the plan-mode call failed, and the producer gave no account"
}

// ---- THE FINDINGS BUBBLE ----

// drawFindings draws a review's typed defect list. The rows keep the tool's
// own order — most-severe first by its contract — and the client never
// re-sorts.
func (r *resolver) drawFindings(s *wsState, at placement, act *conversationv1.AgentActivity, report *conversationv1.AgentReportFindings) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	success, ok := report.GetState().(*conversationv1.AgentReportFindings_Success)
	if !ok {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!ok"})
		// A report that has not landed draws nothing: the bubble's substance
		// is the findings, and there is no in-flight treatment for it.
		return nil, errNotARow
	}

	findings := success.Success.GetFindings()
	rows := make([]*frontendv1.FeedFindingsRow, 0, len(findings))
	for _, finding := range findings {
		rows = append(rows, findingRow(finding))
	}
	bubble := &frontendv1.FeedFindings{
		Heading: &frontendv1.FeedFindingsHeading{Text: findingsHeading(findings, success.Success.GetLevel())},
		Rows:    rows,
	}
	return &frontendv1.FeedRow{
		Id: r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindActivity, ID: unitID}),
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Findings{Findings: bubble},
		}},
	}, nil
}

// findingsHeading composes the bubble's heading line.
func findingsHeading(findings []*conversationv1.AgentFinding, level conversationv1.AgentEffortLevel) string {
	if len(findings) == 0 {
		return "Findings · none"
	}
	heading := fmt.Sprintf("Findings · %d", len(findings))
	if word := effortlevel.Word(level); word != "" {
		heading = heading + " · " + word
	}
	return heading
}

// findingRow renders one finding: the verdict badge, the category chip, the
// location line (drawn AND a jump target), the summary and the folded scenario.
func findingRow(finding *conversationv1.AgentFinding) *frontendv1.FeedFindingsRow {
	row := &frontendv1.FeedFindingsRow{
		Location: findingLocation(finding),
		Summary:  &frontendv1.FeedFindingsSummary{Text: finding.GetSummary()},
		Scenario: &frontendv1.FeedFindingsScenario{Text: finding.GetFailureScenario()},
	}
	switch finding.GetVerdict().(type) {
	case *conversationv1.AgentFinding_Confirmed:
		row.Verdict = &frontendv1.FeedFindingsRow_Confirmed{Confirmed: &frontendv1.FeedFindingsVerdictConfirmed{}}
	case *conversationv1.AgentFinding_Plausible:
		row.Verdict = &frontendv1.FeedFindingsRow_Plausible{Plausible: &frontendv1.FeedFindingsVerdictPlausible{}}
	}
	if finding.Category != nil && finding.GetCategory() != "" {
		row.Category = &frontendv1.FeedFindingsCategory{Text: finding.GetCategory()}
	}
	switch finding.GetOutcome().(type) {
	case *conversationv1.AgentFinding_Fixed:
		row.Outcome = &frontendv1.FeedFindingsRow_Fixed{Fixed: &frontendv1.FeedFindingsOutcomeFixed{}}
	case *conversationv1.AgentFinding_Skipped:
		row.Outcome = &frontendv1.FeedFindingsRow_Skipped{Skipped: &frontendv1.FeedFindingsOutcomeSkipped{}}
	case *conversationv1.AgentFinding_NoChangeNeeded:
		row.Outcome = &frontendv1.FeedFindingsRow_NoChange{NoChange: &frontendv1.FeedFindingsOutcomeNoChange{}}
	}
	return row
}

// findingLocation composes the drawn location line and its jump target.
func findingLocation(finding *conversationv1.AgentFinding) *frontendv1.FeedFindingsLocation {
	location := &frontendv1.FeedFindingsLocation{Text: finding.GetFile(), Path: finding.GetFile()}
	if finding.Line != nil {
		line := finding.GetLine()
		location.Text = fmt.Sprintf("%s:%d", finding.GetFile(), line)
		location.Line = &line
	}
	return location
}

// ---- THE ARTIFACT BUBBLE ----

// drawArtifact draws a published page. Only a PUBLISH draws: a list act is a
// quiet read and produces no row.
func (r *resolver) drawArtifact(s *wsState, at placement, act *conversationv1.AgentActivity, artifact *conversationv1.AgentArtifact) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	u := s.unit(unitID)

	bubble := &frontendv1.FeedArtifact{}
	switch frame := artifact.GetResult().(type) {
	case *conversationv1.AgentArtifact_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawArtifact", "branch": "case *conversationv1.AgentArtifact_Start"})
		publish, ok := frame.Start.GetAct().(*conversationv1.AgentArtifactStart_Publish)
		if !ok {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!ok"})
			return nil, errNotARow
		}
		u.startedAtMs = frame.Start.GetStartedAtMs()
		u.artifactFavicon = publish.Publish.GetFavicon()
		u.input = artifactHeading(u.artifactFavicon, publish.Publish.GetTitle(), publish.Publish.GetFilePath())
		bubble.Heading = &frontendv1.FeedArtifactHeading{Text: u.input}
		bubble.State = &frontendv1.FeedArtifact_Publishing{Publishing: &frontendv1.FeedArtifactPublishing{}}
	case *conversationv1.AgentArtifact_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawArtifact", "branch": "case *conversationv1.AgentArtifact_Success"})
		published, ok := frame.Success.GetOutcome().(*conversationv1.AgentArtifactSuccess_Published)
		if !ok {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!ok"})
			return nil, errNotARow
		}
		heading := u.input
		if title := published.Published.GetTitle(); title != "" {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "title := published.Published.GetTitle(); title != \"\""})
			// The outcome's title WINS over the one the call announced, and the
			// favicon still comes from the call: the publish is the only frame
			// that carries one, so recomposing from the outcome alone dropped
			// the glyph off the finished card (photographed by a headless
			// sandbox run).
			heading = artifactHeading(u.artifactFavicon, title, "")
		}
		bubble.Heading = &frontendv1.FeedArtifactHeading{Text: heading}
		bubble.State = &frontendv1.FeedArtifact_Published{Published: &frontendv1.FeedArtifactPublished{
			Url: &frontendv1.FeedArtifactUrl{Url: published.Published.GetUrl()},
		}}
	case *conversationv1.AgentArtifact_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawArtifact", "branch": "case *conversationv1.AgentArtifact_Failure"})
		// THE FAILURE RESTATES THE ACT, so a replay serving it with no start
		// beside it still draws the failed publish's card. A failed LIST is a
		// quiet read and draws nothing, exactly as its start does.
		switch restated := frame.Failure.GetAct().(type) {
		case *conversationv1.AgentArtifactFailure_List:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawArtifact", "branch": "case *conversationv1.AgentArtifactFailure_List"})
			return nil, errNotARow
		case *conversationv1.AgentArtifactFailure_Publish:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawArtifact", "branch": "case *conversationv1.AgentArtifactFailure_Publish"})
			if u.input == "" {
				u.artifactFavicon = restated.Publish.GetFavicon()
				u.input = artifactHeading(u.artifactFavicon, restated.Publish.GetTitle(), restated.Publish.GetFilePath())
			}
		default:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawArtifact", "branch": "default (the failure restates no act)"})
			if u.input == "" {
				// Neither restated nor held: nothing names the publish.
				return nil, unrestatedErr(act, "artifact")
			}
			r.unrestated(s, act, "artifact", "act", "the start this process held")
		}
		bubble.Heading = &frontendv1.FeedArtifactHeading{Text: u.input}
		bubble.State = &frontendv1.FeedArtifact_Failed{Failed: &frontendv1.FeedArtifactFailed{
			Text: artifactFailureText(frame.Failure.GetFailure()),
		}}
	default:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawArtifact", "branch": "default"})
		return nil, errNotARow
	}

	return &frontendv1.FeedRow{
		Id: r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindActivity, ID: unitID}),
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Artifact{Artifact: bubble},
		}},
	}, nil
}

// artifactHeading composes the bubble's heading: the favicon and the title,
// falling back to the file's name when the call named no title.
func artifactHeading(favicon, title, filePath string) string {
	if title == "" {
		title = filePath
		if i := strings.LastIndexByte(title, '/'); i >= 0 {
			title = title[i+1:]
		}
	}
	if favicon == "" {
		return title
	}
	return favicon + " " + title
}

// artifactFailureText composes the reason a publish failed.
func artifactFailureText(failure *conversationv1.AgentToolFailure) string {
	if text := failureText(failure); text != "" {
		return text
	}
	return "the publish failed, and the producer gave no account"
}

// ---- THE HOOK CARD ----

// drawHook draws a FAILING hook. A succeeded hook draws NOTHING: quiet
// automation stays quiet, and only a refusal or a failure is the user's
// business.
func (r *resolver) drawHook(s *wsState, at placement, act *conversationv1.AgentActivity, hook *conversationv1.AgentHook) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	u := s.unit(unitID)

	card := &frontendv1.FeedHook{}
	switch frame := hook.GetResult().(type) {
	case *conversationv1.AgentHook_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawHook", "branch": "case *conversationv1.AgentHook_Start"})
		u.input = hookHeadline(frame.Start.GetHookName(), frame.Start.GetEvent(), false)
		if gated := frame.Start.GetGatedCall(); gated.GetValue() != "" {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "gated := frame.Start.GetGatedCall(); gated.GetValue() != \"\""})
			s.gatedCalls["hook:"+unitID] = gated.GetValue()
		}
		return nil, errNotARow
	case *conversationv1.AgentHook_BlockingError:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawHook", "branch": "case *conversationv1.AgentHook_BlockingError"})
		card.Headline = &frontendv1.FeedHookHeadline{Text: hookBlockedHeadline(u.input)}
		card.Outcome = &frontendv1.FeedHook_Blocked{Blocked: &frontendv1.FeedHookBlocked{
			Reason: frame.BlockingError.GetBlockingText(),
		}}
	case *conversationv1.AgentHook_NonBlockingError:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawHook", "branch": "case *conversationv1.AgentHook_NonBlockingError"})
		card.Headline = &frontendv1.FeedHookHeadline{Text: hookFailedHeadline(u.input)}
		failed := &frontendv1.FeedHookFailed{ExitCode: frame.NonBlockingError.GetExitCode()}
		if text := hookOutputText(frame.NonBlockingError.GetOutput()); text != "" {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "text := hookOutputText(frame.NonBlockingError.GetOutput()); text != \"\""})
			failed.Output = &frontendv1.FeedHookOutput{Text: text}
		}
		card.Outcome = &frontendv1.FeedHook_Failed{Failed: failed}
	default:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawHook", "branch": "default"})
		// Succeeded and cancelled draw nothing.
		return nil, errNotARow
	}

	if gated, ok := s.gatedCalls["hook:"+unitID]; ok {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "gated, ok := s.gatedCalls[\"hook:\"+unitID]; ok"})
		if u := s.units[gated]; u != nil && u.row != nil {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "u := s.units[gated]; u != nil && u.row != nil"})
			card.GatedCall = &frontendv1.FeedHookGatedCall{Row: u.row.GetId()}
		}
	}
	return &frontendv1.FeedRow{
		Id: r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindActivity, ID: unitID}),
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Hook{Hook: card},
		}},
	}, nil
}

// hookHeadline composes the hook's identifying phrase.
func hookHeadline(name string, event conversationv1.AgentHookEvent, blocked bool) string {
	return fmt.Sprintf("%s (%s)", name, hookEventWord(event))
}

// hookBlockedHeadline words the loud treatment's head line.
func hookBlockedHeadline(phrase string) string {
	if phrase == "" {
		return "hook blocked"
	}
	return "hook blocked: " + phrase
}

// hookFailedHeadline words the ordinary treatment's head line.
func hookFailedHeadline(phrase string) string {
	if phrase == "" {
		return "hook failed"
	}
	return "hook failed: " + phrase
}

// hookOutputText joins a hook's two streams for the capped card.
func hookOutputText(output *conversationv1.AgentHookOutput) string {
	parts := make([]string, 0, 2)
	if output.GetStdout() != "" {
		parts = append(parts, output.GetStdout())
	}
	if output.GetStderr() != "" {
		parts = append(parts, output.GetStderr())
	}
	return strings.Join(parts, "\n")
}

// hookEventWord names the lifecycle event that fired a hook, in the vendor's
// own spelling minus its enum prefix.
func hookEventWord(event conversationv1.AgentHookEvent) string {
	name := event.String()
	name = strings.TrimPrefix(name, "AGENT_HOOK_EVENT_")
	parts := strings.Split(strings.ToLower(name), "_")
	for i, part := range parts {
		if part == "" {
			continue
		}
		parts[i] = strings.ToUpper(part[:1]) + part[1:]
	}
	return strings.Join(parts, "")
}
