package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// The response-styled bubbles — plan, findings, artifact — and the hook card.

// planBubble finds the plan bubble on the root feed.
func (h *harness) planBubble() *frontendv1.FeedPlan {
	h.t.Helper()
	for _, row := range h.rows(rootFeed()) {
		if plan := row.GetActivity().GetPlan(); plan != nil {
			return plan
		}
	}
	h.t.Fatal("no plan bubble on the root feed")
	return nil
}

// planFrame sends one plan-mode frame.
func (h *harness) planFrame(unit string, state any) {
	h.t.Helper()
	plan := &conversationv1.AgentPlanMode{}
	switch s := state.(type) {
	case *conversationv1.AgentPlanModeStart:
		plan.State = &conversationv1.AgentPlanMode_Start{Start: s}
	case *conversationv1.AgentPlanModeSuccess:
		plan.State = &conversationv1.AgentPlanMode_Success{Success: s}
	case *conversationv1.AgentPlanModeFailure:
		plan.State = &conversationv1.AgentPlanMode_Failure{Failure: s}
	}
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item:       &conversationv1.AgentActivity_PlanMode{PlanMode: plan},
	})
}

// ---- THE PLAN BUBBLE ----

func TestEnteringPlanModeDrawsThePlanningTreatment(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.planFrame("unit-1", &conversationv1.AgentPlanModeStart{
		Act:       &conversationv1.AgentPlanModeStart_Enter{Enter: &conversationv1.AgentPlanModeEnter{}},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert.
	if h.planBubble().GetPlanning() == nil {
		t.Fatalf("state = %T, want planning", h.planBubble().GetState())
	}
}

func TestTheEnterAndExitCoalesceOntoOneBubble(t *testing.T) {
	// Arrange: the vendor makes TWO calls with two identities and offers no
	// pairing key.
	h := newHarness(t)
	h.planFrame("unit-1", &conversationv1.AgentPlanModeStart{
		Act:       &conversationv1.AgentPlanModeStart_Enter{Enter: &conversationv1.AgentPlanModeEnter{}},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	entered := h.planBubble()

	// Act: the exit arrives as a DIFFERENT unit.
	path := "/tmp/plan.md"
	h.planFrame("unit-2", &conversationv1.AgentPlanModeSuccess{
		Act: &conversationv1.AgentPlanModeSuccess_Exited{Exited: &conversationv1.AgentPlanModeExited{
			Plan:     &conversationv1.AgentResponseProse{Markdown: "## the plan"},
			FilePath: &path,
		}},
	})

	// Assert: ONE bubble, keyed by the episode rather than by either unit.
	bubbles := 0
	for _, row := range h.rows(rootFeed()) {
		if row.GetActivity().GetPlan() != nil {
			bubbles++
		}
	}
	if bubbles != 1 {
		t.Fatalf("plan bubbles = %d, want 1 — the pair coalesces", bubbles)
	}
	_ = entered
	planned := h.planBubble().GetPlanned()
	if planned.GetProse().GetMarkdown() != "## the plan" {
		t.Fatalf("plan = %q", planned.GetProse().GetMarkdown())
	}
	if planned.GetEdit().GetPath() != path {
		t.Fatalf("edit target = %q, want the vendor's named file", planned.GetEdit().GetPath())
	}
}

func TestThePlansEditAffordanceDrawsOnlyWhenTheVendorNamedTheFile(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.planFrame("unit-1", &conversationv1.AgentPlanModeSuccess{
		Act: &conversationv1.AgentPlanModeSuccess_Exited{Exited: &conversationv1.AgentPlanModeExited{
			Plan: &conversationv1.AgentResponseProse{Markdown: "## the plan"},
		}},
	})

	// Assert.
	if h.planBubble().GetPlanned().GetEdit() != nil {
		t.Fatal("the edit affordance drew with no file named")
	}
}

func TestAnExitWithNoEnterIsLegal(t *testing.T) {
	// Arrange, Act: a session started in the plan permission mode never calls
	// EnterPlanMode at all.
	h := newHarness(t)
	h.planFrame("unit-1", &conversationv1.AgentPlanModeSuccess{
		Act: &conversationv1.AgentPlanModeSuccess_Exited{Exited: &conversationv1.AgentPlanModeExited{
			Plan: &conversationv1.AgentResponseProse{Markdown: "## the plan"},
		}},
	})

	// Assert.
	if h.planBubble().GetPlanned() == nil {
		t.Fatalf("state = %T, want planned", h.planBubble().GetState())
	}
}

func TestASecondEpisodeGetsItsOwnBubble(t *testing.T) {
	// Arrange: one episode, opened and closed.
	h := newHarness(t)
	h.planFrame("unit-1", &conversationv1.AgentPlanModeStart{
		Act:       &conversationv1.AgentPlanModeStart_Enter{Enter: &conversationv1.AgentPlanModeEnter{}},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.planFrame("unit-2", &conversationv1.AgentPlanModeSuccess{
		Act: &conversationv1.AgentPlanModeSuccess_Exited{Exited: &conversationv1.AgentPlanModeExited{
			Plan: &conversationv1.AgentResponseProse{Markdown: "first"},
		}},
	})

	// Act: a second episode.
	h.planFrame("unit-3", &conversationv1.AgentPlanModeStart{
		Act:       &conversationv1.AgentPlanModeStart_Enter{Enter: &conversationv1.AgentPlanModeEnter{}},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 2_000},
	})

	// Assert: two bubbles — the second never collides with the first's id.
	bubbles := 0
	for _, row := range h.rows(rootFeed()) {
		if row.GetActivity().GetPlan() != nil {
			bubbles++
		}
	}
	if bubbles != 2 {
		t.Fatalf("plan bubbles = %d, want 2", bubbles)
	}
}

func TestAnEpisodeReDeliveredDrawsOntoTheSameBubble(t *testing.T) {
	// Arrange: one episode, opened and closed. The SAME vendor record reaches
	// this resolver twice — the shim's stream plane converts it, and the
	// sidecar's file tail converts it again from the transcript — so the exit
	// having closed the episode must not let the re-delivery mint a new one.
	h := newHarness(t)
	episode := func(markdown string) {
		h.t.Helper()
		h.planFrame("unit-1", &conversationv1.AgentPlanModeStart{
			Act:       &conversationv1.AgentPlanModeStart_Enter{Enter: &conversationv1.AgentPlanModeEnter{}},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		})
		h.planFrame("unit-2", &conversationv1.AgentPlanModeSuccess{
			Act: &conversationv1.AgentPlanModeSuccess_Exited{Exited: &conversationv1.AgentPlanModeExited{
				Plan: &conversationv1.AgentResponseProse{Markdown: markdown},
			}},
		})
	}
	episode("## the plan")

	// Act: the other plane delivers the same two calls.
	episode("## the plan")

	// Assert: ONE bubble. A second one is the duplicate plan card a headless
	// sandbox run photographed, drawn from the same record twice.
	bubbles := 0
	for _, row := range h.rows(rootFeed()) {
		if row.GetActivity().GetPlan() != nil {
			bubbles++
		}
	}
	if bubbles != 1 {
		t.Fatalf("plan bubbles = %d, want 1 — the same record re-delivered keys onto the same FeedId", bubbles)
	}
}

func TestASettledEpisodeReDeliveredStaysPlanned(t *testing.T) {
	// Arrange: an episode presented and settled.
	h := newHarness(t)
	h.planFrame("unit-1", &conversationv1.AgentPlanModeStart{
		Act:       &conversationv1.AgentPlanModeStart_Enter{Enter: &conversationv1.AgentPlanModeEnter{}},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.planFrame("unit-2", &conversationv1.AgentPlanModeSuccess{
		Act: &conversationv1.AgentPlanModeSuccess_Exited{Exited: &conversationv1.AgentPlanModeExited{
			Plan: &conversationv1.AgentResponseProse{Markdown: "## the plan"},
		}},
	})

	// Act: the other plane's copy of the ENTER arrives after the exit already
	// settled the bubble, which is the order the sidecar's file tail produces.
	h.planFrame("unit-1", &conversationv1.AgentPlanModeStart{
		Act:       &conversationv1.AgentPlanModeStart_Enter{Enter: &conversationv1.AgentPlanModeEnter{}},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Assert: the presented plan stands; it does not go back to planning.
	if h.planBubble().GetPlanned().GetProse().GetMarkdown() != "## the plan" {
		t.Fatalf("state = %T, want the planned bubble to stand", h.planBubble().GetState())
	}
}

func TestATurnEndingAfterThePlanWasPresentedLeavesItPlanned(t *testing.T) {
	// Arrange: an episode presented and settled within the turn.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "plan it")
	h.planFrame("unit-1", &conversationv1.AgentPlanModeStart{
		Act:       &conversationv1.AgentPlanModeStart_Enter{Enter: &conversationv1.AgentPlanModeEnter{}},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})
	h.planFrame("unit-2", &conversationv1.AgentPlanModeSuccess{
		Act: &conversationv1.AgentPlanModeSuccess_Exited{Exited: &conversationv1.AgentPlanModeExited{
			Plan: &conversationv1.AgentResponseProse{Markdown: "## the plan"},
		}},
	})

	// Act: the turn ends. Plan mode was NOT still open.
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert: the break is for OPEN episodes only.
	if h.planBubble().GetPlanned() == nil {
		t.Fatalf("state = %T, want the presented plan to stand", h.planBubble().GetState())
	}
}

func TestATurnThatEndsInPlanModeBreaksTheEpisode(t *testing.T) {
	// Arrange: an open episode.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "plan it")
	h.planFrame("unit-1", &conversationv1.AgentPlanModeStart{
		Act:       &conversationv1.AgentPlanModeStart_Enter{Enter: &conversationv1.AgentPlanModeEnter{}},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Act.
	h.terminal("turn-1", &conversationv1.AgentSuccess{
		Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
	}, nil)

	// Assert: the bubble says so rather than planning forever.
	failed := h.planBubble().GetFailed()
	if failed == nil {
		t.Fatalf("state = %T, want failed", h.planBubble().GetState())
	}
	if failed.GetText() != "the turn ended while plan mode was still open" {
		t.Fatalf("reason = %q", failed.GetText())
	}
	if !h.hasRecord("debug", "daemon.feed.plan_episode_broken") {
		t.Fatalf("records = %+v, want the break recorded", h.records())
	}
}

func TestAFailedPlanModeCallFailsTheBubble(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.planFrame("unit-1", &conversationv1.AgentPlanModeFailure{
		Error: &conversationv1.AgentToolFailure{
			Content: &conversationv1.ToolResultContent{Blocks: []*conversationv1.ToolResultContentBlock{{
				Block: &conversationv1.ToolResultContentBlock_Text{
					Text: &conversationv1.TextBlock{Text: "plan mode is unavailable"},
				},
			}}},
		},
	})

	// Assert.
	if got := h.planBubble().GetFailed().GetText(); got != "plan mode is unavailable" {
		t.Fatalf("reason = %q", got)
	}
}

// ---- THE FINDINGS BUBBLE ----

// findingsBubble finds the findings bubble.
func (h *harness) findingsBubble() *frontendv1.FeedFindings {
	h.t.Helper()
	for _, row := range h.rows(rootFeed()) {
		if findings := row.GetActivity().GetFindings(); findings != nil {
			return findings
		}
	}
	h.t.Fatal("no findings bubble on the root feed")
	return nil
}

func TestFindingsKeepTheToolsOwnOrder(t *testing.T) {
	// Arrange: most-severe first by the tool's contract.
	h := newHarness(t)
	line := uint32(214)
	category := "correctness"
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_ReportFindings{ReportFindings: &conversationv1.AgentReportFindings{
			State: &conversationv1.AgentReportFindings_Success{Success: &conversationv1.AgentReportFindingsSuccess{
				Level: conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH,
				Findings: []*conversationv1.AgentFinding{
					{
						File: "daemon/server.go", Line: &line,
						Summary:         "the walk is not per reader",
						FailureScenario: "two webviews page each other's feed",
						Category:        &category,
						Verdict: &conversationv1.AgentFinding_Confirmed{
							Confirmed: &conversationv1.AgentFindingConfirmed{},
						},
					},
					{File: "daemon/feed.go", Summary: "a second finding"},
				},
			}},
		}},
	})

	// Assert: the client never re-sorts, and the location is drawn AND a jump
	// target.
	bubble := h.findingsBubble()
	if bubble.GetHeading().GetText() != "Findings · 2 · high" {
		t.Fatalf("heading = %q", bubble.GetHeading().GetText())
	}
	rows := bubble.GetRows()
	if len(rows) != 2 || rows[0].GetSummary().GetText() != "the walk is not per reader" {
		t.Fatalf("rows = %+v, want the tool's order", rows)
	}
	if rows[0].GetLocation().GetText() != "daemon/server.go:214" {
		t.Fatalf("location text = %q", rows[0].GetLocation().GetText())
	}
	if rows[0].GetLocation().GetLine() != 214 {
		t.Fatalf("location line = %d, want the jump target", rows[0].GetLocation().GetLine())
	}
	if rows[0].GetConfirmed() == nil {
		t.Fatalf("verdict = %T, want confirmed", rows[0].GetVerdict())
	}
	if rows[0].GetCategory().GetText() != category {
		t.Fatalf("category = %q", rows[0].GetCategory().GetText())
	}
}

func TestAFindingWithNoLineJumpsToTheFilesTop(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_ReportFindings{ReportFindings: &conversationv1.AgentReportFindings{
			State: &conversationv1.AgentReportFindings_Success{Success: &conversationv1.AgentReportFindingsSuccess{
				Findings: []*conversationv1.AgentFinding{{File: "a.go", Summary: "s"}},
			}},
		}},
	})

	// Assert.
	row := h.findingsBubble().GetRows()[0]
	if row.GetLocation().Line != nil {
		t.Fatalf("line = %+v, want unset", row.GetLocation().Line)
	}
	if row.GetLocation().GetText() != "a.go" {
		t.Fatalf("location text = %q", row.GetLocation().GetText())
	}
}

func TestAReviewThatFoundNothingIsARealReport(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_ReportFindings{ReportFindings: &conversationv1.AgentReportFindings{
			State: &conversationv1.AgentReportFindings_Success{
				Success: &conversationv1.AgentReportFindingsSuccess{},
			},
		}},
	})

	// Assert.
	bubble := h.findingsBubble()
	if bubble.GetHeading().GetText() != "Findings · none" {
		t.Fatalf("heading = %q", bubble.GetHeading().GetText())
	}
	if len(bubble.GetRows()) != 0 {
		t.Fatalf("rows = %d, want none", len(bubble.GetRows()))
	}
}

func TestAReReportsOutcomeBadgeIsDrawn(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_ReportFindings{ReportFindings: &conversationv1.AgentReportFindings{
			State: &conversationv1.AgentReportFindings_Success{Success: &conversationv1.AgentReportFindingsSuccess{
				Findings: []*conversationv1.AgentFinding{{
					File: "a.go", Summary: "s",
					Outcome: &conversationv1.AgentFinding_Fixed{Fixed: &conversationv1.AgentFindingFixed{}},
				}},
			}},
		}},
	})

	// Assert.
	if h.findingsBubble().GetRows()[0].GetFixed() == nil {
		t.Fatalf("outcome = %T, want fixed", h.findingsBubble().GetRows()[0].GetOutcome())
	}
}

func TestAFindingsReportThatHasNotLandedDrawsNothing(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_ReportFindings{ReportFindings: &conversationv1.AgentReportFindings{
			State: &conversationv1.AgentReportFindings_Start{Start: &conversationv1.AgentReportFindingsStart{
				StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
			}},
		}},
	})

	// Assert: the bubble's substance IS the findings.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want none until the report lands", len(rows))
	}
}

// ---- THE ARTIFACT BUBBLE ----

// artifactBubble finds the artifact bubble.
func (h *harness) artifactBubble() *frontendv1.FeedArtifact {
	h.t.Helper()
	for _, row := range h.rows(rootFeed()) {
		if artifact := row.GetActivity().GetArtifact(); artifact != nil {
			return artifact
		}
	}
	h.t.Fatal("no artifact bubble on the root feed")
	return nil
}

func TestAPublishDrawsItsHeadingThenItsUrl(t *testing.T) {
	// Arrange: the publish is announced.
	h := newHarness(t)
	favicon, title := "📊", "Merge Queue Report"
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{
			Result: &conversationv1.AgentArtifact_Start{Start: &conversationv1.AgentArtifactStart{
				Act: &conversationv1.AgentArtifactStart_Publish{Publish: &conversationv1.AgentArtifactPublish{
					FilePath: "/tmp/report.html", Favicon: &favicon, Title: &title,
				}},
				StartedAtMs: 1_000,
			}},
		}},
	})
	if h.artifactBubble().GetPublishing() == nil {
		t.Fatalf("state = %T, want publishing", h.artifactBubble().GetState())
	}

	// Act.
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{
			Result: &conversationv1.AgentArtifact_Success{Success: &conversationv1.AgentArtifactSuccess{
				Outcome: &conversationv1.AgentArtifactSuccess_Published{
					Published: &conversationv1.AgentArtifactPublished{Url: "https://claude.ai/a/1"},
				},
			}},
		}},
	})

	// Assert.
	bubble := h.artifactBubble()
	if bubble.GetHeading().GetText() != "📊 Merge Queue Report" {
		t.Fatalf("heading = %q", bubble.GetHeading().GetText())
	}
	if bubble.GetPublished().GetUrl().GetUrl() != "https://claude.ai/a/1" {
		t.Fatalf("url = %q", bubble.GetPublished().GetUrl().GetUrl())
	}
}

func TestAPublishedTitleKeepsTheFaviconTheCallAnnounced(t *testing.T) {
	// Arrange: the call announces the favicon and a working title.
	h := newHarness(t)
	favicon, title := "📊", "Draft"
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{
			Result: &conversationv1.AgentArtifact_Start{Start: &conversationv1.AgentArtifactStart{
				Act: &conversationv1.AgentArtifactStart_Publish{Publish: &conversationv1.AgentArtifactPublish{
					FilePath: "/tmp/report.html", Favicon: &favicon, Title: &title,
				}},
				StartedAtMs: 1_000,
			}},
		}},
	})

	// Act: the outcome restates the title -- and never a favicon, which no
	// outcome carries.
	published := "Offline Report"
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{
			Result: &conversationv1.AgentArtifact_Success{Success: &conversationv1.AgentArtifactSuccess{
				Outcome: &conversationv1.AgentArtifactSuccess_Published{
					Published: &conversationv1.AgentArtifactPublished{Url: "https://claude.ai/a/1", Title: &published},
				},
			}},
		}},
	})

	// Assert: feed.proto words the heading as "favicon emoji + title", so the
	// finished card keeps the glyph it wore while it was publishing.
	if got := h.artifactBubble().GetHeading().GetText(); got != "📊 Offline Report" {
		t.Fatalf("heading = %q, want the outcome's title under the call's favicon", got)
	}
}

func TestAPublishWithNoTitleFallsBackToTheFilesName(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{
			Result: &conversationv1.AgentArtifact_Start{Start: &conversationv1.AgentArtifactStart{
				Act: &conversationv1.AgentArtifactStart_Publish{
					Publish: &conversationv1.AgentArtifactPublish{FilePath: "/tmp/deep/report.html"},
				},
				StartedAtMs: 1_000,
			}},
		}},
	})

	// Assert.
	if got := h.artifactBubble().GetHeading().GetText(); got != "report.html" {
		t.Fatalf("heading = %q, want the file's name", got)
	}
}

func TestAListingDrawsNoBubble(t *testing.T) {
	// Arrange, Act: a list act is a quiet read.
	h := newHarness(t)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{
			Result: &conversationv1.AgentArtifact_Start{Start: &conversationv1.AgentArtifactStart{
				Act:         &conversationv1.AgentArtifactStart_List{List: &conversationv1.AgentArtifactList{}},
				StartedAtMs: 1_000,
			}},
		}},
	})

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want none for a listing", len(rows))
	}
}

func TestAFailedPublishDrawsItsReasonWhereTheUrlWould(t *testing.T) {
	// Arrange: the publish was announced.
	h := newHarness(t)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{
			Result: &conversationv1.AgentArtifact_Start{Start: &conversationv1.AgentArtifactStart{
				Act: &conversationv1.AgentArtifactStart_Publish{
					Publish: &conversationv1.AgentArtifactPublish{FilePath: "/tmp/report.html"},
				},
				StartedAtMs: 1_000,
			}},
		}},
	})

	// Act.
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{
			Result: &conversationv1.AgentArtifact_Failure{Failure: &conversationv1.AgentArtifactFailure{
				Failure: &conversationv1.AgentToolFailure{
					Content: &conversationv1.ToolResultContent{Blocks: []*conversationv1.ToolResultContentBlock{{
						Block: &conversationv1.ToolResultContentBlock_Text{
							Text: &conversationv1.TextBlock{Text: "the page was refused"},
						},
					}}},
				},
			}},
		}},
	})

	// Assert.
	if got := h.artifactBubble().GetFailed().GetText(); got != "the page was refused" {
		t.Fatalf("reason = %q", got)
	}
}

// failedArtifact is an artifact call's failure, restating `act` (nil for a
// failure that restates none).
func failedArtifact(act any) *conversationv1.AgentActivity {
	failure := &conversationv1.AgentArtifactFailure{
		Failure: &conversationv1.AgentToolFailure{
			Content: &conversationv1.ToolResultContent{Blocks: []*conversationv1.ToolResultContentBlock{{
				Block: &conversationv1.ToolResultContentBlock_Text{Text: &conversationv1.TextBlock{Text: "the page was refused"}},
			}}},
		},
	}
	switch a := act.(type) {
	case *conversationv1.AgentArtifactPublish:
		failure.Act = &conversationv1.AgentArtifactFailure_Publish{Publish: a}
	case *conversationv1.AgentArtifactList:
		failure.Act = &conversationv1.AgentArtifactFailure_List{List: a}
	}
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{
			Result: &conversationv1.AgentArtifact_Failure{Failure: failure},
		}},
	}
}

func TestAReplayedFailedPublishDrawsItsCard(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	favicon, title := "📊", "Merge Queue Report"

	// Act: the failure alone, as a replay serves it.
	h.send(bound(failedArtifact(&conversationv1.AgentArtifactPublish{
		FilePath: "/tmp/report.html", Favicon: &favicon, Title: &title,
	})))

	// Assert.
	bubble := h.artifactBubble()
	if got := bubble.GetHeading().GetText(); got != "📊 Merge Queue Report" {
		t.Fatalf("heading = %q, want the restated publish's", got)
	}
	if got := bubble.GetFailed().GetText(); got != "the page was refused" {
		t.Fatalf("reason = %q", got)
	}
}

func TestAReplayedFailedListingDrawsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act: a listing is a quiet read, failed or not.
	h.send(bound(failedArtifact(&conversationv1.AgentArtifactList{})))

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want 0 for a failed listing", len(rows))
	}
}

func TestABoundArtifactFailureRestatingNoActIsRecordedAtError(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.send(bound(failedArtifact(nil)))

	// Assert.
	if !h.hasRecord("error", "daemon.feed.activity_undrawable") || len(h.rows(rootFeed())) != 0 {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.activity_undrawable and no row", h.records())
	}
}

func TestAPreContractArtifactFailureRestatingNoActIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act: a row written before the contract, replayed alone.
	h.send(failedArtifact(nil))

	// Assert.
	if !h.hasRecord("info", "daemon.feed.settle_predates_contract") || len(h.anyErrors()) != 0 {
		t.Fatalf("records = %+v, want an INFO daemon.feed.settle_predates_contract and no ERROR", h.records())
	}
}

func TestAPreContractArtifactFailureRestatingNoActDrawsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.send(failedArtifact(nil))

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want 0: nothing names the publish", len(rows))
	}
}

func TestABoundArtifactFailureRestatingNoActIsRecordedAtErrorWithTheStartHeld(t *testing.T) {
	// Arrange: the publish was announced.
	h := newHarness(t)
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{
			Result: &conversationv1.AgentArtifact_Start{Start: &conversationv1.AgentArtifactStart{
				Act: &conversationv1.AgentArtifactStart_Publish{
					Publish: &conversationv1.AgentArtifactPublish{FilePath: "/tmp/report.html"},
				},
				StartedAtMs: 1_000,
			}},
		}},
	})

	// Act.
	h.send(bound(failedArtifact(nil)))

	// Assert: drawn from the start, and recorded.
	if !h.hasRecord("error", "daemon.feed.settle_not_restated") {
		t.Fatalf("records = %+v, want an ERROR daemon.feed.settle_not_restated", h.records())
	}
	if got := h.artifactBubble().GetHeading().GetText(); got != "report.html" {
		t.Fatalf("heading = %q, want the held start's", got)
	}
}

// ---- THE HOOK CARD ----

// hookCard finds the hook card.
func (h *harness) hookCard() *frontendv1.FeedHook {
	h.t.Helper()
	for _, row := range h.rows(rootFeed()) {
		if hook := row.GetActivity().GetHook(); hook != nil {
			return hook
		}
	}
	h.t.Fatal("no hook card on the root feed")
	return nil
}

// hookStart announces one hook firing.
func (h *harness) hookStart(unit, name string, event conversationv1.AgentHookEvent, gated string) {
	h.t.Helper()
	start := &conversationv1.AgentHookStart{
		HookName:  name,
		Event:     event,
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	}
	if gated != "" {
		start.GatedCall = &conversationv1.AgentActivityId{Value: gated}
	}
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{
			Result: &conversationv1.AgentHook_Start{Start: start},
		}},
	})
}

func TestASucceededHookDrawsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.hookStart("unit-1", "protect-master", conversationv1.AgentHookEvent_AGENT_HOOK_EVENT_PRE_TOOL_USE, "")

	// Act.
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{
			Result: &conversationv1.AgentHook_Succeeded{Succeeded: &conversationv1.AgentHookSucceeded{
				Command: "true", ExitCode: 0,
			}},
		}},
	})

	// Assert: quiet automation stays quiet.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want none for a succeeded hook", len(rows))
	}
}

func TestABlockingHookDrawsItsOwnStatedReason(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.hookStart("unit-1", "protect-master", conversationv1.AgentHookEvent_AGENT_HOOK_EVENT_PRE_TOOL_USE, "")

	// Act.
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{
			Result: &conversationv1.AgentHook_BlockingError{
				BlockingError: &conversationv1.AgentHookBlockingError{
					Command:      "./protect.sh",
					BlockingText: "never push to master",
				},
			},
		}},
	})

	// Assert: the refusal text is the whole point.
	card := h.hookCard()
	if card.GetHeadline().GetText() != "hook blocked: protect-master (PreToolUse)" {
		t.Fatalf("headline = %q", card.GetHeadline().GetText())
	}
	if card.GetBlocked().GetReason() != "never push to master" {
		t.Fatalf("reason = %q", card.GetBlocked().GetReason())
	}
}

func TestAFailingHookDrawsItsExitChipAndCappedOutput(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.hookStart("unit-1", "lint", conversationv1.AgentHookEvent_AGENT_HOOK_EVENT_POST_TOOL_USE, "")

	// Act.
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{
			Result: &conversationv1.AgentHook_NonBlockingError{
				NonBlockingError: &conversationv1.AgentHookNonBlockingError{
					Command:  "./lint.sh",
					ExitCode: 1,
					Output:   &conversationv1.AgentHookOutput{Stdout: "out", Stderr: "err"},
				},
			},
		}},
	})

	// Assert.
	card := h.hookCard()
	if card.GetHeadline().GetText() != "hook failed: lint (PostToolUse)" {
		t.Fatalf("headline = %q", card.GetHeadline().GetText())
	}
	if card.GetFailed().GetExitCode() != 1 {
		t.Fatalf("exit = %d, want 1", card.GetFailed().GetExitCode())
	}
	if card.GetFailed().GetOutput().GetText() != "out\nerr" {
		t.Fatalf("output = %q", card.GetFailed().GetOutput().GetText())
	}
}

func TestAFailingHookThatPrintedNothingHasNoOutputElement(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.hookStart("unit-1", "lint", conversationv1.AgentHookEvent_AGENT_HOOK_EVENT_POST_TOOL_USE, "")

	// Act.
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{
			Result: &conversationv1.AgentHook_NonBlockingError{
				NonBlockingError: &conversationv1.AgentHookNonBlockingError{Command: "./lint.sh", ExitCode: 1},
			},
		}},
	})

	// Assert.
	if h.hookCard().GetFailed().GetOutput() != nil {
		t.Fatalf("output = %+v, want unset", h.hookCard().GetFailed().GetOutput())
	}
}

func TestABlockedHookLinksTheCallItGated(t *testing.T) {
	// Arrange: the gated call has a card of its own.
	h := newHarness(t)
	h.send(activityOf("unit-call", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
			Command:   &conversationv1.AgentBashCommand{Line: "git push origin master"},
			StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
		}},
	}))
	h.hookStart("unit-1", "protect-master",
		conversationv1.AgentHookEvent_AGENT_HOOK_EVENT_PRE_TOOL_USE, "unit-call")

	// Act.
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{
			Result: &conversationv1.AgentHook_BlockingError{
				BlockingError: &conversationv1.AgentHookBlockingError{
					Command: "./protect.sh", BlockingText: "no",
				},
			},
		}},
	})

	// Assert: the "▸ gated:" link names the call's row exactly as served.
	want := h.rows(rootFeed())[0].GetId().GetValue()
	if got := h.hookCard().GetGatedCall().GetRow().GetValue(); got != want {
		t.Fatalf("gated_call = %q, want the call's row %q", got, want)
	}
}

func TestACancelledHookDrawsNothing(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.hookStart("unit-1", "lint", conversationv1.AgentHookEvent_AGENT_HOOK_EVENT_STOP, "")
	h.send(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "unit-1"},
		Item: &conversationv1.AgentActivity_Hook{Hook: &conversationv1.AgentHook{
			Result: &conversationv1.AgentHook_Cancelled{Cancelled: &conversationv1.AgentHookCancelled{}},
		}},
	})

	// Assert.
	if rows := h.rows(rootFeed()); len(rows) != 0 {
		t.Fatalf("rows = %d, want none for a cancelled hook", len(rows))
	}
}
