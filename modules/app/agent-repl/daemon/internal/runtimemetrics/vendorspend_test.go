package runtimemetrics

import (
	"errors"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/vendorspend"
	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"
)

// spendSurface is an open surface whose account labeler names /work the
// multi-repo root.
func spendSurface(t *testing.T) (*Surface, *dlog.TestLogger) {
	t.Helper()
	s, log := openSurface(t)
	s.SetAccountLabeler(func(configDir string) bool { return configDir == "/work" })
	return s, log
}

func bound(v uint64) *uint64 { return &v }

// warmEntry is CALLS warm main-loop user-prompt calls in the 25k bucket,
// spending READ cache reads and OUTPUT output tokens.
func warmEntry(calls, read, output uint64) *conversationv1.VendorSpend {
	return &conversationv1.VendorSpend{
		Model:           "claude-opus-5-5",
		Agent:           conversationv1.VendorCallAgent_VENDOR_CALL_AGENT_MAIN,
		Cause:           conversationv1.VendorCallCause_VENDOR_CALL_CAUSE_USER_PROMPT,
		CacheOutcome:    conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_WARM,
		ContextLeTokens: bound(25_000),
		Calls:           calls,
		Tokens:          &conversationv1.VendorTokens{CacheRead: read, Output: output},
	}
}

func spendFrame(since int64, configDir string, entries ...*conversationv1.VendorSpend) *shimv1.ShimMeasurements {
	return &shimv1.ShimMeasurements{CountedSinceUnixMs: since, ConfigDir: configDir, VendorSpend: entries}
}

const warmLabels = `account="default",agent="main",cache_outcome="warm",cause="user_prompt",cold_reason="",context="25000",model="claude-opus-5-5",subagent_type="",workspace="ws1"`

func TestShimSpendFolds(t *testing.T) {
	tests := []struct {
		name   string
		frames []*shimv1.ShimMeasurements
		want   []string
	}{
		{
			name:   "a first frame is added whole",
			frames: []*shimv1.ShimMeasurements{spendFrame(100, "/home", warmEntry(2, 1000, 50))},
			want: []string{
				VendorCallsName + `{` + warmLabels + `} 2`,
				VendorTokensName + `{account="default",agent="main",cache_outcome="warm",cause="user_prompt",cold_reason="",kind="cache_read",model="claude-opus-5-5",subagent_type="",workspace="ws1"} 1000`,
			},
		},
		{
			name:   "a later frame of the same run adds its growth",
			frames: []*shimv1.ShimMeasurements{spendFrame(100, "/home", warmEntry(2, 1000, 50)), spendFrame(100, "/home", warmEntry(3, 1500, 70))},
			want:   []string{VendorCallsName + `{` + warmLabels + `} 3`},
		},
		{
			name:   "a new run's frame is added whole on top",
			frames: []*shimv1.ShimMeasurements{spendFrame(100, "/home", warmEntry(2, 1000, 50)), spendFrame(200, "/home", warmEntry(1, 10, 1))},
			want:   []string{VendorCallsName + `{` + warmLabels + `} 3`},
		},
		{
			name:   "the multi-repo root's spend is labeled its account",
			frames: []*shimv1.ShimMeasurements{spendFrame(100, "/work", warmEntry(1, 10, 1))},
			want:   []string{`account="multi_repo"`},
		},
		{
			name:   "the spend is filed by its context bucket",
			frames: []*shimv1.ShimMeasurements{spendFrame(100, "/home", warmEntry(1, 1000, 50))},
			want: []string{
				VendorContextTokensName + `{account="default",context="25000",kind="cache_read",model="claude-opus-5-5"} 1000`,
				VendorCallContextName + `_bucket{account="default",agent="main",model="claude-opus-5-5",le="25000"} 1`,
				VendorCallContextName + `_sum{account="default",agent="main",model="claude-opus-5-5"} 1000`,
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := spendSurface(t)
			// Act
			for _, f := range tt.frames {
				s.FoldShimMeasurements("ws1", f)
			}
			// Assert
			mustContain(t, scrape(t, s), tt.want...)
		})
	}
}

func TestAContextAboveEveryBoundIsCountedOnlyInTheHistogramsTotal(t *testing.T) {
	// Arrange
	s, _ := spendSurface(t)
	e := warmEntry(1, 1_500_000, 1)
	e.ContextLeTokens = nil
	// Act
	s.FoldShimMeasurements("ws1", spendFrame(100, "/home", e))
	// Assert
	mustContain(t, scrape(t, s),
		VendorCallContextName+`_bucket{account="default",agent="main",model="claude-opus-5-5",le="1e+06"} 0`,
		VendorCallContextName+`_count{account="default",agent="main",model="claude-opus-5-5"} 1`,
		`context="+Inf"`)
}

func TestAnInvalidSpendFrameIsRefusedOncePerCause(t *testing.T) {
	cold := warmEntry(1, 10, 1)
	cold.CacheOutcome = conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_COLD_REWRITE
	tests := []struct {
		name  string
		frame *shimv1.ShimMeasurements
		cause string
	}{
		{name: "an invalid entry", frame: spendFrame(100, "/home", cold), cause: "has cold reason"},
		{name: "a tag set sent twice", frame: spendFrame(100, "/home", warmEntry(1, 1, 1), warmEntry(2, 2, 2)), cause: "appears twice"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := spendSurface(t)
			// Act
			s.FoldShimMeasurements("ws1", tt.frame)
			s.FoldShimMeasurements("ws1", tt.frame)
			// Assert
			if strings.Contains(scrape(t, s), VendorCallsName+"{") {
				t.Fatalf("a refused frame was folded")
			}
			errs := errorRecords(log)
			if len(errs) != 1 || errs[0].Context["workspace_id"] != "ws1" || !strings.Contains(errs[0].Context["error"].(string), tt.cause) {
				t.Fatalf("error records = %+v, want one naming %q", errs, tt.cause)
			}
		})
	}
}

func errorRecords(log *dlog.TestLogger) []dlog.Record {
	var errs []dlog.Record
	for _, r := range log.Records() {
		if r.Level == "error" {
			errs = append(errs, r)
		}
	}
	return errs
}

func TestSpendGoingBackwardsWithinARunIsRefused(t *testing.T) {
	// Arrange
	s, log := spendSurface(t)
	s.FoldShimMeasurements("ws1", spendFrame(100, "/home", warmEntry(3, 1000, 50)))
	// Act
	s.FoldShimMeasurements("ws1", spendFrame(100, "/home", warmEntry(2, 1000, 50)))
	// Assert
	mustContain(t, scrape(t, s), VendorCallsName+`{`+warmLabels+`} 3`)
	errs := errorRecords(log)
	if len(errs) != 1 || !strings.Contains(errs[0].Context["error"].(string), "went from 3 to 2") {
		t.Fatalf("error records = %+v, want one naming the regression", errs)
	}
}

func TestSpendFoldedBeforeTheLabelerIsSetIsLabeledUnstatedAndRecorded(t *testing.T) {
	// Arrange
	s, log := openSurface(t)
	// Act
	s.FoldShimMeasurements("ws1", spendFrame(100, "/home", warmEntry(1, 1, 1)))
	// Assert
	mustContain(t, scrape(t, s), `account="unstated"`)
	errs := errorRecords(log)
	if len(errs) != 1 || errs[0].Context["invariant_violation"] == nil {
		t.Fatalf("error records = %+v, want one invariant violation", errs)
	}
}

func TestAShimNamingNoAccountRootIsLabeledUnstated(t *testing.T) {
	// Arrange
	s, _ := spendSurface(t)
	// Act
	s.FoldShimMeasurements("ws1", spendFrame(100, "", warmEntry(1, 1, 1)))
	// Assert
	mustContain(t, scrape(t, s), `account="unstated"`)
}

func TestObserveVendorSpend(t *testing.T) {
	tests := []struct {
		name    string
		entries []*conversationv1.VendorSpend
		invalid bool
	}{
		{name: "a whole report is added", entries: []*conversationv1.VendorSpend{warmEntry(1, 10, 1)}},
		{name: "an empty report is refused", entries: nil, invalid: true},
		{name: "a report with an invalid entry is refused whole", entries: []*conversationv1.VendorSpend{warmEntry(1, 10, 1), {}}, invalid: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := spendSurface(t)
			// Act
			err := s.ObserveVendorSpend("ws1", "/home", tt.entries)
			// Assert
			if tt.invalid != errors.Is(err, vendorspend.ErrInvalid) {
				t.Fatalf("ObserveVendorSpend = %v, want invalid %v", err, tt.invalid)
			}
			folded := strings.Contains(scrape(t, s), VendorCallsName+"{")
			if folded == tt.invalid {
				t.Fatalf("folded = %v for a report refused %v", folded, tt.invalid)
			}
		})
	}
}

func TestKeepaliveSpentCountsOnlyTheKeepAliveCause(t *testing.T) {
	// Arrange
	s, _ := spendSurface(t)
	beat := warmEntry(1, 900, 3)
	beat.Cause = conversationv1.VendorCallCause_VENDOR_CALL_CAUSE_KEEPALIVE
	// Act
	s.FoldShimMeasurements("ws1", spendFrame(100, "/home", beat, warmEntry(1, 5000, 50)))
	// Assert
	got := s.KeepaliveSpent("ws1")
	if got.GetCacheRead() != 900 || got.GetOutput() != 3 {
		t.Fatalf("KeepaliveSpent = %v, want 900 read and 3 output", got)
	}
	if none := s.KeepaliveSpent("ws2"); none.GetCacheRead() != 0 {
		t.Fatalf("an unseen workspace's keep-alive spend = %v, want none", none)
	}
}

func TestAForgottenWorkspacesSpendIsDropped(t *testing.T) {
	// Arrange
	s, _ := spendSurface(t)
	s.FoldShimMeasurements("ws1", spendFrame(100, "/home", warmEntry(1, 1, 1)))
	// Act
	s.ForgetWorkspace("ws1")
	// Assert
	if strings.Contains(scrape(t, s), `workspace="ws1"`) {
		t.Fatal("a forgotten workspace's spend is still served")
	}
}

func TestTheOffSurfaceRecordsNoSpend(t *testing.T) {
	// Arrange
	var s *Surface
	// Act
	s.SetAccountLabeler(func(string) bool { return false })
	err := s.ObserveVendorSpend("ws1", "/home", nil)
	// Assert
	if err != nil || s.KeepaliveSpent("ws1").GetCacheRead() != 0 {
		t.Fatalf("the off surface answered %v", err)
	}
}
