package runtimemetrics

import (
	"errors"
	"math"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

func named(symbol string) *agentreplv1.EditorStallFunction {
	return &agentreplv1.EditorStallFunction{Function: &agentreplv1.EditorStallFunction_Named{Named: &agentreplv1.EditorStallFunctionNamed{Symbol: symbol}}}
}

func anonymous() *agentreplv1.EditorStallFunction {
	return &agentreplv1.EditorStallFunction{Function: &agentreplv1.EditorStallFunction_Anonymous{Anonymous: &agentreplv1.EditorStallFunctionAnonymous{}}}
}

func stallOf(c *agentreplv1.EditorStallCulprit) *agentreplv1.ReportedEditorStall {
	return &agentreplv1.ReportedEditorStall{Seconds: 0.2, Culprit: c}
}

func TestValidateEditorStall(t *testing.T) {
	timer := func(f *agentreplv1.EditorStallFunction) *agentreplv1.EditorStallCulprit {
		return &agentreplv1.EditorStallCulprit{Work: &agentreplv1.EditorStallCulprit_Timer{Timer: &agentreplv1.EditorStallTimer{Function: f}}}
	}
	tests := []struct {
		name  string
		stall *agentreplv1.ReportedEditorStall
		want  StallCulprit
	}{
		{name: "an agent-repl timer is labeled by its name", stall: stallOf(timer(named("agent-repl-roster--reconcile"))), want: StallCulprit{Source: "timer", Function: "agent-repl-roster--reconcile"}},
		{name: "a foreign timer is labeled other", stall: stallOf(timer(named("doom-modeline-update"))), want: StallCulprit{Source: "timer", Function: "other"}},
		{name: "a closure timer is labeled anonymous", stall: stallOf(timer(anonymous())), want: StallCulprit{Source: "timer", Function: "anonymous"}},
		{name: "a command", stall: stallOf(&agentreplv1.EditorStallCulprit{Work: &agentreplv1.EditorStallCulprit_Command{Command: &agentreplv1.EditorStallCommand{Function: named("agent-repl-switch")}}}), want: StallCulprit{Source: "command", Function: "agent-repl-switch"}},
		{name: "a filter is labeled by its rpc method", stall: stallOf(&agentreplv1.EditorStallCulprit{Work: &agentreplv1.EditorStallCulprit_ProcessFilter{ProcessFilter: &agentreplv1.EditorStallProcessFilter{RpcMethod: "WatchHostWorkspace"}}}), want: StallCulprit{Source: "process_filter", Function: "WatchHostWorkspace"}},
		{name: "a sentinel is labeled by its rpc method", stall: stallOf(&agentreplv1.EditorStallCulprit{Work: &agentreplv1.EditorStallCulprit_ProcessSentinel{ProcessSentinel: &agentreplv1.EditorStallProcessSentinel{RpcMethod: "ReportLatencies"}}}), want: StallCulprit{Source: "process_sentinel", Function: "ReportLatencies"}},
		{name: "garbage collection", stall: stallOf(&agentreplv1.EditorStallCulprit{Work: &agentreplv1.EditorStallCulprit_GarbageCollection{GarbageCollection: &agentreplv1.EditorStallGarbageCollection{}}}), want: StallCulprit{Source: "garbage_collection"}},
		{name: "unattributed", stall: stallOf(&agentreplv1.EditorStallCulprit{Work: &agentreplv1.EditorStallCulprit_Unattributed{Unattributed: &agentreplv1.EditorStallUnattributed{}}}), want: StallCulprit{Source: "unattributed"}},
		{name: "a set but unassigned culprit is unknown", stall: stallOf(&agentreplv1.EditorStallCulprit{}), want: StallCulprit{Source: "unknown"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			kind, culprit, err := ValidateEditorStall(tt.stall)

			// Assert
			if err != nil || kind.Name != "editor_stall" || culprit != tt.want {
				t.Fatalf("ValidateEditorStall = (%v, %+v, %v), want (editor_stall, %+v)", kind.Name, culprit, err, tt.want)
			}
		})
	}
}

func TestValidateEditorStallRefusesAMalformedStall(t *testing.T) {
	timer := func(f *agentreplv1.EditorStallFunction) *agentreplv1.EditorStallCulprit {
		return &agentreplv1.EditorStallCulprit{Work: &agentreplv1.EditorStallCulprit_Timer{Timer: &agentreplv1.EditorStallTimer{Function: f}}}
	}
	tests := []struct {
		name  string
		stall *agentreplv1.ReportedEditorStall
	}{
		{name: "no culprit", stall: &agentreplv1.ReportedEditorStall{Seconds: 0.2}},
		{name: "a negative duration", stall: &agentreplv1.ReportedEditorStall{Seconds: -1, Culprit: timer(anonymous())}},
		{name: "a NaN duration", stall: &agentreplv1.ReportedEditorStall{Seconds: math.NaN(), Culprit: timer(anonymous())}},
		{name: "an empty symbol", stall: stallOf(timer(named("")))},
		{name: "a function neither named nor anonymous", stall: stallOf(timer(&agentreplv1.EditorStallFunction{}))},
		{name: "a filter of no AgentRepl method", stall: stallOf(&agentreplv1.EditorStallCulprit{Work: &agentreplv1.EditorStallCulprit_ProcessFilter{ProcessFilter: &agentreplv1.EditorStallProcessFilter{RpcMethod: "NoSuchMethod"}}})},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, _, err := ValidateEditorStall(tt.stall)

			// Assert
			if !errors.Is(err, ErrInvalidLatency) {
				t.Fatalf("err = %v, want ErrInvalidLatency", err)
			}
		})
	}
}

func TestAnEditorStallIsServedAsALatencyAndByCulprit(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	kind := EditorKinds[agentreplv1.EditorLatencyKind_EDITOR_LATENCY_KIND_STALL]
	culprit := StallCulprit{Source: StallSourceProcessFilter, Function: "WatchHostWorkspace"}

	// Act
	s.ObserveEditorStall(kind, culprit, 0.15)
	s.ObserveEditorStall(kind, culprit, 0.25)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_latency_seconds_count{latency="editor_stall",workspace=""} 2`,
		`agentrepl_editor_stall_culprit_seconds_bucket{function="WatchHostWorkspace",source="process_filter",le="0.2"} 1`,
		`agentrepl_editor_stall_culprit_seconds_count{function="WatchHostWorkspace",source="process_filter"} 2`,
		`agentrepl_editor_stall_culprit_seconds_sum{function="WatchHostWorkspace",source="process_filter"} 0.4`,
	)
}

func TestValidSeconds(t *testing.T) {
	tests := []struct {
		name    string
		seconds float64
		want    bool
	}{
		{name: "zero", seconds: 0, want: true},
		{name: "a positive duration", seconds: 0.2, want: true},
		{name: "a negative duration", seconds: -0.001},
		{name: "NaN", seconds: math.NaN()},
		{name: "positive infinity", seconds: math.Inf(1)},
		{name: "negative infinity", seconds: math.Inf(-1)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act / Assert
			if got := validSeconds(tt.seconds); got != tt.want {
				t.Fatalf("validSeconds(%v) = %v, want %v", tt.seconds, got, tt.want)
			}
		})
	}
}
