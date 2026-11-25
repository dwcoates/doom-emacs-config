package runtimemetrics

import (
	"fmt"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"google.golang.org/protobuf/reflect/protoreflect"
)

// EditorStallCulpritName is the histogram of the editor's command-loop
// stalls by the work that held the loop longest across each: its count and
// sum are the stalls each culprit caused and how long they lasted.
const EditorStallCulpritName = "agentrepl_editor_stall_culprit_seconds"

// The `source' label's closed vocabulary: the arm of
// agentrepl.v1.EditorStallCulprit.work, and `unknown' for an arm this
// daemon's schema does not name.
const (
	StallSourceTimer             = "timer"
	StallSourceProcessFilter     = "process_filter"
	StallSourceProcessSentinel   = "process_sentinel"
	StallSourceCommand           = "command"
	StallSourceGarbageCollection = "garbage_collection"
	StallSourceUnattributed      = "unattributed"
	StallSourceUnknown           = "unknown"
)

// The `function' label of a function that is not agent-repl's own, and of
// one with no name.
const (
	StallFunctionOther     = "other"
	StallFunctionAnonymous = "anonymous"
)

// stallOwnPrefix begins the name of every function of agent-repl's own; the
// label names those, and folds every other into StallFunctionOther, so the
// label is a set this codebase's own definitions bound.
const stallOwnPrefix = "agent-repl"

// StallCulprit is one stall's culprit as the metric labels it.
type StallCulprit struct {
	// Source is the kind of work, from the closed vocabulary above.
	Source string
	// Function is the function or rpc method, or "" for a source that has
	// none (garbage collection, unattributed, unknown).
	Function string
}

// agentReplMethods are the AgentRepl service's methods: the closed set a
// process culprit's rpc_method is drawn from.
var agentReplMethods = agentreplv1.File_agentrepl_v1_service_proto.Services().ByName("AgentRepl").Methods()

// ValidateEditorStall checks one stall Emacs reported, answering the
// `editor_stall' latency it records and its culprit's labels. Every error
// wraps ErrInvalidLatency.
func ValidateEditorStall(s *agentreplv1.ReportedEditorStall) (LatencyKind, StallCulprit, error) {
	kind := EditorKinds[agentreplv1.EditorLatencyKind_EDITOR_LATENCY_KIND_STALL]
	if sec := s.GetSeconds(); !validSeconds(sec) {
		return LatencyKind{}, StallCulprit{}, fmt.Errorf("%w: %s took %v seconds", ErrInvalidLatency, kind.Name, sec)
	}
	c := s.GetCulprit()
	if c == nil {
		return LatencyKind{}, StallCulprit{}, fmt.Errorf("%w: a stall names no culprit", ErrInvalidLatency)
	}
	culprit, err := stallCulprit(c)
	if err != nil {
		return LatencyKind{}, StallCulprit{}, err
	}
	return kind, culprit, nil
}

// stallCulprit answers C's labels.
func stallCulprit(c *agentreplv1.EditorStallCulprit) (StallCulprit, error) {
	switch w := c.GetWork().(type) {
	case *agentreplv1.EditorStallCulprit_Timer:
		fn, err := stallFunction(w.Timer.GetFunction())
		return StallCulprit{Source: StallSourceTimer, Function: fn}, err
	case *agentreplv1.EditorStallCulprit_Command:
		fn, err := stallFunction(w.Command.GetFunction())
		return StallCulprit{Source: StallSourceCommand, Function: fn}, err
	case *agentreplv1.EditorStallCulprit_ProcessFilter:
		m, err := stallMethod(w.ProcessFilter.GetRpcMethod())
		return StallCulprit{Source: StallSourceProcessFilter, Function: m}, err
	case *agentreplv1.EditorStallCulprit_ProcessSentinel:
		m, err := stallMethod(w.ProcessSentinel.GetRpcMethod())
		return StallCulprit{Source: StallSourceProcessSentinel, Function: m}, err
	case *agentreplv1.EditorStallCulprit_GarbageCollection:
		return StallCulprit{Source: StallSourceGarbageCollection}, nil
	case *agentreplv1.EditorStallCulprit_Unattributed:
		return StallCulprit{Source: StallSourceUnattributed}, nil
	default:
		// Set but unassigned: a kind of work this schema revision does not name.
		return StallCulprit{Source: StallSourceUnknown}, nil
	}
}

// stallFunction answers the label of a timer's or a command's function.
func stallFunction(f *agentreplv1.EditorStallFunction) (string, error) {
	switch fn := f.GetFunction().(type) {
	case *agentreplv1.EditorStallFunction_Named:
		sym := fn.Named.GetSymbol()
		if sym == "" {
			return "", fmt.Errorf("%w: a stall's function is named by an empty symbol", ErrInvalidLatency)
		}
		if strings.HasPrefix(sym, stallOwnPrefix) {
			return sym, nil
		}
		return StallFunctionOther, nil
	case *agentreplv1.EditorStallFunction_Anonymous:
		return StallFunctionAnonymous, nil
	default:
		return "", fmt.Errorf("%w: a stall's function says neither its name nor that it has none", ErrInvalidLatency)
	}
}

// stallMethod answers M when it names a method of service AgentRepl.
func stallMethod(m string) (string, error) {
	if agentReplMethods.ByName(protoreflect.Name(m)) == nil {
		return "", fmt.Errorf("%w: %q is not a method of service AgentRepl", ErrInvalidLatency, m)
	}
	return m, nil
}

// ObserveEditorStall records one stall, already validated: as the
// `editor_stall' latency under the empty workspace, and by its culprit. The
// off surface records nothing.
func (s *Surface) ObserveEditorStall(kind LatencyKind, culprit StallCulprit, seconds float64) {
	if s == nil {
		return
	}
	s.latencies.observe("", kind, seconds)
	s.latencies.observeCulprit(culprit, kind.Buckets, seconds)
}
