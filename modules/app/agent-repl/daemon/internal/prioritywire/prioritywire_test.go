package prioritywire

import (
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/wsm"
)

var levels = []wsm.Priority{wsm.PriorityP05, wsm.PriorityP1, wsm.PriorityP2, wsm.PriorityP3}

func TestEveryLevelRoundTripsThroughBothSpellings(t *testing.T) {
	for _, p := range levels {
		t.Run(Spelled(p), func(t *testing.T) {
			// Act
			fromProto, okProto := Of(Proto(p))
			fromText, okText := Parse(Spelled(p))

			// Assert
			if !okProto || !okText || fromProto != p || fromText != p {
				t.Fatalf("%d round-trips to %d (%v) and %d (%v)", int(p), int(fromProto), okProto, int(fromText), okText)
			}
		})
	}
}

func TestTheSpellingsAreTheCommandLines(t *testing.T) {
	got := []string{}
	for _, p := range levels {
		got = append(got, Spelled(p))
	}
	if strings.Join(got, ",") != "p05,p1,p2,p3" {
		t.Fatalf("spellings = %v", got)
	}
}

func TestAnUnsetLevelReadsAsNone(t *testing.T) {
	for _, p := range []*agentreplv1.WorkspacePriority{nil, {}} {
		if _, ok := Of(p); ok {
			t.Fatalf("Of(%v) read a level", p)
		}
	}
}

func TestAnUnknownSpellingParsesAsNone(t *testing.T) {
	for _, text := range []string{"", "p4", "P1", "none"} {
		if _, ok := Parse(text); ok {
			t.Fatalf("Parse(%q) read a level", text)
		}
	}
}

func TestAnUndeclaredLevelFailsHard(t *testing.T) {
	for name, f := range map[string]func(){"Proto": func() { Proto(wsm.Priority(9)) }, "Spelled": func() { Spelled(wsm.Priority(9)) }} {
		t.Run(name, func(t *testing.T) {
			defer func() {
				if recover() == nil {
					t.Fatalf("%s of an undeclared level did not panic", name)
				}
			}()
			f()
		})
	}
}
