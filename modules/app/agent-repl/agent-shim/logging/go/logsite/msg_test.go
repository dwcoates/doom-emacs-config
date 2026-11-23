package logsite_test

import (
	"strings"
	"testing"

	"agentrepl/logging/logsite"
)

// The message is keyed by the function that called Msg: this test.
func TestMsgRecordsItsTemplateAndTheFunctionThatMadeIt(t *testing.T) {
	// Act
	m := logsite.Msg("shim {shim} stalled")

	// Assert
	if m.Template() != "shim {shim} stalled" {
		t.Errorf("Template = %q", m.Template())
	}
	if want := "agentrepl/logging/logsite_test.TestMsgRecordsItsTemplateAndTheFunctionThatMadeIt"; m.Function() != want {
		t.Errorf("Function = %q, want %q", m.Function(), want)
	}
	if m.IsZero() {
		t.Errorf("a made message reads as zero")
	}
	if !(logsite.Message{}).IsZero() {
		t.Errorf("the zero message does not read as zero")
	}
}

func TestForwardedAnswersTheFunctionThatMadeTheMessage(t *testing.T) {
	// Arrange
	m := logsite.Msg("forwarded")

	// Act
	got := m.Forwarded()

	// Assert
	if got != m.Function() {
		t.Fatalf("Forwarded = %q, want the function %q", got, m.Function())
	}
}

func TestForwardedRefusesAMessageNoMsgMade(t *testing.T) {
	// Arrange
	defer func() {
		// Assert
		text, _ := recover().(string)
		if !strings.Contains(text, "logsite.Msg") {
			t.Fatalf("panic = %q, want one naming logsite.Msg", text)
		}
	}()

	// Act
	logsite.Message{}.Forwarded()
}
