package logsite_test

import (
	"testing"

	"agentrepl/logging/logsite"
)

// The message is made outside package logsite, whose frames Msg skips.
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
