package convert

import (
	"encoding/json"
	"os"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// subjectArmName names the arm NOTICE set, as the shared fixture spells it.
func subjectArmName(notice *conversationv1.AgentResponseSynthesizedNotice) noticeSubject {
	switch {
	case notice.GetAccount() != nil:
		return subjectAccount
	case notice.GetUsageLimit() != nil:
		return subjectUsageLimit
	case notice.GetUsageWarning() != nil:
		return subjectUsageWarning
	case notice.GetUsageTransition() != nil:
		return subjectUsageTransition
	case notice.GetVendorTransient() != nil:
		return subjectVendorTransient
	case notice.GetUnclassified() != nil:
		return subjectUnclassified
	}
	return ""
}

// sharedNoticeCase is one case of the fixture the stream and the transcript
// both read (testdata/vendor-notices/subjects.json).
type sharedNoticeCase struct {
	Name    string  `json:"name"`
	Class   *string `json:"class"`
	Text    string  `json:"text"`
	Subject string  `json:"subject"`
}

const sharedNoticeFixture = "../../../../../testdata/vendor-notices/subjects.json"

func TestNoticeSubjectAgreesWithTheStream(t *testing.T) {
	// Arrange. The shim reads the same fixture (test/convert/notice-subject.test.ts).
	raw, err := os.ReadFile(sharedNoticeFixture)
	if err != nil {
		t.Fatalf("read the shared fixture: %v", err)
	}
	var fixture struct {
		Cases []sharedNoticeCase `json:"cases"`
	}
	if err := json.Unmarshal(raw, &fixture); err != nil {
		t.Fatalf("decode the shared fixture: %v", err)
	}
	if len(fixture.Cases) == 0 {
		t.Fatal("the shared fixture holds no case")
	}
	for _, tc := range fixture.Cases {
		t.Run(tc.Name, func(t *testing.T) {
			class := ""
			if tc.Class != nil {
				class = *tc.Class
			}

			// Act.
			got := noticeSubjectOf(class, tc.Text)

			// Assert.
			if string(got) != tc.Subject {
				t.Fatalf("noticeSubjectOf(%q, %q) = %q, want %q", class, tc.Text, got, tc.Subject)
			}
		})
	}
}

func TestNoticeSubjectClassDecides(t *testing.T) {
	for class, want := range noticeSubjectByClass {
		t.Run(class, func(t *testing.T) {
			// Act. Words that would read as another subject.
			got := noticeSubjectOf(class, "API Error: 529 overloaded")

			// Assert.
			if got != want {
				t.Fatalf("class %q = %q, want %q", class, got, want)
			}
		})
	}
}

func TestNoticeSubjectFromWords(t *testing.T) {
	tests := []struct {
		name string
		text string
		want noticeSubject
	}{
		{name: "an account", text: "Failed to authenticate", want: subjectAccount},
		{name: "an account before an allowance", text: "OAuth: usage limit", want: subjectAccount},
		{name: "an exhausted allowance", text: "You've hit your monthly spend limit", want: subjectUsageLimit},
		{name: "an approaching allowance", text: "API Error: approaching your limit", want: subjectUsageWarning},
		{name: "a changed window", text: "Your allowance resets at 5pm", want: subjectUsageTransition},
		{name: "a transient status", text: "API Error: 503 service unavailable", want: subjectVendorTransient},
		{name: "a network code alone", text: "EAI_AGAIN", want: subjectVendorTransient},
		{name: "nothing recognizable", text: "Claude's response was interrupted", want: subjectUnclassified},
		{name: "an unknown class reads the words", text: "Login expired", want: subjectAccount},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := noticeSubjectOf("unknown", tc.text)

			// Assert.
			if got != tc.want {
				t.Fatalf("words %q = %q, want %q", tc.text, got, tc.want)
			}
		})
	}
}

func TestNoticeOfSubjectSetsItsArm(t *testing.T) {
	for _, subject := range []noticeSubject{subjectAccount, subjectUsageLimit, subjectUsageWarning, subjectUsageTransition, subjectVendorTransient, subjectUnclassified} {
		t.Run(string(subject), func(t *testing.T) {
			// Act.
			notice := noticeOfSubject(subject)

			// Assert.
			if got := subjectArmName(notice); got != subject {
				t.Fatalf("noticeOfSubject(%q) set the %q arm", subject, got)
			}
			if notice.HttpStatus != nil {
				t.Fatal("a notice carries no HTTP status")
			}
		})
	}
}

func TestNoticeOfSubjectRefusesAnUnknownSubject(t *testing.T) {
	// Assert.
	defer func() {
		if recover() == nil {
			t.Fatal("a subject outside the table must panic")
		}
	}()

	// Act.
	noticeOfSubject("not_a_subject")
}
