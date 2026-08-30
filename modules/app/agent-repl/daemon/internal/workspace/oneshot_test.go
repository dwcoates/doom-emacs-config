package workspace

import (
	"strings"
	"testing"

	"claude-repld/internal/prompts"
)

func TestDecorateOneShotBracketsWhatTheUserDidNotType(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	oneShotBriefs(f)
	v := f.verbs.(*verbs)

	// Act.
	got, err := v.decorateOneShot("USER TEXT", &OneShotFinish{SelfMerge: true})

	// Assert: the user's own words are the only unbracketed span.
	if err != nil {
		t.Fatalf("decorateOneShot: %v", err)
	}
	if !strings.Contains(got, MetaOpen+"PREAMBLE\n"+MetaClose+"USER TEXT"+MetaOpen) {
		t.Fatalf("decorated prompt = %q, want the injected spans meta-wrapped around the user's words", got)
	}
}

func TestDecorateOneShotSelfMergeNamesTheWorkspaceSkill(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	oneShotBriefs(f)
	v := f.verbs.(*verbs)

	// Act.
	got, err := v.decorateOneShot("task", &OneShotFinish{SelfMerge: true})

	// Assert.
	if err != nil {
		t.Fatalf("decorateOneShot: %v", err)
	}
	if !strings.Contains(got, "the "+WorkspaceSkill+" merge skill") {
		t.Fatalf("decorated prompt = %q, want the workspace merge skill named", got)
	}
}

func TestDecorateOneShotOpenPrCarriesOnlyTheFirstGate(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	oneShotBriefs(f)
	v := f.verbs.(*verbs)

	// Act.
	got, err := v.decorateOneShot("task", &OneShotFinish{OpenPr: &OneShotOpenPr{}})

	// Assert: the CICD-gated wrap-up is a post-prompt, so the agent is never
	// told how to finish before it has started.
	if err != nil {
		t.Fatalf("decorateOneShot: %v", err)
	}
	if !strings.Contains(got, openPrActionPhrase) {
		t.Fatalf("decorated prompt = %q, want the pr gate", got)
	}
	if strings.Contains(got, WorkspaceSkill+" close") {
		t.Fatalf("decorated prompt = %q, want the CICD gate held back", got)
	}
}

func TestCreatePrCommandAppendsOnlyTheRequestedFlags(t *testing.T) {
	tests := []struct {
		name string
		pr   *OneShotOpenPr
		want string
	}{
		{name: "no flags", pr: &OneShotOpenPr{}, want: CreatePrSkill + " --patch --rebase"},
		{
			name: "merge queue only",
			pr:   &OneShotOpenPr{AddToMergeQueue: true},
			want: CreatePrSkill + " --patch --rebase --add-to-merge-queue",
		},
		{
			name: "self certified only",
			pr:   &OneShotOpenPr{SelfCertified: true},
			want: CreatePrSkill + " --patch --rebase --self-certified",
		},
		{
			name: "both",
			pr:   &OneShotOpenPr{SelfCertified: true, AddToMergeQueue: true},
			want: CreatePrSkill + " --patch --rebase --add-to-merge-queue --self-certified",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := createPrCommand(tt.pr)
			// Assert.
			if got != tt.want {
				t.Fatalf("createPrCommand() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestDecorateOneShotRefusesWithNoFinish(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	oneShotBriefs(f)
	v := f.verbs.(*verbs)

	// Act.
	_, err := v.decorateOneShot("task", nil)

	// Assert.
	if err == nil {
		t.Fatal("decorateOneShot(nil finish) = nil error, want a refusal")
	}
}

func TestDecorateOneShotRefusesAFinishNamingNeitherArm(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	oneShotBriefs(f)
	v := f.verbs.(*verbs)

	// Act.
	_, err := v.decorateOneShot("task", &OneShotFinish{})

	// Assert.
	if err == nil {
		t.Fatal("decorateOneShot(empty finish) = nil error, want a refusal")
	}
}

func TestDecorateOneShotRefusesAMisspelledPlaceholder(t *testing.T) {
	// Arrange: an edited brief whose placeholder no longer matches the call
	// site fails the operation rather than shipping a hole.
	f := newFixture(t)
	oneShotBriefs(f)
	f.briefs[BriefOneShotSuccessSuffix] = prompts.Prompt{
		Body: "invoke {{invocaton}}", Placeholders: []string{"invocaton"},
	}
	v := f.verbs.(*verbs)

	// Act.
	_, err := v.decorateOneShot("task", &OneShotFinish{SelfMerge: true})

	// Assert.
	if err == nil {
		t.Fatal("decorateOneShot(misspelled placeholder) = nil error, want a refusal")
	}
}

func TestFinishOriginNamesTheRecordedFinish(t *testing.T) {
	tests := []struct {
		name   string
		finish *OneShotFinish
		want   string
	}{
		{name: "none", finish: nil, want: ""},
		{name: "self merge", finish: &OneShotFinish{SelfMerge: true}, want: "self_merge"},
		{name: "plain open pr", finish: &OneShotFinish{OpenPr: &OneShotOpenPr{}}, want: "open_pr"},
		{
			name:   "open pr with both flags",
			finish: &OneShotFinish{OpenPr: &OneShotOpenPr{SelfCertified: true, AddToMergeQueue: true}},
			want:   "open_pr+self_certified+add_to_merge_queue",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := finishOrigin(tt.finish)
			// Assert.
			if got != tt.want {
				t.Fatalf("finishOrigin() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestParseFinishOriginIsFinishOriginsInverse(t *testing.T) {
	tests := []*OneShotFinish{
		{SelfMerge: true},
		{OpenPr: &OneShotOpenPr{}},
		{OpenPr: &OneShotOpenPr{SelfCertified: true}},
		{OpenPr: &OneShotOpenPr{AddToMergeQueue: true}},
		{OpenPr: &OneShotOpenPr{SelfCertified: true, AddToMergeQueue: true}},
	}
	for _, finish := range tests {
		t.Run(finishOrigin(finish), func(t *testing.T) {
			// Arrange in the table. Act.
			got, err := parseFinishOrigin(finishOrigin(finish))
			// Assert.
			if err != nil {
				t.Fatalf("parseFinishOrigin: %v", err)
			}
			if finishOrigin(got) != finishOrigin(finish) {
				t.Fatalf("round trip = %q, want %q", finishOrigin(got), finishOrigin(finish))
			}
		})
	}
}

func TestParseFinishOriginOfNothingIsNoAction(t *testing.T) {
	// Arrange. Act.
	got, err := parseFinishOrigin("")

	// Assert.
	if err != nil || got != nil {
		t.Fatalf("parseFinishOrigin(\"\") = (%v, %v), want no action", got, err)
	}
}

func TestParseFinishOriginRefusesAnUnknownRecord(t *testing.T) {
	tests := []string{"teleport", "self_merge+self_certified", "open_pr+rebase"}
	for _, recorded := range tests {
		t.Run(recorded, func(t *testing.T) {
			// Arrange in the table. Act.
			_, err := parseFinishOrigin(recorded)
			// Assert.
			if err == nil {
				t.Fatalf("parseFinishOrigin(%q) = nil error, want a refusal", recorded)
			}
		})
	}
}

func TestOpenPrFollowupNamesBothCommands(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	oneShotBriefs(f)
	v := f.verbs.(*verbs)

	// Act.
	got, err := v.openPrFollowup(&OneShotOpenPr{AddToMergeQueue: true})

	// Assert.
	if err != nil {
		t.Fatalf("openPrFollowup: %v", err)
	}
	if !strings.Contains(got, "--add-to-merge-queue") || !strings.Contains(got, WorkspaceSkill+" close") {
		t.Fatalf("follow-up = %q, want the pr command and the wrap-up", got)
	}
}
