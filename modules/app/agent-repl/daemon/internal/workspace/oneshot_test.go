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

func TestDecorateOneShotOpenPrChainsTheFollowup(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	oneShotBriefs(f)
	v := f.verbs.(*verbs)

	// Act.
	got, err := v.decorateOneShot("task", &OneShotFinish{OpenPr: &OneShotOpenPr{}})

	// Assert: both gates are present — the implementation gate and the CICD one.
	if err != nil {
		t.Fatalf("decorateOneShot: %v", err)
	}
	if !strings.Contains(got, openPrActionPhrase) || !strings.Contains(got, WorkspaceSkill+" close") {
		t.Fatalf("decorated prompt = %q, want both one-shot gates", got)
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
