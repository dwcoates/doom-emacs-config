package headless

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// scriptedRunner answers each Run with the next of its answers.
type scriptedRunner struct {
	answers []scriptedAnswer
	calls   int
}

type scriptedAnswer struct {
	text string
	err  error
}

func (s *scriptedRunner) Bin() string { return "scriptedRunner" }

func (s *scriptedRunner) Run(context.Context, Request) (Response, error) {
	a := s.answers[s.calls]
	s.calls++
	if a.err != nil {
		return Response{}, a.err
	}
	return Response{Text: a.text, Duration: time.Millisecond}, nil
}

func upper(answer string) (string, error) {
	if answer == "" || strings.ToUpper(answer) != answer {
		return "", errors.New("not upper case")
	}
	return answer, nil
}

func TestAskValidated(t *testing.T) {
	cases := []struct {
		name      string
		answers   []scriptedAnswer
		want      string
		wantCause string
		wantCalls int
		wantLast  string
	}{
		{name: "first answer validates", answers: []scriptedAnswer{{text: "OK"}}, want: "OK", wantCalls: 1},
		{name: "invalid then valid", answers: []scriptedAnswer{{text: "no"}, {text: "YES"}}, want: "YES", wantCalls: 2},
		{name: "failed run then valid", answers: []scriptedAnswer{{err: &Error{Cause: CauseTimeout}}, {text: "YES"}}, want: "YES", wantCalls: 2},
		{name: "every answer invalid", answers: []scriptedAnswer{{text: "a"}, {text: "b"}}, wantCause: CauseInvalidAnswer, wantCalls: 2, wantLast: "b"},
		{name: "every run fails", answers: []scriptedAnswer{{err: &Error{Cause: CauseTimeout}}, {err: &Error{Cause: CauseExitStatus}}}, wantCause: CauseExitStatus, wantCalls: 2},
		{name: "guard refusal is not retried", answers: []scriptedAnswer{{err: &Error{Cause: CauseGuardRefused}}, {text: "YES"}}, wantCause: CauseGuardRefused, wantCalls: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			r := &scriptedRunner{answers: tc.answers}
			log := dlog.NewTestLogger()

			// Act
			got, failure := AskValidated(context.Background(), r, Ask{Request: Request{Site: "s"}, Attempts: 2, Validate: upper}, log, "daemon.test.ask")

			// Assert
			if r.calls != tc.wantCalls {
				t.Fatalf("calls = %d, want %d", r.calls, tc.wantCalls)
			}
			if tc.wantCause == "" {
				if failure != nil || got != tc.want {
					t.Fatalf("AskValidated = %q, %v; want %q", got, failure, tc.want)
				}
				return
			}
			if failure == nil || failure.Cause != tc.wantCause || failure.Attempts != uint32(tc.wantCalls) || failure.Answer != tc.wantLast {
				t.Fatalf("failure = %+v, want cause %q after %d attempts, last answer %q", failure, tc.wantCause, tc.wantCalls, tc.wantLast)
			}
			if len(dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.test.ask"})) != tc.wantCalls {
				t.Fatalf("records = %+v, want one info per attempt", log.Records())
			}
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error"}); ok {
				t.Fatalf("AskValidated recorded an error; the caller owns it")
			}
		})
	}
}

func TestAskValidatedPanicsOnNoAttempts(t *testing.T) {
	defer func() {
		if recover() == nil {
			t.Fatal("AskValidated with no attempts did not panic")
		}
	}()
	AskValidated(context.Background(), &scriptedRunner{}, Ask{Validate: upper}, dlog.NewTestLogger(), "daemon.test.ask")
}

func TestWordsValidate(t *testing.T) {
	w := Words{Noun: "a name", MinWords: 2, MaxWords: 3, MaxChars: 12}
	cases := []struct {
		answer  string
		want    string
		wantErr string
	}{
		{answer: "Two words", want: "Two words"},
		{answer: `  "Quoted  words."  `, want: "Quoted words"},
		{answer: "‘Curly quotes’", want: "Curly quotes"},
		{answer: "One", wantErr: "a name is at least 2 words"},
		{answer: "one two three four", wantErr: "a name is at most 3 words"},
		{answer: "longword longerword", wantErr: "a name is at most 12 characters"},
		{answer: "two\nlines", wantErr: "a name is one line"},
	}
	for _, tc := range cases {
		t.Run(tc.answer, func(t *testing.T) {
			// Act
			got, err := w.Validate(tc.answer)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("Validate(%q) = %q, %v; want an error naming %q", tc.answer, got, err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("Validate(%q) = %q, %v; want %q", tc.answer, got, err, tc.want)
			}
		})
	}
}

func TestWordsValidateSaysOneWordInTheSingular(t *testing.T) {
	// Act
	_, err := Words{Noun: "a gist", MinWords: 1, MaxWords: 2, MaxChars: 10}.Validate("")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "at least 1 word,") {
		t.Fatalf("err = %v, want the singular", err)
	}
}

func TestWordsValidatePanicsOnImpossibleBounds(t *testing.T) {
	for _, w := range []Words{{MinWords: 0, MaxWords: 1, MaxChars: 1}, {MinWords: 2, MaxWords: 1, MaxChars: 1}, {MinWords: 1, MaxWords: 1}} {
		t.Run(fmt.Sprintf("%+v", w), func(t *testing.T) {
			defer func() {
				if recover() == nil {
					t.Fatal("Validate did not panic")
				}
			}()
			_, _ = w.Validate("x")
		})
	}
}
