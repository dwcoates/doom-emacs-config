package childgroup

import (
	"errors"
	"os"
	"slices"
	"syscall"
	"testing"
)

func TestScrubRepositorySelectorsDropsEverySelectorAndKeepsTheRest(t *testing.T) {
	tests := []struct {
		name string
		env  []string
		want []string
	}{
		{name: "every selector dropped", env: append(selectorEntries(), "PATH=/bin"), want: []string{"PATH=/bin"}},
		{name: "a name that only begins like one kept", env: []string{"GIT_DIRTY=1", "HOME=/h"}, want: []string{"GIT_DIRTY=1", "HOME=/h"}},
		{name: "an entry with no value sign kept", env: []string{"GIT_DIR"}, want: []string{"GIT_DIR"}},
		{name: "nothing to scrub", env: nil, want: []string{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := ScrubRepositorySelectors(tt.env)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("ScrubRepositorySelectors = %v, want %v", got, tt.want)
			}
		})
	}
}

func selectorEntries() []string {
	var out []string
	for _, name := range RepositorySelectors {
		out = append(out, name+"=x")
	}
	return out
}

func TestKillGroupReadsAFinishedGroupAsDone(t *testing.T) {
	tests := []struct {
		name   string
		answer error
		want   error
	}{
		{name: "a group killed", answer: nil, want: nil},
		{name: "a group with no members", answer: syscall.ESRCH, want: os.ErrProcessDone},
		{name: "a group whose members wait to be reaped", answer: syscall.EPERM, want: os.ErrProcessDone},
		{name: "any other answer", answer: syscall.EINVAL, want: syscall.EINVAL},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var signalled int
			var sent syscall.Signal
			kill := func(pid int, sig syscall.Signal) error {
				signalled, sent = pid, sig
				return tt.answer
			}

			// Act
			got := KillGroupWith(kill, 4242)

			// Assert
			if !errors.Is(got, tt.want) && got != tt.want {
				t.Fatalf("KillGroupWith = %v, want %v", got, tt.want)
			}
			if signalled != -4242 || sent != syscall.SIGKILL {
				t.Fatalf("kill(%d, %v), want kill(-4242, SIGKILL): the group, not the leader alone", signalled, sent)
			}
		})
	}
}
