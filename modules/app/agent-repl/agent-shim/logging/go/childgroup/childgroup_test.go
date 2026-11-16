package childgroup

import (
	"errors"
	"os"
	"os/exec"
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

func TestKillGroupKillsUntilNoLiveMemberIsLeft(t *testing.T) {
	leader := Member{Pid: 4242, Comm: "sh", State: "sleeping"}
	forked := Member{Pid: 4243, Comm: "sh", State: "being created"}
	tests := []struct {
		name   string
		rounds [][]Member
		group  []error
		want   error
		kills  []int
	}{
		{name: "a group killed in one pass", rounds: [][]Member{{leader}, nil}, group: []error{nil}, want: nil, kills: []int{-4242}},
		{name: "a member forked during the kill is killed on the next pass",
			rounds: [][]Member{{leader}, {forked}, nil}, group: []error{nil, nil}, want: nil, kills: []int{-4242, -4242}},
		{name: "a live member beside a zombie leader is killed by its pid",
			rounds: [][]Member{{forked}, nil}, group: []error{syscall.EPERM}, want: nil, kills: []int{-4242, 4243}},
		{name: "a group with no live member at all", rounds: [][]Member{nil}, want: os.ErrProcessDone},
		{name: "a kill the kernel refuses is the kill failing", rounds: [][]Member{{leader}}, group: []error{syscall.EINVAL}, want: syscall.EINVAL, kills: []int{-4242}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var kills []int
			groupKills := 0
			kill := func(target int, sig syscall.Signal) error {
				if sig != syscall.SIGKILL {
					t.Fatalf("signal %v, want SIGKILL", sig)
				}
				kills = append(kills, target)
				if target < 0 {
					answer := tt.group[groupKills]
					groupKills++
					return answer
				}
				return nil
			}
			reads := 0
			members := func(int) ([]Member, error) {
				m := tt.rounds[reads]
				reads++
				return m, nil
			}

			// Act
			got := killUntilEmpty(4242, kill, members)

			// Assert
			if !errors.Is(got, tt.want) && got != tt.want {
				t.Fatalf("killUntilEmpty = %v, want %v", got, tt.want)
			}
			if !slices.Equal(kills, tt.kills) {
				t.Fatalf("kills = %v, want %v", kills, tt.kills)
			}
		})
	}
}

func TestKillGroupGivesUpOnAGroupThatNeverEmpties(t *testing.T) {
	// Arrange
	member := []Member{{Pid: 4242, Comm: "sh", State: "running"}}
	var kills int
	kill := func(int, syscall.Signal) error { kills++; return nil }

	// Act
	err := killUntilEmpty(4242, kill, func(int) ([]Member, error) { return member, nil })

	// Assert
	if err == nil || kills != maxKillPasses {
		t.Fatalf("killUntilEmpty = %v after %d kills, want an error after %d", err, kills, maxKillPasses)
	}
}

func TestKillGroupKillsARealGroupWithAChildForkedInIt(t *testing.T) {
	// Arrange: a shell and its background subshell, both blocked.
	cmd := exec.Command("sh", "-c", "sleep 600 & sleep 600")
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start: %v", err)
	}
	pgid := cmd.Process.Pid

	// Act
	err := KillGroup(pgid)
	_ = cmd.Wait()

	// Assert
	live, readErr := killableMembers(pgid)
	if err != nil || readErr != nil || len(live) != 0 {
		t.Fatalf("KillGroup = %v; members left %v (%v), want none", err, live, readErr)
	}
}
