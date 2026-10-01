package workspace

import "testing"

func TestOffersColdCompaction(t *testing.T) {
	tests := []struct {
		name         string
		multiRepoDir string
		configDir    string
		want         bool
	}{
		{name: "the work account is not offered compaction", multiRepoDir: "/work", configDir: "/work", want: false},
		{name: "a personal account beside a work account is offered compaction", multiRepoDir: "/work", configDir: "/personal", want: true},
		{name: "a machine with no work account offers compaction", multiRepoDir: "", configDir: "/personal", want: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			accounts := &fakeAccounts{multiRepoDir: tc.multiRepoDir}

			// Act.
			got := offersColdCompaction(accounts, tc.configDir)

			// Assert.
			if got != tc.want {
				t.Fatalf("offersColdCompaction(%q) = %t, want %t", tc.configDir, got, tc.want)
			}
		})
	}
}
