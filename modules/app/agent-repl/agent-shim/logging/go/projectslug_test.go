package logging

import "testing"

func TestVendorProjectSlug(t *testing.T) {
	tests := []struct {
		name string
		cwd  string
		want string
	}{
		{name: "slashes become dashes", cwd: "/Users/x/repo", want: "-Users-x-repo"},
		{name: "a dot becomes a dash, doubling it", cwd: "/Users/x/.config/doom", want: "-Users-x--config-doom"},
		{name: "an underscore becomes a dash", cwd: "/private/var/folders/_m/x", want: "-private-var-folders--m-x"},
		{name: "case and existing dashes survive", cwd: "/Users/X/ship-gns", want: "-Users-X-ship-gns"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act.
			got := VendorProjectSlug(test.cwd)

			// Assert.
			if got != test.want {
				t.Fatalf("VendorProjectSlug(%q) = %q, want %q", test.cwd, got, test.want)
			}
		})
	}
}
