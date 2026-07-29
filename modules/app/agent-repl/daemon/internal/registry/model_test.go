package registry

import "testing"

func TestIsPlaceholderModel(t *testing.T) {
	tests := []struct {
		name  string
		model string
		want  bool
	}{
		{
			// The marker itself, which is what got written onto records and
			// then replayed onto every respawn's argv.
			name:  "the placeholder",
			model: "<synthetic>",
			want:  true,
		},
		{
			name:  "surrounded by whitespace is still the placeholder",
			model: "  <synthetic>\n",
			want:  true,
		},
		{
			// EMPTY IS NOT THE PLACEHOLDER. Empty means "pin nothing and let
			// the CLI choose", which the spawn path already handles by omitting
			// --model, and treating it as unusable would break every session
			// that deliberately pins no model.
			name:  "empty is a legitimate absence",
			model: "",
			want:  false,
		},
		{
			name:  "a real model id",
			model: "claude-fable-5",
			want:  false,
		},
		{
			name:  "a model merely containing the word",
			model: "synthetic-test-model",
			want:  false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			got := IsPlaceholderModel(tc.model)

			// Assert.
			if got != tc.want {
				t.Fatalf("IsPlaceholderModel(%q) = %t, want %t", tc.model, got, tc.want)
			}
		})
	}
}
