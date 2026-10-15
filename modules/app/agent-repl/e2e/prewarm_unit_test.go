package e2e

import (
	"slices"
	"testing"
)

func TestScenarioOverrideReadsEveryRedefinitionForm(t *testing.T) {
	tests := []struct {
		name string
		src  string
		want []string
	}{
		{name: "a cl-letf of a symbol-function", src: "(cl-letf (((symbol-function 'completing-read) (lambda (&rest _) \"a\")))", want: []string{"completing-read"}},
		{name: "a sharp-quoted symbol-function", src: "((symbol-function #'read-string) #'ignore)", want: []string{"read-string"}},
		{name: "an advice-add", src: "(advice-add 'sit-for :override #'ignore)", want: []string{"sit-for"}},
		{name: "a call that redefines nothing", src: "(completing-read \"Pick: \" '(a b))", want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			var got []string
			for _, m := range scenarioOverride.FindAllStringSubmatch(tt.src, -1) {
				got = append(got, m[1])
			}

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("overrides = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestThePrewarmCoversEveryPrimitiveTheScenariosRedefine(t *testing.T) {
	// Arrange / Act
	prewarmed := prewarmedFunctions(t)

	// Assert: the two primitives a scenario's prompt answers redefine, and
	// the boot's own, are all prewarmed.
	for _, name := range []string{"completing-read", "read-string", "yes-or-no-p", "set-window-buffer"} {
		if !slices.Contains(prewarmed, name) {
			t.Errorf("prewarmedFunctions = %v, want it to carry %s", prewarmed, name)
		}
	}
}
