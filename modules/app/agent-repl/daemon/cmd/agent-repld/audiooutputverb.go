package main

import (
	"fmt"
	"io"

	"agent-repld/internal/audiooutput"
)

// runAudioOutputVerb is the audio-output verb (audiooutput.Verb): it prints
// CoreAudio's default output device as one line of JSON, the child the
// banners' speech reads the audio output through, so a CoreAudio call that
// hangs costs the child's bound and never a daemon thread.
//
// runAudioOutputVerb reads the device through read and prints it, answering
// the exit code. A device that cannot be read is said on stderr and exits
// non-zero, which the daemon's Runner carries back as the read's cause.
func runAudioOutputVerb(read func() (audiooutput.Device, error), stdout, stderr io.Writer) int {
	device, err := read()
	if err != nil {
		fmt.Fprintln(stderr, err)
		return exitFailure
	}
	if err := audiooutput.Encode(stdout, device); err != nil {
		fmt.Fprintf(stderr, "write the audio output: %v\n", err)
		return exitFailure
	}
	return exitSuccess
}
