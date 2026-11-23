# Marking a workspace awaiting another's answer

- **AWAITING CORRESPONDENCE.** When this workspace ends a turn to wait for
  another workspace W to answer (you prompted W, or W is finishing something
  this work needs), run `workspace set here -await W` before ending the turn.
  The workspace is drawn in the background (yellow) while it waits. When W's
  answer arrives, run `workspace set here -not-awaiting` FIRST, then act on
  it. The daemon cannot tell whether a reply is the one awaited, so clearing
  the mark is always yours.
