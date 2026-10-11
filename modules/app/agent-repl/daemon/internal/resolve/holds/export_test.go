package holds

// HeldStatus exposes the badge composer to the external test package, so every
// arm — including ones no durable record can reach — is tested directly.
var HeldStatus = heldStatus

// TruncateLabel exposes the command truncation for the same reason.
var TruncateLabel = truncateLabel
