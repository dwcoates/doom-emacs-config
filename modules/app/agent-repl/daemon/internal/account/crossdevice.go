package account

import "syscall"

// crossDeviceErrno is the kernel's EXDEV: a rename whose two paths are on
// different filesystems. It is named once here so the fallback in carryFile /
// carryTree tests for exactly that errno and nothing else.
const crossDeviceErrno = syscall.EXDEV
