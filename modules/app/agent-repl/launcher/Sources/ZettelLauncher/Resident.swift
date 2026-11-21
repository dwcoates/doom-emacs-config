import Foundation

/// The resident launcher's one-instance lock and build report.

/// Another launcher holds the lock.
public struct LockHeld: Error, Equatable, CustomStringConvertible {
    public let description: String
}

/// Takes the exclusive lock at `path` for the life of this process: the
/// kernel releases it when the process exits, however it exits, so ONE
/// launcher serves at a time and a crashed one never leaves it held. Throws
/// LockHeld when another process holds it.
public func takeInstanceLock(at path: URL) throws -> Int32 {
    try FileManager.default.createDirectory(at: path.deletingLastPathComponent(), withIntermediateDirectories: true)
    let fd = open(path.path, O_RDWR | O_CREAT | O_CLOEXEC, 0o644)
    guard fd >= 0 else {
        throw LockHeld(description: "open the launcher's lock \(path.path): \(String(cString: strerror(errno)))")
    }
    guard flock(fd, LOCK_EX | LOCK_NB) == 0 else {
        let cause = errno
        close(fd)
        if cause == EWOULDBLOCK {
            throw LockHeld(description: "another launcher holds \(path.path)")
        }
        throw LockHeld(description: "lock \(path.path): \(String(cString: strerror(cause)))")
    }
    return fd
}

/// The name the launcher's build report carries (agentrepl/logging/
/// buildreport.ServiceLauncher).
public let buildReportService = "launcher"

/// Writes this process's build report into `dir`, in the shape every service
/// reports in (agentrepl/logging/buildreport): its pid and the content hash
/// of its executable, ATOMICALLY, so the daemon's deploy reads the previous
/// report or this one, never half of one.
public func writeBuildReport(dir: URL, pid: Int32, build: String) throws {
    try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
    let payload = try JSONSerialization.data(withJSONObject: ["pid": Int(pid), "build": build], options: [.sortedKeys])
    try payload.write(to: dir.appendingPathComponent("\(buildReportService).build.json"), options: .atomic)
}
