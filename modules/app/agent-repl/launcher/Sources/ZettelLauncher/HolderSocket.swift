import Foundation

/// The socket holder's two answers (agentrepl/logging/sockethold).
let holderStatusOK = UInt8(ascii: "1")
let holderStatusRefused = UInt8(ascii: "0")

/// The holder refused, or the exchange with it failed.
public struct HolderError: Error, Equatable, CustomStringConvertible {
    public let description: String
}

/// The address of the unix socket at `path`; nil when the path does not fit
/// `sun_path`.
public func unixSocketAddress(_ path: String) -> sockaddr_un? {
    var address = sockaddr_un()
    address.sun_family = sa_family_t(AF_UNIX)
    let bytes = Array(path.utf8CString)
    guard bytes.count <= MemoryLayout.size(ofValue: address.sun_path) else { return nil }
    withUnsafeMutableBytes(of: &address.sun_path) { raw in bytes.withUnsafeBytes { raw.copyMemory(from: $0) } }
    return address
}

/// Takes the listening socket `name` from the socket holder serving
/// `control`: one line naming it, then one status byte with the socket's fd
/// attached (SCM_RIGHTS), or a refusal and its reason
/// (agentrepl/logging/sockethold.Take). The holder owns the socket for good,
/// so it outlives every restart of the launcher, and a daemon's dial made
/// while the launcher is down queues in its backlog.
public func takeHeldSocket(control: String, name: String) throws -> Int32 {
    guard !name.isEmpty, !name.contains("\n") else {
        throw HolderError(description: "socket name \(name.debugDescription) is empty or holds a line break")
    }
    let fd = socket(AF_UNIX, SOCK_STREAM, 0)
    guard fd >= 0 else { throw HolderError(description: "open a socket to the holder: \(String(cString: strerror(errno)))") }
    defer { close(fd) }
    guard var address = unixSocketAddress(control) else {
        throw HolderError(description: "the holder's control socket path \(control) is too long")
    }
    let connected = withUnsafePointer(to: &address) {
        $0.withMemoryRebound(to: sockaddr.self, capacity: 1) { connect(fd, $0, socklen_t(MemoryLayout<sockaddr_un>.size)) }
    }
    guard connected == 0 else {
        throw HolderError(description: "connect to the holder at \(control): \(String(cString: strerror(errno)))")
    }
    let line = Array((name + "\n").utf8)
    guard Darwin.write(fd, line, line.count) == line.count else {
        throw HolderError(description: "ask the holder at \(control) for \(name): \(String(cString: strerror(errno)))")
    }
    // ONE STATUS BYTE, WITH THE GRANTED FD AS ITS ANCILLARY DATA.
    let statusBuffer = UnsafeMutablePointer<UInt8>.allocate(capacity: 1)
    defer { statusBuffer.deallocate() }
    let controlLength = MemoryLayout<cmsghdr>.size + MemoryLayout<Int32>.size + 16
    let controlBuffer = UnsafeMutableRawPointer.allocate(byteCount: controlLength, alignment: MemoryLayout<cmsghdr>.alignment)
    defer { controlBuffer.deallocate() }
    controlBuffer.initializeMemory(as: UInt8.self, repeating: 0, count: controlLength)
    var vector = iovec(iov_base: UnsafeMutableRawPointer(statusBuffer), iov_len: 1)
    let received = withUnsafeMutablePointer(to: &vector) { vectorPointer -> Int in
        var message = msghdr(msg_name: nil, msg_namelen: 0, msg_iov: vectorPointer, msg_iovlen: 1,
                             msg_control: controlBuffer, msg_controllen: socklen_t(controlLength), msg_flags: 0)
        return recvmsg(fd, &message, 0)
    }
    guard received == 1 else {
        throw HolderError(description: "read the holder's answer at \(control): \(received < 0 ? String(cString: strerror(errno)) : "it hung up")")
    }
    let status = statusBuffer.pointee
    guard status == holderStatusOK else {
        var reason = Data()
        var byte: UInt8 = 0
        while Darwin.read(fd, &byte, 1) == 1, byte != UInt8(ascii: "\n") { reason.append(byte) }
        throw HolderError(description: "the holder at \(control) refused \(name): \(String(decoding: reason, as: UTF8.self))")
    }
    let header = controlBuffer.load(as: cmsghdr.self)
    guard header.cmsg_level == SOL_SOCKET, header.cmsg_type == SCM_RIGHTS else {
        throw HolderError(description: "the holder at \(control) granted \(name) with no socket attached")
    }
    return controlBuffer.load(fromByteOffset: MemoryLayout<cmsghdr>.size, as: Int32.self)
}
