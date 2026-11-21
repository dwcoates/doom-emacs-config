import Foundation
import XCTest
import ZettelProto
@testable import ZettelLauncher

final class FramingTests: XCTestCase {
    func testAMessageRoundTripsThroughTheDelimitedForm() throws {
        // Arrange
        let request = bannerRequest()

        // Act
        let read = try readDelimited(Launcher_V1_PostBannerRequest.self, from: MemoryChannel(delimited(request)))

        // Assert
        XCTAssertEqual(read, request)
    }

    func testALengthPastOneByteIsAVarint() throws {
        // Arrange
        var request = bannerRequest()
        request.banner.body.text = String(repeating: "x", count: 300)

        // Act
        let bytes = delimited(request)

        // Assert
        XCTAssertEqual(bytes[0] & 0x80, 0x80, "a body of 300 bytes needs a second length byte")
        XCTAssertEqual(try readDelimited(Launcher_V1_PostBannerRequest.self, from: MemoryChannel(bytes)), request)
    }

    func testAStreamEndingEarlyIsAFramingError() {
        let whole = delimited(bannerRequest())
        for (name, bytes) in [("inside the length", Data()), ("inside the message", whole.prefix(whole.count - 1))] {
            XCTAssertThrowsError(try readDelimited(Launcher_V1_PostBannerRequest.self, from: MemoryChannel(bytes)), name) {
                XCTAssertTrue($0 is FramingError, "\(name): \($0)")
            }
        }
    }

    func testALengthPastABannersIsRefused() {
        // Arrange: a varint of 2^21.
        let bytes = Data([0x80, 0x80, 0x80, 0x01])

        // Act / Assert
        XCTAssertThrowsError(try readDelimited(Launcher_V1_PostBannerRequest.self, from: MemoryChannel(bytes))) {
            XCTAssertTrue("\($0)".contains("past the"), "\($0)")
        }
    }
}

final class BannerExchangeTests: XCTestCase {
    var center: FakeCenter!
    var log: RecordingLogger!

    override func setUp() { fresh() }

    /// Fresh fakes.
    func fresh() {
        center = FakeCenter()
        log = RecordingLogger()
    }

    /// Serves one exchange of `request`, answering what the launcher wrote.
    func exchange(_ bytes: Data) async throws -> Launcher_V1_PostBannerResponse {
        let channel = MemoryChannel(bytes)
        await BannerExchange(center: center, log: log).serve(channel)
        return try readDelimited(Launcher_V1_PostBannerResponse.self, from: MemoryChannel(channel.written))
    }

    func testEachResultIsAnsweredAsItsOutcome() async throws {
        let cases: [(PostResult, Launcher_V1_PostBannerResponse, LogLevel, String)] = [
            (.posted, .with { $0.outcome.posted = .init() }, .info, "posted a banner"),
            (.notAllowed, .with { $0.outcome.notAllowed = .init() }, .error,
             "macOS refused the banner: Zettel is not allowed to notify (System Settings → Notifications → Zettel)"),
            (.failed("center busy"), .with { $0.outcome.failed.platformError = "center busy" }, .error, "macOS refused the banner"),
        ]
        for (result, want, level, record) in cases {
            // Arrange
            fresh()
            center.result = result

            // Act
            let got = try await exchange(delimited(bannerRequest()))

            // Assert
            XCTAssertEqual(got, want, "\(result)")
            XCTAssertEqual(center.posted.map(\.group.id), ["agent-repl:ws1"], "\(result)")
            XCTAssertEqual(log.first(level, record)?.context["daemon_pid"], "4242", "\(result)")
        }
    }

    func testAnUnpostableRequestIsRefusedAndPostsNothing() async throws {
        let cases: [(String, (inout Launcher_V1_PostBannerRequest) -> Void)] = [
            ("banner is missing", { $0.clearBanner() }),
            ("banner.title is empty", { $0.banner.title.text = "" }),
            ("banner.body is empty", { $0.banner.body.text = "" }),
            ("banner.group is empty", { $0.banner.group.id = "" }),
            ("banner.click.workspace is empty", { $0.banner.click.workspace.id = "" }),
            ("daemon.pid is not positive", { $0.daemon.pid = 0 }),
        ]
        for (reason, mutate) in cases {
            // Arrange
            fresh()
            var request = bannerRequest()
            mutate(&request)

            // Act
            let got = try await exchange(delimited(request))

            // Assert
            XCTAssertEqual(got, .with { $0.refused.reason = reason })
            XCTAssertTrue(center.posted.isEmpty, reason)
            XCTAssertEqual(log.first(.error, "a daemon's banner was refused; nothing was posted")?.context["reason"], reason)
        }
    }

    func testAnUnreadableRequestPostsNothingAndIsRecorded() async {
        // Arrange
        let channel = MemoryChannel(Data([0x05, 0x01]))

        // Act
        await BannerExchange(center: center, log: log).serve(channel)

        // Assert
        XCTAssertTrue(center.posted.isEmpty)
        XCTAssertTrue(channel.written.isEmpty)
        XCTAssertNotNil(log.first(.error, "a daemon's banner could not be read; nothing was posted"))
    }

    func testAnAnswerThatCannotBeWrittenIsRecorded() async {
        // Arrange
        let channel = MemoryChannel(delimited(bannerRequest()))
        channel.writeError = FramingError(description: "broken pipe")

        // Act
        await BannerExchange(center: center, log: log).serve(channel)

        // Assert
        XCTAssertEqual(log.first(.error, "the banner's answer did not reach its daemon")?.context["cause"], "broken pipe")
    }
}

final class BannerServerTests: XCTestCase {
    func testADaemonsBannerIsPostedOverARealSocket() async throws {
        // Arrange
        let center = FakeCenter()
        let (path, listener) = try listen()
        BannerServer(listener: listener, exchange: BannerExchange(center: center, log: RecordingLogger()), log: RecordingLogger()).start()

        // Act
        let channel = try SocketChannel(fd: try dial(path), timeout: 5)
        try writeDelimited(bannerRequest(), to: channel)
        let answer = try readDelimited(Launcher_V1_PostBannerResponse.self, from: channel)

        // Assert
        XCTAssertEqual(answer, .with { $0.outcome.posted = .init() })
        XCTAssertEqual(center.posted.count, 1)
    }
}

final class HolderSocketTests: XCTestCase {
    func testTheHeldSocketIsTakenFromTheHolder() throws {
        // Arrange
        let (path, held) = try listen()
        let control = try fakeHolder { name, conn in
            XCTAssertEqual(name, "launcher")
            try grant(held, on: conn)
        }

        // Act
        let fd = try takeHeldSocket(control: control, name: "launcher")
        defer { close(fd) }

        // Assert: a dial of the held socket's path is accepted on the taken fd.
        let dialed = try dial(path)
        defer { close(dialed) }
        let accepted = accept(fd, nil, nil)
        XCTAssertGreaterThanOrEqual(accepted, 0, String(cString: strerror(errno)))
        close(accepted)
    }

    func testARefusalIsTheHoldersReason() throws {
        // Arrange
        let control = try fakeHolder { _, conn in
            let refusal = Array("0no socket named launcher\n".utf8)
            _ = Darwin.write(conn, refusal, refusal.count)
        }

        // Act / Assert
        XCTAssertThrowsError(try takeHeldSocket(control: control, name: "launcher")) {
            XCTAssertTrue("\($0)".contains("refused launcher: no socket named launcher"), "\($0)")
        }
    }

    func testNoHolderIsAnErrorNamingItsControlSocket() {
        XCTAssertThrowsError(try takeHeldSocket(control: "/nonexistent/holder.sock", name: "launcher")) {
            XCTAssertTrue("\($0)".contains("/nonexistent/holder.sock"), "\($0)")
        }
    }

    func testANameWithALineBreakIsRefusedBeforeAnyDial() {
        XCTAssertThrowsError(try takeHeldSocket(control: "/nonexistent/holder.sock", name: "a\nb")) {
            XCTAssertTrue("\($0)".contains("line break"), "\($0)")
        }
    }
}

final class UnixSocketAddressTests: XCTestCase {
    func testAPathIsCopiedNulTerminatedIntoTheAddress() {
        // Act
        var address = unixSocketAddress("/tmp/a.sock")!

        // Assert
        XCTAssertEqual(Int32(address.sun_family), AF_UNIX)
        let path = withUnsafeBytes(of: &address.sun_path) { String(cString: $0.bindMemory(to: CChar.self).baseAddress!) }
        XCTAssertEqual(path, "/tmp/a.sock")
    }

    func testAPathPastSunPathHasNoAddress() {
        XCTAssertNil(unixSocketAddress("/" + String(repeating: "x", count: 200)))
    }

    func testTheHolderAndTheTestsDialThroughTheOneHelper() throws {
        // A hand-rolled sockaddr_un beside the helper is a second copy of it.
        let source = try String(contentsOf: URL(fileURLWithPath: #filePath)
            .deletingLastPathComponent().deletingLastPathComponent().deletingLastPathComponent()
            .appendingPathComponent("Sources/ZettelLauncher/HolderSocket.swift"), encoding: .utf8)
        XCTAssertEqual(source.components(separatedBy: "sockaddr_un()").count - 1, 1, "sockaddr_un is built outside unixSocketAddress")
    }
}

final class ResidentTests: XCTestCase {
    func testTheInstanceLockIsExclusiveUntilItsHolderLetsGo() throws {
        // Arrange
        let path = scratchDir().appendingPathComponent("launcher.lock")
        let first = try takeInstanceLock(at: path)

        // Act / Assert
        XCTAssertThrowsError(try takeInstanceLock(at: path)) {
            XCTAssertTrue($0 is LockHeld, "\($0)")
        }
        close(first)
        let again = try takeInstanceLock(at: path)
        close(again)
    }

    func testTheBuildReportIsTheSharedShape() throws {
        // Arrange
        let dir = scratchDir()

        // Act
        try writeBuildReport(dir: dir, pid: 77, build: "abc")

        // Assert
        let data = try Data(contentsOf: dir.appendingPathComponent("launcher.build.json"))
        let object = try JSONSerialization.jsonObject(with: data) as! [String: Any]
        XCTAssertEqual(object["pid"] as? Int, 77)
        XCTAssertEqual(object["build"] as? String, "abc")
        XCTAssertEqual(object.count, 2)
    }
}

// ---- sockets for the tests ----

/// A short scratch directory: a unix socket's path must fit sun_path.
func scratchDir() -> URL {
    let dir = URL(fileURLWithPath: "/tmp").appendingPathComponent("zl-\(UUID().uuidString.prefix(8))")
    try! FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
    return dir
}

/// Binds and listens on a fresh unix socket, answering its path and fd.
func listen() throws -> (String, Int32) {
    let path = scratchDir().appendingPathComponent("s.sock").path
    let fd = socket(AF_UNIX, SOCK_STREAM, 0)
    var address = unixSocketAddress(path)!
    let bound = withUnsafePointer(to: &address) {
        $0.withMemoryRebound(to: sockaddr.self, capacity: 1) { bind(fd, $0, socklen_t(MemoryLayout<sockaddr_un>.size)) }
    }
    guard bound == 0, Darwin.listen(fd, 8) == 0 else { throw FramingError(description: "listen \(path): \(String(cString: strerror(errno)))") }
    return (path, fd)
}

/// Connects to the unix socket at `path`.
func dial(_ path: String) throws -> Int32 {
    let fd = socket(AF_UNIX, SOCK_STREAM, 0)
    var address = unixSocketAddress(path)!
    let connected = withUnsafePointer(to: &address) {
        $0.withMemoryRebound(to: sockaddr.self, capacity: 1) { connect(fd, $0, socklen_t(MemoryLayout<sockaddr_un>.size)) }
    }
    guard connected == 0 else { throw FramingError(description: "dial \(path): \(String(cString: strerror(errno)))") }
    return fd
}


/// A socket holder that answers one request with `answer`, given the name
/// asked for and the connection; answers its control socket's path.
func fakeHolder(_ answer: @escaping @Sendable (String, Int32) throws -> Void) throws -> String {
    let (path, listener) = try listen()
    Thread {
        let conn = accept(listener, nil, nil)
        defer { close(conn); close(listener) }
        var name = Data()
        var byte: UInt8 = 0
        while Darwin.read(conn, &byte, 1) == 1, byte != UInt8(ascii: "\n") { name.append(byte) }
        try? answer(String(decoding: name, as: UTF8.self), conn)
    }.start()
    return path
}

/// Grants `fd` on `conn` as the holder does: a status byte with the fd as its
/// ancillary data.
func grant(_ fd: Int32, on conn: Int32) throws {
    var status = UInt8(ascii: "1")
    let length = MemoryLayout<cmsghdr>.size + MemoryLayout<Int32>.size
    let control = UnsafeMutableRawPointer.allocate(byteCount: length, alignment: MemoryLayout<cmsghdr>.alignment)
    defer { control.deallocate() }
    control.storeBytes(of: cmsghdr(cmsg_len: socklen_t(length), cmsg_level: SOL_SOCKET, cmsg_type: SCM_RIGHTS), as: cmsghdr.self)
    control.storeBytes(of: fd, toByteOffset: MemoryLayout<cmsghdr>.size, as: Int32.self)
    try withUnsafeMutablePointer(to: &status) { statusPointer in
        var vector = iovec(iov_base: UnsafeMutableRawPointer(statusPointer), iov_len: 1)
        try withUnsafeMutablePointer(to: &vector) { vectorPointer in
            var message = msghdr(msg_name: nil, msg_namelen: 0, msg_iov: vectorPointer, msg_iovlen: 1,
                                 msg_control: control, msg_controllen: socklen_t(length), msg_flags: 0)
            guard sendmsg(conn, &message, 0) == 1 else { throw FramingError(description: "sendmsg: \(String(cString: strerror(errno)))") }
        }
    }
}
