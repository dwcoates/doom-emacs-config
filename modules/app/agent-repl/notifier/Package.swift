// swift-tools-version:5.10
//
// Zettel.app, agent-repl's macOS desktop notifier (see AGENTS.md beside this
// file). One executable, the app, over a library holding everything it does
// (so it is unit-tested), over the generated bindings of the DESKTOP NOTIFIER
// section of agentrepl.v1 (proto/gen/swift, linked in as Sources/ZettelProto).
//
// swift-protobuf is the ONE pin for both halves of the Swift bindings: the
// runtime library linked into the app, and the protoc-gen-swift plugin
// proto/Makefile generates them with, built from this package's checkout of
// it (`swift build --product protoc-gen-swift`). A generator and a runtime of
// different versions cannot meet.
import PackageDescription

let package = Package(
    name: "Zettel",
    platforms: [.macOS(.v14)],
    products: [
        .executable(name: "Zettel", targets: ["Zettel"]),
        .executable(name: "ZettelIcon", targets: ["ZettelIcon"]),
    ],
    dependencies: [
        .package(url: "https://github.com/apple/swift-protobuf.git", exact: "1.39.0"),
    ],
    targets: [
        .target(
            name: "ZettelProto",
            dependencies: [.product(name: "SwiftProtobuf", package: "swift-protobuf")]
        ),
        .target(
            name: "ZettelNotifier",
            dependencies: ["ZettelProto", .product(name: "SwiftProtobuf", package: "swift-protobuf")]
        ),
        .executableTarget(name: "Zettel", dependencies: ["ZettelNotifier"]),
        .executableTarget(name: "ZettelIcon"),
        .testTarget(name: "ZettelNotifierTests", dependencies: ["ZettelNotifier", "ZettelProto"]),
    ]
)
