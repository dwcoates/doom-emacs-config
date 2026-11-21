// swift-tools-version:5.10
//
// Zettel.app, the launcher: the resident macOS app that opens Emacs (and with
// it the backend) and posts every desktop banner (see AGENTS.md beside this
// file). One executable, the app, over a library holding everything it does
// (so it is unit-tested), over the generated bindings of launcher.v1 and the
// click it reports (proto/gen/swift, linked in as Sources/ZettelProto).
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
            name: "ZettelLauncher",
            dependencies: ["ZettelProto", .product(name: "SwiftProtobuf", package: "swift-protobuf")]
        ),
        .executableTarget(name: "Zettel", dependencies: ["ZettelLauncher"]),
        .executableTarget(name: "ZettelIcon"),
        .testTarget(name: "ZettelLauncherTests", dependencies: ["ZettelLauncher", "ZettelProto"]),
    ]
)
