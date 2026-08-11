import { afterEach, describe, expect, it } from "vitest";
import fs from "node:fs";
import path from "node:path";
import { create } from "@bufbuild/protobuf";
import { SpillJournal } from "../src/uds/store-spill.js";
import { EventSchema } from "../src/uds/proto.js";
import { tmpSpillDir } from "./uds-harness.js";

const journals: SpillJournal[] = [];
afterEach(() => {
  journals.splice(0).forEach((j) => j.close());
});

/** A journal on a fresh directory, closed by the shared cleanup. */
function openJournal(dir: string, sessionId = "sess-1"): SpillJournal {
  const journal = new SpillJournal({ path: path.join(dir, "store-write-spill.bin"), sessionId });
  journals.push(journal);
  return journal;
}

function event(seq: bigint, writeId: string) {
  return create(EventSchema, { sessionId: "sess-1", seq, writeId });
}

describe("SpillJournal", () => {
  it("reads back an appended batch", async () => {
    // Arrange
    const journal = openJournal(tmpSpillDir());

    // Act
    journal.append([event(1n, "w-1")]);

    // Assert
    expect(journal.read().map((batch) => batch.map((e) => e.writeId))).toEqual([["w-1"]]);
  });

  it("preserves the order batches were appended in", async () => {
    // Arrange: the journal is replayed into the store, and a reordered replay
    // would file the conversation out of order.
    const journal = openJournal(tmpSpillDir());

    // Act
    journal.append([event(1n, "w-1")]);
    journal.append([event(2n, "w-2")]);
    journal.append([event(3n, "w-3")]);

    // Assert
    expect(journal.read().map((batch) => batch[0]!.writeId)).toEqual(["w-1", "w-2", "w-3"]);
  });

  it("carries records across a reopen, which is the whole point", async () => {
    // Arrange: the shim that appended these is gone.
    const dir = tmpSpillDir();
    const first = openJournal(dir);
    first.append([event(1n, "w-1")]);
    first.close();

    // Act
    const second = openJournal(dir);

    // Assert
    expect(second.read().map((batch) => batch[0]!.writeId)).toEqual(["w-1"]);
  });

  it("reports empty when nothing is owed", async () => {
    // Arrange / Act
    const journal = openJournal(tmpSpillDir());

    // Assert
    expect(journal.isEmpty()).toBe(true);
  });

  it("reports non-empty while a batch is owed", async () => {
    // Arrange
    const journal = openJournal(tmpSpillDir());

    // Act
    journal.append([event(1n, "w-1")]);

    // Assert
    expect(journal.isEmpty()).toBe(false);
  });

  it("drops every record on clear", async () => {
    // Arrange
    const journal = openJournal(tmpSpillDir());
    journal.append([event(1n, "w-1")]);

    // Act
    journal.clear();

    // Assert
    expect(journal.read()).toEqual([]);
  });

  it("keeps the complete records before a torn tail", async () => {
    // Arrange: a shim killed mid-append leaves a partial record. It was never
    // fsynced, so nothing was ever reported held for it.
    const dir = tmpSpillDir();
    const journal = openJournal(dir);
    journal.append([event(1n, "w-1")]);
    journal.close();
    const file = path.join(dir, "store-write-spill.bin");
    fs.appendFileSync(file, Buffer.from([0, 0, 0, 64, 1, 2, 3]));

    // Act
    const reopened = openJournal(dir);
    const recovered = reopened.read();

    // Assert
    expect(recovered.map((batch) => batch[0]!.writeId)).toEqual(["w-1"]);
  });

  it("truncates the torn tail away so it is not read twice", async () => {
    // Arrange
    const dir = tmpSpillDir();
    const journal = openJournal(dir);
    journal.append([event(1n, "w-1")]);
    journal.close();
    const file = path.join(dir, "store-write-spill.bin");
    fs.appendFileSync(file, Buffer.from([0, 0, 0, 64, 1, 2, 3]));
    const reopened = openJournal(dir);
    reopened.read();
    reopened.close();

    // Act
    const third = openJournal(dir);

    // Assert
    expect(third.read().map((batch) => batch[0]!.writeId)).toEqual(["w-1"]);
  });

  it("refuses a file that is not a journal and preserves it beside a fresh one", async () => {
    // Arrange: whatever wrote it, parsing a foreign file as records would be
    // worse than refusing it, and deleting it would destroy the evidence.
    const dir = tmpSpillDir();
    const file = path.join(dir, "store-write-spill.bin");
    fs.writeFileSync(file, "this is not a spill journal");

    // Act
    const journal = openJournal(dir);
    const recovered = journal.read();

    // Assert
    expect(recovered).toEqual([]);
    expect(fs.readdirSync(dir).some((name) => name.includes(".foreign-"))).toBe(true);
  });

  it("refuses a journal written in a format version it does not speak", async () => {
    // Arrange: a downgrade must not misread a newer layout as records.
    const dir = tmpSpillDir();
    const file = path.join(dir, "store-write-spill.bin");
    const header = Buffer.alloc(8);
    Buffer.from("ARSP", "ascii").copy(header, 0);
    header.writeUInt32BE(99, 4);
    fs.writeFileSync(file, header);

    // Act
    const journal = openJournal(dir);

    // Assert
    expect(journal.read()).toEqual([]);
    expect(fs.readdirSync(dir).some((name) => name.includes(".foreign-"))).toBe(true);
  });

  it("reports closed once the descriptor is released", async () => {
    // Arrange
    const journal = openJournal(tmpSpillDir());

    // Act
    journal.close();

    // Assert
    expect(journal.isClosed()).toBe(true);
  });

  it("leaves the FILE in place when the descriptor is released", async () => {
    // Arrange: close() is this shim letting go, not the record going away — the
    // next shim on the workspace has to find it.
    const dir = tmpSpillDir();
    const journal = openJournal(dir);
    journal.append([event(1n, "w-1")]);

    // Act
    journal.close();

    // Assert
    expect(fs.existsSync(path.join(dir, "store-write-spill.bin"))).toBe(true);
  });

  it("surfaces an append onto a released descriptor instead of losing it quietly", async () => {
    // Arrange
    const journal = openJournal(tmpSpillDir());
    journal.close();

    // Act / Assert
    expect(() => journal.append([event(1n, "w-1")])).toThrow();
  });
});
