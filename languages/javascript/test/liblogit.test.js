"use strict";

const assert = require("assert");
const fs = require("fs");
const os = require("os");
const path = require("path");
const { LOGIT, loadLogits, levels } = require("../src/liblogit");

function tempDir() {
  return fs.mkdtempSync(path.join(os.tmpdir(), "liblogit-js-"));
}

function readLines(filePath) {
  return fs.readFileSync(filePath, "utf8").trim().split(/\r?\n/);
}

{
  const dir = tempDir();
  const logPath = path.join(dir, "app.log");
  const LogIT = new LOGIT();
  LogIT.localPath = logPath;
  LogIT.level = levels.DEBUG;
  LogIT.timestamp = false;
  LogIT.at(levels.DEBUG).append("hello").append(" world").commit();
  assert.ok(LogIT.sinks.includes("file"));
  assert.deepStrictEqual(readLines(logPath), ["DEBUG hello world"]);
}

{
  const dir = tempDir();
  const logPath = path.join(dir, "app.log");
  const configPath = path.join(dir, "logit.json");
  fs.writeFileSync(
    configPath,
    JSON.stringify({
      version: "0.2",
      defaults: {
        level: "info",
        timestamp: false,
        tag_level: true,
        format: "text",
        sinks: ["file"],
      },
      logits: {
        AppLog: {
          path: logPath,
          level: "debug",
        },
      },
    }),
    "utf8",
  );

  const logits = loadLogits(configPath);
  logits.AppLog.at("debug").append("from config").commit();
  assert.deepStrictEqual(readLines(logPath), ["DEBUG from config"]);
}

{
  const dir = tempDir();
  const logPath = path.join(dir, "audit.jsonl");
  const audit = new LOGIT({
    name: "AuditLog",
    localPath: logPath,
    level: "info",
    timestamp: false,
    format: "json",
    metadata: { component: "audit" },
    sinks: ["file"],
  });
  audit.log("info", "user signed in");
  assert.deepStrictEqual(readLines(logPath), [
    '{"level":"info","logger":"AuditLog","message":"user signed in","metadata":{"component":"audit"}}',
  ]);
}

for (const fixtureName of fs.readdirSync(path.join(__dirname, "../../../tests/conformance/fixtures"))) {
  if (!fixtureName.endsWith(".json")) continue;

  const fixturePath = path.join(__dirname, "../../../tests/conformance/fixtures", fixtureName);
  const fixture = JSON.parse(fs.readFileSync(fixturePath, "utf8"));
  const dir = tempDir();
  const config = fixture.config;

  for (const logitConfig of Object.values(config.logits)) {
    for (const key of ["path", "localPath", "file_location"]) {
      if (logitConfig[key]) logitConfig[key] = path.join(dir, logitConfig[key]);
    }
    for (const key of ["remotePath", "remote_path", "network_path", "network_file_location"]) {
      if (logitConfig[key]) logitConfig[key] = path.join(dir, logitConfig[key]);
    }
  }

  const configPath = path.join(dir, "logit.json");
  fs.writeFileSync(configPath, JSON.stringify(config), "utf8");
  const logits = loadLogits(configPath);

  for (const message of fixture.messages) {
    const builder = logits[message.logger].at(message.level);
    for (const fragment of message.fragments) {
      builder.append(fragment);
    }
    builder.commit();
  }

  for (const [relativePath, expectedLines] of Object.entries(fixture.expected_files)) {
    assert.deepStrictEqual(readLines(path.join(dir, relativePath)), expectedLines, fixture.name);
  }
}

console.log("JavaScript libLogit tests passed");
