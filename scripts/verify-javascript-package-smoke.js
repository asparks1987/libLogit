"use strict";

const assert = require("assert");
const childProcess = require("child_process");
const fs = require("fs");
const os = require("os");
const path = require("path");

const EXPECTED_PACKAGE_NAME = "@liblogit/liblogit";
const EXPECTED_PACKAGE_VERSION = "1.0.0-alpha.1";
const EXPECTED_TARBALL_NAME = "liblogit-liblogit-1.0.0-alpha.1.tgz";

function run(command, args, options = {}) {
  return childProcess.execFileSync(command, args, {
    encoding: "utf8",
    stdio: options.stdio || ["ignore", "pipe", "pipe"],
    ...options,
  });
}

function runNpm(args, options = {}) {
  if (process.platform === "win32") {
    return run(process.env.ComSpec || "cmd.exe", ["/d", "/s", "/c", "npm", ...args], options);
  }
  return run("npm", args, options);
}

function makeTempDir(prefix) {
  return fs.mkdtempSync(path.join(os.tmpdir(), prefix));
}

function removeTempDir(target) {
  fs.rmSync(target, { recursive: true, force: true });
}

function parseNpmPackJson(output) {
  try {
    return JSON.parse(output);
  } catch {
    return undefined;
  }
}

function packPackage(packageDir, packDir, cacheDir) {
  const output = runNpm(["pack", "--json", "--pack-destination", packDir, "--cache", cacheDir], {
    cwd: packageDir,
  });
  let tarball;
  const entries = parseNpmPackJson(output);
  if (Array.isArray(entries) && entries.length === 1 && entries[0].filename) {
    const entry = entries[0];
    assert.strictEqual(entry.name, EXPECTED_PACKAGE_NAME, "unexpected npm pack package name");
    assert.strictEqual(entry.version, EXPECTED_PACKAGE_VERSION, "unexpected npm pack package version");
    assert.strictEqual(entry.filename, EXPECTED_TARBALL_NAME, "unexpected npm tarball filename");
    assert.ok(Array.isArray(entry.files), "npm pack JSON did not include the packed file list");
    tarball = path.join(packDir, entry.filename);
    const packedFiles = new Set(entry.files.map((packedFile) => packedFile.path));
    for (const expected of ["package.json", "README.md", "src/liblogit.js"]) {
      assert.ok(packedFiles.has(expected), `packed npm artifact is missing ${expected}`);
    }
  }

  if (!tarball) {
    const candidates = fs.readdirSync(packDir).filter((entry) => entry.endsWith(".tgz"));
    assert.strictEqual(candidates.length, 1, `expected one npm tarball, found ${candidates.length}`);
    assert.strictEqual(candidates[0], EXPECTED_TARBALL_NAME, "unexpected npm tarball filename");
    tarball = path.join(packDir, candidates[0]);
  }
  assert.ok(fs.existsSync(tarball), `npm tarball was not created: ${tarball}`);
  return tarball;
}

function installPackage(tarball, consumerDir, cacheDir) {
  fs.writeFileSync(
    path.join(consumerDir, "package.json"),
    JSON.stringify(
      {
        name: "liblogit-js-installed-smoke",
        private: true,
        type: "commonjs",
      },
      null,
      2,
    ),
    "utf8",
  );
  runNpm(
    [
      "install",
      "--ignore-scripts",
      "--no-audit",
      "--fund=false",
      "--cache",
      cacheDir,
      tarball,
    ],
    { cwd: consumerDir, stdio: "pipe" },
  );
}

function inspectInstalledPackage(consumerDir) {
  const packageJsonPath = path.join(
    consumerDir,
    "node_modules",
    "@liblogit",
    "liblogit",
    "package.json",
  );
  assert.ok(fs.existsSync(packageJsonPath), `installed package.json is missing: ${packageJsonPath}`);
  const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, "utf8"));
  assert.strictEqual(packageJson.name, EXPECTED_PACKAGE_NAME, "unexpected installed npm package name");
  assert.strictEqual(
    packageJson.version,
    EXPECTED_PACKAGE_VERSION,
    "unexpected installed npm package version",
  );
  assert.strictEqual(packageJson.main, "src/liblogit.js", "unexpected installed npm package main");
}

function runConsumerSmoke(consumerDir) {
  const smoke = String.raw`
const assert = require("assert");
const fs = require("fs");
const path = require("path");
const { LOGIT, levels, loadLogits } = require("@liblogit/liblogit");

const work = process.argv[1];
const logs = path.join(work, "logs");
const remote = path.join(work, "remote");
const localPath = path.join(logs, "installed.jsonl");
const remotePath = path.join(remote, "installed-remote.jsonl");

let stderr = "";
const originalWrite = process.stderr.write.bind(process.stderr);
process.stderr.write = (chunk, ...args) => {
  stderr += String(chunk);
  return originalWrite(chunk, ...args);
};

const LogIT = new LOGIT({
  name: "InstalledSmoke",
  localPath,
  remotePath,
  level: "debug",
  timestamp: false,
  format: "json",
  metadata: { component: "npm-smoke" },
  sinks: ["console", "file", "network"],
});

LogIT.at(levels.DEBUG).append({ event: "installed_npm_smoke" }).commit();
LogIT.log(levels.INFO, "installed npm info event");

process.stderr.write = originalWrite;

assert.ok(stderr.includes("installed_npm_smoke"), "installed package did not emit console output");
assert.ok(fs.existsSync(localPath), "installed package did not create local file output");
assert.ok(fs.existsSync(remotePath), "installed package did not create remote-path output");

const localText = fs.readFileSync(localPath, "utf8");
const remoteText = fs.readFileSync(remotePath, "utf8");
assert.ok(localText.includes("installed npm info event"), "local file output did not include info message");
assert.ok(remoteText.includes("installed_npm_smoke"), "remote-path output did not include debug message");

const configPath = path.join(work, "logit.json");
const configuredPath = path.join(logs, "configured.log");
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
      Configured: {
        localPath: configuredPath,
        level: "debug",
      },
    },
  }),
  "utf8",
);

const logits = loadLogits(configPath);
logits.Configured.at("debug").append("configured package smoke").commit();
assert.strictEqual(fs.readFileSync(configuredPath, "utf8").trim(), "DEBUG configured package smoke");

console.log("Installed npm package smoke test passed.");
`;

  run(process.execPath, ["-e", smoke, consumerDir], {
    cwd: consumerDir,
    stdio: "inherit",
  });
}

function main() {
  const packageDir = path.resolve(process.argv[2] || path.join("languages", "javascript"));
  assert.ok(fs.existsSync(path.join(packageDir, "package.json")), `package.json not found under ${packageDir}`);

  const packDir = makeTempDir("liblogit-js-pack-");
  const consumerDir = makeTempDir("liblogit-js-consumer-");
  const cacheDir = makeTempDir("liblogit-js-npm-cache-");
  try {
    const tarball = packPackage(packageDir, packDir, cacheDir);
    installPackage(tarball, consumerDir, cacheDir);
    inspectInstalledPackage(consumerDir);
    runConsumerSmoke(consumerDir);
    console.log(`Inspected and smoked ${path.basename(tarball)}.`);
  } finally {
    removeTempDir(packDir);
    removeTempDir(consumerDir);
    removeTempDir(cacheDir);
  }
}

main();
