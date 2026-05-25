"use strict";

const fs = require("fs");
const path = require("path");

const LEVELS = {
  trace: 0,
  debug: 1,
  info: 2,
  warn: 3,
  warning: 3,
  error: 4,
  fatal: 5,
};

const LEVEL_LABELS = {
  trace: "TRACE",
  debug: "DEBUG",
  info: "INFO",
  warn: "WARNING",
  error: "ERROR",
  fatal: "CRITICAL",
};

function normalizeLevel(level) {
  if (typeof level !== "string") {
    throw new TypeError(`level must be a string, got ${typeof level}`);
  }
  const normalized = level.trim().toLowerCase();
  if (!(normalized in LEVELS)) {
    throw new Error(`Unknown level: ${level}`);
  }
  return normalized === "warning" ? "warn" : normalized;
}

function normalizeSinks(value, localPath, remotePath) {
  if (value === undefined || value === null) {
    const sinks = ["console"];
    if (localPath) sinks.push("file");
    if (remotePath) sinks.push("network");
    return sinks;
  }
  if (!Array.isArray(value)) {
    throw new TypeError("sinks must be an array");
  }
  const seen = new Set();
  for (const sink of value) {
    if (!["console", "file", "network"].includes(sink)) {
      throw new Error(`Unknown sink: ${sink}`);
    }
    seen.add(sink);
  }
  return Array.from(seen);
}

class LOGIT {
  constructor(structure = {}) {
    if (structure === null || typeof structure !== "object" || Array.isArray(structure)) {
      throw new TypeError("LOGIT structure must be an object");
    }
    this.name = structure.name || "default";
    this._level = normalizeLevel(structure.level || "info");
    this._localPath = structure.localPath || structure.local_path || structure.path || structure.file_location || null;
    this._remotePath =
      structure.remotePath ||
      structure.remote_path ||
      structure.network_path ||
      structure.network_file_location ||
      null;
    this.timestamp = structure.timestamp !== undefined ? Boolean(structure.timestamp) : true;
    this.tagLevel =
      structure.tagLevel !== undefined
        ? Boolean(structure.tagLevel)
        : structure.tag_level !== undefined
          ? Boolean(structure.tag_level)
          : true;
    this.format = structure.format || "text";
    this.metadata = structure.metadata || {};
    this.enabled = structure.enabled !== undefined ? Boolean(structure.enabled) : true;
    this._sinks = normalizeSinks(structure.sinks, this._localPath, this._remotePath);
  }

  get level() {
    return this._level;
  }

  set level(value) {
    this._level = normalizeLevel(value);
  }

  get localPath() {
    return this._localPath;
  }

  set localPath(value) {
    this._localPath = value ? String(value) : null;
    if (this._localPath && !this._sinks.includes("file")) {
      this._sinks.push("file");
    }
    if (!this._localPath) {
      this._sinks = this._sinks.filter((sink) => sink !== "file");
    }
  }

  get path() {
    return this.localPath;
  }

  set path(value) {
    this.localPath = value;
  }

  get remotePath() {
    return this._remotePath;
  }

  set remotePath(value) {
    this._remotePath = value ? String(value) : null;
    if (this._remotePath && !this._sinks.includes("network")) {
      this._sinks.push("network");
    }
    if (!this._remotePath) {
      this._sinks = this._sinks.filter((sink) => sink !== "network");
    }
  }

  get sinks() {
    return this._sinks.slice();
  }

  set sinks(value) {
    this._sinks = normalizeSinks(value, this._localPath, this._remotePath);
  }

  at(level) {
    return new LogBuilder(this, level);
  }

  log(level, message) {
    const normalized = normalizeLevel(level);
    if (!this.enabled || LEVELS[normalized] < LEVELS[this._level]) {
      return;
    }
    const rendered = this.render(normalized, stringify(message));
    if (this._sinks.includes("console")) {
      process.stderr.write(`${rendered}\n`);
    }
    if (this._sinks.includes("file") && this._localPath) {
      appendLine(this._localPath, rendered);
    }
    if (this._sinks.includes("network") && this._remotePath && !looksLikeSocket(this._remotePath)) {
      appendLine(this._remotePath, rendered);
    }
  }

  render(level, message) {
    if (this.format === "json") {
      const event = {
        level,
        logger: this.name,
        message,
      };
      if (Object.keys(this.metadata).length > 0) {
        event.metadata = this.metadata;
      }
      if (this.timestamp) {
        event.timestamp = new Date().toISOString();
      }
      return JSON.stringify(sortObject(event));
    }

    const parts = [];
    if (this.timestamp) {
      parts.push(new Date().toISOString());
    }
    if (this.tagLevel) {
      parts.push(LEVEL_LABELS[level]);
    }
    parts.push(message);
    return parts.join(" ");
  }
}

class LogBuilder {
  constructor(logit, level) {
    this.logit = logit;
    this.level = level;
    this.fragments = [];
    this.committed = false;
  }

  append(value) {
    if (!this.committed) {
      this.fragments.push(stringify(value));
    }
    return this;
  }

  commit() {
    if (!this.committed && this.fragments.length > 0) {
      this.committed = true;
      this.logit.log(this.level, this.fragments.join(""));
    }
    return this;
  }
}

function loadLogits(configPath) {
  const payload = JSON.parse(fs.readFileSync(configPath, "utf8"));
  if (!payload.logits) {
    return {
      default: new LOGIT({
        name: "default",
        level: typeof payload.level === "string" ? payload.level : payload.level.threshold,
        tag_level: typeof payload.level === "object" ? payload.level.tag : true,
        timestamp: payload.timestamp,
        path: payload.file_location,
        remotePath: payload.network_file_location,
      }),
    };
  }

  const defaults = payload.defaults || {};
  const result = {};
  for (const [name, config] of Object.entries(payload.logits)) {
    result[name] = new LOGIT({ ...defaults, ...config, name: config.name || name });
  }
  return result;
}

function appendLine(target, line) {
  const parent = path.dirname(target);
  if (parent && parent !== ".") {
    fs.mkdirSync(parent, { recursive: true });
  }
  fs.appendFileSync(target, `${line}\n`, "utf8");
}

function looksLikeSocket(target) {
  return /^tcp:\/\/|^udp:\/\//i.test(target);
}

function stringify(value) {
  if (typeof value === "string") {
    return value;
  }
  if (value === null || value === undefined) {
    return String(value);
  }
  if (typeof value === "object") {
    return JSON.stringify(value);
  }
  return String(value);
}

function sortObject(value) {
  return Object.keys(value)
    .sort()
    .reduce((result, key) => {
      result[key] = value[key];
      return result;
    }, {});
}

module.exports = {
  LOGIT,
  Logit: LOGIT,
  loadLogits,
  levels: {
    TRACE: "trace",
    DEBUG: "debug",
    INFO: "info",
    WARN: "warn",
    ERROR: "error",
    FATAL: "fatal",
  },
};
