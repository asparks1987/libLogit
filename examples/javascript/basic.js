"use strict";

const { LOGIT, loadLogits, levels } = require("../../languages/javascript/src/liblogit");

const LogIT = new LOGIT();
LogIT.localPath = "logs/javascript-app.log";
LogIT.level = levels.DEBUG;

LogIT.at(levels.INFO).append("JavaScript app started").commit();

const configured = loadLogits("examples/config/v2-basic.json");
configured.AppLog.at(levels.INFO).append("configured JavaScript log").commit();
