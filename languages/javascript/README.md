# @liblogit/liblogit

Alpha Node.js binding for the libLogit `LOGIT` object model.

```javascript
const { LOGIT, levels } = require("@liblogit/liblogit");

const LogIT = new LOGIT();
LogIT.localPath = "logs/javascript-app.log";
LogIT.level = levels.DEBUG;

LogIT.at(levels.INFO).append("JavaScript app started").commit();
```

This package is part of the libLogit v1 Alpha SDK. It supports direct `LOGIT`
objects, config-loaded named loggers, console/file output, text output,
JSON-lines output, and shared conformance fixtures. Advanced runtime behavior
such as async buffering, redaction, retry policy, and real remote transports is
beta-track.
