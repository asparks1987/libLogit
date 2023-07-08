# libLogit

libLogit is a lightweight C++ logging library that allows you to easily log messages to a file with different log levels. It provides a flexible and convenient way to manage logs in your applications.

## Features

- Four log levels: DEBUG, INFO, WARN, ERROR
- Customizable log configuration
- Easy-to-use interface for logging messages
- Robust error handling

## Getting Started

### Prerequisites

- C++11 or above
- A C++ compiler

### Usage

1. Include the `liblog.h` header file in your C++ project:

```cpp
#include "liblog.h"
To configure the log settings, modify the LOGCFG instance of the LogConfig struct:
LOGCFG.level = LogLevel::DEBUG;              // Set the desired log level
LOGCFG.outputFile = "application.log";       // Set the log file name
LOGCFG.headers = true;                       // Enable or disable headers in the log file
Instantiate a LOG object to start logging:
LOG logInstance("mylog.log");                 // Create a LOG instance with a custom log file name
logInstance << "This is a log message.";      // Log a message using the default log level (DEBUG)
Alternatively, you can specify the log level when instantiating a LOG object:
LOG logInstance(LogLevel::INFO);              // Create a LOG instance with a specific log level
logInstance << "This is an informational message.";  // Log an INFO level message
Log Levels
libLogit supports the following log levels:

DEBUG: Detailed debug information.
INFO: General information about the application's execution.
WARN: Warnings that may indicate potential issues.
ERROR: Error messages indicating critical errors in the application.
By default, the log level is set to DEBUG. You can modify the log level in the LOGCFG instance to control the verbosity of the logs.

Error Handling
The library provides robust error handling. The LOG constructor and operator<< overload may throw a std::runtime_error if they encounter an error while opening the log file. Make sure to handle exceptions appropriately in your code.

Contributing
Contributions to libLogit are welcome! If you find any issues or have suggestions for improvements, feel free to open an issue or submit a pull request.

License
libLogit is released under the MIT License. See the LICENSE file for details.