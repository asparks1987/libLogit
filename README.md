# Python Logging Configuration

This Python script provides a simple logging configuration with configurable options. It includes a class `LOG` that facilitates logging messages with different log levels, and a class `LogConfig` to set up the logging configuration.

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Configuration](#configuration)
- [Examples](#examples)

## Installation

No specific installation is required. Simply include the provided script in your project, and you can start using the logging functionality.

## Usage

To use the logging functionality in your Python script, follow these steps:

1. Import the script:

    from logit import LOG, LogConfig
    ```

2. Create a `LogConfig` instance with desired configurations:

    log_config = LogConfig(headers=True, level='INFO', outputFile='MyLog.log')
    ```

3. Create a `LOG` instance with the `LogConfig`:

    ```python
    logger = LOG(log_config)
    ```

4. Log messages using the `log` method:

    ```python
    logger.log('INFO', 'This is an informational message.')
    ```

## Configuration

### LogConfig Class

The `LogConfig` class is used to configure the logging settings. It has the following parameters:

- `headers`: Boolean (default: `False`) - Whether to include headers in the log messages.
- `level`: String (default: `'DEBUG'`) - The logging level. Valid values are `'DEBUG'`, `'INFO'`, `'WARN'`, and `'ERROR'`.
- `outputFile`: String (default: `'Default.log'`) - The name of the log file where the messages will be stored.

### LOG Class

The `LOG` class is responsible for initializing the logging system and providing the `log` method to log messages. It takes a `LogConfig` instance as its configuration.

## Examples

### Example 1: Basic Logging

```python
from logger import LOG, LogConfig

# Configure logging with default settings
log_config = LogConfig()
logger = LOG(log_config)

# Log a message
logger.log('INFO', 'This is an informational message.')
Example 2: Custom Configuration
python
Copy code
from logger import LOG, LogConfig

# Configure logging with custom settings
log_config = LogConfig(headers=True, level='WARN', outputFile='CustomLog.log')
logger = LOG(log_config)

# Log a warning message
logger.log('WARN', 'This is a warning message.')
Feel free to customize the LogConfig parameters to suit your project's logging requirements.