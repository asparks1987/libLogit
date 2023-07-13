import logging

# Define a dictionary to map the log levels from the C++ version to Python version
LOG_LEVELS = {
    'DEBUG': logging.DEBUG,
    'INFO': logging.INFO,
    'WARN': logging.WARNING,
    'ERROR': logging.ERROR
}

class LogConfig:
    def __init__(self, headers=False, level='DEBUG', outputFile='Default.log'):
        self.headers = headers
        self.level = level
        self.outputFile = outputFile


class LOG:
    def __init__(self, log_cfg):
        self.log_cfg = log_cfg
        logging.basicConfig(filename=self.log_cfg.outputFile, 
                            level=LOG_LEVELS[self.log_cfg.level],
                            format='%(asctime)s %(levelname)s: %(message)s',
                            datefmt='%Y/%m/%d %I:%M:%S')

    def log(self, level, msg):
        if LOG_LEVELS[level] >= LOG_LEVELS[self.log_cfg.level]:
            logger = logging.getLogger(__name__)
            if level == 'DEBUG':
                logger.debug(msg)
            elif level == 'INFO':
                logger.info(msg)
            elif level == 'WARN':
                logger.warning(msg)
            elif level == 'ERROR':
                logger.error(msg)
