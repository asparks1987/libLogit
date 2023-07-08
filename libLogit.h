#ifndef LIBLOG_H
#define LIBLOG_H

#include <string>
#include <fstream>
#include <ctime>
#include <sstream>
#include <stdexcept>

#ifndef SSTR
#define SSTR( x ) static_cast< std::ostringstream & >(std::ostringstream() << std::dec << x).str()
#endif // SSTR

#ifndef ENDLINE
#define ENDLINE "\r\n"
#endif // ENDLINE

enum class LogLevel
{
    DEBUG,
    INFO,
    WARN,
    ERROR
};

struct LogConfig
{
    bool headers = false;
    LogLevel level = LogLevel::DEBUG;
    std::string outputFile = "Default.log";
};

extern LogConfig LOGCFG;

class LOG
{
public:
    /**
     * @brief Constructs a LOG instance with a custom log file name.
     * @param fileName The name of the log file.
     * @throws std::runtime_error if unable to open the log file.
     */
    LOG(const std::string& fileName);

    /**
     * @brief Constructs a LOG instance with a specified log level.
     * @param level The log level for this instance.
     * @throws std::runtime_error if unable to open the log file.
     */
    LOG(LogLevel level = LogLevel::DEBUG);

    /**
     * @brief Appends the provided message to the log.
     * @tparam T The type of the message.
     * @param msg The message to be logged.
     * @return A reference to the current LOG instance.
     * @throws std::runtime_error if unable to open the log file.
     */
    template<class T>
    LOG& operator<<(const T& msg)
    {
        if (msglevel >= LOGCFG.level)
        {
            File.open(LOGCFG.outputFile, std::ofstream::app);
            if (!File.is_open())
            {
                throw std::runtime_error("Failed to open the log file.");
            }
            File << msg;
            File.close();
        }
        return *this;
    }

    /**
     * @brief Destroys the LOG instance and closes the log file.
     */
    ~LOG();

private:
    LogLevel msglevel = LogLevel::DEBUG;
    inline std::string getLabel(LogLevel level);
    std::string errorcode = "";
    bool newLog = false;
    std::string currentDateTime();
    std::ofstream File;
};

#endif // LIBLOG_H
