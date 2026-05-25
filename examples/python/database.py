import sqlite3
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from liblogit import INFO, LOGIT, load_logits


def direct_database_logit() -> None:
    logit = LOGIT(
        {
            "name": "DatabaseLog",
            "databasePath": "logs/python-database.sqlite",
            "level": "debug",
            "sinks": ["console", "database"],
            "retention": {"mode": "records", "maxRecords": 1000},
            "metadata": {"component": "example"},
        }
    )

    logit.log(INFO, "database-backed log event")


def config_database_logit() -> None:
    logs = load_logits("examples/config/v2-database.json")
    logs["AppLog"].log(INFO, "configured database log event")


def show_recent_rows() -> None:
    with sqlite3.connect("logs/app-logit.sqlite") as connection:
        rows = connection.execute(
            """
            SELECT created_at, logger, level, message
            FROM logit_events
            ORDER BY sequence DESC
            LIMIT 5
            """
        ).fetchall()

    for row in rows:
        print(row)


if __name__ == "__main__":
    direct_database_logit()
    config_database_logit()
    show_recent_rows()
