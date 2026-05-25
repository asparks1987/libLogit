"""Minimal SQLite log viewer for the libLogit alpha showcase."""

from __future__ import annotations

import argparse
import sqlite3
from pathlib import Path
from typing import Any, Dict, Iterable, Optional


EVENT_COLUMNS = (
    "id",
    "created_at",
    "sequence",
    "logger",
    "level",
    "message",
    "format",
    "metadata_json",
    "source",
)


def load_events(
    database_path: str | Path,
    *,
    logger: Optional[str] = None,
    level: Optional[str] = None,
    search: Optional[str] = None,
    since: Optional[str] = None,
    until: Optional[str] = None,
    limit: int = 200,
) -> list[Dict[str, Any]]:
    """Load recent events from a libLogit SQLite store.

    ``since`` and ``until`` compare against ISO-compatible ``created_at`` text.
    """

    if limit <= 0:
        raise ValueError("limit must be positive")

    clauses: list[str] = []
    params: list[Any] = []
    if logger:
        clauses.append("logger = ?")
        params.append(logger)
    if level:
        clauses.append("level = ?")
        params.append(level.lower())
    if search:
        clauses.append("message LIKE ?")
        params.append(f"%{search}%")
    if since:
        clauses.append("created_at >= ?")
        params.append(since)
    if until:
        clauses.append("created_at <= ?")
        params.append(until)

    where = f"WHERE {' AND '.join(clauses)}" if clauses else ""
    query = f"""
        SELECT {', '.join(EVENT_COLUMNS)}
        FROM logit_events
        {where}
        ORDER BY sequence DESC
        LIMIT ?
    """
    params.append(limit)

    with sqlite3.connect(database_path) as connection:
        connection.row_factory = sqlite3.Row
        rows = connection.execute(query, params).fetchall()
    return [dict(row) for row in rows]


def launch_viewer(database_path: str | Path) -> None:
    """Open a small desktop window for browsing a libLogit SQLite store."""

    import tkinter as tk
    from tkinter import filedialog, ttk

    root = tk.Tk()
    root.title("libLogit Viewer")
    root.geometry("980x560")

    selected_path = tk.StringVar(value=str(database_path))
    logger_filter = tk.StringVar()
    level_filter = tk.StringVar()
    search_filter = tk.StringVar()
    since_filter = tk.StringVar()
    until_filter = tk.StringVar()
    status = tk.StringVar(value="Ready")

    def refresh() -> None:
        for item in table.get_children():
            table.delete(item)
        try:
            events = load_events(
                selected_path.get(),
                logger=logger_filter.get().strip() or None,
                level=level_filter.get().strip() or None,
                search=search_filter.get().strip() or None,
                since=since_filter.get().strip() or None,
                until=until_filter.get().strip() or None,
                limit=500,
            )
        except Exception as exc:  # pragma: no cover - UI feedback path
            status.set(f"Unable to load store: {exc}")
            return
        for event in events:
            table.insert(
                "",
                "end",
                values=(
                    event["created_at"],
                    event["logger"],
                    event["level"],
                    event["message"],
                    event.get("metadata_json") or "",
                ),
            )
        status.set(f"Loaded {len(events)} events")

    def choose_file() -> None:
        path = filedialog.askopenfilename(
            title="Open libLogit SQLite store",
            filetypes=[("SQLite databases", "*.sqlite *.db"), ("All files", "*.*")],
        )
        if path:
            selected_path.set(path)
            refresh()

    toolbar = ttk.Frame(root, padding=8)
    toolbar.pack(fill="x")

    ttk.Label(toolbar, text="Store").grid(row=0, column=0, sticky="w")
    ttk.Entry(toolbar, textvariable=selected_path, width=56).grid(row=0, column=1, sticky="ew", padx=4)
    ttk.Button(toolbar, text="Open", command=choose_file).grid(row=0, column=2, padx=4)
    ttk.Button(toolbar, text="Refresh", command=refresh).grid(row=0, column=3, padx=4)

    filters = ttk.Frame(root, padding=(8, 0, 8, 8))
    filters.pack(fill="x")

    ttk.Label(filters, text="Logger").grid(row=0, column=0, sticky="w")
    ttk.Entry(filters, textvariable=logger_filter, width=18).grid(row=0, column=1, sticky="ew", padx=(4, 12))
    ttk.Label(filters, text="Level").grid(row=0, column=2, sticky="w")
    ttk.Combobox(
        filters,
        textvariable=level_filter,
        values=("", "trace", "debug", "info", "warn", "error", "fatal"),
        width=10,
    ).grid(row=0, column=3, sticky="ew", padx=(4, 12))
    ttk.Label(filters, text="Search").grid(row=0, column=4, sticky="w")
    ttk.Entry(filters, textvariable=search_filter, width=24).grid(row=0, column=5, sticky="ew", padx=(4, 12))
    ttk.Label(filters, text="Since").grid(row=0, column=6, sticky="w")
    ttk.Entry(filters, textvariable=since_filter, width=19).grid(row=0, column=7, sticky="ew", padx=(4, 12))
    ttk.Label(filters, text="Until").grid(row=0, column=8, sticky="w")
    ttk.Entry(filters, textvariable=until_filter, width=19).grid(row=0, column=9, sticky="ew", padx=(4, 0))
    for filter_column in (1, 5, 7, 9):
        filters.columnconfigure(filter_column, weight=1)
    toolbar.columnconfigure(1, weight=1)
    toolbar.columnconfigure(3, weight=1)

    columns = ("created_at", "logger", "level", "message", "metadata")
    table = ttk.Treeview(root, columns=columns, show="headings")
    for column, label, width in (
        ("created_at", "Created", 190),
        ("logger", "Logger", 130),
        ("level", "Level", 80),
        ("message", "Message", 400),
        ("metadata", "Metadata", 220),
    ):
        table.heading(column, text=label)
        table.column(column, width=width, anchor="w")
    table.pack(fill="both", expand=True, padx=8, pady=8)

    ttk.Label(root, textvariable=status, padding=(8, 0, 8, 8)).pack(fill="x")
    refresh()
    root.mainloop()


def print_events(events: Iterable[Dict[str, Any]]) -> None:
    """Print events for quick command-line inspection."""

    for event in events:
        print(f"{event['created_at']} {event['logger']} {event['level']} {event['message']}")


def main(argv: Optional[list[str]] = None) -> int:
    parser = argparse.ArgumentParser(description="Open or query a libLogit SQLite log store.")
    parser.add_argument("database", help="Path to a libLogit SQLite store")
    parser.add_argument("--logger", help="Filter by LOGIT name")
    parser.add_argument("--level", help="Filter by level")
    parser.add_argument("--search", help="Filter by message substring")
    parser.add_argument("--since", help="Only include rows with created_at greater than or equal to this ISO-compatible value")
    parser.add_argument("--until", help="Only include rows with created_at less than or equal to this ISO-compatible value")
    parser.add_argument("--limit", type=int, default=200, help="Maximum rows to load")
    parser.add_argument("--print", action="store_true", dest="print_rows", help="Print rows instead of opening the UI")
    args = parser.parse_args(argv)

    if args.print_rows:
        print_events(
            load_events(
                args.database,
                logger=args.logger,
                level=args.level,
                search=args.search,
                since=args.since,
                until=args.until,
                limit=args.limit,
            )
        )
        return 0

    launch_viewer(args.database)
    return 0


if __name__ == "__main__":  # pragma: no cover - module entrypoint
    raise SystemExit(main())
