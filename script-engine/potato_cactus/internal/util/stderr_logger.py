import sys
from datetime import datetime

class Logger(object):

    def __init__(self, prefix: str) -> None:
        self._prefix = prefix

    def warning(self, msg: str) -> None:
        print(self._format("Warn", msg), file=sys.stderr)

    def error(self, msg: str) -> None:
        print(self._format("Error", msg), file=sys.stderr)

    def info(self, msg: str) -> None:
        print(self._format("Info", msg), file=sys.stderr)

    def _format(self, level: str, msg: str):
        return f"[ScriptingEngine] [{datetime.now().isoformat()}] [{level}] {msg}"
