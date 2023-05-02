from typing import Iterable, Union


class ScriptInvocation(object):
    def __init__(self, f, args: Iterable[Union[str, int]] = None) -> None:
        self.f = f
        self.args = args or []
