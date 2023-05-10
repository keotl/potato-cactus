from typing import Iterable, Optional, Union


class ScriptInvocation(object):

    def __init__(self,
                 f,
                 args: Optional[Iterable[Union[str, int]]] = None) -> None:
        self.f = f"{f.__module__}.{f.__name__}"
        self.args = args or tuple()
