from typing import Callable, Iterable, Optional, Tuple, Union


class ScriptInvocation(object):

    def __init__(self,
                 f: Union[Callable, Tuple[str, str], str],
                 args: Optional[Iterable[Union[str, int]]] = None) -> None:
        if isinstance(f, Callable):
            self.f = f"{f.__module__}.{f.__name__}"
        elif isinstance(f, tuple):
            self.f = ".".join(f)
        else:
            self.f = f
        self.args = args or tuple()
