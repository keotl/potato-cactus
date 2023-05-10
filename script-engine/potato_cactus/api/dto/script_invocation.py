from typing import Callable, Iterable, Optional, Tuple, Union


class ScriptInvocation(object):

    def __init__(self,
                 f: Union[Callable, Tuple[str, str]],
                 args: Optional[Iterable[Union[str, int]]] = None) -> None:
        if isinstance(f, Callable):
            self.f = f"{f.__module__}.{f.__name__}"
        else:
            self.f = ".".join(f)
        self.args = args or tuple()
