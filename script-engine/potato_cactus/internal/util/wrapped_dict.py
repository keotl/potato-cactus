class WrappedDict(object):

    def __init__(self, content: dict):
        self._content = content

    def __getattr__(self, item):
        raw = self._content.get(item)
        if isinstance(raw, dict):
            return WrappedDict(raw)
        if isinstance(raw, list):
            return WrappedList(raw)
        return raw

    def __setitem__(self, __name: str, __value) -> None:
        self._content[__name] = __value


class WrappedList(object):
    def __init__(self, content: list):
        self._content = content

    def __getitem__(self, item):
        raw = self._content[item]
        if isinstance(raw, dict):
            return WrappedDict(raw)
        if isinstance(raw, list):
            return WrappedList(raw)
        return raw
