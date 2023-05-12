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

    def __getitem__(self, key: str):
        return self._content[key]

    def get(self, *args, **kwargs):
        return self._content.get(*args, **kwargs)

    def __setitem__(self, __name: str, __value) -> None:
        self._content[__name] = __value

    def __repr__(self):
        return repr(self._content)

    def __str__(self):
        return str(self._content)


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

    def __len__(self):
        return len(self._content)

    def __repr__(self):
        return repr(self._content)

    def __str__(self):
        return str(self._content)
