from typing import Generic, List, TypeVar

from potato_cactus.internal.util.wrapped_dict import WrappedDict

T = TypeVar("T")


class InboundMessage(Generic[T]):
    def __init__(self, content: dict):
        self._content = content

    @property
    def op(self) -> str:
        return self._content["op"]

    @property
    def body(self) -> T:
        return WrappedDict(self._content["body"])


class InitializeMessageBody(object):
    workers: int
    scriptPaths: List[str]


class InitializeMessage(InboundMessage[InitializeMessageBody]):
    pass


class DoneSendingEventsMessage(InboundMessage[dict]):
    pass


