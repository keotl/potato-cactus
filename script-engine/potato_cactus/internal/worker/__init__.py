from abc import ABC
from potato_cactus.internal.messages.inbound import InboundMessage


class WorkerHandle(ABC):

    def dispatch(self, message: InboundMessage):
        raise NotImplementedError

class OutboundMessageSender(ABC):

    def send(self, message: dict):
        raise NotImplementedError
