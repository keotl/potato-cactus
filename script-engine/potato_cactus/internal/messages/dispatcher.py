import json
from typing import Optional
from potato_cactus.internal.util.stderr_logger import Logger
from potato_cactus.internal.messages.inbound import InboundMessage, InitializeMessageBody
from potato_cactus.internal.worker.simple_worker import SimpleWorker
from potato_cactus.internal.worker import OutboundMessageSender, WorkerHandle


class Dispatcher(OutboundMessageSender):

    def __init__(self):
        self._worker: Optional[WorkerHandle] = None
        self._logger = Logger(self.__class__.__name__)

    def _initialize(self, options: InitializeMessageBody):
        if self._worker is not None:
            self._logger.warning("Tried to initialize an already initialized worker handle! Ignoring...")
            return

        if options.workers == 1:
            self._worker = SimpleWorker(self, options.scriptPaths)
        else:
            self._worker = SimpleWorker(self, options.scriptPaths)
            self._logger.error("Multi-process worker handler not implemented. Using SimpleWorker instead.")

    def send(self, message: dict):
        print(json.dumps(message))

    def dispatch(self, message: InboundMessage):
        if message.op == "initialize":
            self._initialize(message.body)

        if self._worker is None:
            return

        self._worker.dispatch(message)
