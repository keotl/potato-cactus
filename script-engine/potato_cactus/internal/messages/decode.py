import json
from potato_cactus.internal.messages.inbound import InboundMessage

def decode_inbound(msg: str) -> InboundMessage:
    return InboundMessage(json.loads(msg))
