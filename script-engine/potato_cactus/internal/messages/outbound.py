def _outboundMessage(op: str, body: dict):
    return {
        "op": op,
        "body" : body
    }

def internal_processingComplete():
    return _outboundMessage("internal_processingComplete", {})


def dummyEvent(message: str):
    return _outboundMessage("dummyEvent", {"key": message})
