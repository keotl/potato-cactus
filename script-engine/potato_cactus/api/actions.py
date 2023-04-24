class ScriptAction(object):
    op: str
    body: dict

    def __init__(self, op, body):
        self.op = op
        self.body = body


def ClearPlayerInteraction(playerIndex: int) -> ScriptAction:
    return ScriptAction("clearPlayerInteraction", {"playerIndex": playerIndex})


def DummyEvent(key: str) -> ScriptAction:
    return ScriptAction("dummyEvent", {"key": key})
