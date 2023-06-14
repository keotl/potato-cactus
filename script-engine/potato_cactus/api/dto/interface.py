from abc import ABC, abstractmethod


class InterfaceElement(ABC):

    @abstractmethod
    def serialize(self) -> dict:
        raise NotImplementedError


class ChatboxRootWindowElement(InterfaceElement):

    def __init__(self, widgetId: int):
        self.widgetId = widgetId

    def serialize(self) -> dict:
        return {"type": "chatboxRoot", "widgetId": self.widgetId}


class TextElement(InterfaceElement):

    def __init__(self, widgetId: int, msg: str):
        self._widgetId = widgetId
        self._msg = msg

    def serialize(self) -> dict:
        return {"type": "text", "widgetId": self._widgetId, "msg": self._msg}


class NpcChatheadElement(InterfaceElement):

    def __init__(self, widgetId: int, npcId: int) -> None:
        self.widgetId = widgetId
        self.npcId = npcId

    def serialize(self) -> dict:
        return {
            "type": "npcChathead",
            "widgetId": self.widgetId,
            "npcId": self.npcId
        }


class PlayerChatheadElement(InterfaceElement):

    def __init__(self, widgetId: int) -> None:
        self.widgetId = widgetId

    def serialize(self) -> dict:
        return {"type": "playerChathead", "widgetId": self.widgetId}


class ModelAnimationElement(InterfaceElement):

    def __init__(self, widgetId: int, animationId: int) -> None:
        self.widgetId = widgetId
        self.animationId = animationId

    def serialize(self) -> dict:
        return {
            "type": "modelAnimation",
            "widgetId": self.widgetId,
            "animationId": self.animationId
        }
