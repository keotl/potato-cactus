import random
from abc import ABC, abstractmethod
from typing import Callable, Dict, List, Literal, Optional, Tuple, Union

from potato_cactus import get_context
from potato_cactus.api.actions import (ClearPlayerInteraction, CreateInterface,
                                       ScriptAction, ScriptInvocation)
from potato_cactus.api.dto.interface import (ChatboxRootWindowElement,
                                             ModelAnimationElement,
                                             NpcChatheadElement,
                                             PlayerChatheadElement,
                                             TextElement)
from potato_cactus.api.exception import ScriptException
from potato_cactus.internal.util.script_invoker import invoke_script


class DialogueScreen(ABC):

    @abstractmethod
    def configure(self, playerIndex: int,
                  onNext: Optional[ScriptInvocation]) -> List[ScriptAction]:
        raise NotImplementedError

    def onContinue(self) -> Optional[ScriptInvocation]:
        return None


class DialogueNodeRef(object):
    """Describes a callable with the playerIndex as the sole argument.
       Use to chain dialogue nodes or circumvent circular reference between dialogue nodes."""

    def __init__(
        self, f: Union[Callable[[int], List[ScriptAction]],
                       Tuple[str, str]]) -> None:
        self.f = f


class DialogueNode(object):

    def __init__(self,
                 module_name: str,
                 variable_name: str,
                 screens: Optional[List[DialogueScreen]] = None):
        self._screens: List[DialogueScreen] = screens or []
        self._script_ref = (module_name, variable_name)

    def add(self, screen: DialogueScreen) -> "DialogueNode":
        self._screens.append(screen)
        return self

    @property
    def ref(self) -> DialogueNodeRef:
        return DialogueNodeRef(self._script_ref)

    def __call__(self, playerIndex: int, stage: int = 0) -> List[ScriptAction]:
        actions: List[ScriptAction] = []
        if 0 < stage < len(self._screens) + 1:
            prev_screen = self._screens[stage - 1]
            callback = prev_screen.onContinue()
            if callback:
                actions.extend(invoke_script(callback))
        if -1 < stage < len(self._screens):
            actions.extend(self._screens[stage].configure(
                playerIndex,
                ScriptInvocation(self._script_ref, (playerIndex, stage + 1))))
        return actions


Expression = Literal["default", "skeptical", "angry", "worried", "sleepy",
                     "laughing", "sad", "angry_silent", "angry_laughing"]


class NpcDialogueScreen(DialogueScreen):

    def __init__(self,
                 npcId: int,
                 npcName: str,
                 text: List[str],
                 expression: Expression = "default",
                 onContinue: Optional[ScriptInvocation] = None):
        self._npcId = npcId
        self._npcName = npcName
        self._text = text
        self._expression: Expression = expression
        self._onContinue = onContinue

    def configure(self, playerIndex: int,
                  onNext: Optional[ScriptInvocation]) -> List[ScriptAction]:
        return [
            CreateInterface(
                playerIndex,
                "standard",
                [
                    NpcChatheadElement(self._root_window_id + 1, self._npcId),
                    ModelAnimationElement(
                        self._root_window_id + 1,
                        _expression_animation_id(self._expression)),
                    TextElement(self._root_window_id + 2, self._npcName),
                ] + [
                    TextElement(self._root_window_id + 3 + i, l)
                    for i, l in enumerate(self._text)
                ] + [ChatboxRootWindowElement(self._root_window_id)],
                onClose=onNext),
        ]

    def onContinue(self) -> Optional[ScriptInvocation]:
        return self._onContinue

    @property
    def _root_window_id(self) -> int:
        try:
            return _NPC_ROOT_WINDOW_ID[len(self._text)]
        except KeyError:
            raise ScriptException(
                f"Unsupported number of lines '{len(self._text)}' when configuring dialogue."
            )


_NPC_ROOT_WINDOW_ID = {0: 4882, 1: 4882, 2: 4887, 3: 4893, 4: 4900}


class PlayerDialogueScreen(DialogueScreen):

    def __init__(self,
                 text: List[str],
                 expression: Expression = "default",
                 onContinue: Optional[ScriptInvocation] = None,
                 playerNameOverride: Optional[str] = None):
        self._text = text
        self._expression: Expression = expression
        self._onContinue = onContinue
        self._playerNameOverride = playerNameOverride

    def configure(self, playerIndex: int,
                  onNext: Optional[ScriptInvocation]) -> List[ScriptAction]:
        return [
            CreateInterface(
                playerIndex,
                "standard",
                [
                    PlayerChatheadElement(self._root_window_id + 1),
                    ModelAnimationElement(
                        self._root_window_id + 1,
                        _expression_animation_id(self._expression)),
                    TextElement(self._root_window_id + 2,
                                self._player_name(playerIndex)),
                ] + [
                    TextElement(self._root_window_id + 3 + i, l)
                    for i, l in enumerate(self._text)
                ] + [ChatboxRootWindowElement(self._root_window_id)],
                onClose=onNext)
        ]

    def onContinue(self) -> Optional[ScriptInvocation]:
        return self._onContinue

    def _player_name(self, playerIndex: int) -> str:
        if self._playerNameOverride is not None:
            return self._playerNameOverride
        player = get_context().find_player_by_index(playerIndex)
        if player is not None:
            return player.username

        return "Unknown"

    @property
    def _root_window_id(self) -> int:
        try:
            return _PLAYER_ROOT_WINDOW_ID[len(self._text)]
        except KeyError:
            raise ScriptException(
                f"Unsupported number of lines '{len(self._text)}' when configuring dialogue."
            )


_PLAYER_ROOT_WINDOW_ID = {0: 968, 1: 968, 2: 973, 3: 979, 4: 986}


def _expression_animation_id(expression: Expression) -> int:
    try:
        return random.choice(_EXPRESSION_ANIMATION_IDS[expression])
    except KeyError:
        raise ScriptException(
            f"Unknown expression type '{expression}' when configuring dialogue."
        )


_EXPRESSION_ANIMATION_IDS: Dict[Expression, List[int]] = {
    "skeptical": [588, 589, 590, 591],
    "angry": [592, 593, 594, 595],
    "worried": [596, 597, 598, 599],
    "sleepy": [600, 601, 602, 603],
    "laughing": [605, 606, 607, 608],
    "sad": [610, 611, 612, 613],
    "default": [591],
    "angry_silent": [604],
    "angry_laughing": [609]
}


class OptionsDialogueScreen(DialogueScreen):

    def __init__(self, choices: List[Tuple[str, Union[Callable[[], None],
                                                      ScriptInvocation,
                                                      DialogueNodeRef]]]):
        self._choices = choices

    def configure(self, playerIndex: int,
                  onNext: Optional[ScriptInvocation]) -> List[ScriptAction]:
        return [
            CreateInterface(
                playerIndex,
                "standard",
                [
                    TextElement(self._root_window_id + 2 + i, l)
                    for i, (l, _) in enumerate(self._choices)
                ] + [ChatboxRootWindowElement(self._root_window_id)
                     ],  # type: ignore
                onClose=onNext,
                callbacks=self._callbacks(playerIndex))
        ]

    @property
    def _root_window_id(self):
        try:
            return _OPTIONS_ROOT_WINDOW_ID[len(self._choices)]
        except KeyError:
            raise ScriptException(
                f"Unknown number of dialogue options '{len(self._choices)}'.")

    def _callbacks(self, playerIndex: int):
        try:
            button_ids = _OPTIONS_BUTTON_IDS[len(self._choices)]
            return {
                k: self._callback(playerIndex, script_ref)
                for k, (_, script_ref) in zip(button_ids, self._choices)
            }
        except KeyError:
            raise ScriptException(
                f"Unknown number of dialogue options '{len(self._choices)}.'")

    def _callback(
        self, playerIndex: int, callbackRef: Union[Callable[[], None],
                                                   ScriptInvocation,
                                                   DialogueNodeRef]
    ) -> ScriptInvocation:
        if isinstance(callbackRef, ScriptInvocation):
            return callbackRef
        if isinstance(callbackRef, DialogueNodeRef):
            return ScriptInvocation(callbackRef.f, (playerIndex, ))
        return ScriptInvocation(callbackRef)


_OPTIONS_ROOT_WINDOW_ID = {2: 14443, 3: 2469, 4: 8207, 5: 8219}
_OPTIONS_BUTTON_IDS = {
    2: [14445, 14446],
    3: [2471, 2472, 2473],
    4: [8209, 8210, 8211, 8212],
    5: [8221, 8222, 8223, 8224, 8225]
}


def start_dialogue(node_ref: DialogueNodeRef,
                   playerIndex: int) -> List[ScriptAction]:
    return [
        *invoke_script(ScriptInvocation(node_ref.f, (playerIndex, ))),
        ClearPlayerInteraction(playerIndex)
    ]
