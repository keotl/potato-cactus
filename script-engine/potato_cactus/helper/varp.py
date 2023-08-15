from potato_cactus import get_context
from potato_cactus.api.actions import ScriptAction, SetVarbit, SetVarp


class Varp(object):

    def __init__(self, varpId: int, default_value: int = 0):
        self._varpId = varpId
        self._default_value = default_value

    def set(self, playerIndex: int, value: int) -> ScriptAction:
        return SetVarp(playerIndex, self._varpId, value)

    def get(self, playerIndex: int) -> int:
        player = get_context().find_player_by_index(playerIndex)
        if player:
            player.varps.get(str(self._varpId), self._default_value)
        return self._default_value


class Varbit(object):

    def __init__(self,
                 varpId: int,
                 lsb: int,
                 length: int,
                 default_value: int = 0):
        self._varpId = varpId
        self._lsb = lsb
        self._length = length
        self._default_value = default_value

    def set(self, playerIndex: int, value: int) -> ScriptAction:
        return SetVarbit(playerIndex, self._varpId, self._lsb, self._length,
                         value)

    def get(self, playerIndex: int) -> int:
        player = get_context().find_player_by_index(playerIndex)
        if not player:
            return self._default_value

        varp_value = player.varps.get(str(self._varpId), self._default_value)
        mask = ((1 << self._length) - 1) << self._lsb
        return (varp_value & mask) >> self._lsb
