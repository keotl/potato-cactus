from potato_cactus import EventHandler, GameEvent

@EventHandler(GameEvent.NpcEntityTickEvent)
def onNpcTick(event):
    print("handling NPC event from python script!")
