module PotatoCactus.Client.Interface.EncodeInterfaceUpdate (encodeInterfaceUpdate) where

import Data.ByteString (ByteString, concat, empty)
import Data.Maybe (catMaybes)
import PotatoCactus.Game.Interface.InterfaceController (Interface (Interface, configuredElements), InterfaceController (inputInterface, mainInterface, shouldCloseInterfaces, walkableInterface))
import PotatoCactus.Game.Scripting.Actions.CreateInterface (InterfaceElement (ChatboxRootWindowElement, ModelAnimationElement, NpcChatheadElement, PlayerChatheadElement, TextElement))
import PotatoCactus.Network.Packets.Out.ChatboxInterfacePacket (chatboxInterfacePacket)
import PotatoCactus.Network.Packets.Out.CloseInterfacesPacket (closeInterfacesPacket)
import PotatoCactus.Network.Packets.Out.InterfaceAnimationPacket (interfaceAnimationPacket)
import PotatoCactus.Network.Packets.Out.InterfaceChatheadPacket (interfaceNpcChatheadPacket, interfacePlayerChatheadPacket)
import PotatoCactus.Network.Packets.Out.InterfaceTextPacket (interfaceTextPacket)

encodeInterfaceUpdate :: InterfaceController -> ByteString
encodeInterfaceUpdate c =
  if shouldCloseInterfaces c
    then closeInterfacesPacket
    else
      Data.ByteString.concat $
        map encodeInterface_ $
          catMaybes
            [ mainInterface c,
              walkableInterface c,
              inputInterface c
            ]

encodeInterface_ :: Interface -> ByteString
encodeInterface_ Interface {configuredElements = elements} =
  Data.ByteString.concat $ map encodeInterfaceElement_ elements

encodeInterfaceElement_ :: InterfaceElement -> ByteString
encodeInterfaceElement_ (ChatboxRootWindowElement widgetId) =
  chatboxInterfacePacket . fromIntegral $ widgetId
encodeInterfaceElement_ (TextElement widgetId text) =
  interfaceTextPacket (fromIntegral widgetId) text
encodeInterfaceElement_ (NpcChatheadElement widgetId npcId) =
  interfaceNpcChatheadPacket (fromIntegral widgetId) npcId
encodeInterfaceElement_ (PlayerChatheadElement widgetId) =
  interfacePlayerChatheadPacket (fromIntegral widgetId)
encodeInterfaceElement_ (ModelAnimationElement widgetId animationId) =
  interfaceAnimationPacket (fromIntegral widgetId) (fromIntegral animationId)
