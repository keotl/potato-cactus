import sys
import time
from potato_cactus.internal.messages.dispatcher import Dispatcher
from potato_cactus.internal.messages.decode import decode_inbound


dispatcher = Dispatcher()
time.sleep(10)
while True:
    msg = sys.stdin.readline().rstrip("\n")
    decoded = decode_inbound(msg)
    dispatcher.dispatch(decoded)
    # if decoded["op"] == "doneSendingEvents":
    #     print(json.dumps({"op": "dummyEvent", "body": {"key": "foobar"}}))
    #     print(json.dumps({"op": "internalProcessingComplete", "body": {}}))

    # value = input()
    # sys.stdout.writelines([value])
    # print(value)
    # if value == "exit":
    #     break
