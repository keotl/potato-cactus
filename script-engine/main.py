import sys
from potato_cactus.internal.messages.dispatcher import Dispatcher
from potato_cactus.internal.messages.decode import decode_inbound


dispatcher = Dispatcher()
# time.sleep(10)
try:
    while True:
        msg = sys.stdin.readline().rstrip("\n")
        decoded = decode_inbound(msg)
        dispatcher.dispatch(decoded)

except KeyboardInterrupt:
    # Suppress error message
    sys.exit(1)
