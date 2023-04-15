import json
import sys
import time

time.sleep(10)
while True:
    value = sys.stdin.readline().rstrip("\n")
    decoded = json.loads(value)
    if decoded["op"] == "doneSendingEvents":
        print(json.dumps({"op": "dummyEvent", "body": {"key": "foobar"}}))
        print(json.dumps({"op": "internalProcessingComplete", "body": {}}))

    # value = input()
    # sys.stdout.writelines([value])
    # print(value)
    if value == "exit":
        break
