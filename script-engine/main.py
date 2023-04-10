import sys
import time

# time.sleep(8)
while True:
    value = sys.stdin.readline()
    # value = input()
    sys.stdout.writelines([value])
    # print(value)
    if value == "exit":
        break
