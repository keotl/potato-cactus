import json

def dispatch_message(msg: str):
    decoded = json.loads(msg)

    if decoded["op"] == "initialize":
        
