import importlib
from typing import List

from potato_cactus.api.actions import ScriptAction
from potato_cactus.api.dto.script_invocation import ScriptInvocation
from potato_cactus.internal.util.stderr_logger import Logger


def invoke_script(script: ScriptInvocation) -> List[ScriptAction]:
    try:
        modulename, function = script.f.rsplit(".", 1)
        module = importlib.import_module(modulename)
        res = getattr(module, function)(*script.args)
        return res
    except KeyboardInterrupt as e:
        raise e
    except Exception as e:
        _logger.warning(
            f"Unhandled exception while invoking script '{script.f}'. {e}")
    return []


_logger = Logger("script_invoker")
