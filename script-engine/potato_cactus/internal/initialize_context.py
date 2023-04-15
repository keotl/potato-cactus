import potato_cactus
from potato_cactus.internal.impl.context_impl import ContextImpl

def initialize_context():
    potato_cactus.context = ContextImpl()
