# Arithmetic and logic unit
# Connections: params p, q
# Functions: +, -, shift, not, and, or


class alu:
    def __init__(self, pBus, qBus, funcBus, enable, out):
        self.p = pBus
        self.q = qBus
        self.func = funcBus
        self.enable = enable
        self.out = out

    def add(self):
        return 0

    def subtract(self):
        return 0

    def shift(self):
        return 0   

    def NOT(self):
        return 0
    
    def AND(self):
        return 0
    
    def OR(self):
        return 0