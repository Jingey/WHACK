from wires import Wire, Bus

# Arithmetic and logic unit
# Connections: params p, q
# Functions: +, -, shift, not, and, or


class Alu:
    def __init__(
        self, p_bus: Bus, q_bus: Bus, func_bus: Bus, enable: Wire, out_bus: Bus
    ):
        self.p = p_bus
        self.q = q_bus
        self.func = func_bus
        self.enable = enable
        self.enable.enlist(lambda data: self.execute())
        self.out = out_bus

    def execute(self):
        match self.func.data:
            case 0:
                self.add()
            case 1:
                self.subtract()
            case 2:
                self.shift()
            case 3:
                self.NOT()
            case 4:
                self.AND()
            case 5:
                self.OR()
            case _:
                self.NOP()

    def add(self):
        self.out.set(self.p.data + self.q.data)

    def subtract(self):
        self.out.set(self.p.data - self.q.data)

    def shift(self):
        self.out.set(self.p.data >> self.q.data)

    def NOT(self):
        self.out.set(0b11111111_11111111 - self.p.data)

    def AND(self):
        self.out.set(self.p.data & self.q.data)

    def OR(self):
        self.out.set(self.p.data | self.q.data)

    def NOP(self):
        return
