from wires import Wire, Bus

# Arithmetic and logic unit
# Connections: params p, q
# Functions: +, -, shift, not, and, or


MAX_VAL = 32_767
MIN_VAL = -32_768


class Alu:
    def __init__(
        self, p_bus: Bus, q_bus: Bus, func_bus: Bus, enable: Wire, out_bus: Bus
    ):
        self.p = p_bus
        self.q = q_bus
        self.func = func_bus
        self.enable = enable
        self.enable.enlist(self.execute)
        self.out = out_bus
        self.ccr = Bus()

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

    # ZCN in binary
    def set_ccr(self, zero, carry, neg):
        self.ccr.set_data(zero << 2 | carry << 1 | neg)

    def truncate_result(self, result):
        if result & (1 << 15) == 0:
            return result % MAX_VAL
        return -(~(result & ((1 << 15) - 1)) + 1)

    def set_result(self, result):
        carry = result > MAX_VAL or result < MIN_VAL
        result = self.truncate_result(result)

        zero = result == 0
        neg = result < 0

        self.set_ccr(zero, carry, neg)

        self.out.set_data(result)

    def add(self):
        self.set_result(self.p.data + self.q.data)

    def subtract(self):
        self.out.set_data(self.p.data - self.q.data)

    def shift(self):
        self.out.set_data(self.p.data >> self.q.data)

    def NOT(self):
        self.out.set_data(0b11111111_11111111 - self.p.data)

    def AND(self):
        self.out.set_data(self.p.data & self.q.data)

    def OR(self):
        self.out.set_data(self.p.data | self.q.data)

    def NOP(self):
        return
