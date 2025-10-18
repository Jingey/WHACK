from wires import Wire, Bus, Register
from enum import Enum

# Arithmetic and logic unit
# Connections: params p, q
# Functions: +, -, shift, not, and, or


MAX_VAL = 32_767
MIN_VAL = -32_768


class ALUFunction(Enum):
    ADD = 0b000
    SUB = 0b001
    AND = 0b010
    OR = 0b011
    SHIFT = 0b100
    NOT = 0b101


class Alu:
    def __init__(self, acc: Register, q_bus: Bus, func_bus: Bus, enable: Wire):
        self.acc = acc
        self.q = q_bus
        self.func = func_bus
        self.enable = enable
        self.enable.enlist(self.execute)
        self.ccr = Bus()

    def execute(self):
        match self.func.data:
            case ALUFunction.ADD:
                self.add()
            case ALUFunction.SUB:
                self.subtract()
            case ALUFunction.SHIFT:
                self.shift()
            case ALUFunction.NOT:
                self.NOT()
            case ALUFunction.AND:
                self.AND()
            case ALUFunction.OR:
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

        self.acc.data = result

    def add(self):
        self.set_result(self.acc.data + self.q.data)

    def subtract(self):
        self.set_result(self.acc.data - self.q.data)

    def shift(self):
        if (self.q.data & 0b10000) == 0:
            self.set_result(self.acc.data << self.q.data)
        else:
            self.set_result(self.acc.data >> self.q.data)

    def NOT(self):
        self.set_result(0b11111111_11111111 - self.acc.data)

    def AND(self):
        self.set_result(self.acc.data & self.q.data)

    def OR(self):
        self.set_result(self.acc.data | self.q.data)

    def NOP(self):
        return
