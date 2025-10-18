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
        self.emulator = AluEmulator()

    def execute(self):
        bin_str = self.create_bin_str(
            self.func.read_data(), self.acc.data, self.q.read_data()
        )

        result = self.emulator.run(bin_str)

        self.acc.data = result >> 3
        self.ccr.set_data(result & 0b111)


def create_bin_str(func, acc, q):
    return (func << 32) | (acc << 16) | q


def test_emulator(emulator, func, acc, q):
    res = emulator.run(func.value, acc, q)
    data = res >> 3
    ccr = res & 0b111

    print(f"result: {data} ({bin(data)}), ccr:{bin(ccr)} (Z, C, N)")


class AluEmulator:
    def __init__(self):
        pass

    # 3 bit function | 16 bit acc | 16 bit bus
    # outputs 16 bits | 3 bit CCR
    def run(self, binary_input: int):
        function = binary_input >> 32
        left = (binary_input >> 16) & 0xFF_FF
        right = binary_input & 0xFF_FF
        print(left, right)
        match function:
            case ALUFunction.ADD.value:
                return self.add(left, right)
            case ALUFunction.SUB.value:
                return self.subtract(left, right)
            case ALUFunction.SHIFT.value:
                return self.shift(left, right)
            case ALUFunction.NOT.value:
                return self.NOT(left, right)
            case ALUFunction.AND.value:
                return self.AND(left, right)
            case ALUFunction.OR.value:
                return self.OR(left, right)
            case _:
                return left

    def append_ccr(self, result, zero, carry, neg):
        return (result << 3) | (zero << 2 | carry << 1 | neg)

    def truncate_result(self, result):
        if result & (1 << 15) == 0:
            return result % MAX_VAL
        return -(~(result & ((1 << 15) - 1)) + 1)

    def format_result(self, result):
        carry = result > MAX_VAL or result < MIN_VAL
        result = self.truncate_result(result)

        zero = result == 0
        neg = result < 0

        return self.append_ccr(result, zero, carry, neg)

    def add(self, left, right):
        return self.format_result(left + right)

    def subtract(self, left, right):
        return self.format_result(left - right)

    def shift(self, left, right):
        shift = right & 0b01111
        if (right & 0b10000) == 0:
            return self.format_result(left << shift)
        else:
            return self.format_result(left >> shift)

    def NOT(self, left, right):
        return self.format_result(0xFF_FF - left)

    def AND(self, left, right):
        return self.format_result(left & right)

    def OR(self, left, right):
        return self.format_result(left | right)
