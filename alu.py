from wires import Wire, Bus, Register
from enum import Enum
from logs import log

# Arithmetic and logic unit
# Connections: params p, q
# Functions: +, -, shift, not, and, or


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
        self.ccr = Bus("ccr")
        self.emulator = AluEmulator()

    def execute(self):
        acc = self.acc.data
        q = self.q.data
        func = self.func.read_data()

        log.log_alu_calculating(acc, q, func)

        result = self.emulator.run(create_bin_str(func, acc, q))

        self.acc.data = result >> 3
        self.ccr.set_data(result & 0b111)

        log.log_alu_result(result >> 3, result & 0b111)


def create_bin_str(func, acc, q):
    return (func << 32) | (acc << 16) | q


def test_emulator(emulator, func, acc, q):
    res = emulator.run(create_bin_str(func.value, acc, q))
    data = res >> 3
    ccr = res & 0b111

    print(f"result: {data} ({bin(data)}), ccr:{bin(ccr)} (Z, C, N)")
    return (data, ccr)


class AluEmulator:
    def __init__(self):
        pass

    # 3 bit function | 16 bit acc | 16 bit bus
    # outputs 16 bits | 3 bit CCR (Z, C, N)
    def run(self, binary_input: int):
        function = binary_input >> 32
        left = (binary_input >> 16) & 0xFF_FF
        right = binary_input & 0xFF_FF
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

    def append_ccr(self, result, zero, neg):
        return (result << 3) | (zero << 2 | neg)

    def truncate_result(self, result):
        return result & ((1 << 16) - 1)

    def format_result(self, result):
        result = self.truncate_result(result)

        zero = result == 0
        neg = (result & (1 << 15)) != 0

        return self.append_ccr(result, zero, neg)

    def add(self, left, right):
        # set carry
        result = self.format_result(left + right)
        if left + right > 0xFF_FF:
            return result | 0b10
        return result

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
