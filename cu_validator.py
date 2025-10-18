import random
from cu import Opcode

# currently we replace shifts with jumps because shifts are a pain
# (jumps is random btw, its just has less stringent rules)


def make_valid_bit_string(bit_str: int):
    cir = bit_str >> 5
    return (make_valid_instruction(cir) << 5) | (bit_str & 0b11111)


def make_valid_instruction(cir: int):
    op_code = cir >> 12
    operand = cir & 0b1111_11111111
    match op_code:
        case Opcode.HLT.value | Opcode.NOP.value | Opcode.NOT.value:
            return op_code << 12
        case Opcode.STR.value | Opcode.LDR.value:
            # 50% chance to become a acc load
            if random.random() < 0.5:
                return (op_code << 12) | ((operand & 1) << 11) | 0b111_1111_1111
            # prevent random from being an acc load
            if operand & 0b111_1111_1111 == 0b111_1111_1111:
                return ((op_code << 12) | operand) ^ 0b1
        case Opcode.MOV.value:
            new_operand = operand >> 8
            if new_operand & 0b11 == 0:
                new_operand ^= 0b1
            return (op_code << 12) | (new_operand << 8)
        case Opcode.LS.value:
            return Opcode.JMP.value << 12 | operand
        case Opcode.ADD.value | Opcode.SUB.value | Opcode.AND.value | Opcode.OR.value:
            if (operand >> 11) == 0:
                return (op_code << 12) | (operand & 0b1100_0000_0000)
        case Opcode.INP.value | Opcode.OUT.value:
            return (op_code << 12) | (operand & 0b1000_0000_0000)

    return cir
