from alu import ALUFunction, AluEmulator, test_emulator


e = AluEmulator()


print("add test bed")
add_test = lambda x, y: test_emulator(e, ALUFunction.ADD, x, y)  # noqa

assert add_test(1, 2) == (3, 0)
assert add_test(403, 12) == (415, 0)
assert add_test(0, 0) == (0, 0b100)
assert add_test(0xFF_FF, 1) == (0, 0b110)
assert add_test(0xFF_FF - 10, 1002) == (991, 0b010)
assert add_test(0xFF_FF, 0xFF_FF - 2) == (0xFF_FF - 3, 0b011)
assert add_test(0xFF_FF - 1, 1) == (0xFF_FF, 0b001)
assert add_test(0x7F_FF, 1) == (0x80_00, 0b001)


print("sub test bed")
sub_test = lambda x, y: test_emulator(e, ALUFunction.SUB, x, y)  # noqa
assert sub_test(1, 2) == (0xFF_FF, 0b001)
assert sub_test(403, 12) == (391, 0)
assert sub_test(0, 0) == (0, 0b100)
assert sub_test(0, 1) == (0xFF_FF, 0b001)
assert sub_test(0xFF_FF, 1) == (0xFF_FE, 0b001)
assert sub_test(0xFF_FF - 10, 1002) == (0xFF_FF - 1012, 0b001)
assert sub_test(0xFF_FF, 0xFF_FF - 2) == (2, 0b000)
assert sub_test(0xFF_FF - 1, 1) == (0xFF_FF - 2, 0b001)
assert sub_test(0x80_00, 1) == (0x7F_FF, 0b000)


print("AND test bed")
and_test = lambda x, y: test_emulator(e, ALUFunction.AND, x, y)  # noqa
assert and_test(0xFF_FF, 1) == (1, 0b000)
assert and_test(0xFF_FF, 0x40_AB) == (0x40_AB, 0b000)
assert and_test(0x00_00, 0x40_AB) == (0, 0b100)

print("OR test bed")
or_test = lambda x, y: test_emulator(e, ALUFunction.OR, x, y)  # noqa
assert or_test(0xFF_FF, 1) == (0xFF_FF, 0b001)
assert or_test(0x00_00, 0x40_AB) == (0x40_AB, 0b000)

print("SHIFT test bed")
shift_test = lambda x, y: test_emulator(e, ALUFunction.SHIFT, x, y)  # noqa
assert shift_test(0x7F_FF, 0b00001) == (0xFF_FE, 0b001)
assert shift_test(0b1, 0b00011) == (0b1000, 0b000)
assert shift_test(0b1000, 0b10011) == (0b1, 0b000)
assert shift_test(0b1000, 0b10100) == (0b0, 0b100)

print("NOT test bed")
not_test = lambda x, y: test_emulator(e, ALUFunction.NOT, x, y)  # noqa
assert not_test(0xFF_FF, 34290) == (0, 0b100)
assert not_test(0xFF_FF, 0) == (0, 0b100)
assert not_test(0, 3490) == (0xFF_FF, 0b001)
assert not_test(0xF4_81, 23489) == (0x0B_7E, 0b000)
